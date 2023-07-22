#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Required Libraries
library(shiny)
library(shinythemes)
library(shinyMatrix)
library(quantmod)
library(RiskPortfolios)
library(factoextra)
library(dendextend)
library(cluster)
library(RColorBrewer)
library(shinybusy)

# User Interface
ui <- fluidPage(
  
  theme = shinytheme("darkly"),
  
  titlePanel("Statistical Learning based Portfolio Optimization"),
  
  sidebarLayout(
    sidebarPanel(
      matrixInput("input_matrix", "Portfolio",
                  value = matrix(data = c("VTSMX", "VGTSX", "VBMFX", "GLD", "GSG"), nrow = 5, ncol = 1, dimnames = list(NULL, c("Ticker"))),
                  rows = list(
                    extend = TRUE,
                    names = FALSE
                  ),
                  cols = list(
                    names = TRUE
                  )),
      checkboxInput("specify_clusters", "Specify number of clusters?", FALSE),
      conditionalPanel(
        condition = "input.specify_clusters == true",
        numericInput("min_clusters", "Minimum number of clusters", value = 2, min = 2),
        numericInput("max_clusters", "Maximum number of clusters", value = 10, min = 2)
      ),
      selectInput("rebalancing_frequency", "Rebalancing / Sampling Frequency",
                  choices = c("Daily" = "daily", "Weekly" = "weekly", "Monthly" = "monthly", "Quarterly" = "quarterly", "Semi-annually" = "semiannually", "Yearly" = "annual"),
                  selected = "weekly"),
      selectInput("inter_cluster_weighting", "Inter-Cluster Weighting",
                  choices = c("Risk Weighted" = "risk", "Equally Weighted" = "equally")),
      selectInput("intra_cluster_weighting", "Intra-Cluster Weighting",
                  choices = c("Risk Weighted" = "risk", "Equally Weighted" = "equally")),
      actionButton("submit", "Optimize"),
      tags$style(type='text/css', "#info-text { margin-top: 40px; }"),
      HTML("<p id='info-text'><b>Recommendation:</b> It is recommended to use this application on a desktop for the best user experience.<br><br>
This web application utilizes the <a href='https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3237540' target='_blank'>Hierarchical Equal Risk Contribution</a> (HERC) approach, a modern portfolio optimization method developed by Raffinot (2018). It combines the unique strengths of the pioneering <a href='https://papers.ssrn.com/sol3/papers.cfm?abstract_id=2708678' target='_blank'>Hierarchical Risk Parity</a> (HRP) method by López de Prado (2016) and <a href='https://papers.ssrn.com/sol3/papers.cfm?abstract_id=2840729' target='_blank'>Hierarchical Clustering-Based Asset Allocation</a> (HCAA) method by Raffinot (2017).<br><br>

Traditional portfolio optimization suffers from significant instability, primarily due to treating the vector space of return series as a fully connected graph, where each node can potentially substitute for another. This complicated structure magnifies minute estimation errors, leading to unstable solutions.
Hierarchical clustering-based tree structures address this issue by eliminating irrelevant links.<br><br><br>

<b>Your Input:</b><br><br>

1) Use the Ticker table above to enter the securities you wish to invest in. The data is sourced from Yahoo Finance.<br>
2) Specify the range of clusters if you wish to narrow this parameter. If not specified, the Silhouette method will determine the optimal number.<br>
3) Choose the method to weight the clusters in relation to one another and the assets within each cluster. Risk is defined here as volatility.<br><br>

<b>Output:</b><br><br>

On the right panel, you will find:</b><br>
1) A dendrogram illustrating the hierarchical structure of the securities.<br>
2) A labelled pie chart representing the optimized portfolio.<br>
3) A table listing each security's portfolio weight and cluster membership.<br>
4) A graph depicting the cumulative returns of the securities and the optimized portfolio based on the used data.<br>
5) A bar chart comparing the Sharpe ratios of the securities and the optimized portfolio.<br><br>

The application automatically identifies the optimal linkage method.<br><br><br>
For more information on this and my other projects, please visit my <a href='https://github.com/YannickKae/Statistical-Learning-based-Portfolio-Optimization' target='_blank'>GitHub</a>.</p>")
    ),
    
    mainPanel(
      fluidRow(wellPanel(plotOutput("dendrogram_plot"))),
      fluidRow(wellPanel(plotOutput("portfolio_pie"))),
      fluidRow(wellPanel(tableOutput("optimized_portfolio"))),
      fluidRow(wellPanel(plotOutput("cumulative_return_chart"))),
      fluidRow(wellPanel(plotOutput("sharpe_ratio_barplot"))),
    ) 
  )
)


# Server logic
# Server logic
server <- function(input, output) {
  
  observeEvent(input$submit, {
    
    req(input$input_matrix)
    
    show_modal_spinner(color = "white") # show the modal window
    
    ######################## Step 1) gathering the Data ###############################
    
    # Get the tickers from input
    tickers <- input$input_matrix[,1]
    
    # Filter out empty tickers
    tickers <- tickers[tickers != ""]
    
    # Check if the user entered only one ticker
    if(length(tickers) == 1) {
      showModal(modalDialog(
        title = "Input Error",
        "You need at least two assets for optimization."
      ))
      return()
    }
    
    # Initialize the list to store asset data
    assets <- list()
    
    for (ticker in tickers) {
      asset_data <- try(getSymbols(ticker, src = "yahoo", from = "1900-01-01", auto.assign = FALSE), silent = TRUE)
      if (class(asset_data) == "try-error") {
        showModal(modalDialog(
          title = "Input Error",
          paste("The following ticker is not available: ", ticker)
        ))
        return()
      } else {
        assets[[ticker]] <- asset_data
      }
    }
    
    # Helper function to compute returns based on frequency
    compute_returns <- function(price_data, frequency) {
      switch(frequency,
             "daily" = dailyReturn(price_data),
             "weekly" = {price_data <- price_data[.indexwday(price_data) == 5]
             weeklyReturn(price_data)},
             "monthly" = monthlyReturn(price_data, leading = FALSE),
             "quarterly" = quarterlyReturn(price_data, leading = FALSE),
             "annual" = annualReturn(price_data, leading = FALSE),
             stop("Invalid frequency"))
    }
    
    # Get the frequency from input
    return_frequency <- input$rebalancing_frequency
    
    # Create a list to store return data
    returns_list <- list()
    
    for (ticker in tickers) {
      # Get returns
      asset_returns <- compute_returns(assets[[ticker]][, 6], return_frequency)
      
      # Add to returns list
      returns_list[[ticker]] <- asset_returns
    }
    
    # Merge all returns into a single data frame and remove rows with NA values
    return_matrix <- na.omit(do.call("merge.xts", returns_list))
    
    # Set the column names to the ticker names
    colnames(return_matrix) <- tickers
    
    transposed_xts <- t(return_matrix)
    
    ######################## Step 2) Estimating optimal Linkage ###############################
    
    # Define linkage methods
    m <- c("single", "average", "complete", "ward")
    names(m) <- c("average", "single", "complete", "ward")
    
    # Function to compute agglomerative coefficient
    ac <- function(x) {
      agnes(transposed_xts, method = x)$ac
    }
    
    # Calculate agglomerative coefficient for each clustering linkage method
    ac_values <- sapply(m, ac)
    
    # Find the method with the highest agglomerative coefficient
    best_method <- names(which.max(ac_values))
    
    ######################## Step 3) Estimating the optimal number of clusters ###############################
    
    # Define your clustering function
    mycluster <- function(x, k) {
      d <- dist(x)
      hc <- hclust(d, method = best_method) # specify the method here
      cutree(hc, k = k)
    }
    
    # Check if user has chosen to specify number of clusters
    if (input$specify_clusters) {
      
      # Check if minimum clusters is larger than number of tickers - 1
      if (input$min_clusters > (nrow(transposed_xts) - 1)) {
        
        showModal(modalDialog(
          title = "Input Error",
          "The minimum number of clusters must be less than the number of tickers."
        ))
        return()
      }
      
      # Check if maximum clusters is larger than number of tickers - 1
      if (input$max_clusters > nrow(transposed_xts) - 1) {
        
        max_clusters <- nrow(transposed_xts) - 1
        
      } else {
        
        max_clusters <- input$max_clusters
      }
      
      # Initialize a list to store silhouette results
      sil_width <- numeric(max_clusters)
      
      # Compute the silhouette width for each number of clusters
      for (k in input$min_clusters:max_clusters) {
        cluster_assignments <- mycluster(transposed_xts, k)
        sil_results <- silhouette(cluster_assignments, dist(transposed_xts))
        sil_width[k] <- mean(sil_results[, 3]) # average silhouette width
      }
      
      # Find the number of clusters that has the maximum average silhouette width
      optimal_k <- which.max(sil_width)
      
    } else {
      
      # If user did not specify, create sequence from 2 to number of tickers - 1
      if(length(tickers) == 2) {
        # In case there are only two tickers, the optimal number of clusters should be set to 1
        optimal_k <- 2
      } else {
        
        max_clusters <- nrow(transposed_xts) - 1
        # Initialize a list to store silhouette results
        sil_width <- numeric(max_clusters)
        
        # Compute the silhouette width for each number of clusters
        for (k in 2:max_clusters) {
          cluster_assignments <- mycluster(transposed_xts, k)
          sil_results <- silhouette(cluster_assignments, dist(transposed_xts))
          sil_width[k] <- mean(sil_results[, 3]) # average silhouette width
        }
        
        # Find the number of clusters that has the maximum average silhouette width
        optimal_k <- which.max(sil_width)
        
      }
    }
    
    
    ######################## Step 4) Estimating Inter Cluster Weights ###############################
    
    # Compute distance matrix
    d <- dist(transposed_xts)
    
    # Perform hierarchical clustering using Ward's method
    final_clust <- hclust(d, method = best_method)
    
    # Cut the dendrogram into optimal_k clusters
    groups <- cutree(final_clust, k=optimal_k)
    
    # Split the data by cluster assignments
    clustered_data <- split(row.names(transposed_xts), groups)
    
    # Convert hclust object (final_clust) to a dendrogram object
    dend <- as.dendrogram(final_clust)
    
    # Color branches by cluster and labels by their original order
    if(optimal_k > 1) {
      dend <- color_branches(dend, k = optimal_k)
      dend <- color_labels(dend, k = optimal_k)
    }
    
    # Modify the dendrogram
    dend <- hang.dendrogram(dend)
    dend <- set(dend, "labels_cex", 1.25)  # Change the size of the labels
    dend <- set(dend, "branches_lwd", 4)  # Change the width of the branches
    dend <- set(dend, "branches_lty", 1)  # Change the line type of the branches
    
    get_cluster_members <- function(node, dendrogram_structure) {
      if (node < 0) {
        return(-node)
      } else {
        left_members <- get_cluster_members(dendrogram_structure[node, 1], dendrogram_structure)
        right_members <- get_cluster_members(dendrogram_structure[node, 2], dendrogram_structure)
        return(c(left_members, right_members))
      }
    }
    
    if (input$inter_cluster_weighting == "risk") {
      risk_contrib <- apply(return_matrix, 2, sd)
    } else if (input$inter_cluster_weighting == "equally") {
      risk_contrib <- apply(return_matrix, 2, sd)
      asset_names <- names(risk_contrib)
      risk_contrib <- rep(1, length(asset_names))
      names(risk_contrib) <- asset_names
    } else {
      stop("Invalid option for inter cluster weights")
    }
      
      cluster_weights <- c()
      cluster_members <- c()
      cluster_labels <- c()
      
      while (length(cluster_weights) < optimal_k) {
        
        if (length(cluster_weights) < 1) {
          
          # Get the children clusters
          childs <- c()
          childs <- final_clust$merge[nrow(final_clust$merge), ]
          
          rc_1 <- median(risk_contrib[get_cluster_members(childs[1], final_clust$merge)]) / (median(risk_contrib[get_cluster_members(childs[1], final_clust$merge)]) + median(risk_contrib[get_cluster_members(childs[2], final_clust$merge)]))
          rc_2 <- 1-rc_1
          
          w_1 <- rc_2
          w_2 <- rc_1
          cluster_weights <- c(cluster_weights, w_1, w_2)
          
          m_1 <- get_cluster_members(childs[1], final_clust$merge)
          m_2 <- get_cluster_members(childs[2], final_clust$merge)
          cluster_members <- c(cluster_members, list(m_1), list(m_2))
          
          cluster_labels <- c(childs)
          
          old_max_row <- nrow(final_clust$merge)
          new_max_row <- old_max_row - 1
          
        } else {
          
          col_num <- which(cluster_labels == new_max_row)
          
          childs <- c()
          childs <- final_clust$merge[new_max_row, ]
          
          rc_1 <- median(risk_contrib[get_cluster_members(childs[1], final_clust$merge)]) / (median(risk_contrib[get_cluster_members(childs[1], final_clust$merge)]) + median(risk_contrib[get_cluster_members(childs[2], final_clust$merge)]))
          rc_2 <- 1-rc_1
          
          m_1 <- get_cluster_members(childs[1], final_clust$merge)
          m_2 <- get_cluster_members(childs[2], final_clust$merge)
          
          if (col_num == 1) {
            
            w_1 <- rc_2 * cluster_weights[1]  # calculate new w_1
            w_2 <- cluster_weights[1] - w_1
            cluster_weights <- c(w_1, w_2, cluster_weights[2:length(cluster_weights)])
            
            cluster_members <- c(list(m_1), list(m_2), cluster_members[2:length(cluster_members)])
            
            cluster_labels <- c(childs, cluster_labels[2:length(cluster_labels)])
            
          } else if (col_num == length(cluster_weights)) {
            
            w_1 <- rc_2 * cluster_weights[length(cluster_weights)]  # calculate new w_1
            w_2 <- cluster_weights[length(cluster_weights)] - w_1
            cluster_weights <- c(cluster_weights[1:(length(cluster_weights)-1)], w_1, w_2)
            
            cluster_members <- c(cluster_members[1:(length(cluster_members)-1)], list(m_1), list(m_2))
            
            cluster_labels <- c(cluster_labels[1:(length(cluster_labels)-1)], childs)
            
          } else {
            
            w_1 <- rc_2 * cluster_weights[col_num]  # calculate new w_1
            w_2 <- cluster_weights[col_num] - w_1
            cluster_weights <- c(cluster_weights[1:(length(cluster_weights)-1)], w_1, w_2, cluster_weights[(col_num + 1):length(cluster_weights)])
            
            cluster_members <- c(cluster_members[1:(length(cluster_members)-1)], list(m_1), list(m_2), cluster_members[(col_num + 1):length(cluster_members)])
            
            cluster_labels <- c(cluster_labels[1:(length(cluster_labels)-1)], childs, cluster_labels[(col_num + 1):length(cluster_labels)])
            
          }
          
          old_max_row <- new_max_row
          new_max_row <- old_max_row - 1
          
          col_num <- 1
          
        }
      }
    
    ######################## Step 5) Estimating Intra Cluster Weights ###############################
    
    # Cut the dendrogram into optimal_k clusters
    groups <- cutree(final_clust, k = optimal_k)
    
    # Split the data by cluster assignments
    clustered_data <- split(row.names(transposed_xts), groups)
    
    if (input$intra_cluster_weighting == "risk") {
      risk_contrib <- apply(return_matrix, 2, sd)
    } else if (input$intra_cluster_weighting == "equally") {
      risk_contrib <- apply(return_matrix, 2, sd)
      asset_names <- names(risk_contrib)
      risk_contrib <- rep(1, length(asset_names))
      names(risk_contrib) <- asset_names
    } else {
      stop("Invalid option for inter cluster weights")
    }
    
    # Compute naive risk parity weights within each cluster
    naive_risk_parity <- function(cluster, risk_contrib) {
      cluster_risk_contrib <- risk_contrib[cluster]
      inverse_risk <- 1 / cluster_risk_contrib
      nrp_weights <- inverse_risk / sum(inverse_risk)
      return(nrp_weights)
    }
    
    # Calculate final asset weights and their cluster memberships
    final_weights <- list()
    asset_clusters <- list()
    for(i in 1:length(cluster_members)){
      # Use column names to transform the numeric identifiers back to original labels
      cluster <- colnames(return_matrix)[unlist(cluster_members[[i]])]
      nrp_weights <- naive_risk_parity(cluster, risk_contrib)
      final_weights[[i]] <- nrp_weights * cluster_weights[i]
      # Keep track of which cluster each asset belongs to
      asset_clusters[[i]] <- rep(i, length(nrp_weights))
    }
    
    # Convert the list of weights and clusters into single vectors
    final_weights <- unlist(final_weights)
    asset_clusters <- unlist(asset_clusters)
    
    output$dendrogram_plot <- renderPlot({
      par(mar = c(mar = c(5, 5, 5, 10)))
      plot(dend, main = "Hierarchical Asset Structure", cex.main=1.25)
    })
    
    output$optimized_portfolio <- renderTable({
      
      # Create the data frame
      df <- data.frame(
        Weight = paste0(round(final_weights * 100), "%"),  # Convert to percentage, round, and add %
        Cluster = as.character(asset_clusters), 
        row.names = names(final_weights)       # Set row names to asset names
      )
      
      # Transpose the dataframe
      df_transposed <- t(df)
      
      return(df_transposed)
      
    }, rownames = TRUE)
    
    # Create a dataframe from the final_weights object
    df <- as.data.frame(final_weights)
    
    # Set labels for the pie chart
    labels <- rownames(df)
    
    # Set values for the pie chart
    values <- df$final_weights
    
    # Create a data frame with labels and values
    pie_data <- data.frame(labels, values)
    
    # Sort the data frame by values in descending order
    pie_data <- pie_data[order(pie_data$values, decreasing = TRUE), ]
    
    # Create the pie chart
    chart <- ggplot(pie_data, aes(x = "", y = values, fill = labels)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      scale_fill_brewer(palette="Set1") +
      theme_void() +
      theme(legend.text = element_text(size = 15))  # Modify title font size and style
    
    # Display the pie chart
    output$portfolio_pie <- renderPlot({chart})
    
    output$cumulative_return_chart <- renderPlot({
      
      # Multiply each asset's returns by its weight
      weighted_returns <- return_matrix * final_weights
      
      # Sum up to get portfolio return at each time point
      portfolio_returns <- rowSums(weighted_returns)
      
      # Add portfolio returns to your data as a new column
      Portfolio <- return_matrix
      Portfolio$Portfolio_Returns <- portfolio_returns
      
      # Calculate cumulative returns for the portfolio by adding 1 to the returns, calculating the cumulative product, then subtracting 1
      cumulative_returns_portfolio <- cumprod(1 + Portfolio$Portfolio_Returns)
      
      # Convert it to xts object if needed
      cumulative_returns_portfolio <- as.xts(cumulative_returns_portfolio)
      
      # Calculate cumulative returns for each asset
      cumulative_returns_assets <- cumprod(1 + return_matrix)
      
      # Convert it to xts object if needed
      cumulative_returns_assets <- as.xts(cumulative_returns_assets)
      
      # Plot the cumulative returns with log y-axis
      colors <- brewer.pal(n = ncol(cumulative_returns_assets), name = "Set1")
      matplot(index(cumulative_returns_assets), cumulative_returns_assets, type = 'l', lty = 1, lwd = 1, log = 'y', col = colors, xlab = "", ylab = "", yaxt = 'n')
      lines(index(cumulative_returns_portfolio), cumulative_returns_portfolio, col = "black", lwd = 2)
      title(main = "Cumulative Returns", col.main = "black", font.main = 4)
      legend("topleft", legend = c(colnames(cumulative_returns_assets), "Portfolio"), col = c(colors, "black"), lty = 1, cex = 1, lwd = 2, bty = "n", box.col = "white")
      
      # Add a horizontal line at 1
      abline(h = 1, col = "black", lwd = 2, lty = 2)
      
      # Add y axis with percentage format
      at = axTicks(2)
      labels = scales::percent(at)
      axis(2, at = at, labels = labels, las = 1)
    })
    
    # Multiply each asset's returns by its weight
    weighted_returns <- return_matrix * final_weights
    
    # Sum up to get portfolio return at each time point
    portfolio_returns <- rowSums(weighted_returns)
    
    # Add portfolio returns to your data as a new column
    Portfolio <- return_matrix
    Portfolio$Portfolio <- portfolio_returns
    
    # Calculate mean of each column
    mean_returns <- apply(Portfolio, 2, mean)
    
    # Calculate standard deviation of each column
    sd_returns <- apply(Portfolio, 2, sd)
    
    # Calculate Sharpe Ratio for each column
    sharpe_ratio <- mean_returns / sd_returns
    
    # Create a data frame with asset names and Sharpe ratios
    sharpe_data <- data.frame(Asset = names(sharpe_ratio), Sharpe = sharpe_ratio)
    
    output$sharpe_ratio_barplot <- renderPlot({
      
      # Create a color vector for all assets using "Set1" palette, and set portfolio color to black
      color_vector <- ifelse(names(sharpe_ratio) != "Portfolio", brewer.pal(length(sharpe_ratio)-1, "Set1"), "black")
      
      # Sort color vector to match sorted data
      color_vector <- color_vector[order(sharpe_ratio, decreasing = TRUE)]
      
      # Create a data frame with asset names and Sharpe ratios
      sharpe_data <- data.frame(Asset = names(sharpe_ratio), Sharpe = sharpe_ratio)
      
      # Sort the data in descending order
      sharpe_data <- sharpe_data[order(-sharpe_data$Sharpe), ]
      
      # Plot the Sharpe Ratios
      ggplot(sharpe_data, aes(x = reorder(Asset, Sharpe), y = Sharpe, fill = Asset)) +
        geom_bar(stat = "identity", width = 0.5, color = "black") +
        scale_fill_manual(values = color_vector) +
        labs(title = "Sharpe Ratio", x = "", y = "Sharpe Ratio") +
        theme_minimal() +
        theme(legend.position = "none", # Remove legend
              plot.title = element_text(hjust = 0.5, size = 20),  # Center title and adjust size
              axis.text = element_text(size = 14),  # Adjust axis text size
              axis.title = element_text(size = 14))  # Adjust axis title size
    })
    
    remove_modal_spinner() # remove it when done
    
  })
}

shinyApp(ui = ui, server = server)
