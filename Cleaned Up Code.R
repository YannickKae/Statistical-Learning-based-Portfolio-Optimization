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
                  rows = list(extend = TRUE, names = FALSE),
                  cols = list(names = TRUE)),
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
server <- function(input, output) {
  
  observeEvent(input$submit, {
    
    req(input$input_matrix)
    show_modal_spinner(color = "white") # show the modal window
    
    tickers <- clean_tickers(input$input_matrix)
    if (length(tickers) < 2) {
      show_error_modal("You need at least two assets for optimization.")
      return()
    }
    
    assets <- get_asset_data(tickers)
    if (is.null(assets)) return()
    
    return_matrix <- compute_returns_matrix(assets, input$rebalancing_frequency)
    transposed_xts <- t(return_matrix)
    
    best_method <- find_best_linkage_method(transposed_xts)
    optimal_k <- determine_optimal_clusters(transposed_xts, best_method, input$specify_clusters, input$min_clusters, input$max_clusters)
    
    final_clust <- perform_clustering(transposed_xts, best_method, optimal_k)
    dend <- prepare_dendrogram(final_clust, optimal_k)
    
    risk_contrib <- get_risk_contrib(return_matrix, input$inter_cluster_weighting)
    cluster_weights, cluster_members <- calculate_cluster_weights(final_clust, risk_contrib, optimal_k)
    
    risk_contrib <- get_risk_contrib(return_matrix, input$intra_cluster_weighting)
    final_weights, asset_clusters <- calculate_final_weights(cluster_members, risk_contrib, cluster_weights)
    
    output$dendrogram_plot <- render_dendrogram(dend)
    output$optimized_portfolio <- render_optimized_portfolio(final_weights, asset_clusters)
    output$portfolio_pie <- render_portfolio_pie(final_weights)
    output$cumulative_return_chart <- render_cumulative_return_chart(return_matrix, final_weights)
    output$sharpe_ratio_barplot <- render_sharpe_ratio_barplot(return_matrix, final_weights)
    
    remove_modal_spinner() # remove it when done
    
  })
}

shinyApp(ui = ui, server = server)

# Helper functions

clean_tickers <- function(input_matrix) {
  tickers <- input_matrix[,1]
  tickers <- tickers[tickers != ""]
  return(tickers)
}

show_error_modal <- function(message) {
  showModal(modalDialog(
    title = "Input Error",
    message
  ))
}

get_asset_data <- function(tickers) {
  assets <- list()
  for (ticker in tickers) {
    asset_data <- try(getSymbols(ticker, src = "yahoo", from = "1900-01-01", auto.assign = FALSE), silent = TRUE)
    if (class(asset_data) == "try-error") {
      show_error_modal(paste("The following ticker is not available: ", ticker))
      return(NULL)
    } else {
      assets[[ticker]] <- asset_data
    }
  }
  return(assets)
}

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

compute_returns_matrix <- function(assets, frequency) {
  returns_list <- lapply(assets, function(asset) compute_returns(asset[, 6], frequency))
  return_matrix <- na.omit(do.call("merge.xts", returns_list))
  colnames(return_matrix) <- names(returns_list)
  return(return_matrix)
}

find_best_linkage_method <- function(transposed_xts) {
  m <- c("single", "average", "complete", "ward")
  names(m) <- c("average", "single", "complete", "ward")
  ac <- function(x) agnes(transposed_xts, method = x)$ac
  ac_values <- sapply(m, ac)
  best_method <- names(which.max(ac_values))
  return(best_method)
}

determine_optimal_clusters <- function(transposed_xts, best_method, specify_clusters, min_clusters, max_clusters) {
  mycluster <- function(x, k) {
    d <- dist(x)
    hc <- hclust(d, method = best_method)
    cutree(hc, k = k)
  }
  
  if (specify_clusters) {
    if (min_clusters > (nrow(transposed_xts) - 1)) {
      show_error_modal("The minimum number of clusters must be less than the number of tickers.")
      return()
    }
    max_clusters <- min(max_clusters, nrow(transposed_xts) - 1)
  } else {
    min_clusters <- 2
    max_clusters <- nrow(transposed_xts) - 1
  }
  
  sil_width <- numeric(max_clusters)
  for (k in min_clusters:max_clusters) {
    cluster_assignments <- mycluster(transposed_xts, k)
    sil_results <- silhouette(cluster_assignments, dist(transposed_xts))
    sil_width[k] <- mean(sil_results[, 3])
  }
  
  optimal_k <- which.max(sil_width)
  return(optimal_k)
}

perform_clustering <- function(transposed_xts, best_method, optimal_k) {
  d <- dist(transposed_xts)
  final_clust <- hclust(d, method = best_method)
  return(final_clust)
}

prepare_dendrogram <- function(final_clust, optimal_k) {
  dend <- as.dendrogram(final_clust)
  if (optimal_k > 1) {
    dend <- color_branches(dend, k = optimal_k)
    dend <- color_labels(dend, k = optimal_k)
  }
  dend <- hang.dendrogram(dend)
  dend <- set(dend, "labels_cex", 1.25)
  dend <- set(dend, "branches_lwd", 4)
  dend <- set(dend, "branches_lty", 1)
  return(dend)
}

get_risk_contrib <- function(return_matrix, weighting_method) {
  if (weighting_method == "risk") {
    risk_contrib <- apply(return_matrix, 2, sd)
  } else if (weighting_method == "equally") {
    asset_names <- colnames(return_matrix)
    risk_contrib <- rep(1, length(asset_names))
    names(risk_contrib) <- asset_names
  } else {
    stop("Invalid option for weighting method")
  }
  return(risk_contrib)
}

calculate_cluster_weights <- function(final_clust, risk_contrib, optimal_k) {
  cluster_weights <- c()
  cluster_members <- c()
  cluster_labels <- c()
  
  while (length(cluster_weights) < optimal_k) {
    if (length(cluster_weights) < 1) {
      childs <- final_clust$merge[nrow(final_clust$merge), ]
      rc_1 <- median(risk_contrib[get_cluster_members(childs[1], final_clust$merge)]) / (median(risk_contrib[get_cluster_members(childs[1], final_clust$merge)]) + median(risk_contrib[get_cluster_members(childs[2], final_clust$merge)]))
      rc_2 <- 1 - rc_1
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
      childs <- final_clust$merge[new_max_row, ]
      rc_1 <- median(risk_contrib[get_cluster_members(childs[1], final_clust$merge)]) / (median(risk_contrib[get_cluster_members(childs[1], final_clust$merge)]) + median(risk_contrib[get_cluster_members(childs[2], final_clust$merge)]))
      rc_2 <- 1 - rc_1
      m_1 <- get_cluster_members(childs[1], final_clust$merge)
      m_2 <- get_cluster_members(childs[2], final_clust$merge)
      if (col_num == 1) {
        w_1 <- rc_2 * cluster_weights[1]
        w_2 <- cluster_weights[1] - w_1
        cluster_weights <- c(w_1, w_2, cluster_weights[2:length(cluster_weights)])
        cluster_members <- c(list(m_1), list(m_2), cluster_members[2:length(cluster_members)])
        cluster_labels <- c(childs, cluster_labels[2:length(cluster_labels)])
      } else if (col_num == length(cluster_weights)) {
        w_1 <- rc_2 * cluster_weights[length(cluster_weights)]
        w_2 <- cluster_weights[length(cluster_weights)] - w_1
        cluster_weights <- c(cluster_weights[1:(length(cluster_weights) - 1)], w_1, w_2)
        cluster_members <- c(cluster_members[1:(length(cluster_members) - 1)], list(m_1), list(m_2))
        cluster_labels <- c(cluster_labels[1:(length(cluster_labels) - 1)], childs)
      } else {
        w_1 <- rc_2 * cluster_weights[col_num]
        w_2 <- cluster_weights[col_num] - w_1
        cluster_weights <- c(cluster_weights[1:(length(cluster_weights) - 1)], w_1, w_2, cluster_weights[(col_num + 1):length(cluster_weights)])
        cluster_members <- c(cluster_members[1:(length(cluster_members) - 1)], list(m_1), list(m_2), cluster_members[(col_num + 1):length(cluster_members)])
        cluster_labels <- c(cluster_labels[1:(length(cluster_labels) - 1)], childs, cluster_labels[(col_num + 1):length(cluster_labels)])
      }
      old_max_row <- new_max_row
      new_max_row <- old_max_row - 1
      col_num <- 1
    }
  }
  return(list(cluster_weights, cluster_members))
}

calculate_final_weights <- function(cluster_members, risk_contrib, cluster_weights) {
  naive_risk_parity <- function(cluster, risk_contrib) {
    cluster_risk_contrib <- risk_contrib[cluster]
    inverse_risk <- 1 / cluster_risk_contrib
    nrp_weights <- inverse_risk / sum(inverse_risk)
    return(nrp_weights)
  }
  
  final_weights <- list()
  asset_clusters <- list()
  for (i in 1:length(cluster_members)) {
    cluster <- colnames(return_matrix)[unlist(cluster_members[[i]])]
    nrp_weights <- naive_risk_parity(cluster, risk_contrib)
    final_weights[[i]] <- nrp_weights * cluster_weights[i]
    asset_clusters[[i]] <- rep(i, length(nrp_weights))
  }
  
  final_weights <- unlist(final_weights)
  asset_clusters <- unlist(asset_clusters)
  return(list(final_weights, asset_clusters))
}

render_dendrogram <- function(dend) {
  renderPlot({
    par(mar = c(mar = c(5, 5, 5, 10)))
    plot(dend, main = "Hierarchical Asset Structure", cex.main = 1.25)
  })
}

render_optimized_portfolio <- function(final_weights, asset_clusters) {
  renderTable({
    df <- data.frame(
      Weight = paste0(round(final_weights * 100), "%"),
      Cluster = as.character(asset_clusters),
      row.names = names(final_weights)
    )
    df_transposed <- t(df)
    return(df_transposed)
  }, rownames = TRUE)
}

render_portfolio_pie <- function(final_weights) {
  df <- as.data.frame(final_weights)
  labels <- rownames(df)
  values <- df$final_weights
  pie_data <- data.frame(labels, values)
  pie_data <- pie_data[order(pie_data$values, decreasing = TRUE), ]
  
  chart <- ggplot(pie_data, aes(x = "", y = values, fill = labels)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    scale_fill_brewer(palette = "Set1") +
    theme_void() +
    theme(legend.text = element_text(size = 15))
  
  renderPlot({ chart })
}

render_cumulative_return_chart <- function(return_matrix, final_weights) {
  renderPlot({
    weighted_returns <- return_matrix * final_weights
    portfolio_returns <- rowSums(weighted_returns)
    Portfolio <- return_matrix
    Portfolio$Portfolio_Returns <- portfolio_returns
    
    cumulative_returns_portfolio <- cumprod(1 + Portfolio$Portfolio_Returns)
    cumulative_returns_portfolio <- as.xts(cumulative_returns_portfolio)
    cumulative_returns_assets <- cumprod(1 + return_matrix)
    cumulative_returns_assets <- as.xts(cumulative_returns_assets)
    
    colors <- brewer.pal(n = ncol(cumulative_returns_assets), name = "Set1")
    matplot(index(cumulative_returns_assets), cumulative_returns_assets, type = 'l', lty = 1, lwd = 1, log = 'y', col = colors, xlab = "", ylab = "", yaxt = 'n')
    lines(index(cumulative_returns_portfolio), cumulative_returns_portfolio, col = "black", lwd = 2)
    title(main = "Cumulative Returns", col.main = "black", font.main = 4)
    legend("topleft", legend = c(colnames(cumulative_returns_assets), "Portfolio"), col = c(colors, "black"), lty = 1, cex = 1, lwd = 2, bty = "n", box.col = "white")
    abline(h = 1, col = "black", lwd = 2, lty = 2)
    at = axTicks(2)
    labels = scales::percent(at)
    axis(2, at = at, labels = labels, las = 1)
  })
}

render_sharpe_ratio_barplot <- function(return_matrix, final_weights) {
  renderPlot({
    weighted_returns <- return_matrix * final_weights
    portfolio_returns <- rowSums(weighted_returns)
    Portfolio <- return_matrix
    Portfolio$Portfolio <- portfolio_returns
    mean_returns <- apply(Portfolio, 2, mean)
    sd_returns <- apply(Portfolio, 2, sd)
    sharpe_ratio <- mean_returns / sd_returns
    sharpe_data <- data.frame(Asset = names(sharpe_ratio), Sharpe = sharpe_ratio)
    
    color_vector <- ifelse(names(sharpe_ratio) != "Portfolio", brewer.pal(length(sharpe_ratio) - 1, "Set1"), "black")
    color_vector <- color_vector[order(sharpe_ratio, decreasing = TRUE)]
    sharpe_data <- sharpe_data[order(-sharpe_data$Sharpe), ]
    
    ggplot(sharpe_data, aes(x = reorder(Asset, Sharpe), y = Sharpe, fill = Asset)) +
      geom_bar(stat = "identity", width = 0.5, color = "black") +
      scale_fill_manual(values = color_vector) +
      labs(title = "Sharpe Ratio", x = "", y = "Sharpe Ratio") +
      theme_minimal() +
      theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5, size = 20),
            axis.text = element_text(size = 14),
            axis.title = element_text(size = 14))
  })
}

shinyApp(ui = ui, server = server)
