# Statistical Learning based Portfolio Optimization

<p align="center">
  <img src="Hierachical Asset Structure.png" alt="Hierarchical Asset Structure" style="width:100%">
</p>
<p align="center">
  <i>Exemplary Hierarchical Asset Structure</i>
</p>

This R Shiny application utilizes the [Hierarchical Equal Risk Contribution](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3237540) (HERC) approach, a modern portfolio optimization method developed by Raffinot (2018). It combines the unique strengths of the pioneering [Hierarchical Risk Parity](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=2708678) (HRP) method by López de Prado (2016) and [Hierarchical Clustering-Based Asset Allocation](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=2840729) (HCAA) method by Raffinot (2017).

Traditional portfolio optimization suffers from significant instability, primarily due to modeling the vector space of return series as a fully connected graph, where each node can potentially substitute for another. This complicated structure magnifies minute estimation errors, leading to unstable solutions. Hierarchical clustering-based tree structures address this issue by eliminating irrelevant links.

As far as I know, there is no other correct implementation of this methodology in R.

## User Input

- Use the Ticker table above to enter the securities you wish to invest in. The data is sourced from Yahoo Finance.
- Specify the range of clusters if you wish to narrow this parameter. If not specified, the Silhouette method will determine the optimal number.
- Choose the method to weight the clusters in relation to one another and the assets within each cluster. Risk is defined here as volatility.

## Output

- Dendrogram illustrating the hierarchical structure of the securities.
- Pie chart representing the optimized portfolio.
- Table listing each security's portfolio weight and cluster membership.
- Graph depicting the cumulative returns of the securities and the optimized portfolio based on the used data.
- Bar chart comparing the Sharpe ratios of the securities and the optimized portfolio.

The optimal linkage criterion is estimated based on the [agglomerative coefficient](http://strata.uga.edu/8370/lecturenotes/clusterAnalysis.html).

  ## Feature Pipeline

  As soon as I find the time, I will make the app more visually appealing and add more features. These include the following:

- Possibility to upload your own data as a csv file.
- More risk metrics, such as [Conditional Drawdown at Risk](https://breakingdownfinance.com/finance-topics/alternative-investments/conditional-drawdown-at-risk-cdar/) (CDaR)
- Risk-affine weighting
- Shrinkage, denoising and detoning possibilities for the dependency matrix
- Further dependency metrics, such as [mutual information](https://en.wikipedia.org/wiki/Mutual_information).
- Possibility to also enter short positions.
