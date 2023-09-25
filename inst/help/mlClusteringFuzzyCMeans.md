Fuzzy c-means Clustering
===

Fuzzy c-means clustering is a soft partitioning method that provides an output that contains the degree of association for each observation to each cluster. This makes it possible for data observations to be partially assigned to multiple clusters and give a degree of confidence about cluster membership. Fuzzy c-means' approach is quite similar to that of k-means clustering, apart from its soft approach.

### Assumptions
- The data consists of continuous variables.
- (Normally distributed data aids the clustering process).

### Input 
-------
#### Assignment Box 
- Variables: In this box, the variables are need to be considered in the clustering algorithm should be entered. 

#### Tables  
- Cluster means: Generates a table containing the cluster means for each feature variable.
- Cluster information: Displays the size of each cluster and the explained proportion of within-cluster heterogeneity. The latter is the cluster within sum of squares divided by its total over the various clusters. These outputs are shown by default. 
- Within sum of squares: Displays the within sum of squares of each cluster. This option is selected by default.
- Silhouette score: Displays the silhouette score of each cluster.
- Centroids: Displays the centroid per variable of each cluster, which represents the mean of a cluster.
- Between sum of squares: Notes the between sum of squares of the cluster model beneath the cluster information table.
- Total sum of squares: Notes the total sum of squares of the cluster model beneath the cluster information table.

#### Plots
- Elbow method: Generates a plot with the total within sum of squares on the y-axis and the number of clusters on the x-axis. This plot can be used for determining the optimal number of clusters. The plot shows three curves using AIC, BIC, and 'elbow method' optimization.
- Cluster means: For each feature variable, generates a plot showing the mean of each cluster and its 95% confidence interval.
- Cluster densities: For each feature variable, generates a plot showing the overlapping densities for the clusters.
- t-SNE cluster plot: Generates a t-SNE plot of the clustering output. t-SNE plots are used for visualizing high-dimensional data in a low-dimensional space of two dimensions aiming to illustrate the relative distances between data observations. The t-SNE two-dimensional space makes the axes uninterpretable. A t-SNE plot seeks to give an impression of the relative distances between observations and clusters. To recreate the same t-SNE plot across several clustering analyses you can set their seed to the same value, as the t-SNE algorithm uses random starting values.
- Legend: Sets a legend showing the cluster number for each observation. This option is set by default.
- Labels: Shows the clustering labels of the different observations.

#### Training Parameters 
#### Algorithmic Settings
- Max. iterations: Sets the maximum number of iterations. The maximum number of iterations reflects the number of possible instances the algorithm iterates for finding the optimal clustering solution. At default, this is set to 25.
- Fuzziness parameter: Is a scalar and controls the fuzziness of the clustering output. Meaning that if its value increases (> 1) the uncertainty of the memberships to the different clusters also increases. In other words, if the fuzziness parameter approaches 1, the result of the fuzzy clustering is similar to a hard-clustering method, and if the parameter increases, the clustering result becomes fuzzier.
- Scale features: Standardizes the continuous features in the dataset. Standardization ensures that values of features from different scales range into a specific similar scale. As a result, standardizing provides numerical stability. JASP uses the Z-score standardization of a mean of 0 and a standard deviation of 1. This option is selected by default.
- Set seed: Gives the option to set a seed for your analysis. Setting a seed will exclude random processes influencing an analysis. For example, setting a seed makes it possible to re-run analyses with the same outcomes.

#### Cluster Determination
- Fixed: Enables you to generate a fixed amount of clusters. This allows you to generate your own specified number of clusters, and thus, optimize manually.
- Optimized according to: Enables you to choose an optimization method. The options are AIC, BIC, and silhouette. The AIC uses the within sum of squares (within-cluster variation), the number of generated clusters and the number of dimensions for optimizing the clustering output. The BIC uses the within sum of squares (within-cluster variation), the number of generated clusters, the number of dimensions, and the sample size for optimizing the clustering output. The silhouette value uses the similarity of observations within a cluster and their dissimilarity to other clusters for optimizing the clustering output. BIC optimization is set as default.
- Max. clusters: Sets the maximum number of possible clusters to be generated. At default, this is set to 10.

#### Add Predicted Clusters to Data
Generates a new column in your dataset with the cluster labels of your cluster result. This gives you the option to inspect, classify, or predict the generated cluster labels.

### Output
-------

#### Fuzzy c-means Clustering Model Table
- The first column shows the number of generated clusters.
- N: The sample size.
- R<sup>2</sup>: Indicates the amount of variance explained by the model.
- AIC: The AIC value of the model. Lower values represent better clustering outputs.
- BIC: The BIC value of the model. Lower values represent better clustering outputs.
- Silhouette: The Silhouette value of the model. The silhouette value ranges from -1 to 1, where 1 represents a perfect score.

#### Fuzzy c-means Cluster Information
- Size: The size of each cluster.
- Within sum of squares: The within sum of squares of each cluster.
- Silhouette: The Silhouette value of each cluster.
- Centroids: The mean of each cluster per variable.
- Between sum of squares: The between sum of squares of the model noted underneath the table.
- Total sum of squares: The total sum of squares of the model noted underneath the table.

#### Evaluation Metrics Table
- Maximum diameter: The maximum cluster diameter in *euclidean* distance.
- Minimum separation: The minimum cluster separation in *euclidean* distance.
- Pearson's \u03B3: Correlation between distances and a 0-1-vector where 0 means same cluster, 1 means different clusters. 
- Dunn index: Minimum separation / maximum diameter. 
- Entropy: Entropy of the distribution of cluster memberships.
- Calinski-Harabasz index: The variance ratio criterion of the cluster memberships.

### References
-------
- James, G., Witten, D., Hastie, T., & Tibshirani, R. (2013). An introduction to statistical learning. Springer New York.
- Akaike, H. (1987). Factor analysis and AIC. Psychometrika, 52(3), 317–332.
- Tibshirani, R., Walther, G., & Hastie, T. (2001). Estimating the number of clusters in a data set via the gap statistic. Journal of the Royal Statistical Society: Series B (Statistical Methodology), 63(2), 411–423.
- Matthiesen, R. (Ed.). (2010). Bioinformatics methods in clinical research. Humana Press.
- Schwarz, G., et al. (1978). Estimating the dimension of a model. The annals of statistics, 6(2), 461–464.
- Izakian, H., & Abraham, A. (2011). Fuzzy c-means and fuzzy swarm for fuzzy clustering problem. Expert Systems with Applications, 38(3), 1835–1838.

### R-packages 
--- 
- cluster
- e1017
- Rtsne

### Example 
--- 
- For an example data set go to `Open` --> `Data Library` --> `Machine Learning` --> `Iris`.  

