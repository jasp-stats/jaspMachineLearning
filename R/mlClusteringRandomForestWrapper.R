#
# Copyright (C) 2013-2025 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

# This is a generated file. Don't change it!

#' Random Forest Clustering
#'
#' Random Forest clustering is a hard partitioning algorithm which aims to partition data into several clusters, where each observation belongs to only one group. This clustering method uses the Random Forest algorithm in an unsupervised way, with the outcome variable 'y' set to NULL. The Random Forest algorithm generates a proximity matrix which gives an estimate of the distance between observations based on the frequency of observations ending up in the same leaf node.
#' ### Assumptions
#' - The data consists of continuous variables.
#' - (Normally distributed data aids the clustering process).
#'
#' @param addPredictions, Adds a column with the predicted cluster membership (clustering) or predicted values (regression/classification) to the dataset.
#'    Defaults to \code{FALSE}.
#' @param addProbabilities, In classification analyses, append the predicted probabilities for each class to the data. For neural networks, this option provides the output of the final layer.
#'    Defaults to \code{FALSE}.
#' @param addSoftMemberships, Adds one column per cluster with the soft membership or posterior probability of each observation. The column name is used as a prefix (e.g., membership_1, membership_2).
#'    Defaults to \code{FALSE}.
#' @param addTsneCoordinates, Adds the two-dimensional t-SNE embedding (dimension 1 and 2) for each observation. These are the same coordinates used in the t-SNE cluster plot. The column name is used as a prefix (e.g., tsne_1, tsne_2).
#'    Defaults to \code{FALSE}.
#' @param clusterDensityPlot, For each feature variable, generates a plot showing the overlapping densities for the clusters.
#'    Defaults to \code{FALSE}.
#' @param clusterDensityPlotSingleFigure, Group the density plots per feature into a single figure.
#'    Defaults to \code{TRUE}.
#' @param clusterMeanPlot, Creates a plot that visualizes and compares the mean of the feature variables in each cluster.
#'    Defaults to \code{FALSE}.
#' @param clusterMeanPlotBarPlot, Transform the cluster mean figure into a barplot.
#'    Defaults to \code{TRUE}.
#' @param clusterMeanPlotSingleFigure, Group the plots per feature into a single figure.
#'    Defaults to \code{TRUE}.
#' @param elbowMethodPlot, Generates a plot with the total within sum of squares on the y-axis and the number of clusters on the x-axis. This plot can be used for determining the optimal number of clusters. The plot shows three curves using AIC, BIC, and 'elbow method' optimization.
#'    Defaults to \code{FALSE}.
#' @param featureImportancePermutations, Sets the number of permutations on which the mean dropout loss is based.
#' @param featureImportanceTable, Shows the available feature importance metrics for the fitted model.
#'    Defaults to \code{FALSE}.
#' @param manualNumberOfClusters, The number of clusters to be fitted.
#' @param matrixPlot, Creates a *n* x *n* plot that visualizes to which cluster every observation belongs according to the current model.
#'    Defaults to \code{FALSE}.
#' @param maxNumberOfClusters, Sets the maximum number of possible clusters to be generated. At default, this is set to 10.
#' @param modelOptimization, Choose how to determine the number of clusters in the model.
#' \itemize{
#'   \item \code{"manual"}: Enables you to generate a fixed amount of clusters. This allows you to generate your own specified number of clusters, and thus, optimize manually.
#'   \item \code{"optimized"}: Enables you to choose an optimization method. BIC optimization is set as default.
#' }
#' @param modelOptimizationMethod, The method of optimization. The options are AIC, BIC, and silhouette. The AIC uses the within sum of squares (within-cluster variation), the number of generated clusters and the number of dimensions for optimizing the clustering output. The BIC uses the within sum of squares (within-cluster variation), the number of generated clusters, the number of dimensions, and the sample size for optimizing the clustering output. The silhouette value uses the similarity of observations within a cluster and their dissimilarity to other clusters for optimizing the clustering output.
#' @param predictionsColumn, The column name for the predicted values.
#' @param predictors, In this box, the variables are need to be considered by the clustering algorithm should be entered.
#' @param saveModel, When clicked, the model is exported to the specified file path.
#'    Defaults to \code{FALSE}.
#' @param savePath, The file path for the saved model.
#' @param scaleVariables, Standardizes the continuous features in the dataset. Standardization ensures that values of features from different scales range into a specific similar scale. As a result, standardizing provides numerical stability. JASP uses the Z-score standardization of a mean of 0 and a standard deviation of 1. This option is selected by default.
#'    Defaults to \code{TRUE}.
#' @param seed, The value of the seed.
#' @param setSeed, Gives the option to set a seed for your analysis. Setting a seed will exclude random processes influencing an analysis. For example, setting a seed makes it possible to re-run analyses with the same data splits.
#'    Defaults to \code{FALSE}.
#' @param softMembershipsColumn, Prefix for the soft membership columns. One column is created per cluster.
#' @param tableClusterInformation, Displays the size of each cluster and the explained proportion of within-cluster heterogeneity. The latter is the cluster within sum of squares divided by its total over the various clusters. These outputs are shown by default.
#'    Defaults to \code{TRUE}.
#' @param tableClusterInformationBetweenSumOfSquares, Adds a note with the between sum of squares of the cluster model to the table.
#'    Defaults to \code{FALSE}.
#' @param tableClusterInformationCentroids, Adds a row with the center per feature of each cluster to the table. The center can be the mean, median or mode depending on the clustering algorithm.
#'    Defaults to \code{FALSE}.
#' @param tableClusterInformationSilhouetteScore, Adds a row with the silhouette score of each cluster to the table.
#'    Defaults to \code{FALSE}.
#' @param tableClusterInformationTotalSumOfSquares, Adds a note with the total sum of squares of the cluster model to the table.
#'    Defaults to \code{FALSE}.
#' @param tableClusterInformationWithinSumOfSquares, Adds a row with the within sum of squares of each cluster to the table. This option is selected by default.
#'    Defaults to \code{TRUE}.
#' @param tableClusterMeans, Shows a table containing the cluster means for each feature variable.
#'    Defaults to \code{FALSE}.
#' @param tsneClusterPlot, Generates a t-SNE plot of the clustering output. t-SNE plots are used for visualizing high-dimensional data in a low-dimensional space of two dimensions aiming to illustrate the relative distances between data observations. The t-SNE two-dimensional space makes the axes uninterpretable. A t-SNE plot seeks to give an impression of the relative distances between observations and clusters. To recreate the same t-SNE plot across several clustering analyses you can set their seed to the same value, as the t-SNE algorithm uses random starting values.
#'    Defaults to \code{FALSE}.
#' @param tsneClusterPlotLabels, Add the row numbers of the observations in the data set as labels to the plot.
#'    Defaults to \code{FALSE}.
#' @param tsneClusterPlotLegend, Show a legend next to the figure.
#'    Defaults to \code{TRUE}.
#' @param tsneCoordinatesColumn, Prefix for the t-SNE coordinate columns (suffixes _1 and _2 are added).
#' @param validationMeasures, Displays available model performance metrics. For regression, these metrics include mean squared error (MSE), root mean squared error (RMSE), R<sup>2</sup> and more. For classification, these metrics include precision, recall, the F1-score, support, AUC (area under the ROC curve) and more. For clustering, these metrics include entropy, Dunn index and more.
#'    Defaults to \code{FALSE}.
mlClusteringRandomForest <- function(
          data = NULL,
          version = "1",
          addEllips = FALSE,
          addPredictions = FALSE,
          addProbabilities = FALSE,
          addSoftMemberships = FALSE,
          addTsneCoordinates = FALSE,
          clusterDensityPlot = FALSE,
          clusterDensityPlotSingleFigure = TRUE,
          clusterMeanPlot = FALSE,
          clusterMeanPlotBarPlot = TRUE,
          clusterMeanPlotSingleFigure = TRUE,
          elbowMethodPlot = FALSE,
          featureImportancePermutations = 50,
          featureImportanceTable = FALSE,
          manualNumberOfClusters = 3,
          matrixPlot = FALSE,
          maxNumberOfClusters = 10,
          modelOptimization = "optimized",
          modelOptimizationMethod = "bic",
          numberOfTrees = 1000,
          plotHeight = 320,
          plotWidth = 480,
          predictionsColumn = "",
          predictors = list(types = list(), value = list()),
          saveModel = FALSE,
          savePath = "",
          scaleVariables = TRUE,
          seed = 1,
          setSeed = FALSE,
          softMembershipsColumn = "",
          tableClusterInformation = TRUE,
          tableClusterInformationBetweenSumOfSquares = FALSE,
          tableClusterInformationCentroids = FALSE,
          tableClusterInformationSilhouetteScore = FALSE,
          tableClusterInformationTotalSumOfSquares = FALSE,
          tableClusterInformationWithinSumOfSquares = TRUE,
          tableClusterMeans = FALSE,
          tsneClusterPlot = FALSE,
          tsneClusterPlotLabels = FALSE,
          tsneClusterPlotLegend = TRUE,
          tsneCoordinatesColumn = "",
          validationMeasures = FALSE) {

   options <- mget(names(formals(sys.function())), envir = environment(), inherits = FALSE)
   options[["data"]] <- NULL
   options[["version"]] <- NULL


   if (!jaspBase::jaspResultsCalledFromJasp() && !is.null(data)) {
      jaspBase::storeDataSet(data)
   }

   optionsWithFormula <- c("modelOptimizationMethod", "predictors")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   # Options not given in the call get their default from the form, which can depend on the other options.
   options <- options[names(options) %in% names(match.call())[-1L]]
   return(jaspBase::runWrappedAnalysis("jaspMachineLearning", "mlClusteringRandomForest", "mlClusteringRandomForest.qml", options, version, TRUE))
}