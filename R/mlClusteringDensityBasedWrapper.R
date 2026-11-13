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

#' Density-Based Clustering
#'
#' Density-based clustering is a soft clustering method where clusters are constructed as maximal sets of points that are connected to points whose density exceeds some threshold. The density is produced by the concept that for each point within a cluster, the neighborhood within a given radius has to contain at least a minimum amount of points, that results in the density of that neighborhood to exceed a certain threshold. A density-based cluster is recognized by points having a higher density than points outside of the cluster. The set of all high-density points is called the density level. The points that do not exceed a density level are identified as outliers. The density level influences the amount of generated clusters.
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
#' @param matrixPlot, Creates a *n* x *n* plot that visualizes to which cluster every observation belongs according to the current model.
#'    Defaults to \code{FALSE}.
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
mlClusteringDensityBased <- function(
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
          distance = "normalDensities",
          epsilonNeighborhoodSize = 2,
          kDistancePlot = FALSE,
          matrixPlot = FALSE,
          minCorePoints = 5,
          modelOptimization = "manual",
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

   optionsWithFormula <- c("distance", "predictors")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   # Options not given in the call get their default from the form, which can depend on the other options.
   options <- options[names(options) %in% names(match.call())[-1L]]
   return(jaspBase::runWrappedAnalysis("jaspMachineLearning", "mlClusteringDensityBased", "mlClusteringDensityBased.qml", options, version, TRUE))
}