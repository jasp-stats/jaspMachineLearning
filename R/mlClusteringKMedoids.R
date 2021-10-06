#
# Copyright (C) 2013-2021 University of Amsterdam
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

mlClusteringKMedoids <- function(jaspResults, dataset, options, ...) {

  # Preparatory work
  dataset <- .mlClusteringReadData(dataset, options)
  .mlClusteringErrorHandling(dataset, options, type = "kmedoids")

  # Check if analysis is ready to run
  ready <- .mlClusteringReady(options)

  # Compute results and create the model summary table
  .mlClusteringTableSummary(dataset, options, jaspResults, ready, position = 1, type = "kmedoids")

  # If the user wants to add the clusters to the data set
  .mlClusteringAddPredictionsToData(dataset, options, jaspResults, ready)

  # Create the cluster information table
  .mlClusteringTableInformation(options, jaspResults, ready, position = 2, type = "kmedoids")

  # Create the cluster means table
  .mlClusteringTableMeans(dataset, options, jaspResults, ready, position = 3)

  # Create the cluster evaluation metrics table
  .mlClusteringTableMetrics(dataset, options, jaspResults, ready, position = 4)

  # Create the within sum of squares plot
  .mlClusteringPlotElbow(dataset, options, jaspResults, ready, position = 5)

  # Create the cluster means plot
  .mlClusteringPlotMeans(dataset, options, jaspResults, ready, position = 6)

  # Create the cluster densities plot
  .mlClusteringPlotDensities(dataset, options, jaspResults, ready, position = 7)

  # Create the cluster plot
  .mlClusteringPlotTsne(dataset, options, jaspResults, ready, position = 8, type = "kmedoids")
}

.kMedoidsClustering <- function(dataset, options, jaspResults, ready) {
  if (options[["modelOpt"]] == "validationManual") {
    if (options[["algorithm"]] == "pam") {
      fit <- cluster::pam(dataset[, options[["predictors"]]], k = options[["noOfClusters"]], metric = options[["distance"]], nstart = options[["noOfRandomSets"]])
    } else {
      fit <- cluster::clara(dataset[, options[["predictors"]]], k = options[["noOfClusters"]], metric = options[["distance"]], samples = options[["noOfIterations"]])
    }
    clusters <- options[["noOfClusters"]]
  } else {
    avgSilh <- numeric(options[["maxClusters"]] - 1)
    wssStore <- numeric(options[["maxClusters"]] - 1)
    clusterRange <- 2:options[["maxClusters"]]
    aicStore <- numeric(options[["maxClusters"]] - 1)
    bicStore <- numeric(options[["maxClusters"]] - 1)
    startProgressbar(length(clusterRange))
    for (i in clusterRange) {
      if (options[["algorithm"]] == "pam") {
        fit <- cluster::pam(dataset[, options[["predictors"]]], k = i, metric = options[["distance"]], nstart = options[["noOfRandomSets"]])
      } else {
        fit <- cluster::clara(dataset[, options[["predictors"]]], k = i, metric = options[["distance"]], samples = options[["noOfIterations"]])
      }
      silh <- summary(cluster::silhouette(fit[["clustering"]], dist(dataset[, options[["predictors"]]])))
      avgSilh[i - 1] <- silh[["avg.width"]]
      sumSquares <- .sumsqr(dataset[, options[["predictors"]]], as.data.frame(as.matrix(fit[["medoids"]])), fit[["clustering"]])
      wssStore[i - 1] <- sumSquares[["wss"]]
      aicStore[i - 1] <- sumSquares[["tot.within.ss"]] + 2 * ncol(fit[["medoids"]]) * nrow(fit[["medoids"]])
      bicStore[i - 1] <- sumSquares[["tot.within.ss"]] + log(length(fit[["clustering"]])) * ncol(fit[["medoids"]]) * nrow(fit[["medoids"]])
      progressbarTick()
    }
    clusters <- switch(options[["optimizationCriterion"]],
      "validationSilh" = clusterRange[which.max(avgSilh)],
      "validationAIC" = clusterRange[which.min(aicStore)],
      "validationBIC" = clusterRange[which.min(bicStore)]
    )
    if (options[["algorithm"]] == "pam") {
      fit <- cluster::pam(dataset[, options[["predictors"]]], k = clusters, metric = options[["distance"]], nstart = options[["noOfRandomSets"]])
    } else {
      fit <- cluster::clara(dataset[, options[["predictors"]]], k = clusters, metric = options[["distance"]], samples = options[["noOfIterations"]])
    }
  }
  sumSquares <- .sumsqr(dataset[, options[["predictors"]]], as.data.frame(as.matrix(fit[["medoids"]])), fit[["clustering"]])
  silhouettes <- summary(cluster::silhouette(fit[["clustering"]], dist(dataset[, options[["predictors"]]])))
  result <- list()
  result[["pred.values"]] <- fit[["clustering"]]
  result[["clusters"]] <- clusters
  result[["N"]] <- nrow(dataset)
  result[["size"]] <- fit[["clusinfo"]][, 1]
  result[["centroids"]] <- fit[["medoids"]]
  result[["WSS"]] <- sumSquares[["wss"]]
  result[["TSS"]] <- sumSquares[["tss"]]
  result[["BSS"]] <- sumSquares[["bss"]]
  result[["AIC"]] <- sumSquares[["tot.within.ss"]] + 2 * ncol(fit[["medoids"]]) * nrow(fit[["medoids"]])
  result[["BIC"]] <- sumSquares[["tot.within.ss"]] + log(length(fit[["clustering"]])) * ncol(fit[["medoids"]]) * nrow(fit[["medoids"]])
  result[["Silh_score"]] <- silhouettes[["avg.width"]]
  result[["silh_scores"]] <- silhouettes[["clus.avg.widths"]]
  if (options[["modelOpt"]] != "validationManual") {
    result[["silhStore"]] <- avgSilh
    result[["aicStore"]] <- aicStore
    result[["bicStore"]] <- bicStore
    result[["wssStore"]] <- wssStore
  }
  return(result)
}
