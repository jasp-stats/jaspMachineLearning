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

mlClusteringModelBased <- function(jaspResults, dataset, options, ...) {
  
  # Preparatory work
  dataset <- .mlClusteringReadData(dataset, options)
  .mlClusteringErrorHandling(dataset, options, type = "modelbased")

  # Check if analysis is ready to run
  ready <- .mlClusteringReady(options)

  # Compute results and create the model summary table
  .mlClusteringTableSummary(dataset, options, jaspResults, ready, position = 1, type = "modelbased")

  # If the user wants to add the clusters to the data set
  .mlClusteringAddPredictionsToData(dataset, options, jaspResults, ready)

  # Create the cluster information table
  .mlClusteringTableInformation(options, jaspResults, ready, position = 2, type = "modelbased")

  # Create the cluster evaluation metrics table
  .mlClusteringTableMetrics(dataset, options, jaspResults, ready, position = 3)

  # Create the cluster means table
  .mlClusteringTableMeans(dataset, options, jaspResults, ready, position = 4)

  # Create the within sum of squares plot
  .mlClusteringPlotElbow(dataset, options, jaspResults, ready, position = 5)

  # Create the cluster plot
  .mlClusteringPlotTsne(dataset, options, jaspResults, ready, position = 6, type = "modelbased")

  # Create the matrix plot
  .mlClusteringMatrixPlot(dataset, options, jaspResults, ready, position = 7)

  # Create the cluster means plot
  .mlClusteringPlotMeans(dataset, options, jaspResults, ready, position = 8)

  # Create the cluster densities plot
  .mlClusteringPlotDensities(dataset, options, jaspResults, ready, position = 9)
}

.modelBasedClustering <- function(dataset, options, jaspResults, ready) {
  if (options[["modelOptimization"]] == "manual") {
    fit <- mclust::Mclust(dataset[, options[["predictors"]]], G = options[["manualNumberOfClusters"]],
                          modelNames = if (options[["modelName"]] == "auto") NULL else options[["modelName"]],
                          control = mclust::emControl(itmax = options[["maxNumberIterations"]]))
    clusters <- options[["manualNumberOfClusters"]]
  } else {
    avgSilh <- numeric(options[["maxNumberOfClusters"]] - 1)
    wssStore <- numeric(options[["maxNumberOfClusters"]] - 1)
    clusterRange <- 2:options[["maxNumberOfClusters"]]
    aicStore <- numeric(options[["maxNumberOfClusters"]] - 1)
    bicStore <- numeric(options[["maxNumberOfClusters"]] - 1)
    startProgressbar(length(clusterRange))
    for (i in clusterRange) {
      fit <- mclust::Mclust(dataset[, options[["predictors"]]], G = i,
                            control = mclust::emControl(itmax = options[["maxNumberIterations"]]))
      silh <- summary(cluster::silhouette(fit[["classification"]], .mlClusteringCalculateDistances(dataset[, options[["predictors"]]])))
      avgSilh[i - 1] <- silh[["avg.width"]]
      sumSquares <- .sumsqr(dataset[, options[["predictors"]]], t(fit[["parameters"]]$mean), fit[["classification"]])
      wssStore[i - 1] <- sumSquares[["tot.within.ss"]]
      aicStore[i - 1] <- sumSquares[["tot.within.ss"]] + 2 * ncol(t(fit[["parameters"]]$mean)) * nrow(t(fit[["parameters"]]$mean))
      bicStore[i - 1] <- sumSquares[["tot.within.ss"]] + log(length(t(fit[["parameters"]]$mean))) * ncol(t(fit[["parameters"]]$mean)) * nrow(t(fit[["parameters"]]$mean))
      progressbarTick()
    }
    clusters <- switch(options[["modelOptimizationMethod"]],
      "silhouette" = clusterRange[which.max(avgSilh)],
      "aic" = clusterRange[which.min(aicStore)],
      "bic" = clusterRange[which.min(bicStore)]
    )
    fit <- mclust::Mclust(dataset[, options[["predictors"]]], G = clusters,
                          control = mclust::emControl(itmax = options[["maxNumberIterations"]]))
  }
  sumSquares <- .sumsqr(dataset[, options[["predictors"]]], t(fit[["parameters"]]$mean), fit[["classification"]])
  silhouettes <- summary(cluster::silhouette(fit[["classification"]], .mlClusteringCalculateDistances(dataset[, options[["predictors"]]])))
  result <- list()
  result[["pred.values"]] <- fit[["classification"]]
  result[["clusters"]] <- clusters
  result[["N"]] <- nrow(dataset)
  result[["size"]] <- as.numeric(table(fit[["classification"]]))
  result[["centroids"]] <- t(fit[["parameters"]]$mean)
  result[["WSS"]] <- sumSquares[["wss"]]
  result[["TSS"]] <- sumSquares[["tss"]]
  result[["BSS"]] <- sumSquares[["bss"]]
  result[["AIC"]] <- sumSquares[["tot.within.ss"]] + 2 * ncol(t(fit[["parameters"]]$mean)) * nrow(t(fit[["parameters"]]$mean))
  result[["BIC"]] <- sumSquares[["tot.within.ss"]] + log(length(t(fit[["classification"]]))) * ncol(t(fit[["parameters"]]$mean)) * nrow(t(fit[["parameters"]]$mean))
  result[["Silh_score"]] <- silhouettes[["avg.width"]]
  result[["silh_scores"]] <- silhouettes[["clus.avg.widths"]]
  if (options[["modelOptimization"]] != "manual") {
    result[["silhStore"]] <- avgSilh
    result[["aicStore"]] <- aicStore
    result[["bicStore"]] <- bicStore
    result[["wssStore"]] <- wssStore
  }
  result[["parameters"]] <- fit[["parameters"]]
  result[["modelName"]] <- fit[["modelName"]]
  return(result)
}

emControl <- mclust::emControl
mclust.options <- mclust::mclust.options
mclustBIC <- mclust::mclustBIC
