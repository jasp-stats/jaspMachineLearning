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

mlClusteringKMeans <- function(jaspResults, dataset, options, ...) {

  # Preparatory work
  dataset <- .mlClusteringReadData(dataset, options)
  .mlClusteringErrorHandling(dataset, options, type = "kmeans")

  # Check if analysis is ready to run
  ready <- .mlClusteringReady(options)

  # Compute results and create the model summary table
  .mlClusteringTableSummary(dataset, options, jaspResults, ready, position = 1, type = "kmeans")

  # If the user wants to add the clusters to the data set
  .mlClusteringAddPredictionsToData(dataset, options, jaspResults, ready)

  # Create the cluster information table
  .mlClusteringTableInformation(options, jaspResults, ready, position = 2, type = "kmeans")

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
  .mlClusteringPlotTsne(dataset, options, jaspResults, ready, position = 8, type = "kmeans")
}

.kMeansClustering <- function(dataset, options, jaspResults, ready) {
  if (options[["modelOpt"]] == "validationManual") {
    if (options[["centers"]] == "means") {
      fit <- kmeans(dataset[, options[["predictors"]]], centers = options[["noOfClusters"]], iter.max = options[["noOfIterations"]], nstart = options[["noOfRandomSets"]], algorithm = options[["algorithm"]])
    } else if (options[["centers"]] == "medians") {
      fit <- Gmedian::kGmedian(dataset[, options[["predictors"]]], ncenters = options[["noOfClusters"]], nstart = options[["noOfIterations"]], nstartkmeans = options[["noOfRandomSets"]])
    } else if (options[["centers"]] == "medoids") {
      if (options[["algorithm"]] == "pam") {
        fit <- cluster::pam(dataset[, options[["predictors"]]], k = options[["noOfClusters"]], metric = options[["distance"]], nstart = options[["noOfRandomSets"]])
      } else {
        fit <- cluster::clara(dataset[, options[["predictors"]]], k = options[["noOfClusters"]], metric = options[["distance"]], samples = options[["noOfIterations"]])
      }
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
      if (options[["centers"]] == "means") {
        fit <- kmeans(dataset[, options[["predictors"]]], centers = i, iter.max = options[["noOfIterations"]], nstart = options[["noOfRandomSets"]], algorithm = options[["algorithm"]])
      } else if (options[["centers"]] == "medians") {
        fit <- Gmedian::kGmedian(dataset[, options[["predictors"]]], ncenters = i, nstart = options[["noOfIterations"]], nstartkmeans = options[["noOfRandomSets"]])
      } else if (options[["centers"]] == "medoids") {
        if (options[["algorithm"]] == "pam") {
          fit <- cluster::pam(dataset[, options[["predictors"]]], k = i, metric = options[["distance"]], nstart = options[["noOfRandomSets"]])
        } else {
          fit <- cluster::clara(dataset[, options[["predictors"]]], k = i, metric = options[["distance"]], samples = options[["noOfIterations"]])
        }
      }
	  predictions <- if (options[["centers"]] == "medoids") fit[["clustering"]] else fit[["cluster"]]
      silh <- summary(cluster::silhouette(predictions, dist(dataset[, options[["predictors"]]])))
      avgSilh[i - 1] <- silh[["avg.width"]]
	  centers <- if (options[["centers"]] == "medoids") fit[["medoids"]] else fit[["centers"]]
	  sumSquares <- .sumsqr(dataset[, options[["predictors"]]], centers, predictions)
      wssStore[i - 1] <- sumSquares[["tot.within.ss"]]
      aicStore[i - 1] <- sumSquares[["tot.within.ss"]] + 2 * ncol(centers) * nrow(centers)
      bicStore[i - 1] <- sumSquares[["tot.within.ss"]] + log(length(predictions)) * ncol(centers) * nrow(centers)
      progressbarTick()
    }
    clusters <- switch(options[["optimizationCriterion"]],
      "validationSilh" = clusterRange[which.max(avgSilh)],
      "validationAIC" = clusterRange[which.min(aicStore)],
      "validationBIC" = clusterRange[which.min(bicStore)]
    )
    if (options[["centers"]] == "means") {
      fit <- kmeans(dataset[, options[["predictors"]]], centers = clusters, iter.max = options[["noOfIterations"]], nstart = options[["noOfRandomSets"]], algorithm = options[["algorithm"]])
    } else if (options[["centers"]] == "medians") {
      fit <- Gmedian::kGmedian(dataset[, options[["predictors"]]], ncenters = clusters, nstart = options[["noOfIterations"]], nstartkmeans = options[["noOfRandomSets"]])
    } else if (options[["centers"]] == "medoids") {
      if (options[["algorithm"]] == "pam") {
        fit <- cluster::pam(dataset[, options[["predictors"]]], k = clusters, metric = options[["distance"]], nstart = options[["noOfRandomSets"]])
      } else {
        fit <- cluster::clara(dataset[, options[["predictors"]]], k = clusters, metric = options[["distance"]], samples = options[["noOfIterations"]])
      }
    }
  }
  predictions <- if (options[["centers"]] == "medoids") fit[["clustering"]] else fit[["cluster"]]
  centers <- if (options[["centers"]] == "medoids") fit[["medoids"]] else fit[["centers"]]
  sumSquares <- .sumsqr(dataset[, options[["predictors"]]], centers, predictions)
  silhouettes <- summary(cluster::silhouette(predictions, dist(dataset[, options[["predictors"]]])))
  size <- switch(options[["centers"]], "means" = fit[["size"]], "medians" = fit[["size"]][, 1], "medoids" = fit[["clusinfo"]][, 1])
  result <- list()
  result[["pred.values"]] <- predictions
  result[["clusters"]] <- clusters
  result[["N"]] <- nrow(dataset)
  result[["size"]] <- size
  result[["centroids"]] <- centers
  result[["WSS"]] <- sumSquares[["wss"]]
  result[["TSS"]] <- sumSquares[["tss"]]
  result[["BSS"]] <- sumSquares[["bss"]]
  result[["AIC"]] <- sumSquares[["tot.within.ss"]] + 2 * ncol(centers) * nrow(centers)
  result[["BIC"]] <- sumSquares[["tot.within.ss"]] + log(length(predictions)) * ncol(centers) * nrow(centers)
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
