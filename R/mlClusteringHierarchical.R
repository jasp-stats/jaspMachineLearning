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

mlClusteringHierarchical <- function(jaspResults, dataset, options, ...) {

  # Preparatory work
  dataset <- .mlClusteringReadData(dataset, options)
  .mlClusteringErrorHandling(dataset, options, type = "hierarchical")

  # Check if analysis is ready to run
  ready <- .mlClusteringReady(options)

  # Compute results and create the model summary table
  .mlClusteringTableSummary(dataset, options, jaspResults, ready, position = 1, type = "hierarchical")

  # If the user wants to add the clusters to the data set
  .mlClusteringAddPredictionsToData(dataset, options, jaspResults, ready)

  # Create the cluster information table
  .mlClusteringTableInformation(options, jaspResults, ready, position = 2, type = "hierarchical")

  # Create the cluster means table
  .mlClusteringTableMeans(dataset, options, jaspResults, ready, position = 3)

  # Create the cluster evaluation metrics table
  .mlClusteringTableMetrics(dataset, options, jaspResults, ready, position = 4)

  # Create the within sum of squares plot
  .mlClusteringPlotElbow(dataset, options, jaspResults, ready, position = 5)

  # Create dendrogram
  .mlClusteringHierarchicalDendogram(dataset, options, jaspResults, ready, position = 6)

  # Create the cluster means plot
  .mlClusteringPlotMeans(dataset, options, jaspResults, ready, position = 7)

  # Create the cluster densities plot
  .mlClusteringPlotDensities(dataset, options, jaspResults, ready, position = 8)

  # Create the cluster plot
  .mlClusteringPlotTsne(dataset, options, jaspResults, ready, position = 9, type = "hierarchical")
}

.hierarchicalClustering <- function(dataset, options, jaspResults) {
  if (options[["linkage"]] == "wardD") {
    linkage <- "ward.D"
  } else if (options[["linkage"]] == "wardD2") {
    linkage <- "ward.D"
  } else {
    linkage <- options[["linkage"]]
  }
  if (options[["clusterDeterminationMethod"]] == "manual") {
    if (options[["distance"]] == "pearsonCorrelation") {
      distances <- as.dist(1 - cor(t(dataset[, options[["predictors"]]]), method = "pearson"))
      distances[is.na(distances)] <- 1 # We impute the missing correlations with a 1, as 1 - 1 = 0
      fit <- cutree(hclust(distances, method = linkage), k = options[["clusterDeterminationMethodManualNumberOfClusters"]])
    } else {
      distances <- .mlClusteringCalculateDistances(dataset[, options[["predictors"]]])
      fit <- cutree(hclust(distances, method = linkage), k = options[["clusterDeterminationMethodManualNumberOfClusters"]])
    }
    clusters <- options[["clusterDeterminationMethodManualNumberOfClusters"]]
  } else {
    avgSilh <- numeric(options[["clusterDeterminationMethodOptimizedMaxNumberOfClusters"]] - 1)
    wssStore <- numeric(options[["clusterDeterminationMethodOptimizedMaxNumberOfClusters"]] - 1)
    clusterRange <- 2:options[["clusterDeterminationMethodOptimizedMaxNumberOfClusters"]]
    aicStore <- numeric(options[["clusterDeterminationMethodOptimizedMaxNumberOfClusters"]] - 1)
    bicStore <- numeric(options[["clusterDeterminationMethodOptimizedMaxNumberOfClusters"]] - 1)
    startProgressbar(length(clusterRange))
    for (i in clusterRange) {
      if (options[["distance"]] == "pearsonCorrelation") {
        distances <- as.dist(1 - cor(t(dataset[, options[["predictors"]]]), method = "pearson"))
        distances[is.na(distances)] <- 1 # We impute the missing correlations with a 1, as 1 - 1 = 0
        fit <- cutree(hclust(distances, method = linkage), k = i)
      } else {
        distances <- .mlClusteringCalculateDistances(dataset[, options[["predictors"]]])
        fit <- cutree(hclust(distances, method = linkage), k = i)
      }
      silh <- summary(cluster::silhouette(fit, distances))
      avgSilh[i - 1] <- silh[["avg.width"]]
      m <- dim(as.data.frame(dataset[, options[["predictors"]]]))[2]
      wss <- numeric(length(table(fit)))
      for (j in 1:length(table(fit))) {
        wss[j] <- if (m == 1) .ss(dataset[, options[["predictors"]]][fit == j]) else .ss(dataset[, options[["predictors"]]][fit == j, ])
      }
      wssStore[i - 1] <- sum(wss)
      aicStore[i - 1] <- sum(wss) + 2 * m * length(table(fit))
      bicStore[i - 1] <- sum(wss) + log(length(fit)) * m * length(table(fit))
      progressbarTick()
    }
    clusters <- switch(options[["clusterDeterminationMethodOptimizedTypeOptimization"]],
      "silhouette" = clusterRange[which.max(avgSilh)],
      "aic" = clusterRange[which.min(aicStore)],
      "bic" = clusterRange[which.min(bicStore)]
    )
    fit <- cutree(hclust(distances, method = linkage), k = clusters)
  }
  size <- as.data.frame(table(fit))[, 2]
  m <- dim(as.data.frame(dataset[, options[["predictors"]]]))[2]
  wss <- numeric(length(table(fit)))
  for (j in 1:length(table(fit))) {
    wss[j] <- if (m == 1) .ss(dataset[, options[["predictors"]]][fit == j]) else .ss(dataset[, options[["predictors"]]][fit == j, ])
  }
  silhouettes <- summary(cluster::silhouette(fit, distances))
  result <- list()
  result[["pred.values"]] <- fit
  result[["clusters"]] <- clusters
  result[["N"]] <- nrow(dataset)
  result[["size"]] <- size
  result[["WSS"]] <- wss
  result[["TSS"]] <- .tss(.mlClusteringCalculateDistances(dataset[, options[["predictors"]]]))
  result[["BSS"]] <- result[["TSS"]] - sum(result[["WSS"]])
  result[["AIC"]] <- sum(wss) + 2 * m * length(table(fit))
  result[["BIC"]] <- sum(wss) + log(length(fit)) * m * length(table(fit))
  result[["Silh_score"]] <- silhouettes[["avg.width"]]
  result[["silh_scores"]] <- silhouettes[["clus.avg.widths"]]
  if (options[["clusterDeterminationMethod"]] != "manual") {
    result[["silhStore"]] <- avgSilh
    result[["aicStore"]] <- aicStore
    result[["bicStore"]] <- bicStore
    result[["wssStore"]] <- wssStore
  }
  return(result)
}

.mlClusteringHierarchicalDendogram <- function(dataset, options, jaspResults, ready, position) {
  if (!is.null(jaspResults[["dendrogram"]]) || !options[["dendrogram"]]) {
    return()
  }
   if (options[["linkage"]] == "wardD") {
    linkage <- "ward.D"
  } else if (options[["linkage"]] == "wardD2") {
    linkage <- "ward.D"
  } else {
    linkage <- options[["linkage"]]
  }
  plot <- createJaspPlot(plot = NULL, title = gettext("Dendrogram"), width = 400, height = 300)
  plot$position <- position
  plot$dependOn(options = c(
    "predictors", "clusterDeterminationMethodManualNumberOfClusters", "noOfRandomSets", "algorithm", "epsilonNeighborhoodSize", "minCorePoints", "distance",
    "maxNumberIterations", "clusterDeterminationMethod", "ready", "randomSeedValue", "tsneClusterPlot", "clusterDeterminationMethodOptimizedMaxNumberOfClusters", "equalSdScale", "randomSeed",
    "linkage", "fuzzinessParameter", "dendrogram", "clusterDeterminationMethodOptimizedTypeOptimization"
  ))
  jaspResults[["dendrogram"]] <- plot
  if (!ready) {
    return()
  }
  if (options[["randomSeed"]]) {
    set.seed(options[["randomSeedValue"]])
  }
  unique.rows <- which(!duplicated(dataset[, options[["predictors"]]]))
  data <- dataset[unique.rows, options[["predictors"]]]
  if (options[["distance"]] == "pearsonCorrelation") {
    hc <- hclust(as.dist(1 - cor(t(data), method = "pearson")), method = linkage)
  } else {
    hc <- hclust(.mlClusteringCalculateDistances(data), method = linkage)
  }
  p <- ggdendro::ggdendrogram(hc) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw() +
    ggdendro::theme_dendro()
  plot$plotObject <- p
}
