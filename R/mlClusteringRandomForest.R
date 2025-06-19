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

mlClusteringRandomForest <- function(jaspResults, dataset, options, ...) {

  # Preparatory work
  dataset <- .mlClusteringReadData(dataset, options)
  .mlClusteringErrorHandling(dataset, options, type = "randomForest")

  # Check if analysis is ready to run
  ready <- .mlClusteringReady(options)

  # Compute results and create the model summary table
  .mlClusteringTableSummary(dataset, options, jaspResults, ready, position = 1, type = "randomForest")

  # If the user wants to add the clusters to the data set
  .mlClusteringAddPredictionsToData(dataset, options, jaspResults, ready)

  # Create the cluster information table
  .mlClusteringTableInformation(options, jaspResults, ready, position = 2, type = "randomForest")

  # Create the cluster evaluation metrics table
  .mlClusteringTableMetrics(dataset, options, jaspResults, ready, position = 3)

  # Create the cluster means table
  .mlClusteringTableMeans(dataset, options, jaspResults, ready, position = 4)

  # Create the variable importance table
  .mlClusteringRandomForestTableFeatureImportance(options, jaspResults, ready, position = 5)

  # Create the within sum of squares plot
  .mlClusteringPlotElbow(dataset, options, jaspResults, ready, position = 6)

  # Create the cluster plot
  .mlClusteringPlotTsne(dataset, options, jaspResults, ready, position = 7, type = "randomForest")

  # Create the matrix plot
  .mlClusteringMatrixPlot(dataset, options, jaspResults, ready, position = 8, type = "randomForest")

  # Create the cluster means plot
  .mlClusteringPlotMeans(dataset, options, jaspResults, ready, position = 9, type = "randomForest")

  # Create the cluster densities plot
  .mlClusteringPlotDensities(dataset, options, jaspResults, ready, position = 10, type = "randomForest")
}

.randomForestClustering <- function(dataset, options, jaspResults) {
  if (options[["modelOptimization"]] == "manual") {
    fit <- randomForest::randomForest(
      x = dataset[, options[["predictors"]]],
      y = NULL,
      ntree = options[["numberOfTrees"]],
      proximity = TRUE,
      oob.prox = TRUE
    )
    clusters <- options[["manualNumberOfClusters"]]
  } else {
    avgSilh <- numeric(options[["maxNumberOfClusters"]] - 1)
    wssStore <- numeric(options[["maxNumberOfClusters"]] - 1)
    clusterRange <- 2:options[["maxNumberOfClusters"]]
    aicStore <- numeric(options[["maxNumberOfClusters"]] - 1)
    bicStore <- numeric(options[["maxNumberOfClusters"]] - 1)
    startProgressbar(length(clusterRange))
    fit <- randomForest::randomForest(
      x = dataset[, options[["predictors"]]],
      y = NULL,
      ntree = options[["numberOfTrees"]],
      proximity = TRUE,
      oob.prox = TRUE
    )
    hfit <- hclust(as.dist(1 - fit$proximity), method = "ward.D2")
    for (i in clusterRange) {
      predictions <- cutree(hfit, k = i)
      silh <- summary(cluster::silhouette(predictions, .mlClusteringCalculateDistances(dataset[, options[["predictors"]]])))
      avgSilh[i - 1] <- silh[["avg.width"]]
      m <- dim(as.data.frame(dataset[, options[["predictors"]]]))[2]
      wssTmp <- numeric(i)
      for (j in 1:i) {
        wssTmp[j] <- if (m == 1) .ss(dataset[, options[["predictors"]]][predictions == j]) else .ss(dataset[, options[["predictors"]]][predictions == j, ])
      }
      wssStore[i - 1] <- sum(wssTmp)
      aicStore[i - 1] <- sum(wssTmp) + 2 * m * i
      bicStore[i - 1] <- sum(wssTmp) + log(length(predictions)) * m * i
      progressbarTick()
    }
    clusters <- switch(options[["modelOptimizationMethod"]],
      "silhouette" = clusterRange[which.max(avgSilh)],
      "aic" = clusterRange[which.min(aicStore)],
      "bic" = clusterRange[which.min(bicStore)]
    )
    fit <- randomForest::randomForest(
      x = dataset[, options[["predictors"]]],
      y = NULL,
      ntree = options[["numberOfTrees"]],
      proximity = TRUE,
      oob.prox = TRUE
    )
  }
  hfit <- hclust(as.dist(1 - fit$proximity), method = "ward.D2")
  predictions <- cutree(hfit, k = clusters)
  m <- dim(as.data.frame(dataset[, options[["predictors"]]]))[2]
  wss <- numeric(clusters)
  for (i in 1:clusters) {
    wss[i] <- if (m == 1) .ss(dataset[, options[["predictors"]]][predictions == i]) else .ss(dataset[, options[["predictors"]]][predictions == i, ])
  }
  silhouettes <- summary(cluster::silhouette(predictions, .mlClusteringCalculateDistances(dataset[, options[["predictors"]]])))
  result <- list()
  result[["pred.values"]] <- predictions
  result[["clusters"]] <- clusters
  result[["N"]] <- nrow(dataset)
  result[["size"]] <- as.numeric(table(predictions))
  result[["WSS"]] <- wss
  result[["TSS"]] <- .tss(.mlClusteringCalculateDistances(dataset[, options[["predictors"]]]))
  result[["BSS"]] <- result[["TSS"]] - sum(result[["WSS"]])
  result[["AIC"]] <- sum(wss) + 2 * m * clusters
  result[["BIC"]] <- sum(wss) + log(length(predictions)) * m * clusters
  result[["Silh_score"]] <- silhouettes[["avg.width"]]
  result[["silh_scores"]] <- silhouettes[["clus.avg.widths"]]
  result[["fit"]] <- fit
  if (options[["modelOptimization"]] != "manual") {
    result[["silhStore"]] <- avgSilh
    result[["aicStore"]] <- aicStore
    result[["bicStore"]] <- bicStore
    result[["wssStore"]] <- wssStore
  }
  return(result)
}

.mlClusteringRandomForestTableFeatureImportance <- function(options, jaspResults, ready, position) {
  if (!is.null(jaspResults[["importanceTable"]]) || !options[["featureImportanceTable"]]) {
    return()
  }
  table <- createJaspTable(title = gettext("Feature Importance"))
  table$position <- position
  table$dependOn(options = c(.mlClusteringDependencies(), "featureImportanceTable"))
  table$addColumnInfo(name = "variable", title = "", type = "string")
  table$addColumnInfo(name = "measure", title = gettext("Mean decrease in Gini Index"), type = "number")
  jaspResults[["importanceTable"]] <- table
  if (!ready) {
    return()
  }
  state <- jaspResults[["clusterResult"]]$object
  fit <- state[["fit"]]
  varImp <- fit[["importance"]]
  ord <- order(varImp, decreasing = TRUE)
  name <- rownames(varImp)[ord]
  values <- as.numeric(varImp[ord])
  row <- data.frame(variable = name, measure = values)
  table$addRows(row)
}
