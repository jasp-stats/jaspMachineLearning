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

  # Create the cluster variances table
  .mlClusteringTableModelParameters(dataset, options, jaspResults, ready, position = 5)

  # Create the within sum of squares plot
  .mlClusteringPlotElbow(dataset, options, jaspResults, ready, position = 6)

  # Create the cluster plot
  .mlClusteringPlotTsne(dataset, options, jaspResults, ready, position = 7, type = "modelbased")

  # Create the matrix plot
  .mlClusteringMatrixPlot(dataset, options, jaspResults, ready, position = 8)

  # Create the cluster means plot
  .mlClusteringPlotMeans(dataset, options, jaspResults, ready, position = 9)

  # Create the cluster densities plot
  .mlClusteringPlotDensities(dataset, options, jaspResults, ready, position = 10)
}

emControl <- mclust::emControl
mclust.options <- mclust::mclust.options
mclustBIC <- mclust::mclustBIC

.modelBasedClustering <- function(dataset, options, jaspResults, ready) {
  if (options[["modelOptimization"]] == "manual") {
    fit <- mclust::Mclust(dataset[, options[["predictors"]]], G = options[["manualNumberOfClusters"]],
                          modelNames = if (options[["modelName"]] == "auto") NULL else options[["modelName"]],
                          control = mclust::emControl(itmax = options[["maxNumberIterations"]]), verbose = FALSE)
    if (is.null(fit)) {
      .quitAnalysis(gettextf("The %1$s-component %2$s model could not be fitted, try a different model or a different number of clusters.", options[["manualNumberOfClusters"]], options[["modelName"]]))
    }
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
                            modelNames = if (options[["modelName"]] == "auto") NULL else options[["modelName"]],
                            control = mclust::emControl(itmax = options[["maxNumberIterations"]]), verbose = FALSE)
      if (is.null(fit)) {
        avgSilh[i - 1] <- NA
        wssStore[i - 1] <- NA
        aicStore[i - 1] <- NA
        bicStore[i - 1] <- NA
      } else {
        silh <- summary(cluster::silhouette(fit[["classification"]], .mlClusteringCalculateDistances(dataset[, options[["predictors"]]])))
        avgSilh[i - 1] <- silh[["avg.width"]]
        sumSquares <- .sumsqr(dataset[, options[["predictors"]]], t(fit[["parameters"]]$mean), fit[["classification"]])
        wssStore[i - 1] <- sumSquares[["tot.within.ss"]]
        aicStore[i - 1] <- sumSquares[["tot.within.ss"]] + 2 * ncol(t(fit[["parameters"]]$mean)) * nrow(t(fit[["parameters"]]$mean))
        bicStore[i - 1] <- sumSquares[["tot.within.ss"]] + log(length(t(fit[["parameters"]]$mean))) * ncol(t(fit[["parameters"]]$mean)) * nrow(t(fit[["parameters"]]$mean))
      }
      progressbarTick()
    }
    clusters <- switch(options[["modelOptimizationMethod"]],
      "silhouette" = clusterRange[which.max(avgSilh)],
      "aic" = clusterRange[which.min(aicStore)],
      "bic" = clusterRange[which.min(bicStore)]
    )
    fit <- mclust::Mclust(dataset[, options[["predictors"]]], G = clusters,
                          modelNames = if (options[["modelName"]] == "auto") NULL else options[["modelName"]],
                          control = mclust::emControl(itmax = options[["maxNumberIterations"]]), verbose = FALSE)
  }
  sumSquares <- .sumsqr(dataset[, options[["predictors"]]], t(fit[["parameters"]]$mean), fit[["classification"]])
  silhouettes <- summary(cluster::silhouette(fit[["classification"]], .mlClusteringCalculateDistances(dataset[, options[["predictors"]]])))
  result <- list()
  result[["pred.values"]] <- fit[["classification"]]
  result[["clusters"]] <- clusters
  result[["N"]] <- nrow(dataset)
  result[["size"]] <- as.numeric(table(fit[["classification"]]))
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

.mlClusteringTableModelParameters <- function(dataset, options, jaspResults, ready, position) {
  if (!is.null(jaspResults[["modelParametersCollection"]]) || !options[["tableModelParameters"]]) {
    return()
  }
  collection <- createJaspContainer(gettext("Estimated Model Parameters"))
  collection$dependOn(options = c("tableModelParameters", .mlClusteringDependencies(options)))
  collection$position <- position
  jaspResults[["modelParametersCollection"]] <- collection
  if (!ready) {
    return()
  }
  clusterResult <- jaspResults[["clusterResult"]]$object
  parameters <- clusterResult[["parameters"]]
  # Table with mixing probabilities
  if (!is.null(parameters[["pro"]])) {
    table <- createJaspTable(gettext("Mixing Probabilities"))
    table$dependOn(optionsFromObject = collection)
    table$position <- 1
    collection[["tableParametersMixing"]] <- table
    table$addColumnInfo(name = "cluster", title = "", type = "string")
    table$addColumnInfo(name = "prob", title = gettext("Mixing probability"), type = "number")
    rows <- data.frame(cluster = gettextf("Component %1$s", seq_len(clusterResult[["clusters"]])), prob = parameters[["pro"]])
    table$setData(rows)
  }
  # Table with means
  if (!is.null(parameters[["mean"]])) {
    table <- createJaspTable(gettext("Means"))
    table$dependOn(optionsFromObject = collection)
    table$position <- 2
    collection[["tableParametersMean"]] <- table
    table$addColumnInfo(name = "cluster", title = "", type = "string")
    for (i in seq_len(length(options[["predictors"]]))) {
      table$addColumnInfo(name = options[["predictors"]][i], title = options[["predictors"]][i], type = "number")
    }
    rows <- data.frame(cluster = gettextf("Component %1$s", seq_len(clusterResult[["clusters"]])))
    for (i in seq_len(length(options[["predictors"]]))) {
      rows[[options[["predictors"]][i]]] <- parameters[["mean"]][i, ]
    }
    table$setData(rows)
  }
  # Tables with covariance matrices
  if (!is.null(parameters[["variance"]]$sigma)) {
    for (i in seq_len(dim(parameters[["variance"]]$sigma)[3])) {
      table <- createJaspTable(gettextf("Covariance Matrix for Component %1$s", i))
      table$position <- 2 + i
      collection[[paste0("tableParametersCovariance", i)]] <- table
      table$addColumnInfo(name = "predictor", title = "", type = "string")
      for (j in seq_len(length(options[["predictors"]]))) {
        table$addColumnInfo(name = options[["predictors"]][j], title = options[["predictors"]][j], type = "number")
      }
      table$setData(data.frame(predictor = rownames(parameters[["variance"]]$sigma[ , , i]), parameters[["variance"]]$sigma[ , , i]))
    }
  }
  # Table with scale
  if (!is.null(parameters[["variance"]]$scale)) {
    table <- createJaspTable(gettext("Scale of the Covariance"))
    table$dependOn(optionsFromObject = collection)
    table$position <- 2 + clusterResult[["clusters"]] + 1
    collection[["tableParametersScale"]] <- table
    table$addColumnInfo(name = "cluster", title = "", type = "string")
    table$addColumnInfo(name = "scale", title = gettext("Scale"), type = "number")
    rows <- data.frame(cluster = gettextf("Component %1$s", seq_len(clusterResult[["clusters"]])), scale = parameters[["variance"]]$scale)
    table$setData(rows)
  }
  # Table with shape
  if (!is.null(parameters[["variance"]]$shape)) {
    table <- createJaspTable(gettext("Shape of the Covariance Matrix"))
    table$dependOn(optionsFromObject = collection)
    table$position <- 2 + clusterResult[["clusters"]] + 2
    collection[["tableParametersShape"]] <- table
    table$addColumnInfo(name = "cluster", title = "", type = "string")
    for (i in seq_len(length(options[["predictors"]]))) {
      table$addColumnInfo(name = options[["predictors"]][i], title = options[["predictors"]][i], type = "number")
    }
    rows <- data.frame(cluster = gettextf("Component %1$s", seq_len(clusterResult[["clusters"]])))
    if (is.matrix(parameters[["variance"]]$shape)) {
      rows <- cbind(rows, t(parameters[["variance"]]$shape))
      colnames(rows)[-1] <- options[["predictors"]]
    } else {
      for (i in seq_len(length(options[["predictors"]]))) {
        rows[[options[["predictors"]][i]]] <- parameters[["variance"]]$shape[i]
      }
    }
    table$setData(rows)
  }
  # Tables with orientation
  if (!is.null(parameters[["variance"]]$orientation)) {
    for (i in seq_len(length(options[["predictors"]]))) {
      table <- createJaspTable(gettextf("Eigenvalues of the Covariance Matrix for Component %1$s", i))
      table$position <- 2 + clusterResult[["clusters"]] + 2 + i
      collection[[paste0("tableParametersOrientation", i)]] <- table
      table$addColumnInfo(name = "predictor", title = "", type = "string")
      for (j in seq_len(length(options[["predictors"]]))) {
        table$addColumnInfo(name = options[["predictors"]][j], title = options[["predictors"]][j], type = "number")
      }
      if (length(dim(parameters[["variance"]]$orientation)) == 3) {
        table$setData(data.frame(predictor = rownames(parameters[["variance"]]$orientation[ , , i]), parameters[["variance"]]$orientation[ , , i]))
      } else {
        table$setData(data.frame(predictor = rownames(parameters[["variance"]]$orientation), parameters[["variance"]]$orientation))
      }
    }
  }
}
