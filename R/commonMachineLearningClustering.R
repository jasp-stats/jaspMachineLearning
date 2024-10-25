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

# This function should return all options for all analyses upon which a change in all tables/figures is required
.mlClusteringDependencies <- function(options) {
  opt <- c(
    "predictors", "seed", "setSeed",  "scaleVariables", "manualNumberOfClusters", # Common
    "modelOptimization", "modelOptimizationMethod", "maxNumberOfClusters",        # Common
    "epsilonNeighborhoodSize", "minCorePoints", "distance",                       # Density-based
    "maxNumberIterations", "fuzzinessParameter",                                  # Fuzzy c-means
    "linkage",                                                                    # Hierarchical
    "centers", "algorithm", "noOfRandomSets",                                     # K-means
    "numberOfTrees",                                                              # Random forest
    "modelName"                                                                   # Model-based
  )
  return(opt)
}

.mlClusteringReadData <- function(dataset, options) {
  predictors <- unlist(options[["predictors"]])
  predictors <- predictors[predictors != ""]
  dataset <- .readAndAddCompleteRowIndices(options, "predictors")
  if (options[["scaleVariables"]] && length(unlist(options[["predictors"]])) > 0) {
    dataset <- .scaleNumericData(dataset)
  }
  return(dataset)
}

.mlClusteringErrorHandling <- function(dataset, options, type) {
  predictors <- unlist(options$predictors)
  customChecks <- .getCustomErrorChecksClustering(dataset, options, type)
  if (length(predictors[predictors != ""]) > 0L) {
    .hasErrors(dataset,
      type = c("infinity", "observations"), custom = customChecks,
      all.target = predictors, observations.amount = "< 2", exitAnalysisIfErrors = TRUE
    )
  }
}

.getCustomErrorChecksClustering <- function(dataset, options, type) {
  checkClusters <- function() {
    if (type != "densitybased") {
      clusters <- switch(options[["modelOptimization"]],
        "manual" = options[["manualNumberOfClusters"]],
        "optimized" = options[["maxNumberOfClusters"]]
      )
      if (clusters > (nrow(dataset) - 1)) {
        return(gettextf("You have specified more clusters than distinct data points. Please choose a number lower than %s.", nrow(dataset)))
      }
    }
  }
  checkDataHierarchicalClustering <- function() {
    if (type != "hierarchical") {
      return()
    }
    if (nrow(dataset) >= 65536L) {
      return(gettext("R package error: The hclust clustering algorithm from the stats R package cannot handle data that has 65536 or more rows. We are working on a solution for this problem. Please try another algorithm in the meantime."))
    }
  }
  return(list(checkClusters, checkDataHierarchicalClustering))
}

.mlClusteringReady <- function(options) {
  ready <- length(options[["predictors"]][options[["predictors"]] != ""]) >= 2
  return(ready)
}

.sumsqr <- function(x, v, clusters) {
  sumsqr <- function(x) sum(scale(x, scale = FALSE)^2)
  bwss <- sumsqr(v[clusters, ])
  wss <- sapply(split(as.data.frame(x), clusters), sumsqr)
  twss <- sum(wss)
  tss <- bwss + twss
  ss <- list(bwss, wss, twss, tss)
  names(ss) <- c("bss", "wss", "tot.within.ss", "tss")
  return(ss)
}

.disttovar <- function(x) {
  mean(x**2) / 2
}

.tss <- function(x) {
  n <- nrow(as.matrix(x))
  .disttovar(x) * (n - 1)
}

.ss <- function(x) {
  sum(scale(x, scale = FALSE)^2)
}

.mlClusteringComputeResults <- function(dataset, options, jaspResults, ready, type) {
  if (!is.null(jaspResults[["clusterResult"]])) {
    return()
  }
  .setSeedJASP(options) # Set the seed to make results reproducible
  if (ready) {
    p <- try({
      clusterResult <- switch(type,
        "kmeans" = .kMeansClustering(dataset, options, jaspResults),
        "cmeans" = .cMeansClustering(dataset, options, jaspResults),
        "hierarchical" = .hierarchicalClustering(dataset, options, jaspResults),
        "densitybased" = .densityBasedClustering(dataset, options, jaspResults),
        "randomForest" = .randomForestClustering(dataset, options, jaspResults),
        "modelbased" = .modelBasedClustering(dataset, options, jaspResults)
      )
    })
    if (isTryError(p)) { # Fail gracefully
      jaspBase:::.quitAnalysis(gettextf("An error occurred in the analysis: %s", jaspBase:::.extractErrorMessage(p)))
    }
    jaspResults[["clusterResult"]] <- createJaspState(clusterResult)
    jaspResults[["clusterResult"]]$dependOn(options = .mlClusteringDependencies(options))
  }
}

.mlClusteringTableSummary <- function(dataset, options, jaspResults, ready, position, type) {
  if (!is.null(jaspResults[["clusteringTable"]])) {
    return()
  }
  title <- switch(type,
    "kmeans" = switch(options[["centers"]],
      "means" = gettext("K-Means Clustering"),
      "medians" = gettext("K-Medians Clustering"),
      "medoids" = gettext("K-Medoids Clustering")
    ),
    "cmeans" = gettext("Fuzzy C-Means Clustering"),
    "hierarchical" = gettext("Hierarchical Clustering"),
    "densitybased" = gettext("Density-Based Clustering"),
    "randomForest" = gettext("Random Forest Clustering"),
    "modelbased" = gettext("Model-Based Clustering")
  )
  tableTitle <- gettextf("Model Summary: %1$s", title)
  table <- createJaspTable(tableTitle)
  table$position <- position
  table$dependOn(options = .mlClusteringDependencies(options))
  table$addColumnInfo(name = "clusters", title = gettext("Clusters"), type = "integer")
  table$addColumnInfo(name = "n", title = gettext("N"), type = "integer")
  table$addColumnInfo(name = "measure", title = gettextf("R%s", "\u00B2"), type = "number", format = "dp:2")
  table$addColumnInfo(name = "aic", title = gettext("AIC"), type = "number", format = "dp:2")
  table$addColumnInfo(name = "bic", title = gettext("BIC"), type = "number", format = "dp:2")
  table$addColumnInfo(name = "Silh", title = gettext("Silhouette"), type = "number", format = "dp:2")
  if (type == "kmeans") {
    table$addCitation("Hartigan, J. A., & Wong, M. A. (1979). Algorithm AS 136: A k-means clustering algorithm. Journal of the Royal Statistical Society. Series C (Applied Statistics), 28(1), 100-108.")
  } else if (type == "kmedians") {
    table$addCitation(c("Cardot, H., Cenac, P. and Monnez, J-M. (2012). A fast and recursive algorithm for clustering large datasets with k-medians. Computational Statistics and Data Analysis, 56, 1434-1449.",
                        "Cardot, H., Cenac, P. and Zitt, P-A. (2013). Efficient and fast estimation of the geometric median in Hilbert spaces with an averaged stochastic gradient algorithm. Bernoulli, 19, 18-43."))
  }
  if (!ready) {
    table$addFootnote(gettext("Please provide at least 2 features."))
  }
  jaspResults[["clusteringTable"]] <- table
  if (!ready) {
    return()
  }
  .mlClusteringComputeResults(dataset, options, jaspResults, ready, type = type)
  clusterResult <- jaspResults[["clusterResult"]]$object
  if (options[["modelOptimization"]] != "manual") {
    criterion <- switch(options[["modelOptimizationMethod"]],
      "aic" = gettext("AIC"),
      "bic" = gettext("BIC"),
      "silhouette" = gettext("silhouette")
    )
    table$addFootnote(gettextf("The model is optimized with respect to the <i>%s</i> value.", criterion))
  }
  if (clusterResult[["clusters"]] == options[["maxNumberOfClusters"]] && options[["modelOptimization"]] != "manual") {
    message <- gettext("The optimum number of clusters is the maximum number of clusters. You might want to adjust the range of optimization.")
    table$addFootnote(message)
  }
  if (type == "densitybased") {
    if (clusterResult[["zeroMark"]] == 1) {
      table$addFootnote(gettext("The model contains 0 clusters and only Noisepoints, we advise to change 'Epsilon neighborhood size' and 'Min. core points' parameters under 'Training parameters'."), colNames = "clusters")
    }
    if (clusterResult[["oneMark"]] == 1) {
      table$addFootnote(gettext("The model contains 1 cluster and no Noisepoints. You may want to change 'Epsilon neighborhood size' and 'Min. core points' parameters under 'Training parameters'."), colNames = "clusters")
    }
  }
  if (type == "modelbased") {
    modelName <- switch(clusterResult[["modelName"]],
                        "EII" = gettext("spherical with equal volume"),
                        "VII" = gettext("spherical with unequal volume"),
                        "EEI" = gettext("diagonal with equal volume and shape"),
                        "VEI" = gettext("diagonal with varying volume and equal shape"),
                        "EVI" = gettext("diagonal with equal volume and varying shape"),
                        "VVI" = gettext("diagonal with varying volume and shape"),
                        "EEE" = gettext("ellipsoidal with equal volume, shape, and orientation"),
                        "VEE" = gettext("ellipsoidal with equal shape and orientation"),
                        "EVE" = gettext("ellipsoidal with equal volume and orientation"),
                        "VVE" = gettext("ellipsoidal with equal orientation"),
                        "EEV" = gettext("ellipsoidal with equal volume and equal shape"),
                        "VEV" = gettext("ellipsoidal with equal shape"),
                        "EVV" = gettext("ellipsoidal with equal volume"),
                        "VVV" = gettext("ellipsoidal with varying volume, shape, and orientation"))
    table$addFootnote(gettextf("The model is %1$s.", modelName))
  }
  if (!options[["scaleVariables"]]) {
    table$addFootnote(gettext("The features in the model are <b>unstandardized</b>."))
  }
  row <- data.frame(
    clusters = clusterResult[["clusters"]], measure = clusterResult[["BSS"]] / clusterResult[["TSS"]], aic = round(clusterResult[["AIC"]], 2),
    bic = round(clusterResult[["BIC"]], 2), Silh = round(clusterResult[["Silh_score"]], 2), n = clusterResult[["N"]]
  )
  table$addRows(row)
}

.mlClusteringTableInformation <- function(options, jaspResults, ready, position, type) {
  if (!is.null(jaspResults[["clusterInfoTable"]]) || !options[["tableClusterInformation"]]) {
    return()
  }
  table <- createJaspTable(gettext("Cluster Information"))
  table$dependOn(options = c(
    "tableClusterInformation", "tableClusterInformationWithinSumOfSquares", "tableClusterInformationSilhouetteScore", "tableClusterInformationCentroids",
    "tableClusterInformationBetweenSumOfSquares", "tableClusterInformationTotalSumOfSquares", .mlClusteringDependencies(options)
  ))
  table$position <- position
  table$transpose <- TRUE
  table$addColumnInfo(name = "cluster", title = gettext("Cluster"), type = "integer")
  table$addColumnInfo(name = "size", title = gettext("Size"), type = "integer")
  table$addColumnInfo(name = "percentage", title = gettext("Explained proportion within-cluster heterogeneity"), type = "number")
  if (options[["tableClusterInformationWithinSumOfSquares"]]) {
    table$addColumnInfo(name = "withinss", title = gettext("Within sum of squares"), type = "number")
  }
  if (options[["tableClusterInformationSilhouetteScore"]]) {
    table$addColumnInfo(name = "silh_scores", title = gettext("Silhouette score"), type = "number")
  }
  jaspResults[["clusterInfoTable"]] <- table
  if (!ready) {
    return()
  }
  clusterResult <- jaspResults[["clusterResult"]]$object
  if (type == "kmeans" || type == "cmeans") {
    if (options[["tableClusterInformationCentroids"]]) {
      for (i in seq_along(options[["predictors"]])) {
        title <- gettextf("Center %s", options[["predictors"]][i])
        table$addColumnInfo(name = paste0("centroid", i), title = title, type = "number", format = "dp:3")
      }
    }
  }
  size <- clusterResult[["size"]]
  cluster <- 1:clusterResult[["clusters"]]
  withinss <- clusterResult[["WSS"]]
  silh_scores <- clusterResult[["silh_scores"]]
  if (type == "densitybased") {
    if (sum(size) == clusterResult[["noisePoints"]]) {
      cluster <- gettext("Noisepoints")
      withinss <- 0
    } else if (clusterResult[["noisePoints"]] > 0) {
      cluster <- c(gettext("Noisepoints"), 1:(clusterResult[["clusters"]]))
      withinss <- c(0, withinss)
      silh_scores[1] <- 0
    }
  }
  row <- data.frame(
    cluster = cluster,
    size = size,
    percentage = withinss / sum(withinss)
  )
  if (options[["tableClusterInformationWithinSumOfSquares"]]) {
    row <- cbind(row, withinss = withinss)
  }
  if (options[["tableClusterInformationSilhouetteScore"]]) {
    row <- cbind(row, silh_scores = silh_scores)
  }
  if (type == "kmeans" || type == "cmeans") {
    if (options[["tableClusterInformationCentroids"]]) {
      for (i in 1:length(options[["predictors"]])) {
        row <- cbind(row, "tmp" = clusterResult[["centroids"]][, i])
        colnames(row)[length(colnames(row))] <- paste0("centroid", i)
      }
    }
  }
  table$addRows(row)
  if (options[["tableClusterInformationBetweenSumOfSquares"]]) {
    message <- gettextf("The Between Sum of Squares of the %1$s cluster model is %2$s", clusterResult[["clusters"]], round(clusterResult[["BSS"]], 2))
    table$addFootnote(message)
  }
  if (options[["tableClusterInformationTotalSumOfSquares"]]) {
    message <- gettextf("The Total Sum of Squares of the %1$s cluster model is %2$s", clusterResult[["clusters"]], round(clusterResult[["TSS"]], 2))
    table$addFootnote(message)
  }
}

.mlClusteringTableMetrics <- function(dataset, options, jaspResults, ready, position) {
  if (!is.null(jaspResults[["clusterEvaluationMetrics"]]) || !options[["validationMeasures"]]) {
    return()
  }
  table <- createJaspTable(gettext("Model Performance Metrics"))
  table$dependOn(options = c("validationMeasures", .mlClusteringDependencies(options)))
  table$position <- position
  table$addColumnInfo(name = "metric", title = "", type = "string")
  table$addColumnInfo(name = "value", title = gettext("Value"), type = "number")
  jaspResults[["clusterEvaluationMetrics"]] <- table
  if (!ready) {
    return()
  }
  clusterResult <- jaspResults[["clusterResult"]]$object
  clustering <- clusterResult[["pred.values"]]
  distance <- .mlClusteringCalculateDistances(dataset)
  metrics <- fpc::cluster.stats(distance, clustering, silhouette = FALSE, sepindex = FALSE, sepwithnoise = FALSE)
  table[["metric"]] <- c(
    gettext("Maximum diameter"),
    gettext("Minimum separation"),
    gettextf("Pearson's %s", "\u03B3"),
    gettext("Dunn index"),
    gettext("Entropy"),
    gettext("Calinski-Harabasz index")
  )
  if (length(unique(clustering)) != 1) {
    table[["value"]] <- c(metrics[["max.diameter"]], metrics[["min.separation"]], metrics[["pearsongamma"]], metrics[["dunn"]], metrics[["entropy"]], metrics[["ch"]])
    table$addFootnote(gettext("All metrics are based on the <i>euclidean</i> distance."))
  } else {
    table$addFootnote(gettext("Evaluation metrics cannot be computed when there is only 1 cluster."))
  }
}

.mlClusteringPlotTsne <- function(dataset, options, jaspResults, ready, position, type) {
  if (!is.null(jaspResults[["plot2dCluster"]]) || !options[["tsneClusterPlot"]]) {
    return()
  }
  plot <- createJaspPlot(plot = NULL, title = gettext("t-SNE Cluster Plot"), width = 450, height = 300)
  plot$position <- position
  plot$dependOn(options = c("tsneClusterPlot", "tsneClusterPlotLabels", .mlClusteringDependencies(options)))
  jaspResults[["plot2dCluster"]] <- plot
  if (!ready) {
    return()
  }
  clusterResult <- jaspResults[["clusterResult"]]$object
  .setSeedJASP(options) # Set the seed to make results reproducible
  startProgressbar(2)
  progressbarTick()
  duplicates <- which(duplicated(dataset))
  if (length(duplicates) > 0) {
    dataset <- dataset[-duplicates, ]
  }
  if (is.null(jaspResults[["tsneOutput"]])) {
    tsne <- Rtsne::Rtsne(as.matrix(dataset), perplexity = nrow(dataset) / 4, check_duplicates = FALSE)
    jaspResults[["tsneOutput"]] <- createJaspState(tsne)
    jaspResults[["tsneOutput"]]$dependOn(options = c("predictors", "setSeed", "seed"))
  } else {
    tsne <- jaspResults[["tsneOutput"]]$object
  }
  predictions <- clusterResult[["pred.values"]]
  ncolors <- clusterResult[["clusters"]]
  if (type == "densitybased") {
    ncolors <- ncolors + 1
    predictions[predictions == 0] <- gettext("Noisepoint")
  }
  if (length(duplicates) > 0) {
    predictions <- predictions[-duplicates]
  }
  plotData <- data.frame(x = tsne$Y[, 1], y = tsne$Y[, 2], cluster = predictions)
  plotData$cluster <- factor(plotData$cluster)
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(plotData$x)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(plotData$y)
  p <- ggplot2::ggplot(data = plotData, mapping = ggplot2::aes(x = x, y = y)) +
    jaspGraphs::geom_point(mapping = ggplot2::aes(fill = cluster)) +
    ggplot2::scale_x_continuous(name = NULL, limits = range(xBreaks)) +
    ggplot2::scale_y_continuous(name = NULL, limits = range(yBreaks)) +
    ggplot2::scale_fill_manual(name = gettext("Cluster"), values = .mlColorScheme(ncolors)) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw(legend.position = if (options[["tsneClusterPlotLegend"]]) "right" else "none") +
    ggplot2::theme(axis.ticks = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank())
  if (options[["tsneClusterPlotLabels"]]) {
    p <- p + ggrepel::geom_text_repel(ggplot2::aes(label = rownames(dataset), x = x, y = y), hjust = -1, vjust = 1, data = plotData, seed = 1)
  }
  progressbarTick()
  plot$plotObject <- p
}

.mlClusteringPlotElbow <- function(dataset, options, jaspResults, ready, position) {
  if (!is.null(jaspResults[["optimPlot"]]) || !options[["elbowMethodPlot"]] || options[["modelOptimization"]] == "manual") {
    return()
  }
  plot <- createJaspPlot(plot = NULL, title = gettext("Elbow Method Plot"), width = 400, height = 300)
  plot$position <- position
  plot$dependOn(options = c("elbowMethodPlot", .mlClusteringDependencies(options)))
  jaspResults[["optimPlot"]] <- plot
  if (!ready) {
    return()
  }
  clusterResult <- jaspResults[["clusterResult"]]$object
  wss <- clusterResult[["wssStore"]]
  aic <- clusterResult[["aicStore"]]
  bic <- clusterResult[["bicStore"]]
  values <- c(wss, aic, bic)
  type <- rep(c(gettext("WSS"), gettext("AIC"), gettext("BIC")), each = length(wss))
  requiredPoint <- switch(options[["modelOptimizationMethod"]],
    "aic" = gettext("AIC"),
    "bic" = gettext("BIC"),
    "silhouette" = ""
  )
  plotData <- data.frame(x = rep(2:options[["maxNumberOfClusters"]], 3), y = values, type = type)
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(plotData$x, min.n = 4)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(plotData$y, min.n = 4)
  yVals <- values[type == requiredPoint]
  pointData <- data.frame(
    x = clusterResult[["clusters"]],
    y = yVals[clusterResult[["clusters"]] - 1],
    type = requiredPoint
  )
  p <- ggplot2::ggplot(data = plotData, mapping = ggplot2::aes(x = x, y = y)) +
    jaspGraphs::geom_line(mapping = ggplot2::aes(linetype = type)) +
    ggplot2::scale_x_continuous(name = gettext("Number of Clusters"), breaks = xBreaks, limits = range(xBreaks)) +
    ggplot2::scale_y_continuous(name = NULL, breaks = yBreaks, limits = range(yBreaks)) +
    ggplot2::scale_linetype_manual(name = NULL, values = c(3, 2, 1)) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw(legend.position = "top") +
    ggplot2::theme(legend.text = ggplot2::element_text(size = 12))
  if (options[["modelOptimizationMethod"]] != "silhouette") {
    p <- p + jaspGraphs::geom_point(data = pointData, ggplot2::aes(x = x, y = y, fill = "red"), inherit.aes = FALSE) +
      ggplot2::scale_fill_manual(name = NULL, labels = gettextf("Lowest %s", requiredPoint), values = "red") +
      ggplot2::guides(fill = ggplot2::guide_legend(order = 1), linetype = ggplot2::guide_legend(order = 2))
  }
  plot$plotObject <- p
}

.mlClusteringAddPredictionsToData <- function(dataset, options, jaspResults, ready) {
  if (!ready || !options[["addPredictions"]] || options[["predictionsColumn"]] == "") {
    return()
  }
  clusterResult <- jaspResults[["clusterResult"]]$object
  if (is.null(jaspResults[["predictionsColumn"]])) {
    predictions <- clusterResult[["pred.values"]]
    predictionsColumn <- rep(NA, max(as.numeric(rownames(dataset))))
    predictionsColumn[as.numeric(rownames(dataset))] <- predictions
    jaspResults[["predictionsColumn"]] <- createJaspColumn(columnName = options[["predictionsColumn"]])
    jaspResults[["predictionsColumn"]]$dependOn(options = c("addPredictions", "predictionsColumn", .mlClusteringDependencies(options)))
    jaspResults[["predictionsColumn"]]$setNominal(predictionsColumn)
  }
}

.mlClusteringTableMeans <- function(dataset, options, jaspResults, ready, position) {
  if (!is.null(jaspResults[["clusterMeansTable"]]) || !options[["tableClusterMeans"]]) {
    return()
  }
  table <- createJaspTable(gettext("Cluster Means"))
  table$dependOn(options = c("tableClusterMeans", .mlClusteringDependencies(options)))
  table$position <- position
  jaspResults[["clusterMeansTable"]] <- table
  if (!ready) {
    return()
  }
  table$addColumnInfo(name = "cluster", title = "", type = "number")
  if (!identical(options[["predictors"]], "")) {
    for (i in 1:length(unlist(options[["predictors"]]))) {
      columnName <- as.character(options[["predictors"]][i])
      table$addColumnInfo(name = columnName, title = columnName, type = "number")
    }
  }
  clusterResult <- jaspResults[["clusterResult"]]$object
  clusters <- as.factor(clusterResult[["pred.values"]])
  clusterLevels <- as.numeric(levels(clusters))
  clusterTitles <- gettextf("Cluster %s", clusterLevels)
  clusterMeans <- NULL
  for (i in clusterLevels) {
    clusterSubset <- subset(dataset, clusters == i)
    clusterMeans <- rbind(clusterMeans, colMeans(clusterSubset))
  }
  clusterMeans <- cbind(cluster = clusterTitles, data.frame(clusterMeans))
  colnames(clusterMeans) <- c("cluster", as.character(options[["predictors"]]))
  table$setData(clusterMeans)
}

.mlClusteringPlotDensities <- function(dataset, options, jaspResults, ready, position) {
  if (!is.null(jaspResults[["clusterDensities"]]) || !options[["clusterDensityPlot"]]) {
    return()
  }
  plot <- createJaspContainer(gettext("Cluster Density Plots"))
  plot$dependOn(options = c("clusterDensityPlot", "clusterDensityPlotSingleFigure", .mlClusteringDependencies(options)))
  plot$position <- position
  jaspResults[["clusterDensities"]] <- plot
  if (!ready) {
    return()
  }
  clusterResult <- jaspResults[["clusterResult"]]$object
  if (!options[["clusterDensityPlotSingleFigure"]]) {
    for (variable in unlist(options[["predictors"]])) {
      clusters <- as.factor(clusterResult[["pred.values"]])
      xBreaks <- jaspGraphs::getPrettyAxisBreaks(dataset[[variable]], min.n = 4)
      plotData <- data.frame(
        cluster = clusters,
        value = dataset[[variable]]
      )
      p <- ggplot2::ggplot(plotData, ggplot2::aes(x = value)) +
        ggplot2::geom_density(mapping = ggplot2::aes(fill = cluster), color = "black", alpha = 0.6) +
        ggplot2::scale_x_continuous(name = variable, breaks = xBreaks, limits = range(xBreaks)) +
        ggplot2::scale_y_continuous(name = gettext("Density")) +
        ggplot2::scale_fill_manual(name = gettext("Cluster"), values = .mlColorScheme(length(levels(clusters)))) +
        jaspGraphs::geom_rangeframe() +
        jaspGraphs::themeJaspRaw(legend.position = "right") +
        ggplot2::theme(axis.ticks.y = ggplot2::element_blank(), axis.text.y = ggplot2::element_blank())
      plot[[variable]] <- createJaspPlot(plot = p, title = variable, width = 400, height = 300)
      plot[[variable]]$dependOn(optionContainsValue = list("predictors" = variable))
    }
  } else {
    dataList <- c(dataset[, options[["predictors"]]])
    plotData <- data.frame(value = unlist(dataList), variable = rep(options[["predictors"]], lengths(dataList)), cluster = rep(clusterResult[["pred.values"]], length(options[["predictors"]])))
    xBreaks <- jaspGraphs::getPrettyAxisBreaks(plotData[["value"]])
    p <- ggplot2::ggplot(data = plotData, mapping = ggplot2::aes(x = value, y = factor(variable), height = ..density.., fill = factor(cluster))) +
      ggridges::geom_density_ridges(stat = "density", alpha = .6) +
      ggplot2::scale_fill_manual(name = gettext("Cluster"), values = .mlColorScheme(length(unique(clusterResult[["pred.values"]])))) +
      ggplot2::scale_x_continuous(name = gettext("Value"), breaks = xBreaks, limits = range(xBreaks)) +
      ggplot2::scale_y_discrete(name = gettext("Feature")) +
      jaspGraphs::geom_rangeframe(sides = "b") +
      jaspGraphs::themeJaspRaw(legend.position = "right") +
      ggplot2::theme(axis.ticks.y = ggplot2::element_blank())
    plot[["oneFigure"]] <- createJaspPlot(plot = p, title = gettext("All Features"), height = 150 * length(options[["predictors"]]), width = 600)
  }
}

.mlClusteringPlotMeans <- function(dataset, options, jaspResults, ready, position) {
  if (!is.null(jaspResults[["clusterMeans"]]) || !options[["clusterMeanPlot"]]) {
    return()
  }
  plot <- createJaspContainer(gettext("Cluster Mean Plots"))
  plot$dependOn(options = c("clusterMeanPlot", "clusterMeanPlotBarPlot", "clusterMeanPlotSingleFigure", .mlClusteringDependencies(options)))
  plot$position <- position
  jaspResults[["clusterMeans"]] <- plot
  if (!ready) {
    return()
  }
  clusterResult <- jaspResults[["clusterResult"]]$object
  if (options[["clusterMeanPlotSingleFigure"]]) {
    clusters <- as.factor(clusterResult[["pred.values"]])
    xBreaks <- c(1, (as.numeric(levels(clusters)) + 1) * length(options[["predictors"]]))
    clusterMeansData <- aggregate(dataset, list(clusters), mean)
    clusterSdData <- aggregate(dataset, list(clusters), sd)
    clusterLengthData <- aggregate(dataset, list(clusters), length)
    clusterCoord <- rep(clusterMeansData[, 1], length(options[["predictors"]]))
    xCoord <- numeric()
    for (i in 1:length(options[["predictors"]])) {
      if (i == 1) {
        xCoord <- c(xCoord, (length(xCoord) + 1):(length(xCoord) + length(clusterMeansData[, 1])))
      } else {
        xCoord <- c(xCoord, (max(xCoord) + 3):(max(xCoord) + 2 + length(clusterMeansData[, 1])))
      }
    }
    values <- as.numeric(unlist(clusterMeansData[, -1]))
    lowerValues <- as.numeric(unlist(clusterMeansData[, -1])) - qnorm(0.95) * as.numeric(unlist(clusterSdData[, -1])) / sqrt(as.numeric(unlist(clusterLengthData[, -1])))
    upperValues <- as.numeric(unlist(clusterMeansData[, -1])) + qnorm(0.95) * as.numeric(unlist(clusterSdData[, -1])) / sqrt(as.numeric(unlist(clusterLengthData[, -1])))
    plotData <- data.frame(
      xCoord = xCoord,
      Cluster = clusterCoord,
      value = values,
      lower = lowerValues,
      upper = upperValues
    )
    yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(0, unlist(plotData[complete.cases(plotData), -c(1, 2)])), min.n = 4)
    xBreaks <- (1:length(options[["predictors"]])) * (length(levels(clusters)) + 2)
    xBreaks <- xBreaks - (length(levels(clusters)) + 2)
    xBreaks <- xBreaks + 0.5 * length(levels(clusters))
    xLabels <- options[["predictors"]]
    plotData <- plotData[complete.cases(plotData), ]
    p <- ggplot2::ggplot(plotData, ggplot2::aes(x = xCoord, y = value, fill = Cluster))
    if (options[["clusterMeanPlotBarPlot"]]) {
      p <- p + ggplot2::geom_bar(color = "black", stat = "identity") +
        ggplot2::geom_errorbar(data = plotData, ggplot2::aes(x = xCoord, ymin = lower, ymax = upper), width = 0.2, linewidth = 1)
    } else {
      p <- p + ggplot2::geom_segment(ggplot2::aes(x = 0, xend = max(plotData[["xCoord"]]), y = 0, yend = 0), linetype = 2) +
        ggplot2::geom_errorbar(data = plotData, ggplot2::aes(x = xCoord, ymin = lower, ymax = upper), width = 0.2, linewidth = 1) +
        jaspGraphs::geom_point(color = "black")
    }
    p <- p + ggplot2::scale_x_continuous(name = NULL, breaks = xBreaks, labels = xLabels) +
      ggplot2::scale_y_continuous(name = gettext("Cluster Mean"), breaks = yBreaks, limits = range(yBreaks)) +
      ggplot2::scale_fill_manual(name = gettext("Cluster"), values = .mlColorScheme(length(unique(clusterResult[["pred.values"]])))) +
      jaspGraphs::geom_rangeframe(sides = "l") +
      jaspGraphs::themeJaspRaw(legend.position = "right") +
      ggplot2::theme(axis.ticks.x = ggplot2::element_blank(), axis.text.x = ggplot2::element_text(angle = 20))
    plot[["oneFigure"]] <- createJaspPlot(plot = p, title = gettext("All Features"), height = 400, width = 200 * length(options[["predictors"]]))
  } else {
    for (variable in unlist(options[["predictors"]])) {
      clusters <- as.factor(clusterResult[["pred.values"]])
      xBreaks <- as.numeric(levels(clusters))
      clusterMeansData <- aggregate(dataset[[variable]], list(clusters), mean)
      clusterSdData <- aggregate(dataset[[variable]], list(clusters), sd)
      clusterLengthData <- aggregate(dataset[[variable]], list(clusters), length)
      plotData <- data.frame(
        Cluster = clusterMeansData[, 1],
        value = clusterMeansData[, 2],
        lower = clusterMeansData[, 2] - qnorm(0.95) * clusterSdData[, 2] / sqrt(clusterLengthData[, 2]),
        upper = clusterMeansData[, 2] + qnorm(0.95) * clusterSdData[, 2] / sqrt(clusterLengthData[, 2])
      )
      plotData <- plotData[complete.cases(plotData), ]
      yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(0, unlist(plotData[, -1])), min.n = 4)
      p <- ggplot2::ggplot(plotData, ggplot2::aes(x = Cluster, y = value, fill = Cluster))
      if (options[["clusterMeanPlotBarPlot"]]) {
        p <- p + ggplot2::geom_bar(color = "black", stat = "identity") +
          ggplot2::geom_errorbar(data = plotData, ggplot2::aes(x = Cluster, ymin = lower, ymax = upper), width = 0.2, linewidth = 1)
      } else {
        p <- p + ggplot2::geom_errorbar(data = plotData, ggplot2::aes(x = Cluster, ymin = lower, ymax = upper), width = 0.2, linewidth = 1) +
          jaspGraphs::geom_point(color = "black")
      }
      p <- p + ggplot2::scale_x_discrete(name = gettext("Cluster"), breaks = xBreaks) +
        ggplot2::scale_y_continuous(name = variable, breaks = yBreaks, limits = range(yBreaks)) +
        ggplot2::scale_fill_manual(name = gettext("Cluster"), values = .mlColorScheme(length(unique(clusterResult[["pred.values"]])))) +
        jaspGraphs::geom_rangeframe(sides = "l") +
        jaspGraphs::themeJaspRaw() +
        ggplot2::theme(axis.ticks.x = ggplot2::element_blank())
      plot[[variable]] <- createJaspPlot(plot = p, title = variable, width = 400, height = 300)
      plot[[variable]]$dependOn(optionContainsValue = list("predictors" = variable))
    }
  }
}

.mlClusteringCalculateDistances <- function(x) {
  p <- try({
    distx <- dist(x) # This scales terribly in terms of memory (O(n^2))
  })
  if (isTryError(p) && "allocate" %in% strsplit(x = p[[1]], split = " ")[[1]]) {
    jaspBase:::.quitAnalysis(gettextf("Insufficient RAM available to compute the distance matrix. The analysis tried to allocate %s Gb", .extractMemSizeFromError(p)))
  } else if (isTryError(p)) {
    jaspBase:::.quitAnalysis(gettextf("An error occurred in the analysis: %1$s", .extractErrorMessage(p)))
  }
  return(distx)
}

.extractMemSizeFromError <- function(p) {
  unlist(regmatches(p[[1]], gregexpr("[[:digit:]]+\\.*[[:digit:]]*", p[[1]])))
}

.mlClusteringMatrixPlot <- function(dataset, options, jaspResults, ready, position) {
  if (!is.null(jaspResults[["matrixPlot"]]) || !options[["matrixPlot"]]) {
    return()
  }
  plot <- createJaspPlot(title = gettext("Cluster Matrix Plot"), height = 400, width = 300)
  plot$position <- position
  plot$dependOn(options = c(.mlClusteringDependencies(options), "matrixPlot", "addEllips"))
  jaspResults[["matrixPlot"]] <- plot
  if (!ready || length(options[["predictors"]]) < 2) {
    return()
  }
  clusterResult <- jaspResults[["clusterResult"]]$object
  variables <- options[["predictors"]]
  variables <- variables[!vapply(dataset[, variables], is.factor, TRUE)] # remove factors from matrix plot
  l <- length(variables)
  if (l < 2) { # Need at least 2 numeric variables to create a matrix
    plot$setError(gettext("Cannot create matrix: not enough numeric variables remain after removing factor variables. You need at least 2 numeric variables."))
    return()
  }
  if (l <= 2) {
    width <- 400
    height <- 300
  } else {
    width <- 200 * l
    height <- 200 * l
  }
  plot[["width"]] <- width
  plot[["height"]] <- height
  cexText <- 1.6
  plotMat <- matrix(list(), l - 1, l - 1)
  oldFontSize <- jaspGraphs::getGraphOption("fontsize")
  jaspGraphs::setGraphOption("fontsize", .85 * oldFontSize)
  startProgressbar(length(plotMat) + 1)
  for (row in 2:l) {
    for (col in 1:(l - 1)) {
      if (col < row) {
        predictors <- dataset[, variables]
        predictors <- predictors[, c(col, row)]
        plotData <- data.frame(x = predictors[, 1], y = predictors[, 2], cluster = as.factor(clusterResult[["pred.values"]]))
        xBreaks <- jaspGraphs::getPrettyAxisBreaks(plotData$x, min.n = 4)
        yBreaks <- jaspGraphs::getPrettyAxisBreaks(plotData$y, min.n = 4)
        p <- ggplot2::ggplot(data = plotData, mapping = ggplot2::aes(x = x, y = y, fill = cluster)) +
          jaspGraphs::geom_point() +
          ggplot2::scale_fill_manual(name = NULL, values = .mlColorScheme(clusterResult[["clusters"]])) +
          ggplot2::scale_x_continuous(name = NULL, breaks = xBreaks, limits = range(xBreaks)) +
          ggplot2::scale_y_continuous(name = NULL, breaks = yBreaks, limits = range(yBreaks)) +
          jaspGraphs::geom_rangeframe() +
          jaspGraphs::themeJaspRaw()

        # Add the ellipses for model-based clustering
        if (options[["addEllips"]]) {
          for (i in seq_len(clusterResult[["clusters"]])) {
            xvar <- options[["predictors"]][col]
            yvar <- options[["predictors"]][row]
            covmat <- clusterResult[["parameters"]]$variance$sigma[, , i]
            sigma <- matrix(c(covmat[xvar, xvar], covmat[xvar, yvar], covmat[yvar, xvar], covmat[yvar, yvar]), nrow = 2, byrow = TRUE)
            mu <- clusterResult[["parameters"]]$mean[, i]
            mu_x <- mu[xvar]
            mu_y <- mu[yvar]
            a <- sigma[1, 1]
            b <- sigma[2, 1]
            c <- sigma[2, 2]
            lambda1 <- abs((a + c) / 2 + sqrt(((a - 2) / 2)^2 + b^2))
            lambda2 <- abs((a + c) / 2 - sqrt(((a - 2) / 2)^2 + b^2))
            theta <- ifelse(b == 0 && a >= c, 0, ifelse(b == 0 && a < c, pi / 2, atan2(lambda1 - a, b)))
            t <- seq(0, 2 * pi, length.out = 1000)
            x <- sqrt(lambda1) * cos(theta) * cos(t) - sqrt(lambda2) * sin(theta) * sin(t) + mu_x
            y <- sqrt(lambda1) * sin(theta) * cos(t) + sqrt(lambda2) * cos(theta) * sin(t) + mu_y
            ellips <- data.frame(x = x, y = y)
            p <- p + ggplot2::geom_path(data = ellips, mapping = ggplot2::aes(x = x, y = y), color = "black", inherit.aes = FALSE, linewidth = 1.5) +
                       ggplot2::geom_path(data = ellips, mapping = ggplot2::aes(x = x, y = y), color = .mlColorScheme(clusterResult[["clusters"]])[i], inherit.aes = FALSE, linewidth = 0.75)
          }
        }

        plotMat[[row - 1, col]] <- p
      }
      if (l > 2) {
        predictors <- dataset[, options[["predictors"]]]
        plotData <- data.frame(cluster = as.factor(clusterResult[["pred.values"]]), predictor = predictors[, 1])
        p <- ggplot2::ggplot(plotData, ggplot2::aes(y = cluster, x = cluster, show.legend = TRUE)) +
          jaspGraphs::geom_point(ggplot2::aes(fill = cluster), alpha = 0) +
          ggplot2::xlab(NULL) +
          ggplot2::ylab(NULL) +
          ggplot2::theme(legend.key = ggplot2::element_blank()) +
          ggplot2::scale_fill_manual(name = gettext("Cluster"), values = .mlColorScheme(clusterResult[["clusters"]])) +
          jaspGraphs::geom_rangeframe(sides = "") +
          jaspGraphs::themeJaspRaw(legend.position = "left") +
          ggplot2::theme(axis.ticks = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank(), axis.text.y = ggplot2::element_blank()) +
          ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(alpha = 1)))
        plotMat[[1, 2]] <- p
      }
      progressbarTick()
    }
  }
  jaspGraphs::setGraphOption("fontsize", oldFontSize)
  # slightly adjust the positions of the labels left and above the plots.
  labelPos <- matrix(.5, 4, 2)
  labelPos[1, 1] <- .55
  labelPos[4, 2] <- .65
  p <- jaspGraphs::ggMatrixPlot(
    plotList = plotMat, leftLabels = variables[-1], topLabels = variables[-length(variables)],
    scaleXYlabels = NULL, labelPos = labelPos
  )
  progressbarTick()
  plot$plotObject <- p
}
