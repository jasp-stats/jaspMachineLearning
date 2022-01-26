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

.mlClusteringDependencies <- function(options) {
  opt <- c(
    "predictors", "noOfClusters", "noOfRandomSets", "noOfIterations", "algorithm", "modelOpt", "seed", "centers",
    "maxClusters", "seedBox", "scaleEqualSD", "m", "distance", "linkage", "eps", "minPts", "noOfTrees", "maxTrees", "optimizationCriterion"
  )
  return(opt)
}

.mlClusteringReadData <- function(dataset, options) {
  predictors <- unlist(options[["predictors"]])
  predictors <- predictors[predictors != ""]
  if (is.null(dataset)) {
    dataset <- .readAndAddCompleteRowIndices(dataset, predictors)
  }
  if (options[["scaleEqualSD"]] && length(unlist(options[["predictors"]])) > 0) {
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
      clusters <- switch(options[["modelOpt"]],
        "validationManual" = options[["noOfClusters"]],
        "validationOptimized" = options[["maxClusters"]]
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
  # set the seed so that every time the same set is chosen (to prevent random results) ##
  if (options[["seedBox"]]) {
    set.seed(options[["seed"]])
  }
  if (ready) {
    clusterResult <- switch(type,
      "kmeans" = .kMeansClustering(dataset, options, jaspResults),
      "cmeans" = .cMeansClustering(dataset, options, jaspResults),
      "hierarchical" = .hierarchicalClustering(dataset, options, jaspResults),
      "densitybased" = .densityBasedClustering(dataset, options, jaspResults),
      "randomForest" = .randomForestClustering(dataset, options, jaspResults)
    )
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
    "randomForest" = gettext("Random Forest Clustering")
  )
  table <- createJaspTable(title)
  table$position <- position
  table$dependOn(options = .mlClusteringDependencies(options))
  table$addColumnInfo(name = "clusters", title = gettext("Clusters"), type = "integer")
  table$addColumnInfo(name = "n", title = gettext("N"), type = "integer")
  table$addColumnInfo(name = "measure", title = gettextf("R%s", "\u00B2"), type = "number", format = "dp:2")
  table$addColumnInfo(name = "aic", title = gettext("AIC"), type = "number", format = "dp:2")
  table$addColumnInfo(name = "bic", title = gettext("BIC"), type = "number", format = "dp:2")
  table$addColumnInfo(name = "Silh", title = "Silhouette", type = "number", format = "dp:2")
  if (type == "kmeans") {
    table$addCitation("Hartigan, J. A., & Wong, M. A. (1979). Algorithm AS 136: A k-means clustering algorithm. Journal of the Royal Statistical Society. Series C (Applied Statistics), 28(1), 100-108.")
  } else if (type == "kmedians") {
    table$addCitation(c("Cardot, H., Cenac, P. and Monnez, J-M. (2012). A fast and recursive algorithm for clustering large datasets with k-medians. Computational Statistics and Data Analysis, 56, 1434-1449.",
                        "Cardot, H., Cenac, P. and Zitt, P-A. (2013). Efficient and fast estimation of the geometric median in Hilbert spaces with an averaged stochastic gradient algorithm. Bernoulli, 19, 18-43."))
  }
  if (!ready) {
    table$addFootnote(gettext("Please provide at least 2 variables."))
  }
  jaspResults[["clusteringTable"]] <- table
  if (!ready) {
    return()
  }
  .mlClusteringComputeResults(dataset, options, jaspResults, ready, type = type)
  clusterResult <- jaspResults[["clusterResult"]]$object
  if (options[["modelOpt"]] != "validationManual") {
    criterion <- switch(options[["optimizationCriterion"]],
      "validationAIC" = gettext("AIC"),
      "validationBIC" = gettext("BIC"),
      "validationSilh" = gettext("silhouette")
    )
    table$addFootnote(gettextf("The model is optimized with respect to the <i>%s</i> value.", criterion))
  }
  if (clusterResult[["clusters"]] == options[["maxClusters"]] && options[["modelOpt"]] != "validationManual") {
    message <- gettext("The optimum number of clusters is the maximum number of clusters. You might want to adjust the range of optimization.")
    table$addFootnote(message)
  }
  if (type == "densitybased") {
    if (clusterResult[["zeroMark"]] == 1) {
      table$addFootnote(gettext("Your cluster model contains 0 clusters and only Noisepoints, we advise to change the Eps and MinPts parameters."), colNames = "clusters")
    }
    if (clusterResult[["oneMark"]] == 1) {
      table$addFootnote(gettext("Your cluster model contains 1 cluster and 0 Noisepoints. You could change the Eps and MinPts parameters."), colNames = "clusters")
    }
  }
  if (!options[["scaleEqualSD"]]) {
    table$addFootnote(gettext("The variables in the model are <b>unstandardized</b>."))
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
    "tableClusterInformation", "tableClusterInfoWSS", "tableClusterInfoSilhouette", "tableClusterInfoCentroids",
    "tableClusterInfoBetweenSumSquares", "tableClusterInfoTotalSumSquares", .mlClusteringDependencies(options)
  ))
  table$position <- position
  table$transpose <- TRUE
  table$addColumnInfo(name = "cluster", title = gettext("Cluster"), type = "integer")
  table$addColumnInfo(name = "size", title = gettext("Size"), type = "integer")
  table$addColumnInfo(name = "percentage", title = gettext("Explained proportion within-cluster heterogeneity"), type = "number")
  if (options[["tableClusterInfoWSS"]]) {
    table$addColumnInfo(name = "withinss", title = gettext("Within sum of squares"), type = "number")
  }
  if (options[["tableClusterInfoSilhouette"]]) {
    table$addColumnInfo(name = "silh_scores", title = gettext("Silhouette score"), type = "number")
  }
  jaspResults[["clusterInfoTable"]] <- table
  if (!ready) {
    return()
  }
  clusterResult <- jaspResults[["clusterResult"]]$object
  if (type == "kmeans" || type == "cmeans") {
    if (options[["tableClusterInfoCentroids"]]) {
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
  if (options[["tableClusterInfoWSS"]]) {
    row <- cbind(row, withinss = withinss)
  }
  if (options[["tableClusterInfoSilhouette"]]) {
    row <- cbind(row, silh_scores = silh_scores)
  }
  if (type == "kmeans" || type == "cmeans") {
    if (options[["tableClusterInfoCentroids"]]) {
      for (i in 1:length(options[["predictors"]])) {
        row <- cbind(row, "tmp" = clusterResult[["centroids"]][, i])
        colnames(row)[length(colnames(row))] <- paste0("centroid", i)
      }
    }
  }
  table$addRows(row)
  if (options[["tableClusterInfoBetweenSumSquares"]]) {
    message <- gettextf("The Between Sum of Squares of the %1$s cluster model is %2$s", clusterResult[["clusters"]], round(clusterResult[["BSS"]], 2))
    table$addFootnote(message)
  }
  if (options[["tableClusterInfoTotalSumSquares"]]) {
    message <- gettextf("The Total Sum of Squares of the %1$s cluster model is %2$s", clusterResult[["clusters"]], round(clusterResult[["TSS"]], 2))
    table$addFootnote(message)
  }
}

.mlClusteringTableMetrics <- function(dataset, options, jaspResults, ready, position) {
  if (!is.null(jaspResults[["clusterEvaluationMetrics"]]) || !options[["clusterEvaluationMetrics"]]) {
    return()
  }
  table <- createJaspTable(gettext("Evaluation Metrics"))
  table$dependOn(options = c("clusterEvaluationMetrics", .mlClusteringDependencies(options)))
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
  if (!is.null(jaspResults[["plot2dCluster"]]) || !options[["plot2dCluster"]]) {
    return()
  }
  plot <- createJaspPlot(plot = NULL, title = gettext("t-SNE Cluster Plot"), width = 450, height = 300)
  plot$position <- position
  plot$dependOn(options = c("plot2dCluster", "labels", .mlClusteringDependencies(options)))
  jaspResults[["plot2dCluster"]] <- plot
  if (!ready) {
    return()
  }
  clusterResult <- jaspResults[["clusterResult"]]$object
  if (options[["seedBox"]]) {
    set.seed(options[["seed"]])
  }
  startProgressbar(2)
  progressbarTick()
  dataset <- unique(dataset)
  if (is.null(jaspResults[["tsneOutput"]])) {
    tsne <- Rtsne::Rtsne(as.matrix(dataset), perplexity = nrow(dataset) / 4, check_duplicates = FALSE)
    jaspResults[["tsneOutput"]] <- createJaspState(tsne)
    jaspResults[["tsneOutput"]]$dependOn(options = c("predictors", "seedBox", "seed"))
  } else {
    tsne <- jaspResults[["tsneOutput"]]$object
  }
  if (type == "cmeans") {
    fit <- e1071::cmeans(dataset, centers = clusterResult[["clusters"]], iter.max = options[["noOfIterations"]], m = options[["m"]])
    predictions <- fit$cluster
    colSize <- clusterResult[["clusters"]]
  } else if (type == "kmeans") {
    if (options[["centers"]] == "means") {
      fit <- kmeans(dataset, centers = clusterResult[["clusters"]], iter.max = options[["noOfIterations"]], nstart = options[["noOfRandomSets"]], algorithm = options[["algorithm"]])
    } else if (options[["centers"]] == "medians") {
      fit <- Gmedian::kGmedian(dataset, ncenters = clusterResult[["clusters"]], nstart = options[["noOfIterations"]], nstartkmeans = options[["noOfRandomSets"]])
    } else if (options[["centers"]] == "medoids") {
      if (options[["algorithm"]] == "pam") {
        fit <- cluster::pam(dataset, k = clusterResult[["clusters"]], metric = options[["distance"]], nstart = options[["noOfRandomSets"]])
      } else {
        fit <- cluster::clara(dataset, k = clusterResult[["clusters"]], metric = options[["distance"]], samples = options[["noOfIterations"]])
      }
    }
    predictions <- if (options[["centers"]] == "medoids") fit$clustering else fit$cluster
    colSize <- clusterResult[["clusters"]]
  } else if (type == "hierarchical") {
    if (options[["distance"]] == "Pearson correlation") {
      fit <- cutree(hclust(as.dist(1 - cor(t(dataset), method = "pearson")), method = options[["linkage"]]), k = clusterResult[["clusters"]])
    } else {
      fit <- cutree(hclust(.mlClusteringCalculateDistances(dataset), method = options[["linkage"]]), k = clusterResult[["clusters"]])
    }
    predictions <- fit
    colSize <- clusterResult[["clusters"]]
  } else if (type == "densitybased") {
    if (options[["distance"]] == "Correlated densities") {
      fit <- dbscan::dbscan(as.dist(1 - cor(t(dataset), method = "pearson")), eps = options[["eps"]], minPts = options[["minPts"]])
    } else {
      fit <- dbscan::dbscan(dataset, eps = options[["eps"]], minPts = options[["minPts"]])
    }
    predictions <- fit$cluster
    colSize <- clusterResult[["clusters"]] + 1
  } else if (type == "randomForest") {
    fit <- randomForest::randomForest(
      x = dataset, y = NULL, ntree = options[["noOfTrees"]],
      proximity = TRUE, oob.prox = TRUE
    )
    hfit <- hclust(as.dist(1 - fit$proximity), method = "ward.D2")
    predictions <- cutree(hfit, k = clusterResult[["clusters"]])
    colSize <- clusterResult[["clusters"]]
  }
  clusterAssignment <- factor(predictions, levels = sort(unique(predictions), decreasing = FALSE))
  if (type == "densitybased") {
    levels(clusterAssignment)[levels(clusterAssignment) == "0"] <- gettext("Noisepoint")
  }
  plotData <- data.frame(x = tsne$Y[, 1], y = tsne$Y[, 2], cluster = factor(clusterAssignment))
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(plotData$x)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(plotData$y)
  p <- ggplot2::ggplot(data = plotData, mapping = ggplot2::aes(x = x, y = y)) +
    jaspGraphs::geom_point(mapping = ggplot2::aes(fill = cluster)) +
    ggplot2::scale_x_continuous(name = NULL, limits = range(xBreaks)) +
    ggplot2::scale_y_continuous(name = NULL, limits = range(yBreaks)) +
    ggplot2::scale_fill_manual(name = gettext("Cluster"), values = .mlColorScheme(colSize)) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw(legend.position = if (options[["legend"]]) "right" else "none") +
    ggplot2::theme(axis.ticks = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank(), axis.text.y = ggplot2::element_blank())
  if (options[["labels"]]) {
    p <- p + ggrepel::geom_text_repel(ggplot2::aes(label = rownames(dataset), x = x, y = y), hjust = -1, vjust = 1, data = plotData)
  }
  progressbarTick()
  plot$plotObject <- p
}

.mlClusteringPlotElbow <- function(dataset, options, jaspResults, ready, position) {
  if (!is.null(jaspResults[["optimPlot"]]) || !options[["withinssPlot"]] || options[["modelOpt"]] == "validationManual") {
    return()
  }
  plot <- createJaspPlot(plot = NULL, title = gettext("Elbow Method Plot"), width = 400, height = 300)
  plot$position <- position
  plot$dependOn(options = c("withinssPlot", .mlClusteringDependencies(options)))
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
  requiredPoint <- switch(options[["optimizationCriterion"]],
    "validationAIC" = gettext("AIC"),
    "validationBIC" = gettext("BIC"),
    "validationSilh" = ""
  )
  plotData <- data.frame(x = rep(2:options[["maxClusters"]], 3), y = values, type = type)
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
  if (options[["optimizationCriterion"]] != "validationSilh") {
    p <- p + jaspGraphs::geom_point(data = pointData, ggplot2::aes(x = x, y = y, fill = "red"), inherit.aes = FALSE) +
      ggplot2::scale_fill_manual(name = NULL, labels = gettextf("Lowest %s", requiredPoint), values = "red")
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
  if (options[["predictors"]] != "") {
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
  if (!is.null(jaspResults[["clusterDensities"]]) || !options[["plotClusterDensities"]]) {
    return()
  }
  plot <- createJaspContainer(gettext("Cluster Density Plots"))
  plot$dependOn(options = c("plotClusterDensities", "oneFigureDensity", .mlClusteringDependencies(options)))
  plot$position <- position
  jaspResults[["clusterDensities"]] <- plot
  if (!ready) {
    return()
  }
  clusterResult <- jaspResults[["clusterResult"]]$object
  if (!options[["oneFigureDensity"]]) {
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
      ggplot2::scale_y_discrete(name = gettext("Variable")) +
      jaspGraphs::geom_rangeframe(sides = "b") +
      jaspGraphs::themeJaspRaw(legend.position = "right") +
      ggplot2::theme(axis.ticks.y = ggplot2::element_blank())
    plot[["oneFigure"]] <- createJaspPlot(plot = p, title = gettext("All Variables"), height = 150 * length(options[["predictors"]]), width = 600)
  }
}

.mlClusteringPlotMeans <- function(dataset, options, jaspResults, ready, position) {
  if (!is.null(jaspResults[["clusterMeans"]]) || !options[["plotClusterMeans"]]) {
    return()
  }
  plot <- createJaspContainer(gettext("Cluster Mean Plots"))
  plot$dependOn(options = c("plotClusterMeans", "showBars", "oneFigure", .mlClusteringDependencies(options)))
  plot$position <- position
  jaspResults[["clusterMeans"]] <- plot
  if (!ready) {
    return()
  }
  clusterResult <- jaspResults[["clusterResult"]]$object
  if (options[["oneFigure"]]) {
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
    if (options[["showBars"]]) {
      p <- p + ggplot2::geom_bar(color = "black", stat = "identity") +
        ggplot2::geom_errorbar(data = plotData, ggplot2::aes(x = xCoord, ymin = lower, ymax = upper), width = 0.2, size = 1)
    } else {
      p <- p + ggplot2::geom_segment(ggplot2::aes(x = 0, xend = max(plotData[["xCoord"]]), y = 0, yend = 0), linetype = 2) +
        ggplot2::geom_errorbar(data = plotData, ggplot2::aes(x = xCoord, ymin = lower, ymax = upper), width = 0.2, size = 1) +
        jaspGraphs::geom_point(color = "black")
    }
    p <- p + ggplot2::scale_x_continuous(name = NULL, breaks = xBreaks, labels = xLabels) +
      ggplot2::scale_y_continuous(name = gettext("Cluster Mean"), breaks = yBreaks, limits = range(yBreaks)) +
      jaspGraphs::geom_rangeframe(sides = "l") +
      jaspGraphs::themeJaspRaw(legend.position = "right") +
      ggplot2::theme(axis.ticks.x = ggplot2::element_blank(), axis.text.x = ggplot2::element_text(angle = 20))
    plot[["oneFigure"]] <- createJaspPlot(plot = p, title = gettext("All Variables"), height = 400, width = 200 * length(options[["predictors"]]))
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
      if (options[["showBars"]]) {
        p <- p + ggplot2::geom_bar(color = "black", stat = "identity") +
          ggplot2::geom_errorbar(data = plotData, ggplot2::aes(x = Cluster, ymin = lower, ymax = upper), width = 0.2, size = 1)
      } else {
        p <- p + ggplot2::geom_errorbar(data = plotData, ggplot2::aes(x = Cluster, ymin = lower, ymax = upper), width = 0.2, size = 1) +
          jaspGraphs::geom_point(color = "black")
      }
      p <- p + ggplot2::scale_x_discrete(name = gettext("Cluster"), breaks = xBreaks) +
        ggplot2::scale_y_continuous(name = variable, breaks = yBreaks, limits = range(yBreaks)) +
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
