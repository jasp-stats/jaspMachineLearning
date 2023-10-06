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

mlClusteringDensityBased <- function(jaspResults, dataset, options, ...) {

  # Preparatory work
  dataset <- .mlClusteringReadData(dataset, options)
  .mlClusteringErrorHandling(dataset, options, type = "densitybased")

  # Check if analysis is ready to run
  ready <- .mlClusteringReady(options)

  # Compute results and create the model summary table
  .mlClusteringTableSummary(dataset, options, jaspResults, ready, position = 1, type = "densitybased")

  # If the user wants to add the clusters to the data set
  .mlClusteringAddPredictionsToData(dataset, options, jaspResults, ready)

  # Create the cluster information table
  .mlClusteringTableInformation(options, jaspResults, ready, position = 2, type = "densitybased")

  # Create the cluster evaluation metrics table
  .mlClusteringTableMetrics(dataset, options, jaspResults, ready, position = 3)

  # Create the cluster means table
  .mlClusteringTableMeans(dataset, options, jaspResults, ready, position = 4)

  # Create the k-distance plot
  .mlClusteringDensityBasedPlotError(dataset, options, jaspResults, ready, position = 5)

  # Create the cluster plot
  .mlClusteringPlotTsne(dataset, options, jaspResults, ready, position = 6, type = "densitybased")

  # Create the matrix plot
  .mlClusteringMatrixPlot(dataset, options, jaspResults, ready, position = 7)

  # Create the cluster means plot
  .mlClusteringPlotMeans(dataset, options, jaspResults, ready, position = 8)

  # Create the cluster densities plot
  .mlClusteringPlotDensities(dataset, options, jaspResults, ready, position = 9)
}

.densityBasedClustering <- function(dataset, options, jaspResults) {
  if (options[["distance"]] == "correlatedDensities") {
    fit <- dbscan::dbscan(as.dist(1 - cor(t(as.data.frame(dataset[, options[["predictors"]]])), method = "pearson")), eps = options[["epsilonNeighborhoodSize"]], minPts = options[["minCorePoints"]])
  } else {
    fit <- dbscan::dbscan(as.data.frame(dataset[, options[["predictors"]]]), eps = options[["epsilonNeighborhoodSize"]], minPts = options[["minCorePoints"]])
  }
  noisePoints <- length(fit[["cluster"]][fit[["cluster"]] == 0])
  clusters <- ifelse(noisePoints > 0, yes = length(table(fit[["cluster"]])) - 1, no = length(table(fit[["cluster"]])))
  m <- dim(as.data.frame(dataset[, options[["predictors"]]]))[2]
  wss <- numeric(clusters)
  for (i in 1:clusters) {
    wss[i] <- if (m == 1) .ss(dataset[, options[["predictors"]]][fit[["cluster"]] == i]) else .ss(dataset[, options[["predictors"]]][fit[["cluster"]] == i, ])
  }
  predictions <- fit[["cluster"]]
  nullClusters <- oneClusters <- 0
  for (i in 1:length(fit[["cluster"]])) {
    if (fit[["cluster"]][i] == 0) {
      nullClusters <- nullClusters + 1
    } else if (fit[["cluster"]][i] == 1) {
      oneClusters <- oneClusters + 1
    }
  }
  zeroMark <- nullClusters == length(predictions)
  oneMark <- oneClusters == length(predictions)
  if (!oneMark && !zeroMark) {
    if (options[["distance"]] == "normalDensities") {
      silhouettes <- summary(cluster::silhouette(predictions, .mlClusteringCalculateDistances(dataset[, options[["predictors"]]])))
    } else if (options[["distance"]] == "correlatedDensities") {
      silhouettes <- summary(cluster::silhouette(predictions, as.dist(1 - cor(t(dataset[, options[["predictors"]]])))))
    }
  } else {
    silhouettes <- list("avg.width" = 0, "clus.avg.widths" = rep(0, max(1, clusters)))
  }
  result <- list()
  result[["pred.values"]] <- predictions
  result[["clusters"]] <- clusters
  result[["N"]] <- nrow(dataset)
  result[["size"]] <- as.data.frame(table(predictions))[, 2]
  result[["WSS"]] <- wss
  result[["TSS"]] <- if (noisePoints > 0) .tss(.mlClusteringCalculateDistances(dataset[, options[["predictors"]]][fit[["cluster"]] != 0, ])) else .tss(.mlClusteringCalculateDistances(dataset[, options[["predictors"]]]))
  result[["BSS"]] <- result[["TSS"]] - sum(result[["WSS"]])
  result[["AIC"]] <- sum(wss) + 2 * m * clusters
  result[["BIC"]] <- sum(wss) + log(length(predictions)) * m * clusters
  result[["Silh_score"]] <- silhouettes[["avg.width"]]
  result[["silh_scores"]] <- silhouettes[["clus.avg.widths"]]
  result[["zeroMark"]] <- zeroMark
  result[["oneMark"]] <- oneMark
  result[["noisePoints"]] <- noisePoints
  return(result)
}

.mlClusteringDensityBasedPlotError <- function(dataset, options, jaspResults, ready, position) {
  if (!is.null(jaspResults[["kdistPlot"]]) || !options[["kDistancePlot"]]) {
    return()
  }
  plot <- createJaspPlot(plot = NULL, title = gettext("K-Distance Plot"), width = 400, height = 300)
  plot$position <- position
  plot$dependOn(options = c(.mlClusteringDependencies(), "kDistancePlot"))
  jaspResults[["kdistPlot"]] <- plot
  if (!ready) {
    return()
  }
  uniqueRows <- which(!duplicated(dataset[, options[["predictors"]]]))
  data <- dataset[uniqueRows, options[["predictors"]]]
  if (options[["distance"]] == "correlatedDensities") {
    knnDist <- dbscan::kNNdist(as.dist(1 - cor(t(as.data.frame(data)), method = "pearson")), k = options[["minCorePoints"]])
  } else {
    knnDist <- dbscan::kNNdist(data, k = options[["minCorePoints"]])
  }
  knnValues <- seq(from = 1, to = length(knnDist), by = 1)
  knnDistances <- sort(knnDist)
  plotData <- data.frame(x = knnValues, y = knnDistances)
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(plotData$x, min.n = 4)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(0, plotData$y, options[["epsilonNeighborhoodSize"]]), min.n = 4)
  yKnee <- try(findCutoff(knnValues, knnDistances, method = "curvature")[["y"]])
  if (inherits(yKnee, "try-error")) { # this can cause a stackoverflow, in which case we abort and don't add it
    suggestedLine <- NULL
  } else {
    suggestedLine <- data.frame(xstart = xBreaks[1], xend = xBreaks[length(xBreaks)], ystart = yKnee, yend = yKnee)
  }
  lineData <- data.frame(xstart = xBreaks[1], xend = xBreaks[length(xBreaks)], ystart = options[["epsilonNeighborhoodSize"]], yend = options[["epsilonNeighborhoodSize"]])
  p <- ggplot2::ggplot(data = plotData, ggplot2::aes(x = x, y = y)) +
    ggplot2::scale_x_continuous(name = gettext("Points Sorted by Distance"), breaks = xBreaks, limits = c(0, max(xBreaks))) +
    ggplot2::scale_y_continuous(name = gettextf("%s-Nearest Neighbors \nDistance", options[["minCorePoints"]]), breaks = yBreaks, limits = range(yBreaks))
  if (!is.null(suggestedLine)) {
    p <- p + ggplot2::geom_segment(ggplot2::aes(x = xstart, xend = xend, y = ystart, yend = yend), data = suggestedLine, linetype = "dashed", color = "darkred") +
      ggplot2::geom_text(
        data = suggestedLine, ggplot2::aes(label = gettextf("Maximum curvature = %s", round(yend, 2)), x = xstart, y = yend + 0.05 * abs(yend)),
        color = "darkred", vjust = "inward", hjust = "inward"
      )
  }
  p <- p + ggplot2::geom_segment(ggplot2::aes(x = xstart, xend = xend, y = ystart, yend = yend), data = lineData, linetype = 2, color = "darkgray") +
    jaspGraphs::geom_line() +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()
  plot$plotObject <- p
}

# this function is a direct replicate of KneeArrower::findCutoff(), with added stackoverflow protection
# the algorithm should probably be rewritten in a way that does not require recursion
findCutoff <- function(x, y, method = "first", frac.of.steepest.slope = 0.5) {
  stack <- Cstack_info()[names(Cstack_info()) == "eval_depth"] * 6 # each run adds 6 to the stack
  if (getOption("expressions") <= (stack + 6)) {
    stop(gettext("End of recursion reached without converging"))
  }

  is.invalid <- function(x) {
    any((!is.numeric(x)) | is.infinite(x))
  }
  if (is.invalid(x) || is.invalid(y)) {
    stop(gettext("x and y must be numeric and finite. Missing values not allowed."))
  }
  if (length(x) != length(y)) {
    stop(gettext("x and y must be of equal length."))
  }

  new.x <- seq(from = min(x), to = max(x), length.out = length(x))
  sp <- smooth.spline(x, y)
  new.y <- predict(sp, new.x)$y
  largest.odd.num.lte <- function(x) {
    x.int <- floor(x)
    if (x.int %% 2 == 0) {
      x.int - 1
    } else {
      x.int
    }
  }
  smoothen <- function(y, p = p, filt.length = NULL, ...) {
    ts <- (max(new.x) - min(new.x)) / length(new.x)
    p <- 3
    if (is.null(filt.length)) {
      filt.length <- min(
        largest.odd.num.lte(length(new.x)),
        7
      )
    }
    if (filt.length <= p) {
      stop(gettext("Need more points to find cutoff."))
    }
    signal::sgolayfilt(y,
      p = p, n = filt.length, ts = ts,
      ...
    )
  }
  first.deriv <- smoothen(new.y, m = 1)
  second.deriv <- smoothen(new.y, m = 2)
  pick.sign <- function(x) {
    most.extreme <- which(abs(x) == max(abs(x), na.rm = TRUE))[1]
    sign(x[most.extreme])
  }
  first.deriv.sign <- pick.sign(first.deriv)
  second.deriv.sign <- pick.sign(second.deriv)
  x.sign <- 1
  y.sign <- 1
  if ((first.deriv.sign == -1) && (second.deriv.sign == -1)) {
    x.sign <- -1
  } else if ((first.deriv.sign == -1) && (second.deriv.sign ==
    1)) {
    y.sign <- -1
  } else if ((first.deriv.sign == 1) && (second.deriv.sign ==
    1)) {
    x.sign <- -1
    y.sign <- -1
  }
  if ((x.sign == -1) || (y.sign == -1)) {
    results <- findCutoff(x.sign * x, y.sign * y,
      method = method,
      frac.of.steepest.slope = frac.of.steepest.slope
    )
    return(list(x = x.sign * results$x, y = y.sign * results$y))
  }
  cutoff.x <- NA
  if (method == "first") {
    if (is.invalid(frac.of.steepest.slope)) {
      stop(gettext("Need to specify fraction of maximum slope."))
    }
    if (frac.of.steepest.slope <= 0 || frac.of.steepest.slope >
      1) {
      stop(gettext("Fraction of maximum slope must be positive and be less than or equal to 1."))
    }
    slope.cutoff <- frac.of.steepest.slope * max(first.deriv)
    cutoff.x <- findInverse(new.x, first.deriv, slope.cutoff)
  } else if (method == "curvature") {
    curvature <- abs(second.deriv) / (1 + first.deriv^2)^(3 / 2)
    cutoff.x <- findInverse(new.x, curvature, max(curvature))
  } else {
    stop(gettext("Method must be either 'first' or 'curvature'."))
  }
  if (is.na(cutoff.x)) {
    warning(gettext("Cutoff point is beyond range. Returning NA."))
    list(x = NA, y = NA)
  } else {
    approx(new.x, new.y, cutoff.x)
  }
}

findInverse <- function(x, y, y0) {
  if (y0 < min(y) | max(y) < y0) {
    return(NA)
  } else {
    f <- approxfun(x, y, rule = 1)
    return(optimize(function(x) abs(f(x) - y0), range(x))$minimum)
  }
}
