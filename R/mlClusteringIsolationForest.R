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

mlClusteringIsolationForest <- function(jaspResults, dataset, options, ...) {

  # Preparatory work
  dataset <- .mlClusteringReadData(dataset, options)
  .mlIsoForestErrorHandling(dataset, options)

  # Check if analysis is ready to run
  ready <- .mlClusteringReady(options)

  # Compute results and create the model summary table
  .mlIsoForestTableSummary(dataset, options, jaspResults, ready, position = 1)

  # If the user wants to add the predictions to the data set
  .mlRegressionAddPredictionsToData(dataset, options, jaspResults, ready)

  # Create the table containing anomaly scores
  .mlIsoForestTableScores(dataset, options, jaspResults, ready, position = 2)

  # Create the plot
  .mlIsoForestMatrixPlot(dataset, options, jaspResults, ready, position = 3)
}

.mlIsoForestDependencies <- function(options) {
  opt <- c("predictors", "scaleVariables", "sampleSize", "nTrees", "numberOfPredictors", "scoringMetric")
  return(opt)
}

.mlIsoForestErrorHandling <- function(dataset, options) {
  predictors <- unlist(options$predictors)
  if (length(predictors[predictors != ""]) > 0L) {
    .hasErrors(dataset,
      type = c("infinity", "observations"), 
      all.target = predictors, observations.amount = "< 2", exitAnalysisIfErrors = TRUE
    )
  }
}

.mlIsoForestComputeResults <- function(dataset, options, jaspResults, ready, type) {
  if (!is.null(jaspResults[["regressionResult"]])) {
    return()
  }
  if (ready) {
      fit <- isotree::isolation.forest(data = dataset, sample_size = options[["sampleSize"]], ntrees = options[["nTrees"]],
                                   ndim = options[["numberOfPredictors"]], standardize_data = FALSE, scoring_metric = options[["scoringMetric"]])
  scores <- predict(fit, newdata = dataset)
  result <- list()
  result[["model"]] <- fit
  result[["values"]] <- scores
  result[["N"]] <- nrow(dataset)
    jaspResults[["regressionResult"]] <- createJaspState(result)
    jaspResults[["regressionResult"]]$dependOn(options = .mlIsoForestDependencies(options))
  }
}

.mlIsoForestTableSummary <- function(dataset, options, jaspResults, ready, position) {
  if (!is.null(jaspResults[["anomalyTable"]])) {
    return()
  }
  table <- createJaspTable(gettext("Isolation Forest Clustering"))
  table$position <- position
  table$dependOn(options = c(.mlIsoForestDependencies(options), "cutoff"))
  table$addColumnInfo(name = "ntrees", title = gettext("Trees"), type = "integer")
  table$addColumnInfo(name = "npred", title = gettext("Features per split"), type = "integer")
  table$addColumnInfo(name = "n", title = "N", type = "integer")
  table$addColumnInfo(name = "nabove", title = "Anomalies", type = "integer")
  if (!ready) {
    table$addFootnote(gettext("Please provide at least 2 features."))
  } else {
    table$addFootnote(gettextf("The number of anomalies are based on a threshold of %1$s.", options[["cutoff"]]))
  }
  jaspResults[["anomalyTable"]] <- table
  if (!ready) {
    return()
  }
  .mlIsoForestComputeResults(dataset, options, jaspResults, ready)
  anomalyResult <- jaspResults[["regressionResult"]]$object

  if (!options[["scaleVariables"]]) {
    table$addFootnote(gettext("The features in the model are <b>unstandardized</b>."))
  }
  row <- data.frame(
    n = anomalyResult[["N"]],
    nabove = length(which(anomalyResult[["values"]] >= options[["cutoff"]])),
	ntrees = options[["nTrees"]],
	npred = options[["numberOfPredictors"]]
  )
  table$addRows(row)
}

.mlIsoForestTableScores <- function(dataset, options, jaspResults, ready, position) {
  if (!is.null(jaspResults[["tableAnomalyScores"]]) || !options[["tableAnomalyScores"]]) {
    return()
  }
  table <- createJaspTable(gettext("Anomalies"))
  table$dependOn(options = c(
    "tableAnomalyScores", "cutoff", "tableAnomalyScoresFeatures", .mlIsoForestDependencies(options)
  ))
  table$position <- position
  table$addColumnInfo(name = "row", title = gettext("Row"), type = "integer")
  table$addColumnInfo(name = "score", title = gettext("Anomaly Score"), type = "number")
  if (options[["tableAnomalyScoresFeatures"]]) {
	types <- ifelse(apply(dataset, 2, is.factor), "number", "string")
	print(types)
    for (i in seq_len(ncol(dataset))) {
      table$addColumnInfo(name = colnames(dataset)[i], title = colnames(dataset)[i], type = types[i])
    } 
  }
  jaspResults[["tableAnomalyScores"]] <- table
  if (!ready) {
    return()
  }
  anomalyResult <- jaspResults[["regressionResult"]]$object
  scores <- anomalyResult[["values"]]
  indexes <- which(scores > options[["cutoff"]])
  table[["row"]] <- indexes
  table[["score"]] <- scores[indexes]
  if (options[["tableAnomalyScoresFeatures"]]) {
    for (i in seq_len(ncol(dataset))) {
      table[[colnames(dataset)[i]]] <- dataset[[colnames(dataset)[i]]][indexes]
    } 
  }
  if (length(indexes) == 0L) {
    table$addFootnote(gettextf("There are no anomaly scores >= %1$s.", options[["cutoff"]]))
  } else {
    table$addFootnote(gettextf("Display is limited to anomaly scores >= %1$s.", options[["cutoff"]]))
  }
}

.mlIsoForestMatrixPlot <- function(dataset, options, jaspResults, ready, position) {
  if (!is.null(jaspResults[["matrixPlot"]]) || !options[["matrixPlot"]]) {
    return()
  }
  plot <- createJaspPlot(title = gettext("Anomaly Matrix Plot"), height = 400, width = 300)
  plot$position <- position
  plot$dependOn(options = c(.mlClusteringDependencies(options), "matrixPlot", "matrixPlotLabels", "matrixPlotPoints", "cutoff"))
  jaspResults[["matrixPlot"]] <- plot
  if (!ready || length(options[["predictors"]]) < 2) {
    return()
  }
  anomalyResult <- jaspResults[["regressionResult"]]$object
  variables <- options[["predictors"]]
  variables <- variables[!vapply(dataset[, variables], is.factor, TRUE)] # remove factors from boundary plot
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
        plotData <- data.frame(x = predictors[, 1], y = predictors[, 2], score = anomalyResult[["values"]])
        xBreaks <- jaspGraphs::getPrettyAxisBreaks(plotData$x, min.n = 4)
        yBreaks <- jaspGraphs::getPrettyAxisBreaks(plotData$y, min.n = 4)
gridModel <- isotree::isolation.forest(data = plotData[, 1:2], sample_size = options[["sampleSize"]], ntrees = options[["nTrees"]],
                                               ndim = options[["numberOfPredictors"]], standardize_data = FALSE, scoring_metric = options[["scoringMetric"]])
		grid <- expand.grid(seq(min(xBreaks), max(xBreaks), length = 100), seq(min(yBreaks), max(yBreaks), length = 100))
		colnames(grid) <- c("x", "y")
		gridScores <- predict(gridModel, newdata = grid)
		gridData <- data.frame(x = grid[, 1], y = grid[, 2], score = gridScores)
        p <- ggplot2::ggplot(data = plotData, mapping = ggplot2::aes(x = x, y = y, fill = score)) +
		  ggplot2::geom_raster(data = gridData, mapping = ggplot2::aes(x = x, y = y, fill = score)) +
          ggplot2::scale_fill_gradient2(low = "green", mid = "white", high = "red", limits = c(0, 1), midpoint = 0.5) +
          ggplot2::scale_x_continuous(name = NULL, breaks = xBreaks, limits = range(xBreaks)) +
          ggplot2::scale_y_continuous(name = NULL, breaks = yBreaks, limits = range(yBreaks)) +
		  ggplot2::geom_contour(mapping = ggplot2::aes(z = score)) +
          jaspGraphs::geom_rangeframe() +
          jaspGraphs::themeJaspRaw()
		  if (options[["matrixPlotPoints"]]) {
			p <- p + jaspGraphs::geom_point()
		  if (options[["matrixPlotLabels"]]) {
            labelData <- subset(plotData, plotData$score >= options[["cutoff"]])
            p <- p + ggrepel::geom_text_repel(ggplot2::aes(label = rownames(labelData), x = x, y = y), hjust = -1, vjust = 1, data = labelData)
          }
		  }
        plotMat[[row - 1, col]] <- p
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
