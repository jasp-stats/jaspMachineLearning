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

.mlAnomalyDependencies <- function(options) {
  opt <- c(
    "predictors", "scaleVariables",                                                     # Common
    "maxDepth",                                                                         # Outlier tree
    "sampleSize", "nTrees", "numberOfPredictors", "scoringMetric", "cutoff",            # Isolation forest
    "weights", "degree", "gamma", "complexityParameter", "cost", "tolerance", "epsilon" # Svm
  )
  return(opt)
}

.mlAnomalyReadData <- function(dataset, options) {
  predictors <- unlist(options[["predictors"]])
  predictors <- predictors[predictors != ""]
  if (is.null(dataset)) {
    factorIndices <- unlist(lapply(options[["predictors"]], .columnIsNominal)) | unlist(lapply(options[["predictors"]], .columnIsNominalText)) | unlist(lapply(options[["predictors"]], .columnIsOrdinal))
    dataset <- .readDataSetToEnd(columns.as.numeric = options[["predictors"]][!factorIndices], columns.as.factor = options[["predictors"]][factorIndices])
  }
  if (options[["scaleVariables"]] && length(unlist(options[["predictors"]])) > 0) {
    dataset <- .scaleNumericData(dataset)
  }
  return(dataset)
}

.mlAnomalyReady <- function(options) {
  ready <- length(options[["predictors"]][options[["predictors"]] != ""]) >= 1
  return(ready)
}

.mlAnomalyErrorHandling <- function(dataset, options) {
  predictors <- unlist(options$predictors)
  if (length(predictors[predictors != ""]) > 0L) {
    .hasErrors(dataset,
      type = c("infinity", "observations"),
      all.target = predictors, observations.amount = "< 2", exitAnalysisIfErrors = TRUE
    )
  }
}

.mlAnomalyTableSummary <- function(dataset, options, jaspResults, ready, position, type) {
  if (!is.null(jaspResults[["anomalyTable"]])) {
    return()
  }
  title <- switch(type,
   "outliertree" = gettext("Outlier Tree Anomaly Detection"),
   "isoforest" = gettext("Isolation Forest Anomaly Detection"),
   "svm" = gettext("Support Vector Machine Anomaly Detection")
  )
  table <- createJaspTable(title)
  table$position <- position
  table$dependOn(options = .mlAnomalyDependencies(options))
  if (type == "isoforest") {
    table$addColumnInfo(name = "ntrees", title = gettext("Trees"), type = "integer")
    table$addColumnInfo(name = "npred", title = gettext("Features per split"), type = "integer")
  } else if (type == "svm") {
    table$addColumnInfo(name = "svec", title = gettext("Support vectors"), type = "integer")
  }
  table$addColumnInfo(name = "N", title = "N", type = "integer")
  table$addColumnInfo(name = "n", title = gettext("Anomalies"), type = "integer")
  if (!ready) {
    table$addFootnote(gettext("Please provide at least 1 feature."))
  } else if (type == "isoforest") {
    table$addFootnote(gettextf("The number of anomalies is based on a threshold of %1$s.", options[["cutoff"]]))
  }
  jaspResults[["anomalyTable"]] <- table
  if (!ready) {
    return()
  }
  .mlAnomalyComputeResults(dataset, options, jaspResults, ready, type)
  anomalyResult <- jaspResults[["anomalyResult"]]$object

  if (!options[["scaleVariables"]]) {
    table$addFootnote(gettext("The features in the model are <b>unstandardized</b>."))
  }
  row <- data.frame(N = anomalyResult[["N"]], n = anomalyResult[["noutlier"]])
  if (type == "isoforest") {
    row <- cbind(row, ntrees = options[["nTrees"]], npred = options[["numberOfPredictors"]])
  } else if (type == "svm") {
	row <- cbind(row, svec = nrow(anomalyResult[["model"]]$SV))
  }
  table$addRows(row)
}

.mlAnomalyComputeResults <- function(dataset, options, jaspResults, ready, type) {
  if (!is.null(jaspResults[["anomalyResult"]]) || !ready) {
    return()
  }
  # set the seed so that every time the same set is chosen (to prevent random results) ##
  if (options[["setSeed"]]) {
    .setSeedJASP(options)
  }
  result <- switch(type,
    "isoforest" = .mlIsoForestComputeResults(dataset, options),
    "outliertree" = .mlOutlierTreeComputeResults(dataset, options),
    "svm" = .mlSvmAnomalyComputeResults(dataset, options)

  )
  jaspResults[["anomalyResult"]] <- createJaspState(result)
  jaspResults[["anomalyResult"]]$dependOn(options = .mlAnomalyDependencies(options))
}

.mlAnomalyTableScores <- function(dataset, options, jaspResults, ready, position, type) {
  if (!is.null(jaspResults[["tableAnomalyScores"]]) || !options[["tableAnomalyScores"]]) {
    return()
  }
  table <- createJaspTable(gettext("Detected Anomalies"))
  table$dependOn(options = c("tableAnomalyScores", "tableAnomalyScoresFeatures", .mlAnomalyDependencies(options)))
  table$position <- position
  table$addColumnInfo(name = "row", title = gettext("Case"), type = "integer")
  table$addColumnInfo(name = "pred", title = gettext("Predicted"), type = "string")
  if (type == "isoforest") {
    table$addColumnInfo(name = "score", title = gettext("Score"), type = "number")
  }
  if (options[["tableAnomalyScoresFeatures"]]) {
    types <- ifelse(unlist(lapply(dataset[, options[["predictors"]]], is.factor)), yes = "string", no = "number")
    for (i in seq_len(ncol(dataset[, options[["predictors"]], drop = FALSE]))) {
      table$addColumnInfo(name = options[["predictors"]][i], title = options[["predictors"]][i], type = types[i])
    }
  }
  jaspResults[["tableAnomalyScores"]] <- table
  if (!ready) {
    return()
  }
  anomalyResult <- jaspResults[["anomalyResult"]]$object
  indexes <- anomalyResult[["ioutlier"]]
  table[["row"]] <- indexes
  table[["pred"]] <- as.character(anomalyResult[["classes"]][indexes])
  if (type == "isoforest") {
    table[["score"]] <- anomalyResult[["values"]][indexes]
  }
  if (options[["tableAnomalyScoresFeatures"]]) {
    for (i in seq_len(ncol(dataset))) {
      table[[options[["predictors"]][i]]] <- switch(types[i],
        "number" = dataset[indexes, options[["predictors"]][i]],
        "string" = as.character(dataset[indexes, options[["predictors"]][i]])
      )
    }
  }
  if (length(indexes) == 0L) {
    table$addFootnote(gettext("There are no anomalies detected."))
  }
}

.mlAnomalyMatrixPlot <- function(dataset, options, jaspResults, ready, position, type) {
  if (!is.null(jaspResults[["matrixPlot"]]) || !options[["matrixPlot"]]) {
    return()
  }
  plot <- createJaspPlot(title = gettext("Anomaly Matrix Plot"), height = 400, width = 300)
  plot$position <- position
  plot$dependOn(options = c(.mlAnomalyDependencies(), "matrixPlot", "matrixPlotLabels", "matrixPlotPoints"))
  jaspResults[["matrixPlot"]] <- plot
  if (!ready || length(options[["predictors"]]) < 2) {
    return()
  }
  anomalyResult <- jaspResults[["anomalyResult"]]$object
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
  plotMat <- matrix(list(), l - 1, l - 1)
  oldFontSize <- jaspGraphs::getGraphOption("fontsize")
  jaspGraphs::setGraphOption("fontsize", .85 * oldFontSize)
  startProgressbar(length(plotMat) + 1)
  for (row in 2:l) {
    for (col in seq_len(l - 1)) {
      if (col < row) {
        predictors <- dataset[, variables]
        predictors <- predictors[, c(col, row)]
        plotData <- data.frame(x = predictors[, 1], y = predictors[, 2], score = anomalyResult[["values"]], outlier = anomalyResult[["outlier"]])
        xBreaks <- jaspGraphs::getPrettyAxisBreaks(plotData$x, min.n = 4)
        yBreaks <- jaspGraphs::getPrettyAxisBreaks(plotData$y, min.n = 4)
        p <- ggplot2::ggplot(data = plotData, mapping = ggplot2::aes(x = x, y = y, fill = score)) +
          ggplot2::scale_fill_gradient2(low = "lightgray", high = "firebrick", limits = c(0, 1), midpoint = 0.5) +
          jaspGraphs::geom_point() +
          ggplot2::scale_x_continuous(name = NULL, breaks = xBreaks, limits = range(xBreaks)) +
          ggplot2::scale_y_continuous(name = NULL, breaks = yBreaks, limits = range(yBreaks)) +
          jaspGraphs::geom_rangeframe() +
          jaspGraphs::themeJaspRaw()
          if (options[["matrixPlotLabels"]]) {
            labelData <- subset(plotData, plotData$outlier)
            p <- p + ggrepel::geom_text_repel(ggplot2::aes(label = rownames(labelData), x = x, y = y), hjust = -1, vjust = 1, data = labelData)
          }
        plotMat[[row - 1, col]] <- p
      } else if (row == 2 && col == 2) {
        plotMat[[row - 1, col]] <- .legendPlotAnomaly(dataset, options, type)
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

.legendPlotAnomaly <- function(dataset, options, type) {
  plotData <- data.frame(x = c(1, 1), y = c(1, 1), type = c(gettext("Anomaly"), gettext("Standard")))
  p <- ggplot2::ggplot(data = plotData, ggplot2::aes(y = y, x = x, show.legend = TRUE)) +
    ggplot2::xlab(NULL) +
    ggplot2::ylab(NULL) +
    ggplot2::theme(legend.key = ggplot2::element_blank())
	if (type == "isoforest") {
		p <- p + jaspGraphs::geom_point(ggplot2::aes(fill = y), alpha = 0) +
		ggplot2::scale_fill_gradient2(name = gettext("Predicted Score"), low = "lightgray", high = "firebrick",  limits = c(0, 1), midpoint = 0.5, breaks = c(0, 0.25, 0.5, 0.75, 1))
	} else {
		p <- p + jaspGraphs::geom_point(ggplot2::aes(fill = type), alpha = 0) +
		ggplot2::scale_fill_manual(name = gettext("Predicted"), values = c("firebrick", "lightgray"))
	}
	p <- p + jaspGraphs::geom_rangeframe(sides = "") +
    jaspGraphs::themeJaspRaw(legend.position = "left") +
    ggplot2::theme(axis.ticks = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank(), axis.text.y = ggplot2::element_blank()) +
    ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(alpha = 1)))
  return(p)
}

.mlAnomalyAddPredictionsToData <- function(dataset, options, jaspResults, ready) {
  if (!ready || !options[["addPredictions"]] || options[["predictionsColumn"]] == "") {
    return()
  }
  anomalyResult <- jaspResults[["anomalyResult"]]$object
  if (is.null(jaspResults[["predictionsColumn"]])) {
    predictions <- as.character(anomalyResult[["classes"]])
    predictionsColumn <- rep(NA, max(as.numeric(rownames(dataset))))
    predictionsColumn[as.numeric(rownames(dataset))] <- predictions
    predictionsColumn <- factor(predictionsColumn)
    jaspResults[["predictionsColumn"]] <- createJaspColumn(columnName = options[["predictionsColumn"]])
    jaspResults[["predictionsColumn"]]$dependOn(options = c(.mlAnomalyDependencies(options), "predictionsColumn", "addPredictions"))
    jaspResults[["predictionsColumn"]]$setNominal(predictionsColumn)
  }
}
