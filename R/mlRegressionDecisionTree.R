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

mlRegressionDecisionTree <- function(jaspResults, dataset, options, state = NULL) {

  # Preparatory work
  dataset <- .readDataRegressionAnalyses(dataset, options)
  .mlRegressionErrorHandling(dataset, options, type = "rpart")

  # Check if analysis is ready to run
  ready <- .mlRegressionReady(options, type = "rpart")

  # Compute results and create the model summary table
  .mlRegressionTableSummary(dataset, options, jaspResults, ready, position = 1, type = "rpart")

  # If the user wants to add the values to the data set
  .mlRegressionAddPredictionsToData(dataset, options, jaspResults, ready)

  # Add test set indicator to data
  .mlAddTestIndicatorToData(options, jaspResults, ready, purpose = "regression")

  # Create the data split plot
  .mlPlotDataSplit(dataset, options, jaspResults, ready, position = 2, purpose = "regression", type = "rpart")

  # Create the evaluation metrics table
  .mlRegressionTableMetrics(dataset, options, jaspResults, ready, position = 3)

  # Create the splits table
  .mlDecisionTreeTableSplits(options, jaspResults, ready, position = 4, purpose = "regression")

  # Create the variable importance table
  .mlDecisionTreeTableVarImp(options, jaspResults, ready, position = 5, purpose = "regression")

  # Create the predicted performance plot
  .mlRegressionPlotPredictedPerformance(options, jaspResults, ready, position = 6)

  # Create the decision tree plot
  .mlDecisionTreePlotTree(dataset, options, jaspResults, ready, position = 7, purpose = "regression")
}

.decisionTreeRegression <- function(dataset, options, jaspResults, ready) {
  # Import model formula from jaspResults
  formula <- jaspResults[["formula"]]$object
  # Split the data into training and test sets
  if (options[["holdoutData"]] == "testSetIndicator" && options[["testSetIndicatorVariable"]] != "") {
    # Select observations according to a user-specified indicator (included when indicator = 1)
    trainingIndex <- which(dataset[, options[["testSetIndicatorVariable"]]] == 0)
  } else {
    # Sample a percentage of the total data set
    trainingIndex <- sample.int(nrow(dataset), size = ceiling((1 - options[["testDataManual"]]) * nrow(dataset)))
  }
  trainingSet <- dataset[trainingIndex, ]
  # Create the generated test set indicator
  testIndicatorColumn <- rep(1, nrow(dataset))
  testIndicatorColumn[trainingIndex] <- 0
  # Just create a train and a test set (no optimization)
  testSet <- dataset[-trainingIndex, ]
  trainingFit <- rpart::rpart(
    formula = formula, data = trainingSet, method = "anova", x = TRUE, y = TRUE,
    control = rpart::rpart.control(minsplit = options[["nSplit"]], minbucket = options[["nNode"]], maxdepth = options[["intDepth"]], cp = options[["cp"]])
  )
  # Use the specified model to make predictions for dataset
  testPredictions <- predict(trainingFit, newdata = testSet)
  dataPredictions <- predict(trainingFit, newdata = dataset)
  # Create results object
  result <- list()
  result[["formula"]] <- formula
  result[["model"]] <- trainingFit
  result[["testMSE"]] <- mean((testPredictions - testSet[, options[["target"]]])^2)
  result[["ntrain"]] <- nrow(trainingSet)
  result[["train"]] <- trainingSet
  result[["ntest"]] <- nrow(testSet)
  result[["testReal"]] <- testSet[, options[["target"]]]
  result[["testPred"]] <- testPredictions
  result[["testIndicatorColumn"]] <- testIndicatorColumn
  result[["values"]] <- dataPredictions
  return(result)
}

.mlDecisionTreeTableVarImp <- function(options, jaspResults, ready, position, purpose) {
  if (!is.null(jaspResults[["tableVariableImportance"]]) || !options[["tableVariableImportance"]]) {
    return()
  }
  table <- createJaspTable(title = gettext("Variable Importance"))
  table$position <- position
  table$dependOn(options = c(
    "tableVariableImportance", "trainingDataManual", "scaleEqualSD", "target", "predictors", "seed", "seedBox",
    "testSetIndicatorVariable", "testSetIndicator", "holdoutData", "testDataManual", "nSplit", "nNode", "intDepth", "cp"
  ))
  table$addColumnInfo(name = "predictor", title = " ", type = "string")
  table$addColumnInfo(name = "imp", title = gettext("Relative Importance"), type = "number")
  jaspResults[["tableVariableImportance"]] <- table
  if (!ready) {
    return()
  }
  result <- switch(purpose,
    "classification" = jaspResults[["classificationResult"]]$object,
    "regression" = jaspResults[["regressionResult"]]$object
  )
  varImpOrder <- sort(result[["model"]][["variable.importance"]], decreasing = TRUE)
  table[["predictor"]] <- as.character(names(varImpOrder))
  table[["imp"]] <- as.numeric(varImpOrder) / sum(as.numeric(varImpOrder)) * 100
}

.mlDecisionTreeTableSplits <- function(options, jaspResults, ready, position, purpose) {
  if (!is.null(jaspResults[["tableSplits"]]) || !options[["tableSplits"]]) {
    return()
  }
  table <- createJaspTable(title = gettext("Splits in Tree"))
  table$position <- position
  table$dependOn(options = c(
    "tableSplits", "trainingDataManual", "scaleEqualSD", "target", "predictors", "seed", "seedBox",
    "testSetIndicatorVariable", "testSetIndicator", "holdoutData", "testDataManual", "nSplit", "nNode", "intDepth", "cp"
  ))
  table$addColumnInfo(name = "predictor", title = " ", type = "string")
  table$addColumnInfo(name = "index", title = gettext("Split Point"), type = "number")
  table$addColumnInfo(name = "count", title = gettext("Obs. in Split"), type = "integer")
  table$addColumnInfo(name = "improve", title = gettext("Improvement"), type = "number")
  jaspResults[["tableSplits"]] <- table
  if (!ready) {
    return()
  }
  result <- switch(purpose,
    "classification" = jaspResults[["classificationResult"]]$object,
    "regression" = jaspResults[["regressionResult"]]$object
  )
  if (is.null(result[["model"]]$splits)) {
    table$addFootnote(gettext("No splits were made in the tree."))
    return()
  }
  splits <- result[["model"]]$splits
  table[["predictor"]] <- rownames(splits)
  table[["index"]] <- splits[, 4]
  table[["count"]] <- splits[, 1]
  table[["improve"]] <- splits[, 3]
}

.mlDecisionTreePlotTree <- function(dataset, options, jaspResults, ready, position, purpose) {
  if (!is.null(jaspResults[["plotDecisionTree"]]) || !options[["plotDecisionTree"]]) {
    return()
  }
  plot <- createJaspPlot(plot = NULL, title = gettext("Decision Tree Plot"), width = 600, height = 500)
  plot$position <- position
  plot$dependOn(options = c(
    "plotDecisionTree", "trainingDataManual", "scaleEqualSD", "target", "predictors", "seed", "seedBox",
    "testSetIndicatorVariable", "testSetIndicator", "holdoutData", "testDataManual", "nNode", "nSplit", "intDepth", "cp"
  ))
  jaspResults[["plotDecisionTree"]] <- plot
  if (!ready) {
    return()
  }
  result <- switch(purpose,
    "classification" = jaspResults[["classificationResult"]]$object,
    "regression" = jaspResults[["regressionResult"]]$object
  )
  result[["model"]]$call$data <- result[["train"]] # Required
  plotData <- partykit::as.party(result[["model"]])
  p <- ggparty::ggparty(plotData)
  # The following lines come from rpart:::print.rpart()
  x <- result[["model"]]
  frame <- x$frame
  ylevel <- attr(x, "ylevels")
  digits <- 3
  tfun <- (x$functions)$print
  if (!is.null(tfun)) {
    if (is.null(frame$yval2)) {
      yval <- tfun(frame$yval, ylevel, digits)
    } else {
      yval <- tfun(frame$yval2, ylevel, digits)
    }
  } else {
    yval <- format(signif(frame$yval, digits))
  }
  leafs <- which(x$frame$var == "<leaf>")
  labels <- yval[leafs]
  if (purpose == "classification") {
    labels <- strsplit(labels, split = " ")
    labels <- unlist(lapply(labels, `[[`, 1))
  }
  nodeNames <- p$data$splitvar
  nodeNames[is.na(nodeNames)] <- labels
  p$data$info <- paste0(nodeNames, "\nn = ", p$data$nodesize)
  p <- p + ggparty::geom_edge() +
    ggparty::geom_edge_label(mapping = ggplot2::aes(label = paste(substr(breaks_label, start = 1, stop = 15))), fill = NA) +
    ggparty::geom_node_splitvar(mapping = ggplot2::aes(size = max(3, nodesize) / 2, label = info), fill = "white", col = "black") +
    ggparty::geom_node_label(mapping = ggplot2::aes(label = info, size = max(3, nodesize) / 2), ids = "terminal", fill = "white", col = "black") +
    ggplot2::scale_x_continuous(name = NULL, limits = c(min(p$data$x) - abs(0.1 * min(p$data$x)), max(p$data$x) * 1.1)) +
    ggplot2::scale_y_continuous(name = NULL, limits = c(min(p$data$y) - abs(0.1 * min(p$data$y)), max(p$data$y) * 1.1)) +
    jaspGraphs::geom_rangeframe(sides = "") +
    jaspGraphs::themeJaspRaw() +
    ggplot2::theme(
      axis.ticks = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank()
    )
  plot$plotObject <- p
}
