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

  # Create the variable importance table
  .mlDecisionTreeTableVarImp(options, jaspResults, ready, position = 4, purpose = "regression")

  # Create the shap table
  .mlTableShap(dataset, options, jaspResults, ready, position = 5, purpose = "regression")

  # Create the splits table
  .mlDecisionTreeTableSplits(options, jaspResults, ready, position = 6, purpose = "regression")

  # Create the predicted performance plot
  .mlRegressionPlotPredictedPerformance(options, jaspResults, ready, position = 7)

  # Create the optimization plot
  .mlDecisionTreePlotError(dataset, options, jaspResults, ready, position = 8, purpose = "regression")

  # Create the decision tree plot
  .mlDecisionTreePlotTree(dataset, options, jaspResults, ready, position = 9, purpose = "regression")
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
  trainingAndValidationSet <- dataset[trainingIndex, ]
  # Create the generated test set indicator
  testIndicatorColumn <- rep(1, nrow(dataset))
  testIndicatorColumn[trainingIndex] <- 0
  if (options[["modelOptimization"]] == "manual") {
    # Just create a train and a test set (no optimization)
    trainingSet <- trainingAndValidationSet
    testSet <- dataset[-trainingIndex, ]
    complexityPenalty <- options[["complexityParameter"]]
    trainingFit <- rpart::rpart(
      formula = formula, data = trainingSet, method = "anova", x = TRUE, y = TRUE,
      control = rpart::rpart.control(minsplit = options[["minObservationsForSplit"]],
      minbucket = options[["minObservationsInNode"]], maxdepth = options[["interactionDepth"]], cp = complexityPenalty)
    )
  } else if (options[["modelOptimization"]] == "optimized") {
    # Create a train, validation and test set (optimization)
    validationIndex <- sample.int(nrow(trainingAndValidationSet), size = ceiling(options[["validationDataManual"]] * nrow(trainingAndValidationSet)))
    testSet <- dataset[-trainingIndex, ]
    validationSet <- trainingAndValidationSet[validationIndex, ]
    trainingSet <- trainingAndValidationSet[-validationIndex, ]
    cps <- seq(0, options[["maxComplexityParameter"]], by = 0.01)
    errorStore <- trainErrorStore <- numeric(length(cps))
    startProgressbar(length(cps))
    for (i in seq_along(cps)) {
      trainingFit <- rpart::rpart(
        formula = formula, data = trainingSet, method = "anova", x = TRUE, y = TRUE,
        control = rpart::rpart.control(minsplit = options[["minObservationsForSplit"]], 
        minbucket = options[["minObservationsInNode"]], maxdepth = options[["interactionDepth"]], cp = cps[i])
      )
      errorStore[i] <- mean((predict(trainingFit, newdata = validationSet) - validationSet[, options[["target"]]])^2)
      trainErrorStore[i] <- mean((predict(trainingFit, newdata = trainingSet) - trainingSet[, options[["target"]]])^2)
      progressbarTick()
    }
    complexityPenalty <- cps[which.min(errorStore)]
    trainingFit <- rpart::rpart(
      formula = formula, data = trainingSet, method = "anova", x = TRUE, y = TRUE,
      control = rpart::rpart.control(minsplit = options[["minObservationsForSplit"]], 
      minbucket = options[["minObservationsInNode"]], maxdepth = options[["interactionDepth"]], cp = complexityPenalty)
    )
    validationPredictions <- predict(trainingFit, newdata = validationSet)
  }
  # Use the specified model to make predictions for dataset
  testPredictions <- predict(trainingFit, newdata = testSet)
  dataPredictions <- predict(trainingFit, newdata = dataset)
  # Create results object
  result <- list()
  result[["formula"]] <- formula
  result[["model"]] <- trainingFit
  result[["penalty"]] <- complexityPenalty
  result[["testMSE"]] <- mean((testPredictions - testSet[, options[["target"]]])^2)
  result[["ntrain"]] <- nrow(trainingSet)
  result[["train"]] <- trainingSet
  result[["test"]] <- testSet
  result[["ntest"]] <- nrow(testSet)
  result[["testReal"]] <- testSet[, options[["target"]]]
  result[["testPred"]] <- testPredictions
  result[["testIndicatorColumn"]] <- testIndicatorColumn
  result[["values"]] <- dataPredictions
  if (options[["modelOptimization"]] != "manual") {
    result[["accuracyStore"]] <- errorStore
    result[["validMSE"]] <- mean((validationPredictions - validationSet[, options[["target"]]])^2)
    result[["nvalid"]] <- nrow(validationSet)
    result[["valid"]] <- validationSet
    result[["trainAccuracyStore"]] <- trainErrorStore
  }
  result[["explainer"]] <- DALEX::explain(result[["model"]], type = "regression", data = result[["train"]][, options[["predictors"]], drop = FALSE], y = result[["train"]][, options[["target"]]], predict_function = function(model, data) predict(model, newdata = data))
  return(result)
}

.mlDecisionTreeTableVarImp <- function(options, jaspResults, ready, position, purpose) {
  if (!is.null(jaspResults[["featureImportanceTable"]]) || !options[["featureImportanceTable"]]) {
    return()
  }
  table <- createJaspTable(title = gettext("Feature Importance Metrics"))
  table$position <- position
  table$dependOn(options = c(
    "featureImportanceTable", "trainingDataManual", "scaleVariables", "target", "predictors", "seed", "setSeed",
    "testSetIndicatorVariable", "testSetIndicator", "holdoutData", "testDataManual", "minObservationsForSplit", "minObservationsInNode", "interactionDepth", "complexityParameter"
  ))
  table$addColumnInfo(name = "predictor", title = " ", type = "string")
  table$addColumnInfo(name = "imp", title = gettext("Relative Importance"), type = "number")
  table$addColumnInfo(name = "dl", title = gettext("Mean dropout loss"), type = "number")
  jaspResults[["featureImportanceTable"]] <- table
  if (!ready) {
    return()
  }
  result <- switch(purpose,
    "classification" = jaspResults[["classificationResult"]]$object,
    "regression" = jaspResults[["regressionResult"]]$object
  )
  if (is.null(result[["model"]][["variable.importance"]])) {
    table$addFootnote(gettext("No splits were made in the tree."))
    return()
  }
  varImpOrder <- sort(result[["model"]][["variable.importance"]], decreasing = TRUE)
  vars <- as.character(names(varImpOrder))
  table[["predictor"]] <- vars
  table[["imp"]] <- as.numeric(varImpOrder) / sum(as.numeric(varImpOrder)) * 100
  .setSeedJASP(options) # Set the seed to make results reproducible
  if (purpose == "regression") {
    fi <- DALEX::model_parts(result[["explainer"]], B = 50)
  } else if (purpose == "classification") {
    fi <- DALEX::model_parts(result[["explainer_fi"]], B = 50)
  }
  fi <- aggregate(x = fi[["dropout_loss"]], by = list(y = fi[["variable"]]), FUN = mean)
  table[["dl"]] <- fi[match(vars, fi[["y"]]), "x"]
  table$addFootnote(gettext("Mean dropout loss is based on 50 permutations."))
}

.mlDecisionTreeTableSplits <- function(options, jaspResults, ready, position, purpose) {
  if (!is.null(jaspResults[["splitsTable"]]) || !options[["splitsTable"]]) {
    return()
  }
  table <- createJaspTable(title = gettext("Splits in Tree"))
  table$position <- position
  if (purpose == "regression") {
    table$dependOn(options = c("splitsTable", "splitsTreeTable", .mlRegressionDependencies()))
  } else {
    table$dependOn(options = c("splitsTable", "splitsTreeTable", .mlClassificationDependencies()))
  }
  table$addColumnInfo(name = "predictor", title = "", type = "string")
  table$addColumnInfo(name = "count", title = gettext("Obs. in Split"), type = "integer")
  table$addColumnInfo(name = "index", title = gettext("Split Point"), type = "number")
  table$addColumnInfo(name = "improve", title = gettext("Improvement"), type = "number")
  jaspResults[["splitsTable"]] <- table
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
  } else if (options[["splitsTreeTable"]]) {
    table$addFootnote(gettext("For each level of the tree, only the split with the highest improvement in deviance is shown."))
  }
  splits <- result[["model"]]$splits
  if (options[["splitsTreeTable"]]) {
    # Only show the splits actually in the tree (aka with the highest OOB improvement)
    splits <- splits[splits[, 1] > 0, , drop = FALSE] # Discard the leaf splits 
    df <- as.data.frame(splits)
    df$names <- rownames(splits)
    df$group <- c(1, 1 + cumsum(splits[-1, 1] != splits[-nrow(df), 1]))
    splitList <- split(df, f = df$group)
    rows <- as.data.frame(matrix(0, nrow = length(splitList), ncol = 4))
    for(i in 1:length(splitList)) {
      maxImprove <- splitList[[i]][which.max(splitList[[i]][["improve"]]), ]
      rows[i, 1] <- maxImprove$names
      rows[i, 2] <- maxImprove$count
      rows[i, 3] <- as.numeric(maxImprove$index)
      rows[i, 4] <- as.numeric(maxImprove$improve)
    }
    table[["predictor"]] <- rows[, 1]
    table[["count"]]     <- rows[, 2]
    table[["index"]]     <- rows[, 3]
    table[["improve"]]   <- rows[, 4]
  } else {
    table[["predictor"]] <- rownames(splits)
    table[["count"]]     <- splits[, 1]
    table[["index"]]     <- splits[, 4]
    table[["improve"]]   <- splits[, 3]
  }
}

.mlDecisionTreePlotTree <- function(dataset, options, jaspResults, ready, position, purpose) {
  if (!is.null(jaspResults[["decisionTreePlot"]]) || !options[["decisionTreePlot"]]) {
    return()
  }
  plot <- createJaspPlot(plot = NULL, title = gettext("Decision Tree Plot"), width = 600, height = 500)
  plot$position <- position
  if (purpose == "regression") {
    plot$dependOn(options = c("decisionTreePlot", .mlRegressionDependencies()))
  } else {
    plot$dependOn(options = c("decisionTreePlot", .mlClassificationDependencies()))
  }
  jaspResults[["decisionTreePlot"]] <- plot
  if (!ready) {
    return()
  }
  result <- switch(purpose,
    "classification" = jaspResults[["classificationResult"]]$object,
    "regression" = jaspResults[["regressionResult"]]$object
  )
  result[["model"]]$call$data <- result[["train"]] # Required
  if (is.null(result[["model"]]$splits)) {
    plot$setError(gettext("Plotting not possible: No splits were made in the tree."))
    return()
  }
  ptry <- try({
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
        yval <- tfun(frame$yval, ylevel, digits, nsmall = 20)
      } else {
        yval <- tfun(frame$yval2, ylevel, digits, nsmall = 20)
      }
    } else {
      yval <- format(signif(frame$yval, digits))
    }
    leafs <- which(x$frame$var == "<leaf>")
    labels <- yval[leafs]
    if (purpose == "classification") {
      labels <- strsplit(labels, split = " ")
      labels <- unlist(lapply(labels, `[[`, 1))
      colors <- .mlColorScheme(length(unique(labels)))
      cols <- colors[factor(labels)]
      alpha <- 0.3
    } else {
      cols <- "white"
      alpha <- 1
    }
    nodeNames <- p$data$splitvar
    nodeNames[is.na(nodeNames)] <- labels
    p$data$info <- paste0(nodeNames, "\nn = ", p$data$nodesize)
    for (i in 2:length(p$data$breaks_label)) {
      s <- strsplit(p$data$breaks_label[[i]], split = " ")
      if (!("NA" %in% s[[1]])) { # That means that it is a non-numeric split
        p$data$breaks_label[[i]] <- paste(p$data$breaks_label[[i]], collapse = " + ")
      } else {
        s[[1]][length(s[[1]])] <- format(as.numeric(s[[1]][length(s[[1]])]), digits = 3)
        s <- paste0(s[[1]], collapse = " ")
        p$data$breaks_label[[i]] <- s
      }
    }
    p <- p + ggparty::geom_edge() +
      ggparty::geom_edge_label(fill = "white", col = "darkred") +
      ggparty::geom_node_splitvar(mapping = ggplot2::aes(size = max(3, nodesize) / 2, label = info), fill = "white", col = "black") +
      ggparty::geom_node_label(mapping = ggplot2::aes(label = info, size = max(3, nodesize) / 2), ids = "terminal", fill = cols, col = "black", alpha = alpha) +
      ggplot2::scale_x_continuous(name = NULL, limits = c(min(p$data$x) - abs(0.1 * min(p$data$x)), max(p$data$x) * 1.1)) +
      ggplot2::scale_y_continuous(name = NULL, limits = c(min(p$data$y) - abs(0.1 * min(p$data$y)), max(p$data$y) * 1.1)) +
      jaspGraphs::geom_rangeframe(sides = "") +
      jaspGraphs::themeJaspRaw() +
      ggplot2::theme(
        axis.ticks = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank()
      )
  })
  if (isTryError(ptry)) {
    plot$setError(gettextf("Plotting not possible: An error occurred while creating this plot: %s", .extractErrorMessage(ptry)))
  } else {
    plot$plotObject <- p
  }
}

.mlDecisionTreePlotError <- function(dataset, options, jaspResults, ready, position, purpose) {
  if (!is.null(jaspResults[["optimPlot"]]) || !options[["optimPlot"]] || options[["modelOptimization"]] == "manual") {
    return()
  }
  plotTitle <- switch(purpose,
    "classification" = gettext("Classification Accuracy Plot"),
    "regression" = gettext("Mean Squared Error Plot")
  )
  plot <- createJaspPlot(plot = NULL, title = plotTitle, width = 400, height = 300)
  plot$position <- position
  if (purpose == "regression") {
    plot$dependOn(options = c("optimPlot", .mlRegressionDependencies()))
  } else {
    plot$dependOn(options = c("optimPlot", .mlClassificationDependencies()))
  }
  jaspResults[["optimPlot"]] <- plot
  if (!ready) {
    return()
  }
  result <- switch(purpose,
    "classification" = jaspResults[["classificationResult"]]$object,
    "regression" = jaspResults[["regressionResult"]]$object
  )
  ylabel <- switch(purpose,
    "classification" = gettext("Classification Accuracy"),
    "regression"     = gettext("Mean Squared Error")
  )
  xvalues <- rep(seq(0, options[["maxComplexityParameter"]], by = 0.01), 2)
  yvalues1 <- result[["accuracyStore"]]
  yvalues2 <- result[["trainAccuracyStore"]]
  yvalues <- c(yvalues1, yvalues2)
  type <- rep(c(gettext("Validation set"), gettext("Training set")), each = length(yvalues1))
  plotData <- data.frame(x = xvalues, y = yvalues, type = type)
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(0, plotData$x), min.n = 4)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(plotData$y, min.n = 4)
  pointData <- data.frame(
    x = result[["penalty"]],
    y = yvalues1[which(xvalues == result[["penalty"]])]
  )
   p <- ggplot2::ggplot(data = plotData, ggplot2::aes(x = x, y = y)) +
    jaspGraphs::geom_line(mapping = ggplot2::aes(linetype = type)) +
    ggplot2::scale_x_continuous(name = gettext("Complexity Penalty"), breaks = xBreaks, limits = c(0, max(xBreaks))) +
    ggplot2::scale_y_continuous(name = ylabel, breaks = yBreaks, limits = range(yBreaks)) +
    ggplot2::labs(linetype = NULL) +
    ggplot2::scale_linetype_manual(values = c(2, 1)) +
    jaspGraphs::geom_point(data = pointData, ggplot2::aes(x = x, y = y), fill = "red", inherit.aes = FALSE) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw(legend.position = "top")
  plot$plotObject <- p
}
