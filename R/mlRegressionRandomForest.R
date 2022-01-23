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

mlRegressionRandomForest <- function(jaspResults, dataset, options, ...) {

  # Preparatory work
  dataset <- .readDataRegressionAnalyses(dataset, options)
  .mlRegressionErrorHandling(dataset, options, type = "randomForest")

  # Check if analysis is ready to run
  ready <- .mlRegressionReady(options, type = "randomForest")

  # Compute results and create the model summary table
  .mlRegressionTableSummary(dataset, options, jaspResults, ready, position = 1, type = "randomForest")

  # If the user wants to add the values to the data set
  .mlRegressionAddPredictionsToData(dataset, options, jaspResults, ready)

  # Add test set indicator to data
  .mlAddTestIndicatorToData(options, jaspResults, ready, purpose = "regression")

  # Create the data split plot
  .mlPlotDataSplit(dataset, options, jaspResults, ready, position = 2, purpose = "regression", type = "randomForest")

  # Create the evaluation metrics table
  .mlRegressionTableMetrics(dataset, options, jaspResults, ready, position = 3)

  # Create the variable importance table
  .mlRandomForestTableVarImp(options, jaspResults, ready, position = 4, purpose = "regression")

  # Create the trees vs model error plot
  .mlRandomForestPlotError(options, jaspResults, ready, position = 5, purpose = "regression")

  # Create the predicted performance plot
  .mlRegressionPlotPredictedPerformance(options, jaspResults, ready, position = 6)

  # Create the mean decrease in accuracy plot
  .mlRandomForestPlotDecreaseAccuracy(options, jaspResults, ready, position = 7, purpose = "regression")

  # Create the total increase in node purity plot
  .mlRandomForestPlotIncreasePurity(options, jaspResults, ready, position = 8, purpose = "regression")
}

.randomForestRegression <- function(dataset, options, jaspResults) {
  # Set model-specific parameters
  noOfPredictors <- switch(options[["noOfPredictors"]],
    "manual" = options[["numberOfPredictors"]],
    "auto" = floor(sqrt(length(options[["predictors"]])))
  )
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
  if (options[["modelOpt"]] == "optimizationManual") {
    # Just create a train and a test set (no optimization)
    trainingSet <- trainingAndValidationSet
    testSet <- dataset[-trainingIndex, ]
    testFit <- randomForest::randomForest(
      x = trainingSet[, options[["predictors"]]], y = trainingSet[, options[["target"]]],
      xtest = testSet[, options[["predictors"]]], ytest = testSet[, options[["target"]]],
      ntree = options[["noOfTrees"]], mtry = noOfPredictors,
      sampsize = ceiling(options[["bagFrac"]] * nrow(trainingSet)),
      importance = TRUE, keep.forest = TRUE
    )
    noOfTrees <- options[["noOfTrees"]]
  } else if (options[["modelOpt"]] == "optimizationError") {
    # Create a train, validation and test set (optimization)
    validationIndex <- sample.int(nrow(trainingAndValidationSet), size = ceiling(options[["validationDataManual"]] * nrow(trainingAndValidationSet)))
    testSet <- dataset[-trainingIndex, ]
    validationSet <- trainingAndValidationSet[validationIndex, ]
    trainingSet <- trainingAndValidationSet[-validationIndex, ]
    validationFit <- randomForest::randomForest(
      x = trainingSet[, options[["predictors"]]], y = trainingSet[, options[["target"]]],
      xtest = validationSet[, options[["predictors"]]], ytest = validationSet[, options[["target"]]],
      ntree = options[["maxTrees"]], mtry = noOfPredictors,
      sampsize = ceiling(options[["bagFrac"]] * nrow(trainingSet)),
      importance = TRUE, keep.forest = TRUE
    )
    oobError <- validationFit[["mse"]]
    optimTrees <- which.min(oobError)[length(which.min(oobError))]
    testFit <- randomForest::randomForest(
      x = trainingSet[, options[["predictors"]]], y = trainingSet[, options[["target"]]],
      xtest = testSet[, options[["predictors"]]], ytest = testSet[, options[["target"]]],
      ntree = optimTrees, mtry = noOfPredictors,
      sampsize = ceiling(options[["bagFrac"]] * nrow(trainingSet)),
      importance = TRUE, keep.forest = TRUE
    )
    noOfTrees <- optimTrees
  }
  # Train a model on the training data
  trainingFit <- randomForest::randomForest(
    x = trainingSet[, options[["predictors"]]], y = trainingSet[, options[["target"]]],
    xtest = trainingSet[, options[["predictors"]]], ytest = trainingSet[, options[["target"]]],
    ntree = noOfTrees, mtry = noOfPredictors,
    sampsize = ceiling(options[["bagFrac"]] * nrow(trainingSet)),
    importance = TRUE, keep.forest = TRUE
  )
  # Use the specified model to make predictions for dataset
  dataPredictions <- predict(testFit, newdata = dataset)
  # Create results object
  result <- list()
  result[["model"]] <- testFit
  result[["rfit_test"]] <- testFit
  result[["rfit_train"]] <- trainingFit
  result[["noOfTrees"]] <- noOfTrees
  result[["predPerSplit"]] <- noOfPredictors
  result[["bagFrac"]] <- ceiling(options[["bagFrac"]] * nrow(dataset))
  result[["testMSE"]] <- mean((testFit$test[["predicted"]] - testSet[, options[["target"]]])^2)
  result[["testPred"]] <- testFit$test[["predicted"]]
  result[["testReal"]] <- testSet[, options[["target"]]]
  result[["oobError"]] <- testFit$mse[length(testFit$mse)]
  result[["ntrain"]] <- nrow(trainingSet)
  result[["ntest"]] <- nrow(testSet)
  result[["train"]] <- trainingSet
  result[["test"]] <- testSet
  result[["testIndicatorColumn"]] <- testIndicatorColumn
  result[["values"]] <- dataPredictions
  result[["varImp"]] <- plyr::arrange(data.frame(
    Variable = as.factor(names(testFit$importance[, 1])),
    MeanIncrMSE = testFit$importance[, 1],
    TotalDecrNodeImp = testFit$importance[, 2]
  ), -TotalDecrNodeImp)
  if (options[["modelOpt"]] != "optimizationManual") {
    result[["validMSE"]] <- mean((validationFit$test[["predicted"]] - validationSet[, options[["target"]]])^2)
    result[["nvalid"]] <- nrow(validationSet)
    result[["valid"]] <- validationSet
    result[["rfit_valid"]] <- validationFit
  }
  return(result)
}

.mlRandomForestTableVarImp <- function(options, jaspResults, ready, position, purpose) {
  if (!is.null(jaspResults[["tableVariableImportance"]]) || !options[["tableVariableImportance"]]) {
    return()
  }
  table <- createJaspTable(title = gettext("Variable Importance"))
  table$position <- position
  table$dependOn(options = c(
    "tableVariableImportance", "scaleEqualSD", "target", "predictors", "modelOpt", "maxTrees",
    "noOfTrees", "bagFrac", "noOfPredictors", "numberOfPredictors", "seed", "seedBox",
    "testSetIndicatorVariable", "testSetIndicator", "validationDataManual", "holdoutData", "testDataManual"
  ))
  table$addColumnInfo(name = "predictor", title = " ", type = "string")
  table$addColumnInfo(name = "MDiA", title = gettext("Mean decrease in accuracy"), type = "number")
  table$addColumnInfo(name = "MDiNI", title = gettext("Total increase in node purity"), type = "number")
  jaspResults[["tableVariableImportance"]] <- table
  if (!ready) {
    return()
  }
  result <- switch(purpose,
    "classification" = jaspResults[["classificationResult"]]$object,
    "regression" = jaspResults[["regressionResult"]]$object
  )
  varImpOrder <- sort(result[["rfit_test"]]$importance[, 1], decr = TRUE, index.return = TRUE)$ix
  table[["predictor"]] <- as.character(result[["varImp"]]$Variable)
  table[["MDiA"]] <- result[["varImp"]]$MeanIncrMSE
  table[["MDiNI"]] <- result[["varImp"]]$TotalDecrNodeImp
}

.mlRandomForestPlotError <- function(options, jaspResults, ready, position, purpose) {
  if (!is.null(jaspResults[["plotTreesVsModelError"]]) || !options[["plotTreesVsModelError"]]) {
    return()
  }
  title <- switch(purpose,
    "classification" = gettext("Out-of-bag Classification Accuracy Plot"),
    "regression" = gettext("Out-of-bag Mean Squared Error Plot")
  )
  plot <- createJaspPlot(plot = NULL, title = title, width = 400, height = 300)
  plot$position <- position
  plot$dependOn(options = c(
    "plotTreesVsModelError", "trainingDataManual", "scaleEqualSD", "modelOpt", "maxTrees",
    "target", "predictors", "seed", "seedBox", "noOfTrees", "bagFrac", "noOfPredictors", "numberOfPredictors",
    "testSetIndicatorVariable", "testSetIndicator", "validationDataManual", "holdoutData", "testDataManual"
  ))
  jaspResults[["plotTreesVsModelError"]] <- plot
  if (!ready) {
    return()
  }
  result <- switch(purpose,
    "classification" = jaspResults[["classificationResult"]]$object,
    "regression" = jaspResults[["regressionResult"]]$object
  )
  yTitle <- switch(purpose,
    "classification" = gettextf("Out-of-bag %sClassification Accuracy", "\n"),
    "regression"     = gettextf("Out-of-bag %sMean Squared Error", "\n")
  )
  values <- switch(purpose,
    "classification" = 1 - result[["rfit_train"]]$err.rate[, 1],
    "regression" = result[["rfit_train"]]$mse
  )
  if (options[["modelOpt"]] != "optimizationManual") {
    values2 <- switch(purpose,
      "classification" = 1 - result[["rfit_valid"]]$err.rate[1:result[["noOfTrees"]], 1],
      "regression" = result[["rfit_valid"]]$mse[1:result[["noOfTrees"]]]
    )
    values <- c(values2, values)
    treesMSE <- data.frame(
      trees = rep(1:length(values2), 2),
      error = values,
      type = rep(c(gettext("Validation set"), gettext("Training set")), each = length(values2))
    )
    xBreaks <- jaspGraphs::getPrettyAxisBreaks(treesMSE[["trees"]], min.n = 4)
    yBreaks <- jaspGraphs::getPrettyAxisBreaks(treesMSE[["error"]], min.n = 4)
    p <- ggplot2::ggplot(data = treesMSE, mapping = ggplot2::aes(x = trees, y = error, linetype = type)) +
      jaspGraphs::geom_line() +
      ggplot2::scale_x_continuous(name = gettext("Number of Trees"), breaks = xBreaks, limits = range(xBreaks)) +
      ggplot2::scale_y_continuous(name = yTitle, breaks = yBreaks, limits = range(yBreaks)) +
      ggplot2::labs(linetype = "") +
      ggplot2::scale_linetype_manual(values = c(2, 1)) +
      jaspGraphs::geom_rangeframe() +
      jaspGraphs::themeJaspRaw(legend.position = "top")
  } else {
    treesMSE <- data.frame(
      trees = 1:length(values),
      error = values,
      type = rep(gettext("Training set"), each = length(values))
    )
    xBreaks <- jaspGraphs::getPrettyAxisBreaks(treesMSE[["trees"]], min.n = 4)
    yBreaks <- jaspGraphs::getPrettyAxisBreaks(treesMSE[["error"]], min.n = 4)
    p <- ggplot2::ggplot(data = treesMSE, mapping = ggplot2::aes(x = trees, y = error, linetype = type)) +
      jaspGraphs::geom_line() +
      ggplot2::scale_x_continuous(name = gettext("Number of Trees"), breaks = xBreaks, limits = range(xBreaks)) +
      ggplot2::scale_y_continuous(name = yTitle, breaks = yBreaks, limits = range(yBreaks)) +
      ggplot2::labs(linetype = NULL) +
      jaspGraphs::geom_rangeframe() +
      jaspGraphs::themeJaspRaw(legend.position = "top")
  }
  plot$plotObject <- p
}

.mlRandomForestPlotDecreaseAccuracy <- function(options, jaspResults, ready, position, purpose) {
  if (!is.null(jaspResults[["plotDecreaseAccuracy"]]) || !options[["plotDecreaseAccuracy"]]) {
    return()
  }
  plot <- createJaspPlot(plot = NULL, title = gettext("Mean Decrease in Accuracy"), width = 450, height = 300)
  plot$position <- position
  plot$dependOn(options = c(
    "plotDecreaseAccuracy", "trainingDataManual", "scaleEqualSD", "modelOpt", "maxTrees",
    "target", "predictors", "seed", "seedBox", "noOfTrees", "bagFrac", "noOfPredictors", "numberOfPredictors",
    "testSetIndicatorVariable", "testSetIndicator", "validationDataManual", "holdoutData", "testDataManual"
  ))
  jaspResults[["plotDecreaseAccuracy"]] <- plot
  if (!ready) {
    return()
  }
  result <- switch(purpose,
    "classification" = jaspResults[["classificationResult"]]$object,
    "regression" = jaspResults[["regressionResult"]]$object
  )
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(0, result[["varImp"]]$MeanIncrMSE))
  p <- ggplot2::ggplot(result[["varImp"]], ggplot2::aes(x = reorder(Variable, MeanIncrMSE), y = MeanIncrMSE)) +
    ggplot2::geom_bar(stat = "identity", fill = "grey", col = "black") +
    ggplot2::scale_y_continuous(name = gettext("Mean Decrease in Accuracy"), breaks = yBreaks, limits = range(yBreaks)) +
    ggplot2::scale_x_discrete(name = NULL) +
    ggplot2::coord_flip() +
    jaspGraphs::geom_rangeframe(sides = "b") +
    jaspGraphs::themeJaspRaw() +
    ggplot2::theme(axis.ticks.y = ggplot2::element_blank(), axis.text.y = ggplot2::element_text(size = 12))
  plot$plotObject <- p
}

.mlRandomForestPlotIncreasePurity <- function(options, jaspResults, ready, position, purpose) {
  if (!is.null(jaspResults[["plotIncreasePurity"]]) || !options[["plotIncreasePurity"]]) {
    return()
  }
  plot <- createJaspPlot(plot = NULL, title = gettext("Total Increase in Node Purity"), width = 450, height = 300)
  plot$position <- position
  plot$dependOn(options = c(
    "plotIncreasePurity", "trainingDataManual", "scaleEqualSD", "modelOpt", "maxTrees",
    "target", "predictors", "seed", "seedBox", "noOfTrees", "bagFrac", "noOfPredictors", "numberOfPredictors",
    "testSetIndicatorVariable", "testSetIndicator", "validationDataManual", "holdoutData", "testDataManual"
  ))
  jaspResults[["plotIncreasePurity"]] <- plot
  if (!ready) {
    return()
  }
  result <- switch(purpose,
    "classification" = jaspResults[["classificationResult"]]$object,
    "regression" = jaspResults[["regressionResult"]]$object
  )
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(0, result[["varImp"]]$TotalDecrNodeImp))
  p <- ggplot2::ggplot(result[["varImp"]], ggplot2::aes(x = reorder(Variable, TotalDecrNodeImp), y = TotalDecrNodeImp)) +
    ggplot2::geom_bar(stat = "identity", fill = "grey", col = "black") +
    ggplot2::scale_y_continuous(name = gettext("Total Increase in Node Purity"), breaks = yBreaks, limits = range(yBreaks)) +
    ggplot2::scale_x_discrete(name = NULL) +
    ggplot2::coord_flip() +
    jaspGraphs::geom_rangeframe(sides = "b") +
    jaspGraphs::themeJaspRaw() +
    ggplot2::theme(axis.ticks.y = ggplot2::element_blank(), axis.text.y = ggplot2::element_text(size = 12))
  plot$plotObject <- p
}
