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

mlRegressionSvm <- function(jaspResults, dataset, options, state = NULL) {

  # Preparatory work
  dataset <- .readDataRegressionAnalyses(dataset, options)
  .mlRegressionErrorHandling(dataset, options, type = "svm")

  # Check if analysis is ready to run
  ready <- .mlRegressionReady(options, type = "svm")

  # Compute results and create the model summary table
  .mlRegressionTableSummary(dataset, options, jaspResults, ready, position = 1, type = "svm")

  # If the user wants to add the values to the data set
  .mlRegressionAddPredictionsToData(dataset, options, jaspResults, ready)

  # Add test set indicator to data
  .mlAddTestIndicatorToData(options, jaspResults, ready, purpose = "regression")

  # Create the data split plot
  .mlPlotDataSplit(dataset, options, jaspResults, ready, position = 2, purpose = "regression", type = "svm")

  # Create the evaluation metrics table
  .mlRegressionTableMetrics(dataset, options, jaspResults, ready, position = 3)

  # Create the feature importance table
  .mlTableFeatureImportance(options, jaspResults, ready, position = 4, purpose = "regression")

  # Create the shap table
  .mlTableShap(dataset, options, jaspResults, ready, position = 5, purpose = "regression")

  # Create the support vectors table
  .mlSvmTableSupportVectors(options, jaspResults, ready, position = 6, purpose = "regression")

  # Create the predicted performance plot
  .mlRegressionPlotPredictedPerformance(options, jaspResults, ready, position = 7)

  # Create the optimization plot
  .mlSvmPlotError(dataset, options, jaspResults, ready, position = 8, purpose = "regression")
}

.svmRegression <- function(dataset, options, jaspResults, ready) {
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
    # Check for factor levels in the test set that are not in the training set
    .checkForNewFactorLevelsInPredictionSet(trainingSet, testSet, "test")
    cost <- options[["cost"]]
    trainingFit <- e1071::svm(
      formula = formula, data = trainingSet, type = "eps-regression", kernel = options[["weights"]], cost = cost, tolerance = options[["tolerance"]],
      epsilon = options[["epsilon"]], scale = FALSE, degree = options[["degree"]], gamma = options[["gamma"]], coef0 = options[["complexityParameter"]]
    )
  } else if (options[["modelOptimization"]] == "optimized") {
    # Create a train, validation and test set (optimization)
    validationIndex <- sample.int(nrow(trainingAndValidationSet), size = ceiling(options[["validationDataManual"]] * nrow(trainingAndValidationSet)))
    testSet <- dataset[-trainingIndex, ]
    validationSet <- trainingAndValidationSet[validationIndex, ]
    trainingSet <- trainingAndValidationSet[-validationIndex, ]
    # Check for factor levels in the test set that are not in the training set
    .checkForNewFactorLevelsInPredictionSet(trainingSet, testSet, "test")
    # Check for factor levels in the validation set that are not in the training set
    .checkForNewFactorLevelsInPredictionSet(trainingSet, validationSet, "validation")
    costs <- seq(0.01, options[["maxCost"]], 0.01)
    errorStore <- trainErrorStore <- numeric(length(costs))
    startProgressbar(length(costs))
    for (i in seq_along(costs)) {
      trainingFit <- e1071::svm(
        formula = formula, data = trainingSet, type = "eps-regression", kernel = options[["weights"]], cost = costs[i], tolerance = options[["tolerance"]],
        epsilon = options[["epsilon"]], scale = FALSE, degree = options[["degree"]], gamma = options[["gamma"]], coef0 = options[["complexityParameter"]]
      )
      errorStore[i] <- mean((predict(trainingFit, newdata = validationSet) - validationSet[, options[["target"]]])^2)
      trainErrorStore[i] <- mean((predict(trainingFit, newdata = trainingSet) - trainingSet[, options[["target"]]])^2)
      progressbarTick()
    }
    cost <- costs[which.min(errorStore)]
    trainingFit <- e1071::svm(
      formula = formula, data = trainingSet, type = "eps-regression", kernel = options[["weights"]], cost = cost, tolerance = options[["tolerance"]],
      epsilon = options[["epsilon"]], scale = FALSE, degree = options[["degree"]], gamma = options[["gamma"]], coef0 = options[["complexityParameter"]]
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
  result[["cost"]] <- cost
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
  result[["explainer"]] <- DALEX::explain(result[["model"]], type = "regression", data = result[["train"]][, options[["predictors"]]], y = result[["train"]][, options[["target"]]], predict_function = function(model, data) predict(model, newdata = data))
  return(result)
}

.mlSvmTableSupportVectors <- function(options, jaspResults, ready, position, purpose) {
  if (!is.null(jaspResults[["supportVectorsTable"]]) || !options[["supportVectorsTable"]]) {
    return()
  }
  table <- createJaspTable(title = gettext("Support Vectors"))
  table$position <- position
  if (purpose == "regression") {
    table$dependOn(options = c("supportVectorsTable", .mlRegressionDependencies()))
  } else {
    table$dependOn(options = c("supportVectorsTable", .mlClassificationDependencies()))
  }
  table$addColumnInfo(name = "row", title = gettext("Row"), type = "string")
  jaspResults[["supportVectorsTable"]] <- table
  if (!ready) {
    return()
  }
  result <- switch(purpose,
    "classification" = jaspResults[["classificationResult"]]$object,
    "regression" = jaspResults[["regressionResult"]]$object
  )
  vectors <- cbind(result[["model"]]$index, result[["model"]]$SV)
  colnames(vectors)[1] <- "row"
  vectors <- vectors[order(vectors[, 1]), ]
  for (i in 2:ncol(vectors)) {
    table$addColumnInfo(name = colnames(vectors)[i], title = colnames(vectors)[i], type = "number")
  }
  table$setData(vectors)
}

.mlSvmPlotError <- function(dataset, options, jaspResults, ready, position, purpose) {
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
  xvalues <- rep(seq(0.01, options[["maxCost"]], by = 0.01), 2)
  yvalues1 <- result[["accuracyStore"]]
  yvalues2 <- result[["trainAccuracyStore"]]
  yvalues <- c(yvalues1, yvalues2)
  type <- rep(c(gettext("Validation set"), gettext("Training set")), each = length(yvalues1))
  plotData <- data.frame(x = xvalues, y = yvalues, type = type)
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(0, plotData$x), min.n = 4)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(plotData$y, min.n = 4)
  pointData <- data.frame(
    x = result[["cost"]],
    y = yvalues1[which(xvalues == result[["cost"]])]
  )
  p <- ggplot2::ggplot(data = plotData, ggplot2::aes(x = x, y = y)) +
    jaspGraphs::geom_line(mapping = ggplot2::aes(linetype = type)) +
    ggplot2::scale_x_continuous(name = gettext("Cost of Constraints Violation"), breaks = xBreaks, limits = c(0, max(xBreaks))) +
    ggplot2::scale_y_continuous(name = ylabel, breaks = yBreaks, limits = range(yBreaks)) +
    ggplot2::labs(linetype = NULL) +
    ggplot2::scale_linetype_manual(values = c(2, 1)) +
    jaspGraphs::geom_point(data = pointData, ggplot2::aes(x = x, y = y), fill = "red", inherit.aes = FALSE) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw(legend.position = "top")
  plot$plotObject <- p
}
