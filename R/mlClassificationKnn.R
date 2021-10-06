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

mlClassificationKnn <- function(jaspResults, dataset, options, ...) {

  # Preparatory work
  dataset <- .mlClassificationReadData(dataset, options)
  .mlClassificationErrorHandling(dataset, options, type = "knn")

  # Check if analysis is ready to run
  ready <- .mlClassificationReady(options, type = "knn")

  # Compute results and create the model summary table
  .mlClassificationTableSummary(dataset, options, jaspResults, ready, position = 1, type = "knn")

  # If the user wants to add the classes to the data set
  .mlClassificationAddPredictionsToData(dataset, options, jaspResults, ready)

  # Add test set indicator to data
  .mlAddTestIndicatorToData(options, jaspResults, ready, purpose = "classification")

  # Create the data split plot
  .mlPlotDataSplit(dataset, options, jaspResults, ready, position = 2, purpose = "classification", type = "knn")

  # Create the confusion table
  .mlClassificationTableConfusion(dataset, options, jaspResults, ready, position = 3)

  # Create the class proportions table
  .mlClassificationTableProportions(dataset, options, jaspResults, ready, position = 4)

  # Create the validation measures table
  .mlClassificationTableMetrics(dataset, options, jaspResults, ready, position = 5)

  # Create the weights plot
  .mlKnnPlotWeights(options, jaspResults, position = 6)

  # Create the classification error plot
  .mlKnnPlotError(dataset, options, jaspResults, ready, position = 7, purpose = "classification")

  # Create the ROC curve
  .mlClassificationPlotRoc(dataset, options, jaspResults, ready, position = 8, type = "knn")

  # Create the Andrews curves
  .mlClassificationPlotAndrews(dataset, options, jaspResults, ready, position = 9)

  # Decision boundaries
  .mlClassificationPlotBoundaries(dataset, options, jaspResults, ready, position = 10, type = "knn")
}

.knnClassification <- function(dataset, options, jaspResults) {
  # Import model formula from jaspResults
  formula <- jaspResults[["formula"]]$object
  # Set model specific parameters
  weights <- options[["weights"]]
  distance <- options[["distanceParameterManual"]]
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
    testFit <- kknn::kknn(
      formula = formula, train = trainingSet, test = testSet, k = options[["noOfNearestNeighbours"]],
      distance = distance, kernel = weights, scale = FALSE
    )
    nn <- options[["noOfNearestNeighbours"]]
  } else if (options[["modelOpt"]] == "optimizationError") {
    # Create a train, validation and test set (optimization)
    validationIndex <- sample.int(nrow(trainingAndValidationSet), size = ceiling(options[["validationDataManual"]] * nrow(trainingAndValidationSet)))
    testSet <- dataset[-trainingIndex, ]
    validationSet <- trainingAndValidationSet[validationIndex, ]
    trainingSet <- trainingAndValidationSet[-validationIndex, ]
    if (options[["modelValid"]] == "validationManual") {
      nnRange <- 1:options[["maxK"]]
      accuracyStore <- numeric(length(nnRange))
      trainAccuracyStore <- numeric(length(nnRange))
      startProgressbar(length(nnRange))
      for (i in nnRange) {
        validationFit <- kknn::kknn(
          formula = formula, train = trainingSet, test = validationSet, k = i,
          distance = options[["distanceParameterManual"]], kernel = options[["weights"]], scale = FALSE
        )
        accuracyStore[i] <- sum(diag(prop.table(table(validationFit$fitted.values, validationSet[, options[["target"]]]))))
        trainingFit <- kknn::kknn(
          formula = formula, train = trainingSet, test = trainingSet, k = i,
          distance = options[["distanceParameterManual"]], kernel = options[["weights"]], scale = FALSE
        )
        trainAccuracyStore[i] <- sum(diag(prop.table(table(trainingFit$fitted.values, trainingSet[, options[["target"]]]))))
        progressbarTick()
      }
      nn <- switch(options[["modelOpt"]],
        "optimizationError" = nnRange[which.max(accuracyStore)]
      )
      testFit <- kknn::kknn(
        formula = formula, train = trainingSet, test = testSet, k = nn,
        distance = options[["distanceParameterManual"]], kernel = options[["weights"]], scale = FALSE
      )
    } else if (options[["modelValid"]] == "validationKFold") {
      nnRange <- 1:options[["maxK"]]
      accuracyStore <- numeric(length(nnRange))
      startProgressbar(length(nnRange))
      for (i in nnRange) {
        validationFit <- kknn::cv.kknn(
          formula = formula, data = trainingAndValidationSet, distance = options[["distanceParameterManual"]], kernel = options[["weights"]],
          kcv = options[["noOfFolds"]], k = i
        )
        accuracyStore[i] <- sum(diag(prop.table(table(validationFit[[1]][, 1], validationFit[[1]][, 2]))))
        progressbarTick()
      }
      nn <- switch(options[["modelOpt"]],
        "optimizationError" = nnRange[which.max(accuracyStore)]
      )
      validationFit <- kknn::cv.kknn(
        formula = formula, data = trainingAndValidationSet, distance = options[["distanceParameterManual"]], kernel = options[["weights"]],
        kcv = options[["noOfFolds"]], k = nn
      )
      validationFit <- list(fitted.values = validationFit[[1]][, 2])
      testFit <- kknn::kknn(formula = formula, train = trainingAndValidationSet, test = testSet, k = nn, distance = distance, kernel = weights, scale = FALSE)
      trainingSet <- trainingAndValidationSet
      validationSet <- trainingAndValidationSet
    } else if (options[["modelValid"]] == "validationLeaveOneOut") {
      nnRange <- 1:options[["maxK"]]
      validationFit <- kknn::train.kknn(formula = formula, data = trainingAndValidationSet, ks = nnRange, scale = FALSE, distance = options[["distanceParameterManual"]], kernel = options[["weights"]])
      accuracyStore <- as.numeric(1 - kfit_valid$MISCLASS)
      nn <- switch(options[["modelOpt"]],
        "optimizationError" = nnRange[which.max(accuracyStore)]
      )
      validationFit <- list(fitted.values = validationFit[["fitted.values"]][[1]])
      testFit <- kknn::kknn(formula = formula, train = trainingAndValidationSet, test = testSet, k = nn, distance = distance, kernel = weights, scale = FALSE)
      trainingSet <- trainingAndValidationSet
      validationSet <- trainingAndValidationSet
    }
  }
  # Create results object
  result <- list()
  result[["formula"]] <- formula
  result[["model"]] <- testFit
  result[["model"]]$predictive <- kknn::train.kknn(formula = formula, data = trainingSet, ks = nn, distance = options[["distanceParameterManual"]], kernel = options[["weights"]])
  result[["nn"]] <- nn
  result[["weights"]] <- weights
  result[["distance"]] <- distance
  result[["confTable"]] <- table("Pred" = testFit$fitted.values, "Real" = testSet[, options[["target"]]])
  result[["testAcc"]] <- sum(diag(prop.table(result[["confTable"]])))
  result[["auc"]] <- .classificationCalcAUC(testSet, trainingSet, options, "knnClassification", nn = nn, distance = distance, weights = weights)
  result[["ntrain"]] <- nrow(trainingSet)
  result[["ntest"]] <- nrow(testSet)
  result[["testReal"]] <- testSet[, options[["target"]]]
  result[["testPred"]] <- testFit$fitted.values
  result[["train"]] <- trainingSet
  result[["test"]] <- testSet
  result[["testIndicatorColumn"]] <- testIndicatorColumn
  result[["classes"]] <- predict(kknn::kknn(formula = formula, train = trainingSet, test = dataset, k = nn, distance = distance, kernel = weights, scale = FALSE))
  if (options[["modelOpt"]] != "optimizationManual") {
    result[["accuracyStore"]] <- accuracyStore
    result[["valid"]] <- validationSet
    result[["nvalid"]] <- nrow(validationSet)
    result[["validationConfTable"]] <- table("Pred" = validationFit$fitted.values, "Real" = validationSet[, options[["target"]]])
    result[["validAcc"]] <- sum(diag(prop.table(result[["validationConfTable"]])))
    if (options[["modelValid"]] == "validationManual") {
      result[["trainAccuracyStore"]] <- trainAccuracyStore
    }
  }
  return(result)
}
