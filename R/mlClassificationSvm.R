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

mlClassificationSvm <- function(jaspResults, dataset, options, ...) {

  # Preparatory work
  dataset <- .mlClassificationReadData(dataset, options)
  .mlClassificationErrorHandling(dataset, options, type = "svm")

  # Check if analysis is ready to run
  ready <- .mlClassificationReady(options, type = "svm")

  # Compute results and create the model summary table
  .mlClassificationTableSummary(dataset, options, jaspResults, ready, position = 1, type = "svm")

  # If the user wants to add the classes to the data set
  .mlClassificationAddPredictionsToData(dataset, options, jaspResults, ready)

  # Add test set indicator to data
  .mlAddTestIndicatorToData(options, jaspResults, ready, purpose = "classification")

  # Create the data split plot
  .mlPlotDataSplit(dataset, options, jaspResults, ready, position = 2, purpose = "classification", type = "svm")

  # Create the confusion table
  .mlClassificationTableConfusion(dataset, options, jaspResults, ready, position = 3)

  # Create the class proportions table
  .mlClassificationTableProportions(dataset, options, jaspResults, ready, position = 4)

  # Create the validation measures table
  .mlClassificationTableMetrics(dataset, options, jaspResults, ready, position = 5)

  # Create the feature importance table
  .mlTableFeatureImportance(options, jaspResults, ready, position = 6, purpose = "classification")

  # Create the shap table
  .mlTableShap(dataset, options, jaspResults, ready, position = 7, purpose = "classification")

  # Create the support vectors table
  .mlSvmTableSupportVectors(options, jaspResults, ready, position = 8, purpose = "classification")

  # Create the ROC curve
  .mlClassificationPlotRoc(dataset, options, jaspResults, ready, position = 9, type = "svm")

  # Create the Andrews curves
  .mlClassificationPlotAndrews(dataset, options, jaspResults, ready, position = 10)

  # Create the optimization plot
  .mlSvmPlotError(dataset, options, jaspResults, ready, position = 11, purpose = "classification")

  # Decision boundaries
  .mlClassificationPlotBoundaries(dataset, options, jaspResults, ready, position = 12, type = "svm")
}

.svmClassification <- function(dataset, options, jaspResults, ready) {
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
    cost <- options[["cost"]]
    trainingFit <- e1071::svm(
      formula = formula, data = trainingSet, type = "C-classification", kernel = options[["weights"]], cost = cost, tolerance = options[["tolerance"]],
      epsilon = options[["epsilon"]], scale = FALSE, degree = options[["degree"]], gamma = options[["gamma"]], coef0 = options[["complexityParameter"]], probability = TRUE
    )
  } else if (options[["modelOptimization"]] == "optimized") {
    # Create a train, validation and test set (optimization)
    validationIndex <- sample.int(nrow(trainingAndValidationSet), size = ceiling(options[["validationDataManual"]] * nrow(trainingAndValidationSet)))
    testSet <- dataset[-trainingIndex, ]
    validationSet <- trainingAndValidationSet[validationIndex, ]
    trainingSet <- trainingAndValidationSet[-validationIndex, ]
    costs <- seq(0.01, options[["maxCost"]], 0.01)
    accuracyStore <- trainAccuracyStore <- numeric(length(costs))
    startProgressbar(length(costs))
    for (i in seq_along(costs)) {
      trainingFit <- e1071::svm(
        formula = formula, data = trainingSet, type = "C-classification", kernel = options[["weights"]], cost = costs[i], tolerance = options[["tolerance"]],
        epsilon = options[["epsilon"]], scale = FALSE, degree = options[["degree"]], gamma = options[["gamma"]], coef0 = options[["complexityParameter"]], probability = TRUE
      )
      accuracyStore[i] <- length(which(predict(trainingFit, newdata = validationSet) == validationSet[, options[["target"]]])) / nrow(validationSet)
      trainAccuracyStore[i] <- length(which(predict(trainingFit, newdata = trainingSet) == trainingSet[, options[["target"]]])) / nrow(trainingSet)
      progressbarTick()
    }
    cost <- costs[which.max(accuracyStore)]
    trainingFit <- e1071::svm(
      formula = formula, data = trainingSet, type = "C-classification", kernel = options[["weights"]], cost = cost, tolerance = options[["tolerance"]],
      epsilon = options[["epsilon"]], scale = FALSE, degree = options[["degree"]], gamma = options[["gamma"]], coef0 = options[["complexityParameter"]], probability = TRUE
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
  result[["model"]]$data <- trainingSet
  result[["cost"]] <- cost
  result[["confTable"]] <- table("Pred" = testPredictions, "Real" = testSet[, options[["target"]]])
  result[["testAcc"]] <- sum(diag(prop.table(result[["confTable"]])))
  result[["auc"]] <- .classificationCalcAUC(testSet, trainingSet, options, "svmClassification", cost = result[["cost"]])
  result[["ntrain"]] <- nrow(trainingSet)
  result[["ntest"]] <- nrow(testSet)
  result[["testReal"]] <- testSet[, options[["target"]]]
  result[["testPred"]] <- testPredictions
  result[["train"]] <- trainingSet
  result[["test"]] <- testSet
  result[["testIndicatorColumn"]] <- testIndicatorColumn
  result[["classes"]] <- dataPredictions
  if (options[["modelOptimization"]] != "manual") {
    result[["accuracyStore"]] <- accuracyStore
    result[["valid"]] <- validationSet
    result[["nvalid"]] <- nrow(validationSet)
    result[["validationConfTable"]] <- table("Pred" = validationPredictions, "Real" = validationSet[, options[["target"]]])
    result[["validAcc"]] <- sum(diag(prop.table(result[["validationConfTable"]])))
    result[["trainAccuracyStore"]] <- trainAccuracyStore
  }
  result[["explainer"]] <- DALEX::explain(result[["model"]], type = "multiclass", data = result[["train"]], y = result[["train"]][, options[["target"]]], predict_function = function(model, data) attr(predict(model, newdata = data, probability = TRUE), "probabilities"))
  if (nlevels(result[["testReal"]]) == 2) {
    result[["explainer_fi"]] <- DALEX::explain(result[["model"]], type = "classification", data = result[["train"]], y = as.numeric(result[["train"]][, options[["target"]]]) - 1, predict_function = function(model, data) as.numeric(predict(model, newdata = data)) - 1)
  } else {
    result[["explainer_fi"]] <- DALEX::explain(result[["model"]], type = "multiclass", data = result[["train"]], y = result[["train"]][, options[["target"]]], predict_function = function(model, data) attr(predict(model, newdata = data, probability = TRUE), "probabilities"))
  }
  return(result)
}
