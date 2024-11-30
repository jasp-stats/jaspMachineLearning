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

mlClassificationBoosting <- function(jaspResults, dataset, options, ...) {

  # Preparatory work
  dataset <- .mlClassificationReadData(dataset, options)
  .mlClassificationErrorHandling(dataset, options, type = "boosting")

  # Check if analysis is ready to run
  ready <- .mlClassificationReady(options, type = "boosting")

  # Compute results and create the model summary table
  .mlClassificationTableSummary(dataset, options, jaspResults, ready, position = 1, type = "boosting")

  # If the user wants to add the classes to the data set
  .mlClassificationAddPredictionsToData(dataset, options, jaspResults, ready)

  # Add test set indicator to data
  .mlAddTestIndicatorToData(options, jaspResults, ready, purpose = "classification")

  # Create the data split plot
  .mlPlotDataSplit(dataset, options, jaspResults, ready, position = 2, purpose = "classification", type = "boosting")

  # Create the confusion table
  .mlClassificationTableConfusion(dataset, options, jaspResults, ready, position = 3)

  # Create the class proportions table
  .mlClassificationTableProportions(dataset, options, jaspResults, ready, position = 4)

  # Create the validation measures table
  .mlClassificationTableMetrics(dataset, options, jaspResults, ready, position = 5)

  # Create the relative influence table
  .mlBoostingTableFeatureImportance(options, jaspResults, ready, position = 6, purpose = "classification")

  # Create the shap table
  .mlTableShap(dataset, options, jaspResults, ready, position = 7, purpose = "classification")

  # Create the ROC curve
  .mlClassificationPlotRoc(dataset, options, jaspResults, ready, position = 8, type = "boosting")

  # Create the Andrews curves
  .mlClassificationPlotAndrews(dataset, options, jaspResults, ready, position = 9)

  # Create the OOB improvement plot
  .mlBoostingPlotOobImprovement(options, jaspResults, ready, position = 10, purpose = "classification")

  # Create the deviance plot
  .mlBoostingPlotDeviance(options, jaspResults, ready, position = 11, purpose = "classification")

  # Create the relative influence plot
  .mlBoostingPlotRelInf(options, jaspResults, ready, position = 12, purpose = "classification")

  # Decision boundaries
  .mlClassificationPlotBoundaries(dataset, options, jaspResults, ready, position = 13, type = "boosting")
}

.boostingClassification <- function(dataset, options, jaspResults) {
  jaspBase:::assignFunctionInPackage(fakeGbmCrossValModelBuild, "gbmCrossValModelBuild", "gbm")
  jaspBase:::assignFunctionInPackage(fakeGbmCrossValErr, "gbmCrossValErr", "gbm")
  # Import model formula from jaspResults
  formula <- jaspResults[["formula"]]$object
  # Set model-specific parameters
  trees <- switch(options[["modelOptimization"]],
    "manual" = options[["noOfTrees"]],
    "optimized" = options[["maxTrees"]]
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
  # gbm expects the columns in the data to be in the same order as the variables...
  trainingAndValidationSet <- trainingAndValidationSet[, match(names(trainingAndValidationSet)[which(names(trainingAndValidationSet) %in% all.vars(formula))], all.vars(formula)), drop = FALSE]
  if (options[["modelOptimization"]] == "manual") {
    # Just create a train and a test set (no optimization)
    trainingSet <- trainingAndValidationSet
    testSet <- dataset[-trainingIndex, ]
    # Check for factor levels in the test set that are not in the training set
    .checkForNewFactorLevelsInPredictionSet(trainingSet, testSet, "test")
    noOfFolds <- 0
    .mlBoostingCheckMinObsNode(options, trainingSet) # Check for min obs in nodes
    fit <- gbm::gbm(
      formula = formula, data = trainingSet, n.trees = trees,
      shrinkage = options[["shrinkage"]], interaction.depth = options[["interactionDepth"]],
      cv.folds = noOfFolds, bag.fraction = options[["baggingFraction"]], n.minobsinnode = options[["minObservationsInNode"]],
      distribution = "multinomial", n.cores = 1, keep.data = TRUE
    ) # Multiple cores breaks modules in JASP, see: INTERNAL-jasp#372
    noOfTrees <- options[["noOfTrees"]]
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
    if (options[["modelValid"]] == "validationManual") {
      noOfFolds <- 0
    } else if (options[["modelValid"]] == "validationKFold") {
      noOfFolds <- options[["noOfFolds"]]
      trainingSet <- trainingAndValidationSet
      validationSet <- trainingAndValidationSet
    }
    .mlBoostingCheckMinObsNode(options, trainingSet) # Check for min obs in nodes
    fit <- gbm::gbm(
      formula = formula, data = trainingSet, n.trees = trees,
      shrinkage = options[["shrinkage"]], interaction.depth = options[["interactionDepth"]],
      cv.folds = noOfFolds, bag.fraction = options[["baggingFraction"]], n.minobsinnode = options[["minObservationsInNode"]],
      distribution = "multinomial", n.cores = 1, keep.data = TRUE
    ) # Multiple cores breaks modules in JASP, see: INTERNAL-jasp#372
    noOfTrees <- gbm::gbm.perf(fit, plot.it = FALSE, method = "OOB")[1]
    fit <- gbm::gbm(
      formula = formula, data = trainingSet, n.trees = noOfTrees,
      shrinkage = options[["shrinkage"]], interaction.depth = options[["interactionDepth"]],
      cv.folds = noOfFolds, bag.fraction = options[["baggingFraction"]], n.minobsinnode = options[["minObservationsInNode"]],
      distribution = "multinomial", n.cores = 1
    ) # Multiple cores breaks modules in JASP, see: INTERNAL-jasp#372
    validationProbs <- gbm::predict.gbm(fit, newdata = validationSet, n.trees = noOfTrees, type = "response")
    validationPredictions <- colnames(validationProbs)[apply(validationProbs, 1, which.max)]
  }
  # Use the specified model to make predictions for dataset
  dataProbs <- gbm::predict.gbm(fit, newdata = dataset, n.trees = noOfTrees, type = "response")
  dataPredictions <- colnames(dataProbs)[apply(dataProbs, 1, which.max)]
  testPredictions <- dataPredictions[-trainingIndex]
  # Create results object
  result <- list()
  result[["model"]] <- fit
  result[["formula"]] <- formula
  result[["noOfFolds"]] <- noOfFolds
  result[["noOfTrees"]] <- noOfTrees
  result[["confTable"]] <- table("Pred" = testPredictions, "Real" = testSet[, options[["target"]]])
  result[["testAcc"]] <- sum(diag(prop.table(result[["confTable"]])))
  result[["relInf"]] <- summary(fit, plot = FALSE)
  result[["auc"]] <- .classificationCalcAUC(testSet, trainingSet, options, "boostingClassification", noOfFolds = noOfFolds, noOfTrees = noOfTrees)
  result[["ntrain"]] <- nrow(trainingSet)
  result[["ntest"]] <- nrow(testSet)
  result[["testPred"]] <- testPredictions
  result[["testReal"]] <- testSet[, options[["target"]]]
  result[["train"]] <- trainingSet
  result[["test"]] <- testSet
  result[["method"]] <- if (options[["modelValid"]] == "validationManual") "OOB" else ""
  result[["testIndicatorColumn"]] <- testIndicatorColumn
  result[["classes"]] <- dataPredictions
  if (options[["modelOptimization"]] != "manual") {
    result[["validationConfTable"]] <- table("Pred" = validationPredictions, "Real" = validationSet[, options[["target"]]])
    result[["validAcc"]] <- sum(diag(prop.table(result[["validationConfTable"]])))
    result[["nvalid"]] <- nrow(validationSet)
    result[["valid"]] <- validationSet
  }
  result[["explainer"]] <- DALEX::explain(result[["model"]], type = "multiclass", data = result[["train"]], y = result[["train"]][, options[["target"]]], predict_function = function(model, data) predict(model, newdata = data, type = "response", n.trees = model$n.trees))
  if (nlevels(result[["testReal"]]) == 2) {
    result[["explainer_fi"]] <- DALEX::explain(result[["model"]], type = "classification", data = result[["train"]], y = as.numeric(result[["train"]][, options[["target"]]]) - 1, predict_function = function(model, data) predict(model, newdata = data, type = "response", n.trees = model$n.trees)[, 2, 1])
  } else {
    result[["explainer_fi"]] <- DALEX::explain(result[["model"]], type = "multiclass", data = result[["train"]], y = result[["train"]][, options[["target"]]], predict_function = function(model, data) predict(model, newdata = data, type = "response", n.trees = model$n.trees)[, , 1])
  }
  return(result)
}
