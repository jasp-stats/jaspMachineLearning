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

mlClassificationRandomForest <- function(jaspResults, dataset, options, ...) {

  # Preparatory work
  dataset <- .mlClassificationReadData(dataset, options)
  .mlClassificationErrorHandling(dataset, options, type = "randomForest")

  # Check if analysis is ready to run
  ready <- .mlClassificationReady(options, type = "randomForest")

  # Compute results and create the model summary table
  .mlClassificationTableSummary(dataset, options, jaspResults, ready, position = 1, type = "randomForest")

  # If the user wants to add the classes to the data set
  .mlClassificationAddPredictionsToData(dataset, options, jaspResults, ready)

  # Add test set indicator to data
  .mlAddTestIndicatorToData(options, jaspResults, ready, purpose = "classification")

  # Create the data split plot
  .mlPlotDataSplit(dataset, options, jaspResults, ready, position = 2, purpose = "classification", type = "randomForest")

  # Create the confusion table
  .mlClassificationTableConfusion(dataset, options, jaspResults, ready, position = 3)

  # Create the class proportions table
  .mlClassificationTableProportions(dataset, options, jaspResults, ready, position = 4)

  # Create the validation measures table
  .mlClassificationTableMetrics(dataset, options, jaspResults, ready, position = 5)

  # Create the variable importance table
  .mlRandomForestTableVarImp(options, jaspResults, ready, position = 6, purpose = "classification")

  # Create the shap table
  .mlTableShap(dataset, options, jaspResults, ready, position = 7, purpose = "classification")

  # Create the trees vs model error plot
  .mlRandomForestPlotError(options, jaspResults, ready, position = 8, purpose = "classification")

  # Create the ROC curve
  .mlClassificationPlotRoc(dataset, options, jaspResults, ready, position = 9, type = "randomForest")

  # Create the Andrews curves
  .mlClassificationPlotAndrews(dataset, options, jaspResults, ready, position = 10)

  # Create the mean decrease in accuracy plot
  .mlRandomForestPlotDecreaseAccuracy(options, jaspResults, ready, position = 11, purpose = "classification")

  # Create the total increase in node purity plot
  .mlRandomForestPlotIncreasePurity(options, jaspResults, ready, position = 12, purpose = "classification")

  # Decision boundaries
  .mlClassificationPlotBoundaries(dataset, options, jaspResults, ready, position = 13, type = "randomForest")
}

.randomForestClassification <- function(dataset, options, jaspResults) {
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
  if (options[["modelOptimization"]] == "manual") {
    # Just create a train and a test set (no optimization)
    trainingSet <- trainingAndValidationSet
    testSet <- dataset[-trainingIndex, ]
    testFit <- randomForest::randomForest(
      x = trainingSet[, options[["predictors"]]], y = trainingSet[, options[["target"]]],
      xtest = testSet[, options[["predictors"]]], ytest = testSet[, options[["target"]]],
      ntree = options[["noOfTrees"]], mtry = noOfPredictors,
      sampsize = ceiling(options[["baggingFraction"]] * nrow(trainingSet)),
      importance = TRUE, keep.forest = TRUE
    )
    noOfTrees <- options[["noOfTrees"]]
  } else if (options[["modelOptimization"]] == "optimized") {
    # Create a train, validation and test set (optimization)
    validationIndex <- sample.int(nrow(trainingAndValidationSet), size = ceiling(options[["validationDataManual"]] * nrow(trainingAndValidationSet)))
    testSet <- dataset[-trainingIndex, ]
    validationSet <- trainingAndValidationSet[validationIndex, ]
    trainingSet <- trainingAndValidationSet[-validationIndex, ]
    validationFit <- randomForest::randomForest(
      x = trainingSet[, options[["predictors"]]], y = trainingSet[, options[["target"]]],
      xtest = validationSet[, options[["predictors"]]], ytest = validationSet[, options[["target"]]],
      ntree = options[["maxTrees"]], mtry = noOfPredictors,
      sampsize = ceiling(options[["baggingFraction"]] * nrow(trainingSet)),
      importance = TRUE, keep.forest = TRUE
    )
    oobAccuracy <- 1 - validationFit[["err.rate"]][, 1]
    noOfTrees <- which.max(oobAccuracy)
    testFit <- randomForest::randomForest(
      x = trainingSet[, options[["predictors"]]], y = trainingSet[, options[["target"]]],
      xtest = testSet[, options[["predictors"]]], ytest = testSet[, options[["target"]]],
      ntree = noOfTrees, mtry = noOfPredictors,
      sampsize = ceiling(options[["baggingFraction"]] * nrow(trainingSet)),
      importance = TRUE, keep.forest = TRUE
    )
  }
  # Train a model on the training data
  trainingFit <- randomForest::randomForest(
    x = trainingSet[, options[["predictors"]]], y = trainingSet[, options[["target"]]],
    xtest = trainingSet[, options[["predictors"]]], ytest = trainingSet[, options[["target"]]],
    ntree = noOfTrees, mtry = noOfPredictors,
    sampsize = ceiling(options[["baggingFraction"]] * nrow(trainingSet)),
    importance = TRUE, keep.forest = TRUE
  )
  # Create results object
  result <- list()
  result[["model"]] <- testFit
  result[["rfit_test"]] <- testFit
  result[["rfit_train"]] <- trainingFit
  result[["noOfTrees"]] <- noOfTrees
  result[["predPerSplit"]] <- noOfPredictors
  result[["baggingFraction"]] <- ceiling(options[["baggingFraction"]] * nrow(dataset))
  result[["confTable"]] <- table("Pred" = testFit$test[["predicted"]], "Real" = testSet[, options[["target"]]])
  result[["testAcc"]] <- sum(diag(prop.table(result[["confTable"]])))
  result[["auc"]] <- .classificationCalcAUC(testSet, trainingSet, options, "randomForestClassification", dataset = dataset, noOfTrees = noOfTrees, noOfPredictors = noOfPredictors)
  result[["testPred"]] <- testFit$test[["predicted"]]
  result[["testReal"]] <- testSet[, options[["target"]]]
  result[["ntrain"]] <- nrow(trainingSet)
  result[["ntest"]] <- nrow(testSet)
  result[["train"]] <- trainingSet
  result[["test"]] <- testSet
  result[["testIndicatorColumn"]] <- testIndicatorColumn
  result[["classes"]] <- predict(testFit, newdata = dataset)
  result[["oobAccuracy"]] <- 1 - testFit[["err.rate"]][length(testFit[["err.rate"]])]
  result[["varImp"]] <- plyr::arrange(data.frame(
    Variable = as.factor(names(testFit[["importance"]][, 1])),
    MeanIncrMSE = testFit[["importance"]][, 1],
    TotalDecrNodeImp = testFit[["importance"]][, 2]
  ), -TotalDecrNodeImp)
  if (options[["modelOptimization"]] != "manual") {
    result[["rfit_valid"]] <- validationFit
    result[["validationConfTable"]] <- table("Pred" = validationFit$test[["predicted"]], "Real" = validationSet[, options[["target"]]])
    result[["validAcc"]] <- sum(diag(prop.table(result[["validationConfTable"]])))
    result[["nvalid"]] <- nrow(validationSet)
    result[["valid"]] <- validationSet
    result[["oobValidStore"]] <- oobAccuracy
  }
  result[["explainer"]] <- DALEX::explain(result[["model"]], type = "multiclass", data = result[["train"]], y = result[["train"]][, options[["target"]]] , predict_function = function(model, data) predict(model, newdata = data, type = "prob"))
  if (nlevels(result[["testReal"]]) == 2) {
    result[["explainer_fi"]] <- DALEX::explain(result[["model"]], type = "classification", data = result[["train"]], y = as.numeric(result[["train"]][, options[["target"]]]) , predict_function = function(model, data) predict(model, newdata = data, type = "response"))
  } else {
    result[["explainer_fi"]] <- DALEX::explain(result[["model"]], type = "multiclass", data = result[["train"]], y = result[["train"]][, options[["target"]]] , predict_function = function(model, data) predict(model, newdata = data, type = "prob"))
  }
  return(result)
}
