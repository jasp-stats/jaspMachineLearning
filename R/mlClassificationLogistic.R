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

mlClassificationLogistic <- function(jaspResults, dataset, options, ...) {

  # Preparatory work
  dataset <- .mlClassificationReadData(dataset, options)
  .mlClassificationErrorHandling(dataset, options, type = "logistic")

  # Check if analysis is ready to run
  ready <- .mlClassificationReady(options, type = "logistic")

  # Compute results and create the model summary table
  .mlClassificationTableSummary(dataset, options, jaspResults, ready, position = 1, type = "logistic")

  # If the user wants to add the classes to the data set
  .mlClassificationAddPredictionsToData(dataset, options, jaspResults, ready)

  # Add test set indicator to data
  .mlAddTestIndicatorToData(options, jaspResults, ready, purpose = "classification")

  # Create the data split plot
  .mlPlotDataSplit(dataset, options, jaspResults, ready, position = 2, purpose = "classification", type = "logistic")

  # Create the confusion table
  .mlClassificationTableConfusion(dataset, options, jaspResults, ready, position = 3)

  # Create the class proportions table
  .mlClassificationTableProportions(dataset, options, jaspResults, ready, position = 4)

  # Create the validation measures table
  .mlClassificationTableMetrics(dataset, options, jaspResults, ready, position = 5)

#   # Create the variable importance table
#   .mlTableFeatureImportance(options, jaspResults, ready, position = 6, purpose = "classification")

#   # Create the shap table
#   .mlTableShap(dataset, options, jaspResults, ready, position = 7, purpose = "classification")

#   # Create the ROC curve
#   .mlClassificationPlotRoc(dataset, options, jaspResults, ready, position = 8, type = "logistic")

  # Create the Andrews curves
  .mlClassificationPlotAndrews(dataset, options, jaspResults, ready, position = 9)

#   # Decision boundaries
#   .mlClassificationPlotBoundaries(dataset, options, jaspResults, ready, position = 10, type = "logistic")
}

.logisticRegressionClassification <- function(dataset, options, jaspResults, ready) {
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
  if (nlevels(trainingSet[[options[["target"]]]]) == 2) {
    family = "binomial"
    trainingFit <- stats::glm(formula, data = trainingSet, family = family)
    # Use the specified model to make predictions for dataset
    testPredictions <- levels(trainingSet[[options[["target"]]]])[round(predict(trainingFit, newdata = testSet, type = "response"), 0) + 1]
    dataPredictions <- levels(trainingSet[[options[["target"]]]])[round(predict(trainingFit, newdata = dataset, type = "response"), 0) + 1]
  } else {
    family <- "multinomial"
    trainingFit <- VGAM::vglm(formula, data = trainingSet, family = family)
    # Use the specified model to make predictions for dataset
    testPredictions <- .mlClassificationMultinomialPredictions(trainingSet, options, predict(trainingFit, newdata = testSet))
    dataPredictions <- .mlClassificationMultinomialPredictions(trainingSet, options, predict(trainingFit, newdata = dataset))
  }
  # Create results object
  result <- list()
  result[["formula"]] <- formula
  result[["family"]] <- family
  result[["model"]] <- trainingFit
  result[["confTable"]] <- table("Pred" = testPredictions, "Real" = testSet[, options[["target"]]])
  result[["testAcc"]] <- sum(diag(prop.table(result[["confTable"]])))
#   result[["auc"]] <- .classificationCalcAUC(testSet, trainingSet, options, "logisticClassification")
  result[["ntrain"]] <- nrow(trainingSet)
  result[["ntest"]] <- nrow(testSet)
  result[["testReal"]] <- testSet[, options[["target"]]]
  result[["testPred"]] <- testPredictions
  result[["train"]] <- trainingSet
  result[["test"]] <- testSet
  result[["testIndicatorColumn"]] <- testIndicatorColumn
  result[["classes"]] <- dataPredictions
#   result[["explainer"]] <- DALEX::explain(result[["model"]], type = "classification", data = result[["train"]], y = result[["train"]][, options[["target"]]], predict_function = function(model, data) predict(model, newdata = data, type = "raw"))
#   if (nlevels(result[["testReal"]]) == 2) {
#     result[["explainer_fi"]] <- DALEX::explain(result[["model"]], type = "classification", data = result[["train"]], y = as.numeric(result[["train"]][, options[["target"]]]) - 1, predict_function = function(model, data) predict(model, newdata = data, type = "class"))
#   } else {
#     result[["explainer_fi"]] <- DALEX::explain(result[["model"]], type = "multiclass", data = result[["train"]], y = result[["train"]][, options[["target"]]] , predict_function = function(model, data) predict(model, newdata = data, type = "raw"))
#   }
  return(result)
}

.mlClassificationMultinomialPredictions <- function(trainingSet, options, predictions) {
  num_categories <- ncol(predictions) + 1
  probs <- matrix(0, nrow = nrow(predictions), ncol = num_categories)
  for (i in 1:(num_categories - 1)) {
    probs[, i] <- exp(predictions[, i])
  }
  probs[, num_categories] <- 1
  row_sums <- rowSums(probs)
  probs <- probs / row_sums
  predicted_category <- apply(probs, 1, which.max)
  categories <- levels(trainingSet[[options[["target"]]]])
  predicted_categories <- categories[predicted_category]
  return(predicted_categories)
}
