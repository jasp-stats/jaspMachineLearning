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

  # Create the support vectors table
  .mlSvmTableSupportVectors(options, jaspResults, ready, position = 3, purpose = "regression")

  # Create the predicted performance plot
  .mlRegressionPlotPredictedPerformance(options, jaspResults, ready, position = 5)
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
  trainingSet <- dataset[trainingIndex, ]
  # Create the generated test set indicator
  testIndicatorColumn <- rep(1, nrow(dataset))
  testIndicatorColumn[trainingIndex] <- 0
  # Just create a train and a test set (no optimization)
  testSet <- dataset[-trainingIndex, ]
  trainingFit <- e1071::svm(
    formula = formula, data = trainingSet, type = "eps-regression", kernel = options[["weights"]], cost = options[["cost"]], tolerance = options[["tolerance"]],
    epsilon = options[["epsilon"]], scale = FALSE, degree = options[["degree"]], gamma = options[["gamma"]], coef0 = options[["cp"]]
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

.mlSvmTableSupportVectors <- function(options, jaspResults, ready, position, purpose) {
  if (!is.null(jaspResults[["tableSupportVectors"]]) || !options[["tableSupportVectors"]]) {
    return()
  }
  table <- createJaspTable(title = gettext("Support Vectors"))
  table$position <- position
  table$dependOn(options = c(
    "tableSupportVectors", "trainingDataManual", "scaleEqualSD", "target", "predictors", "seed", "seedBox",
    "testSetIndicatorVariable", "testSetIndicator", "holdoutData", "testDataManual", "weights", "cost", "tolerance", "epsilon"
  ))
  table$addColumnInfo(name = "row", title = gettext("Row"), type = "string")
  jaspResults[["tableSupportVectors"]] <- table
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
