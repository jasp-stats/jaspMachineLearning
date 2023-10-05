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

mlRegressionLinear <- function(jaspResults, dataset, options, ...) {

  # Preparatory work
  dataset <- .readDataRegressionAnalyses(dataset, options)
  .mlRegressionErrorHandling(dataset, options, type = "lm")

  # Check if analysis is ready to run
  ready <- .mlRegressionReady(options, type = "lm")

  # Compute results and create the model summary table
  .mlRegressionTableSummary(dataset, options, jaspResults, ready, position = 1, type = "lm")

  # If the user wants to add the values to the data set
  .mlRegressionAddPredictionsToData(dataset, options, jaspResults, ready)

  # Add test set indicator to data
  .mlAddTestIndicatorToData(options, jaspResults, ready, purpose = "regression")

  # Create the data split plot
  .mlPlotDataSplit(dataset, options, jaspResults, ready, position = 2, purpose = "regression", type = "lm")

  # Create the evaluation metrics table
  .mlRegressionTableMetrics(dataset, options, jaspResults, ready, position = 3)

  # Create the shap table
  .mlTableShap(dataset, options, jaspResults, ready, position = 4, purpose = "regression")

  # Create the regression coefficients table
  .mlRegressionLinearTableCoef(options, jaspResults, ready, position = 5)

  # Create the predicted performance plot
  .mlRegressionPlotPredictedPerformance(options, jaspResults, ready, position = 6)
}

.linearRegression <- function(dataset, options, jaspResults) {
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
  # Just create a train and a test set (no optimization)
  trainingSet <- trainingAndValidationSet
  testSet <- dataset[-trainingIndex, ]
  fit <- lm(jaspResults[["formula"]]$object, data = trainingSet)
  # Use the specified model to make predictions for the test set
  testFit <- predict(fit, newdata = testSet)
  # Use the specified model to make predictions for dataset
  dataPredictions <- predict(fit, newdata = dataset)
  # Create results object
  result <- list()
  result[["model"]] <- fit
  result[["testMSE"]] <- mean((testFit - testSet[, options[["target"]]])^2)
  result[["testPred"]] <- testFit
  result[["testReal"]] <- testSet[, options[["target"]]]
  result[["ntrain"]] <- nrow(trainingSet)
  result[["ntest"]] <- nrow(testSet)
  result[["train"]] <- trainingSet
  result[["test"]] <- testSet
  result[["testIndicatorColumn"]] <- testIndicatorColumn
  result[["values"]] <- dataPredictions
  result[["explainer"]] <- DALEX::explain(result[["model"]], type = "regression", data = result[["train"]][, options[["predictors"]], drop = FALSE], y = result[["train"]][, options[["target"]]], predict_function = function(model, data) predict(model, newdata = data))
  return(result)
}

.mlRegressionLinearTableCoef <- function(options, jaspResults, ready, position) {
  if (!is.null(jaspResults[["coefTable"]]) || !options[["coefTable"]]) {
    return()
  }
  table <- createJaspTable(gettext("Regression Coefficients"))
  table$position <- position
  table$dependOn(options = c("coefTable", .mlRegressionDependencies()))
  table$addColumnInfo(name = "var", title = "", type = "string")
  table$addColumnInfo(name = "coefs", title = gettextf("Coefficient (%s)", "\u03B2"), type = "number")
  jaspResults[["coefTable"]] <- table
  if (!ready && options[["target"]] == "" && length(unlist(options[["predictors"]])) > 0) {
    varStrings <- options[["predictors"]]
    varStrings <- c("(Intercept)", varStrings)
    table[["var"]] <- varStrings
  }
  if (!ready) {
    return()
  }
  regressionResult <- jaspResults[["regressionResult"]]$object
  coefs <- coef(regressionResult[["model"]])
  table[["var"]] <- c("(Intercept)", names(coefs)[-1])
  table[["coefs"]] <- as.numeric(coefs)
}

.mlRegressionFormatCoefNames <- function(names) {
  # Use regular expressions to separate variable and level
  formatted_names <- gsub("(\\w+)(\\w+)$", "\\1 = \\2", names)
  return(formatted_names)
}
