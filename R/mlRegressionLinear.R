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
  dataset <- .mlRegressionRegularizedReadData(dataset, options)
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

  # Create the feature importance table
  .mlTableFeatureImportance(options, jaspResults, ready, position = 4, purpose = "regression")

  # Create the shap table
  .mlTableShap(dataset, options, jaspResults, ready, position = 5, purpose = "regression")

  # Create the regression coefficients table
  .mlRegressionLinearTableCoef(options, jaspResults, ready, position = 6)

  # Create the predicted performance plot
  .mlRegressionPlotPredictedPerformance(options, jaspResults, ready, position = 8) # position + 1 for regression equation
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
  if (options[["intercept"]]) {
    formula <- formula(paste(options[["target"]], "~ 1 + ", paste(options[["predictors"]], collapse = " + ")))
  } else {
    formula <- formula(paste(options[["target"]], "~ 0 + ", paste(options[["predictors"]], collapse = " + ")))
  }
  fit <- stats::lm(formula, data = trainingSet[, c(options[["target"]], options[["predictors"]])], weights = if (options[["weights"]] != "") trainingSet[, options[["weights"]]] else NULL)
  # Use the specified model to make predictions for the test set
  testFit <- predict(fit, newdata = testSet)
  # Use the specified model to make predictions for dataset
  dataPredictions <- predict(fit, newdata = dataset)
  # Create the coefficients table
  coefs <- summary(fit)$coefficients
  vars <- rownames(coefs)
  for (i in seq_along(vars)) {
    if (!(vars[i] %in% options[["predictors"]]) && vars[i] != "(Intercept)") {
      for (j in options[["predictors"]]) {
        vars[i] <- gsub(pattern = j, replacement = paste0(j, " ("), x = vars[i])
      }
      vars[i] <- paste0(vars[i], ")")
    }
  }
  coefs <- cbind(coefs, confint(fit, level = options[["coefTableConfIntLevel"]]))
  rownames(coefs) <- vars
  colnames(coefs) <- c("coefficient", "se", "t", "p", "lower", "upper")
  # Create the formula
  if (options[["intercept"]]) {
    regform <- paste0(options[["target"]], " = ", round(as.numeric(coefs[, 1])[1], 3))
    start <- 2
  } else {
    regform <- paste0(options[["target"]], " = ")
    start <- 1
  }
  for (i in start:nrow(coefs)) {
    regform <- paste0(regform, if (round(as.numeric(coefs[, 1])[i], 3) < 0) " - " else " + ", abs(round(as.numeric(coefs[, 1])[i], 3)), " x ", vars[i])
  }
  # Create results object
  result <- list()
  result[["model"]] <- fit
  result[["coefficients"]] <- coefs
  result[["formula"]] <- regform
  result[["testMSE"]] <- mean((testFit - testSet[, options[["target"]]])^2)
  result[["rsquared"]] <- summary(fit)[["r.squared"]]
  result[["arsquared"]] <- summary(fit)[["adj.r.squared"]]
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
  table$dependOn(options = c("coefTable", "coefTableConfInt", "coefTableConfIntLevel", "formula", .mlRegressionDependencies()))
  table$addColumnInfo(name = "var", title = "", type = "string")
  table$addColumnInfo(name = "coefs", title = gettextf("Coefficient (%s)", "\u03B2"), type = "number")
  table$addColumnInfo(name = "se", title = gettext("Standard Error"), type = "number")
  table$addColumnInfo(name = "t", title = gettext("t"), type = "number")
  table$addColumnInfo(name = "p", title = gettext("p"), type = "pvalue")
  if (options[["coefTableConfInt"]]) {
    overtitle <- gettextf("%1$s%% Confidence interval", round(options[["coefTableConfIntLevel"]] * 100, 3))
    table$addColumnInfo(name = "lower", title = gettext("Lower"), type = "number", overtitle = overtitle)
	table$addColumnInfo(name = "upper", title = gettext("Upper"), type = "number", overtitle = overtitle)
  }
  if (options[["scaleVariables"]]) {
    table$addFootnote(gettext("The regression coefficients for numeric features are standardized."))
  } else {
    table$addFootnote(gettext("The regression coefficients are unstandardized."))
  }
  jaspResults[["coefTable"]] <- table
  if (!ready) {
    if (options[["target"]] == "" && length(unlist(options[["predictors"]])) > 0) {
      table[["var"]] <- c(if (options[["intercept"]]) "(Intercept)" else NULL, options[["predictors"]])
    }
    return()
  }
  regressionResult <- jaspResults[["regressionResult"]]$object
  coefs <- regressionResult[["coefficients"]]
  table[["var"]] <- rownames(coefs)
  table[["coefs"]] <- as.numeric(coefs[, "coefficient"])
  table[["se"]] <- as.numeric(coefs[, "se"])
  table[["t"]] <- as.numeric(coefs[, "t"])
  table[["p"]] <- as.numeric(coefs[, "p"])
  if (options[["coefTableConfInt"]]) {
    table[["lower"]] <- coefs[, "lower"]
    table[["upper"]] <- coefs[, "upper"]
  }
  if (options[["formula"]]) {
    formula <- createJaspHtml(gettextf("<b>Regression equation:</b>\n%1$s", regressionResult[["formula"]]), "p")
    formula$position <- position + 1
    formula$dependOn(options = c("coefTable", "formula"), optionsFromObject = jaspResults[["regressionResult"]])
    jaspResults[["regressionFormula"]] <- formula
  }
}
