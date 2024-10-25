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

mlRegressionRegularized <- function(jaspResults, dataset, options, ...) {

  # Preparatory work
  dataset <- .mlRegressionRegularizedReadData(dataset, options)
  .mlRegressionErrorHandling(dataset, options, type = "regularized")

  # Check if analysis is ready to run
  ready <- .mlRegressionReady(options, type = "regularized")

  # Compute results and create the model summary table
  .mlRegressionTableSummary(dataset, options, jaspResults, ready, position = 1, type = "regularized")

  # If the user wants to add the values to the data set
  .mlRegressionAddPredictionsToData(dataset, options, jaspResults, ready)

  # Add test set indicator to data
  .mlAddTestIndicatorToData(options, jaspResults, ready, purpose = "regression")

  # Create the data split plot
  .mlPlotDataSplit(dataset, options, jaspResults, ready, position = 2, purpose = "regression", type = "regularized")

  # Create the evaluation metrics table
  .mlRegressionTableMetrics(dataset, options, jaspResults, ready, position = 3)

  # Create the feature importance table
  .mlTableFeatureImportance(options, jaspResults, ready, position = 4, purpose = "regression")

  # Create the shap table
  .mlTableShap(dataset, options, jaspResults, ready, position = 5, purpose = "regression")

  # Create the regression coefficients table
  .mlRegressionRegularizedTableCoef(options, jaspResults, ready, position = 6)

  # Create the predicted performance plot
  .mlRegressionPlotPredictedPerformance(options, jaspResults, ready, position = 8) # position + 1 for regression equation

  # Create the variable trace plot
  .mlRegressionRegularizedPlotVariableTrace(options, jaspResults, ready, position = 9)

  # Create the lambda evaluation plot
  .mlRegressionRegularizedPlotLambda(options, jaspResults, ready, position = 10)
}

# Read dataset
.mlRegressionRegularizedReadData <- function(dataset, options) {
  target <- NULL
  weights <- NULL
  testSetIndicator <- NULL
  if (options[["target"]] != "") {
    target <- options[["target"]]
  }
  if (options[["weights"]] != "") {
    weights <- options[["weights"]]
  }
  if (options[["testSetIndicatorVariable"]] != "" && options[["holdoutData"]] == "testSetIndicator")
    testSetIndicator <- "testSetIndicatorVariable"

  predictors <- unlist(options["predictors"])
  predictors <- predictors[predictors != ""]
  dataset <- .readAndAddCompleteRowIndices(options, c("target", "predictors", "weights"), testSetIndicator)
  if (length(unlist(options[["predictors"]])) > 0 && options[["scaleVariables"]]) {
    dataset[, options[["predictors"]]] <- .scaleNumericData(dataset[, options[["predictors"]], drop = FALSE])
  }
  return(dataset)
}

.regularizedRegression <- function(dataset, options, jaspResults) {
  # Set model-specific parameters
  alpha <- switch(options[["penalty"]],
    "ridge" = 0,
    "lasso" = 1,
    "elasticNet" = options[["alpha"]]
  )
  penalty <- switch(options[["penalty"]],
    "ridge" = gettext("L2 (Ridge)"),
    "lasso" = gettext("L1 (Lasso)"),
    "elasticNet" = gettext("Elastic Net")
  )
  weights <- if (options[["weights"]] != "") dataset[, options[["weights"]]] else rep(1, nrow(dataset))
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
    trainingFit <- glmnet::cv.glmnet(
      x = as.matrix(trainingSet[, options[["predictors"]]]), y = trainingSet[, options[["target"]]],
      nfolds = 10, type.measure = "deviance",
      family = "gaussian", weights = weights[trainingIndex], offset = NULL, alpha = alpha,
      standardize = FALSE, intercept = options[["intercept"]], convergenceThreshold = options[["convergenceThreshold"]]
    )
    lambda <- options[["lambda"]]
    testPredictions <- predict(trainingFit,
      newx = as.matrix(testSet[, options[["predictors"]]]), s = lambda, type = "link", exact = TRUE,
      x = as.matrix(trainingSet[, options[["predictors"]]]), y = trainingSet[, options[["target"]]],
      weights = weights[trainingIndex], offset = NULL,
      alpha = alpha, standardize = FALSE, intercept = options[["intercept"]], convergenceThreshold = options[["convergenceThreshold"]]
    )
  } else {
    # Create a train, validation and test set (optimization)
    validationIndex <- sample.int(nrow(trainingAndValidationSet), size = ceiling(options[["validationDataManual"]] * nrow(trainingAndValidationSet)))
    testSet <- dataset[-trainingIndex, ]
    validationSet <- trainingAndValidationSet[validationIndex, ]
    trainingSet <- trainingAndValidationSet[-validationIndex, ]
    trainingWeights <- weights[trainingIndex]
    trainingFit <- glmnet::cv.glmnet(
      x = as.matrix(trainingSet[, options[["predictors"]]]), y = trainingSet[, options[["target"]]],
      nfolds = 10, type.measure = "deviance",
      family = "gaussian", weights = trainingWeights[-validationIndex], offset = NULL, alpha = alpha,
      standardize = FALSE, intercept = options[["intercept"]], convergenceThreshold = options[["convergenceThreshold"]]
    )
    lambda <- switch(options[["modelOptimization"]],
      "optMin" = trainingFit[["lambda.min"]],
      "opt1SE" = trainingFit[["lambda.1se"]]
    )
    validationPredictions <- predict(trainingFit,
      newx = as.matrix(validationSet[, options[["predictors"]]]), s = lambda, type = "link", exact = TRUE,
      x = as.matrix(trainingSet[, options[["predictors"]]]), y = trainingSet[, options[["target"]]],
      weights = trainingWeights[-validationIndex], offset = NULL,
      alpha = alpha, standardize = FALSE, intercept = options[["intercept"]], convergenceThreshold = options[["convergenceThreshold"]]
    )
    testPredictions <- predict(trainingFit,
      newx = as.matrix(testSet[, options[["predictors"]]]), s = lambda, type = "link", exact = TRUE,
      x = as.matrix(trainingSet[, options[["predictors"]]]), y = trainingSet[, options[["target"]]],
      weights = trainingWeights[-validationIndex], offset = NULL,
      alpha = alpha, standardize = FALSE, intercept = options[["intercept"]], convergenceThreshold = options[["convergenceThreshold"]]
    )
  }
  # Use the specified model to make predictions for dataset
  dataPredictions <- predict(trainingFit,
    newx = as.matrix(dataset[, options[["predictors"]]]), s = lambda, type = "link", exact = TRUE,
    x = as.matrix(dataset[, options[["predictors"]]]), y = dataset[, options[["target"]]], weights = weights, offset = NULL,
    alpha = alpha, standardize = FALSE, intercept = options[["intercept"]], convergenceThreshold = options[["convergenceThreshold"]]
  )
  # Create the formula
  coefs <- coef(trainingFit, s = lambda)
  if (options[["intercept"]]) {
    regform <- paste0(options[["target"]], " = ", round(as.numeric(coefs[, 1])[1], 3))
    start <- 2
    form_coefs <- coefs
  } else {
    regform <- paste0(options[["target"]], " = ")
    start <- 1
    form_coefs <- coefs[-1, , drop = FALSE] # There is still a row with (Intercept) but its value is 0
  }
  for (i in start:nrow(form_coefs)) {
    regform <- paste0(regform, if (round(as.numeric(form_coefs[, 1])[i], 3) < 0) " - " else (if (!options[["intercept"]] && i == 1) "" else " + "), abs(round(as.numeric(form_coefs[, 1])[i], 3)), " x ", rownames(form_coefs)[i])
  }
  result <- list()
  result[["model"]] <- trainingFit
  result[["formula"]] <- regform
  result[["lambda"]] <- lambda
  result[["penalty"]] <- penalty
  result[["alpha"]] <- alpha
  result[["testMSE"]] <- mean((as.numeric(testPredictions) - testSet[, options[["target"]]])^2)
  result[["testReal"]] <- testSet[, options[["target"]]]
  result[["testPred"]] <- as.numeric(testPredictions)
  result[["ntrain"]] <- nrow(trainingSet)
  result[["ntest"]] <- nrow(testSet)
  result[["train"]] <- trainingSet
  result[["test"]] <- testSet
  result[["coefTable"]] <- coefs
  result[["cvMSE"]] <- trainingFit[["cvm"]][trainingFit[["lambda"]] == lambda]
  result[["cvMSELambda"]] <- data.frame(lambda = trainingFit[["lambda"]], MSE = trainingFit[["cvm"]], sd = trainingFit[["cvsd"]])
  result[["testIndicatorColumn"]] <- testIndicatorColumn
  result[["values"]] <- dataPredictions
  if (options[["modelOptimization"]] != "manual") {
    result[["validMSE"]] <- mean((as.numeric(validationPredictions) - validationSet[, options[["target"]]])^2)
    result[["nvalid"]] <- nrow(validationSet)
  }
  result[["explainer"]] <- DALEX::explain(result[["model"]], type = "regression", data = result[["train"]][, options[["predictors"]], drop = FALSE], y = result[["train"]][, options[["target"]]], predict_function = function(model, data) predict(model, newx = as.matrix(data), type = "response", s = result[["lambda"]]))
  return(result)
}

.mlRegressionRegularizedTableCoef <- function(options, jaspResults, ready, position) {
  if (!is.null(jaspResults[["coefTable"]]) || !options[["coefTable"]]) {
    return()
  }
  table <- createJaspTable(gettext("Regression Coefficients"))
  table$position <- position
  table$dependOn(options = c("coefTable", "formula", .mlRegressionDependencies()))
  table$addColumnInfo(name = "var", title = "", type = "string")
  table$addColumnInfo(name = "coefs", title = gettextf("Coefficient (%s)", "\u03B2"), type = "number")
  jaspResults[["coefTable"]] <- table
  if (!ready && options[["target"]] == "" && length(unlist(options[["predictors"]])) > 0) {
    varStrings <- options[["predictors"]]
    if (options[["intercept"]]) {
      varStrings <- c("(Intercept)", varStrings)
    }
    table[["var"]] <- varStrings
  }
  if (!ready) {
    return()
  }
  regressionResult <- jaspResults[["regressionResult"]]$object
  coefTab <- regressionResult[["coefTable"]]
  if (!options[["intercept"]]) {
    labs <- rownames(coefTab)[-1]
    values <- as.numeric(coefTab)[-1]
  } else {
    labs <- c("(Intercept)", rownames(coefTab)[-1])
    values <- as.numeric(coefTab)
  }
  table[["var"]] <- labs
  table[["coefs"]] <- values
  if (options[["formula"]]) {
    formula <- createJaspHtml(gettextf("<b>Regression equation:</b>\n%1$s", regressionResult[["formula"]]), "p")
    formula$position <- position + 1
    formula$dependOn(options = c("coefTable", "formula"), optionsFromObject = jaspResults[["regressionResult"]])
    jaspResults[["regressionFormula"]] <- formula
  }
}

.mlRegressionRegularizedPlotVariableTrace <- function(options, jaspResults, ready, position) {
  if (!is.null(jaspResults[["variableTrace"]]) || !options[["variableTrace"]]) {
    return()
  }
  plot <- createJaspPlot(plot = NULL, title = gettext("Variable Trace Plot"), width = 500, height = 300)
  plot$position <- position
  plot$dependOn(options = c("variableTrace", "variableTraceLegend", .mlRegressionDependencies()))
  jaspResults[["variableTrace"]] <- plot
  if (!ready) {
    return()
  }
  regressionResult <- jaspResults[["regressionResult"]]$object
  model <- regressionResult[["model"]]$glmnet.fit
  coefs <- as.matrix(regressionResult[["model"]]$glmnet.fit$beta)
  plotData <- stack(as.data.frame(coefs))
  plotData[["variable"]] <- rep(rownames(coefs), (nrow(plotData) / nrow(coefs)))
  plotData[["lambda"]] <- rep(model[["lambda"]], each = nrow(coefs))
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(plotData[["lambda"]], min.n = 4)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(plotData[["values"]], min.n = 4)
  p <- ggplot2::ggplot(data = plotData, mapping = ggplot2::aes(x = lambda, y = values)) +
    jaspGraphs::geom_line(mapping = ggplot2::aes(color = variable)) +
    ggplot2::scale_x_continuous(name = "\u03BB", breaks = xBreaks, limits = range(xBreaks)) +
    ggplot2::scale_y_continuous(name = gettext("Coefficients"), breaks = yBreaks, limits = range(yBreaks)) +
    ggplot2::scale_color_manual(values = .mlColorScheme(length(options[["predictors"]]))) +
    ggplot2::labs(color = NULL) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw(legend.position = if (options[["variableTraceLegend"]]) "right" else "none")
  plot$plotObject <- p
}

.mlRegressionRegularizedPlotLambda <- function(options, jaspResults, ready, position) {
  if (!is.null(jaspResults[["lambdaEvaluation"]]) || !options[["lambdaEvaluation"]]) {
    return()
  }
  plot <- createJaspPlot(plot = NULL, title = gettext("Lambda Evaluation Plot"), width = 500, height = 300)
  plot$position <- position
  plot$dependOn(options = c("lambdaEvaluation", "lambdaEvaluationLegend", .mlRegressionDependencies()))
  jaspResults[["lambdaEvaluation"]] <- plot
  if (!ready) {
    return()
  }
  regressionResult <- jaspResults[["regressionResult"]]$object
  tempValues <- c(regressionResult[["cvMSELambda"]]$MSE - regressionResult[["cvMSELambda"]]$sd, regressionResult[["cvMSELambda"]]$MSE + regressionResult[["cvMSELambda"]]$sd)
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(regressionResult[["cvMSELambda"]]$lambda, min.n = 4)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(tempValues, min.n = 4)
  p <- ggplot2::ggplot(data = regressionResult[["cvMSELambda"]], mapping = ggplot2::aes(x = lambda, y = MSE)) +
    ggplot2::geom_ribbon(data = regressionResult[["cvMSELambda"]], mapping = ggplot2::aes(ymin = MSE - sd, ymax = MSE + sd), fill = "grey90") +
    jaspGraphs::geom_line() +
    ggplot2::scale_x_continuous(name = "\u03BB", breaks = xBreaks, limits = range(xBreaks)) +
    ggplot2::scale_y_continuous(name = gettextf("Cross-Validated %sMean Squared Error", "\n"), breaks = yBreaks, limits = range(yBreaks)) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = regressionResult[["model"]]$lambda.min, color = "lambdaMin"), linetype = "dashed") +
    ggplot2::geom_vline(ggplot2::aes(xintercept = regressionResult[["model"]]$lambda.1se, color = "lambda1se"), linetype = "dashed") +
    ggplot2::scale_color_manual(name = NULL, values = c(lambdaMin = "#14a1e3", lambda1se = "#99c454"), labels = c(lambdaMin = gettext("Min. CV MSE"), lambda1se = gettextf("%s 1 SE", "\u03BB"))) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw(legend.position = if (options[["lambdaEvaluationLegend"]]) "top" else "none")
  plot$plotObject <- p
}
