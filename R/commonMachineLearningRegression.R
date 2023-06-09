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

.mlColorScheme <- function(n) {
  colors <- colorspace::qualitative_hcl(n)
  return(colors)
}

.mlRegressionDependencies <- function(options, includeSaveOptions = FALSE) {
  opt <- c(
    "noOfNearestNeighbours", "trainingDataManual", "distanceParameterManual", "weights", "scaleVariables", "modelOptimization", "maxTrees",
    "target", "predictors", "seed", "setSeed", "validationLeaveOneOut", "confusionProportions", "maxNearestNeighbors", "noOfFolds", "modelValid",
    "penalty", "alpha", "convergenceThreshold", "intercept", "shrinkage", "lambda", "noOfTrees", "noOfPredictors", "numberOfPredictors", "baggingFraction",
    "interactionDepth", "minObservationsInNode", "distance", "testSetIndicatorVariable", "testSetIndicator", "validationDataManual", "minObservationsForSplit",
    "holdoutData", "testDataManual", "complexityParameter", "degree", "gamma",
    "threshold", "algorithm", "learningRate", "lossFunction", "actfct", "layers", "maxTrainingRepetitions", "maxGenerations", "populationSize", "maxLayers", "maxNodes", "mutationRate", "elitism", "selectionMethod", "crossoverMethod", "mutationMethod", "survivalMethod", "elitismProportion", "candidates"
  )
  if (includeSaveOptions) {
    opt <- c(opt, "saveModel", "savePath")
  }
  return(opt)
}

.readDataRegressionAnalyses <- function(dataset, options, jaspResults) {
  if (is.null(dataset)) {
    dataset <- .readDataClassificationRegressionAnalyses(dataset, options)
  }
  if (length(unlist(options[["predictors"]])) > 0 && options[["target"]] != "" && options[["scaleVariables"]]) {
    dataset[, c(options[["predictors"]], options[["target"]])] <- .scaleNumericData(dataset[, c(options[["predictors"]], options[["target"]]), drop = FALSE])
  }
  return(dataset)
}

.readDataClassificationRegressionAnalyses <- function(dataset, options) {
  target <- NULL
  if (options[["target"]] != "") {
    target <- options[["target"]]
  }
  predictors <- NULL
  if (length(options[["predictors"]]) > 0) {
    predictors <- unlist(options[["predictors"]])
  }
  testSetIndicator <- NULL
  if (options[["testSetIndicatorVariable"]] != "" && options[["holdoutData"]] == "testSetIndicator") {
    testSetIndicator <- options[["testSetIndicatorVariable"]]
  }
  return(.readAndAddCompleteRowIndices(dataset, columns = c(target, predictors), columnsAsNumeric = testSetIndicator))
}

.readAndAddCompleteRowIndices <- function(dataset, columns = NULL, columnsAsNumeric = NULL) {
  dataset <- .readDataSetToEnd(columns = columns, columns.as.numeric = columnsAsNumeric)
  complete.index <- which(complete.cases(dataset))
  dataset <- na.omit(dataset)
  rownames(dataset) <- as.character(complete.index)
  return(dataset)
}

.mlRegressionErrorHandling <- function(dataset, options, type) {
  .errorHandlingClassificationRegressionAnalyses(dataset, options, type)
  if (type == "regularized" &&
    "weights" %in% names(options) && !is.null(options[["weights"]]) && options[["weights"]] != "") {
    .hasErrors(dataset,
      type = c("infinity", "limits", "observations"),
      all.target = options[["weights"]], limits.min = 0, observations.amount = "< 2",
      exitAnalysisIfErrors = TRUE
    )
  }
}

.errorHandlingClassificationRegressionAnalyses <- function(dataset, options, type) {
  predictors <- unlist(options["predictors"])
  predictors <- predictors[predictors != ""]
  target <- NULL
  if (options[["target"]] != "") {
    target <- options[["target"]]
  }
  variables <- c(predictors, target)
  if (length(variables) == 0) {
    return()
  }
  if (options[["testSetIndicatorVariable"]] != "" && options[["holdoutData"]] == "testSetIndicator") {
    if (options[["testSetIndicatorVariable"]] %in% predictors) {
      jaspBase:::.quitAnalysis(gettextf("The variable '%s' can't be both a feature and a test set indicator.", options[["testSetIndicatorVariable"]]))
    }
    indicatorVals <- unique(dataset[, options[["testSetIndicatorVariable"]]])
    if (length(indicatorVals) != 2 || !all(0:1 %in% indicatorVals)) {
      jaspBase:::.quitAnalysis(gettext("Your test set indicator should be binary, containing only 1 (included in test set) and 0 (excluded from test set)."))
    }
  }
  customChecks <- .getCustomErrorChecksKnnBoosting(dataset, options, type)
  .hasErrors(dataset,
    type = c("infinity", "observations", "variance"), custom = customChecks,
    all.target = variables,
    observations.amount = "< 2",
    exitAnalysisIfErrors = TRUE
  )
  if (options[["target"]] != "" && length(options[["predictors"]]) > 0) {
    predictorData <- dataset[, options[["predictors"]], drop = FALSE]
    predictorsAreFactors <- vapply(predictorData, FUN = function(x) is.character(x) || is.factor(x), FUN.VALUE = logical(1L))
    if (any(predictorsAreFactors)) {
      predictorFactorsWithUniqueLevels <- which(sapply(predictorData[, predictorsAreFactors, drop = FALSE], FUN = function(x) length(x) == length(unique(x))))
      if (length(predictorFactorsWithUniqueLevels) == 1) {
        jaspBase:::.quitAnalysis(gettextf("There is only one observation in each level of the factor %1$s, please remove this factor as a feature.", names(predictorFactorsWithUniqueLevels)))
      } else if (length(predictorFactorsWithUniqueLevels) > 1) {
        jaspBase:::.quitAnalysis(gettextf("There is only one observation in each level of the factors %1$s, please remove these factors as a feature.", paste(names(predictorFactorsWithUniqueLevels), sep = "&")))
      }
    }
  }
}

.getCustomErrorChecksKnnBoosting <- function(dataset, options, type) {
  if (!type %in% c("knn", "boosting")) {
    return()
  }
  if (options[["testSetIndicatorVariable"]] != "" && options[["holdoutData"]] == "testSetIndicator") {
    nTrainAndValid <- length(which(dataset[, options[["testSetIndicatorVariable"]]] == 0))
  } else {
    nTrainAndValid <- ceiling(nrow(dataset) - nrow(dataset) * options[["testDataManual"]])
  }
  # check for too many nearest neighbors (nn > nTrain) before the analysis starts
  checkNearestNeighbors <- function() {
    if (type != "knn") {
      return()
    }
    nn <- switch(options[["modelOptimization"]],
      "manual" = options[["noOfNearestNeighbours"]],
      "optimized" = options[["maxNearestNeighbors"]]
    )
    valueToTest <- nTrainAndValid
    if (options[["modelOptimization"]] == "optimized") {
      if (options[["modelValid"]] == "validationManual") {
        nTrain <- ceiling(nTrainAndValid - nTrainAndValid * options[["validationDataManual"]])
      }
      if (options[["modelValid"]] == "validationKFold") {
        nTrain <- ceiling(nTrainAndValid - nTrainAndValid / (options[["noOfFolds"]] - 1))
      }
      if (options[["modelValid"]] == "validationLeaveOneOut") {
        nTrain <- nTrainAndValid - 1
      }
      valueToTest <- nTrain
    }
    if (nn >= valueToTest) {
      return(gettextf("You have specified more nearest neighbors than there are observations in the training set. Please choose a number lower than %d.", as.integer(valueToTest)))
    }
  }
  # check for too many folds (folds > nTrain+validation) before the analysis starts
  checkIfFoldsExceedValidation <- function() {
    if (options[["modelValid"]] == "validationKFold") {
      kFolds <- options[["noOfFolds"]]
      if (kFolds > nTrainAndValid) {
        return(gettextf("You have specified more folds than there are observations in the training and validation set. Please choose a number lower than %d.", as.integer(nTrainAndValid + 1)))
      }
    }
  }
  return(list(checkNearestNeighbors, checkIfFoldsExceedValidation))
}

.mlRegressionReady <- function(options, type) {
  if (type == "randomForest" || type == "boosting" || type == "regularized") {
    ready <- length(options[["predictors"]][options[["predictors"]] != ""]) >= 2 && options[["target"]] != ""
  } else if (type == "knn" || type == "neuralnet" || type == "rpart" || type == "svm") {
    ready <- length(options[["predictors"]][options[["predictors"]] != ""]) >= 1 && options[["target"]] != ""
  }
  return(ready)
}

.mlRegressionSetFormula <- function(options, jaspResults) {
  predictors <- options[["predictors"]]
  target <- options[["target"]]
  formula <- formula(paste(target, "~", paste(predictors, collapse = " + ")))
  jaspResults[["formula"]] <- createJaspState(formula)
  jaspResults[["formula"]]$dependOn(options = c("predictors", "target"))
}

.mlRegressionComputeResults <- function(dataset, options, jaspResults, ready, type) {
  if (!is.null(jaspResults[["regressionResult"]])) {
    return()
  }
  # set the seed so that every time the same set is chosen (to prevent random results) ##
  if (options[["setSeed"]]) {
    set.seed(options[["seed"]])
  }
  if (ready) {
    .mlRegressionSetFormula(options, jaspResults)
    regressionResult <- switch(type,
      "knn" = .knnRegression(dataset, options, jaspResults),
      "regularized" = .regularizedRegression(dataset, options, jaspResults),
      "randomForest" = .randomForestRegression(dataset, options, jaspResults),
      "boosting" = .boostingRegression(dataset, options, jaspResults),
      "neuralnet" = .neuralnetRegression(dataset, options, jaspResults),
      "rpart" = .decisionTreeRegression(dataset, options, jaspResults),
      "svm" = .svmRegression(dataset, options, jaspResults)
    )
    jaspResults[["regressionResult"]] <- createJaspState(regressionResult)
    jaspResults[["regressionResult"]]$dependOn(options = .mlRegressionDependencies(options))
  }
}

.mlRegressionTableSummary <- function(dataset, options, jaspResults, ready, position, type) {
  if (!is.null(jaspResults[["regressionTable"]])) {
    return()
  }
  title <- switch(type,
    "knn" = gettext("K-Nearest Neighbors Regression"),
    "regularized" = gettext("Regularized Linear Regression"),
    "randomForest" = gettext("Random Forest Regression"),
    "boosting" = gettext("Boosting Regression"),
    "neuralnet" = gettext("Neural Network Regression"),
    "rpart" = gettext("Decision Tree Regression"),
    "svm" = gettext("Support Vector Machine Regression")
  )
  table <- createJaspTable(title)
  table$position <- position
  table$dependOn(options = .mlRegressionDependencies(options, includeSaveOptions = TRUE))
  # Add analysis-specific columns
  if (type == "knn") {
    table$addColumnInfo(name = "nn", title = gettext("Nearest neighbors"), type = "integer")
    table$addColumnInfo(name = "weights", title = gettext("Weights"), type = "string")
    table$addColumnInfo(name = "distance", title = gettext("Distance"), type = "string")
  } else if (type == "regularized") {
    table$addColumnInfo(name = "penalty", title = gettext("Penalty"), type = "string")
    if (options[["penalty"]] == "elasticNet") {
      table$addColumnInfo(name = "alpha", title = "\u03B1", type = "number")
    }
    table$addColumnInfo(name = "lambda", title = "\u03BB", type = "number")
  } else if (type == "randomForest") {
    table$addColumnInfo(name = "trees", title = gettext("Trees"), type = "integer")
    table$addColumnInfo(name = "preds", title = gettext("Features per split"), type = "integer")
  } else if (type == "boosting") {
    table$addColumnInfo(name = "trees", title = gettext("Trees"), type = "integer")
    table$addColumnInfo(name = "shrinkage", title = gettext("Shrinkage"), type = "number")
    table$addColumnInfo(name = "distribution", title = gettext("Loss function"), type = "integer")
  } else if (type == "neuralnet") {
    table$addColumnInfo(name = "layers", title = gettext("Hidden Layers"), type = "integer")
    table$addColumnInfo(name = "nodes", title = gettext("Nodes"), type = "integer")
  } else if (type == "rpart") {
    table$addColumnInfo(name = "splits", title = gettext("Splits"), type = "integer")
  } else if (type == "svm") {
    table$addColumnInfo(name = "vectors", title = gettext("Support Vectors"), type = "integer")
  }
  # Add common columns
  table$addColumnInfo(name = "nTrain", title = gettext("n(Train)"), type = "integer")
  if (options[["modelOptimization"]] != "manual") {
    table$addColumnInfo(name = "nValid", title = gettext("n(Validation)"), type = "integer")
  }
  table$addColumnInfo(name = "nTest", title = gettext("n(Test)"), type = "integer")
  if (options[["modelOptimization"]] != "manual") {
    table$addColumnInfo(name = "validMSE", title = gettext("Validation MSE"), type = "number")
  }
  table$addColumnInfo(name = "testMSE", title = gettext("Test MSE"), type = "number")
  # Add analysis-specific columns after common columns
  if (type == "randomForest") {
    table$addColumnInfo(name = "oob", title = gettext("OOB Error"), type = "number")
  }
  # If no analysis is run, specify the required variables in a footnote
  if (!ready) {
    table$addFootnote(gettextf("Please provide a target variable and at least %d feature variable(s).", if (type == "knn" || type == "neuralnet" || type == "rpart" || type == "svm") 1L else 2L))
  }
  if (options[["savePath"]] != "") {
    if (options[["saveModel"]]) {
      table$addFootnote(gettextf("The trained model is saved as <i>%1$s</i>.", basename(options[["savePath"]])))
    } else {
      table$addFootnote(gettext("The trained model is not saved until 'Save trained model' is checked."))
    }
  }
  jaspResults[["regressionTable"]] <- table
  if (!ready) {
    return()
  }
  .mlRegressionComputeResults(dataset, options, jaspResults, ready, type = type)
  regressionResult <- jaspResults[["regressionResult"]]$object
  nTrain <- regressionResult[["ntrain"]]
  if (options[["modelOptimization"]] != "manual") {
    nValid <- regressionResult[["nvalid"]]
    if (options[["modelValid"]] == "validationKFold") {
      # Adjust displayed train and test size for cross-validation
      nValid <- floor(nValid / options[["noOfFolds"]])
      nTrain <- nTrain - nValid
    } else if (options[["modelValid"]] == "validationLeaveOneOut") {
      nValid <- 1
      nTrain <- nTrain - 1
    }
  }
  # Fill the table per analysis
  if (type == "knn") {
    if (options[["modelOptimization"]] == "optimized") {
      table$addFootnote(gettext("The model is optimized with respect to the <i>validation set mean squared error</i>."))
    }
    if (regressionResult[["nn"]] == options[["maxNearestNeighbors"]] && options[["modelOptimization"]] != "optimized") {
      table$addFootnote(gettext("The optimum number of nearest neighbors is the maximum number. You might want to adjust the range of optimization."))
    }
    distance <- ifelse(regressionResult[["distance"]] == 1, yes = "Manhattan", no = "Euclidean")
    row <- data.frame(
      nn = regressionResult[["nn"]],
      weights = regressionResult[["weights"]],
      distance = distance,
      nTrain = nTrain,
      nTest = regressionResult[["ntest"]],
      testMSE = regressionResult[["testMSE"]]
    )
    if (options[["modelOptimization"]] != "manual") {
      row <- cbind(row, nValid = nValid, validMSE = regressionResult[["validMSE"]])
    }
    table$addRows(row)
  } else if (type == "regularized") {
    if (options[["modelOptimization"]] != "manual") {
      table$addFootnote(gettext("The model is optimized with respect to the <i>validation set mean squared error</i>."))
    }
    if (regressionResult[["lambda"]] == 0) {
      table$addFootnote(gettextf("When %s is set to 0 linear regression is performed.", "\u03BB"))
    }
    row <- data.frame(
      penalty = regressionResult[["penalty"]],
      lambda = regressionResult[["lambda"]],
      nTrain = nTrain,
      nTest = regressionResult[["ntest"]],
      testMSE = regressionResult[["testMSE"]]
    )
    if (options[["modelOptimization"]] != "manual") {
      row <- cbind(row, nValid = nValid, validMSE = regressionResult[["validMSE"]])
    }
    if (options[["penalty"]] == "elasticNet") {
      row <- cbind(row, alpha = regressionResult[["alpha"]])
    }
    table$addRows(row)
  } else if (type == "randomForest") {
    if (options[["modelOptimization"]] == "optimized") {
      table$addFootnote(gettext("The model is optimized with respect to the <i>out-of-bag mean squared error</i>."))
    }
    row <- data.frame(
      trees = regressionResult[["noOfTrees"]],
      preds = regressionResult[["predPerSplit"]],
      nTrain = nTrain,
      nTest = regressionResult[["ntest"]],
      testMSE = regressionResult[["testMSE"]],
      oob = regressionResult[["oobError"]]
    )
    if (options[["modelOptimization"]] != "manual") {
      row <- cbind(row, nValid = nValid, validMSE = regressionResult[["validMSE"]])
    }
    table$addRows(row)
  } else if (type == "boosting") {
    if (options[["modelOptimization"]] == "optimized") {
      table$addFootnote(gettext("The model is optimized with respect to the <i>out-of-bag mean squared error</i>."))
    }
    distribution <- .regressionGetDistributionFromDistance(options[["distance"]])
    row <- data.frame(
      trees = regressionResult[["noOfTrees"]],
      shrinkage = options[["shrinkage"]],
      distribution = distribution,
      nTrain = nTrain,
      nTest = regressionResult[["ntest"]],
      testMSE = regressionResult[["testMSE"]]
    )
    if (options[["modelOptimization"]] != "manual") {
      row <- cbind(row, nValid = nValid, validMSE = regressionResult[["validMSE"]])
    }
    table$addRows(row)
  } else if (type == "neuralnet") {
    if (options[["modelOptimization"]] == "manual") {
      table$addFootnote(gettext("The model is optimized with respect to the <i>sum of squares</i>."))
    } else if (options[["modelOptimization"]] == "optimized") {
      table$addFootnote(gettext("The model is optimized with respect to the <i>validation set mean squared error</i>."))
    }
    row <- data.frame(
      layers = regressionResult[["nLayers"]],
      nodes = regressionResult[["nNodes"]],
      nTrain = nTrain,
      nTest = regressionResult[["ntest"]],
      testMSE = regressionResult[["testMSE"]]
    )
    if (options[["modelOptimization"]] != "manual") {
      row <- cbind(row, nValid = nValid, validMSE = regressionResult[["validMSE"]])
    }
    table$addRows(row)
  } else if (type == "rpart") {
    splits <- if (!is.null(regressionResult[["model"]]$splits)) nrow(regressionResult[["model"]]$splits) else 0
    row <- data.frame(
      splits = splits,
      nTrain = nTrain,
      nTest = regressionResult[["ntest"]],
      testMSE = regressionResult[["testMSE"]]
    )
    table$addRows(row)
  } else if (type == "svm") {
    row <- data.frame(
      vectors = nrow(regressionResult[["model"]]$SV),
      nTrain = nTrain,
      nTest = regressionResult[["ntest"]],
      testMSE = regressionResult[["testMSE"]]
    )
    table$addRows(row)
  }
  # Save the model if requested
  if (options[["saveModel"]] && options[["savePath"]] != "") {
    model <- regressionResult[["model"]]
    model[["jaspVars"]] <- decodeColNames(options[["predictors"]])
    model[["jaspVersion"]] <- .baseCitation
    model <- .decodeJaspMLobject(model)
    class(model) <- c(class(regressionResult[["model"]]), "jaspRegression", "jaspMachineLearning")
    saveRDS(model, file = options[["savePath"]])
  }
}

.regressionGetDistributionFromDistance <- function(distance) {
  return(switch(distance,
    "tdist"    = gettext("t"),
    "gaussian" = gettext("Gaussian"),
    "laplace"  = gettext("Laplace")
  ))
}

.mlRegressionTableMetrics <- function(dataset, options, jaspResults, ready, position) {
  if (!is.null(jaspResults[["validationMeasures"]]) || !options[["validationMeasures"]]) {
    return()
  }
  table <- createJaspTable(title = "Evaluation Metrics")
  table$position <- position
  table$dependOn(options = c(.mlRegressionDependencies(options), "validationMeasures"))
  table$addColumnInfo(name = "measures", title = "", type = "string")
  table$addColumnInfo(name = "values", title = gettext("Value"), type = "string")
  measures <- c("MSE", "RMSE", "MAE / MAD", "MAPE", "R\u00B2")
  table[["measures"]] <- measures
  jaspResults[["validationMeasures"]] <- table
  if (!ready) {
    return()
  }
  regressionResult <- jaspResults[["regressionResult"]]$object
  predDat <- data.frame(obs = unname(regressionResult[["testReal"]]), pred = unname(regressionResult[["testPred"]]))
  predDat <- predDat[complete.cases(predDat), ]
  obs <- predDat[["obs"]]
  pred <- predDat[["pred"]]
  mse <- round(regressionResult[["testMSE"]], 3)
  rmse <- round(sqrt(mse), 3)
  mae <- round(mean(abs(obs - pred)), 3)
  mape <- paste0(round(mean(abs((obs - pred) / obs)) * 100, 2), "%")
  r_squared <- round(cor(obs, pred)^2, 3)
  values <- c(mse, rmse, mae, mape, r_squared)
  table[["values"]] <- values
  if (is.na(r_squared)) {
    table$addFootnote(gettextf("R%s cannot be computed due to lack of variance in the predictions.</i>", "\u00B2"))
  }
}

.mlRegressionPlotPredictedPerformance <- function(options, jaspResults, ready, position) {
  if (!is.null(jaspResults[["predictedPerformancePlot"]]) || !options[["predictedPerformancePlot"]]) {
    return()
  }
  plot <- createJaspPlot(plot = NULL, title = gettext("Predictive Performance Plot"), width = 400, height = 300)
  plot$position <- position
  plot$dependOn(options = c(.mlRegressionDependencies(options), "predictedPerformancePlot"))
  jaspResults[["predictedPerformancePlot"]] <- plot
  if (!ready) {
    return()
  }
  regressionResult <- jaspResults[["regressionResult"]]$object
  predPerformance <- data.frame(true = c(regressionResult[["testReal"]]), predicted = regressionResult[["testPred"]])
  breaks <- jaspGraphs::getPrettyAxisBreaks(unlist(predPerformance), min.n = 4)
  p <- ggplot2::ggplot(data = predPerformance, mapping = ggplot2::aes(x = true, y = predicted)) +
    jaspGraphs::geom_line(data = data.frame(x = range(breaks), y = range(breaks)), mapping = ggplot2::aes(x = x, y = y), col = "darkred", size = 1) +
    jaspGraphs::geom_point() +
    ggplot2::scale_x_continuous(name = gettext("Observed Test Values"), breaks = breaks, limits = range(breaks)) +
    ggplot2::scale_y_continuous(name = gettext("Predicted Test Values"), breaks = breaks, limits = range(breaks)) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()
  plot$plotObject <- p
}

.mlPlotDataSplit <- function(dataset, options, jaspResults, ready, position, purpose, type) {
  if (!is.null(jaspResults[["plotDataSplit"]]) || !options[["dataSplitPlot"]]) {
    return()
  }
  plot <- createJaspPlot(plot = NULL, title = gettext("Data Split"), width = 800, height = 30)
  plot$position <- position
  plot$dependOn(options = c("dataSplitPlot", "target", "predictors", "trainingDataManual", "modelValid", "testSetIndicatorVariable", "testSetIndicator", "validationDataManual", "holdoutData", "testDataManual", "modelOptimization"))
  jaspResults[["plotDataSplit"]] <- plot
  if (!ready) {
    return()
  }
  result <- switch(purpose,
    "classification" = jaspResults[["classificationResult"]]$object,
    "regression" = jaspResults[["regressionResult"]]$object
  )
  if (options[["modelOptimization"]] == "manual") {
    # For a fixed model, draw only a training and a test set
    nTrain <- result[["ntrain"]]
    nTest <- result[["ntest"]]
    plotData <- data.frame(y = c(nTrain, nTest), x = c("Train", "Test"), group = c(1, 1))
    p <- ggplot2::ggplot(data = plotData, mapping = ggplot2::aes(x = group, y = y, fill = factor(x, levels = c("Test", "Train")))) +
      ggplot2::geom_bar(stat = "identity", col = "black", size = 0.5) +
      ggplot2::scale_y_continuous(name = NULL, limits = c(0, nTrain + nTest + ((nTrain + nTest) / 5))) + # adjust limits to include "Total" text
      ggplot2::coord_flip() +
      ggplot2::labs(x = NULL) +
      ggplot2::scale_fill_manual(name = NULL, values = c("tomato2", "steelblue2")) +
      ggplot2::annotate("text", y = c(0, nTrain, nTrain + nTest), x = 1, label = c(gettextf("Train: %d", nTrain), gettextf("Test: %d", nTest), gettextf("Total: %d", nTrain + nTest)), size = 4, vjust = 0.5, hjust = -0.1) +
      jaspGraphs::geom_rangeframe(sides = "") +
      jaspGraphs::themeJaspRaw() +
      ggplot2::theme(axis.ticks = ggplot2::element_blank(), axis.text.y = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank())
  } else {
    # For an optimized model, draw a training, a validation, and a test set
    if (options[["modelValid"]] == "validationManual" || type == "randomForest" || type == "regularized" || type == "lda") {
      nTrain <- result[["ntrain"]]
      nValid <- result[["nvalid"]]
      nTest <- result[["ntest"]]
      plotData <- data.frame(y = c(nTrain, nValid, nTest), x = c("Train", "Validation", "Test"), group = c(1, 1, 1))
      p <- ggplot2::ggplot(data = plotData, mapping = ggplot2::aes(x = group, y = y, fill = factor(x, levels = c("Test", "Validation", "Train")))) +
        ggplot2::geom_bar(stat = "identity", col = "black", size = 0.5) +
        ggplot2::scale_y_continuous(name = NULL, limits = c(0, nTrain + nValid + nTest + ((nTrain + nValid + nTest) / 5))) + # adjust limits to include "Total" text
        ggplot2::coord_flip() +
        ggplot2::labs(x = NULL) +
        ggplot2::scale_fill_manual(name = NULL, values = c("tomato2", "darkgoldenrod2", "steelblue2")) +
        ggplot2::annotate("text",
          y = c(0, nTrain, nTrain + nValid, nTrain + nValid + nTest), x = 1,
          label = c(gettextf("Train: %d", nTrain), gettextf("Validation: %d", nValid), gettextf("Test: %d", nTest), gettextf("Total: %d", nTrain + nValid + nTest)),
          size = 4, vjust = 0.5, hjust = -0.1
        ) +
        jaspGraphs::geom_rangeframe(sides = "") +
        jaspGraphs::themeJaspRaw() +
        ggplot2::theme(
          axis.ticks = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_blank()
        )
    } else {
      nTrainAndValid <- result[["nvalid"]]
      nTest <- result[["ntest"]]
      plotData <- data.frame(y = c(nTrainAndValid, nTest), x = c("Train and validation", "Test"), group = c(1, 1))
      p <- ggplot2::ggplot(data = plotData, mapping = ggplot2::aes(x = group, y = y, fill = factor(x, levels = c("Test", "Train and validation")))) +
        ggplot2::geom_bar(stat = "identity", col = "black", size = 0.5) +
        ggplot2::scale_y_continuous(name = NULL, limits = c(0, nTrainAndValid + nTest + ((nTrainAndValid + nTest) / 5))) + # adjust limits to include "Total" text
        ggplot2::coord_flip() +
        ggplot2::xlab(NULL) +
        ggplot2::scale_fill_manual(name = NULL, values = c("tomato2", "seagreen2")) +
        ggplot2::annotate("text",
          y = c(0, nTrainAndValid, nTrainAndValid + nTest), x = 1,
          label = c(gettextf("Train and validation: %d", nTrainAndValid), gettextf("Test: %d", nTest), gettextf("Total: %d", nTrainAndValid + nTest)),
          size = 4, vjust = 0.5, hjust = -0.1
        ) +
        jaspGraphs::geom_rangeframe(sides = "") +
        jaspGraphs::themeJaspRaw() +
        ggplot2::theme(axis.ticks = ggplot2::element_blank(), axis.text.y = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank())
    }
  }
  plot$plotObject <- p
}

.mlAddTestIndicatorToData <- function(options, jaspResults, ready, purpose) {
  if (!ready || !options[["addIndicator"]] || options[["holdoutData"]] != "holdoutManual" || options[["testIndicatorColumn"]] == "") {
    return()
  }
  result <- switch(purpose,
    "classification" = jaspResults[["classificationResult"]]$object,
    "regression" = jaspResults[["regressionResult"]]$object
  )
  if (is.null(jaspResults[["testIndicatorColumn"]])) {
    testIndicatorColumn <- result[["testIndicatorColumn"]]
    jaspResults[["testIndicatorColumn"]] <- createJaspColumn(columnName = options[["testIndicatorColumn"]])
    jaspResults[["testIndicatorColumn"]]$dependOn(options = c(.mlRegressionDependencies(options), "testIndicatorColumn", "addIndicator"))
    jaspResults[["testIndicatorColumn"]]$setNominal(testIndicatorColumn)
  }
}

# these could also extend the S3 method scale although that could be somewhat unexpected
.scaleNumericData <- function(x, ...) {
  UseMethod(".scaleNumericData", x)
}

.scaleNumericData.data.frame <- function(x, center = TRUE, scale = TRUE) {
  if (nrow(x) == 0) {
    return(x)
  }
  idx <- sapply(x, function(x) is.numeric(x) && length(unique(x)) > 1)
  x[, idx] <- scale(x[, idx, drop = FALSE], center, scale)
  attr(x, which = "scaled:center") <- NULL
  attr(x, which = "scaled:scale") <- NULL
  return(x)
}

.scaleNumericData.matrix <- function(x, center = TRUE, scale = TRUE) {
  if (!is.numeric(x)) {
    warning(sprintf("Object passed to .scaleNumericData.matrix was not numeric!"))
    return(x)
  }
  x <- scale(x, center, scale)
  attr(x, which = "scaled:center") <- NULL
  attr(x, which = "scaled:scale") <- NULL
  return(x)
}

.scaleNumericData.numeric <- function(x, center = TRUE, scale = TRUE) {
  if (center) {
    x <- x - mean(x)
  }
  if (scale && length(unique(x)) > 1) {
    x <- x / sd(x)
  }
  return(x)
}

# fallback when .scaleNumericData is called with factor/ character data
.scaleNumericData.default <- function(x, center = TRUE, scale = TRUE) {
  return(x)
}

.mlRegressionAddPredictionsToData <- function(dataset, options, jaspResults, ready) {
  if (!ready || !options[["addPredictions"]] || options[["predictionsColumn"]] == "") {
    return()
  }
  regressionResult <- jaspResults[["regressionResult"]]$object
  if (is.null(jaspResults[["predictionsColumn"]])) {
    predictions <- regressionResult[["values"]]
    predictionsColumn <- rep(NA, max(as.numeric(rownames(dataset))))
    predictionsColumn[as.numeric(rownames(dataset))] <- predictions
    jaspResults[["predictionsColumn"]] <- createJaspColumn(columnName = options[["predictionsColumn"]])
    jaspResults[["predictionsColumn"]]$dependOn(options = c(.mlRegressionDependencies(options), "predictionsColumn", "addPredictions"))
    jaspResults[["predictionsColumn"]]$setScale(predictionsColumn)
  }
}

.mlShapAnalysis <- function(options, fit, trainingSet, testSet, type) {
  predict_model.gbm <- gbm:::predict.gbm
  predict_model.randomForest <- randomForest:::predict.randomForest
  class(fit) <- type
  explainer <- shapr::shapr(trainingSet[, options[["predictors"]]], fit)
  p0 <- mean(trainingSet[, options[["target"]]])
  explanation <- shapr::explain(x = testSet, explainer, approach = "empirical", prediction_zero = p0)
  out <- cbind(explanation[["p"]], explanation[["dt"]])
  colnames(out) <- c("pred", "avg", options[["predictors"]])
  return(out)
}

.mlRegressionTableShap <- function(dataset, options, jaspResults, ready, position) {
  if (!is.null(jaspResults[["shapTable"]]) || !options[["shapTable"]]) {
    return()
  }
  table <- createJaspTable(title = "Feature Contribution to Predictions in the Test Set")
  table$position <- position
  table$dependOn(options = c(.mlRegressionDependencies(options), "shapTable", "shapFrom", "shapTo"))
  table$addColumnInfo(name = "id", title = "#", type = "integer")
  table$addColumnInfo(name = "pred", title = gettext("Predicted"), type = "number")
  for (i in options[["predictors"]]) {
    table$addColumnInfo(name = i, title = i, type = "number")
  }
  jaspResults[["shapTable"]] <- table
  if (!ready) {
    return()
  }
  regressionResult <- jaspResults[["regressionResult"]]$object
  out <- regressionResult[["shap"]]
  table$addFootnote(gettextf("The numbers for the features represent their contribution to the predicted value without features (%1$s).", round(unique(out[, 2]), 3)))
  out <- out[order(-out[["pred"]]), ]
  out <- cbind(id = seq_len(nrow(out)), out[, -2])
  from <- min(c(options[["shapFrom"]], options[["shapTo"]] - 1, nrow(out)))
  to <- min(c(options[["shapTo"]], nrow(out)))
  out <- out[from:to, ]
  table$setData(out)
}
