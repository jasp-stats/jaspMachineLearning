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

mlRegressionKnn <- function(jaspResults, dataset, options, state = NULL) {

  # Preparatory work
  dataset <- .readDataRegressionAnalyses(dataset, options)
  .mlRegressionErrorHandling(dataset, options, type = "knn")

  # Check if analysis is ready to run
  ready <- .mlRegressionReady(options, type = "knn")

  # Compute results and create the model summary table
  .mlRegressionTableSummary(dataset, options, jaspResults, ready, position = 1, type = "knn")

  # If the user wants to add the values to the data set
  .mlRegressionAddPredictionsToData(dataset, options, jaspResults, ready)

  # Add test set indicator to data
  .mlAddTestIndicatorToData(options, jaspResults, ready, purpose = "regression")

  # Create the data split plot
  .mlPlotDataSplit(dataset, options, jaspResults, ready, position = 2, purpose = "regression", type = "knn")

  # Create the weights plot
  .mlKnnPlotWeights(options, jaspResults, position = 3)

  # Create the evaluation metrics table
  .mlRegressionTableMetrics(dataset, options, jaspResults, ready, position = 4)

  # Create the shap table
  .mlRegressionTableShap(dataset, options, jaspResults, ready, position = 5, type = "knn")

  # Create the mean squared error plot
  .mlKnnPlotError(dataset, options, jaspResults, ready, position = 6, purpose = "regression")

  # Create the predicted performance plot
  .mlRegressionPlotPredictedPerformance(options, jaspResults, ready, position = 7)
}

.knnRegression <- function(dataset, options, jaspResults, ready) {
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
  if (options[["modelOptimization"]] == "manual") {
    # Just create a train and a test set (no optimization)
    trainingSet <- trainingAndValidationSet
    testSet <- dataset[-trainingIndex, ]
    testFit <- kknn::kknn(
      formula = formula, train = trainingSet, test = testSet, k = options[["noOfNearestNeighbours"]],
      distance = distance, kernel = weights, scale = FALSE
    )
    nn <- options[["noOfNearestNeighbours"]]
  } else if (options[["modelOptimization"]] == "optimized") {
    # Create a train, validation and test set (optimization)
    validationIndex <- sample.int(nrow(trainingAndValidationSet), size = ceiling(options[["validationDataManual"]] * nrow(trainingAndValidationSet)))
    testSet <- dataset[-trainingIndex, ]
    validationSet <- trainingAndValidationSet[validationIndex, ]
    trainingSet <- trainingAndValidationSet[-validationIndex, ]
    if (options[["modelValid"]] == "validationManual") {
      nnRange <- 1:options[["maxNearestNeighbors"]]
      errorStore <- numeric(length(nnRange))
      trainErrorStore <- numeric(length(nnRange))
      startProgressbar(length(nnRange))
      for (i in nnRange) {
        validationFit <- kknn::kknn(
          formula = formula, train = trainingSet, test = validationSet, k = i,
          distance = distance, kernel = weights, scale = FALSE
        )
        errorStore[i] <- mean((validationFit$fitted.values - validationSet[, options[["target"]]])^2)
        trainingFit <- kknn::kknn(
          formula = formula, train = trainingSet, test = trainingSet, k = i,
          distance = distance, kernel = weights, scale = FALSE
        )
        trainErrorStore[i] <- mean((trainingFit$fitted.values - trainingSet[, options[["target"]]])^2)
        progressbarTick()
      }
      nn <- switch(options[["modelOptimization"]],
        "optimized" = nnRange[which.min(errorStore)]
      )
      testFit <- kknn::kknn(
        formula = formula, train = trainingSet, test = testSet, k = nn,
        distance = distance, kernel = weights, scale = FALSE
      )
    } else if (options[["modelValid"]] == "validationKFold") {
      nnRange <- 1:options[["maxNearestNeighbors"]]
      errorStore <- numeric(length(nnRange))
      startProgressbar(length(nnRange))
      for (i in nnRange) {
        validationFit <- kknn::cv.kknn(
          formula = formula, data = trainingAndValidationSet, distance = distance, kernel = weights,
          kcv = options[["noOfFolds"]], k = i
        )
        errorStore[i] <- mean((validationFit[[1]][, 1] - validationFit[[1]][, 2])^2)
        progressbarTick()
      }
      nn <- switch(options[["modelOptimization"]],
        "optimized" = nnRange[which.min(errorStore)]
      )
      validationFit <- kknn::cv.kknn(
        formula = formula, data = trainingAndValidationSet, distance = distance, kernel = weights,
        kcv = options[["noOfFolds"]], k = nn
      )
      validationFit <- list(fitted.values = as.numeric(validationFit[[1]][, 2]))
      testFit <- kknn::kknn(formula = formula, train = trainingAndValidationSet, test = testSet, k = nn, distance = distance, kernel = weights, scale = FALSE)
      trainingSet <- trainingAndValidationSet
      validationSet <- trainingAndValidationSet
    } else if (options[["modelValid"]] == "validationLeaveOneOut") {
      nnRange <- 1:options[["maxNearestNeighbors"]]
      validationFit <- kknn::train.kknn(formula = formula, data = trainingAndValidationSet, ks = nnRange, scale = FALSE, distance = distance, kernel = weights)
      errorStore <- as.numeric(validationFit$MEAN.SQU)
      nn <- switch(options[["modelOptimization"]],
        "optimized" = nnRange[which.min(errorStore)]
      )
      validationFit <- list(fitted.values = validationFit[["fitted.values"]][[1]])
      testFit <- kknn::kknn(formula = formula, train = trainingAndValidationSet, test = testSet, k = nn, distance = distance, kernel = weights, scale = FALSE)
      trainingSet <- trainingAndValidationSet
      validationSet <- trainingAndValidationSet
    }
  }
  # Use the specified model to make predictions for dataset
  dataPredictions <- predict(kknn::kknn(formula = formula, train = trainingSet, test = dataset, k = nn, distance = distance, kernel = weights, scale = FALSE))
  # Create results object
  result <- list()
  result[["formula"]] <- formula
  result[["model"]] <- testFit
  result[["model"]]$predictive <- kknn::train.kknn(formula = formula, data = trainingSet, ks = nn, distance = options[["distanceParameterManual"]], kernel = options[["weights"]])
  result[["nn"]] <- nn
  result[["weights"]] <- weights
  result[["distance"]] <- distance
  result[["testMSE"]] <- mean((testFit$fitted.values - testSet[, options[["target"]]])^2)
  result[["ntrain"]] <- nrow(trainingSet)
  result[["ntest"]] <- nrow(testSet)
  result[["train"]] <- trainingSet
  result[["test"]] <- testSet
  result[["testReal"]] <- testSet[, options[["target"]]]
  result[["testPred"]] <- testFit$fitted.values
  result[["testIndicatorColumn"]] <- testIndicatorColumn
  result[["values"]] <- dataPredictions
  if (options[["modelOptimization"]] != "manual") {
    result[["accuracyStore"]] <- errorStore
    result[["validMSE"]] <- mean((validationFit$fitted.values - validationSet[, options[["target"]]])^2)
    result[["nvalid"]] <- nrow(validationSet)
    result[["valid"]] <- validationSet
    if (options[["modelValid"]] == "validationManual") {
      result[["trainAccuracyStore"]] <- trainErrorStore
    }
  }
  return(result)
}

.mlKnnPlotError <- function(dataset, options, jaspResults, ready, position, purpose) {
  if (!is.null(jaspResults[["errorVsKPlot"]]) || !options[["errorVsKPlot"]] || options[["modelOptimization"]] == "manual") {
    return()
  }
  plotTitle <- switch(purpose,
    "classification" = gettext("Classification Accuracy Plot"),
    "regression" = gettext("Mean Squared Error Plot")
  )
  plot <- createJaspPlot(plot = NULL, title = plotTitle, width = 400, height = 300)
  plot$position <- position
  plot$dependOn(options = c(
    "errorVsKPlot", "noOfNearestNeighbours", "trainingDataManual", "distanceParameterManual", "weights", "scaleVariables", "modelOptimization",
    "target", "predictors", "seed", "setSeed", "modelValid", "maxNearestNeighbors", "noOfFolds", "modelValid",
    "testSetIndicatorVariable", "testSetIndicator", "validationDataManual", "holdoutData", "testDataManual"
  ))
  jaspResults[["errorVsKPlot"]] <- plot
  if (!ready) {
    return()
  }
  result <- switch(purpose,
    "classification" = jaspResults[["classificationResult"]]$object,
    "regression" = jaspResults[["regressionResult"]]$object
  )
  ylabel <- switch(purpose,
    "classification" = gettext("Classification Accuracy"),
    "regression"     = gettext("Mean Squared Error")
  )
  if (options[["modelValid"]] == "validationManual") {
    xvalues <- rep(1:options[["maxNearestNeighbors"]], 2)
    yvalues1 <- result[["accuracyStore"]]
    yvalues2 <- result[["trainAccuracyStore"]]
    yvalues <- c(yvalues1, yvalues2)
    type <- rep(c(gettext("Validation set"), gettext("Training set")), each = length(yvalues1))
    plotData <- data.frame(x = xvalues, y = yvalues, type = type)
    xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(0, plotData$x), min.n = 4)
    yBreaks <- jaspGraphs::getPrettyAxisBreaks(plotData$y, min.n = 4)
    pointData <- data.frame(
      x = result[["nn"]],
      y = yvalues1[result[["nn"]]]
    )
    p <- ggplot2::ggplot(data = plotData, ggplot2::aes(x = x, y = y)) +
      jaspGraphs::geom_line(mapping = ggplot2::aes(linetype = type)) +
      ggplot2::scale_x_continuous(name = gettext("Number of Nearest Neighbors"), breaks = xBreaks, limits = c(0, max(xBreaks))) +
      ggplot2::scale_y_continuous(name = ylabel, breaks = yBreaks, limits = range(yBreaks)) +
      ggplot2::labs(linetype = NULL) +
      ggplot2::scale_linetype_manual(values = c(2, 1)) +
      jaspGraphs::geom_point(data = pointData, ggplot2::aes(x = x, y = y), fill = "red", inherit.aes = FALSE) +
      jaspGraphs::geom_rangeframe() +
      jaspGraphs::themeJaspRaw(legend.position = "top")
  } else if (options[["modelValid"]] != "validationManual") {
    xvalues <- 1:options[["maxNearestNeighbors"]]
    yvalues <- result[["accuracyStore"]]
    type <- rep(gettext("Training and validation set"), each = length(xvalues))
    plotData <- data.frame(x = xvalues, y = yvalues, type = type)
    xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(0, plotData$x), min.n = 4)
    yBreaks <- jaspGraphs::getPrettyAxisBreaks(plotData$y, min.n = 4)
    p <- ggplot2::ggplot(data = plotData, ggplot2::aes(x = x, y = y, linetype = type)) +
      jaspGraphs::geom_line() +
      ggplot2::scale_x_continuous(name = gettext("Number of Nearest Neighbors"), breaks = xBreaks, limits = c(0, max(xBreaks))) +
      ggplot2::scale_y_continuous(name = ylabel, breaks = yBreaks, limits = range(yBreaks)) +
      jaspGraphs::geom_point(ggplot2::aes(x = x, y = y, linetype = type), data = data.frame(x = result[["nn"]], y = yvalues[result[["nn"]]], type = gettext("Training and validation set")), fill = "red") +
      ggplot2::labs(linetype = NULL) +
      jaspGraphs::geom_rangeframe() +
      jaspGraphs::themeJaspRaw(legend.position = "top")
  }
  plot$plotObject <- p
}

.mlKnnPlotWeights <- function(options, jaspResults, position) {
  if (!is.null(jaspResults[["weightsPlot"]]) || !options[["weightsPlot"]]) {
    return()
  }
  weights <- switch(options[["weights"]],
    "rectangular"  = gettext("Rectangular"),
    "triangular"   = gettext("Triangular"),
    "epanechnikov" = gettext("Epanechnikov"),
    "biweight"     = gettext("Biweight"),
    "triweight"    = gettext("Triweight"),
    "cos"          = gettext("Cosine"),
    "inv"          = gettext("Inverse"),
    "gaussian"     = gettext("Gaussian"),
    "rank"         = gettext("Rank"),
    "optimal"      = gettext("Optimal")
  )
  plot <- createJaspPlot(title = gettextf("%1$s Weight Function", weights), width = 400, height = 300)
  plot$position <- position
  plot$dependOn(options = c("weightsPlot", "weights"))
  jaspResults[["weightsPlot"]] <- plot
  if (options[["weights"]] == "rank" || options[["weights"]] == "optimal") {
    plot$setError(gettext("Plotting not possible: The selected weighting scheme cannot be visualized separately from the data."))
    return()
  }
  # Weighting schemes from the kknn::kknn() function
  func <- switch(options[["weights"]],
    "rectangular"  = function(x) 1,
    "triangular"   = function(x) 1 - x,
    "epanechnikov" = function(x) 0.75 * (1 - x^2),
    "biweight"     = function(x) stats::dbeta((x + 1) / 2, shape1 = 3, shape2 = 3),
    "triweight"    = function(x) stats::dbeta((x + 1) / 2, shape1 = 4, shape2 = 4),
    "cos"          = function(x) cos(x * pi / 2),
    "inv"          = function(x) 1 / x,
    "gaussian"     = function(x) stats::dnorm(x)
  )
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(0, 1), min.n = 4)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(0, 1), min.n = 4) # 0.001 for Inf at x = 0 in 'inv' weights
  plotFunc <- function(x) func(x) / func(0.001)
  p <- ggplot2::ggplot() +
    ggplot2::stat_function(fun = plotFunc, size = 1, xlim = c(0.001, 1)) +
    ggplot2::scale_x_continuous(name = gettext("Proportion of Max. Distance"), breaks = xBreaks, limits = c(0, 1)) +
    ggplot2::scale_y_continuous(name = gettext("Relative Weight"), breaks = yBreaks, limits = c(0, 1)) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()
  plot$plotObject <- p
}

# kknn::kknn calls stats::model.matrix which needs these two functions and looks for them by name in the global namespace
contr.dummy <- kknn::contr.dummy
contr.ordinal <- kknn::contr.ordinal
