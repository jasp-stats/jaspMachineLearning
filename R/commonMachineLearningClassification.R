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

# This is a temporary fix
# TODO: remove it when R will solve this problem!
gettextf <- function(fmt, ..., domain = NULL) {
  return(sprintf(gettext(fmt, domain = domain), ...))
}

# This function should return all options for all analyses upon which a change in all tables/figures is required
.mlClassificationDependencies <- function(options, includeSaveOptions = FALSE) {
  opt <- c(
    "target", "predictors", "seed", "setSeed",                                            # Common
    "trainingDataManual", "scaleVariables", "modelOptimization",                          # Common
    "testSetIndicatorVariable", "testSetIndicator", "holdoutData", "testDataManual",      # Common
    "modelValid", "validationDataManual", "validationLeaveOneOut", "noOfFolds",           # Common
	"shrinkage", "interactionDepth", "minObservationsInNode",                             # Boosting
    "minObservationsForSplit",                                                            # Decision tree
    "distanceParameterManual", "noOfNearestNeighbours", "weights", "maxNearestNeighbors", # k-Nearest neighbors
    "estimationMethod",                                                                   # Linear discriminant analysis
    "threshold", "algorithm", "learningRate", "lossFunction", "actfct", "layers",         # Neural network
    "maxTrainingRepetitions", "maxGenerations", "populationSize", "maxLayers",            # Neural network
    "maxNodes", "mutationRate", "elitism", "selectionMethod", "crossoverMethod",          # Neural network
    "mutationMethod", "survivalMethod", "elitismProportion", "candidates",                # Neural network
    "noOfTrees", "maxTrees", "baggingFraction", "noOfPredictors", "numberOfPredictors",   # Random forest
    "complexityParameter", "degree", "gamma", "cost", "tolerance", "epsilon"              # Support vector machine
  )
  if (includeSaveOptions) {
    opt <- c(opt, "saveModel", "savePath")
  }
  return(opt)
}

.mlClassificationReadData <- function(dataset, options) {
  if (is.null(dataset)) {
    dataset <- .readDataClassificationRegressionAnalyses(dataset, options)
  }
  if (length(unlist(options[["predictors"]])) > 0 && options[["scaleVariables"]]) {
    dataset[, options[["predictors"]]] <- .scaleNumericData(dataset[, options[["predictors"]], drop = FALSE])
  }
  if (options[["target"]] != "") {
    dataset[, options[["target"]]] <- factor(dataset[, options[["target"]]])
  }
  return(dataset)
}

.mlClassificationErrorHandling <- function(dataset, options, type) {
  .errorHandlingClassificationRegressionAnalyses(dataset, options, type)
  # add checks specifically for classification analyses here
}

.mlClassificationReady <- function(options, type) {
  if (type == "lda" || type == "randomForest" || type == "boosting") {
    ready <- length(options[["predictors"]][options[["predictors"]] != ""]) >= 2 && options[["target"]] != ""
  } else if (type == "knn" || type == "neuralnet" || type == "rpart" || type == "svm") {
    ready <- length(options[["predictors"]][options[["predictors"]] != ""]) >= 1 && options[["target"]] != ""
  }
  return(ready)
}

.mlClassificationSetFormula <- function(options, jaspResults) {
  predictors <- options[["predictors"]]
  target <- options[["target"]]
  formula <- formula(paste(target, "~", paste(predictors, collapse = " + ")))
  jaspResults[["formula"]] <- createJaspState(formula)
  jaspResults[["formula"]]$dependOn(options = c("predictors", "target"))
}

.mlClassificationComputeResults <- function(dataset, options, jaspResults, ready, type) {
  if (!is.null(jaspResults[["classificationResult"]])) {
    return()
  }
  .setSeedJASP(options) # Set the seed to make results reproducible
  if (ready) {
    .mlClassificationSetFormula(options, jaspResults)
    classificationResult <- switch(type,
      "knn" = .knnClassification(dataset, options, jaspResults),
      "lda" = .ldaClassification(dataset, options, jaspResults),
      "randomForest" = .randomForestClassification(dataset, options, jaspResults),
      "boosting" = .boostingClassification(dataset, options, jaspResults),
      "neuralnet" = .neuralnetClassification(dataset, options, jaspResults),
      "rpart" = .decisionTreeClassification(dataset, options, jaspResults),
      "svm" = .svmClassification(dataset, options, jaspResults)
    )
    jaspResults[["classificationResult"]] <- createJaspState(classificationResult)
    jaspResults[["classificationResult"]]$dependOn(options = .mlClassificationDependencies(options))
  }
}

.mlClassificationTableSummary <- function(dataset, options, jaspResults, ready, position, type) {
  if (!is.null(jaspResults[["classificationTable"]])) {
    return()
  }
  title <- switch(type,
    "knn" = gettext("K-Nearest Neighbors Classification"),
    "lda" = gettext("Linear Discriminant Classification"),
    "randomForest" = gettext("Random Forest Classification"),
    "boosting" = gettext("Boosting Classification"),
    "neuralnet" = gettext("Neural Network Classification"),
    "rpart" = gettext("Decision Tree Classification"),
    "svm" = gettext("Support Vector Machine Classification")
  )
  table <- createJaspTable(title)
  table$position <- position
  table$dependOn(options = .mlClassificationDependencies(options, includeSaveOptions = TRUE))
  # Add analysis-specific columns
  if (type == "knn") {
    table$addColumnInfo(name = "nn", title = gettext("Nearest neighbors"), type = "integer")
    table$addColumnInfo(name = "weights", title = gettext("Weights"), type = "string")
    table$addColumnInfo(name = "distance", title = gettext("Distance"), type = "string")
  } else if (type == "lda") {
    table$addColumnInfo(name = "lda", title = gettext("Linear Discriminants"), type = "integer")
    table$addColumnInfo(name = "method", title = gettext("Method"), type = "string")
  } else if (type == "randomForest") {
    table$addColumnInfo(name = "trees", title = gettext("Trees"), type = "integer")
    table$addColumnInfo(name = "preds", title = gettext("Features per split"), type = "integer")
  } else if (type == "boosting") {
    table$addColumnInfo(name = "trees", title = gettext("Trees"), type = "integer")
    table$addColumnInfo(name = "shrinkage", title = gettext("Shrinkage"), type = "number")
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
    table$addColumnInfo(name = "validAcc", title = gettext("Validation Accuracy"), type = "number")
  }
  table$addColumnInfo(name = "testAcc", title = gettext("Test Accuracy"), type = "number")
  # Add analysis-specific columns after common columns
  if (type == "randomForest") {
    table$addColumnInfo(name = "oob", title = gettext("OOB Accuracy"), type = "number")
  }
  # If no analysis is run, specify the required variables in a footnote
  if (!ready) {
    table$addFootnote(gettextf("Please provide a target variable and at least %i feature variable(s).", if (type == "knn" || type == "neuralnet" || type == "rpart" || type == "svm") 1L else 2L))
  }
  if (options[["savePath"]] != "") {
    validNames <- (length(grep(" ", decodeColNames(colnames(dataset)))) == 0) && (length(grep("_", decodeColNames(colnames(dataset)))) == 0)
    if (options[["saveModel"]] && validNames) {
      table$addFootnote(gettextf("The trained model is saved as <i>%1$s</i>.", basename(options[["savePath"]])))
    } else if (options[["saveModel"]] && !validNames) {
      table$addFootnote(gettext("The trained model is <b>not</b> saved because the some of the variable names in the model contain spaces (i.e., ' ') or underscores (i.e., '_'). Please remove all such characters from the variable names and try saving the model again."))
    } else {
      table$addFootnote(gettext("The trained model is not saved until 'Save trained model' is checked."))
    }
  }
  jaspResults[["classificationTable"]] <- table
  if (!ready) {
    return()
  }
  # Perform the actual analysis
  .mlClassificationComputeResults(dataset, options, jaspResults, ready, type = type)
  classificationResult <- jaspResults[["classificationResult"]]$object
  nTrain <- classificationResult[["ntrain"]]
  if (options[["modelOptimization"]] != "manual") {
    nValid <- classificationResult[["nvalid"]]
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
      table$addFootnote(gettext("The model is optimized with respect to the <i>validation set accuracy</i>."))
    }
    if (classificationResult[["nn"]] == options[["maxNearestNeighbors"]] && options[["modelOptimization"]] != "optimized") {
      table$addFootnote(gettext("The optimum number of nearest neighbors is the maximum number. You might want to adjust the range of optimization."))
    }
    distance <- if (classificationResult[["distance"]] == 1) gettext("Manhattan") else gettext("Euclidean")
    row <- data.frame(
      nn = classificationResult[["nn"]],
      weights = classificationResult[["weights"]],
      distance = distance,
      nTrain = nTrain,
      nTest = classificationResult[["ntest"]],
      testAcc = classificationResult[["testAcc"]]
    )
    if (options[["modelOptimization"]] != "manual") {
      row <- cbind(row, nValid = nValid, validAcc = classificationResult[["validAcc"]])
    }
    table$addRows(row)
  } else if (type == "lda") {
    method <- switch(options[["estimationMethod"]],
      "moment" = gettext("Moment"),
      "mle"    = gettext("MLE"),
      "covMve" = gettext("MVE"),
      "t"      = gettext("t")
    )
    row <- data.frame(
      lda = ncol(classificationResult[["scaling"]]),
      method = method,
      nTrain = nTrain,
      nTest = classificationResult[["ntest"]],
      testAcc = classificationResult[["testAcc"]]
    )
    table$addRows(row)
  } else if (type == "randomForest") {
    if (options[["modelOptimization"]] == "optimized") {
      table$addFootnote(gettext("The model is optimized with respect to the <i>out-of-bag accuracy</i>."))
    }
    row <- data.frame(
      trees = classificationResult[["noOfTrees"]],
      preds = classificationResult[["predPerSplit"]],
      nTrain = nTrain,
      nTest = classificationResult[["ntest"]],
      testAcc = classificationResult[["testAcc"]],
      oob = classificationResult[["oobAccuracy"]]
    )
    if (options[["modelOptimization"]] != "manual") {
      row <- cbind(row, nValid = nValid, validAcc = classificationResult[["validAcc"]])
    }
    table$addRows(row)
  } else if (type == "boosting") {
    if (options[["modelOptimization"]] == "optimized") {
      table$addFootnote(gettext("The model is optimized with respect to the <i>out-of-bag accuracy</i>."))
    }
    row <- data.frame(
      trees = classificationResult[["noOfTrees"]],
      shrinkage = options[["shrinkage"]],
      nTrain = nTrain,
      nTest = classificationResult[["ntest"]],
      testAcc = classificationResult[["testAcc"]]
    )
    if (options[["modelOptimization"]] != "manual") {
      row <- cbind(row, nValid = nValid, validAcc = classificationResult[["validAcc"]])
    }
    table$addRows(row)
  } else if (type == "neuralnet") {
    if (options[["modelOptimization"]] == "manual") {
      table$addFootnote(gettext("The model is optimized with respect to the <i>sum of squares</i>."))
    } else if (options[["modelOptimization"]] == "optimized") {
      table$addFootnote(gettext("The model is optimized with respect to the <i>validation set accuracy</i>."))
    }
    row <- data.frame(
      layers = classificationResult[["nLayers"]],
      nodes = classificationResult[["nNodes"]],
      nTrain = nTrain,
      nTest = classificationResult[["ntest"]],
      testAcc = classificationResult[["testAcc"]]
    )
    if (options[["modelOptimization"]] != "manual") {
      row <- cbind(row, nValid = nValid, validAcc = classificationResult[["validAcc"]])
    }
    table$addRows(row)
  } else if (type == "rpart") {
    splits <- if (!is.null(classificationResult[["model"]]$splits)) nrow(classificationResult[["model"]]$splits) else 0
    row <- data.frame(
      splits = splits,
      nTrain = nTrain,
      nTest = classificationResult[["ntest"]],
      testAcc = classificationResult[["testAcc"]]
    )
    table$addRows(row)
  } else if (type == "svm") {
    row <- data.frame(
      vectors = nrow(classificationResult[["model"]]$SV),
      nTrain = nTrain,
      nTest = classificationResult[["ntest"]],
      testAcc = classificationResult[["testAcc"]]
    )
    table$addRows(row)
  }
  # Save the applied model if requested
  if (options[["saveModel"]] && options[["savePath"]] != "") {
    validNames <- (length(grep(" ", decodeColNames(colnames(dataset)))) == 0) && (length(grep("_", decodeColNames(colnames(dataset)))) == 0)
    if (!validNames) {
      return()
    }
    model <- classificationResult[["model"]]
    model[["jaspVars"]] <- decodeColNames(options[["predictors"]])
    model[["jaspVersion"]] <- .baseCitation
    model <- .decodeJaspMLobject(model)
    class(model) <- c(class(classificationResult[["model"]]), "jaspClassification", "jaspMachineLearning")
    saveRDS(model, file = options[["savePath"]])
  }
}

.mlClassificationTableConfusion <- function(dataset, options, jaspResults, ready, position) {
  if (!is.null(jaspResults[["confusionTable"]]) || !options[["confusionTable"]]) {
    return()
  }
  table <- createJaspTable(title = gettext("Confusion Matrix"))
  table$position <- position
  table$dependOn(options = c(.mlClassificationDependencies(options), "confusionTable", "confusionProportions"))
  jaspResults[["confusionTable"]] <- table
  if (ready) {
    classificationResult <- jaspResults[["classificationResult"]]$object
    table$addColumnInfo(name = "obs_name", title = "", type = "string")
    table$addColumnInfo(name = "varname_obs", title = "", type = "string")
    confTable <- classificationResult[["confTable"]]
    if (options[["confusionProportions"]]) {
      confTable <- round(confTable / classificationResult[["ntest"]], 2)
    }
    table[["obs_name"]] <- c(gettext("Observed"), rep("", nrow(confTable) - 1))
    table[["varname_obs"]] <- colnames(confTable)
    for (i in seq_along(colnames(confTable))) {
      name <- paste("varname_pred", i, sep = "")
      table$addColumnInfo(name = name, title = colnames(confTable)[i], type = "integer", overtitle = gettext("Predicted"))
      if (colnames(confTable)[i] %in% rownames(confTable)) {
        table[[name]] <- confTable[which(rownames(confTable) == colnames(confTable)[i]), ]
      } else {
        table[[name]] <- rep(0, length(colnames(confTable)))
      }
    }
  } else if (options[["target"]] != "" && !ready) {
    table$addColumnInfo(name = "obs_name", title = "", type = "string")
    table$addColumnInfo(name = "varname_obs", title = "", type = "string")
    factorLevels <- levels(dataset[, options[["target"]]])
    table[["obs_name"]] <- c(gettext("Observed"), rep("", length(factorLevels) - 1))
    table[["varname_obs"]] <- factorLevels
    for (i in seq_along(factorLevels)) {
      name <- paste("varname_pred", i, sep = "")
      table$addColumnInfo(name = name, title = factorLevels[i], type = "integer", overtitle = gettext("Predicted"))
      table[[name]] <- rep(".", length(factorLevels))
    }
  } else {
    table$addColumnInfo(name = "obs_name", title = "", type = "string")
    table$addColumnInfo(name = "varname_obs", title = "", type = "string")
    table$addColumnInfo(name = "varname_pred1", title = ".", type = "integer")
    table$addColumnInfo(name = "varname_pred2", title = ".", type = "integer")
    table[["obs_name"]] <- c(gettext("Observed"), "")
    table[["varname_obs"]] <- rep(".", 2)
    table[["varname_pred1"]] <- rep("", 2)
    table[["varname_pred2"]] <- rep("", 2)
  }
}

.mlClassificationPlotBoundaries <- function(dataset, options, jaspResults, ready, position, type) {
  if (!is.null(jaspResults[["decisionBoundary"]]) || !options[["decisionBoundary"]]) {
    return()
  }
  plot <- createJaspPlot(title = gettext("Decision Boundary Matrix"), height = 400, width = 300)
  plot$position <- position
  plot$dependOn(options = c(.mlClassificationDependencies(options), "decisionBoundary", "pointsShown", "legendShown"))
  jaspResults[["decisionBoundary"]] <- plot
  if (!ready || length(options[["predictors"]]) < 2) {
    return()
  }
  .classificationFillDecisionBoundary(dataset, options, jaspResults, plot, type)
}

.classificationFillDecisionBoundary <- function(dataset, options, jaspResults, plot, type) {
  variables <- options[["predictors"]]
  variables <- variables[!vapply(dataset[, variables], is.factor, TRUE)] # remove factors from boundary plot
  l <- length(variables)
  if (l < 2) { # Need at least 2 numeric variables to create a matrix
    plot$setError(gettext("Cannot create matrix: not enough numeric variables remain after removing factor variables. You need at least 2 numeric variables."))
    return()
  }
  if (l <= 2) {
    width <- 400
    height <- 300
  } else {
    width <- 200 * l
    height <- 200 * l
  }
  plot[["width"]] <- width
  plot[["height"]] <- height
  plotMat <- matrix(list(), l - 1, l - 1)
  oldFontSize <- jaspGraphs::getGraphOption("fontsize")
  jaspGraphs::setGraphOption("fontsize", .85 * oldFontSize)
  target <- dataset[, options[["target"]]]
  startProgressbar(length(plotMat) + 1)
  for (row in 2:l) {
    for (col in 1:(l - 1)) {
      if (col < row) {
        predictors <- dataset[, variables]
        predictors <- predictors[, c(col, row)]
        formula <- formula(paste(options[["target"]], "~", paste(colnames(predictors), collapse = " + ")))
        plotMat[[row - 1, col]] <- .decisionBoundaryPlot(dataset, options, jaspResults, predictors, target, formula, l, type = type)
      }
      if (l > 2 && options[["legendShown"]]) {
        plotMat[[1, 2]] <- .legendPlot(dataset, options, col)
      }
      progressbarTick()
    }
  }
  jaspGraphs::setGraphOption("fontsize", oldFontSize)
  # slightly adjust the positions of the labels left and above the plots.
  labelPos <- matrix(.5, 4, 2)
  labelPos[1, 1] <- .55
  labelPos[4, 2] <- .65
  p <- jaspGraphs::ggMatrixPlot(
    plotList = plotMat, leftLabels = variables[-1], topLabels = variables[-length(variables)],
    scaleXYlabels = NULL, labelPos = labelPos
  )
  progressbarTick()
  plot$plotObject <- p
}

.decisionBoundaryPlot <- function(dataset, options, jaspResults, predictors, target, formula, l, type) {
  x_min <- min(predictors[, 1]) - 0.5
  x_max <- max(predictors[, 1]) + 0.5
  y_min <- min(predictors[, 2]) - 0.5
  y_max <- max(predictors[, 2]) + 0.5
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(predictors[, 1], min.n = 4)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(predictors[, 2], min.n = 4)
  x_min <- xBreaks[1]
  x_max <- xBreaks[length(xBreaks)]
  y_min <- yBreaks[1]
  y_max <- yBreaks[length(yBreaks)]
  # Adjust the graining
  hs <- min(c(diff(range(xBreaks)), diff(range(yBreaks)))) / 50
  grid <- as.data.frame(expand.grid(seq(x_min, x_max, by = hs), seq(y_min, y_max, by = hs)))
  colnames(grid) <- colnames(predictors)
  classificationResult <- jaspResults[["classificationResult"]]$object
  if (type == "lda") {
    fit <- MASS::lda(formula, data = dataset)
    predictions <- predict(fit, newdata = grid)$class
  } else if (type == "knn") {
    fit <- kknn::train.kknn(
      formula = formula, data = dataset, ks = classificationResult[["nn"]],
      distance = classificationResult[["distance"]], kernel = classificationResult[["weights"]], scale = FALSE
    )
    predictions <- predict(fit, newdata = grid)
  } else if (type == "randomForest") {
    fit <- randomForest::randomForest(
      x = predictors, y = target,
      ntree = classificationResult[["noOfTrees"]], mtry = classificationResult[["predPerSplit"]],
      sampsize = classificationResult[["baggingFraction"]], importance = TRUE, keep.forest = TRUE
    )
    predictions <- predict(fit, newdata = grid)
  } else if (type == "boosting") {
    fit <- gbm::gbm(
      formula = formula, data = dataset, n.trees = classificationResult[["noOfTrees"]],
      shrinkage = options[["shrinkage"]], interaction.depth = options[["interactionDepth"]],
      cv.folds = classificationResult[["noOfFolds"]], bag.fraction = options[["baggingFraction"]], n.minobsinnode = options[["minObservationsInNode"]],
      distribution = "multinomial", n.cores = 1
    ) # multiple cores breaks modules in JASP, see: INTERNAL-jasp#372
    probabilities <- gbm::predict.gbm(fit, newdata = grid, n.trees = classificationResult[["noOfTrees"]], type = "response")
    predictions <- colnames(probabilities)[apply(probabilities, 1, which.max)]
  } else if (type == "neuralnet") {
    structure <- .getNeuralNetworkStructure(options)
    fit <- neuralnet::neuralnet(
      formula = formula, data = dataset,
      hidden = structure,
      learningrate = options[["learningRate"]],
      threshold = options[["threshold"]],
      stepmax = options[["maxTrainingRepetitions"]],
      rep = 1, # The rep parameter is nothing more than a wrapper for looping over creating a neural network.
      startweights = NULL,
      algorithm = options[["algorithm"]],
      err.fct = "sse", # jaspResults[["lossFunction"]]$object, -> This does not work in the neuralnet package
      act.fct = jaspResults[["actfct"]]$object,
      linear.output = FALSE
    )
    predictions <- as.factor(max.col(predict(fit, newdata = grid)))
    levels(predictions) <- unique(dataset[, options[["target"]]])
  } else if (type == "rpart") {
    fit <- rpart::rpart(formula, data = dataset, method = "class", control = rpart::rpart.control(minsplit = options[["minObservationsForSplit"]], minbucket = options[["minObservationsInNode"]], maxdepth = options[["interactionDepth"]], cp = options[["complexityParameter"]]))
    predictions <- as.factor(max.col(predict(fit, newdata = grid)))
    levels(predictions) <- unique(dataset[, options[["target"]]])
  } else if (type == "svm") {
    fit <- e1071::svm(formula,
      data = dataset, method = "C-classification", kernel = options[["weights"]], cost = options[["cost"]], tolerance = options[["tolerance"]],
      epsilon = options[["epsilon"]], scale = FALSE, degree = options[["degree"]], gamma = options[["gamma"]], coef0 = options[["complexityParameter"]]
    )
    predictions <- predict(fit, newdata = grid)
  }
  shapes <- rep(21, nrow(dataset))
  if (type == "svm") {
    shapes[fit$index] <- 23
  }
  gridData <- data.frame(x = grid[, 1], y = grid[, 2])
  pointData <- data.frame(x = predictors[, 1], y = predictors[, 2], target = target, shapes = shapes)
  p <- ggplot2::ggplot(data = gridData, mapping = ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_raster(mapping = ggplot2::aes(fill = predictions), alpha = 0.3, show.legend = FALSE) +
    ggplot2::labs(fill = options[["target"]]) +
    ggplot2::scale_fill_manual(values = .mlColorScheme(n = length(unique(target)))) +
    ggplot2::scale_x_continuous(name = NULL, breaks = xBreaks, limits = range(xBreaks)) +
    ggplot2::scale_y_continuous(name = NULL, breaks = yBreaks, limits = range(yBreaks))
  if (options[["pointsShown"]]) {
    p <- p + jaspGraphs::geom_point(data = pointData, ggplot2::aes(x = x, y = y, fill = factor(target)), shape = shapes)
  }
  if (l <= 2) {
    p <- p + jaspGraphs::geom_rangeframe() +
      jaspGraphs::themeJaspRaw(legend.position = if (options[["legendShown"]]) "right" else "none")
  } else {
    p <- p + jaspGraphs::geom_rangeframe() +
      jaspGraphs::themeJaspRaw()
  }
  return(p)
}

.legendPlot <- function(dataset, options, col) {
  target <- dataset[, options[["target"]]]
  predictors <- dataset[, options[["predictors"]]]
  predictors <- predictors[, 1]
  plotData <- data.frame(target = target, predictors = predictors)
  p <- ggplot2::ggplot(plotData, ggplot2::aes(y = target, x = target, show.legend = TRUE)) +
    jaspGraphs::geom_point(ggplot2::aes(fill = target), alpha = 0) +
    ggplot2::xlab(NULL) +
    ggplot2::ylab(NULL) +
    ggplot2::theme(legend.key = ggplot2::element_blank()) +
    ggplot2::scale_fill_manual(name = options[["target"]], values = .mlColorScheme(length(unique(target)))) +
    jaspGraphs::geom_rangeframe(sides = "") +
    jaspGraphs::themeJaspRaw(legend.position = "left") +
    ggplot2::theme(axis.ticks = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank(), axis.text.y = ggplot2::element_blank()) +
    ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(alpha = 1)))
  return(p)
}

.mlClassificationPlotRoc <- function(dataset, options, jaspResults, ready, position, type) {
  if (!is.null(jaspResults[["rocCurve"]]) || !options[["rocCurve"]]) {
    return()
  }
  plot <- createJaspPlot(plot = NULL, title = gettext("ROC Curves Plot"), width = 450, height = 300)
  plot$position <- position
  plot$dependOn(options = c(.mlClassificationDependencies(options), "rocCurve"))
  jaspResults[["rocCurve"]] <- plot
  if (!ready) {
    return()
  }
  classificationResult <- jaspResults[["classificationResult"]]$object
  train <- classificationResult[["train"]]
  test <- classificationResult[["test"]]
  lvls <- levels(factor(train[, options[["target"]]]))
  predictors <- options[["predictors"]]
  formula <- formula(paste("levelVar", "~", paste(predictors, collapse = " + ")))
  linedata <- data.frame(x = c(0, 1), y = c(0, 1))
  p <- ggplot2::ggplot(data = linedata, mapping = ggplot2::aes(x = x, y = y)) +
    jaspGraphs::geom_line(col = "black", linetype = "dashed") +
    ggplot2::xlab(gettext("False Positive Rate")) +
    ggplot2::ylab(gettext("True Positive Rate"))
  rocXstore <- NULL
  rocYstore <- NULL
  rocNamestore <- NULL
  for (i in seq_along(lvls)) {
    levelVar <- train[, options[["target"]]] == lvls[i]
    typeData <- cbind(train, levelVar = factor(levelVar))
    column <- which(colnames(typeData) == options[["target"]])
    typeData <- typeData[, -column]
    actual.class <- test[, options[["target"]]] == lvls[i]
    if (length(levels(factor(actual.class))) != 2) { # This variable is not in the test set, we should skip it
      next
    }
    if (type == "knn") {
      fit <- kknn::kknn(
        formula = formula, train = typeData, test = test, k = classificationResult[["nn"]],
        distance = classificationResult[["distance"]], kernel = classificationResult[["weights"]], scale = FALSE
      )
      score <- predict(fit, test, type = "prob")[, "TRUE"]
    } else if (type == "lda") {
      fit <- MASS::lda(formula = formula, data = typeData, method = classificationResult[["method"]], CV = FALSE)
      score <- predict(fit, test, type = "prob")$posterior[, "TRUE"]
    } else if (type == "boosting") {
      levelVar <- as.character(levelVar)
      levelVar[levelVar == "TRUE"] <- 1
      levelVar[levelVar == "FALSE"] <- 0
      levelVar <- as.numeric(levelVar)
      column <- which(colnames(typeData) == "levelVar")
      typeData <- typeData[, -column]
      typeData <- cbind(typeData, levelVar = levelVar)
      fit <- gbm::gbm(
        formula = formula, data = typeData, n.trees = classificationResult[["noOfTrees"]],
        shrinkage = options[["shrinkage"]], interaction.depth = options[["interactionDepth"]],
        cv.folds = classificationResult[["noOfFolds"]], bag.fraction = options[["baggingFraction"]], n.minobsinnode = options[["minObservationsInNode"]],
        distribution = "bernoulli", n.cores = 1
      ) # multiple cores breaks modules in JASP, see: INTERNAL-jasp#372
      score <- predict(fit, newdata = test, n.trees = classificationResult[["noOfTrees"]], type = "response")
    } else if (type == "randomForest") {
      column <- which(colnames(typeData) == "levelVar")
      typeData <- typeData[, -column]
      fit <- randomForest::randomForest(
        x = typeData, y = factor(levelVar),
        ntree = classificationResult[["noOfTrees"]], mtry = classificationResult[["predPerSplit"]],
        sampsize = classificationResult[["baggingFraction"]], importance = TRUE, keep.forest = TRUE
      )
      score <- predict(fit, test, type = "prob")[, "TRUE"]
    } else if (type == "neuralnet") {
      structure <- .getNeuralNetworkStructure(options)
      fit <- neuralnet::neuralnet(
        formula = formula, data = typeData,
        hidden = structure,
        learningrate = options[["learningRate"]],
        threshold = options[["threshold"]],
        stepmax = options[["maxTrainingRepetitions"]],
        rep = 1,
        startweights = NULL,
        algorithm = options[["algorithm"]],
        err.fct = "sse", # jaspResults[["lossFunction"]]$object,
        act.fct = jaspResults[["actfct"]]$object,
        linear.output = FALSE
      )
      score <- max.col(predict(fit, test))
    } else if (type == "rpart") {
      fit <- rpart::rpart(formula, data = typeData, method = "class", control = rpart::rpart.control(minsplit = options[["minObservationsForSplit"]], minbucket = options[["minObservationsInNode"]], maxdepth = options[["interactionDepth"]], cp = options[["complexityParameter"]]))
      score <- max.col(predict(fit, test))
    } else if (type == "svm") {
      fit <- e1071::svm(formula,
        data = typeData, type = "C-classification", kernel = options[["weights"]], cost = options[["cost"]], tolerance = options[["tolerance"]],
        epsilon = options[["epsilon"]], scale = FALSE, degree = options[["degree"]], gamma = options[["gamma"]], coef0 = options[["complexityParameter"]]
      )
      score <- as.numeric(predict(fit, test))
    }
    pred <- ROCR::prediction(score, actual.class)
    nbperf <- ROCR::performance(pred, "tpr", "fpr")
    rocXstore <- c(rocXstore, unlist(nbperf@x.values))
    rocYstore <- c(rocYstore, unlist(nbperf@y.values))
    rocNamestore <- c(rocNamestore, rep(lvls[i], length(unlist(nbperf@y.values))))
  }
  rocData <- data.frame(x = rocXstore, y = rocYstore, name = rocNamestore)
  rocData <- na.omit(rocData) # Remove classes that are not in the test set
  p <- p + jaspGraphs::geom_line(data = rocData, mapping = ggplot2::aes(x = x, y = y, col = name)) +
    ggplot2::scale_color_manual(values = .mlColorScheme(length(lvls))) +
    ggplot2::scale_y_continuous(limits = c(0, 1.1), breaks = seq(0, 1, 0.2)) +
    ggplot2::scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    ggplot2::labs(color = options[["target"]]) +
    jaspGraphs::geom_point(data = data.frame(x = 0, y = 1), mapping = ggplot2::aes(x = x, y = y)) +
    ggrepel::geom_text_repel(
      data = data.frame(x = 0, y = 1),
      mapping = ggplot2::aes(label = gettext("Perfect separation"), x = x, y = y),
      nudge_x = 0.1, nudge_y = 0.05, xlim = c(0, 1), ylim = c(0, 1.1), seed = 1
    ) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw(legend.position = "right")
  plot$plotObject <- p
}

.mlClassificationPlotAndrews <- function(dataset, options, jaspResults, ready, position) {
  if (!is.null(jaspResults[["andrewsCurve"]]) || !options[["andrewsCurve"]]) {
    return()
  }
  plot <- createJaspPlot(plot = NULL, title = gettext("Andrews Curves Plot"), width = 450, height = 300)
  plot$position <- position
  plot$dependOn(options = c("andrewsCurve", "scaleVariables", "target", "predictors", "seed", "setSeed"))
  jaspResults[["andrewsCurve"]] <- plot
  if (!ready) {
    return()
  }
  if (length(options[["predictors"]]) < 2) {
    plot$setError(gettext("Andrews curves require a minimum of 2 feature variables."))
    return()
  }
  if (nrow(dataset) > 500) {
    sample <- sample(seq_len(nrow(dataset)), size = 500, replace = FALSE) # Sample to prevent crazy long loading times with big data
  } else {
    sample <- seq_len(nrow(dataset))
  }
  predictors <- dataset[sample, options[["predictors"]]]
  target <- dataset[sample, options[["target"]]]
  # Taken from function `andrewsplot()` in R package "andrewsplot", thanks!
  n <- nrow(predictors)
  m <- ncol(predictors)
  npts <- 100
  xpts <- seq(0 - pi, 0 + pi, length = npts)
  Y <- matrix(NA, nrow = n, ncol = npts)
  for (i in 1:n) {
    xs <- as.numeric(predictors[i, ])
    ypts <- c()
    for (p in xpts) {
      y <- xs[1]
      for (j in 2:m) {
        if (j %% 2 == 1) {
          y <- y + xs[j] * sin((j %/% 2) * p)
        } else {
          y <- y + xs[j] * cos((j %/% 2) * p)
        }
      }
      ypts <- c(ypts, y)
    }
    Y[i, ] <- as.numeric(ypts)
  }
  Yvec <- NULL
  for (i in seq_len(nrow(Y))) {
    Yvec <- c(Yvec, Y[i, ])
  }
  plotData <- data.frame(
    x = rep(xpts, n),
    y = Yvec,
    target = rep(target, each = length(xpts)),
    observation = rep(1:n, each = length(xpts))
  )
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(plotData$y, min.n = 4)
  p <- ggplot2::ggplot(data = plotData, mapping = ggplot2::aes(x = x, y = y, color = target, group = observation)) +
    ggplot2::geom_line(linewidth = 0.2) +
    ggplot2::scale_x_continuous(name = NULL, breaks = c(-pi, -pi / 2, 0, pi / 2, pi), labels = c("-\u03C0", "-\u03C0/2", "0", "\u03C0/2", "\u03C0"), limits = c(-pi, pi)) +
    ggplot2::scale_y_continuous(name = NULL, breaks = yBreaks, limits = range(yBreaks)) +
    ggplot2::labs(color = options[["target"]]) +
    ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 1))) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw(legend.position = "right")
  plot$plotObject <- p
}

.boxM <- function(data, grouping) {
  # Taken from the R package "biotools", thanks!
  dname <- deparse(substitute(data))
  data <- as.matrix(data)
  grouping <- as.factor(as.character(grouping))
  p <- ncol(data)
  nlev <- nlevels(grouping)
  lev <- levels(grouping)
  dfs <- tapply(grouping, grouping, length) - 1
  mats <- aux <- list()
  for (i in 1:nlev) {
    mats[[i]] <- cov(data[grouping == lev[i], ])
    aux[[i]] <- mats[[i]] * dfs[i]
  }
  names(mats) <- lev
  pooled <- Reduce("+", aux) / sum(dfs)
  logdet <- log(unlist(lapply(mats, det)))
  minus2logM <- sum(dfs) * log(det(pooled)) - sum(logdet *
    dfs)
  sum1 <- sum(1 / dfs)
  Co <- (((2 * p^2) + (3 * p) - 1) / (6 * (p + 1) * (nlev - 1))) *
    (sum1 - (1 / sum(dfs)))
  X2 <- minus2logM * (1 - Co)
  dfchi <- (choose(p, 2) + p) * (nlev - 1)
  pval <- pchisq(X2, dfchi, lower.tail = FALSE)
  out <- structure(list(
    statistic = c(`Chi-Sq (approx.)` = X2),
    parameter = c(df = dfchi), p.value = pval, cov = mats,
    pooled = pooled, logDet = logdet, data.name = dname,
    method = gettext(" Box's M-test for Homogeneity of Covariance Matrices")
  ),
  class = c("htest", "boxM")
  )
  return(out)
}

.mlClassificationTableMetrics <- function(dataset, options, jaspResults, ready, position) {
  if (!is.null(jaspResults[["validationMeasures"]]) || !options[["validationMeasures"]]) {
    return()
  }
  table <- createJaspTable(title = gettext("Model Performance Metrics"))
  table$position <- position
  table$transpose <- TRUE
  table$dependOn(options = c(.mlClassificationDependencies(options), "validationMeasures"))
  table$addColumnInfo(name = "group", title = "", type = "string")
  table$addColumnInfo(name = "support", title = gettext("Support"), type = "integer")
  table$addColumnInfo(name = "accuracy", title = gettext("Accuracy"), type = "number")
  table$addColumnInfo(name = "precision", title = gettext("Precision (Positive Predictive Value)"), type = "number")
  table$addColumnInfo(name = "recall", title = gettext("Recall (True Positive Rate)"), type = "number")
  table$addColumnInfo(name = "fpr", title = gettext("False Positive Rate"), type = "number")
  table$addColumnInfo(name = "fdr", title = gettext("False Discovery Rate"), type = "number")
  table$addColumnInfo(name = "f1", title = gettext("F1 Score"), type = "number")
  table$addColumnInfo(name = "mcc", title = gettext("Matthews Correlation Coefficient"), type = "number")
  table$addColumnInfo(name = "auc", title = gettext("Area Under Curve (AUC)"), type = "number")
  table$addColumnInfo(name = "npv", title = gettext("Negative Predictive Value"), type = "number")
  table$addColumnInfo(name = "tnr", title = gettext("True Negative Rate"), type = "number")
  table$addColumnInfo(name = "fnr", title = gettext("False Negative Rate"), type = "number")
  table$addColumnInfo(name = "for", title = gettext("False Omission Rate"), type = "number")
  table$addColumnInfo(name = "ts", title = gettext("Threat Score"), type = "number")
  table$addColumnInfo(name = "stp", title = gettext("Statistical Parity"), type = "number")
  table$addFootnote(gettext("All metrics are calculated for every class against all other classes."))
  if (options[["target"]] != "") {
    table[["group"]] <- c(levels(factor(dataset[, options[["target"]]])), gettext("Average / Total"))
  }
  jaspResults[["validationMeasures"]] <- table
  if (!ready) {
    return()
  }
  classificationResult <- jaspResults[["classificationResult"]]$object
  pred <- factor(classificationResult[["testPred"]])
  real <- factor(classificationResult[["testReal"]])
  lvls <- levels(as.factor(real))
  support <- rep(NA, length(lvls))
  accuracy <- rep(NA, length(lvls))
  precision <- rep(NA, length(lvls))
  recall <- rep(NA, length(lvls))
  f1 <- rep(NA, length(lvls))
  mcc <- rep(NA, length(lvls))
  auc <- classificationResult[["auc"]]
  tnr <- rep(NA, length(lvls))
  npv <- rep(NA, length(lvls))
  fnr <- rep(NA, length(lvls))
  fpr <- rep(NA, length(lvls))
  fdr <- rep(NA, length(lvls))
  foor <- rep(NA, length(lvls))
  ts <- rep(NA, length(lvls))
  stp <- rep(NA, length(lvls))
  for (i in seq_along(lvls)) {
    TP <- length(which(pred == lvls[i] & real == lvls[i]))
    TN <- length(which(pred != lvls[i] & real != lvls[i]))
    FN <- length(which(pred != lvls[i] & real == lvls[i]))
    FP <- length(which(pred == lvls[i] & real != lvls[i]))
    support[i]  <- length(which(real == lvls[i]))
    accuracy[i] <- (TP + TN) / (TP + FN + FP + TN)
    precision[i] <- TP / (TP + FP)
    recall[i] <- TP / (TP + FN)
    f1[i] <- 2 * ((precision[i] * recall[i]) / (precision[i] + recall[i]))
    mcc[i] <- ((TP * TN) - (FP * FN)) / sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))
    # Source: https://github.com/ModelOriented/fairmodels
    tnr[i] <- TN / (TN + FP)
    npv[i] <- TN / (TN + FN)
    fnr[i] <- FN / (FN + TP)
    fpr[i] <- FP / (FP + TN)
    fdr[i] <- FP / (FP + TP)
    foor[i] <- FN / (FN + TN)
    ts[i] <- TP / (FP + FN + FP)
    stp[i] <- (TP + FP) / (TP + FN + FP + TN)
  }
  support[length(support) + 1] <- sum(support, na.rm = TRUE)
  accuracy[length(accuracy) + 1] <- mean(accuracy, na.rm = TRUE)
  precision[length(precision) + 1] <- sum(precision * support[seq_along(lvls)], na.rm = TRUE) / sum(support[seq_along(lvls)], na.rm = TRUE)
  recall[length(recall) + 1] <- sum(recall * support[seq_along(lvls)], na.rm = TRUE) / sum(support[seq_along(lvls)], na.rm = TRUE)
  f1[length(f1) + 1] <- sum(f1 * support[seq_along(lvls)], na.rm = TRUE) / sum(support[seq_along(lvls)], na.rm = TRUE)
  mcc[length(mcc) + 1] <- mean(mcc, na.rm = TRUE)
  auc[length(auc) + 1] <- mean(auc, na.rm = TRUE)
  tnr[length(tnr) + 1] <- mean(tnr, na.rm = TRUE)
  npv[length(npv) + 1] <- mean(npv, na.rm = TRUE)
  fnr[length(fnr) + 1] <- mean(fnr, na.rm = TRUE)
  fpr[length(fpr) + 1] <- mean(fpr, na.rm = TRUE)
  fdr[length(fdr) + 1] <- mean(fdr, na.rm = TRUE)
  foor[length(foor) + 1] <- mean(foor, na.rm = TRUE)
  ts[length(ts) + 1] <- mean(ts, na.rm = TRUE)
  stp[length(stp) + 1] <- sum(stp, na.rm = TRUE)
  table[["group"]] <- c(levels(factor(classificationResult[["test"]][, options[["target"]]])), "Average / Total") # fill again to adjust for missing categories
  table[["accuracy"]] <- accuracy
  table[["precision"]] <- precision
  table[["recall"]] <- recall
  table[["f1"]] <- f1
  table[["mcc"]] <- mcc
  table[["support"]] <- support
  table[["auc"]] <- auc
  table[["tnr"]] <- tnr
  table[["npv"]] <- npv
  table[["fnr"]] <- fnr
  table[["fpr"]] <- fpr
  table[["fdr"]] <- fdr
  table[["for"]] <- foor
  table[["ts"]] <- ts
  table[["stp"]] <- stp
}

.mlClassificationTableProportions <- function(dataset, options, jaspResults, ready, position) {
  if (!is.null(jaspResults[["classProportionsTable"]]) || !options[["classProportionsTable"]]) {
    return()
  }
  table <- createJaspTable(title = gettext("Class Proportions"))
  table$position <- position
  table$dependOn(options = c(.mlClassificationDependencies(options), "classProportionsTable"))
  table$addColumnInfo(name = "group", title = "", type = "string")
  table$addColumnInfo(name = "dataset", title = gettext("Data Set"), type = "number")
  if (options[["modelOptimization"]] == "manual") {
    table$addColumnInfo(name = "train", title = gettext("Training Set"), type = "number")
  } else {
    if (options[["modelValid"]] == "validationManual") {
      table$addColumnInfo(name = "train", title = gettext("Training Set"), type = "number")
      table$addColumnInfo(name = "valid", title = gettext("Validation Set"), type = "number")
    } else {
      table$addColumnInfo(name = "train", title = gettext("Training and Validation Set"), type = "number")
    }
  }
  table$addColumnInfo(name = "test", title = gettext("Test Set"), type = "number")
  if (options[["target"]] != "") {
    table[["group"]] <- levels(factor(dataset[, options[["target"]]]))
    Dlevels <- levels(factor(dataset[, options[["target"]]]))
  }
  jaspResults[["classProportionsTable"]] <- table
  if (!ready) {
    return()
  }
  classificationResult <- jaspResults[["classificationResult"]]$object
  dataValues <- rep(0, length(table[["group"]]))
  trainingValues <- rep(0, length(table[["group"]]))
  if (options[["modelOptimization"]] != "manual") {
    validValues <- rep(0, length(table[["group"]]))
  }
  testValues <- rep(0, length(table[["group"]]))
  dataTable <- prop.table(table(dataset[, options[["target"]]]))
  trainingTable <- prop.table(table(classificationResult[["train"]][, options[["target"]]]))
  if (options[["modelOptimization"]] != "manual") {
    validTable <- prop.table(table(classificationResult[["valid"]][, options[["target"]]]))
  }
  testTable <- prop.table(table(classificationResult[["test"]][, options[["target"]]]))
  for (i in seq_along(Dlevels)) {
    # Dataset
    dataIndex <- which(names(dataTable) == as.character(Dlevels)[i])
    dataValues[dataIndex] <- as.numeric(dataTable)[dataIndex]
    # Training data
    trainingIndex <- which(names(trainingTable) == as.character(Dlevels)[i])
    trainingValues[trainingIndex] <- as.numeric(trainingTable)[trainingIndex]
    # Validation set
    if (options[["modelOptimization"]] != "manual") {
      validIndex <- which(names(validTable) == as.character(Dlevels)[i])
      validValues[validIndex] <- as.numeric(validTable)[validIndex]
    }
    # Test set
    testIndex <- which(names(testTable) == as.character(Dlevels)[i])
    testValues[testIndex] <- as.numeric(testTable)[testIndex]
  }
  table[["dataset"]] <- dataValues
  table[["train"]] <- trainingValues
  if (options[["modelOptimization"]] != "manual") {
    table[["valid"]] <- validValues
  }
  table[["test"]] <- testValues
}

.mlClassificationAddPredictionsToData <- function(dataset, options, jaspResults, ready) {
  if (!ready || !options[["addPredictions"]] || options[["predictionsColumn"]] == "") {
    return()
  }
  classificationResult <- jaspResults[["classificationResult"]]$object
  if (is.null(jaspResults[["predictionsColumn"]])) {
    predictions <- as.character(classificationResult[["classes"]])
    predictionsColumn <- rep(NA, max(as.numeric(rownames(dataset))))
    predictionsColumn[as.numeric(rownames(dataset))] <- predictions
    predictionsColumn <- factor(predictionsColumn)
    jaspResults[["predictionsColumn"]] <- createJaspColumn(columnName = options[["predictionsColumn"]])
    jaspResults[["predictionsColumn"]]$dependOn(options = c(.mlClassificationDependencies(options), "predictionsColumn", "addPredictions"))
    # make sure to create to classification column with the same type as the target!
    if (.columnIsScale(options$target)) jaspResults[["predictionsColumn"]]$setScale(predictionsColumn)
    if (.columnIsOrdinal(options$target)) jaspResults[["predictionsColumn"]]$setOrdinal(predictionsColumn)
    if (.columnIsNominal(options$target)) jaspResults[["predictionsColumn"]]$setNominal(predictionsColumn)
    if (.columnIsNominalText(options$target)) jaspResults[["predictionsColumn"]]$setNominalText(predictionsColumn)
  }
}

.classificationCalcAUC <- function(test, train, options, class, ...) {
  lvls <- levels(factor(test[, options[["target"]]]))
  auc <- numeric(length(lvls))
  predictorNames <- options[["predictors"]]
  AUCformula <- formula(paste("levelVar", "~", paste(predictorNames, collapse = " + ")))
  class(AUCformula) <- c(class(AUCformula), class)
  for (i in seq_along(lvls)) {
    levelVar <- train[, options[["target"]]] == lvls[i]
    if (all(!levelVar)) { # This class is not in the training set
      auc[i] <- 0
    } else {
      typeData <- cbind(train, levelVar = factor(levelVar))
      typeData <- typeData[, -which(colnames(typeData) == options[["target"]])]
      score <- .calcAUCScore(AUCformula, train = train, test = test, typeData = typeData, levelVar = levelVar, options = options, ...)
      actual.class <- test[, options[["target"]]] == lvls[i]
      if (length(levels(factor(actual.class))) == 2) {
        pred <- ROCR::prediction(score, actual.class)
        auc[i] <- ROCR::performance(pred, "auc")@y.values[[1]]
      } else { # This variable is not in the test set, we should skip it
        auc[i] <- 0 # Gets removed in table
      }
    }
  }
  return(auc)
}

.calcAUCScore <- function(x, ...) {
  UseMethod(".calcAUCScore", x)
}

.calcAUCScore.ldaClassification <- function(AUCformula, test, typeData, LDAmethod, ...) {
  fit <- MASS::lda(formula = AUCformula, data = typeData, method = LDAmethod, CV = FALSE)
  score <- predict(fit, test, type = "prob")$posterior[, "TRUE"]
  return(score)
}

.calcAUCScore.boostingClassification <- function(AUCformula, test, typeData, levelVar, options, noOfFolds, noOfTrees, ...) {
  levelVar <- as.numeric(levelVar)
  typeData$levelVar <- levelVar
  fit <- gbm::gbm(
    formula = AUCformula, data = typeData, n.trees = noOfTrees,
    shrinkage = options[["shrinkage"]], interaction.depth = options[["interactionDepth"]],
    cv.folds = noOfFolds, bag.fraction = options[["baggingFraction"]], n.minobsinnode = options[["minObservationsInNode"]],
    distribution = "bernoulli", n.cores = 1
  ) # Multiple cores breaks modules in JASP, see: INTERNAL-jasp#372
  score <- predict(fit, newdata = test, n.trees = noOfTrees, type = "response")
  return(score)
}

.calcAUCScore.knnClassification <- function(AUCformula, test, typeData, nn, distance, weights, ...) {
  fit <- kknn::kknn(formula = AUCformula, train = typeData, test = test, k = nn, distance = distance, kernel = weights, scale = FALSE)
  score <- predict(fit, test, type = "prob")[, "TRUE"]
  return(score)
}

.calcAUCScore.randomForestClassification <- function(AUCformula, train, test, typeData, levelVar, options, noOfTrees, noOfPredictors, ...) {
  typeData <- typeData[, -which(colnames(typeData) == "levelVar")]
  fit <- randomForest::randomForest(
    x = typeData, y = factor(levelVar), ntree = noOfTrees, mtry = noOfPredictors,
    sampsize = ceiling(options[["baggingFraction"]] * nrow(train)), importance = TRUE, keep.forest = TRUE
  )
  score <- predict(fit, test, type = "prob")[, "TRUE"]
  return(score)
}

.calcAUCScore.nnClassification <- function(AUCformula, test, typeData, options, jaspResults, ...) {
  structure <- .getNeuralNetworkStructure(options)
  fit <- neuralnet::neuralnet(
    formula = AUCformula,
    data = typeData,
    hidden = structure,
    learningrate = options[["learningRate"]],
    threshold = options[["threshold"]],
    stepmax = options[["maxTrainingRepetitions"]],
    rep = 1,
    startweights = NULL,
    algorithm = options[["algorithm"]],
    err.fct = "sse",
    act.fct = jaspResults[["actfct"]]$object,
    linear.output = FALSE
  )
  score <- max.col(predict(fit, test))
  return(score)
}

.calcAUCScore.partClassification <- function(AUCformula, test, typeData, options, jaspResults, ...) {
  fit <- rpart::rpart(AUCformula, data = typeData, method = "class", control = rpart::rpart.control(minsplit = options[["minObservationsForSplit"]], minbucket = options[["minObservationsInNode"]], maxdepth = options[["interactionDepth"]], cp = options[["complexityParameter"]]))
  score <- max.col(predict(fit, test))
  return(score)
}

.calcAUCScore.svmClassification <- function(AUCformula, test, typeData, options, jaspResults, ...) {
  fit <- e1071::svm(AUCformula,
    data = typeData, type = "C-classification", kernel = options[["weights"]], cost = options[["cost"]], tolerance = options[["tolerance"]],
    epsilon = options[["epsilon"]], scale = FALSE, degree = options[["degree"]], gamma = options[["gamma"]], coef0 = options[["complexityParameter"]]
  )
  score <- as.numeric(predict(fit, test))
  return(score)
}
