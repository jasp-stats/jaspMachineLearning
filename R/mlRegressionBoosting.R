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

mlRegressionBoosting <- function(jaspResults, dataset, options, ...) {

  # Preparatory work
  dataset <- .readDataRegressionAnalyses(dataset, options)
  .mlRegressionErrorHandling(dataset, options, type = "boosting")

  # Check if analysis is ready to run
  ready <- .mlRegressionReady(options, type = "boosting")

  # Compute results and create the model summary table
  .mlRegressionTableSummary(dataset, options, jaspResults, ready, position = 1, type = "boosting")

  # If the user wants to add the values to the data set
  .mlRegressionAddPredictionsToData(dataset, options, jaspResults, ready)

  # Add test set indicator to data
  .mlAddTestIndicatorToData(options, jaspResults, ready, purpose = "regression")

  # Create the data split plot
  .mlPlotDataSplit(dataset, options, jaspResults, ready, position = 2, purpose = "regression", type = "boosting")

  # Create the evaluation metrics table
  .mlRegressionTableMetrics(dataset, options, jaspResults, ready, position = 3)

  # Create the relative influence table
  .mlBoostingTableRelInf(options, jaspResults, ready, position = 4, purpose = "regression")

  # Create the OOB improvement plot
  .mlBoostingPlotOobImprovement(options, jaspResults, ready, position = 5, purpose = "regression")

  # Create the predicted performance plot
  .mlRegressionPlotPredictedPerformance(options, jaspResults, ready, position = 6)

  # Create the deviance plot
  .mlBoostingPlotDeviance(options, jaspResults, ready, position = 7, purpose = "regression")

  # Create the relative influence plot
  .mlBoostingPlotRelInf(options, jaspResults, ready, position = 8, purpose = "regression")
}

.boostingRegression <- function(dataset, options, jaspResults) {
  jaspBase:::assignFunctionInPackage(fakeGbmCrossValModelBuild, "gbmCrossValModelBuild", "gbm")
  jaspBase:::assignFunctionInPackage(fakeGbmCrossValErr, "gbmCrossValErr", "gbm")
  # Import model formula from jaspResults
  formula <- jaspResults[["formula"]]$object
  # Set model-specific parameters
  trees <- switch(options[["modelOpt"]],
    "optimizationManual" = options[["noOfTrees"]],
    "optimizationOOB" = options[["maxTrees"]]
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
  trainingAndValidationSet <- trainingAndValidationSet[, match(names(trainingAndValidationSet)[which(names(trainingAndValidationSet) %in% all.vars(formula))], all.vars(formula))]
  if (options[["modelOpt"]] == "optimizationManual") {
    # Just create a train and a test set (no optimization)
    trainingSet <- trainingAndValidationSet
    testSet <- dataset[-trainingIndex, ]
    noOfFolds <- 0
    trainingFit <- gbm::gbm(
      formula = formula, data = trainingAndValidationSet, n.trees = trees,
      shrinkage = options[["shrinkage"]], interaction.depth = options[["intDepth"]],
      cv.folds = noOfFolds, bag.fraction = options[["bagFrac"]],
      n.minobsinnode = options[["nNode"]], distribution = options[["distance"]], n.cores = 1
    ) # Multiple cores breaks modules in JASP, see: INTERNAL-jasp#372
    noOfTrees <- options[["noOfTrees"]]
  } else if (options[["modelOpt"]] == "optimizationOOB") {
    # Create a train, validation and test set (optimization)
    validationIndex <- sample.int(nrow(trainingAndValidationSet), size = ceiling(options[["validationDataManual"]] * nrow(trainingAndValidationSet)))
    testSet <- dataset[-trainingIndex, ]
    validationSet <- trainingAndValidationSet[validationIndex, ]
    trainingSet <- trainingAndValidationSet[-validationIndex, ]
    if (options[["modelValid"]] == "validationManual") {
      noOfFolds <- 0
    } else if (options[["modelValid"]] == "validationKFold") {
      noOfFolds <- options[["noOfFolds"]]
      trainingSet <- trainingAndValidationSet
      validationSet <- trainingAndValidationSet
    }
    trainingFit <- gbm::gbm(
      formula = formula, data = trainingSet, n.trees = trees,
      shrinkage = options[["shrinkage"]], interaction.depth = options[["intDepth"]],
      cv.folds = noOfFolds, bag.fraction = options[["bagFrac"]],
      n.minobsinnode = options[["nNode"]], distribution = options[["distance"]], n.cores = 1
    ) # Multiple cores breaks modules in JASP, see: INTERNAL-jasp#372
    noOfTrees <- gbm::gbm.perf(trainingFit, plot.it = FALSE, method = "OOB")[1] # pick the optimal number of trees
    trainingFit <- gbm::gbm(
      formula = formula, data = trainingSet, n.trees = noOfTrees,
      shrinkage = options[["shrinkage"]], interaction.depth = options[["intDepth"]],
      cv.folds = noOfFolds, bag.fraction = options[["bagFrac"]], n.minobsinnode = options[["nNode"]],
      distribution = options[["distance"]], n.cores = 1
    ) # Multiple cores breaks modules in JASP, see: INTERNAL-jasp#372
    validationPredictions <- gbm::predict.gbm(trainingFit, validationSet, n.trees = noOfTrees, type = "response")
  }
  # Use the specified model to make predictions
  dataPredictions <- gbm::predict.gbm(trainingFit, dataset, n.trees = noOfTrees, type = "response")
  testPredictions <- dataPredictions[-trainingIndex]
  # Create results object
  result <- list()
  result[["model"]] <- trainingFit
  result[["formula"]] <- formula
  result[["noOfFolds"]] <- noOfFolds
  result[["noOfTrees"]] <- noOfTrees
  result[["method"]] <- ifelse(options[["modelValid"]] == "validationManual", yes = "OOB", no = "")
  result[["testMSE"]] <- mean((testPredictions - testSet[, options[["target"]]])^2)
  result[["relInf"]] <- summary(trainingFit, plot = FALSE)
  result[["ntrain"]] <- nrow(trainingSet)
  result[["ntest"]] <- nrow(testSet)
  result[["testPred"]] <- testPredictions
  result[["testReal"]] <- testSet[, options[["target"]]]
  result[["train"]] <- trainingSet
  result[["test"]] <- testSet
  result[["testIndicatorColumn"]] <- testIndicatorColumn
  result[["values"]] <- dataPredictions
  if (options[["modelOpt"]] != "optimizationManual") {
    result[["validMSE"]] <- mean((validationPredictions - validationSet[, options[["target"]]])^2)
    result[["nvalid"]] <- nrow(validationSet)
    result[["valid"]] <- validationSet
  }
  return(result)
}

.mlBoostingTableRelInf <- function(options, jaspResults, ready, position, purpose) {
  if (!options[["classBoostRelInfTable"]] || !is.null(jaspResults[["classBoostRelInfTable"]])) {
    return()
  }
  table <- createJaspTable(title = gettext("Relative Influence"))
  table$position <- position
  table$dependOn(options = c(
    "classBoostRelInfTable", "target", "predictors", "modelOpt", "maxTrees", "intDepth", "shrinkage",
    "noOfTrees", "bagFrac", "noOfPredictors", "numberOfPredictors", "seed", "seedBox", "modelValid",
    "nNode", "distance", "testSetIndicatorVariable", "testSetIndicator", "validationDataManual",
    "holdoutData", "testDataManual"
  ))
  table$addColumnInfo(name = "predictor", title = "", type = "string")
  table$addColumnInfo(name = "relIn", title = gettext("Relative Influence"), type = "number")
  jaspResults[["classBoostRelInfTable"]] <- table
  if (!ready) {
    return()
  }
  result <- switch(purpose,
    "classification" = jaspResults[["classificationResult"]]$object,
    "regression" = jaspResults[["regressionResult"]]$object
  )
  table[["predictor"]] <- as.character(result[["relInf"]]$var)
  table[["relIn"]] <- result[["relInf"]]$rel.inf
}

.mlBoostingPlotOobImprovement <- function(options, jaspResults, ready, position, purpose) {
  if (!is.null(jaspResults[["plotOOBChangeDev"]]) || !options[["plotOOBChangeDev"]]) {
    return()
  }
  plot <- createJaspPlot(plot = NULL, title = gettext("Out-of-bag Improvement Plot"), width = 400, height = 300)
  plot$position <- position
  plot$dependOn(options = c(
    "plotOOBChangeDev", "target", "predictors", "modelOpt", "maxTrees", "intDepth", "shrinkage",
    "noOfTrees", "bagFrac", "noOfPredictors", "numberOfPredictors", "seed", "seedBox", "modelValid",
    "nNode", "distance", "testSetIndicatorVariable", "testSetIndicator", "validationDataManual",
    "holdoutData", "testDataManual"
  ))
  jaspResults[["plotOOBChangeDev"]] <- plot
  if (!ready) {
    return()
  }
  result <- switch(purpose,
    "classification" = jaspResults[["classificationResult"]]$object,
    "regression" = jaspResults[["regressionResult"]]$object
  )
  oobDev <- data.frame(trees = 1:result[["model"]]$n.trees, oobImprove = result[["model"]]$oobag.improve, type = gettext("Training set"))
  if (purpose == "classification") {
    if (nlevels(result[["test"]][, options[["target"]]]) > 2L) {
      ylab <- gettextf("OOB Change in %s Multinomial Deviance", "\n")
    } else {
      ylab <- gettextf("OOB Change in %s Binomial Deviance", "\n")
    }
  } else {
    distribution <- .regressionGetDistributionFromDistance(options[["distance"]])
    ylab <- gettextf("OOB Change in %s%s Deviance", "\n", distribution)
  }
  if (nrow(oobDev) <= 5L) {
    geom <- jaspGraphs::geom_point
    xBreaks <- 1:xend
  } else {
    geom <- jaspGraphs::geom_line
    xBreaks <- jaspGraphs::getPrettyAxisBreaks(oobDev[["trees"]], min.n = 4)
  }
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(0, oobDev[["oobImprove"]]), min.n = 4)
  xLabels <- jaspGraphs::axesLabeller(xBreaks)
  yLabels <- jaspGraphs::axesLabeller(yBreaks)
  xend <- max(xBreaks)
  p <- ggplot2::ggplot(data = oobDev, mapping = ggplot2::aes(x = trees, y = oobImprove)) +
    ggplot2::geom_segment(
      data = data.frame(xstart = 0, xend = xend, ystart = 0, yend = 0),
      mapping = ggplot2::aes(x = xstart, xend = xend, y = ystart, yend = yend), linetype = "dashed", col = "darkgrey"
    ) +
    jaspGraphs::geom_line(mapping = ggplot2::aes(linetype = type)) +
    ggplot2::geom_smooth(method = "loess", formula = y ~ x, size = 1, colour = "darkred", se = FALSE) +
    ggplot2::scale_x_continuous(name = gettext("Number of Trees"), labels = xLabels, breaks = xBreaks, limits = c(0, max(xBreaks))) +
    ggplot2::scale_y_continuous(name = ylab, labels = yLabels, breaks = yBreaks, limits = range(yBreaks)) +
    ggplot2::labs(linetype = NULL) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw(legend.position = "top")
  plot$plotObject <- p
}

.mlBoostingPlotDeviance <- function(options, jaspResults, ready, position, purpose) {
  if (!is.null(jaspResults[["plotDeviance"]]) || !options[["plotDeviance"]]) {
    return()
  }
  plot <- createJaspPlot(plot = NULL, title = gettext("Deviance Plot"), width = 450, height = 300)
  plot$position <- position
  plot$dependOn(options = c(
    "plotDeviance", "target", "predictors", "modelOpt", "maxTrees", "intDepth", "shrinkage",
    "noOfTrees", "bagFrac", "noOfPredictors", "numberOfPredictors", "seed", "seedBox", "modelValid",
    "nNode", "distance", "testSetIndicatorVariable", "testSetIndicator", "validationDataManual",
    "holdoutData", "testDataManual"
  ))
  jaspResults[["plotDeviance"]] <- plot
  if (!ready) {
    return()
  }
  result <- switch(purpose,
    "classification" = jaspResults[["classificationResult"]]$object,
    "regression" = jaspResults[["regressionResult"]]$object
  )
  deviance <- data.frame(
    trees = 1:result[["model"]]$n.trees,
    trainError = c(result[["model"]]$train.error, result[["model"]]$cv.error),
    what = rep(c("Out-of-bag", "Cross-validated"), c(length(result[["model"]]$train.error), length(result[["model"]]$cv.error)))
  )
  if (purpose == "classification") {
    if (nlevels(result[["test"]][, options[["target"]]]) > 2L) {
      ylab <- gettext("Multinomial Deviance")
    } else {
      ylab <- gettext("Binomial Deviance")
    }
  } else {
    distribution <- .regressionGetDistributionFromDistance(options[["distance"]])
    ylab <- gettextf("%s Deviance", distribution)
  }
  if (max(deviance[["trees"]]) <= 5L) {
    geom <- jaspGraphs::geom_point
    xBreaks <- 1:max(deviance[["trees"]])
  } else {
    geom <- jaspGraphs::geom_line
    xBreaks <- jaspGraphs::getPrettyAxisBreaks(deviance[["trees"]], min.n = 4)
  }
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(0, deviance[["trainError"]]), min.n = 4)
  xLabels <- jaspGraphs::axesLabeller(xBreaks)
  yLabels <- jaspGraphs::axesLabeller(yBreaks)
  xend <- max(xBreaks)
  p <- ggplot2::ggplot(data = deviance, mapping = ggplot2::aes(x = trees, y = trainError, group = what, color = what)) +
    ggplot2::geom_segment(
      data = data.frame(xstart = 0, xend = xend, ystart = 0, yend = 0, group = "Out-of-bag", what = "Out-of-bag"),
      ggplot2::aes(x = xstart, xend = xend, y = ystart, yend = yend), linetype = "dashed", col = "darkgrey"
    ) +
    geom(show.legend = result[["method"]] != "OOB") +
    ggplot2::scale_x_continuous(name = gettext("Number of Trees"), labels = xLabels, breaks = xBreaks, limits = c(0, max(xBreaks))) +
    ggplot2::scale_y_continuous(name = ylab, labels = yLabels, breaks = yBreaks, limits = range(yBreaks)) +
    ggplot2::scale_color_manual(name = NULL, values = c("Out-of-bag" = "gray20", "Cross-validated" = "#99c454")) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw(legend.position = "top")
  plot$plotObject <- p
}

.mlBoostingPlotRelInf <- function(options, jaspResults, ready, position, purpose) {
  if (!is.null(jaspResults[["plotRelInf"]]) || !options[["plotRelInf"]]) {
    return()
  }
  plot <- createJaspPlot(plot = NULL, title = gettext("Relative Influence Plot"), width = 450, height = 300)
  plot$position <- position
  plot$dependOn(options = c(
    "plotRelInf", "target", "predictors", "modelOpt", "maxTrees", "intDepth", "shrinkage",
    "noOfTrees", "bagFrac", "noOfPredictors", "numberOfPredictors", "seed", "seedBox", "modelValid",
    "nNode", "distance", "testSetIndicatorVariable", "testSetIndicator", "validationDataManual",
    "holdoutData", "testDataManual"
  ))
  jaspResults[["plotRelInf"]] <- plot
  if (!ready) {
    return()
  }
  result <- switch(purpose,
    "classification" = jaspResults[["classificationResult"]]$object,
    "regression" = jaspResults[["regressionResult"]]$object
  )
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(0, result[["relInf"]]$rel.inf))
  p <- ggplot2::ggplot(result[["relInf"]], ggplot2::aes(x = reorder(as.factor(var), rel.inf), y = rel.inf)) +
    ggplot2::geom_bar(stat = "identity", fill = "gray", col = "black") +
    ggplot2::scale_y_continuous(name = gettext("Relative Influence"), breaks = yBreaks, limits = c(0, max(yBreaks))) +
    ggplot2::scale_x_discrete(name = NULL) +
    ggplot2::coord_flip() +
    jaspGraphs::geom_rangeframe(sides = "b") +
    jaspGraphs::themeJaspRaw() +
    ggplot2::theme(axis.ticks.y = ggplot2::element_blank(), axis.text.y = ggplot2::element_text(size = 12))
  plot$plotObject <- p
}

# identical to gbm::gbmCrossValModelBuild except it isn't parallel and so doesn't trigger a firewall message
fakeGbmCrossValModelBuild <- function(cv.folds, cv.group, n.cores, i.train, x, y, offset,
                                      distribution, w, var.monotone, n.trees, interaction.depth,
                                      n.minobsinnode, shrinkage, bag.fraction, var.names, response.name,
                                      group) {
  # the first two lines create a parallel backend and trigger a firewall message
  # cluster <- gbmCluster(n.cores)
  # on.exit(parallel::stopCluster(cluster))
  seeds <- as.integer(runif(cv.folds, -(2^31 - 1), 2^31))
  # parallel::parLapply(cl = NULL, X = 1:cv.folds, fun = gbmDoFold,
  #                     i.train, x, y, offset, distribution, w, var.monotone,
  #                     n.trees, interaction.depth, n.minobsinnode, shrinkage,
  #                     bag.fraction, cv.group, var.names, response.name, group,
  #                     seeds)
  # NOTE: gbm::gbmDoFold calls library(gbm, silent = TRUE) so we make another fake function
  fakeGbmDoFold <- function(X, i.train, x, y, offset, distribution, w, var.monotone,
                            n.trees, interaction.depth, n.minobsinnode, shrinkage, bag.fraction,
                            cv.group, var.names, response.name, group, s) {
    set.seed(s[[X]])
    i <- order(cv.group == X)
    x <- x[i.train, , drop = TRUE][i, , drop = FALSE]
    y <- y[i.train][i]
    offset <- offset[i.train][i]
    nTrain <- length(which(cv.group != X))
    group <- group[i.train][i]
    res <- gbm::gbm.fit(
      x = x, y = y, offset = offset, distribution = distribution,
      w = w, var.monotone = var.monotone, n.trees = n.trees,
      interaction.depth = interaction.depth, n.minobsinnode = n.minobsinnode,
      shrinkage = shrinkage, bag.fraction = bag.fraction, nTrain = nTrain,
      keep.data = FALSE, verbose = FALSE, response.name = response.name,
      group = group
    )
    res
  }
  lapply(
    X = 1:cv.folds, FUN = fakeGbmDoFold, i.train, x, y, offset, distribution, w, var.monotone, n.trees,
    interaction.depth, n.minobsinnode, shrinkage, bag.fraction, cv.group, var.names, response.name, group, seeds
  )
}

# identical to gbm::gbmCrossValErr except it uses `rowSums(as.matrix(cv.error))/nTrain` instead of `rowSums(cv.error)/nTrain` (presumably also to deal with the lack of parallelness)
fakeGbmCrossValErr <- function(cv.models, cv.folds, cv.group, nTrain, n.trees) {
  in.group <- tabulate(cv.group, nbins = cv.folds)
  cv.error <- vapply(1:cv.folds, function(index) {
    model <- cv.models[[index]]
    model$valid.error * in.group[[index]]
  }, double(n.trees))
  return(rowSums(as.matrix(cv.error)) / nTrain)
}
