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

mlRegressionNeuralNetwork <- function(jaspResults, dataset, options, ...) {

  # Preparatory work
  dataset <- .readDataRegressionAnalyses(dataset, options)
  .mlRegressionErrorHandling(dataset, options, type = "neuralnet")

  # Check if analysis is ready to run
  ready <- .mlRegressionReady(options, type = "neuralnet")

  # Determine activation and loss function for the neural net
  .mlNeuralNetworkActFunction(options, jaspResults)
  # .mlNeuralNetworkLossFunction(options, jaspResults)

  # Compute results and create the model summary table
  .mlRegressionTableSummary(dataset, options, jaspResults, ready, position = 1, type = "neuralnet")

  # If the user wants to add the values to the data set
  .mlRegressionAddPredictionsToData(dataset, options, jaspResults, ready)

  # Add test set indicator to data
  .mlAddTestIndicatorToData(options, jaspResults, ready, purpose = "regression")

  # Create the data split plot
  .mlPlotDataSplit(dataset, options, jaspResults, ready, position = 2, purpose = "regression", type = "neuralnet")

  # Create the evaluation metrics table
  .mlRegressionTableMetrics(dataset, options, jaspResults, ready, position = 3)

  # Create the network weights table
  .mlNeuralNetworkTableWeights(dataset, options, jaspResults, ready, purpose = "regression", position = 4)

  # Create the error plot
  .mlNeuralNetworkPlotError(dataset, options, jaspResults, ready, position = 5, purpose = "regression")

  # Create the predicted performance plot
  .mlRegressionPlotPredictedPerformance(options, jaspResults, ready, position = 6)

  # Create the activation function plot
  .mlNeuralNetworkPlotActivationFunction(options, jaspResults, position = 7)

  # Create the network graph
  .mlNeuralNetworkPlotStructure(dataset, options, jaspResults, ready, purpose = "regression", position = 8)
}

.getNeuralNetworkStructure <- function(options) {
  structure <- NULL
  for (i in 1:length(options[["layers"]])) {
    structure <- c(structure, options[["layers"]][[i]]$nodes)
  }
  return(structure) # Vector of number of nodes in each layer
}

.mlNeuralNetworkActFunction <- function(options, jaspResults) {
  # For an overview of these activation functions, see https://en.wikipedia.org/wiki/Activation_function
  if (options[["actfct"]] == "linear") {
    .ac <- function(x) x
  } else if (options[["actfct"]] == "binary") {
    .ac <- function(x) ifelse(x < 0, yes = 0, no = 1)
  } else if (options[["actfct"]] == "sigmoid") {
    .ac <- function(x) 1 / (1 + exp(1)^(-x))
  } else if (options[["actfct"]] == "sin") {
    .ac <- function(x) sin(x)
  } else if (options[["actfct"]] == "cosin") {
    .ac <- function(x) cos(x)
  } else if (options[["actfct"]] == "arctan") {
    .ac <- function(x) atan(x)
  } else if (options[["actfct"]] == "tanh") {
    .ac <- function(x) tanh(x)
  } else if (options[["actfct"]] == "relu") {
    .ac <- function(x) ifelse(x < 0, yes = 0, no = x)
  } else if (options[["actfct"]] == "softplus") {
    .ac <- function(x) log(1 + exp(1)^x)
  } else if (options[["actfct"]] == "softsign") {
    .ac <- function(x) x / (abs(x) + 1)
  } else if (options[["actfct"]] == "elu") {
    .ac <- function(x) ifelse(x <= 0, yes = exp(1)^x - 1, no = x)
  } else if (options[["actfct"]] == "lrelu") {
    .ac <- function(x) ifelse(x < 0, yes = 0.01 * x, no = x)
  } else if (options[["actfct"]] == "silu") {
    .ac <- function(x) x / (1 + exp(1)^(-x))
  } else if (options[["actfct"]] == "mish") {
    .ac <- function(x) x * tanh(log(1 + exp(1)^x))
  } else if (options[["actfct"]] == "gaussian") {
    .ac <- function(x) exp(1) * (-x^2)
  } else if (options[["actfct"]] == "gelu") {
    .ac <- function(x) 0.5 * x * (1 + tanh(sqrt(2 / pi) * (x + 0.044715 * x^3)))
  }
  jaspResults[["actfct"]] <- createJaspState(.ac)
}

.mlNeuralNetworkLossFunction <- function(options, jaspResults) {
  # For an overview of these loss functions, see https://machinelearningmastery.com/loss-and-loss-functions-for-training-deep-learning-neural-networks/
  if (options[["lossFunction"]] == "sumOfSquares") {
    .ec <- function(x, y) 1 / 2 * (y - x)^2
  } else if (options[["lossFunction"]] == "crossEntropy") {
    .ec <- function(x, y) -(y * log(x) + (1 - y) * log(1 - x))
  }
  jaspResults[["lossFunction"]] <- createJaspState(.ec)
}

.neuralnetRegression <- function(dataset, options, jaspResults) {
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
  trainingAndValidationSet <- dataset[trainingIndex, ]
  # Create the generated test set indicator
  testIndicatorColumn <- rep(1, nrow(dataset))
  testIndicatorColumn[trainingIndex] <- 0
  if (options[["modelOptimization"]] == "optimizationManual") {
    # Just create a train and a test set (no optimization)
    trainingSet <- trainingAndValidationSet
    testSet <- dataset[-trainingIndex, ]
    # Structure of neural network
    structure <- .getNeuralNetworkStructure(options)
    p <- try({
      trainingFit <- neuralnet::neuralnet(
        formula = formula,
        data = trainingSet,
        hidden = structure,
        learningrate = options[["learningRate"]],
        threshold = options[["threshold"]],
        stepmax = options[["maxTrainingRepetitions"]],
        rep = 1,
        startweights = NULL,
        algorithm = options[["algorithm"]],
        err.fct = "sse",
        act.fct = jaspResults[["actfct"]]$object,
        linear.output = if (options[["actfct"]] == "linear") TRUE else FALSE
      )
    })
    if (isTryError(p)) {
      jaspBase:::.quitAnalysis(gettextf("Analysis not possible: The algorithm did not converge within the maximum number of training repetitions (%1$s).", options[["maxTrainingRepetitions"]]))
    }
  } else if (options[["modelOptimization"]] == "optimizationError") {
    # Create a train, validation and test set (optimization)
    validationIndex <- sample.int(nrow(trainingAndValidationSet), size = ceiling(options[["validationDataManual"]] * nrow(trainingAndValidationSet)))
    testSet <- dataset[-trainingIndex, ]
    valid <- trainingAndValidationSet[validationIndex, ]
    trainingSet <- trainingAndValidationSet[-validationIndex, ]
    errorStore <- numeric(options[["maxGenerations"]])
    trainErrorStore <- numeric(options[["maxGenerations"]])
    # For plotting
    plot_x <- numeric()
    plot_y <- numeric()
    plot_type <- character()
    startProgressbar(options[["maxGenerations"]], gettext("Optimizing network topology"))
    # First generation
    population <- .mlNeuralNetworkOptimInit(options)
    # Fit and reproduce
    for (gen in 1:options[["maxGenerations"]]) {
      progressbarTick()
      fitness <- numeric(options[["populationSize"]])
      subTrainErrorStore <- numeric(options[["populationSize"]])
      for (i in 1:options[["populationSize"]]) {
        p <- try({
          trainingFit <- neuralnet::neuralnet(
            formula = formula,
            data = trainingSet,
            hidden = population[[i]]$structure,
            learningrate = options[["learningRate"]],
            threshold = options[["threshold"]],
            stepmax = options[["maxTrainingRepetitions"]],
            rep = 1,
            startweights = NULL,
            algorithm = options[["algorithm"]],
            err.fct = "sse",
            act.fct = jaspResults[["actfct"]]$object,
            linear.output = if (options[["actfct"]] == "linear") TRUE else FALSE
          )
          validationPredictions <- predict(trainingFit, newdata = valid)
        })
        if (isTryError(p)) {
          jaspBase:::.quitAnalysis(gettextf("Analysis not possible: The algorithm did not converge within the maximum number of training repetitions (%1$s).", options[["maxTrainingRepetitions"]]))
        }
        fitness[i] <- mean((validationPredictions - valid[, options[["target"]]])^2)
        trainingPredictions <- predict(trainingFit, newdata = trainingSet)
        subTrainErrorStore[i] <- mean((trainingPredictions - trainingSet[, options[["target"]]])^2)
        population[[i]][["fitness"]] <- 1 / fitness[i]
        population[[i]][["age"]] <- population[[i]][["age"]] + 1
        plot_x <- c(plot_x, gen, gen)
        plot_y <- c(plot_y, fitness[i], subTrainErrorStore[i])
        plot_type <- c(plot_type, "Validation set", "Training set")
      }
      # Find out best performance
      bestFitIndex <- order(fitness, decreasing = TRUE)[1]
      structure <- population[[bestFitIndex]]$structure # Best performing network structure
      # For plotting we store the mean MSE of the generation
      errorStore[gen] <- mean(fitness)
      trainErrorStore[gen] <- mean(subTrainErrorStore)
      # Stop when maximum generations is reached
      if (gen == options[["maxGenerations"]]) {
        break()
      }
      # Stage 1: Select parents for crossover (population of k = 20 will give n = k / 3 = 7 parent pairs)
      parents <- .mlNeuralNetworkOptimSelection(population, options)
      # Stage 2: Crossover of parents into children (n = 7 parent pairs will give m = n * 2 = 14 children)
      children <- .mlNeuralNetworkOptimCrossover(parents, options)
      # Stage 3: Mutation of offspring (m = 14 children will remain m = 14 children)
      children <- .mlNeuralNetworkOptimMutate(children, options)
      # Stage 4: Selection of survivors (k = 20 networks remain k = 20 networks)
      population <- .mlNeuralNetworkOptimSurvivors(population, children, options)
    }
    # Fit best network structure after optimization
    p <- try({
      trainingFit <- neuralnet::neuralnet(
        formula = formula,
        data = trainingSet,
        hidden = structure,
        learningrate = options[["learningRate"]],
        threshold = options[["threshold"]],
        stepmax = options[["maxTrainingRepetitions"]],
        rep = 1,
        startweights = NULL,
        algorithm = options[["algorithm"]],
        err.fct = "sse",
        act.fct = jaspResults[["actfct"]]$object,
        linear.output = if (options[["actfct"]] == "linear") TRUE else FALSE
      )
      validationPredictions <- predict(trainingFit, newdata = valid)
    })
    if (isTryError(p)) {
      jaspBase:::.quitAnalysis(gettextf("Analysis not possible: The algorithm did not converge within the maximum number of training repetitions (%1$s).", options[["maxTrainingRepetitions"]]))
    }
  }
  p <- try({
    # Use the specified model to make predictions for dataset
    dataPredictions <- predict(trainingFit, newdata = dataset)
    testPredictions <- predict(trainingFit, newdata = testSet)
  })
  if (isTryError(p)) {
    jaspBase:::.quitAnalysis(gettextf("Analysis not possible: The algorithm did not converge within the maximum number of training repetitions (%1$s).", options[["maxTrainingRepetitions"]]))
  }
  # Create results object
  result <- list()
  result[["formula"]] <- formula
  result[["structure"]] <- structure
  result[["model"]] <- trainingFit
  result[["nLayers"]] <- length(structure)
  result[["nNodes"]] <- sum(structure)
  result[["testMSE"]] <- mean((testPredictions - testSet[, options[["target"]]])^2)
  result[["ntrain"]] <- nrow(trainingSet)
  result[["ntest"]] <- nrow(testSet)
  result[["testReal"]] <- testSet[, options[["target"]]]
  result[["testPred"]] <- testPredictions
  result[["train"]] <- trainingSet
  result[["test"]] <- testSet
  result[["testIndicatorColumn"]] <- testIndicatorColumn
  result[["values"]] <- dataPredictions
  if (options[["modelOptimization"]] != "optimizationManual") {
    result[["accuracyStore"]] <- errorStore
    result[["validMSE"]] <- mean((validationPredictions - valid[, options[["target"]]])^2)
    result[["nvalid"]] <- nrow(valid)
    result[["valid"]] <- valid
    result[["plotFrame"]] <- data.frame(x = plot_x, y = plot_y, type = plot_type)
    if (options[["modelValid"]] == "validationManual") {
      result[["trainAccuracyStore"]] <- trainErrorStore
    }
  }
  return(result)
}

.mlNeuralNetworkTableWeights <- function(dataset, options, jaspResults, ready, purpose, position) {
  if (!is.null(jaspResults[["coefficientsTable"]]) || !options[["coefficientsTable"]]) {
    return()
  }
  result <- switch(purpose,
    "classification" = jaspResults[["classificationResult"]]$object,
    "regression" = jaspResults[["regressionResult"]]$object
  )
  structure <- result[["structure"]]
  table <- createJaspTable(title = gettext("Network Weights"))
  table$position <- position
  table$dependOn(options = c(
    "coefficientsTable", "scaleVariables", "target", "predictors", "seed", "setSeed", "holdoutData", "testDataManual",
    "testSetIndicatorVariable", "testSetIndicator",
    "threshold", "algorithm", "learningRate", "lossFunction", "actfct", "layers", "maxTrainingRepetitions", "maxGenerations", "populationSize", "maxLayers", "maxNodes",
    "mutationRate", "elitism", "selectionMethod", "crossoverMethod", "mutationMethod", "survivalMethod", "elitismProportion", "candidates"
  ))
  table$addColumnInfo(name = "fromNode", title = gettext("Node"), type = "string")
  table$addColumnInfo(name = "fromLayer", title = gettext("Layer"), type = "string")
  table$addColumnInfo(name = "separator", title = "", type = "string")
  table$addColumnInfo(name = "toNode", title = gettext("Node"), type = "string")
  table$addColumnInfo(name = "toLayer", title = gettext("Layer"), type = "string")
  table$addColumnInfo(name = "value", title = gettext("Weight"), type = "number")
  weights <- switch(options[["actfct"]],
    "linear"   = gettext("linear"),
    "binary"   = gettext("binary step"),
    "sigmoid"  = gettext("logistic sigmoid"),
    "sin"      = gettext("sine"),
    "cosin"    = gettext("cosine"),
    "arctan"   = gettext("inverse tangent"),
    "tanh"     = gettext("hyperbolic tangent"),
    "relu"     = gettext("rectified linear unit (ReLU)"),
    "softplus" = gettext("softplus"),
    "softsign" = gettext("softsign"),
    "elu"      = gettext("exponential linear unit (ELU)"),
    "lrelu"    = gettext("leaky rectified linear unit (Leaky ReLU)"),
    "silu"     = gettext("sigmoid linear unit (SiLU)"),
    "mish"     = gettext("mish"),
    "gaussian" = gettext("gaussian"),
    "gelu"     = gettext("gaussian error linear unit (GeLU)")
  )
  table$addFootnote(gettextf("The weights are input for the %1$s activation function.", weights))
  jaspResults[["coefficientsTable"]] <- table
  if (!ready) {
    return()
  }
  result <- switch(purpose,
    "classification" = jaspResults[["classificationResult"]]$object,
    "regression" = jaspResults[["regressionResult"]]$object
  )
  model <- result[["model"]]
  weights <- model$result.matrix
  index <- which(rownames(weights) == "steps")
  weights <- weights[-(1:index), ]
  weightNames <- names(weights)
  nodeNames <- strsplit(weightNames, split = "[.]to[.]")
  startNode <- unlist(lapply(nodeNames, `[[`, 1))
  endNode <- unlist(lapply(nodeNames, `[[`, 2))
  layerNames <- strsplit(endNode, split = "layhid")
  endLayer <- as.numeric(unlist(lapply(layerNames, `[[`, 1)))
  endLayer[which(is.na(endLayer))] <- max(endLayer, na.rm = TRUE) + 1
  startLayer <- endLayer - 1
  for (i in 1:length(structure)) {
    startNode <- gsub(pattern = paste0(i, "layhid"), startNode, replacement = "Hidden ")
    endNode <- gsub(pattern = paste0(i, "layhid"), endNode, replacement = "Hidden ")
  }
  startLayer[which(startLayer == 0)] <- "input"
  startLayer[which(startNode == "Intercept")] <- NA
  endLayer[which(endLayer == max(endLayer))] <- "output"
  table[["fromNode"]] <- startNode
  table[["fromLayer"]] <- startLayer
  table[["separator"]] <- rep("\u2192", length(startNode))
  table[["toNode"]] <- endNode
  table[["toLayer"]] <- endLayer
  table[["value"]] <- weights
}

.mlNeuralNetworkPlotStructure <- function(dataset, options, jaspResults, ready, purpose, position) {
  if (!is.null(jaspResults[["networkGraph"]]) || !options[["networkGraph"]]) {
    return()
  }
  plot <- createJaspPlot(title = gettext("Network Structure Plot"), height = 500, width = 600)
  plot$position <- position
  plot$dependOn(options = c(
    "networkGraph", "target", "predictors", "layers", "modelOptimization",
    "maxTrainingRepetitions", "maxGenerations", "populationSize", "maxLayers", "maxNodes", "mutationRate", "elitism",
    "selectionMethod", "crossoverMethod", "mutationMethod", "survivalMethod", "elitismProportion", "candidates"
  ))
  jaspResults[["networkGraph"]] <- plot
  if (!ready) {
    return()
  }
  result <- switch(purpose,
    "classification" = jaspResults[["classificationResult"]]$object,
    "regression" = jaspResults[["regressionResult"]]$object
  )
  model <- result[["model"]]
  structure <- result[["structure"]]
  weights <- model$result.matrix
  index <- which(rownames(weights) == "steps")
  weights <- weights[-(1:index), ]
  weightNames <- names(weights)
  nodeNames <- strsplit(weightNames, split = "[.]to[.]")
  startNode <- unlist(lapply(nodeNames, `[[`, 1))
  endNode <- unlist(lapply(nodeNames, `[[`, 2))
  for (i in 1:length(structure)) {
    startNode <- gsub(pattern = paste0(i, "layhid"), startNode, replacement = paste0(i, "_Hidden "))
    endNode <- gsub(pattern = paste0(i, "layhid"), endNode, replacement = paste0(i, "_Hidden "))
  }
  allnames <- unique(c(startNode, endNode))
  adjacency_matrix <- matrix(NA, nrow = length(allnames), ncol = length(allnames))
  rownames(adjacency_matrix) <- allnames
  colnames(adjacency_matrix) <- allnames
  for (i in 1:length(startNode)) {
    rowI <- which(rownames(adjacency_matrix) == startNode[i])
    colI <- which(colnames(adjacency_matrix) == endNode[i])
    adjacency_matrix[rowI, colI] <- weights[i]
  }
  y_inc <- 1 / (length(structure) + 1)
  if (length(options[["predictors"]]) == 1) {
    x_pos <- c(-0.3, 0.5) # x-location intercept and predictors
  } else {
    x_pos <- c(-0.3, seq(0, 1, length.out = length(options[["predictors"]]))) # x-location intercept and predictors
  }
  y_pos <- c(0.6, rep(1.2, length(options[["predictors"]]))) # y-location intercept and predictors
  for (i in 1:length(structure)) {
    if (structure[i] == 1) {
      x_inc <- 0.5
    } else {
      x_inc <- 1 / (structure[i] + 1)
    }
    x_pos <- c(x_pos, x_inc * (1:structure[i]))
    y_pos <- c(y_pos, rep(1 - (y_inc * i), structure[i]))
  }
  if (purpose == "classification") {
    x_pos <- c(x_pos, seq(0, 1, length.out = length(unique(dataset[, options[["target"]]]))))
    y_pos <- c(y_pos, rep(-0.2, length(unique(dataset[, options[["target"]]]))))
  } else {
    x_pos <- c(x_pos, 0.5)
    y_pos <- c(y_pos, -0.2)
  }
  posMat <- matrix(c(x_pos, y_pos), ncol = 2)
  groups <- list()
  groups[["Intercept"]] <- 1
  groups[["Predictors"]] <- 2:(length(options[["predictors"]]) + 1)
  for (i in 1:length(structure)) {
    groups[[paste0("Layer ", i)]] <- (max(groups[[length(groups)]]) + 1):(max(groups[[length(groups)]]) + structure[i])
  }
  groups[["Target"]] <- (max(groups[[length(groups)]]) + 1):nrow(adjacency_matrix)
  # Code below is for ggplot network
  adjacency_matrix[which(is.na(adjacency_matrix))] <- 0
  net <- network::network(adjacency_matrix, directed = TRUE)
  plotNet <- ggnetwork::ggnetwork(net, layout = posMat, arrow.gap = 0.025)
  # Shape of the nodes
  shape <- c(24, rep(21, length(options[["predictors"]]) + sum(structure)))
  shape[which(unique(plotNet[["vertex.names"]]) %in% options[["predictors"]])] <- 22
  # Color of the nodes
  color <- c("#E69F00", rep("#56B4E9", length(options[["predictors"]]) + sum(structure)))
  color[which(unique(plotNet[["vertex.names"]]) %in% options[["predictors"]])] <- "#D55E00"
  if (purpose == "classification") {
    shape <- c(shape, rep(22, length(unique(dataset[, options[["target"]]]))))
    color <- c(color, rep("#009E73", length(unique(dataset[, options[["target"]]]))))
  } else {
    shape <- c(shape, 22)
    color <- c(color, "#009E73")
  }
  # y position of hidden layer text
  layer_yPos <- sort(unique(plotNet[grepl("Hidden", plotNet[["vertex.names"]], fixed = TRUE), ][["y"]]))
  p <- ggplot2::ggplot(plotNet, ggplot2::aes(x = x, y = y, xend = xend, yend = yend)) +
    ggnetwork::geom_edges(color = "darkgray", size = 0.2, arrow = ggplot2::arrow(length = ggplot2::unit(3, "pt"), type = "closed")) +
    ggnetwork::geom_nodes(size = 7, shape = shape, fill = color, stroke = 1) +
    ggplot2::xlab(NULL) +
    ggplot2::xlim(c(-0.1, 1.25)) +
    ggplot2::ylab(NULL) +
    ggplot2::ylim(c(0, 1)) +
    ggplot2::annotate("text",
      x = c(-0.10, 1.15, 1.15, rep(1.15, length(layer_yPos))), y = c(0.575, 1, 0, layer_yPos), size = 5,
      label = c(gettext("Intercept"), gettext("Input layer"), gettext("Output layer"), gettextf("Hidden layer %1$s", length(structure):1))
    ) +
    jaspGraphs::geom_rangeframe(sides = "") +
    jaspGraphs::themeJaspRaw() +
    ggplot2::theme(axis.ticks = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank(), axis.text.y = ggplot2::element_blank())
  plot$plotObject <- p
}

.mlNeuralNetworkPlotActivationFunction <- function(options, jaspResults, position) {
  if (!is.null(jaspResults[["activationFunctionPlot"]]) || !options[["activationFunctionPlot"]]) {
    return()
  }
  weights <- switch(options[["actfct"]],
    "linear"   = gettext("Linear"),
    "binary"   = gettext("Binary"),
    "sigmoid"  = gettext("Logistic Sigmoid"),
    "sin"      = gettext("Sine"),
    "cosin"    = gettext("Cosine"),
    "arctan"   = gettext("Inverse Tangent"),
    "tanh"     = gettext("Hyperbolic Tangent"),
    "relu"     = gettext("ReLU"),
    "softplus" = gettext("Softplus"),
    "softsign" = gettext("Softsign"),
    "elu"      = gettext("ELU"),
    "lrelu"    = gettext("Leaky ReLU"),
    "silu"     = gettext("SiLU"),
    "mish"     = gettext("Mish"),
    "gaussian" = gettext("Gaussian"),
    "gelu"     = gettext("GeLU")
  )
  plot <- createJaspPlot(title = gettextf("%1$s Activation Function", weights), width = 400, height = 300)
  plot$position <- position
  plot$dependOn(options = c("activationFunctionPlot", "actfct"))
  jaspResults[["activationFunctionPlot"]] <- plot
  ac <- jaspResults[["actfct"]]$object
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(-6, 6), min.n = 4)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(-1, 1), min.n = 4)
  p <- ggplot2::ggplot() +
    ggplot2::stat_function(fun = ac, size = 1) +
    ggplot2::scale_x_continuous(name = gettext("Input"), breaks = xBreaks, limits = c(-6, 6)) +
    ggplot2::scale_y_continuous(name = gettext("Output"), breaks = yBreaks, limits = c(-1, 1)) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()
  plot$plotObject <- p
}

.mlNeuralNetworkOptimInit <- function(options) {
  # This function returns a population of random network structures
  # with a maximum number of hidden layers and maximum number of hidden nodes
  population <- list()
  for (i in 1:options[["populationSize"]]) {
    member <- list()
    member[["structure"]] <- sample.int(
      n = options[["maxNodes"]],
      size = sample.int(options[["maxLayers"]], size = 1),
      replace = TRUE
    )
    member[["age"]] <- 0
    member[["fitness"]] <- NA
    population[[i]] <- member
  }
  return(population)
}

.mlNeuralNetworkOptimSelection <- function(population, options) {
  # This function takes a list of neural network objects and returns m sets of parent
  # networks for crossover according to the selection method
  parents <- list()
  required_sets <- ceiling(length(population) / 3) # E.g., Population of 20 gives 7 networks
  fitness <- as.numeric(unlist(population)[which(names(unlist(population)) == "fitness")])
  for (i in 1:required_sets) {
    if (options[["selectionMethod"]] == "roulette") {
      parent1 <- sample.int(n = length(population), size = 1, prob = fitness)
      parent2 <- sample(x = (1:length(population))[-parent1], size = 1, prob = fitness[-parent1])
    } else if (options[["selectionMethod"]] == "universal") {
      tmp_parents <- sample.int(n = length(population), size = 2, prob = fitness, replace = FALSE)
      parent1 <- tmp_parents[1]
      parent2 <- tmp_parents[2]
    } else if (options[["selectionMethod"]] == "rank") {
      ranks <- order(fitness)
      parent1 <- sample.int(n = length(population), size = 1, prob = 1 / ranks)
      parent2 <- sample(x = (1:length(population))[-parent1], size = 1, prob = 1 / ranks[-parent1])
    } else if (options[["selectionMethod"]] == "random") {
      parent1 <- sample.int(n = length(population), size = 1, replace = TRUE)
      parent2 <- sample(x = (1:length(population))[-parent1], size = 1, replace = TRUE)
    } else if (options[["selectionMethod"]] == "tournament") {
      candidates <- sample.int(n = length(population), size = options[["candidates"]], replace = FALSE)
      parent1 <- which(order(fitness[candidates]) == 1)
      candidates <- sample(x = (1:length(population))[-parent1], size = options[["candidates"]], replace = FALSE)
      parent2 <- which(order(fitness[-parent1][candidates]) == 1)
    }
    parents[[i]] <- population[c(parent1, parent2)]
  }
  return(parents)
}

.mlNeuralNetworkOptimCrossover <- function(parents, options) {
  # This function takes a list containing sets of parent networks
  # and crosses over their chromosomes (structure) according to the crossover method.
  children <- list()
  for (i in 1:length(parents)) {
    parent1 <- parents[[i]][[1]]$structure
    parent2 <- parents[[i]][[2]]$structure
    child1 <- list()
    child2 <- list()
    if (options[["crossoverMethod"]] == "uniform") {
      parent1 <- c(parent1, rep(0, options[["maxLayers"]] - length(parent1)))
      parent2 <- c(parent2, rep(0, options[["maxLayers"]] - length(parent2)))
      child1[["structure"]] <- numeric(options[["maxLayers"]])
      child2[["structure"]] <- numeric(options[["maxLayers"]])
      for (j in 1:options[["maxLayers"]]) {
        child1[["structure"]][j] <- if (j %% 2 == 0) parent1[j] else parent2[j]
        child2[["structure"]][j] <- if (j %% 2 == 0) parent2[j] else parent1[j]
      }
      child1[["structure"]] <- child1[["structure"]][which(child1[["structure"]] != 0)]
      child2[["structure"]] <- child2[["structure"]][which(child2[["structure"]] != 0)]
    } else if (options[["crossoverMethod"]] == "onepoint") {
      fromParent1 <- parent1[1:(ceiling(length(parent1) / 2))]
      fromParent2 <- parent2[1:(ceiling(length(parent2) / 2))]
      child1[["structure"]] <- c(fromParent1, fromParent2)
      child2[["structure"]] <- c(fromParent2, fromParent1)
    } else if (options[["crossoverMethod"]] == "multipoint") {

    }
    child1[["age"]] <- 0
    child2[["age"]] <- 0
    child1[["fitness"]] <- NA
    child2[["fitness"]] <- NA
    children[[length(children) + 1]] <- child1
    children[[length(children) + 1]] <- child2
  }
  return(children)
}

.mlNeuralNetworkOptimMutate <- function(children, options) {
  # This function takes a list of child network structures
  # and mutates the number of nodes in their layers according to the mutation probability
  for (i in 1:length(children)) {
    child <- children[[i]]$structure
    mutate <- sample(0:1, size = 1, prob = c(1 - options[["mutationRate"]], options[["mutationRate"]]))
    if (mutate) {
      if (options[["mutationMethod"]] == "random") {
        location <- sample.int(n = length(child), size = 1)
        child[location] <- sample.int(n = options[["maxNodes"]], size = 1)
      } else if (options[["mutationMethod"]] == "swap") {
        locations <- sample.int(n = length(child), size = min(length(child), 2), replace = FALSE)
        child[locations] <- rev(child[locations])
      } else if (options[["mutationMethod"]] == "scramble") {
        locations <- sample.int(n = length(child), size = min(length(child), 2), replace = FALSE)
        child[locations[1]:locations[2]] <- sample(child[locations[1]:locations[2]])
      } else if (options[["mutationMethod"]] == "inversion") {
        locations <- sample.int(n = length(child), size = min(length(child), 2), replace = FALSE)
        child[locations[1]:locations[2]] <- rev(child[locations[1]:locations[2]])
      }
    }
    children[[i]]$structure <- child
  }
  return(children)
}

.mlNeuralNetworkOptimSurvivors <- function(population, children, options) {
  # This function takes a list of neural network structures and a vector of their fitness
  # and returns the networks with the higest fitness as elites.
  fitness <- unlist(population)[which(names(unlist(population)) == "fitness")]
  # Elitism
  elites <- NULL
  if (options[["elitism"]]) {
    eliteIndex <- order(-fitness)[1:ceiling(options[["populationSize"]] * options[["elitismProportion"]])]
    elites <- population[eliteIndex]
  }
  # Further replacements
  required_nets <- options[["populationSize"]] - length(elites) - length(children)
  population_without_elites <- population[-eliteIndex]
  if (options[["survivalMethod"]] == "age") {
    age <- unlist(population_without_elites)[which(names(unlist(population_without_elites)) == "age")]
    surviving_nets <- population_without_elites[order(age)[1:required_nets]]
  } else if (options[["survivalMethod"]] == "fitness") {
    fitness <- unlist(population_without_elites)[which(names(unlist(population_without_elites)) == "fitness")]
    surviving_nets <- population_without_elites[order(-fitness)][1:required_nets]
  } else if (options[["survivalMethod"]] == "random") {
    surviving_nets <- population_without_elites[sample.int(n = length(population_without_elites), size = required_nets, replace = FALSE)]
  }
  population <- c(elites, surviving_nets, children)
  return(population)
}

.mlNeuralNetworkPlotError <- function(dataset, options, jaspResults, ready, position, purpose) {
  if (!is.null(jaspResults[["meanSquaredErrorPlot"]]) || !options[["meanSquaredErrorPlot"]] || options[["modelOptimization"]] == "optimizationManual") {
    return()
  }
  plotTitle <- switch(purpose,
    "classification" = gettext("Classification Accuracy Plot"),
    "regression" = gettext("Mean Squared Error Plot")
  )
  plot <- createJaspPlot(plot = NULL, title = plotTitle, width = 400, height = 300)
  plot$position <- position
  plot$dependOn(options = c(
    "meanSquaredErrorPlot", "scaleVariables", "target", "predictors", "seed", "setSeed", "holdoutData", "testDataManual", "validationDataManual",
    "testSetIndicatorVariable", "testSetIndicator", "modelOptimization",
    "threshold", "algorithm", "learningRate", "lossFunction", "actfct", "layers", "maxTrainingRepetitions", "maxGenerations", "populationSize", "maxLayers", "maxNodes", "mutationRate", "elitism", "selectionMethod", "crossoverMethod", "mutationMethod", "survivalMethod", "elitismProportion", "candidates"
  ))
  jaspResults[["meanSquaredErrorPlot"]] <- plot
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
  plotData <- result[["plotFrame"]]
  plotData$x <- as.numeric(plotData$x)
  plotData$y <- as.numeric(plotData$y)
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(0, plotData$x), min.n = 4)
  ylm1 <- loess(y ~ x, data = plotData[which(plotData$type == "Training set"), ])
  ylm2 <- loess(y ~ x, data = plotData[which(plotData$type == "Validation set"), ])
  pred1 <- predict(ylm1, newdata = plotData[which(plotData$type == "Training set"), ], se = TRUE)
  pred2 <- predict(ylm2, newdata = plotData[which(plotData$type == "Validation set"), ], se = TRUE)
  lwr <- c(pred1$fit - 2.5 * pred1$se.fit, pred2$fit - 2.5 * pred2$se.fit)
  upr <- c(pred1$fit + 2.5 * pred1$se.fit, pred2$fit + 2.5 * pred2$se.fit)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(lwr, upr), min.n = 4)
  p <- ggplot2::ggplot(data = plotData, ggplot2::aes(x = x, y = y, linetype = type, fill = type)) +
    ggplot2::geom_smooth(method = "loess", color = "black", se = TRUE, show.legend = FALSE) +
    ggplot2::geom_smooth(method = "loess", color = "black", se = FALSE) +
    ggplot2::scale_x_continuous(name = gettext("Generation"), breaks = xBreaks, limits = c(0, max(xBreaks))) +
    ggplot2::scale_y_continuous(name = ylabel, breaks = yBreaks, limits = range(yBreaks)) +
    ggplot2::labs(linetype = "") +
    ggplot2::scale_linetype_manual(values = c(2, 1)) +
    ggplot2::scale_fill_manual(values = c("lightgray", "darkgray"), guide = "none") +
    ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(fill = NA))) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw(legend.position = "top")
  plot$plotObject <- p
}
