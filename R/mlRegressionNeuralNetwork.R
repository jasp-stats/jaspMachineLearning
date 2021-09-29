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
  .errorHandlingRegressionAnalyses(dataset, options, type = "neuralnet")
  
  # Check if analysis is ready to run
  ready <- .regressionAnalysesReady(options, type = "neuralnet")
  
  # Determine activation and loss function for the neural net
  .getNeuralNetworkActFunction(options, jaspResults)
  # .getNeuralNetworkLossFunction(options, jaspResults)
  
  # Compute results and create the model summary table
  .regressionMachineLearningTable(dataset, options, jaspResults, ready, position = 1, type = "neuralnet")
  
  # If the user wants to add the values to the data set
  .regressionAddValuesToData(dataset, options, jaspResults, ready)
  
  # Add test set indicator to data
  .addTestIndicatorToData(options, jaspResults, ready, purpose = "regression")
  
  # Create the data split plot
  .dataSplitPlot(dataset, options, jaspResults, ready, position = 2, purpose = "regression", type = "neuralnet")
  
  # Create the evaluation metrics table
  .regressionEvaluationMetrics(dataset, options, jaspResults, ready, position = 3)
  
  # Create the network weights table
  .neuralNetworkTable(dataset, options, jaspResults, ready, purpose = "regression", position = 4)
  
  # Create the error plot
  .nnErrorPlot(dataset, options, jaspResults, ready, position = 5, purpose = "regression")
  
  # Create the predicted performance plot
  .regressionPredictedPerformancePlot(options, jaspResults, ready, position = 6)
  
  # Create the activation function plot
  .neuralNetworkActivationFunctionPlot(options, jaspResults, position = 7)
  
  # Create the network graph
  .neuralNetworkGraph(dataset, options, jaspResults, ready, purpose = "regression", position = 8)
  
}

.getNeuralNetworkStructure <- function(options) {
  structure <- NULL
  for (i in 1:length(options[["layers"]]))
    structure <- c(structure, options[["layers"]][[i]]$nodes)
  return(structure) # Vector of number of nodes in each layer
}

.getNeuralNetworkActFunction <- function(options, jaspResults) {
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
    .ac <- function(x) exp(1)*(-x^2)
  } else if (options[["actfct"]] == "gelu") {
    .ac <- function(x)  0.5 * x * (1 + tanh(sqrt(2/pi) * (x + 0.044715 * x^3)))
  }
  jaspResults[["actfct"]] <- createJaspState(.ac)
}

.getNeuralNetworkLossFunction <- function(options, jaspResults) {
  # For an overview of these loss functions, see https://machinelearningmastery.com/loss-and-loss-functions-for-training-deep-learning-neural-networks/
  if (options[["errfct"]] == "sse") {
    .ec <- function(x, y) 1/2 * (y - x)^2
  } else if (options[["errfct"]] == "ce") {
    .ec <- function(x, y) -(y * log(x) + (1 - y) * log(1 - x))
  }
  jaspResults[["errfct"]] <- createJaspState(.ec)
}

.neuralnetRegression <- function(dataset, options, jaspResults) {
  
  # Import model formula from jaspResults
  formula <- jaspResults[["formula"]]$object
  
  # Split the data into training and test sets
  if (options[["holdoutData"]] == "testSetIndicator" && options[["testSetIndicatorVariable"]] != "") {
    # Select observations according to a user-specified indicator (included when indicator = 1)
    train.index <- which(dataset[,options[["testSetIndicatorVariable"]]] == 0)
  } else {
    # Sample a percentage of the total data set
    train.index <- sample.int(nrow(dataset), size = ceiling( (1 - options[['testDataManual']]) * nrow(dataset)))
  }
  trainAndValid <- dataset[train.index, ]
  
  # Create the generated test set indicator
  testIndicatorColumn <- rep(1, nrow(dataset))
  testIndicatorColumn[train.index] <- 0
  
  if (options[["modelOpt"]] == "optimizationManual") {
    # Just create a train and a test set (no optimization)
    train <- trainAndValid
    test <- dataset[-train.index, ]
    
    # Structure of neural network
    structure <- .getNeuralNetworkStructure(options)
    
    p <- try({
      nfit_test  <- neuralnet::neuralnet(formula = formula, 
                                         data = train, 
                                         hidden = structure,
                                         learningrate = options[["learningRate"]],
                                         threshold = options[["threshold"]],
                                         stepmax = options[["stepMax"]],
                                         rep = 1,
                                         startweights = NULL,
                                         algorithm = options[["algorithm"]],
                                         err.fct = "sse",# jaspResults[["errfct"]]$object,
                                         act.fct = jaspResults[["actfct"]]$object, 
                                         linear.output = if (options[["actfct"]] == "linear") TRUE else FALSE)
    })
    if (isTryError(p)) # kind off harsh right now
      jaspBase:::.quitAnalysis(gettext("The network did not converge."))
    
  } else if (options[["modelOpt"]] == "optimizationError") {
    # Genetic optimization of network topology
    
    # Create a train, validation and test set (optimization)
    valid.index             <- sample.int(nrow(trainAndValid), size = ceiling(options[['validationDataManual']] * nrow(trainAndValid)))
    test                    <- dataset[-train.index, ]
    valid                   <- trainAndValid[valid.index, ]
    train                   <- trainAndValid[-valid.index, ]
    
    errorStore           <- numeric(options[["maxGen"]])
    trainErrorStore      <- numeric(options[["maxGen"]])
    
    # For plotting
    plot_x            <- numeric()
    plot_y            <- numeric()
    plot_type         <- character()
    
    startProgressbar(options[["maxGen"]], gettext("Optimizing network topology"))
    
    # First generation
    population <- .neuralNetworkOptim_init(options)
    
    # Fit and reproduce
    for (gen in 1:options[["maxGen"]]) {
      
      progressbarTick()
      
      fitness             <- numeric(options[["genSize"]])
      tmp_trainErrorStore <- numeric(options[["genSize"]])
      
      for (i in 1:options[["genSize"]]) {
        
        p <- try({
          nfit_valid  <- neuralnet::neuralnet(formula = formula, 
                                              data = train, 
                                              hidden = population[[i]]$structure,
                                              learningrate = options[["learningRate"]],
                                              threshold = options[["threshold"]],
                                              stepmax = options[["stepMax"]],
                                              rep = 1,
                                              startweights = NULL,
                                              algorithm = options[["algorithm"]],
                                              err.fct = "sse",# jaspResults[["errfct"]]$object,
                                              act.fct = jaspResults[["actfct"]]$object, 
                                              linear.output = if (options[["actfct"]] == "linear") TRUE else FALSE)
        })
        
        validPredictions <- predict(nfit_valid, newdata = valid)
        fitness[i] <- mean( (validPredictions - valid[, options[["target"]]])^2 )
        trainPredictions <- predict(nfit_valid, newdata = train)
        tmp_trainErrorStore[i] <- mean( (trainPredictions - train[, options[["target"]]])^2 )
        
        population[[i]][["fitness"]] <- 1 / fitness[i]
        population[[i]][["age"]] <- population[[i]][["age"]] + 1
        
        plot_x <- c(plot_x, gen, gen)
        plot_y <- c(plot_y, fitness[i], tmp_trainErrorStore[i])
        plot_type <- c(plot_type, "Validation", "Training")  
      }
      
      # Find out best performance
      bestFitIndex <- order(fitness, decreasing = TRUE)[1]
      structure <- population[[bestFitIndex]]$structure # Best performing network structure
      
      # For plotting we store the mean MSE of the generation
      errorStore[gen] <- mean(fitness)
      trainErrorStore[gen] <- mean(tmp_trainErrorStore)
      
      # Stop when maximum generations is reached
      if (gen == options[["maxGen"]])
        break()
      
      # Stage 1: Select parents for crossover (population of k = 20 will give n = k / 3 = 7 parent pairs)
      parents <- .neuralNetworkOptim_selection(population, options)
      
      # Stage 2: Crossover of parents into children (n = 7 parent pairs will give m = n * 2 = 14 children)
      children <- .neuralNetworkOptim_crossover(parents, options) 
      
      # Stage 3: Mutation of offspring (m = 14 children will remain m = 14 children)
      children <- .neuralNetworkOptim_mutate(children, options)
      
      # Stage 4: Selection of survivors (k = 20 networks remain k = 20 networks)
      population <- .neuralNetworkOptim_survivors(population, children, options)   
    }
    
    # Fit best network structure after optimization
    p <- try({
      nfit_test  <- neuralnet::neuralnet(formula = formula, 
                                         data = train, 
                                         hidden = structure,
                                         learningrate = options[["learningRate"]],
                                         threshold = options[["threshold"]],
                                         stepmax = options[["stepMax"]],
                                         rep = 1,
                                         startweights = NULL,
                                         algorithm = options[["algorithm"]],
                                         err.fct = "sse",# jaspResults[["errfct"]]$object,
                                         act.fct = jaspResults[["actfct"]]$object, 
                                         linear.output = if (options[["actfct"]] == "linear") TRUE else FALSE)
    })
    if (isTryError(p)) # kind off harsh right now
      jaspBase:::.quitAnalysis(gettext("The network did not converge."))
    
    validPredictions <- predict(nfit_test, newdata = valid)
    
  }
  
  # Use the specified model to make predictions for dataset
  predictions <- predict(nfit_test, newdata = dataset)
  testPredictions <- predict(nfit_test, newdata = test)
  
  # Create results object
  regressionResult <- list()
  
  regressionResult[["formula"]]             <- formula
  regressionResult[["structure"]]           <- structure
  regressionResult[["model"]]               <- nfit_test
  regressionResult[["nLayers"]]             <- length(structure)
  regressionResult[["nNodes"]]              <- sum(structure)
  regressionResult[['testMSE']]     	      <- mean( (testPredictions - test[, options[["target"]]])^2 )
  regressionResult[["ntrain"]]              <- nrow(train)
  regressionResult[["ntest"]]               <- nrow(test)
  regressionResult[["testReal"]]            <- test[, options[["target"]]]
  regressionResult[["testPred"]]            <- testPredictions
  regressionResult[["train"]]               <- train
  regressionResult[["test"]]                <- test
  regressionResult[["testIndicatorColumn"]] <- testIndicatorColumn
  regressionResult[["values"]]              <- predictions
  
  if (options[["modelOpt"]] != "optimizationManual") {
    regressionResult[["accuracyStore"]] <- errorStore
    regressionResult[['validMSE']]    <- mean( (validPredictions - valid[, options[["target"]]])^2 )
    regressionResult[["nvalid"]]      <- nrow(valid)
    regressionResult[["valid"]]       <- valid
    regressionResult[["plotFrame"]]   <- data.frame(x = plot_x, y = plot_y, type = plot_type)
    
    if (options[["modelValid"]] == "validationManual")
      regressionResult[["trainAccuracyStore"]] <- trainErrorStore
  }
  
  return(regressionResult)
}

.neuralNetworkTable <- function(dataset, options, jaspResults, ready, purpose, position) {
  
  if (!is.null(jaspResults[["coefficientsTable"]]) || !options[["coefficientsTable"]]) return()
  
  result <- switch(purpose, 
                   "classification" = jaspResults[["classificationResult"]]$object,
                   "regression" = jaspResults[["regressionResult"]]$object)
  
  structure <- result[["structure"]]
  
  table <- createJaspTable(title = gettext("Network Weights"))
  table$position <- position
  table$dependOn(options = c("coefficientsTable", "scaleEqualSD", "target", "predictors", "seed", "seedBox", "holdoutData", "testDataManual",
                             "testSetIndicatorVariable", "testSetIndicator", 
                             "threshold", "algorithm", "learningRate", "errfct", "actfct", "layers", "stepMax", "maxGen", "genSize", "maxLayers", "maxNodes", 
                             "mutationRate", "elitism", "selectionMethod", "crossoverMethod", "mutationMethod", "survivalMethod", "elitismProp", "candidates"))
  
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
                    "gelu"     = gettext("gaussian error linear unit (GeLU)"))
  table$addFootnote(gettextf("The weights are input for the %1$s activation function.", weights))
  
  jaspResults[["coefficientsTable"]] <- table
  
  if (!ready)  return()
  
  result <- switch(purpose, 
                   "classification" = jaspResults[["classificationResult"]]$object,
                   "regression" = jaspResults[["regressionResult"]]$object)
  
  model <- result[["model"]]
  weights <- model$result.matrix
  index <- which(rownames(weights) == "steps")
  weights <- weights[-(1:index),]
  
  weightNames <- names(weights)
  nodeNames <- strsplit(weightNames, split = "[.]to[.]")
  from_node <- unlist(lapply(nodeNames, `[[`, 1))
  to_node <- unlist(lapply(nodeNames, `[[`, 2))
  
  layerNames <- strsplit(to_node, split = "layhid")
  to_layer <- as.numeric(unlist(lapply(layerNames, `[[`, 1)))
  to_layer[which(is.na(to_layer))] <- max(to_layer, na.rm = TRUE) + 1
  from_layer <- to_layer - 1
  
  for (i in 1:length(structure)) {
    from_node <- gsub(pattern = paste0(i, "layhid"), from_node, replacement = "Hidden ")
    to_node <- gsub(pattern = paste0(i, "layhid"), to_node, replacement = "Hidden ")
  }
  
  from_layer[which(from_layer == 0)] <- "input"
  from_layer[which(from_node == "Intercept")] <- NA
  to_layer[which(to_layer == max(to_layer))] <- "output"
  
  table[["fromNode"]]  <- from_node
  table[["fromLayer"]] <- from_layer
  table[["separator"]] <- rep("\u2192", length(from_node))
  table[["toNode"]]    <- to_node
  table[["toLayer"]]   <- to_layer
  table[["value"]]     <- weights
  
}

.neuralNetworkGraph <- function(dataset, options, jaspResults, ready, purpose, position) {
  
  if (!is.null(jaspResults[["networkGraph"]]) || !options[["networkGraph"]]) return()
  
  plot <- createJaspPlot(title = gettext("Network Structure Plot"), height = 500, width = 600)
  plot$position <- position
  plot$dependOn(options = c("networkGraph", "target", "predictors", "layers", "modelOpt",
                            "stepMax", "maxGen", "genSize", "maxLayers", "maxNodes", "mutationRate", "elitism", 
                            "selectionMethod", "crossoverMethod", "mutationMethod", "survivalMethod", "elitismProp", "candidates"))
  jaspResults[["networkGraph"]] <- plot
  
  if (!ready)  return()
  
  result <- switch(purpose, 
                   "classification" = jaspResults[["classificationResult"]]$object,
                   "regression" = jaspResults[["regressionResult"]]$object)
  model <- result[["model"]]
  
  structure <- result[["structure"]]
  
  weights <- model$result.matrix
  index <- which(rownames(weights) == "steps")
  weights <- weights[-(1:index),]
  
  weightNames <- names(weights)
  nodeNames <- strsplit(weightNames, split = "[.]to[.]")
  from_node <- unlist(lapply(nodeNames, `[[`, 1))
  to_node <- unlist(lapply(nodeNames, `[[`, 2))
  
  for (i in 1:length(structure)) {
    from_node <- gsub(pattern = paste0(i, "layhid"), from_node, replacement = paste0(i, "_Hidden "))
    to_node <- gsub(pattern = paste0(i, "layhid"), to_node, replacement = paste0(i, "_Hidden "))
  }
  
  allnames <- unique(c(from_node, to_node))
  adjacency_matrix <- matrix(NA, nrow = length(allnames), ncol = length(allnames))
  rownames(adjacency_matrix) <- allnames
  colnames(adjacency_matrix) <- allnames
  
  for (i in 1:length(from_node)) {
    rowI <- which(rownames(adjacency_matrix) == from_node[i])
    colI <- which(colnames(adjacency_matrix) == to_node[i])
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
    ggplot2::annotate("text", x = c(-0.10, 1.15, 1.15, rep(1.15, length(layer_yPos))), y = c(0.575, 1, 0, layer_yPos), size = 5,
                      label = c(gettext("Intercept"), gettext("Input layer"), gettext("Output layer"), gettextf("Hidden layer %1$s", length(structure):1)))
  p <- jaspGraphs::themeJasp(p, sides = "") + 
    ggplot2::theme(axis.ticks = ggplot2::element_blank(), 
                   axis.text.x = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank())
  plot$plotObject <- p
  
  # Code below is for qgraph network
  #
  # icons <- c("triangle", rep("square", length(groups[["Predictors"]])), rep("circle", sum(structure)), rep("square", length(groups[["Target"]])))
  # plot$plotObject <- qgraph::qgraph(input = adjacency_matrix, 
  #                                   layout = posMat,
  #                                   DoNotPlot = TRUE, 
  #                                   groups = groups,
  #                                   nodeNames = rownames(adjacency_matrix),
  #                                   labels = rep("", nrow(adjacency_matrix)), #if (options[["plotNetworkLegend"]]) 1:length(icons) else rownames(adjacency_matrix),
  #                                   edge.labels = FALSE, #options[["plotEdgeLabels"]],
  #                                   edge.label.color = "black",
  #                                   edge.color = "darkgray",
  #                                   lty = 1,
  #                                   negDashed = FALSE,
  #                                   fade = FALSE,
  #                                   esize = 0,
  #                                   palette = 'pastel',
  #                                   usePCH = TRUE,
  #                                   shape = icons,
  #                                   legend = FALSE)#options[["plotNetworkLegend"]]) 
}

.neuralNetworkActivationFunctionPlot <- function(options, jaspResults, position) {
  
  if (!is.null(jaspResults[["actFuncPlot"]]) || !options[["actFuncPlot"]]) return()
  
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
                    "gelu"     = gettext("GeLU"))
  
  plot <- createJaspPlot(title = gettextf("%1$s Activation Function", weights), height = 300, width = 400)
  plot$position <- position
  plot$dependOn(options = c("actFuncPlot", "actfct"))
  jaspResults[["actFuncPlot"]] <- plot
  
  ac <- jaspResults[["actfct"]]$object
  
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(-6, 6), min.n = 4)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(-1, 1), min.n = 4)
  
  p <- ggplot2::ggplot() + 
    ggplot2::stat_function(fun = ac, size = 1) +
    ggplot2::scale_x_continuous(name = gettext("Input"), breaks = xBreaks, limits = c(-6, 6)) +
    ggplot2::scale_y_continuous(name = gettext("Output"), breaks = yBreaks, limits = c(-1, 1))
  p <- jaspGraphs::themeJasp(p)
  
  plot$plotObject <- p
  
}

.neuralNetworkOptim_init <- function(options) {
  # This function returns a population of random network structures
  # with a maximum number of hidden layers and maximum number of hidden nodes
  population <- list()
  for (i in 1:options[["genSize"]]) {
    member <- list()
    member[["structure"]] <- sample.int(n = options[["maxNodes"]], 
                                        size = sample.int(options[["maxLayers"]], size = 1), 
                                        replace = TRUE)
    member[["age"]] <- 0
    member[["fitness"]] <- NA
    population[[i]] <- member
  }
  return(population)
}

.neuralNetworkOptim_selection <- function(population, options) {
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
    } else if (options[["selectionMethod"]] == 'rank') {
      ranks <- order(fitness)
      parent1 <- sample.int(n = length(population), size = 1, prob = 1 / ranks)
      parent2 <- sample(x = (1:length(population))[-parent1], size = 1, prob = 1 / ranks[-parent1])
    } else if (options[["selectionMethod"]] == 'random') {
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

.neuralNetworkOptim_crossover <- function(parents, options) {
  # This function takes a list containing sets of parent networks
  # and crosses over their chromosomes (structure) according to the crossover method.
  children <- list()
  for (i in 1:length(parents)) {
    parent1 <- parents[[i]][[1]]$structure
    parent2 <- parents[[i]][[2]]$structure
    child1 <- list()
    child2 <- list()
    if(options[["crossoverMethod"]] == "uniform") {
      parent1 <- c(parent1, rep(0, options[["maxLayers"]] - length(parent1)))
      parent2 <- c(parent2, rep(0, options[["maxLayers"]] - length(parent2)))
      child1[["structure"]] <- numeric(options[["maxLayers"]])
      child2[["structure"]] <- numeric(options[["maxLayers"]])
      for (j in 1:options[["maxLayers"]]) {
        child1[["structure"]][j] <- if (j%%2 == 0) parent1[j] else parent2[j]
        child2[["structure"]][j] <- if (j%%2 == 0) parent2[j] else parent1[j]
      }
      child1[["structure"]] <- child1[["structure"]][which(child1[["structure"]] != 0)]
      child2[["structure"]] <- child2[["structure"]][which(child2[["structure"]] != 0)]
    } else if (options[["crossoverMethod"]] == "onepoint") {
      fromParent1 <- parent1[1:(ceiling(length(parent1)/2))]
      fromParent2 <- parent2[1:(ceiling(length(parent2)/2))]
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

.neuralNetworkOptim_mutate <- function(children, options) {
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

.neuralNetworkOptim_survivors <- function(population, children, options) {
  # This function takes a list of neural network structures and a vector of their fitness
  # and returns the networks with the higest fitness as elites.
  fitness <- unlist(population)[which(names(unlist(population)) == "fitness")]
  # Elitism
  elites <- NULL
  if (options[["elitism"]]) {
    eliteIndex <- order(-fitness)[1:ceiling(options[["genSize"]] * options[["elitismProp"]])]
    elites <- population[eliteIndex]
  }
  # Further replacements
  required_nets <- options[["genSize"]] - length(elites) - length(children)
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

.nnErrorPlot <- function(dataset, options, jaspResults, ready, position, purpose) {
  
  if (!is.null(jaspResults[["plotError"]]) || !options[["plotError"]] || options[["modelOpt"]] == "optimizationManual") return()
  
  plotTitle <- base::switch(purpose, "classification" = gettext("Classification Accuracy Plot"), "regression" = gettext("Mean Squared Error Plot"))
  
  plot <- createJaspPlot(plot = NULL, title = plotTitle, width = 500, height = 300)
  plot$position <- position
  plot$dependOn(options = c("plotError", "scaleEqualSD", "target", "predictors", "seed", "seedBox", "holdoutData", "testDataManual", "validationDataManual",
                            "testSetIndicatorVariable", "testSetIndicator", "modelOpt",
                            "threshold", "algorithm", "learningRate", "errfct", "actfct", "layers", "stepMax", "maxGen", "genSize", "maxLayers", "maxNodes", "mutationRate", "elitism", "selectionMethod", "crossoverMethod", "mutationMethod", "survivalMethod", "elitismProp", "candidates"))
  jaspResults[["plotError"]] <- plot
  
  if (!ready) return()
  
  result <- base::switch(purpose,
                         "classification" = jaspResults[["classificationResult"]]$object,
                         "regression" = jaspResults[["regressionResult"]]$object)
  
  ylabel <- base::switch(purpose,
                         "classification" = gettext("Classification Accuracy"),
                         "regression"     = gettext("Mean Squared Error"))
  
  d <- result[["plotFrame"]]
  d$x <- as.numeric(d$x)
  d$y <- as.numeric(d$y)
  
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(0, d$x), min.n = 4)
  ylm1 <- loess(y ~ x, data = d[which(d$type == "Training"),])
  ylm2 <- loess(y ~ x, data = d[which(d$type == "Validation"),])
  pred1 <- predict(ylm1, newdata = d[which(d$type == "Training"),], se = TRUE)
  pred2 <- predict(ylm2, newdata = d[which(d$type == "Validation"),], se = TRUE)
  lwr <- c(pred1$fit-2.5*pred1$se.fit, pred2$fit-2.5*pred2$se.fit)
  upr <- c(pred1$fit+2.5*pred1$se.fit, pred2$fit+2.5*pred2$se.fit)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(lwr, upr), min.n = 4)
  
  p <- ggplot2::ggplot(data = d, ggplot2::aes(x = x, y = y, linetype = type, fill = type)) +
    ggplot2::geom_smooth(method="loess", color = "black", se=TRUE, show.legend = FALSE) +
    ggplot2::geom_smooth(method="loess", color = "black", se=FALSE) +
    ggplot2::scale_x_continuous(name = gettext("Generation"), breaks = xBreaks, labels = xBreaks, limits = c(0, max(xBreaks))) +
    ggplot2::scale_y_continuous(name = ylabel, breaks = yBreaks, labels = yBreaks, limits = range(yBreaks)) +
    ggplot2::labs(linetype = "") +
    ggplot2::scale_linetype_manual(values = c(2,1)) +
    ggplot2::scale_fill_manual(values = c("#cccccc", "#cccccc"), guide = 'none') +
    ggplot2::guides(color=ggplot2::guide_legend(override.aes=list(fill=NA)))
  p <- jaspGraphs::themeJasp(p, legend.position = "top")
  
  plot$plotObject <- p
}