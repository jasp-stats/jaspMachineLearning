#
# Copyright (C) 2017 University of Amsterdam
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
  
  # Create the predicted performance plot
  .regressionPredictedPerformancePlot(options, jaspResults, ready, position = 5)
  
  # Create the activation function plot
  .neuralNetworkActivationFunctionPlot(options, jaspResults, position = 6)
  
  # Create the network graph
  .neuralNetworkGraph(dataset, options, jaspResults, ready, purpose = "regression", position = 7)
  
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
  
  # Use the specified model to make predictions for dataset
  predictions <- predict(nfit_test, newdata = dataset)
  testPredictions <- predict(nfit_test, newdata = test)
  
  # Create results object
  regressionResult <- list()
  
  regressionResult[["formula"]]             <- formula
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
  
  return(regressionResult)
}

.neuralNetworkTable <- function(dataset, options, jaspResults, ready, purpose, position) {
  
  if (!is.null(jaspResults[["coefficientsTable"]]) || !options[["coefficientsTable"]]) return()
  
  structure <- .getNeuralNetworkStructure(options)
  
  table <- createJaspTable(title = gettext("Network Weights"))
  table$position <- position
  table$dependOn(options = c("coefficientsTable", "scaleEqualSD", "target", "predictors", "seed", "seedBox", "holdoutData", "testDataManual",
                             "testSetIndicatorVariable", "testSetIndicator", 
                             "threshold", "algorithm", "learningRate", "errfct", "actfct", "layers", "stepMax"))
  
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
  
  structure <- .getNeuralNetworkStructure(options)
  
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
  plot$dependOn(options = c("networkGraph", "target", "predictors", "layers"))
  jaspResults[["networkGraph"]] <- plot
  
  if (!ready)  return()
  
  structure <- .getNeuralNetworkStructure(options)
  
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
  x_pos <- c(-0.3, seq(0, 1, length.out = length(options[["predictors"]]))) # x-location intercept and predictors
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