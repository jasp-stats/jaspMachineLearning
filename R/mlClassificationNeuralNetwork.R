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

mlClassificationNeuralNetwork <- function(jaspResults, dataset, options, ...) {
  
  # Preparatory work
  dataset <- .readDataClassificationAnalyses(dataset, options)
  .errorHandlingClassificationAnalyses(dataset, options, type = "neuralnet")
  
  # Check if analysis is ready to run
  ready <- .classificationAnalysesReady(options, type = "neuralnet")
  
  # Determine activation and loss function for the neural net
  .getNeuralNetworkActFunction(options, jaspResults)
  # .getNeuralNetworkLossFunction(options, jaspResults)
  
  # Compute results and create the model summary table
  .classificationTable(dataset, options, jaspResults, ready, position = 1, type = "neuralnet")
  
  # If the user wants to add the classes to the data set
  .classificationAddClassesToData(dataset, options, jaspResults, ready)
  
  # Add test set indicator to data
  .addTestIndicatorToData(options, jaspResults, ready, purpose = "classification")
  
  # Create the data split plot
  .dataSplitPlot(dataset, options, jaspResults, ready, position = 2, purpose = "classification", type = "neuralnet")
  
  # Create the confusion table
  .classificationConfusionTable(dataset, options, jaspResults, ready, position = 3)
  
  # Create the class proportions table
  .classificationClassProportions(dataset, options, jaspResults, ready, position = 4)
  
  # Create the validation measures table
  .classificationEvaluationMetrics(dataset, options, jaspResults, ready, position = 5)
  
  # Create the network weights table
  .neuralNetworkTable(dataset, options, jaspResults, ready, purpose = "classification", position = 6)
  
  # Create the error plot
  .nnErrorPlot(dataset, options, jaspResults, ready, position = 7, purpose = "classification")
  
  # Create the ROC curve
  .rocCurve(dataset, options, jaspResults, ready, position = 8, type = "neuralnet")
  
  # Create the Andrews curves
  .classificationAndrewsCurves(dataset, options, jaspResults, ready, position = 9)
  
  # Create the activation function plot
  .neuralNetworkActivationFunctionPlot(options, jaspResults, position = 10)
  
  # Create the network graph
  .neuralNetworkGraph(dataset, options, jaspResults, ready, purpose = "classification", position = 11)
  
  # Decision boundaries
  .classificationDecisionBoundaries(dataset, options, jaspResults, ready, position = 12, type = "neuralnet")
  
}

.neuralnetClassification <- function(dataset, options, jaspResults) {
  
  # Import model formula from jaspResults
  formula <- jaspResults[["formula"]]$object
  
  # Split the data into training and test sets
  if (options[["holdoutData"]] == "testSetIndicator" && options[["testSetIndicatorVariable"]] != "") {
    # Select observations according to a user-specified indicator (included when indicator = 1)
    train.index <- which(dataset[, options[["testSetIndicatorVariable"]]] == 0)
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
    
    # Structure of hidden layers
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
    
    accuracyStore           <- numeric(options[["maxGen"]])
    trainAccuracyStore      <- numeric(options[["maxGen"]])
    
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
      
      fitness                <- numeric(options[["genSize"]])
      tmp_trainAccuracyStore <- numeric(options[["genSize"]])
      
      for (i in 1:length(population)) {
        
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
        
        validPredictions <- levels(train[, options[["target"]]])[max.col(predict(nfit_valid, newdata = valid))]
        fitness[i] <- sum(diag(prop.table(table(validPredictions, valid[, options[["target"]]]))))
        trainPredictions <- levels(train[, options[["target"]]])[max.col(predict(nfit_valid, newdata = train))]
        tmp_trainAccuracyStore[i] <- sum(diag(prop.table(table(trainPredictions, train[, options[["target"]]]))))
        
        population[[i]][["fitness"]] <- fitness[i]
        population[[i]][["age"]] <- population[[i]][["age"]] + 1
        
        plot_x <- c(plot_x, gen, gen)
        plot_y <- c(plot_y, fitness[i], tmp_trainAccuracyStore[i])
        plot_type <- c(plot_type, "Validation", "Training")   
      }
      
      # Find out best performance
      bestFitIndex <- order(fitness, decreasing = TRUE)[1]
      structure <- population[[bestFitIndex]]$structure # Best performing network structure
      
      # For plotting we store the mean accuracy of the generation
      accuracyStore[gen] <- mean(fitness)
      trainAccuracyStore[gen] <- mean(tmp_trainAccuracyStore)
      
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
    
    validPredictions <- levels(train[, options[["target"]]])[max.col(predict(nfit_test, newdata = valid))]
  }
  
  # Calculate AUC
  auc <- .classificationCalcAUC(test, train, options, "nnClassification", jaspResults = jaspResults)
  
  # Use the specified model to make predictions for dataset
  predictions <- levels(train[, options[["target"]]])[max.col(predict(nfit_test, newdata = dataset))]
  testPredictions <- levels(train[, options[["target"]]])[max.col(predict(nfit_test, newdata = test))]
  
  # Create results object
  classificationResult <- list()
  
  classificationResult[["formula"]]             <- formula
  classificationResult[["structure"]]           <- structure
  classificationResult[["model"]]               <- nfit_test
  classificationResult[["nLayers"]]             <- length(structure)
  classificationResult[["nNodes"]]              <- sum(structure)
  classificationResult[['confTable']]           <- table('Pred' = testPredictions, 'Real' = test[, options[["target"]]])
  classificationResult[['testAcc']]             <- sum(diag(prop.table(classificationResult[['confTable']])))
  classificationResult[["auc"]]                 <- auc
  classificationResult[["ntrain"]]              <- nrow(train)
  classificationResult[["ntest"]]               <- nrow(test)
  classificationResult[["testReal"]]            <- test[, options[["target"]]]
  classificationResult[["testPred"]]            <- testPredictions
  classificationResult[["train"]]               <- train
  classificationResult[["test"]]                <- test
  classificationResult[["testIndicatorColumn"]] <- testIndicatorColumn
  classificationResult[["classes"]]             <- predictions
  
  if (options[["modelOpt"]] != "optimizationManual") {
    classificationResult[["accuracyStore"]]       <- accuracyStore
    classificationResult[["valid"]]               <- valid
    classificationResult[["nvalid"]]              <- nrow(valid)
    classificationResult[["validationConfTable"]] <- table('Pred' = validPredictions, 'Real' = valid[, options[["target"]]])
    classificationResult[['validAcc']]            <- sum(diag(prop.table(classificationResult[['validationConfTable']])))
    classificationResult[["plotFrame"]]           <- data.frame(x = plot_x, y = plot_y, type = plot_type)
    
    if (options[["modelValid"]] == "validationManual")
      classificationResult[["trainAccuracyStore"]]  <- trainAccuracyStore
  }
  
  return(classificationResult)
}