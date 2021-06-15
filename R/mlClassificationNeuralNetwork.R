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
  
  # Create the ROC curve
  .rocCurve(dataset, options, jaspResults, ready, position = 7, type = "neuralnet")
  
  # Create the Andrews curves
  .classificationAndrewsCurves(dataset, options, jaspResults, ready, position = 8)
  
  # Create the activation function plot
  .neuralNetworkActivationFunctionPlot(options, jaspResults, position = 9)
  
  # Create the network graph
  .neuralNetworkGraph(dataset, options, jaspResults, ready, purpose = "classification", position = 10)
  
  # Decision boundaries
  .classificationDecisionBoundaries(dataset, options, jaspResults, ready, position = 11, type = "neuralnet")
  
}

.neuralnetClassification <- function(dataset, options, jaspResults) {
  
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
  
  # Calculate AUC
  auc <- .classificationCalcAUC(test, train, options, "nnClassification", jaspResults = jaspResults)
  
  # Use the specified model to make predictions for dataset
  predictions <- levels(train[, options[["target"]]])[max.col(predict(nfit_test, newdata = dataset))]
  testPredictions <- levels(train[, options[["target"]]])[max.col(predict(nfit_test, newdata = test))]

  # Create results object
  classificationResult <- list()
  
  classificationResult[["formula"]]             <- formula
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
  
  return(classificationResult)
}