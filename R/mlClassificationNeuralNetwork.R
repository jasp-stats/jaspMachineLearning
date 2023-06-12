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
  dataset <- .mlClassificationReadData(dataset, options)
  .mlClassificationErrorHandling(dataset, options, type = "neuralnet")

  # Check if analysis is ready to run
  ready <- .mlClassificationReady(options, type = "neuralnet")

  # Determine activation and loss function for the neural net
  .mlNeuralNetworkActFunction(options, jaspResults)
  # .mlNeuralNetworkLossFunction(options, jaspResults)

  # Compute results and create the model summary table
  .mlClassificationTableSummary(dataset, options, jaspResults, ready, position = 1, type = "neuralnet")

  # If the user wants to add the classes to the data set
  .mlClassificationAddPredictionsToData(dataset, options, jaspResults, ready)

  # Add test set indicator to data
  .mlAddTestIndicatorToData(options, jaspResults, ready, purpose = "classification")

  # Create the data split plot
  .mlPlotDataSplit(dataset, options, jaspResults, ready, position = 2, purpose = "classification", type = "neuralnet")

  # Create the confusion table
  .mlClassificationTableConfusion(dataset, options, jaspResults, ready, position = 3)

  # Create the class proportions table
  .mlClassificationTableProportions(dataset, options, jaspResults, ready, position = 4)

  # Create the validation measures table
  .mlClassificationTableMetrics(dataset, options, jaspResults, ready, position = 5)

  # Create the feature importance table
  .mlTableFeatureImportance(options, jaspResults, ready, position = 6, purpose = "classification")

  # Create the shap table
  .mlTableShap(dataset, options, jaspResults, ready, position = 7, purpose = "classification")

  # Create the error plot
  .mlNeuralNetworkPlotError(dataset, options, jaspResults, ready, position = 8, purpose = "classification")

  # Create the network weights table
  .mlNeuralNetworkTableWeights(dataset, options, jaspResults, ready, position = 9, purpose = "classification")

  # Create the ROC curve
  .mlClassificationPlotRoc(dataset, options, jaspResults, ready, position = 10, type = "neuralnet")

  # Create the Andrews curves
  .mlClassificationPlotAndrews(dataset, options, jaspResults, ready, position = 11)

  # Create the activation function plot
  .mlNeuralNetworkPlotActivationFunction(options, jaspResults, position = 12)

  # Create the network graph
  .mlNeuralNetworkPlotStructure(dataset, options, jaspResults, ready, purpose = "classification", position = 13)

  # Decision boundaries
  .mlClassificationPlotBoundaries(dataset, options, jaspResults, ready, position = 14, type = "neuralnet")
}

.neuralnetClassification <- function(dataset, options, jaspResults) {
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
  if (options[["modelOptimization"]] == "manual") {
    trainingSet <- trainingAndValidationSet
    testSet <- dataset[-trainingIndex, ]
    structure <- .getNeuralNetworkStructure(options)
    p <- try({
      fit <- neuralnet::neuralnet(
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
  } else if (options[["modelOptimization"]] == "optimized") {
    validationIndex <- sample.int(nrow(trainingAndValidationSet), size = ceiling(options[["validationDataManual"]] * nrow(trainingAndValidationSet)))
    testSet <- dataset[-trainingIndex, ]
    validationSet <- trainingAndValidationSet[validationIndex, ]
    trainingSet <- trainingAndValidationSet[-validationIndex, ]
    accuracyStore <- numeric(options[["maxGenerations"]])
    trainAccuracyStore <- numeric(options[["maxGenerations"]])
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
      for (i in 1:length(population)) {
        p <- try({
          fit <- neuralnet::neuralnet(
            formula = formula,
            data = trainingSet,
            hidden = population[[i]]$structure,
            learningrate = options[["learningRate"]],
            threshold = options[["threshold"]],
            stepmax = options[["maxTrainingRepetitions"]],
            rep = 1,
            startweights = NULL,
            algorithm = options[["algorithm"]],
            err.fct = "sse", # jaspResults[["lossFunction"]]$object,
            act.fct = jaspResults[["actfct"]]$object,
            linear.output = if (options[["actfct"]] == "linear") TRUE else FALSE
          )
          validationPredictions <- levels(trainingSet[, options[["target"]]])[max.col(predict(fit, newdata = validationSet))]
        })
        if (isTryError(p)) {
          jaspBase:::.quitAnalysis(gettextf("Analysis not possible: The algorithm did not converge within the maximum number of training repetitions (%1$s).", options[["maxTrainingRepetitions"]]))
        }
        fitness[i] <- sum(diag(prop.table(table(validationPredictions, validationSet[, options[["target"]]]))))
        trainingPredictions <- levels(trainingSet[, options[["target"]]])[max.col(predict(fit, newdata = trainingSet))]
        subTrainErrorStore[i] <- sum(diag(prop.table(table(trainingPredictions, trainingSet[, options[["target"]]]))))
        population[[i]][["fitness"]] <- fitness[i]
        population[[i]][["age"]] <- population[[i]][["age"]] + 1
        plot_x <- c(plot_x, gen, gen)
        plot_y <- c(plot_y, fitness[i], subTrainErrorStore[i])
        plot_type <- c(plot_type, "Validation set", "Training set")
      }
      # Find out best performance
      bestFitIndex <- order(fitness, decreasing = TRUE)[1]
      structure <- population[[bestFitIndex]]$structure # Best performing network structure
      # For plotting we store the mean accuracy of the generation
      accuracyStore[gen] <- mean(fitness)
      trainAccuracyStore[gen] <- mean(subTrainErrorStore)
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
      fit <- neuralnet::neuralnet(
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
    validationPredictions <- levels(trainingSet[, options[["target"]]])[max.col(predict(fit, newdata = validationSet))]
  }
  # Use the specified model to make predictions for dataset
  dataPredictions <- levels(trainingSet[, options[["target"]]])[max.col(predict(fit, newdata = dataset))]
  testPredictions <- levels(trainingSet[, options[["target"]]])[max.col(predict(fit, newdata = testSet))]
  # Create results object
  result <- list()
  result[["formula"]] <- formula
  result[["structure"]] <- structure
  result[["model"]] <- fit
  result[["nLayers"]] <- length(structure)
  result[["nNodes"]] <- sum(structure)
  result[["confTable"]] <- table("Pred" = testPredictions, "Real" = testSet[, options[["target"]]])
  result[["testAcc"]] <- sum(diag(prop.table(result[["confTable"]])))
  result[["auc"]] <- .classificationCalcAUC(testSet, trainingSet, options, "nnClassification", jaspResults = jaspResults)
  result[["ntrain"]] <- nrow(trainingSet)
  result[["ntest"]] <- nrow(testSet)
  result[["testReal"]] <- testSet[, options[["target"]]]
  result[["testPred"]] <- testPredictions
  result[["train"]] <- trainingSet
  result[["test"]] <- testSet
  result[["testIndicatorColumn"]] <- testIndicatorColumn
  result[["classes"]] <- dataPredictions
  if (options[["modelOptimization"]] != "manual") {
    result[["accuracyStore"]] <- accuracyStore
    result[["valid"]] <- validationSet
    result[["nvalid"]] <- nrow(validationSet)
    result[["validationConfTable"]] <- table("Pred" = validationPredictions, "Real" = validationSet[, options[["target"]]])
    result[["validAcc"]] <- sum(diag(prop.table(result[["validationConfTable"]])))
    result[["plotFrame"]] <- data.frame(x = plot_x, y = plot_y, type = plot_type)
    if (options[["modelValid"]] == "validationManual") {
      result[["trainAccuracyStore"]] <- trainAccuracyStore
    }
  }
  result[["explainer"]] <- DALEX::explain(result[["model"]], type = "multiclass", data = result[["train"]], y = result[["train"]][, options[["target"]]], predict_function = function(model, data) scales::rescale(x = predict(model, newdata = data), from = range(predict(model, newdata = data)), to = c(0, 1)))
  if (nlevels(result[["testReal"]]) == 2) {
    result[["explainer_fi"]] <- DALEX::explain(result[["model"]], type = "classification", data = result[["train"]], y = as.numeric(result[["train"]][, options[["target"]]]) - 1, predict_function = function(model, data) apply(predict(model, newdata = data), 1, which.max) - 1)
  } else {
    result[["explainer_fi"]] <- DALEX::explain(result[["model"]], type = "multiclass", data = result[["train"]], y = result[["train"]][, options[["target"]]], predict_function = function(model, data) scales::rescale(x = predict(model, newdata = data), from = range(predict(model, newdata = data)), to = c(0, 1)))
  }
  return(result)
}
