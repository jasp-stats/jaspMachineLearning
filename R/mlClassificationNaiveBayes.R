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

mlClassificationNaiveBayes <- function(jaspResults, dataset, options, ...) {

  # Preparatory work
  dataset <- .mlClassificationReadData(dataset, options)
  .mlClassificationErrorHandling(dataset, options, type = "naivebayes")

  # Check if analysis is ready to run
  ready <- .mlClassificationReady(options, type = "naivebayes")

  # Compute results and create the model summary table
  .mlClassificationTableSummary(dataset, options, jaspResults, ready, position = 1, type = "naivebayes")

  # If the user wants to add the classes to the data set
  .mlClassificationAddPredictionsToData(dataset, options, jaspResults, ready)

  # Add test set indicator to data
  .mlAddTestIndicatorToData(options, jaspResults, ready, purpose = "classification")

  # Create the data split plot
  .mlPlotDataSplit(dataset, options, jaspResults, ready, position = 2, purpose = "classification", type = "naivebayes")

  # Create the confusion table
  .mlClassificationTableConfusion(dataset, options, jaspResults, ready, position = 3)

  # Create the class proportions table
  .mlClassificationTableProportions(dataset, options, jaspResults, ready, position = 4)

  # Create the validation measures table
  .mlClassificationTableMetrics(dataset, options, jaspResults, ready, position = 5)

  # Create the variable importance table
  .mlTableFeatureImportance(options, jaspResults, ready, position = 6, purpose = "classification")

  # Create the shap table
  .mlTableShap(dataset, options, jaspResults, ready, position = 7, purpose = "classification")

  # Create the naive Bayes tables
  .mlNaiveBayesTablePosterior(dataset, options, jaspResults, ready, position = 8)

  # Create the ROC curve
  .mlClassificationPlotRoc(dataset, options, jaspResults, ready, position = 9, type = "naivebayes")

  # Create the Andrews curves
  .mlClassificationPlotAndrews(dataset, options, jaspResults, ready, position = 10)

  # Decision boundaries
  .mlClassificationPlotBoundaries(dataset, options, jaspResults, ready, position = 11, type = "naivebayes")
}

.naiveBayesClassification <- function(dataset, options, jaspResults, ready) {
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
  trainingSet <- dataset[trainingIndex, ]
  # Create the generated test set indicator
  testIndicatorColumn <- rep(1, nrow(dataset))
  testIndicatorColumn[trainingIndex] <- 0
  # Just create a train and a test set (no optimization)
  testSet <- dataset[-trainingIndex, ]
  trainingFit <- e1071::naiveBayes(formula, data = trainingSet, laplace = options[["smoothingParameter"]])
  # Use the specified model to make predictions for dataset
  testPredictions <- predict(trainingFit, newdata = testSet, type = "class")
  dataPredictions <- predict(trainingFit, newdata = dataset, type = "class")
  # Create results object
  result <- list()
  result[["formula"]] <- formula
  result[["model"]] <- trainingFit
  result[["model"]]$data <- trainingSet
  result[["confTable"]] <- table("Pred" = testPredictions, "Real" = testSet[, options[["target"]]])
  result[["testAcc"]] <- sum(diag(prop.table(result[["confTable"]])))
  result[["auc"]] <- .classificationCalcAUC(testSet, trainingSet, options, "bayesClassification")
  result[["ntrain"]] <- nrow(trainingSet)
  result[["ntest"]] <- nrow(testSet)
  result[["testReal"]] <- testSet[, options[["target"]]]
  result[["testPred"]] <- testPredictions
  result[["train"]] <- trainingSet
  result[["test"]] <- testSet
  result[["testIndicatorColumn"]] <- testIndicatorColumn
  result[["classes"]] <- dataPredictions
  result[["explainer"]] <- DALEX::explain(result[["model"]], type = "classification", data = result[["train"]], y = result[["train"]][, options[["target"]]], predict_function = function(model, data) predict(model, newdata = data, type = "raw"))
  if (nlevels(result[["testReal"]]) == 2) {
    result[["explainer_fi"]] <- DALEX::explain(result[["model"]], type = "classification", data = result[["train"]], y = as.numeric(result[["train"]][, options[["target"]]]) - 1, predict_function = function(model, data) predict(model, newdata = data, type = "class"))
  } else {
    result[["explainer_fi"]] <- DALEX::explain(result[["model"]], type = "multiclass", data = result[["train"]], y = result[["train"]][, options[["target"]]] , predict_function = function(model, data) predict(model, newdata = data, type = "raw"))
  }
  return(result)
}

.mlNaiveBayesTablePosterior <- function(dataset, options, jaspResults, ready, position) {
  if (!options[["tablePosterior"]] || !is.null(jaspResults[["tablePosterior"]])) {
    return()
  }
  collection <- createJaspContainer(title = gettext("Posterior Statistics"))
  collection$position <- position
  collection$dependOn(options = c("tablePosterior", .mlClassificationDependencies()))
  for (i in seq_along(options[["predictors"]])) {
    table <- createJaspTable(title = gettextf("Feature: %1$s", options[["predictors"]][i]))
    table$addColumnInfo(name = "level", title = "", type = "string")
    table$position <- i
    collection[[paste0("feature", i)]] <- table
  }
  jaspResults[["tablePosterior"]] <- collection
  if (!ready) {
    return()
  }
  result <- jaspResults[["classificationResult"]]$object
  for (i in seq_along(options[["predictors"]])) {
    if (result[["model"]]$isnumeric[i]) {
      collection[[paste0("feature", i)]]$addColumnInfo(name = "mean", title = gettext("Mean"), type = "number")
      collection[[paste0("feature", i)]]$addColumnInfo(name = "sd", title = gettext("Std. deviation"), type = "number")
      collection[[paste0("feature", i)]]$addFootnote(gettext("The table displays the mean and standard deviation of the feature given the target class."))
      tab <- cbind.data.frame(result[["model"]]$levels, result[["model"]]$tables[[options[["predictors"]][i]]])
      colnames(tab) <- c("level", "mean", "sd")
    } else {
      tab <- matrix(result[["model"]]$tables[[options[["predictors"]][i]]], nrow = nrow(result[["model"]]$tables[[options[["predictors"]][i]]]), ncol = ncol(result[["model"]]$tables[[options[["predictors"]][i]]]))
      tab <- cbind(result[["model"]]$levels, round(as.data.frame(tab), 3))
      colnames(tab) <- c("level", colnames(result[["model"]]$tables[[options[["predictors"]][i]]]))
	  collection[[paste0("feature", i)]]$addFootnote(gettext("The table displays the conditional probabilities given the target class."))
    }
    collection[[paste0("feature", i)]]$setData(tab)
  }
}
