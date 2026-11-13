#
# Copyright (C) 2013-2025 University of Amsterdam
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

# This is a generated file. Don't change it!

#' Boosting Regression
#'
#' Boosting works by sequentially adding features to an decision tree ensemble, each one correcting its predecessor. Boosting tries to fit the new feature to the residual errors made by the previous feature.
#' ### Assumptions
#' - The target variable is a continuous variable.
#' - The feature variables consist of continuous, nominal, or ordinal variables.
#'
#' @param addIndicator, Add the generated test set indicator from the option above to your data set.
#'    Defaults to \code{FALSE}.
#' @param addPredictions, Adds a column with the predicted cluster membership (clustering) or predicted values (regression/classification) to the dataset.
#'    Defaults to \code{FALSE}.
#' @param addProbabilities, In classification analyses, append the predicted probabilities for each class to the data. For neural networks, this option provides the output of the final layer.
#'    Defaults to \code{FALSE}.
#' @param addSoftMemberships, Adds one column per cluster with the soft membership or posterior probability of each observation. The column name is used as a prefix (e.g., membership_1, membership_2).
#'    Defaults to \code{FALSE}.
#' @param addTsneCoordinates, Adds the two-dimensional t-SNE embedding (dimension 1 and 2) for each observation. These are the same coordinates used in the t-SNE cluster plot. The column name is used as a prefix (e.g., tsne_1, tsne_2).
#'    Defaults to \code{FALSE}.
#' @param baggingFraction, Select the percentage of training data that is used to train each individual tree.
#' @param dataSplitPlot, Shows how the data is split into training (and validation), and test set.
#'    Defaults to \code{TRUE}.
#' @param deviancePlot, Shows the prediction error plotted against the number of trees.
#'    Defaults to \code{FALSE}.
#' @param distance, The loss function used.
#' @param featureImportancePermutations, Sets the number of permutations on which the mean dropout loss is based.
#' @param featureImportanceTable, Shows the available feature importance metrics for the fitted model.
#'    Defaults to \code{FALSE}.
#' @param fromIndex, The test set index of the first row to be displayed in the table.
#' @param holdoutData, Choose how to create the test set.
#' \itemize{
#'   \item \code{"holdoutManual"}: Choose a percentage to randomly sample from your data to derive prediction error. Generates an internal indicator variable that indicates whether the observation is included (1) or excluded (0) from the test set.
#'   \item \code{"testSetIndicator"}: Use an indicator variable to select data for the test set. This indicator should be a column of type scale in your data that consists of only 0 (excluded from the test set) and 1 (included in the test set). The data will then be split into a training (and validation if requested) set (0), and a test set (1) according to your indicator.
#' }
#' @param interactionDepth, Integer specifying the maximum depth of each tree (i.e., the highest level of variable interactions allowed. A value of 1 implies an additive model, a value of 2 implies a model with up to 2-way interactions, etc. Default is 1.
#' @param maxTrees, Sets the maximum number of possible decision trees to be considered. At default, this is set to 100.
#' @param minObservationsInNode, Integer specifying the minimum number of observations in the terminal nodes of the trees. Note that this is the actual number of observations, not the total weight.
#' @param modelOptimization, Choose how to optimize the model.
#' \itemize{
#'   \item \code{"manual"}: Enables you to use a user-specified number of decision trees.
#'   \item \code{"optimized"}: Enables you to optimize the prediction error on a validation data set with respect to the number of trees.
#' }
#' @param modelValid, Choose how to create the validation set.
#' \itemize{
#'   \item \code{"validationManual"}: Randomly sample a percentage from the remaining training data (after selecting the test set).
#'   \item \code{"validationKFold"}: Partition the remaining data in *k* parts.
#'   \item \code{"validationLeaveOneOut"}: Partition the remaining data in *n* parts.
#' }
#' @param noOfFolds, The number of folds to be used.
#' @param noOfTrees, The number of trees.
#' @param outOfBagImprovementPlot, Plots the number of trees against the out-of-bag classification accuracy improvement of the model. Accuracy is assessed for the training set.
#'    Defaults to \code{FALSE}.
#' @param predictedPerformancePlot, Plots the true values of the observations in the test set against their predicted values.
#'    Defaults to \code{FALSE}.
#' @param predictionsColumn, The column name for the predicted values.
#' @param predictors, In this box, the variables that provide information about the target variable should be entered.
#' @param relativeInfluencePlot, Shows the relative influence of the features.
#'    Defaults to \code{FALSE}.
#' @param saveModel, When clicked, the model is exported to the specified file path.
#'    Defaults to \code{FALSE}.
#' @param savePath, The file path for the saved model.
#' @param scaleVariables, Standardizes the continuous features in the dataset. Standardization ensures that values of features from different scales range into a specific similar scale. As a result, standardizing provides numerical stability. JASP uses the Z-score standardization of a mean of 0 and a standard deviation of 1. This option is selected by default.
#'    Defaults to \code{TRUE}.
#' @param seed, The value of the seed.
#' @param setSeed, Gives the option to set a seed for your analysis. Setting a seed will exclude random processes influencing an analysis. For example, setting a seed makes it possible to re-run analyses with the same data splits.
#'    Defaults to \code{FALSE}.
#' @param shrinkage, A shrinkage parameter applied to each tree in the expansion. Also known as the learning rate or step-size reduction 0.001 to 0.1 usually work, but a smaller learning rate typically requires more trees.
#' @param softMembershipsColumn, Prefix for the soft membership columns. One column is created per cluster.
#' @param tableShap, Shows a decomposition of the predictions of the model into contributions that can be attributed to individual model features. This feature uses the breakdown algoritm from the `ibreakdown` R package. For more details about this method, see Gosiewska and Biecek (2019).
#'    Defaults to \code{FALSE}.
#' @param target, In this box, the variable that needs to be predicted should be entered.
#' @param testDataManual, The percentage of observations to use for the test set.
#' @param testIndicatorColumn, The column name for the generated test set indicator.
#' @param testSetIndicatorVariable, The variable in the data set that is used as the test set indicator.
#' @param toIndex, The test set index of the last row to be displayed in the table.
#' @param tsneCoordinatesColumn, Prefix for the t-SNE coordinate columns (suffixes _1 and _2 are added).
#' @param validationDataManual, The percentage of observations to use for the validation set.
#' @param validationMeasures, Displays available model performance metrics. For regression, these metrics include mean squared error (MSE), root mean squared error (RMSE), R<sup>2</sup> and more. For classification, these metrics include precision, recall, the F1-score, support, AUC (area under the ROC curve) and more. For clustering, these metrics include entropy, Dunn index and more.
#'    Defaults to \code{FALSE}.
mlRegressionBoosting <- function(
          data = NULL,
          version = "1",
          addIndicator = FALSE,
          addPredictions = FALSE,
          addProbabilities = FALSE,
          addSoftMemberships = FALSE,
          addTsneCoordinates = FALSE,
          baggingFraction = 0.5,
          dataSplitPlot = TRUE,
          deviancePlot = FALSE,
          distance = "gaussian",
          featureImportancePermutations = 50,
          featureImportanceTable = FALSE,
          fromIndex = 1,
          holdoutData = "holdoutManual",
          interactionDepth = 1,
          maxTrees = 100,
          minObservationsInNode = 10,
          modelOptimization = "optimized",
          modelValid = "validationManual",
          noOfFolds = 5,
          noOfTrees = 100,
          outOfBagImprovementPlot = FALSE,
          plotHeight = 320,
          plotWidth = 480,
          predictedPerformancePlot = FALSE,
          predictionsColumn = "",
          predictors = list(types = list(), value = list()),
          relativeInfluencePlot = FALSE,
          saveModel = FALSE,
          savePath = "",
          scaleVariables = TRUE,
          seed = 1,
          setSeed = FALSE,
          shrinkage = 0.1,
          softMembershipsColumn = "",
          tableShap = FALSE,
          target = list(types = list(), value = ""),
          testDataManual = 0.2,
          testIndicatorColumn = "",
          testSetIndicatorVariable = list(types = "unknown", value = ""),
          toIndex = 5,
          tsneCoordinatesColumn = "",
          validationDataManual = 0.2,
          validationMeasures = FALSE) {

   options <- mget(names(formals(sys.function())), envir = environment(), inherits = FALSE)
   options[["data"]] <- NULL
   options[["version"]] <- NULL


   if (!jaspBase::jaspResultsCalledFromJasp() && !is.null(data)) {
      jaspBase::storeDataSet(data)
   }

   optionsWithFormula <- c("distance", "predictors", "target", "testSetIndicatorVariable")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   # Options not given in the call get their default from the form, which can depend on the other options.
   options <- options[names(options) %in% names(match.call())[-1L]]
   return(jaspBase::runWrappedAnalysis("jaspMachineLearning", "mlRegressionBoosting", "mlRegressionBoosting.qml", options, version, TRUE))
}