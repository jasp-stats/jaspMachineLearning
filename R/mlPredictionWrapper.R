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

#' Prediction
#'
#' The prediction analysis enables you to load a trained machine learning model and apply it to new data. It is important that the features in the new dataset have the same names as in the original dataset used for training.
#'
#' @param addPredictions, Adds a column with the predicted cluster membership (clustering) or predicted values (regression/classification) to the dataset.
#'    Defaults to \code{FALSE}.
#' @param addProbabilities, In classification analyses, append the predicted probabilities for each class to the data. For neural networks, this option provides the output of the final layer.
#'    Defaults to \code{FALSE}.
#' @param addSoftMemberships, Adds one column per cluster with the soft membership or posterior probability of each observation. The column name is used as a prefix (e.g., membership_1, membership_2).
#'    Defaults to \code{FALSE}.
#' @param addTsneCoordinates, Adds the two-dimensional t-SNE embedding (dimension 1 and 2) for each observation. These are the same coordinates used in the t-SNE cluster plot. The column name is used as a prefix (e.g., tsne_1, tsne_2).
#'    Defaults to \code{FALSE}.
#' @param predictionsColumn, The column name for the predicted values.
#' @param saveModel, When clicked, the model is exported to the specified file path.
#'    Defaults to \code{FALSE}.
#' @param savePath, The file path for the saved model.
#' @param softMembershipsColumn, Prefix for the soft membership columns. One column is created per cluster.
#' @param tsneCoordinatesColumn, Prefix for the t-SNE coordinate columns (suffixes _1 and _2 are added).
mlPrediction <- function(
          data = NULL,
          version = "1",
          addPredictions = FALSE,
          addProbabilities = FALSE,
          addSoftMemberships = FALSE,
          addTsneCoordinates = FALSE,
          fromIndex = 1,
          plotHeight = 320,
          plotWidth = 480,
          predictionsColumn = "",
          predictionsTable = TRUE,
          predictionsTableFeatures = FALSE,
          predictors = list(types = list(), value = list()),
          saveModel = FALSE,
          savePath = "",
          softMembershipsColumn = "",
          tableShap = FALSE,
          toIndex = 20,
          trainedModelFilePath = "",
          tsneCoordinatesColumn = "") {

   options <- mget(names(formals(sys.function())), envir = environment(), inherits = FALSE)
   options[["data"]] <- NULL
   options[["version"]] <- NULL


   if (!jaspBase::jaspResultsCalledFromJasp() && !is.null(data)) {
      jaspBase::storeDataSet(data)
   }

   optionsWithFormula <- c("predictors")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   # Options not given in the call get their default from the form, which can depend on the other options.
   options <- options[names(options) %in% names(match.call())[-1L]]
   return(jaspBase::runWrappedAnalysis("jaspMachineLearning", "mlPrediction", "mlPrediction.qml", options, version, TRUE))
}