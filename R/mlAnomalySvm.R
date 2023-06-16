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

mlAnomalySvm <- function(jaspResults, dataset, options, ...) {
  # Preparatory work
  dataset <- .mlAnomalyReadData(dataset, options)
  .mlAnomalyErrorHandling(dataset, options)

  # Check if analysis is ready to run
  ready <- .mlAnomalyReady(options)

  # Compute results and create the model summary table
  .mlAnomalyTableSummary(dataset, options, jaspResults, ready, position = 1, type = "svm")

  # If the user wants to add the predictions to the data set
  .mlAnomalyAddPredictionsToData(dataset, options, jaspResults, ready)

  # Create the table containing anomaly scores
  .mlAnomalyTableScores(dataset, options, jaspResults, ready, position = 2, type = "svm")

  # Create the support vectors table
  .TMPmlSvmTableSupportVectors(options, jaspResults, ready, position = 3, purpose = "anomaly")

  # Create the plot
  .mlAnomalyMatrixPlot(dataset, options, jaspResults, ready, position = 4, type = "svm")
}

.mlSvmAnomalyComputeResults <- function(dataset, options) {
  fit <- e1071::svm(dataset[, options[["predictors"]]], y = NULL, type = 'one-classification',
                    kernel = options[["weights"]], cost = options[["cost"]], tolerance = options[["tolerance"]],
                    epsilon = options[["epsilon"]], scale = FALSE, degree = options[["degree"]],
					gamma = options[["gamma"]], coef0 = options[["complexityParameter"]])
  result <- list()
  result[["model"]] <- fit
  result[["outlier"]] <- as.logical(!predict(fit, dataset[, options[["predictors"]]]))
  result[["values"]] <- as.numeric(result[["outlier"]])
  result[["classes"]] <- as.factor(ifelse(result[["outlier"]], yes = gettext("Anomaly"), no = gettext("Standard")))
  result[["noutlier"]] <- sum(result[["outlier"]])
  result[["ioutlier"]] <- which(result[["outlier"]])
  result[["N"]] <- nrow(dataset)
  return(result)
}

.TMPmlSvmTableSupportVectors <- function(options, jaspResults, ready, position, purpose) {
  if (!is.null(jaspResults[["supportVectorsTable"]]) || !options[["supportVectorsTable"]]) {
    return()
  }
  table <- createJaspTable(title = gettext("Support Vectors"))
  table$position <- position
   table$dependOn(options = c(.mlAnomalyDependencies(options), "supportVectorsTable"))
  table$addColumnInfo(name = "row", title = gettext("Row"), type = "string")
  jaspResults[["supportVectorsTable"]] <- table
  if (!ready) {
    return()
  }
  result <- jaspResults[["anomalyResult"]]$object
  vectors <- cbind(result[["model"]]$index, result[["model"]]$SV)
  colnames(vectors)[1] <- "row"
  vectors <- vectors[order(vectors[, 1]), ]
  for (i in 2:ncol(vectors)) {
    table$addColumnInfo(name = colnames(vectors)[i], title = colnames(vectors)[i], type = "number")
  }
  table$setData(vectors)
}
