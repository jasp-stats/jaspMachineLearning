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

mlAnomalyOutlierTree <- function(jaspResults, dataset, options, ...) {
  # Preparatory work
  dataset <- .mlAnomalyReadData(dataset, options)
  .mlAnomalyErrorHandling(dataset, options)

  # Check if analysis is ready to run
  ready <- .mlAnomalyReady(options)

  # Compute results and create the model summary table
  .mlAnomalyTableSummary(dataset, options, jaspResults, ready, position = 1, type = "outliertree")

  # If the user wants to add the predictions to the data set
  .mlRegressionAddPredictionsToData(dataset, options, jaspResults, ready)

  # Create the table containing anomaly scores
  .mlAnomalyTableScores(dataset, options, jaspResults, ready, position = 2)

  # Create the plot
  .mlAnomalyMatrixPlot(dataset, options, jaspResults, ready, position = 3)
}

.mlOutlierTreeComputeResults <- function(dataset, options) {
  fit <- outliertree::outlier.tree(dataset[, options[["predictors"]]], save_outliers = TRUE, nthreads = 1,
                                   max_depth = options[["maxDepth"]])
  result <- list()
  result[["model"]] <- fit
  result[["values"]] <- unlist(as.numeric(lapply(fit[["outliers_data"]], `[[`, "outlier_score")))
  result[["N"]] <- nrow(dataset)
  return(result)
}
