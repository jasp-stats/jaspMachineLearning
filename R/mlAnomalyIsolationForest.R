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

mlAnomalyIsolationForest <- function(jaspResults, dataset, options, ...) {
  # Preparatory work
  dataset <- .mlAnomalyReadData(dataset, options)
  .mlAnomalyErrorHandling(dataset, options)

  # Check if analysis is ready to run
  ready <- .mlAnomalyReady(options)

  # Compute results and create the model summary table
  .mlAnomalyTableSummary(dataset, options, jaspResults, ready, position = 1, type = "isoforest")

  # If the user wants to add the predictions to the data set
  .mlRegressionAddPredictionsToData(dataset, options, jaspResults, ready)

  # Create the table containing anomaly scores
  .mlAnomalyTableScores(dataset, options, jaspResults, ready, position = 2)

  # Create the plot
  .mlAnomalyMatrixPlot(dataset, options, jaspResults, ready, position = 3)
}

.mlIsoForestComputeResults <- function(dataset, options) {
  fit <- isotree::isolation.forest(
    data = dataset[, options[["predictors"]]], sample_size = options[["sampleSize"]], ntrees = options[["nTrees"]],
    ndim = options[["numberOfPredictors"]], standardize_data = FALSE, scoring_metric = options[["scoringMetric"]], nthreads = 1
  )
  result <- list()
  result[["model"]] <- fit
  result[["values"]] <- predict(fit, newdata = dataset)
  result[["N"]] <- nrow(dataset)
  result[["n"]] <- length(which(result[["values"]] >= options[["cutoff"]]))
  return(result)
}
