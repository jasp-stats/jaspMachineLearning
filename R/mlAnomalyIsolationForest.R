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
  .mlAnomalyAddPredictionsToData(dataset, options, jaspResults, ready)

  # Create the table containing anomaly scores
  .mlAnomalyTableScores(dataset, options, jaspResults, ready, position = 2, type = "isoforest")

  # Create the plot
  .mlAnomalyMatrixPlot(dataset, options, jaspResults, ready, position = 3, type = "isoforest")
}

.mlIsoForestComputeResults <- function(dataset, options) {
  result <- list()
  result[["model"]] <- isotree::isolation.forest(
    data = dataset[, options[["predictors"]]], sample_size = options[["sampleSize"]], ntrees = options[["nTrees"]],
    ndim = options[["numberOfPredictors"]], standardize_data = FALSE, scoring_metric = options[["scoringMetric"]], nthreads = 1
  )
  result[["values"]] <- as.numeric(predict(result[["model"]], newdata = dataset))
  result[["outlier"]] <- as.logical(result[["values"]] >= options[["cutoff"]])
  result[["classes"]] <- as.factor(ifelse(result[["outlier"]], yes = gettext("Anomaly"), no = gettext("Standard")))
  result[["noutlier"]] <- sum(result[["outlier"]])
  result[["ioutlier"]] <- which(result[["outlier"]])
  result[["N"]] <- nrow(dataset)
  return(result)
}
