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
  .mlAnomalyAddPredictionsToData(dataset, options, jaspResults, ready)

  # Create the table containing anomaly scores
  .mlAnomalyTableScores(dataset, options, jaspResults, ready, position = 2, type = "outliertree")

  # Create the plot
  .mlAnomalyMatrixPlot(dataset, options, jaspResults, ready, position = 3, type = "outliertree")
}

.mlOutlierTreeComputeResults <- function(dataset, options) {
  result <- list()
  result[["model"]] <- outliertree::outlier.tree(dataset[, options[["predictors"]]], save_outliers = TRUE, nthreads = 1,
                                   max_depth = options[["maxDepth"]], min_gain = options[["complexityParameter"]])
  out <- outliertree:::list.to.outliers((result[["model"]]$outliers_data))
  result[["values"]] <- as.numeric(ifelse(is.na(out[["outlier_score"]]), yes = 0, no = out[["outlier_score"]]))
  result[["depth"]] <- as.numeric(ifelse(is.na(out[["outlier_score"]]), yes = 0, no = out[["tree_depth"]]))
  result[["ntrees"]] <- result[["model"]]$obj_from_cpp$ntrees
  result[["nclusters"]] <- result[["model"]]$obj_from_cpp$nclust
  result[["susp"]] <- character(length(result[["depth"]]))
  for (i in seq_along(result[["depth"]])) {
	result[["susp"]][i] <- if (is.null(out$suspicous_value[[i]]$column)) "" else out$suspicous_value[[i]]$column
  }
  result[["outlier"]] <- as.logical(result[["values"]] != 0)
  result[["classes"]] <- as.factor(ifelse(result[["outlier"]], yes = gettext("Anomaly"), no = gettext("Standard")))
  result[["noutlier"]] <- sum(result[["outlier"]])
  result[["ioutlier"]] <- which(result[["outlier"]])
  result[["N"]] <- nrow(dataset)
  return(result)
}
