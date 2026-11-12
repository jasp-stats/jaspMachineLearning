context("Machine Learning Prediction")

# Predictors assigned, but no trained model loaded ############################
options <- initMlOptions("mlPrediction")
options$trainedModelFilePath <- ""
options$predictors <- c("Petal.Width", "Sepal.Length", "Species")
options$predictors.types <- c("scale", "scale", "nominal")
set.seed(1)
results <- jaspTools::runAnalysis("mlPrediction", "iris.csv", options)

test_that("Prediction without a trained model does not fail", {
  expect_identical(results[["status"]], "complete")
  expect_null(results[["results"]][["errorMessage"]])
})

test_that("Model summary and predictions tables are empty without a trained model", {
  expect_false(is.null(results[["results"]][["modelSummaryTable"]]))
  expect_false(is.null(results[["results"]][["predictionsTable"]]))
  expect_length(results[["results"]][["modelSummaryTable"]][["data"]], 0)
  expect_length(results[["results"]][["predictionsTable"]][["data"]], 0)
})
