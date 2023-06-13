context("Machine Learning Neural Network Regression")

options <- initMlOptions("mlRegressionNeuralNetwork")
options$addIndicator <- FALSE
options$addPredictions <- FALSE
options$algorithm <- "backprop"
options$coefficientsTable <- TRUE
options$dataSplitPlot <- FALSE
options$holdoutData <- "holdoutManual"
options$layers <- list(list(nodes = 1, value = "#"))
options$modelOptimization <- "manual"
options$modelValid <- "validationManual"
options$predictionsColumn <- ""
options$predictors <- c("Sepal.Width", "Petal.Length", "Petal.Width")
options$saveModel <- FALSE
options$savePath <- ""
options$setSeed <- TRUE
options$target <- "Sepal.Length"
options$testDataManual <- 0.2
options$testIndicatorColumn <- ""
options$testSetIndicatorVariable <- ""
options$threshold <- 0.05
options$validationDataManual <- 0.2
options$validationMeasures <- TRUE
options$tableShap <- TRUE
options$fromIndex <- 1
options$toIndex <- 5
options$featureImportanceTable <- TRUE
set.seed(1)
results <- jaspTools::runAnalysis("mlRegressionNeuralNetwork", "iris.csv", options)


test_that("Network Weights table results match", {
	table <- results[["results"]][["coefficientsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list("", "Intercept", "<unicode>", 1, "Hidden 1", -0.54934857434766,
			 "input", "Sepal.Width", "<unicode>", 1, "Hidden 1", 0.260789614258883,
			 "input", "Petal.Length", "<unicode>", 1, "Hidden 1", 1.24992021596332,
			 "input", "Petal.Width", "<unicode>", 1, "Hidden 1", -0.480713945895261,
			 "", "Intercept", "<unicode>", "output", "Sepal.Length", -2.30388152109292,
			 1, "Hidden 1", "<unicode>", "output", "Sepal.Length", 6.0564624457196
			))
})

test_that("Neural Network Regression table results match", {
	table <- results[["results"]][["regressionTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(1, 30, 120, 1, 0.136174480403049))
})

test_that("Model Performance Metrics table results match", {
	table <- results[["results"]][["validationMeasures"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list("MSE", 0.136, "RMSE", 0.369, "MAE / MAD", 0.298, "MAPE", "82.02%",
			 "R<unicode>", 0.839))
})

test_that("Additive Explanations for Predictions of Test Set Cases table results match", {
	table <- results[["results"]][["tableShap"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(-1.59997915500303, 0.364874209449954, 0.289237798262734, -0.0238595278151637,
			 1, -0.969726675105506, -1.55805185666379, 0.384932855375104,
			 0.368095038008247, -0.0238595278151637, 2, -0.828883491095599,
			 -1.51400903259349, 0.405665015995474, 0.185445554270068, -0.0238595278151637,
			 3, -0.946757990143114, -1.63984856831888, 0.246683433870073,
			 0.429244500561797, -0.0238595278151637, 4, -0.987780161702174,
			 -1.46779671903952, 0.366702686290143, 0.455883569487823, -0.0238595278151637,
			 5, -0.669069991076721))
})

test_that("Feature Importance Metrics table results match", {
	table <- results[["results"]][["featureImportanceTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(2.0937880548016, "Petal.Length", 0.830806976081752, "Petal.Width",
			 0.587318501907843, "Sepal.Width"))
})
