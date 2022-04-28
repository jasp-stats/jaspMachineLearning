context("Machine Learning Neural Network Regression")

options <- jaspTools::analysisOptions("mlRegressionNeuralNetwork")
options$addIndicator <- FALSE
options$addPredictions <- FALSE
options$algorithm <- "backprop"
options$coefficientsTable <- TRUE
options$dataSplitPlot <- FALSE
options$holdoutData <- "holdoutManual"
options$layers <- list(list(nodes = 1, value = "#"))
options$modelOpt <- "optimizationManual"
options$modelValid <- "validationManual"
options$predictionsColumn <- ""
options$predictors <- c("Sepal.Width", "Petal.Length", "Petal.Width")
options$saveModel <- FALSE
options$savePath <- ""
options$seedBox <- TRUE
options$target <- "Sepal.Length"
options$testDataManual <- 0.2
options$testIndicatorColumn <- ""
options$testSetIndicatorVariable <- ""
options$threshold <- 0.05
options$validationDataManual <- 0.2
options$validationMeasures <- TRUE
set.seed(1)
results <- jaspTools::runAnalysis("mlRegressionNeuralNetwork", "iris.csv", options)


test_that("Network Weights table results match", {
	table <- results[["results"]][["coefficientsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list("", "Intercept", "<unicode><unicode><unicode>", 1, "Hidden 1",
			 1.4064573611948, "input", "Sepal.Width", "<unicode><unicode><unicode>",
			 1, "Hidden 1", -0.993505935501929, "input", "Petal.Length",
			 "<unicode><unicode><unicode>", 1, "Hidden 1", -4.75534097725333,
			 "input", "Petal.Width", "<unicode><unicode><unicode>", 1, "Hidden 1",
			 1.30637749345671, "", "Intercept", "<unicode><unicode><unicode>",
			 "output", "Sepal.Length", 4.54648003765896, 1, "Hidden 1", "<unicode><unicode><unicode>",
			 "output", "Sepal.Length", -11.5080804386269))
})

test_that("Neural Network Regression table results match", {
	table <- results[["results"]][["regressionTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(1, 1, 30, 120, 0.423937879574653))
})

test_that("Evaluation Metrics table results match", {
	table <- results[["results"]][["validationMeasures"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list("MSE", 0.424, "RMSE", 0.651, "MAE / MAD", 0.515, "MAPE", "87.06%", "R<unicode><unicode>",
			 0.591))
})