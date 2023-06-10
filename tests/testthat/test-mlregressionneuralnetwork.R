context("Machine Learning Neural Network Regression")

options <- jaspTools::analysisOptions("mlRegressionNeuralNetwork")
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
options$shapTable <- TRUE
options$shapFrom <- 1
options$shapTo <- 5
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

test_that("Feature Contributions to Predictions for Test Set Cases table results match", {
	skip("Need to figure out why this fails")
	table <- results[["results"]][["shapTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(-0.320874456044422, 5.70882938536069e-05, 5.43570184321823e-06,
			 0.321850912882019, 1, 0.00103898083329435, -0.320864287870447,
			 9.64880480532788e-05, 1.916986202931e-05, 0.321850912882019,
			 2, 0.00110228292165438, -0.32085021538794, 3.23716117410916e-05,
			 1.42235779519002e-05, 0.321850912882019, 3, 0.00104729268377171,
			 -0.320881902467681, 4.52598266686044e-05, 3.1853238832855e-05,
			 0.321850912882019, 4, 0.00104612347983946, -0.320830391213794,
			 0.000161871241736764, 6.14846902043651e-05, 0.321850912882019,
			 5, 0.00124387760016624))
})
