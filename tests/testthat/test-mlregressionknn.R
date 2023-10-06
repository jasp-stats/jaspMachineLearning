context("Machine Learning KNN Regression")

options <- initMlOptions("mlRegressionKnn")
options$addIndicator <- FALSE
options$addPredictions <- FALSE
options$savePath <- ""
options$saveModel <- FALSE
options$holdoutData <- "holdoutManual"
options$modelOptimization <- "optimized"
options$modelValid <- "validationManual"
options$noOfFolds <- 5
options$errorVsKPlot <- TRUE
options$predictedPerformancePlot <- TRUE
options$predictors <- list("Malic", "Ash", "Alcalinity", "Magnesium", "Phenols", "Flavanoids", 
                           "Nonflavanoids", "Proanthocyanins", "Color", "Hue", "Dilution", 
                           "Proline")
options$setSeed <- TRUE
options$target <- "Alcohol"
options$testDataManual <- 0.2
options$testIndicatorColumn <- ""
options$testSetIndicatorVariable <- ""
options$validationDataManual <- 0.2
options$validationMeasures <- TRUE
options$predictionsColumn <- ""
options$tableShap <- TRUE
options$fromIndex <- 1
options$toIndex <- 5
options$featureImportanceTable <- TRUE
set.seed(1)
results <- jaspTools::runAnalysis("mlRegressionKnn", "wine.csv", options)


test_that("Data Split plot matches", {
  plotName <- results[["results"]][["plotDataSplit"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "data-split")
})

test_that("Mean Squared Error Plot matches", {
  plotName <- results[["results"]][["errorVsKPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "mean-squared-error-plot")
})

test_that("Predictive Performance Plot matches", {
  plotName <- results[["results"]][["predictedPerformancePlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "predictive-performance-plot")
})

test_that("K-Nearest Neighbors Regression table results match", {
	table <- results[["results"]][["regressionTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list("Euclidean", 35, 114, 29, 4, 0.37930375, 0.345521827586207, "rectangular"
			))
})

test_that("Model Performance Metrics table results match", {
	table <- results[["results"]][["validationMeasures"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list("MSE", 0.379, "RMSE", 0.616, "MAE / MAD", 0.512, "MAPE", "3.98%",
			 "R<unicode>", 0.49))
})

test_that("Feature Importance Metrics table results match", {
	table <- results[["results"]][["featureImportanceTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(0.556118980818432, "Proline", 0.529799874524849, "Color", 0.496756839489028,
			 "Nonflavanoids", 0.483666004544205, "Malic", 0.479508609203753,
			 "Ash", 0.478323316516838, "Dilution", 0.478111992674601, "Alcalinity",
			 0.472073634928589, "Magnesium", 0.470565483165322, "Hue", 0.469851373192504,
			 "Proanthocyanins", 0.468903797905332, "Phenols", 0.468837949879727,
			 "Flavanoids"))
})

test_that("Additive Explanations for Predictions of Test Set Cases table results match", {
	table <- results[["results"]][["tableShap"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(-0.0864035087719248, 0.0501973684210526, -0.077017543859645, -0.0187500000000025,
			 0.0237938596491336, 0.128684210526309, 0.191820175438606, 0.0370614035087691,
			 0.0243640350877179, 0.0652850877192943, 0.0411622807017551,
			 -0.0553508771929838, 13.1026535087719, 1, 13.4275, -0.00848684210526152,
			 0.011228070175445, -0.0341228070175461, 0.0382456140350929,
			 0.0517763157894535, 0.0712061403508759, 0.060570175438599, -0.0117982456140471,
			 0.024276315789475, 0.0358333333333309, -0.0710964912280705,
			 0.277214912280701, 13.1026535087719, 2, 13.5475, 0.206074561403501,
			 0.0446929824561373, 0.0288596491228024, 0.082061403508769, -0.0530921052631665,
			 -0.092807017543846, 0.0501973684210597, -0.0188596491227973,
			 0.0681140350877278, 0.14436403508773, 0.291008771929823, 0.191732456140352,
			 13.1026535087719, 3, 14.045, 0.104144736842107, 0.0335964912280708,
			 0.0816228070175367, 0.059473684210527, -0.0963596491227783,
			 -0.0229385964912296, 0.0222587719298168, 0.00550438596491176,
			 -0.00664473684210876, 0.264758771929833, -0.0450877192982251,
			 0.387017543859656, 13.1026535087719, 4, 13.89, -0.0373026315789442,
			 0.0040131578947431, 0.0603728070175489, -0.0179824561403539,
			 0.101052631578945, -0.0175657894736609, 0.0355482456140326,
			 -0.0255701754386042, 0.0128728070175406, 0.0723684210526265,
			 0.0514912280701711, 0.275548245614035, 13.1026535087719, 5,
			 13.6175))
})
