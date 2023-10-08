context("Machine Learning Decision Tree Regression")

options <- initMlOptions("mlRegressionDecisionTree")
options$addIndicator <- FALSE
options$addPredictions <- FALSE
options$holdoutData <- "holdoutManual"
options$modelOptimization <- "manual"
options$modelValid <- "validationManual"
options$noOfFolds <- 5
options$decisionTreePlot <- TRUE
options$predictedPerformancePlot <- TRUE
options$predictionsColumn <- ""
options$predictors <- c("Sepal.Width", "Petal.Length", "Petal.Width", "Species")
options$saveModel <- FALSE
options$savePath <- ""
options$setSeed <- TRUE
options$featureImportanceTable <- TRUE
options$target <- "Sepal.Length"
options$testDataManual <- 0.2
options$testIndicatorColumn <- ""
options$testSetIndicatorVariable <- ""
options$validationDataManual <- 0.2
options$validationMeasures <- TRUE
options$tableShap <- TRUE
options$fromIndex <- 1
options$toIndex <- 5
set.seed(1)
results <- jaspTools::runAnalysis("mlRegressionDecisionTree", "iris.csv", options)


test_that("Data Split plot matches", {
	plotName <- results[["results"]][["plotDataSplit"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "data-split")
})

test_that("Decision Tree Plot matches", {
	plotName <- results[["results"]][["decisionTreePlot"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "decision-tree-plot")
})

test_that("Predictive Performance Plot matches", {
	plotName <- results[["results"]][["predictedPerformancePlot"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "predictive-performance-plot")
})

test_that("Decision Tree Regression table results match", {
	table <- results[["results"]][["regressionTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(0.01, 30, 120, 37, 0.10398915405807))
})

test_that("Model Performance Metrics table results match", {
	table <- results[["results"]][["validationMeasures"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list("MSE", 0.104, "RMSE", 0.322, "MAE / MAD", 0.266, "MAPE", "4.55%",
			 "R<unicode>", 0.818))
})

test_that("Additive Explanations for Predictions of Test Set Cases table results match", {
	table <- results[["results"]][["tableShap"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(-0.978962962962963, 0, 0.323629629629635, 0, 5.82333333333333,
			 1, 5.168, -0.978962962962963, 0, 0.323629629629635, 0, 5.82333333333333,
			 2, 5.168, -0.978962962962963, 0, 0.323629629629635, 0, 5.82333333333333,
			 3, 5.168, -0.978962962962963, 0, 0.323629629629635, 0, 5.82333333333333,
			 4, 5.168, -0.978962962962963, 0, 0.323629629629635, 0, 5.82333333333333,
			 5, 5.168))
})

test_that("Feature Importance Metrics table results match", {
	table <- results[["results"]][["featureImportanceTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(1.21020786888865, 38.0537799528193, "Petal.Length", 0.316663949793413,
			 27.0512145757422, "Petal.Width", 0.316663949793413, 21.2388630350472,
			 "Species", 0.396327064485046, 13.6561424363913, "Sepal.Width"
			))
})
