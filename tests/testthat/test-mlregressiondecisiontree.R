context("Machine Learning Decision Tree Regression")

# Test fixed model #############################################################
options <- initMlOptions("mlRegressionDecisionTree")
options$target <- "Sepal.Length"
options$target.types <- "scale"
options$predictors <- c("Sepal.Width", "Petal.Length", "Petal.Width")
options$predictors.types <- rep("scale", 3)
options$modelOptimization <- "manual"
options$holdoutData <- "holdoutManual"
options$modelValid <- "validationManual"
options$savePath <- ""
options$predictionsColumn <- ""
options$testIndicatorColumn <- ""
options$testSetIndicatorVariable <- ""
options$dataSplitPlot <- FALSE
options$setSeed <- TRUE
set.seed(1)
results <- jaspTools::runAnalysis("mlRegressionDecisionTree", "iris.csv", options)

table <- results[["results"]][["regressionTable"]][["data"]]
jaspTools::expect_equal_tables(table,
		list(30, 120, 0.01, 29, 0.10398915405807))

# Test optimized model #########################################################
options <- initMlOptions("mlRegressionDecisionTree")
options$addIndicator <- FALSE
options$addPredictions <- FALSE
options$holdoutData <- "holdoutManual"
options$modelOptimization <- "optimized"
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
options$target.types <- "scale"
options$testDataManual <- 0.2
options$testIndicatorColumn <- ""
options$testSetIndicatorVariable <- ""
options$validationDataManual <- 0.2
options$validationMeasures <- TRUE
options$tableShap <- TRUE
options$fromIndex <- 1
options$toIndex <- 5
options$optimPlot <- TRUE
set.seed(1)
results <- jaspTools::runAnalysis("mlRegressionDecisionTree", "iris.csv", options)


test_that("Decision Tree Plot matches", {
	plotName <- results[["results"]][["decisionTreePlot"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "decision-tree-plot")
})

test_that("Feature Importance Metrics table results match", {
	table <- results[["results"]][["featureImportanceTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(1.27712381058387, 39.9000216493892, "Petal.Length", 0.337167110036066,
			 28.6623341222225, "Petal.Width", 0.283663885556263, 25.0579150108176,
			 "Species", 0.359928871626635, 6.37972921757074, "Sepal.Width"
			))
})

test_that("Mean Squared Error Plot matches", {
	plotName <- results[["results"]][["optimPlot"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "mean-squared-error-plot")
})

test_that("Data Split plot matches", {
	plotName <- results[["results"]][["plotDataSplit"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "data-split")
})

test_that("Predictive Performance Plot matches", {
	plotName <- results[["results"]][["predictedPerformancePlot"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "predictive-performance-plot")
})

test_that("Decision Tree Regression table results match", {
	table <- results[["results"]][["regressionTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(30, 96, 24, 0, 41, 0.180120741111441, 0.156299879128076))
})

test_that("Additive Explanations for Predictions of Test Set Cases table results match", {
	table <- results[["results"]][["tableShap"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(-1.01335784313725, 0, 0.343566176470584, 0, 5.85729166666667,
			 1, 5.1875, -1.01335784313725, 0, 0.343566176470584, 0, 5.85729166666667,
			 2, 5.1875, -1.01335784313725, 0, 0.343566176470584, 0, 5.85729166666667,
			 3, 5.1875, -1.01335784313725, 0, 0.343566176470584, 0, 5.85729166666667,
			 4, 5.1875, -1.01335784313725, 0, 0.343566176470584, 0, 5.85729166666667,
			 5, 5.1875))
})

test_that("Model Performance Metrics table results match", {
	table <- results[["results"]][["validationMeasures"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list("MSE", 0.18, "MSE(scaled)", 0.349, "RMSE", 0.424, "MAE / MAD", 0.354, "MAPE", "6.01%",
			 "R<unicode>", 0.671))
})
