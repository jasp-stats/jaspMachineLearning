context("Machine Learning Decision Tree Regression")

options <- jaspTools::analysisOptions("mlRegressionDecisionTree")
options$addIndicator <- FALSE
options$addPredictions <- FALSE
options$holdoutData <- "holdoutManual"
options$modelOptimization <- "optimizationManual"
options$modelValid <- "validationManual"
options$noOfFolds <- 5
options$decisionTreePlot <- TRUE
options$predictedPerformancePlot <- TRUE
options$predictionsColumn <- ""
options$predictors <- c("Sepal.Width", "Petal.Length", "Petal.Width", "Species")
options$saveModel <- FALSE
options$savePath <- ""
options$setSeed <- TRUE
options$variableImportanceTable <- TRUE
options$target <- "Sepal.Length"
options$testDataManual <- 0.2
options$testIndicatorColumn <- ""
options$testSetIndicatorVariable <- ""
options$validationDataManual <- 0.2
options$validationMeasures <- TRUE
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
		list(30, 120, 37, 0.151655444002406))
})

test_that("Variable Importance table results match", {
	table <- results[["results"]][["variableImportanceTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(38.0537799528194, "Petal.Length", 27.0512145757421, "Petal.Width",
			 21.2388630350472, "Species", 13.6561424363914, "Sepal.Width"
			))
})

test_that("Evaluation Metrics table results match", {
	table <- results[["results"]][["validationMeasures"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list("MSE", 0.152, "RMSE", 0.39, "MAE / MAD", 0.321, "MAPE", "106.28%", "R<unicode><unicode>",
			 0.818))
})