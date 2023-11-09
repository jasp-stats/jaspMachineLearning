context("Machine Learning Decision Tree Classification")

# Test fixed model #########################################################
options <- initMlOptions("mlClassificationDecisionTree")
options$holdoutData <- "holdoutManual"
options$modelOptimization <- "manual"
options$modelValid <- "validationManual"
options$predictionsColumn <- ""
options$predictors <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
options$savePath <- ""
options$setSeed <- TRUE
options$target <- "Species"
options$testIndicatorColumn <- ""
options$testSetIndicatorVariable <- ""
options$dataSplitPlot <- FALSE
options$confusionTable <- FALSE
set.seed(1)
results <- jaspTools::runAnalysis("mlClassificationDecisionTree", "iris.csv", options)

table <- results[["results"]][["classificationTable"]][["data"]]
jaspTools::expect_equal_tables(table,
		list(30, 120, 0.01, 14, 0.933333333333333))

# Test optimized model #########################################################
options <- initMlOptions("mlClassificationDecisionTree")
options$addIndicator <- FALSE
options$addPredictions <- FALSE
options$classProportionsTable <- TRUE
options$holdoutData <- "holdoutManual"
options$modelOptimization <- "optimized"
options$modelValid <- "validationManual"
options$noOfFolds <- 5
options$decisionTreePlot <- TRUE
options$predictionsColumn <- ""
options$predictors <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
options$saveModel <- FALSE
options$savePath <- ""
options$setSeed <- TRUE
options$featureImportanceTable <- TRUE
options$target <- "Species"
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
results <- jaspTools::runAnalysis("mlClassificationDecisionTree", "iris.csv", options)


test_that("Class Proportions table results match", {
	table <- results[["results"]][["classProportionsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(0.333333333333333, "setosa", 0.333333333333333, 0.3125, 0.416666666666667,
			 0.333333333333333, "versicolor", 0.266666666666667, 0.354166666666667,
			 0.333333333333333, 0.333333333333333, "virginica", 0.4, 0.333333333333333,
			 0.25))
})

test_that("Decision Tree Classification table results match", {
	table <- results[["results"]][["classificationTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(30, 96, 24, 0, 14, 0.933333333333333, 0.916666666666667))
})

test_that("Confusion Matrix table results match", {
	table <- results[["results"]][["confusionTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list("Observed", "setosa", 10, 0, 0, "", "versicolor", 0, 8, 0, "",
			 "virginica", 0, 2, 10))
})

test_that("Decision Tree Plot matches", {
	plotName <- results[["results"]][["decisionTreePlot"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "decision-tree-plot")
})

test_that("Feature Importance Metrics table results match", {
	table <- results[["results"]][["featureImportanceTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(228.509889886484, 34.9229388038115, "Petal.Width", 379.227393851953,
			 32.8034648431739, "Petal.Length", 7.60641458623834, 19.9951618453291,
			 "Sepal.Length", 7.60641458623834, 12.2784345076854, "Sepal.Width"
			))
})

test_that("Classification Accuracy Plot matches", {
	plotName <- results[["results"]][["optimPlot"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "classification-accuracy-plot")
})

test_that("Data Split plot matches", {
	plotName <- results[["results"]][["plotDataSplit"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "data-split")
})

test_that("Additive Explanations for Predictions of Test Set Cases table results match", {
	table <- results[["results"]][["tableShap"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(0.6875, 0, 0, 0, 0.3125, 1, "setosa (1)", 0.6875, 0, 0, 0, 0.3125,
			 2, "setosa (1)", 0.6875, 0, 0, 0, 0.3125, 3, "setosa (1)", 0.6875,
			 0, 0, 0, 0.3125, 4, "setosa (1)", 0.6875, 0, 0, 0, 0.3125, 5,
			 "setosa (1)"))
})

test_that("Model Performance Metrics table results match", {
	table <- results[["results"]][["validationMeasures"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(1, 1, 1, 0, 0, 0, 0, "setosa", 1, 1, 1, 1, 0.333333333333333,
			 10, 1, "<unicode>", 0.933333333333333, 0.954545454545454, 0.888888888888889,
			 0.2, 0, 0, 0.0909090909090909, "versicolor", 0.852802865422442,
			 1, 0.8, 1, 0.333333333333333, 8, 0.909090909090909, 2, 0.933333333333333,
			 0.916666666666667, 0.909090909090909, 0, 0.166666666666667,
			 0.1, 0, "virginica", 0.866025403784439, 0.9, 1, 0.833333333333333,
			 0.333333333333333, 12, 1, 5, 0.955555555555556, 0.957070707070707,
			 0.934006734006734, 0.0666666666666667, 0.0555555555555556, 0.0333333333333333,
			 0.0303030303030303, "Average / Total", 0.906276089735627, 0.966666666666667,
			 0.946666666666667, 0.933333333333333, 1, 30, 0.96969696969697,
			 "<unicode>"))
})
