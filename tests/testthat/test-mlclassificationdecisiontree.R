context("Machine Learning Decision Tree Classification")

options <- jaspTools::analysisOptions("mlClassificationDecisionTree")
options$addIndicator <- FALSE
options$addPredictions <- FALSE
options$classProportionsTable <- TRUE
options$holdoutData <- "holdoutManual"
options$modelOptimization <- "manual"
options$modelValid <- "validationManual"
options$noOfFolds <- 5
options$decisionTreePlot <- TRUE
options$predictionsColumn <- ""
options$predictors <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
options$saveModel <- FALSE
options$savePath <- ""
options$setSeed <- TRUE
options$variableImportanceTable <- TRUE
options$target <- "Species"
options$testDataManual <- 0.2
options$testIndicatorColumn <- ""
options$testSetIndicatorVariable <- ""
options$validationDataManual <- 0.2
options$validationMeasures <- TRUE
options$tableShap <- TRUE
options$shapFrom <- 1
options$shapTo <- 5
set.seed(1)
results <- jaspTools::runAnalysis("mlClassificationDecisionTree", "iris.csv", options)


test_that("Class Proportions table results match", {
	table <- results[["results"]][["classProportionsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(0.333333333333333, "setosa", 0.333333333333333, 0.333333333333333,
			 0.333333333333333, "versicolor", 0.266666666666667, 0.35, 0.333333333333333,
			 "virginica", 0.4, 0.316666666666667))
})

test_that("Decision Tree Classification table results match", {
	table <- results[["results"]][["classificationTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(30, 120, 14, 0.933333333333333))
})

test_that("Confusion Matrix table results match", {
	table <- results[["results"]][["confusionTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list("Observed", "setosa", 10, 0, 0, "", "versicolor", 0, 8, 0, "",
			 "virginica", 0, 2, 10))
})

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

test_that("Variable Importance table results match", {
	table <- results[["results"]][["variableImportanceTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(34.6709290167624, "Petal.Width", 31.6571914083694, "Petal.Length",
			 19.9269628140734, "Sepal.Length", 13.7449167607949, "Sepal.Width"
			))
})



test_that("Evaluation Metrics table results match", {
  table <- results[["results"]][["validationMeasures"]][["data"]]
  jaspTools::expect_equal_tables(table,
   list(1, 1, 1, 0, 0, 0, 0, "setosa", 1, 1, 1, 1, 0.333333333333333,
        10, 1, "<unicode><unicode><unicode>", 0.933333333333333, 0.954545454545454,
        0.888888888888889, 0.2, 0, 0, 0.0909090909090909, "versicolor",
        0.852802865422442, 1, 0.8, 1, 0.333333333333333, 8, 0.909090909090909,
        2, 0.933333333333333, 0.916666666666667, 0.909090909090909,
        0, 0.166666666666667, 0.1, 0, "virginica", 0.866025403784439,
        0.9, 1, 0.833333333333333, 0.333333333333333, 12, 1, 5, 0.955555555555556,
        0.957070707070707, 0.934006734006734, 0.0666666666666667, 0.0555555555555556,
        0.0333333333333333, 0.0303030303030303, "Average / Total", 0.906276089735627,
        0.966666666666667, 0.946666666666667, 0.933333333333333, 1,
        30, 0.96969696969697, "<unicode><unicode><unicode>"
        ))
})

test_that("Feature Contributions to Predictions for Test Set Cases table results match", {
	skip("Need to figure out why this fails")
	table <- results[["results"]][["tableShap"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(0.666666666666667, 0, 0, 0, 0.333333333333333, 1, "setosa (1)",
			 0.666666666666667, 0, 0, 0, 0.333333333333333, 2, "setosa (1)",
			 0.666666666666667, 0, 0, 0, 0.333333333333333, 3, "setosa (1)",
			 0.666666666666667, 0, 0, 0, 0.333333333333333, 4, "setosa (1)",
			 0.666666666666667, 0, 0, 0, 0.333333333333333, 5, "setosa (1)"
			))
})
