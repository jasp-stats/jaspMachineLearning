context("Machine Learning Logistic / Multinomial Regression Classification")

# Test fixed model #############################################################
options <- initMlOptions("mlClassificationLogisticMultinomial")
options$addIndicator <- FALSE
options$addPredictions <- FALSE
options$classProportionsTable <- TRUE
options$holdoutData <- "holdoutManual"
options$modelOptimization <- "manual"
options$modelValid <- "validationManual"
options$predictionsColumn <- ""
options$predictors <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
options$predictors.types <- rep("scale", 4)
options$saveModel <- FALSE
options$savePath <- ""
options$setSeed <- TRUE
options$target <- "Species"
options$target.types <- "nominal"
options$testDataManual <- 0.2
options$testIndicatorColumn <- ""
options$testSetIndicatorVariable <- ""
options$validationDataManual <- 0.2
options$validationMeasures <- TRUE
options$tableShap <- TRUE
options$fromIndex <- 1
options$toIndex <- 5
options$featureImportanceTable <- TRUE
set.seed(1)
results <- jaspTools::runAnalysis("mlClassificationLogisticMultinomial", "iris.csv", options)

test_that("Class Proportions table results match", {
	table <- results[["results"]][["classProportionsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(0.333333333333333, "setosa", 0.333333333333333, 0.333333333333333,
			 0.333333333333333, "versicolor", 0.266666666666667, 0.35, 0.333333333333333,
			 "virginica", 0.4, 0.316666666666667))
})

test_that("Model Summary: Multinomial Regression Classification table results match", {
	table <- results[["results"]][["classificationTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list("Multinomial", 30, 120, 1))
})

test_that("Confusion Matrix table results match", {
	table <- results[["results"]][["confusionTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list("Observed", "setosa", 10, 0, 0, "", "versicolor", 0, 8, 0, "",
			 "virginica", 0, 0, 12))
})

test_that("Data Split plot matches", {
	plotName <- results[["results"]][["plotDataSplit"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "data-split")
})

test_that("Model Performance Metrics table results match", {
	table <- results[["results"]][["validationMeasures"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(1, 1, 1, 0, 0, 0, 0, "setosa", 1, 1, 1, 1, 0.333333333333333,
			 10, 1, "<unicode>", 1, 0.613636363636364, 1, 0, 0, 0, 0, "versicolor",
			 1, 1, 1, 1, 0.266666666666667, 8, 1, "<unicode>", 1, 1, 1, 0,
			 0, 0, 0, "virginica", 1, 1, 1, 1, 0.4, 12, 1, "<unicode>", 1,
			 0.871212121212121, 1, 0, 0, 0, 0, "Average / Total", 1, 1, 1,
			 1, 1, 30, 1, "<unicode>"))
})