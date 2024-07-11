context("Machine Learning Naive Bayes Classification")

# Test fixed model #############################################################
options <- initMlOptions("mlClassificationNaiveBayes")
options$addIndicator <- FALSE
options$addPredictions <- FALSE
options$classProportionsTable <- TRUE
options$holdoutData <- "holdoutManual"
options$modelOptimization <- "manual"
options$modelValid <- "validationManual"
options$noOfFolds <- 5
options$predictionsColumn <- ""
options$predictors <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
options$predictors.types <- rep("scale", 4)
options$saveModel <- FALSE
options$savePath <- ""
options$setSeed <- TRUE
options$supportVectorsTable <- TRUE
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
options$tablePosterior <- TRUE
set.seed(1)
results <- jaspTools::runAnalysis("mlClassificationNaiveBayes", "iris.csv", options)

test_that("Class Proportions table results match", {
	table <- results[["results"]][["classProportionsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(0.333333333333333, "setosa", 0.333333333333333, 0.333333333333333,
			 0.333333333333333, "versicolor", 0.266666666666667, 0.35, 0.333333333333333,
			 "virginica", 0.4, 0.316666666666667))
})

test_that("Naive Bayes Classification table results match", {
	table <- results[["results"]][["classificationTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(0, 30, 120, 0.933333333333333))
})

test_that("Confusion Matrix table results match", {
	table <- results[["results"]][["confusionTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list("Observed", "setosa", 10, 0, 0, "", "versicolor", 0, 8, 0, "",
			 "virginica", 0, 2, 10))
})

test_that("Feature Importance Metrics table results match", {
	table <- results[["results"]][["featureImportanceTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(367.294111641075, "Petal.Width", 327.44389556469, "Petal.Length",
			 12.389624058683, "Sepal.Length", 12.1155516071998, "Sepal.Width"
			))
})

test_that("Data Split plot matches", {
	plotName <- results[["results"]][["plotDataSplit"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "data-split")
})

test_that("Feature: Sepal.Length table results match", {
	table <- results[["results"]][["tablePosterior"]][["collection"]][["tablePosterior_feature1"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list("setosa", -1.04862800686438, 0.426962749287489, "versicolor",
			 0.0885597548983344, 0.657086555415105, "virginica", 0.929665560990182,
			 0.811889042216069))
})

test_that("Feature: Sepal.Width table results match", {
	table <- results[["results"]][["tablePosterior"]][["collection"]][["tablePosterior_feature2"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list("setosa", 0.780438126147034, 0.915485516118744, "versicolor",
			 -0.683258912427182, 0.727388010910457, "virginica", -0.0772005634982874,
			 0.722438739863254))
})

test_that("Feature: Petal.Length table results match", {
	table <- results[["results"]][["tablePosterior"]][["collection"]][["tablePosterior_feature3"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list("setosa", -1.30034685173936, 0.0857546418235293, "versicolor",
			 0.280055201476138, 0.283165878623355, "virginica", 1.02108883463916,
			 0.324893053996134))
})

test_that("Feature: Petal.Width table results match", {
	table <- results[["results"]][["tablePosterior"]][["collection"]][["tablePosterior_feature4"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list("setosa", -1.25857507756316, 0.14185991996451, "versicolor", 0.175798186650587,
			 0.269748295281221, "virginica", 1.11601236898582, 0.340677934597233
			))
})

test_that("Additive Explanations for Predictions of Test Set Cases table results match", {
	table <- results[["results"]][["tableShap"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(0.666655074505453, 1.30432331602037e-11, 1.06145057998663e-06,
			 1.05307106397223e-05, 0.333333333320284, 1, "setosa (1)", 0.666633012004413,
			 1.30432331602037e-11, 1.49072378297088e-06, 3.21639384768257e-05,
			 0.333333333320284, 2, "setosa (1)", 0.666522114033375, 1.30432331602037e-11,
			 0.000144552633280504, 1.7430501486615e-14, 0.333333333320284,
			 3, "setosa (1)", 0.664692021290772, 1.06680775324719e-11, 2.0288353114406e-05,
			 0.00195435702516034, 0.333333333320284, 4, "setosa (1)", 0.65856041509887,
			 1.29588562103322e-11, 0.000265658219975262, 0.00784059334768517,
			 0.333333333320284, 5, "setosa (1)"))
})

test_that("Model Performance Metrics table results match", {
	table <- results[["results"]][["validationMeasures"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(1, 1, 1, 0, 0, 0, 0, "setosa", 1, 1, 1, 1, 0.333333333333333,
			 10, 1, "<unicode>", 0.933333333333333, 0.931818181818182, 0.888888888888889,
			 0.2, 0, 0, 0.0909090909090909, "versicolor", 0.852802865422442,
			 1, 0.8, 1, 0.333333333333333, 8, 0.909090909090909, 2, 0.933333333333333,
			 0.958333333333333, 0.909090909090909, 0, 0.166666666666667,
			 0.1, 0, "virginica", 0.866025403784439, 0.9, 1, 0.833333333333333,
			 0.333333333333333, 12, 1, 5, 0.955555555555556, 0.963383838383838,
			 0.934006734006734, 0.0666666666666667, 0.0555555555555556, 0.0333333333333333,
			 0.0303030303030303, "Average / Total", 0.906276089735627, 0.966666666666667,
			 0.946666666666667, 0.933333333333333, 1, 30, 0.96969696969697,
			 "<unicode>"))
})
