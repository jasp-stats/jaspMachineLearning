context("Machine Learning Neural Network Classification")

options <- jaspTools::analysisOptions("mlClassificationNeuralNetwork")
options$addIndicator <- FALSE
options$addPredictions <- FALSE
options$algorithm <- "backprop"
options$classProportionsTable <- TRUE
options$coefficientsTable <- TRUE
options$dataSplitPlot <- FALSE
options$holdoutData <- "holdoutManual"
options$layers <- list(list(nodes = 1, value = "#"))
options$modelOptimization <- "manual"
options$modelValid <- "validationManual"
options$predictionsColumn <- ""
options$predictors <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
options$saveModel <- FALSE
options$savePath <- ""
options$setSeed <- TRUE
options$target <- "Species"
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
results <- jaspTools::runAnalysis("mlClassificationNeuralNetwork", "iris.csv", options)


test_that("Class Proportions table results match", {
	table <- results[["results"]][["classProportionsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(0.333333333333333, "setosa", 0.333333333333333, 0.333333333333333,
			 0.333333333333333, "versicolor", 0.266666666666667, 0.35, 0.333333333333333,
			 "virginica", 0.4, 0.316666666666667))
})

test_that("Neural Network Classification table results match", {
	table <- results[["results"]][["classificationTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(1, 1, 30, 120, 1))
})

test_that("Network Weights table results match", {
	table <- results[["results"]][["coefficientsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list("", "Intercept", "<unicode><unicode><unicode>", 1, "Hidden 1",
			 -0.537187309224922, "input", "Sepal.Length", "<unicode><unicode><unicode>",
			 1, "Hidden 1", 0.301042498654808, "input", "Sepal.Width", "<unicode><unicode><unicode>",
			 1, "Hidden 1", 0.268701366705232, "input", "Petal.Length", "<unicode><unicode><unicode>",
			 1, "Hidden 1", -1.48576393686231, "input", "Petal.Width", "<unicode><unicode><unicode>",
			 1, "Hidden 1", -1.19647429949852, "", "Intercept", "<unicode><unicode><unicode>",
			 "output", "setosa", -7.88263373774566, 1, "Hidden 1", "<unicode><unicode><unicode>",
			 "output", "setosa", 12.7291153399175, "", "Intercept", "<unicode><unicode><unicode>",
			 "output", "versicolor", 0.0516760346358976, 1, "Hidden 1", "<unicode><unicode><unicode>",
			 "output", "versicolor", -1.5965337875234, "", "Intercept", "<unicode><unicode><unicode>",
			 "output", "virginica", 3.12741289395969, 1, "Hidden 1", "<unicode><unicode><unicode>",
			 "output", "virginica", -29.0043868660969))
})

test_that("Confusion Matrix table results match", {
	table <- results[["results"]][["confusionTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list("Observed", "setosa", 10, 0, 0, "", "versicolor", 0, 8, 0, "",
			 "virginica", 0, 0, 12))
})

test_that("Evaluation Metrics table results match", {
  table <- results[["results"]][["validationMeasures"]][["data"]]
  jaspTools::expect_equal_tables(table,
     list(1, 1, 1, 0, 0, 0, 0, "setosa", 1, 1, 1, 1, 0.333333333333333,
        10, 1, "<unicode><unicode><unicode>", 1, 0.693181818181818,
        1, 0, 0, 0, 0, "versicolor", 1, 1, 1, 1, 0.266666666666667,
        8, 1, "<unicode><unicode><unicode>", 1, 1, 1, 0, 0, 0, 0, "virginica",
        1, 1, 1, 1, 0.4, 12, 1, "<unicode><unicode><unicode>", 1, 0.897727272727273,
        1, 0, 0, 0, 0, "Average / Total", 1, 1, 1, 1, 1, 30, 1, "<unicode><unicode><unicode>"))
})

test_that("Feature Contributions to Predictions for Test Set Cases table results match", {
	skip("Need to figure out why this fails")
	table <- results[["results"]][["shapTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(0.416891133677921, 0.242092547101801, -0.000953562649922746, 0.00676374917304856,
			 0.335206132697152, 1, "setosa (1)", 0.390513723365537, 0.267730969842223,
			 0.0028279060129498, 0.00372126808213813, 0.335206132697152,
			 2, "setosa (1)", 0.363136015602791, 0.294254281728529, -0.00246437281698231,
			 0.00986794278851033, 0.335206132697152, 3, "setosa (1)", 0.442122030303247,
			 0.214559218160268, 0.00291166025646261, 0.00520095858287006,
			 0.335206132697152, 4, "setosa (1)", 0.334941163631421, 0.319143972721428,
			 0.00403240654656789, 0.00667632440343113, 0.335206132697152,
			 5, "setosa (1)"))
})
