context("Machine Learning Linear Regression")

# Test fixed model #############################################################
options <- initMlOptions("mlRegressionLinear")
options$addIndicator <- FALSE
options$addPredictions <- FALSE
options$savePath <- ""
options$saveModel <- FALSE
options$holdoutData <- "holdoutManual"
options$modelOptimization <- "manual"
options$predictedPerformancePlot <- TRUE
options$predictors <- list("Petal.Width", "Sepal.Length", "Sepal.Width", "Species")
options$predictors.types <- rep("scale", length(options$predictors))
options$setSeed <- TRUE
options$target <- "Petal.Length"
options$target.types <- "scale"
options$testDataManual <- 0.2
options$testIndicatorColumn <- ""
options$testSetIndicatorVariable <- ""
options$validationDataManual <- 0.2
options$validationMeasures <- TRUE
options$predictionsColumn <- ""
options$tableShap <- TRUE
options$fromIndex <- 1
options$toIndex <- 5
options$coefTable <- TRUE
set.seed(1)
results <- jaspTools::runAnalysis("mlRegressionLinear", "iris.csv", options)


test_that("Regression Coefficients table results match", {
	table <- results[["results"]][["coefTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(2.7764979664412, 5.62992231822954e-36, 0.150678162687198, 18.4266778737215,
			 "(Intercept)", 0.569663326040314, 2.33686655716093e-07, 0.103537019751495,
			 5.50202553065172, "Petal.Width", 0.498144903409647, 3.07673545874542e-20,
			 0.0442219550449187, 11.2646513005509, "Sepal.Length", -0.0956708827919907,
			 0.0112932808625884, 0.0371487237432377, -2.57534776842518, "Sepal.Width",
			 1.26625363234331, 6.19287531684742e-10, 0.187328298393861, 6.7595427023043,
			 "Species (versicolor)", 1.67778302418503, 6.13639218953803e-09,
			 0.266861154480148, 6.28710097373818, "Species (virginica)"
			))
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

test_that("Linear Regression table results match", {
	table <- results[["results"]][["regressionTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(0.978181001299926, 30, 120, 0.979097765951189, 0.0792006506573426
			))
})

test_that("Additive Explanations for Predictions of Test Set Cases table results match", {
	table <- results[["results"]][["tableShap"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(-0.744242579381093, -0.495297766627452, -0.119442377594166, -0.974486728978749,
			 3.73666666666667, 1, 1.40319721408521, -0.744242579381093, -0.254667272719379,
			 -0.141391972251587, -0.974486728978749, 3.73666666666667, 2,
			 1.62187811333585, -0.744242579381093, -0.615613013581487, -0.0755431882793085,
			 -0.974486728978749, 3.73666666666667, 3, 1.32678115644603, -0.594771266367318,
			 -0.254667272719378, -0.185291161566442, -0.974486728978749,
			 3.73666666666667, 4, 1.72745023703478, -0.669506922874206, -0.0741944022883203,
			 -0.163341566909019, -0.974486728978749, 3.73666666666667, 5,
			 1.85513704561637))
})

test_that("Model Performance Metrics table results match", {
	table <- results[["results"]][["validationMeasures"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list("MSE", 0.079, "MSE(scaled)", 0.023, "RMSE", 0.281, "MAE / MAD", 0.228, "MAPE", "8.08%",
			 "R<unicode>", 0.976))
})
