context("Machine Learning Neural Network Regression")

# Test fixed model #############################################################
options <- initMlOptions("mlRegressionNeuralNetwork")
options$addIndicator <- FALSE
options$addPredictions <- FALSE
options$algorithm <- "rprop+"
options$coefficientsTable <- TRUE
options$dataSplitPlot <- FALSE
options$holdoutData <- "holdoutManual"
options$layers <- list(list(nodes = 1, value = "#"))
options$modelOptimization <- "manual"
options$modelValid <- "validationManual"
options$predictionsColumn <- ""
options$predictors <- c("Sepal.Width", "Petal.Length", "Petal.Width")
options$predictors.types <- rep("scale", 3)
options$saveModel <- FALSE
options$savePath <- ""
options$setSeed <- TRUE
options$target <- "Sepal.Length"
options$target.types <- "scale"
options$testDataManual <- 0.2
options$testIndicatorColumn <- ""
options$testSetIndicatorVariable <- ""
options$threshold <- 0.05
options$validationDataManual <- 0.2
options$validationMeasures <- TRUE
options$tableShap <- TRUE
options$fromIndex <- 1
options$toIndex <- 5
options$featureImportanceTable <- TRUE
set.seed(1)
results <- jaspTools::runAnalysis("mlRegressionNeuralNetwork", "iris.csv", options)


test_that("Network Weights table results match", {
	table <- results[["results"]][["coefficientsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list("", "Intercept", "<unicode>", 1, "Hidden 1", -0.459053558328494,
			 "input", "Sepal.Width", "<unicode>", 1, "Hidden 1", 0.166055410149693,
			 "input", "Petal.Length", "<unicode>", 1, "Hidden 1", 0.784888387189343,
			 "input", "Petal.Width", "<unicode>", 1, "Hidden 1", -0.296163895895148,
			 "", "Intercept", "<unicode>", "output", "Sepal.Length", 2.91282840238973,
			 1, "Hidden 1", "<unicode>", "output", "Sepal.Length", 7.4557636688605
			))
})

test_that("Neural Network Regression table results match", {
	table <- results[["results"]][["regressionTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(1, 30, 120, 1, 0.0887985623673364))
})

test_that("Model Performance Metrics table results match", {
	table <- results[["results"]][["validationMeasures"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list("MSE", 0.089, "MSE(scaled)", 0.156, "RMSE", 0.298, "MAE / MAD", 0.243, "MAPE", "4.18%",
			 "R<unicode>", 0.845))
})

test_that("Additive Explanations for Predictions of Test Set Cases table results match", {
	table <- results[["results"]][["tableShap"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(-1.50657470219107, 0.440406134738438, 0.292162548262356, 5.82291787731192,
			 1, 5.04891185812166, -1.45649928327683, 0.452400878665353, 0.358105053342753,
			 5.82291787731192, 2, 5.17692452604319, -1.40511272518823, 0.464433833318559,
			 0.186125039180594, 5.82291787731192, 3, 5.06836402462283, -1.55534335961616,
			 0.406228474472487, 0.363122598033607, 5.82291787731192, 4, 5.03692559020185,
			 -1.35241281180798, 0.419827259413805, 0.42794844160868, 5.82291787731192,
			 5, 5.31828076652644))
})

test_that("Feature Importance Metrics table results match", {
	table <- results[["results"]][["featureImportanceTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(1.80936927092479, "Petal.Length", 0.719874353320246, "Petal.Width",
			 0.496225742289646, "Sepal.Width"))
})
