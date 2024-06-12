context("Machine Learning Boosting Regression")

# Test fixed model #############################################################
options <- initMlOptions("mlRegressionBoosting")
options$target <- "Sepal.Length"
options$predictors <- c("Sepal.Width", "Petal.Length", "Petal.Width")
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
results <- jaspTools::runAnalysis("mlRegressionBoosting", "iris.csv", options)

# Tests specific for windows and linux
test_that("Boosting Regression table results match", {
  testthat::skip_on_os("mac")
  table <- results[["results"]][["regressionTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
		list("Gaussian", 30, 120, 0.1, 0.10476507592285, 100))
})

# Tests specific for mac
test_that("Boosting Regression table results match", {
  testthat::skip_on_os(c("windows", "linux"))
  table <- results[["results"]][["regressionTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
		list("Gaussian", 30, 120, 0.1, 0.104957074621182, 100))
})

# Test optimized model #########################################################
options <- initMlOptions("mlRegressionBoosting")
options$addIndicator <- FALSE
options$addPredictions <- FALSE
options$featureImportanceTable <- TRUE
options$savePath <- ""
options$saveModel <- FALSE
options$holdoutData <- "holdoutManual"
options$modelOptimization <- "optimized"
options$modelValid <- "validationManual"
options$noOfFolds <- 5
options$deviancePlot <- TRUE
options$outOfBagImprovementPlot <- TRUE
options$relativeInfluencePlot <- TRUE
options$predictedPerformancePlot <- TRUE
options$predictors <- list("Malic", "Ash", "Alcalinity", "Magnesium", "Phenols", "Flavanoids", 
                           "Nonflavanoids", "Proanthocyanins", "Color", "Hue", "Dilution", 
                           "Proline")
options$setSeed <- TRUE
options$target <- "Alcohol"
options$testDataManual <- 0.2
options$testIndicatorColumn <- ""
options$testSetIndicatorVariable <- ""
options$validationDataManual <- 0.2
options$validationMeasures <- TRUE
options$predictionsColumn <- ""
options$tableShap <- TRUE
options$fromIndex <- 1
options$toIndex <- 5
set.seed(1)
results <- jaspTools::runAnalysis("mlRegressionBoosting", "wine.csv", options)


test_that("Feature Importance Metrics table results match", {
  table <- results[["results"]][["featureImportanceTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                      list(0.748565097082193, "Color", 60.3564909956942, 0.550560041765682, "Proline", 28.9359792394392, 0.485069345289906, "Phenols",
                           4.02747773147275, 0.478734027246261, "Flavanoids", 2.65410848555859, 0.475602197216243, "Hue", 1.59224895077339,
                           0.476836798618079, "Proanthocyanins", 1.14701822224105, 0.476913378504938, "Malic", 0.744077365382741,
                           0.476113986546058, "Alcalinity", 0.542599009438009, 0.47367397597535, "Ash", 0, 0.47367397597535, "Magnesium", 0, 0.47367397597535, "Nonflavanoids",
                           0, 0.47367397597535, "Dilution", 0))
})

test_that("Data Split plot matches", {
  plotName <- results[["results"]][["plotDataSplit"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "data-split")
})

test_that("Deviance Plot matches", {
  plotName <- results[["results"]][["deviancePlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "deviance-plot")
})

test_that("Out-of-bag Improvement Plot matches", {
  plotName <- results[["results"]][["outOfBagImprovementPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "out-of-bag-improvement-plot")
})

test_that("Relative Influence Plot matches", {
  plotName <- results[["results"]][["relativeInfluencePlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "relative-influence-plot")
})

test_that("Predictive Performance Plot matches", {
  plotName <- results[["results"]][["predictedPerformancePlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "predictive-performance-plot")
})

test_that("Boosting Regression table results match", {
  table <- results[["results"]][["regressionTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                      list("Gaussian", 35, 114, 29, 0.1, 0.280491345224988, 24, 0.356794331649178
                      ))
})

test_that("Evaluation Metrics table results match", {
  table <- results[["results"]][["validationMeasures"]][["data"]]
  jaspTools::expect_equal_tables(table,
                      list("MSE", 0.28, "MSE(scaled)", 0.374, "RMSE", 0.529, "MAE / MAD", 0.425, "MAPE", "3.33%",
                           "R<unicode><unicode>", 0.652))
})

test_that("Feature Contributions to Predictions for Test Set Cases table results match", {
	skip("Does not reproduce on machine KD <-> GitHub Actions")
	table <- results[["results"]][["tableShap"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(-0.00670014251730083, 0, 0.12999822317264, 0, -0.0163668300906356,
			 0.0294712645212337, 0, 0.00731990826097717, 0, 0.0855713316980962,
			 -0.0133417280043384, 0.0313569607532998, 0.0313047600892007,
			 1, 0.278613747883173, -0.00670014251730122, 0, 0.356808234264028,
			 0, -0.0163668300906357, 0.029471264521234, 0, 0.00731990826097728,
			 0, 0.0298161593778583, -0.0133417280043382, 0.351720295929325,
			 0.0313047600892007, 2, 0.770031921830347, 0.0314906698313151,
			 0, 0.356808234264028, 0, 0.0582659151226618, 0.0294712645212338,
			 0, 0.00731990826097739, 0, 0.0855713316980959, 0.0357214007858095,
			 0.351720295929325, 0.0313047600892007, 3, 0.987673780502648,
			 0.0314906698313151, 0, 0.356808234264028, 0, 0.0582659151226618,
			 0.0294712645212338, 0, 0.00731990826097739, 0, 0.0855713316980959,
			 0.0357214007858095, 0.351720295929325, 0.0313047600892007, 4,
			 0.987673780502648, -0.00670014251730122, 0, 0.356808234264028,
			 0, 0.0582659151226618, 0.0294712645212337, 0, 0.00731990826097684,
			 0, 0.0855713316980959, 0.0357214007858095, 0.351720295929325,
			 0.0313047600892007, 5, 0.949482968154031))
})
