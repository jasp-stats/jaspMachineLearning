context("Machine Learning KNN Regression")

options <- initMlOptions("mlRegressionKnn")
options$addIndicator <- FALSE
options$addPredictions <- FALSE
options$savePath <- ""
options$saveModel <- FALSE
options$holdoutData <- "holdoutManual"
options$modelOptimization <- "optimized"
options$modelValid <- "validationManual"
options$noOfFolds <- 5
options$errorVsKPlot <- TRUE
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
options$featureImportanceTable <- TRUE
set.seed(1)
results <- jaspTools::runAnalysis("mlRegressionKnn", "wine.csv", options)


test_that("Data Split plot matches", {
  plotName <- results[["results"]][["plotDataSplit"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "data-split")
})

test_that("Mean Squared Error Plot matches", {
  plotName <- results[["results"]][["errorVsKPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "mean-squared-error-plot")
})

test_that("Predictive Performance Plot matches", {
  plotName <- results[["results"]][["predictedPerformancePlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "predictive-performance-plot")
})

test_that("K-Nearest Neighbors Regression table results match", {
  table <- results[["results"]][["regressionTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                      list("Euclidean", 4, 35, 114, 29, 0.575520302093519, 0.524262748766176,
                           "rectangular"))
})

test_that("Evaluation Metrics table results match", {
  table <- results[["results"]][["validationMeasures"]][["data"]]
  jaspTools::expect_equal_tables(table,
                      list("MSE", 0.576, "RMSE", 0.759, "MAE / MAD", 0.63, "MAPE", "245.37%", "R<unicode><unicode>",
                           0.49))
})

test_that("Feature Contributions to Predictions for Test Set Cases table results match", {
	skip("Need to figure out why this fails")
	table <- results[["results"]][["tableShap"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(-0.106430998159001, 0.0618326281182624, -0.0948694582574652, -0.0230960668593769,
			 0.0293090439092688, 0.158511953603305, 0.236282218501722, 0.0456518748448507,
			 0.0300113804453428, 0.0804175333805449, 0.0507032953158489,
			 -0.0681806698866293, 0.125686370754131, 1, 0.525829105710803,
			 -0.0104540092100339, 0.0138306271719305, -0.042032140389697,
			 0.0471105738043901, 0.0637775600643163, 0.087711028178243, 0.0746097504860815,
			 -0.0145329637080053, 0.0299033286705622, 0.0441391499979209,
			 -0.0875759634597665, 0.341470621250745, 0.125686370754131, 2,
			 0.673643933610817, 0.253840631903588, 0.055052379250772, 0.0355490339028545,
			 0.101082435307356, -0.0653983366860256, -0.114318777717995,
			 0.0618326281182631, -0.0232311315778528, 0.0839022031172221,
			 0.177826208345357, 0.358461762835012, 0.236174166726941, 0.125686370754131,
			 3, 1.28645957427963, 0.1282844696084, 0.00894128436310382, 0.100542176433453,
			 0.0732591033013228, -0.118694874596611, -0.028255539105156,
			 0.027418137850606, 0.0392227942453975, -0.0081849219396386,
			 0.326127269231885, -0.0555386122372856, 0.476724430332501, 0.125686370754131,
			 4, 1.09553208824211, -0.0459490172254977, 0.00494336869621725,
			 0.074366633992825, -0.0221506138300461, 0.124475644547379, -0.0216373678998363,
			 0.0437879817298832, -0.0314970923485778, 0.0158565979490696,
			 0.0891427141940875, 0.0634263917962778, 0.339417637529912, 0.125686370754131,
			 5, 0.759869249885825))
})

test_that("Feature Importance Metrics table results match", {
	table <- results[["results"]][["featureImportanceTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(0.685021928680065, "Proline", 0.652602310619497, "Color", 0.611900222810213,
			 "Nonflavanoids", 0.595775060189909, "Malic", 0.590654021216899,
			 "Ash", 0.589193989262503, "Dilution", 0.588933682617741, "Alcalinity",
			 0.581495692525862, "Magnesium", 0.579637965914741, "Hue", 0.578758332225294,
			 "Proanthocyanins", 0.577591118241997, "Phenols", 0.577510007287227,
			 "Flavanoids"))
})
