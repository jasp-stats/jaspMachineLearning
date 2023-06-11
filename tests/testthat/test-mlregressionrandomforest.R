context("Machine Learning Random Forest Regression")

options <- jaspTools::analysisOptions("mlRegressionRandomForest")
options$addIndicator <- FALSE
options$addPredictions <- FALSE
options$savePath <- ""
options$saveModel <- FALSE
options$holdoutData <- "holdoutManual"
options$modelOptimization <- "optimized"
options$modelValid <- "validationManual"
options$noOfFolds <- 5
options$accuracyDecreasePlot <- TRUE
options$purityIncreasePlot <- TRUE
options$treesVsModelErrorPlot <- TRUE
options$predictedPerformancePlot <- TRUE
options$predictors <- list("Malic", "Ash", "Alcalinity", "Magnesium", "Phenols", "Flavanoids", 
                           "Nonflavanoids", "Proanthocyanins", "Color", "Hue", "Dilution", 
                           "Proline")
options$setSeed <- TRUE
options$variableImportanceTable <- TRUE
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
results <- jaspTools::runAnalysis("mlRegressionRandomForest", "wine.csv", options)


test_that("Data Split plot matches", {
  plotName <- results[["results"]][["plotDataSplit"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "data-split")
})

test_that("Mean Decrease in Accuracy plot matches", {
  plotName <- results[["results"]][["accuracyDecreasePlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "mean-decrease-in-accuracy")
})

test_that("Total Increase in Node Purity plot matches", {
  plotName <- results[["results"]][["purityIncreasePlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "total-increase-in-node-purity")
})

test_that("Out-of-bag Mean Squared Error Plot matches", {
  plotName <- results[["results"]][["treesVsModelErrorPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "out-of-bag-mean-squared-error-plot")
})

test_that("Predictive Performance Plot matches", {
  plotName <- results[["results"]][["predictedPerformancePlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "predictive-performance-plot")
})

test_that("Random Forest Regression table results match", {
  table <- results[["results"]][["regressionTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                      list(35, 114, 29, 0.398951826523355, 3, 0.484311284922699, 97, 0.611083026333054
                      ))
})

test_that("Variable Importance table results match", {
  table <- results[["results"]][["variableImportanceTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                      list(0.475727661943587, 13.9713939210907, "Color", 0.133970922782775,
                           6.27626368698486, "Proline", 0.117761043024228, 3.97309320553953,
                           "Flavanoids", 0.0538014659638475, 3.65347091537435, "Phenols",
                           0.036092478567819, 3.64937884621186, "Alcalinity", 0.0202836370094357,
                           3.14440781708078, "Magnesium", 0.044396608949806, 2.94894309954197,
                           "Malic", 0.041519532754131, 2.62081725098105, "Dilution", 0.00929571084292303,
                           2.3343823192425, "Ash", 0.0384877159897528, 2.21651993528447,
                           "Hue", 0.00439249198505879, 1.83777482063651, "Proanthocyanins",
                           0.00557107935772629, 1.46246316922245, "Nonflavanoids"))
})

test_that("Evaluation Metrics table results match", {
  table <- results[["results"]][["validationMeasures"]][["data"]]
  jaspTools::expect_equal_tables(table,
                      list("MSE", 0.484, "RMSE", 0.696, "MAE / MAD", 0.539, "MAPE", "169.83%",
                           "R<unicode><unicode>", 0.578))
})

test_that("Feature Contributions to Predictions for Test Set Cases table results match", {
	skip("Need to figure out why this fails")
	table <- results[["results"]][["tableShap"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(-0.0980762484150122, 0.0363452082600033, 0.14256335085434, -0.0150232807943395,
			 0.0581747218010062, -0.0197525397535068, 0.0327360538744631,
			 0.0850347207816432, 0.00686183438910332, 0.12797343485671, 0.0114729117196029,
			 -0.0326972066887685, 0.0665605360689242, 1, 0.402173496954169,
			 -0.0619117052951038, -0.0189812880527432, 0.21655826744696,
			 0.00548478526770774, 0.0049195394208868, 0.00593342524091228,
			 -0.0355709658404741, 0.0587875940402237, 0.023919640059417,
			 -0.00201304315408402, -0.0110588740409419, 0.216806464946292,
			 0.0665605360689242, 2, 0.469434376107977, 0.1664732148008, 0.0207495424837227,
			 0.203636079139844, 0.038584034601807, 0.133661685475323, -0.00567194637759627,
			 0.0194006961618768, 0.0969929972145696, -0.0040214876760909,
			 0.187434197885471, -0.0793127361416788, 0.170216372682028, 0.0665605360689242,
			 3, 1.014703186319, 0.130911864281952, 0.0347041719305212, 0.212698246425984,
			 -0.0148438376683649, 0.136163341327626, -0.0026020539597047,
			 0.0291283466109056, 0.0874259129891937, 0.0372395296459114,
			 0.182645092392713, -0.0470432665530166, 0.196623358165244, 0.0665605360689242,
			 4, 1.04961124165789, -0.0237978567049248, 0.0177131589793192,
			 0.211515947765325, 0.0657526564641612, 0.137111881550523, 0.0365626624567494,
			 0.0483771592810298, 0.0951062717305265, 0.00283651987932299,
			 0.149796291636588, 0.050097530053485, 0.198126435532283, 0.0665605360689242,
			 5, 1.05575919469332))
})
