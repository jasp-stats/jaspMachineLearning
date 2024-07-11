context("Machine Learning Boosting Classification")

# Test fixed model #########################################################
options <- initMlOptions("mlClassificationBoosting")
options$holdoutData <- "holdoutManual"
options$modelOptimization <- "manual"
options$modelValid <- "validationManual"
options$predictionsColumn <- ""
options$predictors <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
options$predictors.types <- rep("scale", 4)
options$savePath <- ""
options$setSeed <- TRUE
options$target <- "Species"
options$target.types <- "nominal"
options$testIndicatorColumn <- ""
options$testSetIndicatorVariable <- ""
options$dataSplitPlot <- FALSE
options$confusionTable <- FALSE
set.seed(1)
results <- jaspTools::runAnalysis("mlClassificationBoosting", "iris.csv", options)

table <- results[["results"]][["classificationTable"]][["data"]]
jaspTools::expect_equal_tables(table,
		list(30, 120, 0.1, 0.933333333333333, 100))

# Test optimized model #########################################################
options <- initMlOptions("mlClassificationBoosting")
options$addPredictions <- FALSE
options$addIndicator <- FALSE
options$andrewsCurve <- TRUE
options$featureImportanceTable <- TRUE
options$predictionsColumn <- ""
options$classProportionsTable <- TRUE
options$savePath <- ""
options$saveModel <- FALSE
options$holdoutData <- "holdoutManual"
options$modelOptimization <- "optimized"
options$modelValid <- "validationManual"
options$noOfFolds <- 5
options$deviancePlot <- TRUE
options$outOfBagImprovementPlot <- TRUE
options$relativeInfluencePlot <- TRUE
options$predictors <- list("Alcohol", "Malic", "Ash", "Alcalinity", "Magnesium", "Phenols",
                           "Flavanoids", "Nonflavanoids", "Proanthocyanins", "Color",
                           "Hue", "Dilution", "Proline")
options$predictors.types <- rep("scale", length(options$predictors))
options$rocCurve <- TRUE
options$setSeed <- TRUE
options$target <- "Type"
options$target.types <- "nominal"
options$testDataManual <- 0.2
options$testIndicatorColumn <- ""
options$testSetIndicatorVariable <- ""
options$validationDataManual <- 0.2
options$validationMeasures <- TRUE
options$tableShap <- TRUE
options$fromIndex <- 1
options$toIndex <- 5
set.seed(1)
results <- jaspTools::runAnalysis("mlClassificationBoosting", "wine.csv", options)

test_that("Class Proportions table results match", {
  table <- results[["results"]][["classProportionsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                      list(0.331460674157303, 1, 0.371428571428571, 0.324561403508772, 0.310344827586207,
                           0.398876404494382, 2, 0.4, 0.368421052631579, 0.517241379310345,
                           0.269662921348315, 3, 0.228571428571429, 0.307017543859649,
                           0.172413793103448))
})

test_that("Confusion Matrix table results match", {
  table <- results[["results"]][["confusionTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                      list("Observed", 1, 12, 0, 1, "", 2, 1, 13, 0, "", 3, 0, 0, 8))
})

test_that("Evaluation Metrics table results match", {
  table <- results[["results"]][["validationMeasures"]][["data"]]
  jaspTools::expect_equal_tables(table,
   list(0.942857142857143, 0.951048951048951, 0.923076923076923, 0.0769230769230769,
        0.0769230769230769, 0.0454545454545455, 0.0454545454545455,
        1, 0.877622377622378, 0.954545454545455, 0.923076923076923,
        0.923076923076923, 0.371428571428571, 13, 0.954545454545455,
        4, 0.971428571428571, 1, 0.962962962962963, 0, 0.0714285714285714,
        0.0454545454545455, 0, 2, 0.941468871691272, 0.954545454545455,
        1, 0.928571428571429, 0.371428571428571, 14, 1, 13, 0.971428571428571,
        1, 0.941176470588235, 0.111111111111111, 0, 0, 0.037037037037037,
        3, 0.925184888651615, 1, 0.888888888888889, 1, 0.257142857142857,
        8, 0.962962962962963, 4, 0.961904761904762, 0.983682983682984,
        0.943168378462496, 0.0626780626780627, 0.0494505494505494, 0.0303030303030303,
        0.0274971941638608, "Average / Total", 0.914758712655088, 0.96969696969697,
        0.946031746031746, 0.942857142857143, 1, 35, 0.972502805836139,
        7))
})

test_that("Feature Contributions to Predictions for Test Set Cases table results match", {
	skip("Does not reproduce on machine KD <-> GitHub Actions")
	table <- results[["results"]][["tableShap"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(-0.0150822123508907, 0.159929656349742, 0.0417039066834324, 0.0319091531835033,
			 0.0507163099941813, 0.013427306659579, 0.116594538544877, 0.105521840234203,
			 0.0999814923383288, 0.00284520779706354, 0.0167544051680328,
			 0, -0.0336486168114327, 0.324584764525254, 1, "1 (0.915)", 5.37876421069505e-07,
			 0.0181252669779804, 1.10340389214425e-06, 0.184912951717921,
			 0.000536054987528733, 0.293751186421069, 0.0546718264256107,
			 7.1285765782525e-06, 5.00288138793881e-05, 1.42973015537784e-07,
			 1.65533451035316e-06, 0, 0.123354284265876, 0.324584764525254,
			 2, "1 (1)", 2.42044409766162e-05, 0.0123209555645536, 5.6333492063354e-06,
			 0.156003257147095, 7.45997318882807e-05, 0.353846301230481,
			 0.0295431352261265, 4.00556964907928e-06, 0.000222745823264647,
			 5.76329424184507e-07, 3.64201702562905e-06, 0, 0.123354284265876,
			 0.324584764525254, 3, "1 (1)", 2.42044409766162e-05, 0.0123209555645536,
			 5.6333492063354e-06, 0.156003257147095, 7.45997318882807e-05,
			 0.353846301230481, 0.0295431352261265, 4.00556964907928e-06,
			 0.000222745823264647, 5.76329424184507e-07, 3.64201702562905e-06,
			 0, 0.123354284265876, 0.324584764525254, 4, "1 (1)", -2.09319118182894e-06,
			 0.00968685617741993, 5.69083017809557e-06, 0.156003257147095,
			 7.54586161424431e-05, 0.353846301230481, 0.0322349904847964,
			 2.13704773278645e-05, 0.000173208048563356, 6.39177424877069e-07,
			 4.23279087402584e-06, 0, 0.123354284265876, 0.324584764525254,
			 5, "1 (1)"))
})

test_that("Data Split plot matches", {
  plotName <- results[["results"]][["plotDataSplit"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "data-split")
})

# Tests specific for windows and linux
test_that("Andrews Curves Plot matches", {
  testthat::skip_on_os("mac")
  plotName <- results[["results"]][["andrewsCurve"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "andrews-curves-plot")
})

test_that("Relative Influence table results match", {
  testthat::skip_on_os("mac")
  table <- results[["results"]][["featureImportanceTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                      list(16.3458735499254, "Proline", 31.1797927388636, 1.92298108138859, "Color", 20.173253110114, 6.11564217900192, "Flavanoids",
                           19.8438729446546, 0.580073791483201, "Alcohol", 11.3080195393476, 1.46276253431049, "Hue", 9.04159967448256,
                           0.170561653950044, "Dilution", 5.78903515515603, 0.0655054520688096, "Phenols", 2.06207785543184, 0.134398642455039, "Malic",
                           0.318048182535448, 0.0796903953021984, "Magnesium", 0.198970273683845, 0.0820480575235912, "Ash", 0.0761054305928261,
                           0.0583723130067498, "Nonflavanoids", 0.00505359720114565, 0.0642402307210398, "Alcalinity", 0.00416148218437122,
                           0.0549408459183189, "Proanthocyanins", 1.00157520318044e-05))
})

test_that("Boosting Classification table results match", {
  testthat::skip_on_os("mac")
  table <- results[["results"]][["classificationTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                      list(35, 114, 29, 0.1, 0.942857142857143, 84, 0.96551724137931))
})

test_that("Deviance Plot matches", {
  testthat::skip_on_os("mac")
  plotName <- results[["results"]][["deviancePlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "deviance-plot")
})

test_that("Out-of-bag Improvement Plot matches", {
  testthat::skip_on_os("mac")
  plotName <- results[["results"]][["outOfBagImprovementPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "out-of-bag-improvement-plot")
})

test_that("Relative Influence Plot matches", {
  testthat::skip_on_os("mac")
  plotName <- results[["results"]][["relativeInfluencePlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "relative-influence-plot")
})

test_that("ROC Curves Plot matches", {
  testthat::skip_on_os("mac")
  plotName <- results[["results"]][["rocCurve"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "roc-curves-plot")
})

# Tests specific for mac
test_that("Andrews Curves Plot matches", {
  testthat::skip_on_os(c("windows", "linux"))
  plotName <- results[["results"]][["andrewsCurve"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "andrews-curves-plot-mac")
})

test_that("Feature Importance Metrics table results match", {
	testthat::skip_on_os(c("windows", "linux"))
	table <- results[["results"]][["featureImportanceTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(16.6628167288637, "Proline", 31.1710479606241, 2.30906967311753,
			 "Color", 20.2027929087086, 5.40187240801467, "Flavanoids", 18.9322276092634,
			 0.571452911587973, "Alcohol", 11.3558373164206, 1.28390495673821,
			 "Hue", 9.00503138188843, 0.202003460109013, "Dilution", 6.60449049492057,
			 0.0767927443803931, "Phenols", 2.0342983889147, 0.16239385993903,
			 "Malic", 0.349135278911687, 0.0887765925280579, "Magnesium",
			 0.194500356950665, 0.0894373829126091, "Ash", 0.0798365790091133,
			 0.0756253804363032, "Alcalinity", 0.0659567852299842, 0.0662982852417071,
			 "Nonflavanoids", 0.00484493915816381, 0.0625753440706065, "Proanthocyanins",
			 0))
})

test_that("Boosting Classification table results match", {
  testthat::skip_on_os(c("windows", "linux"))
  table <- results[["results"]][["classificationTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                      list(35, 114, 29, 0.1, 0.942857142857143, 83, 0.96551724137931))
})

test_that("Deviance Plot matches", {
  testthat::skip_on_os(c("windows", "linux"))
  plotName <- results[["results"]][["deviancePlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "deviance-plot-mac")
})

test_that("Out-of-bag Improvement Plot matches", {
  testthat::skip_on_os(c("windows", "linux"))
  plotName <- results[["results"]][["outOfBagImprovementPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "out-of-bag-improvement-plot-mac")
})

test_that("Relative Influence Plot matches", {
  testthat::skip_on_os(c("windows", "linux"))
  plotName <- results[["results"]][["relativeInfluencePlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "relative-influence-plot-mac")
})

test_that("ROC Curves Plot matches", {
  testthat::skip_on_os(c("windows", "linux"))
  plotName <- results[["results"]][["rocCurve"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "roc-curves-plot-mac")
})
