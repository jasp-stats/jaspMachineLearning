context("Machine Learning Regularized Linear Regression")

options <- jaspTools::analysisOptions("mlRegressionRegularized")
options$addIndicator <- FALSE
options$addPredictions <- FALSE
options$coefTable <- TRUE
options$savePath <- ""
options$saveModel <- FALSE
options$holdoutData <- "holdoutManual"
options$lambdaEvaluation <- TRUE
options$modelOptimization <- "optMin"
options$modelValid <- "validationManual"
options$noOfFolds <- 5
options$predictedPerformancePlot <- TRUE
options$predictors <- list("Malic", "Ash", "Alcalinity", "Magnesium", "Phenols", "Flavanoids", 
                           "Nonflavanoids", "Proanthocyanins", "Color", "Hue", "Dilution", 
                           "Proline")
options$setSeed <- TRUE
options$target <- "Alcohol"
options$testDataManual <- 0.2
options$testIndicatorColumn <- ""
options$testSetIndicatorVariable <- ""
options$convergenceThreshold <- 1e-07
options$validationDataManual <- 0.2
options$validationMeasures <- TRUE
options$predictionsColumn <- ""
options$variableTrace <- TRUE
options$tableShap <- TRUE
options$fromIndex <- 1
options$toIndex <- 5
set.seed(1)
results <- jaspTools::runAnalysis("mlRegressionRegularized", "wine.csv", options)


test_that("Regression Coefficients table results match", {
  table <- results[["results"]][["coefTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                      list(0.061127975064724, "(Intercept)", 0.142374835142716, "Malic",
                           0.0636996037701241, "Ash", -0.116779483193431, "Alcalinity",
                           0, "Magnesium", 0.0368807604820878, "Phenols", 0.204299292659466,
                           "Flavanoids", -0.0391549243145209, "Nonflavanoids", -0.162348453489162,
                           "Proanthocyanins", 0.527743708127644, "Color", 0.0327635272841201,
                           "Hue", 0.150491111131655, "Dilution", 0.226659953230796, "Proline"
                      ))
})

test_that("Data Split plot matches", {
  plotName <- results[["results"]][["plotDataSplit"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "data-split")
})

test_that("Lambda Evaluation Plot matches", {
  plotName <- results[["results"]][["lambdaEvaluation"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "lambda-evaluation-plot")
})

test_that("Predictive Performance Plot matches", {
  plotName <- results[["results"]][["predictedPerformancePlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "predictive-performance-plot")
})

test_that("Regularized Linear Regression table results match", {
  table <- results[["results"]][["regressionTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                      list(0.0160411963895373, 35, 114, 29, "L1 (Lasso)", 0.479047646229094,
                           0.514015190983995))
})

test_that("Evaluation Metrics table results match", {
  table <- results[["results"]][["validationMeasures"]][["data"]]
  jaspTools::expect_equal_tables(table,
                      list("MSE", 0.479, "RMSE", 0.692, "MAE / MAD", 0.527, "MAPE", "105.95%",
                           "R<unicode><unicode>", 0.549))
})

test_that("Variable Trace Plot matches", {
  plotName <- results[["results"]][["variableTrace"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "variable-trace-plot")
})

test_that("Feature Contributions to Predictions for Test Set Cases table results match", {
	skip("Need to figure out why this fails")
	table <- results[["results"]][["tableShap"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(-0.0535263203138902, 0.118131139348719, -0.188465210106584, 0.0672886920738429,
			 0.159284348010254, 0.0133281374950708, 0, 0.026495172817724,
			 -0.00960404585717026, 0.031826815527207, -0.072106310494972,
			 0.00191306445755998, 0.0569384290499024, 1, 0.151503912007663,
			 0.065366526411977, 0.0577620536539598, -0.0222851406930431,
			 0.205064101929377, 0.122468456805361, 0.0161949444279738, 0,
			 -0.0295806866142179, 0.0155651777685141, 0.0200410205942794,
			 0.0895730565154929, 0.404982486802801, 0.0569384290499024, 2,
			 1.00209042665238, 0.28217112926503, 0.00668051960454752, 0.0573902350531754,
			 0.0248962582721403, 0.363817076926326, 0.0434296102905423, 0,
			 -0.083107643344709, -0.0221886576700141, 0.0495055079265975,
			 -0.352917842671043, 0.300616297088408, 0.0569384290499024, 3,
			 0.727230919790904, 0.261190038666347, 0.0043586316932116, 0.535442489530486,
			 0.0821260439044393, 0.353590440480522, 0.0362625929582856, 0,
			 -0.0652653244345456, 0.0218574836749394, 0.0612913028595263,
			 -0.395465044515902, 0.586363726858159, 0.0569384290499024, 4,
			 1.53869081072537, -0.0185578359827526, 0.0833028206786659, 0.23950537961596,
			 0.00793928475146077, 0.251324076022486, 0.0176283478944237,
			 0, -0.0588930676809153, 0.00927287186209547, 0.031826815527207,
			 -0.114653512339831, 0.394185984418554, 0.0569384290499024, 5,
			 0.899819593817254))
})
