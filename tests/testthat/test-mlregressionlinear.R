context("Machine Learning Linear Regression")

options <- initMlOptions("mlRegressionLinear")
options$addIndicator <- FALSE
options$addPredictions <- FALSE
options$savePath <- ""
options$saveModel <- FALSE
options$holdoutData <- "holdoutManual"
options$modelOptimization <- "manual"
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
options$coefTable <- TRUE
set.seed(1)
results <- jaspTools::runAnalysis("mlRegressionLinear", "wine.csv", options)


test_that("Regression Coefficients table results match", {
	table <- results[["results"]][["coefTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(12.992432675446, "(Intercept)", 0.149071095543274, "Malic", 0.0223033286484404,
			 "Ash", -0.0979753787055038, "Alcalinity", -0.0164426736394874,
			 "Magnesium", 0.0962033264305484, "Phenols", -0.0315735274593793,
			 "Flavanoids", -0.12099582346758, "Nonflavanoids", -0.131852616455409,
			 "Proanthocyanins", 0.44928457954439, "Color", 0.0564664898240421,
			 "Hue", 0.141817138809287, "Dilution", 0.249152433308135, "Proline"
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
		list(35, 143, 0.344518595222833))
})

test_that("Additive Explanations for Predictions of Test Set Cases table results match", {
	table <- results[["results"]][["tableShap"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(-0.0434117043096673, 0.0409554658132141, -0.147450883211382, 0.0606778248675361,
			 -0.0223079806507407, 0.0213076961138281, -0.0218656520824432,
			 0.0322960219969204, -0.028282716176868, 0.0823616834194727,
			 -0.0524528027323665, -0.00217439618415405, 12.9904895104895,
			 1, 12.9101420673529, 0.0563367391466674, 0.0198182612194469,
			 -0.00597663835931428, 0.190512166981653, -0.016618251935423,
			 0.0262485110064414, -0.0253193946241641, -0.0264172315149604,
			 0.0494947533095402, 0.0516184813077789, 0.0788564095131932,
			 0.440893406423482, 12.9904895104895, 2, 13.8299367229639, 0.23823095956703,
			 0.00193293425551033, 0.0618534790355874, 0.0207287965247325,
			 -0.0539175846247453, 0.0731862524863001, 0.0092180307932157,
			 -0.0824617007762889, -0.0671714509200783, 0.128476486586987,
			 -0.280516171369378, 0.326170493248297, 12.9904895104895, 3,
			 13.3662200352967, 0.220628293074739, 0.00111996484799093, 0.468834183404704,
			 0.0746599847875125, -0.0523371044260497, 0.0608342152547561,
			 -0.0034456918598238, -0.063780211022511, 0.0689391206811436,
			 0.159219688698668, -0.315071227223486, 0.640273917596899, 12.9904895104895,
			 4, 14.2503646443041, -0.0140739268225101, 0.0287609247014302,
			 0.216893747366672, 0.00474918518762735, -0.0365323024390367,
			 0.028718918452741, -0.0241681471102506, -0.0571082503961708,
			 0.0300503859379209, 0.0823616834194851, -0.0870078585864729,
			 0.429025518853635, 12.9904895104895, 5, 13.5921593890546))
})

test_that("Model Performance Metrics table results match", {
	table <- results[["results"]][["validationMeasures"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list("MSE", 0.345, "RMSE", 0.587, "MAE / MAD", 0.459, "MAPE", "3.56%",
			 "R<unicode>", 0.514))
})
