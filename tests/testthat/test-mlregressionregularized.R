context("Machine Learning Regularized Linear Regression")

# Test fixed model #############################################################
options <- initMlOptions("mlRegressionRegularized")
options$target <- "Sepal.Length"
options$target.types <- "scale"
options$predictors <- c("Sepal.Width", "Petal.Length", "Petal.Width")
options$predictors.types <- rep("scale", 3)
options$modelOptimization <- "manual"
options$holdoutData <- "holdoutManual"
options$modelValid <- "validationManual"
options$savePath <- ""
options$predictionsColumn <- ""
options$testIndicatorColumn <- ""
options$testSetIndicatorVariable <- ""
options$setSeed <- TRUE
options$dataSplitPlot <- FALSE
set.seed(1)
results <- jaspTools::runAnalysis("mlRegressionRegularized", "iris.csv", options)

table <- results[["results"]][["regressionTable"]][["data"]]
jaspTools::expect_equal_tables(table,
		list(1, 30, 120, "L1 (Lasso)", 0.521122222222222))

# Test optimized model #########################################################
options <- initMlOptions("mlRegressionRegularized")
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
options$predictors.types <- rep("scale", length(options$predictors))
options$setSeed <- TRUE
options$target <- "Alcohol"
options$target.types <- "scale"
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
options$featureImportanceTable <- TRUE
set.seed(1)
results <- jaspTools::runAnalysis("mlRegressionRegularized", "wine.csv", options)


test_that("Regression Coefficients table results match", {
	table <- results[["results"]][["coefTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(13.0502432898856, "(Intercept)", 0.115583669645867, "Malic", 0.0517130288403993,
			 "Ash", -0.094804683609334, "Alcalinity", 0, "Magnesium", 0.029940780197386,
			 "Phenols", 0.1658555874675, "Flavanoids", -0.0317870067305739,
			 "Nonflavanoids", -0.131798783133663, "Proanthocyanins", 0.428436347709024,
			 "Color", 0.0265983010863477, "Hue", 0.122172677851877, "Dilution",
			 0.184008565048544, "Proline"))
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
		list(0.0130226688875131, 35, 114, 29, "L1 (Lasso)", 0.315722256934242,
			 0.338768048343228))
})

test_that("Model Performance Metrics table results match", {
	table <- results[["results"]][["validationMeasures"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list("Values", 0.427706057315286, 0.0333852369095772, 0.315722256934242,
	 0.503930362151054, 0.548524025364545, 0.561891677224571))
})

test_that("Variable Trace Plot matches", {
  plotName <- results[["results"]][["variableTrace"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "variable-trace-plot")
})

test_that("Additive Explanations for Predictions of Test Set Cases table results match", {
	table <- results[["results"]][["tableShap"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(-0.0434540873126217, 0.0959019938881607, -0.153001059055375, 0.0546267459332501,
			 0.12931126080368, 0.0108201357206976, 0, 0.0215094844224808,
			 -0.00779681929908804, 0.0258378534652071, -0.0585378164175143,
			 0.001553076495588, 13.0468421052632, 1, 13.1236128739076, 0.0530662808385092,
			 0.0468927680459998, -0.0180916686178101, 0.1664764799386, 0.0994231433032216,
			 0.0131474856681635, 0, -0.0240143864058524, 0.0126362243812945,
			 0.016269832367156, 0.0727177843695745, 0.328775530214115, 13.0468421052632,
			 2, 13.8141415793661, 0.229074010996433, 0.00542342310266619,
			 0.0465909158385536, 0.0202114431623617, 0.295356358028508, 0.0352573101691149,
			 0, -0.0674689903783552, -0.0180133411392394, 0.0401898851122695,
			 -0.286508070416128, 0.244048287733422, 13.0468421052632, 3,
			 13.5910033374727, 0.212041004852113, 0.0035384528779332, 0.434686422576771,
			 0.0666721019030536, 0.287054103167266, 0.0294389353004529, 0,
			 -0.0529841223875316, 0.0177444853013391, 0.0497579062103188,
			 -0.321049017991673, 0.47602563438747, 13.0468421052632, 4, 14.2497680114607,
			 -0.0150657437387505, 0.0676274405176809, 0.194436823167395,
			 0.0064453220540095, 0.204031554554845, 0.0143111606418884, 0,
			 -0.0478109552479342, 0.00752796346116824, 0.0258378534651982,
			 -0.0930787639930504, 0.320010643060952, 13.0468421052632, 5,
			 13.7311154032066))
})

test_that("Feature Importance Metrics table results match", {
	table <- results[["results"]][["featureImportanceTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(0.831119979179072, "Color", 0.567789274071525, "Proline", 0.564655595738101,
			 "Flavanoids", 0.548096645017086, "Proanthocyanins", 0.545270298357235,
			 "Dilution", 0.544198193635207, "Malic", 0.531812032359821, "Alcalinity",
			 0.52187002086854, "Ash", 0.517073160388491, "Hue", 0.516981362159554,
			 "Phenols", 0.516472659338966, "Nonflavanoids", 0.514704888657746,
			 "Magnesium"))
})
