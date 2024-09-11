context("Machine Learning Logistic / Multinomial Regression Classification")

# Test logistic regression model ############################################
options <- initMlOptions("mlClassificationLogisticMultinomial")
options$addIndicator <- FALSE
options$addPredictions <- FALSE
options$coefTable <- TRUE
options$coefTableConfInt <- TRUE
options$classProportionsTable <- TRUE
options$holdoutData <- "holdoutManual"
options$modelOptimization <- "manual"
options$modelValid <- "validationManual"
options$predictionsColumn <- ""
options$predictors <- c("x", "y")
options$predictors.types <- rep("scale", 2)
options$saveModel <- FALSE
options$savePath <- ""
options$setSeed <- TRUE
options$target <- "color"
options$target.types <- "nominal"
options$testDataManual <- 0.2
options$testIndicatorColumn <- ""
options$testSetIndicatorVariable <- ""
options$validationDataManual <- 0.2
options$validationMeasures <- TRUE
options$tableShap <- TRUE
options$fromIndex <- 1
options$toIndex <- 5
options$featureImportanceTable <- TRUE
options$seed <- 2
set.seed(1)
results <- jaspTools::runAnalysis("mlClassificationLogisticMultinomial", "spiral.csv", options)

test_that("Class Proportions table results match", {
	table <- results[["results"]][["classProportionsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(0.5, "Black", 0.575, 0.48125, 0.5, "Red", 0.425, 0.51875))
})

test_that("Model Summary: Logistic Regression Classification table results match", {
	table <- results[["results"]][["classificationTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list("Binomial", "Logit", 40, 160, 0.675))
})

test_that("Regression Coefficients table results match", {
	table <- results[["results"]][["coefTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(0.0784909051640228, -0.241691087574101, 0.630847847956443, 0.163341057208965,
			 0.400211265504598, "(Intercept)", 0.480533838247466, -0.0733280693358763,
			 -0.389515022216725, 0.647229030486731, 0.160239637348256, 0.241355688318239,
			 "x", -0.457615047995329, -0.520574613112014, -0.864252780481388,
			 0.00221841580161717, 0.170160350681228, -0.194437228429846,
			 "y", -3.0593179376272))
})

test_that("Confusion Matrix table results match", {
	table <- results[["results"]][["confusionTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list("Observed", "Black", 14, 9, "", "Red", 4, 13))
})

test_that("Feature Importance Metrics table results match", {
	table <- results[["results"]][["featureImportanceTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(0.49553121577218, "y", 0.367228915662651, "x"))
})

test_that("Data Split plot matches", {
	plotName <- results[["results"]][["plotDataSplit"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "data-split-1")
})

test_that("Additive Explanations for Predictions of Test Set Cases table results match", {
	table <- results[["results"]][["tableShap"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(0.518749999999986, 1, "Red (0.511)", -0.0104280206091216, 0.00223442804446772,
			 0.481250000000014, 2, "Black (0.505)", 0.0113934073181472, 0.0126826432048778,
			 0.518749999999986, 3, "Red (0.694)", -0.0215827660760237, 0.196894941069013,
			 0.518749999999986, 4, "Red (0.707)", -0.0148699072618508, 0.203086871619157,
			 0.481250000000014, 5, "Black (0.584)", 0.00703631455756359,
			 0.0957186107999987))
})

test_that("Model Performance Metrics table results match", {
	table <- results[["results"]][["validationMeasures"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(0.675, 0.686700767263427, 0.682926829268293, 0.222222222222222,
			 0.391304347826087, 0.409090909090909, 0.235294117647059, "Black",
			 0.371036713180216, 0.590909090909091, 0.777777777777778, 0.608695652173913,
			 0.45, 23, 0.764705882352941, 0.823529411764706, 0.675, 0.686700767263427,
			 0.666666666666667, 0.409090909090909, 0.235294117647059, 0.222222222222222,
			 0.391304347826087, "Red", 0.371036713180216, 0.777777777777778,
			 0.590909090909091, 0.764705882352941, 0.55, 17, 0.608695652173913,
			 0.590909090909091, 0.675, 0.686700767263427, 0.676016260162602,
			 0.315656565656566, 0.313299232736573, 0.315656565656566, 0.313299232736573,
			 "Average / Total", 0.371036713180216, 0.684343434343434, 0.698358585858586,
			 0.675, 1, 40, 0.686700767263427, 0.707219251336898))
})

# Test multinomial regression model ############################################
options <- initMlOptions("mlClassificationLogisticMultinomial")
options$addIndicator <- FALSE
options$addPredictions <- FALSE
options$coefTable <- TRUE
options$coefTableConfInt <- TRUE
options$classProportionsTable <- TRUE
options$holdoutData <- "holdoutManual"
options$modelOptimization <- "manual"
options$modelValid <- "validationManual"
options$predictionsColumn <- ""
options$predictors <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
options$predictors.types <- rep("scale", 4)
options$saveModel <- FALSE
options$savePath <- ""
options$setSeed <- TRUE
options$target <- "Species"
options$target.types <- "nominal"
options$testDataManual <- 0.2
options$testIndicatorColumn <- ""
options$testSetIndicatorVariable <- ""
options$validationDataManual <- 0.2
options$validationMeasures <- TRUE
options$tableShap <- TRUE
options$fromIndex <- 1
options$toIndex <- 5
options$featureImportanceTable <- TRUE
set.seed(1)
results <- jaspTools::runAnalysis("mlClassificationLogisticMultinomial", "iris.csv", options)

test_that("Class Proportions table results match", {
	table <- results[["results"]][["classProportionsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(0.333333333333333, "setosa", 0.333333333333333, 0.333333333333333,
			 0.333333333333333, "versicolor", 0.266666666666667, 0.35, 0.333333333333333,
			 "virginica", 0.4, 0.316666666666667))
})

test_that("Model Summary: Multinomial Regression Classification table results match", {
	table <- results[["results"]][["classificationTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list("Multinomial", "Logit", 30, 120, 1))
})

test_that("Regression Coefficients table results match", {
	table <- results[["results"]][["coefTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(-0.42267961226173, -4222.39134720741, 0.999843438690878, 2154.10522892129,
			 4221.54598798288, "(Intercept) : setosa", -0.000196220503337896,
			 17.8212683047785, 0.307905475852188, 0.0461059069011903, 8.93555339132221,
			 35.3346311337047, "(Intercept) : versicolor", 1.99442245200903,
			 7.95554619079237, -9967.35637439074, 0.998752813237481, 5089.53837890162,
			 9983.26746677233, "Sepal.Length : setosa", 0.00156311743787445,
			 1.89635371704686, -2.10497557567811, 0.352947362657274, 2.04153205073509,
			 5.89768300977184, "Sepal.Length : versicolor", 0.928887555972511,
			 4.70317443290632, -2533.61853897752, 0.997102445459767, 1295.08589618604,
			 2543.02488784333, "Sepal.Width : setosa", 0.00363155405116905,
			 2.30296713825466, -1.92502652612214, 0.285708816748704, 2.15717926335722,
			 6.53096080263146, "Sepal.Width : versicolor", 1.0675826424692,
			 -38.7099474348317, -15129.1258351686, 0.995988491696961, 7699.33325651139,
			 15051.705940299, "Petal.Length : setosa", -0.00502770124959773,
			 -14.4906493502573, -31.1295653321292, 0.087838572245474, 8.48939884258976,
			 2.14826663161466, "Petal.Length : versicolor", -1.7069111275065,
			 -24.205993975534, -10348.1025945441, 0.996333380037363, 5267.39097350877,
			 10299.690606593, "Petal.Width : setosa", -0.00459544280978437,
			 -12.7574125117412, -27.3435062790441, 0.0864846111196134, 7.44202132404277,
			 1.82868125556169, "Petal.Width : versicolor", -1.7142402522452
			))
})

test_that("Confusion Matrix table results match", {
	table <- results[["results"]][["confusionTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list("Observed", "setosa", 10, 0, 0, "", "versicolor", 0, 8, 0, "",
			 "virginica", 0, 0, 12))
})

test_that("Feature Importance Metrics table results match", {
	table <- results[["results"]][["featureImportanceTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(530.419652531233, "Petal.Length", 258.101247632355, "Petal.Width",
			 11.6632506085855, "Sepal.Width", 10.7849556008181, "Sepal.Length"
			))
})

test_that("Data Split plot matches", {
	plotName <- results[["results"]][["plotDataSplit"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "data-split-2")
})

test_that("Additive Explanations for Predictions of Test Set Cases table results match", {
	table <- results[["results"]][["tableShap"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(0.629688901507271, 0.0369777616411127, 2.01684224876431e-11, 3.29787464004028e-09,
			 0.333333333528117, 1, "setosa (1)", 0.608365336494419, 0.0583013168725895,
			 5.75901548671709e-11, 1.30466177861166e-08, 0.333333333528117,
			 2, "setosa (1)", 0.578930644244146, 0.0877359705540646, -1.91210501876427e-08,
			 6.96919006948349e-08, 0.333333333528117, 3, "setosa (1)", 0.644504774615733,
			 0.0221618748599225, 2.48312481687663e-11, 1.69711121822402e-08,
			 0.333333333528117, 4, "setosa (1)", 0.544084722419656, 0.122581029133867,
			 2.34213792804638e-09, 9.1257324152938e-07, 0.333333333528117,
			 5, "setosa (1)"))
})

test_that("Model Performance Metrics table results match", {
	table <- results[["results"]][["validationMeasures"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(1, 1, 1, 0, 0, 0, 0, "setosa", 1, 1, 1, 1, 0.333333333333333,
			 10, 1, "<unicode>", 1, 0.613636363636364, 1, 0, 0, 0, 0, "versicolor",
			 1, 1, 1, 1, 0.266666666666667, 8, 1, "<unicode>", 1, 1, 1, 0,
			 0, 0, 0, "virginica", 1, 1, 1, 1, 0.4, 12, 1, "<unicode>", 1,
			 0.871212121212121, 1, 0, 0, 0, 0, "Average / Total", 1, 1, 1,
			 1, 1, 30, 1, "<unicode>"))
})