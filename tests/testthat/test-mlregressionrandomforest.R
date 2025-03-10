context("Machine Learning Random Forest Regression")

# Test fixed model #############################################################
options <- initMlOptions("mlRegressionRandomForest")
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
results <- jaspTools::runAnalysis("mlRegressionRandomForest", "iris.csv", options)

# Tests specific to windows and linux
test_that("Random Forest Regression table results match", {
  testthat::skip_on_os("mac")
  table <- results[["results"]][["regressionTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
		list(30, 120, 0.154553780715417, 1, 0.0939947930433144, 100))
})

# Tests specific to mac
test_that("Random Forest Regression table results match", {
  testthat::skip_on_os(c("windows", "linux"))
  table <- results[["results"]][["regressionTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
		list(30, 120, 0.154745360868566, 1, 0.0939947930433144, 100))
})

# Test optimized model #########################################################
options <- initMlOptions("mlRegressionRandomForest")
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
options$predictors.types <- rep("scale", length(options$predictors))
options$setSeed <- TRUE
options$featureImportanceTable <- TRUE
options$target <- "Alcohol"
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
set.seed(1)
results <- jaspTools::runAnalysis("mlRegressionRandomForest", "wine.csv", options)


test_that("Data Split plot matches", {
  plotName <- results[["results"]][["plotDataSplit"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "data-split")
})

test_that("Total Increase in Node Purity plot matches", {
  plotName <- results[["results"]][["purityIncreasePlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "total-increase-in-node-purity")
})

test_that("Feature Contributions to Predictions for Test Set Cases table results match", {
	skip("Does not reproduce on machine KD <-> GitHub Actions")
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

# Tests specific for windows and linux
test_that("Random Forest Regression table results match", {
  testthat::skip_on_os("mac")
  table <- results[["results"]][["regressionTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                      list(35, 114, 29, 0.290897987516422, 3, 0.35516909919041, 38, 0.401169845352203
                      ))
})

test_that("Evaluation Metrics table results match", {
  testthat::skip_on_os("mac")
  table <- results[["results"]][["validationMeasures"]][["data"]]
  jaspTools::expect_equal_tables(table,
                      list("MSE", 0.355, "MSE(scaled)", 0.531, "RMSE", 0.596, "MAE / MAD", 0.473, "MAPE", "3.71%",
                           "R<unicode><unicode>", 0.528))
})

test_that("Feature Importance Metrics table results match", {
  testthat::skip_on_os("mac")
  table <- results[["results"]][["featureImportanceTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                      list(0.307663962052935, 7.0343691905776, 0.595808564481455, "Color", 0.0747131651235033,
                           4.22786015675743, 0.424064269976166, "Flavanoids", 0.0359932779052557, 3.61895125740788,
                           0.409770970482336, "Phenols", 0.111256935132681, 3.26768977057375, 0.423439714991959, "Proline",
                           0.0152626450040049, 2.91627904379051, 0.399654153492494, "Alcalinity", 0.0438531063289394,
                           1.76974836172652, 0.397381477144956, "Malic", 0.0195664237526175, 1.72001045784324,
                           0.387365661732971, "Hue", 0.0135869015081512, 1.71283101502024, 0.383750280509374, "Dilution", 0.0145300260542608,
                           1.43782951785641, 0.38488531466346, "Ash", 0.00752052501101563, 1.41690658037497, 0.382710960885085,
                           "Magnesium", 0.0135558535655626, 1.4057430301992, 0.384041452184052, "Proanthocyanins",
                           -0.00394389539625634, 0.876409911962745, 0.373175045745205, "Nonflavanoids"))
})

test_that("Mean Decrease in Accuracy plot matches", {
  testthat::skip_on_os("mac")
  plotName <- results[["results"]][["accuracyDecreasePlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "mean-decrease-in-accuracy")
})

test_that("Out-of-bag Mean Squared Error Plot matches", {
  testthat::skip_on_os("mac")
  plotName <- results[["results"]][["treesVsModelErrorPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "out-of-bag-mean-squared-error-plot")
})

test_that("Predictive Performance Plot matches", {
  testthat::skip_on_os("mac")
  plotName <- results[["results"]][["predictedPerformancePlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "predictive-performance-plot")
})

# Tests specific for mac
test_that("Random Forest Regression table results match", {
  testthat::skip_on_os(c("windows", "linux"))
  table <- results[["results"]][["regressionTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                      list(35, 114, 29, 0.291321779981174, 3, 0.35516909919041, 38, 0.401664270085537
                      ))
})

test_that("Evaluation Metrics table results match", {
  testthat::skip_on_os(c("windows", "linux"))
  table <- results[["results"]][["validationMeasures"]][["data"]]
  jaspTools::expect_equal_tables(table,
                      list("Values", 0.474173308270677, 0.0371687545468009, 0.355223330180166,
	 0.530948953827784, 0.528118252477076, 0.59600614944828))
})

test_that("Feature Importance Metrics table results match", {
  testthat::skip_on_os(c("windows", "linux"))
  table <- results[["results"]][["featureImportanceTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                      list(0.30801152309703, 7.0343691905776, 0.595933685617779, "Color", 0.0758113177168946,
                           4.22786015675743, 0.424215588182356, "Flavanoids", 0.0354811022864141, 3.61895125740788,
                           0.410009159266135, "Phenols", 0.111985839320969, 3.26768977057375, 0.42360723852141, "Proline",
                           0.0155999234810833, 2.91627904379051, 0.399962552432926, "Alcalinity", 0.0436597732462326,
                           1.76974836172652, 0.397567937436591, "Malic", 0.0195664237526175, 1.72001045784324,
                           0.387611251589476, "Hue", 0.0135196305597889, 1.71283101502024, 0.383793852191865, "Dilution", 0.0145300260542608,
                           1.43782951785641, 0.385071193382805, "Ash", 0.00752052501101563, 1.41690658037497, 0.382939334010029,
                           "Magnesium", 0.013899214903179, 1.4057430301992, 0.384287964943447, "Proanthocyanins",
                           -0.00394389539625634, 0.876409911962745, 0.373175045745205, "Nonflavanoids"))
})

test_that("Mean Decrease in Accuracy plot matches", {
  testthat::skip_on_os(c("windows", "linux"))
  plotName <- results[["results"]][["accuracyDecreasePlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "mean-decrease-in-accuracy-mac")
})

test_that("Out-of-bag Mean Squared Error Plot matches", {
  testthat::skip_on_os(c("windows", "linux"))
  plotName <- results[["results"]][["treesVsModelErrorPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "out-of-bag-mean-squared-error-plot-mac")
})

test_that("Predictive Performance Plot matches", {
  testthat::skip_on_os(c("windows", "linux"))
  plotName <- results[["results"]][["predictedPerformancePlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "predictive-performance-plot-mac")
})
