context("Machine Learning Random Forest Classification")

# Test fixed model #########################################################
options <- initMlOptions("mlClassificationRandomForest")
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
results <- jaspTools::runAnalysis("mlClassificationRandomForest", "iris.csv", options)

table <- results[["results"]][["classificationTable"]][["data"]]
jaspTools::expect_equal_tables(table,
		list(30, 120, 0.894736842105263, 2, 0.933333333333333, 100))

# Test optimized model #########################################################
options <- initMlOptions("mlClassificationRandomForest")
options$addPredictions <- FALSE
options$addIndicator <- FALSE
options$andrewsCurve <- TRUE
options$predictionsColumn <- ""
options$classProportionsTable <- TRUE
options$savePath <- ""
options$saveModel <- FALSE
options$holdoutData <- "holdoutManual"
options$modelOptimization <- "optimized"
options$modelValid <- "validationManual"
options$noOfFolds <- 5
options$accuracyDecreasePlot <- TRUE
options$purityIncreasePlot <- TRUE
options$treesVsModelErrorPlot <- TRUE
options$predictors <- list("Alcohol", "Malic", "Ash", "Alcalinity", "Magnesium", "Phenols",
                           "Flavanoids", "Nonflavanoids", "Proanthocyanins", "Color",
                           "Hue", "Dilution", "Proline")
options$predictors.types <- rep("scale", length(options$predictors))
options$rocCurve <- TRUE
options$setSeed <- TRUE
options$featureImportanceTable <- TRUE
options$target <- "Type"
options$target.types <- "scale"
options$testDataManual <- 0.2
options$testIndicatorColumn <- ""
options$testSetIndicatorVariable <- ""
options$validationDataManual <- 0.2
options$validationMeasures <- TRUE
options$tableShap <- TRUE
options$fromIndex <- 1
options$toIndex <- 5
set.seed(1)
results <- jaspTools::runAnalysis("mlClassificationRandomForest", "wine.csv", options)


test_that("Andrews Curves Plot matches", {
  plotName <- results[["results"]][["andrewsCurve"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "andrews-curves-plot")
})

test_that("Class Proportions table results match", {
  table <- results[["results"]][["classProportionsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                      list(0.331460674157303, 1, 0.371428571428571, 0.324561403508772, 0.310344827586207,
                           0.398876404494382, 2, 0.4, 0.368421052631579, 0.517241379310345,
                           0.269662921348315, 3, 0.228571428571429, 0.307017543859649,
                           0.172413793103448))
})

test_that("Random Forest Classification table results match", {
  table <- results[["results"]][["classificationTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                      list(35, 114, 29, 1, 3, 0.971428571428571, 38, 0.931034482758621))
})

test_that("Confusion Matrix table results match", {
  table <- results[["results"]][["confusionTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                      list("Observed", 1, 13, 0, 0, "", 2, 0, 13, 1, "", 3, 0, 0, 8))
})

test_that("Data Split plot matches", {
  plotName <- results[["results"]][["plotDataSplit"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "data-split")
})

test_that("ROC Curves Plot matches", {
  plotName <- results[["results"]][["rocCurve"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "roc-curves-plot")
})

test_that("Feature Contributions to Predictions for Test Set Cases table results match", {
	skip("Does not reproduce on machine KD <-> GitHub Actions")
	table <- results[["results"]][["tableShap"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(-0.0211810012836975, 0.0641848523748396, 0.0203252032520327, 0.045143346170304,
			 0.0303808301240908, 0.0740265297389816, 0.0248181429182713,
			 0.0211810012836967, 0.0117672229353872, -0.00813008130081316,
			 0.0761660248181432, 0.0102695763799739, -0.0160462130937101,
			 0.325631151048352, 1, "1 (0.659)", 0.0273855370132654, 0.0823705605477109,
			 0.0203252032520325, 0.042789901583226, 0.0605477107402651, 0.0791613179289691,
			 0.0590500641848525, 0.0346598202824133, 0.0203252032520326,
			 0.0162601626016248, 0.0667522464698331, -0.0192554557124522,
			 0.110825845100556, 0.325631151048352, 2, "1 (0.927)", 0.0280273855370136,
			 0.0982028241335046, 0.00556268720581932, 0.0425759520753094,
			 0.0243902439024392, 0.104835258878904, 0.0744544287548139, 0.0308087291399228,
			 0.0117672229353879, -0.00534873769790212, 0.0890029952931112,
			 0.0104835258878905, 0.110825845100556, 0.325631151048352, 3,
			 "1 (0.951)", 0.0280273855370133, 0.100342319212666, 0.00556268720581965,
			 0.0577663671373557, 0.0303808301240908, 0.10397946084724, 0.0744544287548137,
			 0.0258878904578513, 0.011767222935387, 0.0162601626016267, 0.0890029952931107,
			 0.0104835258878905, 0.120453572956782, 0.325631151048352, 4,
			 "1 (1)", -0.00706033376123194, 0.0982028241335046, 0.0029952931108258,
			 0.053701326486949, 0.0243902439024392, 0.104835258878904, 0.0744544287548139,
			 0.0269576379974326, 0.0117672229353872, 0.0162601626016248,
			 0.0708172871202396, 0.0130509199828839, 0.110825845100556, 0.325631151048352,
			 5, "1 (0.927)"))
})

# Tests specific for windows and linux
test_that("Feature Importance Metrics table results match", {
  testthat::skip_on_os("mac")
  table <- results[["results"]][["featureImportanceTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                      list(0.0929184156013314, 0.11584466455711, 28.9155424269627, "Color", 0.0746170330176111,
                           0.0740156345749835, 19.448078950963, "Alcohol", 0.227882931822773, 0.0519798563590929,
                           26.418991981014, "Proline", 0.104210419210419, 0.038220702722991, 27.3543977112687, "Dilution",
                           0.0392152459372139, 0.0269934390675145, 19.8918388161141, "Hue", 0.148036748647974,
                           0.00844623148177441, 26.682160490794, "Flavanoids", 0.0192297690867485, 0.00698621553884712,
                           13.2574950728752, "Alcalinity", 0.00674978530241688, 0.00569058515854854, 11.8285559639254, "Ash",
                           0.0214415086298743, 0.00440369101163704, 13.4442844540554, "Malic", 0.00579683474420317,
                           0.0025062656641604, 11.4619631102683, "Magnesium", 0.0831850839152417, -0.000437779601050258,
                           18.1627003897205, "Phenols", 0.0194222905441742, -0.000592846851428087, 13.0768563565669, "Proanthocyanins",
                           0.0238594357587493, -0.00457882900654936, 13.0837916724828, "Nonflavanoids"))
})

test_that("Evaluation Metrics table results match", {
  testthat::skip_on_os("mac")
  table <- results[["results"]][["validationMeasures"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1, 0.993006993006993, 1, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0.371428571428571,
        13, 1, "<unicode><unicode><unicode>", 0.971428571428571, 0.998299319727891,
        0.962962962962963, 0, 0.0714285714285714, 0.0454545454545455,
        0, 2, 0.941468871691272, 0.954545454545455, 1, 0.928571428571429,
        0.371428571428571, 14, 1, 13, 0.971428571428571, 1, 0.941176470588235,
        0.111111111111111, 0, 0, 0.037037037037037, 3, 0.925184888651615,
        1, 0.888888888888889, 1, 0.257142857142857, 8, 0.962962962962963,
        4, 0.980952380952381, 0.997102104244961, 0.971739807033925,
        0.037037037037037, 0.0238095238095238, 0.0151515151515152, 0.0123456790123457,
        "Average / Total", 0.955551253447629, 0.984848484848485, 0.974603174603175,
        0.971428571428571, 1, 35, 0.987654320987654, "<unicode><unicode><unicode>"))
})

test_that("Mean Decrease in Accuracy plot matches", {
  testthat::skip_on_os("mac")
  plotName <- results[["results"]][["accuracyDecreasePlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "mean-decrease-in-accuracy")
})

test_that("Total Increase in Node Purity plot matches", {
  testthat::skip_on_os("mac")
  plotName <- results[["results"]][["purityIncreasePlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "total-increase-in-node-purity")
})

test_that("Out-of-bag Classification Accuracy Plot matches", {
  testthat::skip_on_os("mac")
  plotName <- results[["results"]][["treesVsModelErrorPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "out-of-bag-classification-accuracy-plot")
})

# Tests specific for mac
test_that("Feature Importance Metrics table results match", {
	testthat::skip_on_os(c("windows", "linux"))
	table <- results[["results"]][["featureImportanceTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(0.0842355188871508, 0.140028607384325, 25.5081304764804, "Color",
			 0.1910695271336, 0.0817233664218454, 29.4726331694755, "Proline",
			 0.0826624092070316, 0.0654880838290449, 19.1495415964802, "Alcohol",
			 0.0377722392024452, 0.0282158474206529, 21.7344936916165, "Hue",
			 0.159056561711024, 0.0276355721149772, 25.126989492684, "Flavanoids",
			 0.083825542334515, 0.0229680114677254, 26.3595041365273, "Dilution",
			 0.0085213358442914, 0.0126326694747747, 12.0047263714573, "Ash",
			 0.0139256895098157, 0.0118547570850202, 12.9475336867388, "Alcalinity",
			 0.0609110664316614, 0.0100432705695864, 15.7756074396539, "Phenols",
			 0.0382445601267112, 0.00847398215819268, 13.2585876070529, "Magnesium",
			 -0.0011441647597254, -5.20074890784274e-05, 11.4094026164425,
			 "Malic", 0.0319776714513557, -0.000500572082379863, 12.4960241802194,
			 "Proanthocyanins", 0.0130769230769231, -0.00314354261722683,
			 12.2265958852755, "Nonflavanoids"))
})

test_that("Model Performance Metrics table results match", {
	testthat::skip_on_os(c("windows", "linux"))
	table <- results[["results"]][["validationMeasures"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(0.971428571428571, 0.993006993006993, 0.962962962962963, 0.0714285714285714,
			 0, 0, 0.0454545454545455, 1, 0.941468871691272, 1, 0.928571428571429,
			 1, 0.4, 13, 0.954545454545455, 6.5, 0.971428571428571, 1, 0.962962962962963,
			 0, 0.0714285714285714, 0.0454545454545455, 0, 2, 0.941468871691272,
			 0.954545454545455, 1, 0.928571428571429, 0.371428571428571,
			 14, 1, 13, 1, 1, 1, 0, 0, 0, 0, 3, 1, 1, 1, 1, 0.228571428571429,
			 8, 1, "<unicode>", 0.980952380952381, 0.997668997668998, 0.971428571428571,
			 0.0238095238095238, 0.0238095238095238, 0.0151515151515152,
			 0.0151515151515152, "Average / Total", 0.960979247794181, 0.984848484848485,
			 0.973469387755102, 0.971428571428571, 1, 35, 0.984848484848485,
			 "<unicode>"))
})

test_that("Mean Decrease in Accuracy plot matches", {
  testthat::skip_on_os(c("windows", "linux"))
  plotName <- results[["results"]][["accuracyDecreasePlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "mean-decrease-in-accuracy-mac")
})

test_that("Total Increase in Node Purity plot matches", {
  testthat::skip_on_os(c("windows", "linux"))
  plotName <- results[["results"]][["purityIncreasePlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "total-increase-in-node-purity-mac")
})

test_that("Out-of-bag Classification Accuracy Plot matches", {
  testthat::skip_on_os(c("windows", "linux"))
  plotName <- results[["results"]][["treesVsModelErrorPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "out-of-bag-classification-accuracy-plot-mac")
})
