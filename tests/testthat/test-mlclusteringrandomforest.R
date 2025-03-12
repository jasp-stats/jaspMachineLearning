context("Machine Learning Random Forest Clustering")

# Test fixed model #############################################################
options <- initMlOptions("mlClusteringRandomForest")
options$predictors <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
options$predictors.types <- rep("scale", length(options$predictors))
options$modelOptimization <- "manual"
options$predictionsColumn <- ""
options$setSeed <- TRUE
options$tableClusterInformation <- FALSE
set.seed(1)
results <- jaspTools::runAnalysis("mlClusteringRandomForest", "iris.csv", options)

# Tests specific for windows and linux
test_that("Random Forest Clustering table results match", {
  testthat::skip_on_os("mac")
  table <- results[["results"]][["clusteringTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
		list(3, 150, 0.756064175255188, 169.39, 205.51, 0.450))
})

# Tests specific for mac
test_that("Random Forest Clustering table results match", {
  testthat::skip_on_os(c("windows", "linux"))
  table <- results[["results"]][["clusteringTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
		list(3, 150, 0.754087819907326, 170.56, 206.69, 0.450))
})

# Test optimized model #########################################################
options <- initMlOptions("mlClusteringRandomForest")
options$addPredictions <- FALSE
options$predictionsColumn <- ""
options$validationMeasures <- TRUE
options$featureImportanceTable <- TRUE
options$modelOptimization <- "optimized"
options$tsneClusterPlot <- TRUE
options$predictors <- c("Alcohol", "Malic", "Ash", "Alcalinity", "Magnesium", "Phenols",
                           "Flavanoids", "Nonflavanoids", "Proanthocyanins", "Color",
                           "Hue", "Dilution", "Proline")
options$predictors.types <- rep("scale", length(options$predictors))
options$setSeed <- TRUE
options$tableClusterInformationBetweenSumOfSquares <- TRUE
options$tableClusterInformationSilhouetteScore <- TRUE
options$tableClusterInformationTotalSumOfSquares <- TRUE
options$elbowMethodPlot <- TRUE
options$clusterMeanPlot <- TRUE
options$clusterMeanPlotBarPlot <- TRUE
options$clusterMeanPlotSingleFigure <- TRUE
set.seed(1)
results <- jaspTools::runAnalysis("mlClusteringRandomForest", "wine.csv", options)

test_that("t-SNE Cluster Plot matches", {
  skip("Does not reproduce on windows <-> osx")
	plotName <- results[["results"]][["plot2dCluster"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "t-sne-cluster-plot")
})

# Tests specific for windows and linux
test_that("Evaluation Metrics table results match", {
	testthat::skip_on_os("mac")
	table <- results[["results"]][["clusterEvaluationMetrics"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list("Maximum diameter", 11.1799587393255, "Minimum separation", 1.91296244175494,
			 "Pearson's <unicode><unicode>", 0.556526809045383, "Dunn index",
			 0.171106395502703, "Entropy", 1.07850807265, "Calinski-Harabasz index",
			 64.0845132710531))
})

test_that("Cluster Information table results match", {
	testthat::skip_on_os("mac")
	table <- results[["results"]][["clusterInfoTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(1, 0.201146634109287, 0.409774198440774, 57, 267.166873258102,
			 2, 0.605247626150293, 0.0820430005029718, 75, 803.901673729259,
			 3, 0.19360573974042, 0.394537832399485, 46, 257.15091062954
			))
})

test_that("All predictors plot matches", {
	testthat::skip_on_os("mac")
	plotName <- results[["results"]][["clusterMeans"]][["collection"]][["clusterMeans_oneFigure"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "all-predictors")
})

test_that("Random Forest Clustering table results match", {
	testthat::skip_on_os("mac")
	table <- results[["results"]][["clusteringTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(0.27, 1406.22, 1530.31, 3, 0.422764251361625, 178))
})

test_that("Variable Importance table results match", {
	testthat::skip_on_os("mac")
	table <- results[["results"]][["importanceTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(20.9122039237879, "Flavanoids", 16.7564424183852, "Phenols", 16.1286527609624,
			 "Dilution", 14.8508385264296, "Proline", 14.5421300011253, "Color",
			 13.6772809549555, "Hue", 13.4601315432519, "Alcohol", 13.4314873093631,
			 "Proanthocyanins", 11.6570120327778, "Malic", 11.0596207822425,
			 "Alcalinity", 10.9112870469843, "Nonflavanoids", 10.058673972256,
			 "Magnesium", 10.0524690645575, "Ash"))
})

test_that("Elbow Method Plot matches", {
	testthat::skip_on_os("mac")
	plotName <- results[["results"]][["optimPlot"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "elbow-method-plot")
})

# Tests specific for mac
test_that("Evaluation Metrics table results match", {
	testthat::skip_on_os(c("windows", "linux"))
	table <- results[["results"]][["clusterEvaluationMetrics"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list("Maximum diameter", 11.1799587393255, "Minimum separation", 1.73082516802066,
			 "Pearson's <unicode><unicode>", 0.547109179015303, "Dunn index",
			 0.154814987101202, "Entropy", 1.07566294260672, "Calinski-Harabasz index",
			 62.7877944611769))
})

test_that("Cluster Information table results match", {
	testthat::skip_on_os(c("windows", "linux"))
	table <- results[["results"]][["clusterInfoTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(1, 0.199425939703478, 0.411069251367668, 57, 267.166873258102,
			 2, 0.612907360221033, 0.0747428309831974, 76, 821.0995183004,
			 3, 0.187666700075488, 0.39282044191035, 45, 251.413259219857
			))
})

test_that("All predictors plot matches", {
	testthat::skip_on_os(c("windows", "linux"))
	plotName <- results[["results"]][["clusterMeans"]][["collection"]][["clusterMeans_oneFigure"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "all-predictors-mac")
})

test_that("Random Forest Clustering table results match", {
	testthat::skip_on_os(c("windows", "linux"))
	table <- results[["results"]][["clusteringTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(0.26, 1417.68, 1541.77, 3, 0.417783724129353, 178))
})

test_that("Variable Importance table results match", {
	testthat::skip_on_os(c("windows", "linux"))
	table <- results[["results"]][["importanceTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(20.3865617163522, "Flavanoids", 16.7221375429735, "Dilution", 16.5937155443686,
			 "Phenols", 15.1922351110861, "Proline", 14.7701748175345, "Color",
			 13.763257093803, "Proanthocyanins", 13.6109822805815, "Hue", 13.4924520858126,
			 "Alcohol", 12.0407649461782, "Malic", 10.9165824200026,
			 "Alcalinity", 10.6695338188, "Nonflavanoids", 9.6982366053581,
			 "Magnesium", 9.64091096096958, "Ash"))
})

test_that("Elbow Method Plot matches", {
	testthat::skip_on_os(c("windows", "linux"))
	plotName <- results[["results"]][["optimPlot"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "elbow-method-plot-mac")
})
