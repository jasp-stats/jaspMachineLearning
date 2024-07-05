context("Machine Learning Fuzzy C-Means Clustering")

# Test fixed model #############################################################
options <- initMlOptions("mlClusteringFuzzyCMeans")
options$predictors <- list("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
options$predictors.types <- rep("scale", length(options$predictors))
options$modelOptimization <- "manual"
options$predictionsColumn <- ""
options$setSeed <- TRUE
options$tableClusterInformation <- FALSE
set.seed(1)
results <- jaspTools::runAnalysis("mlClusteringFuzzyCMeans", "iris.csv", options)

table <- results[["results"]][["clusteringTable"]][["data"]]
jaspTools::expect_equal_tables(table,
		list(3, 150, 0.589422839657918, 220.57, 256.7, 0.32))

# Test optimized model #########################################################
options <- initMlOptions("mlClusteringFuzzyCMeans")
options$addPredictions <- FALSE
options$predictionsColumn <- ""
options$validationMeasures <- TRUE
options$modelOptimization <- "optimized"
options$tsneClusterPlot <- TRUE
options$predictors <- list("Alcohol", "Malic", "Ash", "Alcalinity", "Magnesium", "Phenols",
                           "Flavanoids", "Nonflavanoids", "Proanthocyanins", "Color",
                           "Hue", "Dilution", "Proline")
options$predictors.types <- rep("scale", length(options$predictors))
options$setSeed <- TRUE
options$tableClusterInformationBetweenSumOfSquares <- TRUE
options$tableClusterInformationCentroids <- TRUE
options$tableClusterInformationSilhouetteScore <- TRUE
options$tableClusterInformationTotalSumOfSquares <- TRUE
options$elbowMethodPlot <- TRUE
options$clusterMeanPlot <- TRUE
options$clusterMeanPlotBarPlot <- TRUE
options$clusterMeanPlotSingleFigure <- TRUE
set.seed(1)
results <- jaspTools::runAnalysis("mlClusteringFuzzyCMeans", "wine.csv", options)


test_that("Evaluation Metrics table results match", {
	table <- results[["results"]][["clusterEvaluationMetrics"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list("Maximum diameter", 9.7831459107885, "Minimum separation", 1.38013225067267,
			 "Pearson's <unicode><unicode>", 0.465708057418426, "Dunn index",
			 0.141072438585498, "Entropy", 1.16867550715933, "Calinski-Harabasz index",
			 32.7800078708869))
})

test_that("Cluster Information table results match", {
	table <- results[["results"]][["clusterInfoTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(1.60120153499477, 0.988303143304228, 0.980735732338681, 0.519098835868951,
			 2.37601519647931, -0.390480111226582, 0.0739783669466817, -2.05894236195967,
			 0.182766660968452, 1.50655293527746, 1.51366105534294, -0.543527101886099,
			 2.19496791376556, 1, 0.0124096523566579, 0.285500544441926,
			 6, 18.2437457658118, -0.836219252964655, -0.832329201374392,
			 -0.348980731103696, 0.407337324226026, -1.15578537140965, -0.683993215426626,
			 -1.23621522921151, -0.0674082627884001, -0.485902536106231,
			 0.0142201789932861, -0.0518728783660091, 0.183538890309772,
			 0.583620998978253, 2, 0.227930302046909, 0.142621636407501,
			 37, 335.086138060711, -0.867419645100303, -0.860658733131303,
			 1.1393120322887, -0.448243753071963, -0.705176229844058, -0.43573853746754,
			 -0.112842807150426, 0.822365565830556, -0.564838054368077, -0.734001997815396,
			 -0.384589977567659, 0.529797388802047, -0.163378649860915, 3,
			 0.518344921787501, 0.136543674258336, 77, 762.032061842346,
			 0.962738126015396, 0.383824805888347, -0.100744370575173, 0.866303680093817,
			 0.731256566138224, -0.467163376276762, -0.605603437599103, -0.880332260508523,
			 0.168912584934826, 1.08181365184212, 1.13366675651553, -1.00569428999695,
			 0.782951106505343, 4, 0.241315123808932, 0.0648620182441498,
			 58, 354.763505188246))
})

test_that("All Predictors plot matches", {
	plotName <- results[["results"]][["clusterMeans"]][["collection"]][["clusterMeans_oneFigure"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "all-predictors")
})

test_that("Fuzzy C-Means Clustering table results match", {
	table <- results[["results"]][["clusteringTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(0.12, 1574.13, 1739.58, 4, 0.411120174471616, 178))
})

test_that("Elbow Method Plot matches", {
	plotName <- results[["results"]][["optimPlot"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "elbow-method-plot")
})

test_that("t-SNE Cluster Plot matches", {
	skip("Does not reproduce on windows <-> osx")
	plotName <- results[["results"]][["plot2dCluster"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "t-sne-cluster-plot")
})
