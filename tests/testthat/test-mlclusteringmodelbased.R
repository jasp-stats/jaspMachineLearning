context("Machine Learning K-Means Clustering")

# Test fixed model #############################################################
options <- initMlOptions("mlClusteringModelBased")
options$predictors <- list("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
options$modelOptimization <- "manual"
options$predictionsColumn <- ""
options$setSeed <- TRUE
options$tableClusterInformation <- FALSE
set.seed(1)
results <- jaspTools::runAnalysis("mlClusteringModelBased", "iris.csv", options)

table <- results[["results"]][["clusteringTable"]][["data"]]
jaspTools::expect_equal_tables(table,
		list(3, 150, 0.71682015857423, 193.04, 229.17, 0.37))

# Test optimized model #########################################################
options <- initMlOptions("mlClusteringModelBased")
options$predictors <- list("Alcohol", "Malic", "Ash", "Alcalinity", "Magnesium", "Phenols", 
    "Flavanoids", "Nonflavanoids", "Proanthocyanins", "Color", 
    "Hue", "Dilution", "Proline")
options$tableClusterInformationSilhouetteScore <- TRUE
options$tableClusterInformationCentroids <- TRUE
options$tableClusterInformationBetweenSumOfSquares <- TRUE
options$tableClusterInformationTotalSumOfSquares <- TRUE
options$validationMeasures <- TRUE
options$elbowMethodPlot <- TRUE
options$clusterMeanPlot <- TRUE
options$tsneClusterPlot <- TRUE
options$setSeed <- TRUE
options$modelOptimization <- "optimized"
options$addPredictions <- FALSE
options$predictionsColumn <- ""
set.seed(1)
results <- jaspTools::runAnalysis("mlClusteringModelBased", "wine.csv", options)


test_that("Evaluation Metrics table results match", {
	table <- results[["results"]][["clusterEvaluationMetrics"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list("Maximum diameter", 11.17996, "Minimum separation", 1.790417,
			 "Pearson's <unicode><unicode>", 0.5790489, "Dunn index",
			 0.1601452, "Entropy", 1.084467, "Calinski-Harabasz index",
			 66.8498))
})

test_that("Cluster Information table results match", {
	table <- results[["results"]][["clusterInfoTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(0.957909979827348, 0.237988496628352, 0.476079849546062, 0.775758444256193, 
			1.2371003466187, -0.341327082334816, 0.2598098318776, -0.803622446183438, 
			0.431342260756213, 0.885911466135863, 0.973090706263375, -0.620135584585034, 
			0.548359137723541, 1, 0.198670219387692, 0.313060291811978, 0.408694169619655, 
			56, 259.150095821772, -0.832508550734794, -0.827038736324661, 0.426273855267938, 
			0.273831908203008, -0.692106527530303, -0.323384312900427, -0.384563634780119, 
			0.225819604714505, -0.340645152323922, -0.0128889567053076, 0.092813347750555, 
			0.0440226628508435, 0.0844678928848581, 2, 0.55585808750625, 0.411432227933174, 
			0.111942422928099, 73, 725.074332149633, 0.154759028496962, 0.964640384445781, 
			-1.1775524145738, -1.29042752914273, -0.372157100160835, 0.870781018767304, 
			0.27906948727968, 0.575929242586772, 0.0185703122374601, -0.987417001104397, 
			-1.24433087113339, 0.638920891200776, -0.749243849353898, 3, 0.245471693106059, 
			0.275507480254849, 0.357009685073831, 49, 320.199036302614
			))
})

test_that("All predictors plot matches", {
	plotName <- results[["results"]][["clusterMeans"]][["collection"]][["clusterMeans_oneFigure"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "all-predictors")
})

test_that("Model-Based Clustering table results match", {
	table <- results[["results"]][["clusteringTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(0.27, 1382.42, 1506.51, 3, 0.4330966, 178))
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
