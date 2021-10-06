context("Machine Learning K-Medoids Clustering")

options <- jaspTools::analysisOptions("mlClusteringKMedoids")
options$addPredictions <- FALSE
options$algorithm <- "pam"
options$clusterEvaluationMetrics <- TRUE
options$modelOpt <- "validationOptimized"
options$predictionsColumn <- ""
options$predictors <- c("Sepal.Width", "Petal.Length")
options$saveModel <- FALSE
options$savePath <- ""
options$seedBox <- TRUE
options$tableClusterInfoBetweenSumSquares <- TRUE
options$tableClusterInfoCentroids <- TRUE
options$tableClusterInfoSilhouette <- TRUE
options$tableClusterInfoTotalSumSquares <- TRUE
options$tableClusterMeans <- TRUE
options$withinssPlot <- TRUE
set.seed(1)
results <- jaspTools::runAnalysis("mlClusteringKMedoids", "iris.csv", options)


test_that("Evaluation Metrics table results match", {
	table <- results[["results"]][["clusterEvaluationMetrics"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list("Maximum diameter", 2.30544385945597, "Minimum separation", 0.056647652003457,
			 "Pearson's <unicode><unicode>", 0.57771934631805, "Dunn index",
			 0.0245712563206048, "Entropy", 1.54566485858906, "Calinski-Harabasz index",
			 210.767122144636))
})

test_that("Cluster Information table results match", {
	table <- results[["results"]][["clusterInfoTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(1.24503015126641, -1.33575163424152, 1, 0.262250725770635, 0.389359723396298,
			 31, 11.4686926984701, 0.0978893485025193, -1.27910398223806,
			 2, 0.0304867267537149, 0.712287118776832, 18, 1.33323901961812,
			 -1.50810777536693, 0.0237920138414519, 3, 0.138621643863798,
			 0.371453839470283, 18, 6.06217210708927, 0.0978893485025193,
			 0.930154445896767, 4, 0.340024257248358, 0.303331250705639,
			 41, 14.8698681574578, -0.590395133155817, 0.533620881872567,
			 5, 0.228616646363494, 0.36993383189379, 42, 9.99781432517702
			))
})

test_that("Cluster Means table results match", {
	table <- results[["results"]][["clusterMeansTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(-1.28458601307711, 1.36344468574527, "Cluster 1", -1.32316326712964,
			 0.110635357422118, "Cluster 2", 0.0300861973973917, -1.57183781996493,
			 "Cluster 3", 0.971603947362711, 0.232188759557804, "Cluster 4",
			 0.553852186159515, -0.606782858909587, "Cluster 5"))
})

test_that("K-Medoids Clustering table results match", {
	table <- results[["results"]][["clusteringTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(0.4, 63.73, 93.84, 5, 0.843631226131727, 150))
})

test_that("Elbow Method Plot matches", {
	plotName <- results[["results"]][["optimPlot"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "elbow-method-plot")
})