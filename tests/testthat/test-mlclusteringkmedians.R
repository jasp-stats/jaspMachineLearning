context("Machine Learning K-Medians Clustering")

options <- jaspTools::analysisOptions("mlClusteringKMedians")
options$predictors <- c("Sepal.Width", "Petal.Length")
options$tableClusterMeans <- TRUE
options$tableClusterInfoSilhouette <- TRUE
options$tableClusterInfoCentroids <- TRUE
options$tableClusterInfoBetweenSumSquares <- TRUE
options$tableClusterInfoTotalSumSquares <- TRUE
options$clusterEvaluationMetrics <- TRUE
options$withinssPlot <- TRUE
options$seedBox <- TRUE
options$modelOpt <- "validationOptimized"
options$addPredictions <- FALSE
options$predictionsColumn <- ""
options$saveModel <- FALSE
options$savePath <- ""
set.seed(1)
results <- jaspTools::runAnalysis("mlClusteringKMedians", "iris.csv", options)


test_that("Evaluation Metrics table results match", {
	table <- results[["results"]][["clusterEvaluationMetrics"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list("Maximum diameter", 2.30056703744114, "Minimum separation", 0.0566476520034569,
			 "Pearson's <unicode><unicode>", 0.588252683087994, "Dunn index",
			 0.0246233433242896, "Entropy", 1.54935418774129, "Calinski-Harabasz index",
			 225.120589040842))
})

test_that("Cluster Information table results match", {
	table <- results[["results"]][["clusterInfoTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(0.219755172974839, 0.94822410423453, 1, 0.320599985014869, 0.284399286639367,
			 36, 13.2504630717343, 1.68444313216648, -1.28975797919559, 2,
			 0.104385886498613, 0.518947480126447, 16, 4.31429007769908,
			 0.453929554921232, -1.29117749831427, 3, 0.156561450601329,
			 0.57131941012006, 33, 6.47071683285901, -0.48308427801346, 0.489815002179442,
			 4, 0.241039166937652, 0.383612571899548, 44, 9.96219815855834,
			 -1.45895558510676, 0.0683800216730154, 5, 0.177413510947538,
			 0.369544962397745, 21, 7.33253675956364))
})

test_that("Cluster Means table results match", {
	table <- results[["results"]][["clusterMeansTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(1.01355237801297, 0.282706477836702, "Cluster 1", -1.29326589523893,
			 1.81860055264836, "Cluster 2", -1.30141972393639, 0.459412510585686,
			 "Cluster 3", 0.564519601147179, -0.512180987512824, "Cluster 4",
			 0.110112245465768, -1.51903292586945, "Cluster 5"))
})

test_that("K-Medians Clustering table results match", {
	table <- results[["results"]][["clusteringTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(0.41, 61.33, 91.44, 5, 0.849495187080768, 150))
})

test_that("Elbow Method Plot matches", {
	plotName <- results[["results"]][["optimPlot"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "elbow-method-plot")
})

