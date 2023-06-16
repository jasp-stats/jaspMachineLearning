context("Machine Learning Isolation Forest Anomaly Detection")

options <- analysisOptions("mlAnomalyOutlierTree")
options$predictors <- c("Alcohol", "Proline", "Hue", "Ash", "Malic", "Magnesium", "Alcalinity", "Phenols", "Flavanoids")
options$tableAnomalyScores <- TRUE
options$tableAnomalyScoresFeatures <- TRUE
options$addPredictions <- FALSE
options$predictionsColumn <- ""
options$supportVectors <- TRUE
set.seed(1)
results <- runAnalysis("mlAnomalyOutlierTree", "wine.csv", options)


test_that("Outlier Tree Anomaly Detection table results match", {
	table <- results[["results"]][["anomalyTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(178, 1))
})

test_that("Detected Anomalies table results match", {
	table <- results[["results"]][["tableAnomalyScores"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(1.64843571303763, 1.64983769286716, 1.21556562218747, -0.720091514155623,
			 -1.69509027422384, -0.121937689047059, -0.587522362570391, 0.806721729379673,
			 -0.275932664432648, "Anomaly", 159))
})