context("Machine Learning SVM Classification")

options <- jaspTools::analysisOptions("mlClassificationSvm")
options$addIndicator <- FALSE
options$addPredictions <- FALSE
options$classProportionsTable <- TRUE
options$holdoutData <- "holdoutManual"
options$modelOptimization <- "manual"
options$modelValid <- "validationManual"
options$noOfFolds <- 5
options$predictionsColumn <- ""
options$predictors <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
options$saveModel <- FALSE
options$savePath <- ""
options$setSeed <- TRUE
options$supportVectorsTable <- TRUE
options$target <- "Species"
options$testDataManual <- 0.2
options$testIndicatorColumn <- ""
options$testSetIndicatorVariable <- ""
options$validationDataManual <- 0.2
options$validationMeasures <- TRUE
options$shapTable <- TRUE
options$shapFrom <- 1
options$shapTo <- 5
set.seed(1)
results <- jaspTools::runAnalysis("mlClassificationSvm", "iris.csv", options)

test_that("Class Proportions table results match", {
	table <- results[["results"]][["classProportionsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(0.333333333333333, "setosa", 0.333333333333333, 0.333333333333333,
			 0.333333333333333, "versicolor", 0.266666666666667, 0.35, 0.333333333333333,
			 "virginica", 0.4, 0.316666666666667))
})

test_that("Support Vector Machine Classification table results match", {
	table <- results[["results"]][["classificationTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(30, 120, 0.966666666666667, 26))
})

test_that("Confusion Matrix table results match", {
	table <- results[["results"]][["confusionTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list("Observed", "setosa", 10, 0, 0, "", "versicolor", 0, 8, 0, "",
			 "virginica", 0, 1, 11))
})

test_that("Data Split plot matches", {
	plotName <- results[["results"]][["plotDataSplit"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "data-split")
})

test_that("Support Vectors table results match", {
	table <- results[["results"]][["supportVectorsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(0.420325577865652, 0.394452647658782, -0.535383972794483, -0.13153881205026,
			 3, 0.760211489886395, 0.394452647658782, 0.551485746412362,
			 -0.590395133155817, 4, 0.646916185879481, 0.394452647658782,
			 1.27606555921693, 0.0978893485025193, 14, 0.760211489886395,
			 0.919223354078447, -0.0523307642581081, -0.819823293708595,
			 20, 0.760211489886395, 0.525645324263698, 0.18919584001008,
			 -0.819823293708595, 23, 0.420325577865652, 0.656838000868614,
			 -1.13920048346495, -1.27867961481415, 29, -1.39239928624498,
			 -1.17985947160021, -1.62225369200133, -1.73753593591971, 30,
			 -0.259446246175834, -0.2615107353658, -1.13920048346495, -1.50810777536693,
			 31, 0.590268533876024, 0.788030677473531, 0.0684325378759866,
			 0.327317509055298, 42, 0.533620881872567, 0.525645324263698,
			 0.551485746412362, 0.556745669608076, 45, 0.420325577865652,
			 0.525645324263698, 0.18919584001008, 0.786173830160855, 52,
			 -1.16580867823115, -0.917474118390381, -0.897673879196766, 0.556745669608076,
			 54, 0.986802097900224, 0.788030677473531, 0.79301235068055,
			 -0.13153881205026, 66, 0.420325577865652, 0.394452647658782,
			 -0.293857368526296, -0.13153881205026, 72, -0.429389202186205,
			 -0.130318058760884, -0.897673879196766, -1.27867961481415, 79,
			 0.420325577865652, 0.394452647658782, 0.430722444278268, -1.96696409647249,
			 80, 0.703563837882938, 0.656838000868614, 1.03453895494874,
			 -0.13153881205026, 83, 0.760211489886395, 1.05041603068336,
			 0.79301235068055, 0.327317509055298, 87, 0.590268533876024,
			 0.788030677473531, 0.430722444278268, -0.590395133155817, 89,
			 0.590268533876024, 0.788030677473531, 0.18919584001008, -0.13153881205026,
			 97, 0.986802097900224, 0.788030677473531, 0.672249048546457,
			 0.0978893485025193, 106, 0.760211489886395, 0.788030677473531,
			 0.0684325378759866, -0.13153881205026, 111, 0.646916185879481,
			 0.394452647658782, 0.551485746412362, -1.27867961481415, 112,
			 0.476973229869109, 0.394452647658782, 0.79301235068055, -0.590395133155817,
			 115, 0.646916185879481, 0.788030677473531, 0.309959142144174,
			 -0.13153881205026, 117, 1.1567450539106, 0.525645324263698,
			 1.63835546561921, -0.13153881205026, 119))
})



test_that("Evaluation Metrics table results match", {
  table <- results[["results"]][["validationMeasures"]][["data"]]
  jaspTools::expect_equal_tables(table,
      list(1, 1, 1, 0, 0, 0, 0, "setosa", 1, 1, 1, 1, 0.333333333333333,
        10, 1, "<unicode><unicode><unicode>", 0.966666666666667, 0.551136363636364,
        0.941176470588235, 0.111111111111111, 0, 0, 0.0454545454545455,
        "versicolor", 0.921132372943677, 1, 0.888888888888889, 1, 0.3,
        8, 0.954545454545455, 4, 0.966666666666667, 0.958333333333333,
        0.956521739130435, 0, 0.0833333333333333, 0.0526315789473684,
        0, "virginica", 0.931891116296093, 0.947368421052632, 1, 0.916666666666667,
        0.366666666666667, 12, 1, 11, 0.977777777777778, 0.836489898989899,
        0.96692242114237, 0.037037037037037, 0.0277777777777778, 0.0175438596491228,
        0.0151515151515152, "Average / Total", 0.95100782974659, 0.982456140350877,
        0.97037037037037, 0.966666666666667, 1, 30, 0.984848484848485,
        "<unicode><unicode><unicode>"))
})

test_that("Feature Contributions to Predictions for Test Set Cases table results match", {
	table <- results[["results"]][["shapTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(-0.633333333333333, -0.341666666666667, 0, -0.0166666666666666,
			 1.99166666666667, 1, "setosa (1)", -0.616666666666666, -0.341666666666667,
			 0, -0.0333333333333334, 1.99166666666667, 2, "setosa (1)", -0.616666666666666,
			 -0.341666666666667, 0, -0.0333333333333334, 1.99166666666667,
			 3, "setosa (1)", -0.333333333333333, -0.6, 0, -0.0583333333333333,
			 1.99166666666667, 4, "setosa (1)", -0.583333333333333, -0.333333333333333,
			 0, -0.075, 1.99166666666667, 5, "setosa (1)"))
})
