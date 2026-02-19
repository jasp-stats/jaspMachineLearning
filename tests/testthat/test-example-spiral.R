context("Example: spiral")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in the module's examples/ folder.

test_that("mlClassificationSvm results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("..", "..", "examples", "spiral.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("mlClassificationSvm", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["classificationTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1, 40, 160, 0.8, 124))

  table <- results[["results"]][["confusionTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Observed", "Black", 13, 3, "", "Red", 5, 19))

  plotName <- results[["results"]][["decisionBoundary"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_decision-boundary-matrix")

  plotName <- results[["results"]][["plotDataSplit"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-2_data-split")

  table <- results[["results"]][["validationMeasures"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.8, 0.802083333333333, 0.764705882352941, 0.277777777777778,
     0.1875, 0.136363636363636, 0.208333333333333, "Black", 0.594942206400108,
     0.863636363636364, 0.722222222222222, 0.8125, 0.45, 16, 0.791666666666667,
     1, 0.8, 0.802083333333333, 0.826086956521739, 0.136363636363636,
     0.208333333333333, 0.277777777777778, 0.1875, "Red", 0.594942206400108,
     0.722222222222222, 0.863636363636364, 0.791666666666667, 0.55,
     24, 0.8125, 1.72727272727273, 0.8, 0.802083333333333, 0.80153452685422,
     0.207070707070707, 0.197916666666667, 0.207070707070707, 0.197916666666667,
     "Average / Total", 0.594942206400108, 0.792929292929293, 0.807070707070707,
     0.8, 1, 40, 0.802083333333333, 1.36363636363636))

})

