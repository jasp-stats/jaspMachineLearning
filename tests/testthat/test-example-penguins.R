context("Example: penguins")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in the module's examples/ folder.

test_that("mlClassificationDecisionTree results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("..", "..", "examples", "penguins.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("mlClassificationDecisionTree", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["classificationTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(33, 300, 0.01, 22, 1))

  table <- results[["results"]][["confusionTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Observed", "Adelie", 17, 0, 0, "", "Chinstrap", 0, 10, 0, "",
     "Gentoo", 0, 0, 6))

  plotName <- results[["results"]][["decisionBoundary"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_decision-boundary-matrix")

  plotName <- results[["results"]][["decisionTreePlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-2_decision-tree-plot")

  plotName <- results[["results"]][["plotDataSplit"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-3_data-split")

  plotName <- results[["results"]][["rocCurve"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-4_roc-curves-plot")

  table <- results[["results"]][["validationMeasures"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1, 0.970588235294118, 1, 0, 0, 0, 0, "Adelie", 1, 1, 1, 1, 0.515151515151515,
     17, 1, "<unicode>", 1, 1, 1, 0, 0, 0, 0, "Chinstrap", 1, 1,
     1, 1, 0.303030303030303, 10, 1, "<unicode>", 1, 1, 1, 0, 0,
     0, 0, "Gentoo", 1, 1, 1, 1, 0.181818181818182, 6, 1, "<unicode>",
     1, 0.990196078431373, 1, 0, 0, 0, 0, "Average / Total", 1, 1,
     1, 1, 1, 33, 1, "<unicode>"))

})

