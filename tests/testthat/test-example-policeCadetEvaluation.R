context("Example: policeCadetEvaluation")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in the module's examples/ folder.

test_that("mlClassificationDecisionTree results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("..", "..", "examples", "policeCadetEvaluation.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("mlClassificationDecisionTree", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["classificationTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(400, 1600, 0.001, 22, 0.79))

  table <- results[["results"]][["confusionTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Observed", "Fail", 0, 81, "", "Pass", 3, 316))

  plotName <- results[["results"]][["decisionTreePlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_decision-tree-plot")

  plotName <- results[["results"]][["plotDataSplit"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-2_data-split")

  table <- results[["results"]][["splitsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1600, 40.9912821282016, 0.5, "jaspColumn3", 739, 1.25676113142475,
     0.5, "jaspColumn2", 524, 0.65386154342491, 5, "jaspColumn4",
     501, 0.695752656461394, 7, "jaspColumn5", 63, 3.30687830687831,
     26.5, "jaspColumn1", 215, 1.31686685407615, 39.5, "jaspColumn1"
    ))

})

