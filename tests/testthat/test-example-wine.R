context("Example: wine")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in the module's examples/ folder.

test_that("mlClusteringRandomForest results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("..", "..", "examples", "wine.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("mlClusteringRandomForest", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["clusterInfoTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1, 0.199425939703478, 0.411069251367668, 57, 267.166873258102,
     2, 0.612907360221033, 0.0747428309831974, 76, 821.0995183004,
     3, 0.187666700075488, 0.39282044191035, 45, 251.413259219857
    ))

  table <- results[["results"]][["clusteringTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.26, 1417.68, 1541.77, 3, 0.417783724129353, 178))

  table <- results[["results"]][["importanceTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(20.3865617163522, "jaspColumn7", 16.7221375429735, "jaspColumn12",
     16.5937155443686, "jaspColumn6", 15.1922351110861, "jaspColumn13",
     14.7701748175345, "jaspColumn10", 13.763257093803, "jaspColumn9",
     13.6109822805815, "jaspColumn11", 13.4924520858126, "jaspColumn1",
     12.0407649461782, "jaspColumn2", 10.9165824200026, "jaspColumn4",
     10.6695338188, "jaspColumn8", 9.6982366053581, "jaspColumn5",
     9.64091096096958, "jaspColumn3"))

  plotName <- results[["results"]][["optimPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_elbow-method-plot")

  plotName <- results[["results"]][["plot2dCluster"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-2_t-sne-cluster-plot")

})

