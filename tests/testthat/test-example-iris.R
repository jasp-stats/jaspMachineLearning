context("Example: iris")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in the module's examples/ folder.

test_that("mlClusteringKMeans results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("..", "..", "examples", "iris.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("mlClusteringKMeans", encoded$dataset, encoded$options, encodedDataset = TRUE)

  plotName <- results[["results"]][["clusterDensities"]][["collection"]][["clusterDensities_oneFigure"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_all-features")

  table <- results[["results"]][["clusterInfoTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(-1.01119138320281, 0.850413715115632, -1.30063008999938, -1.25070351696687,
     1, 0.340925770899256, 0.636316174439295, 50, 47.3506211055712,
     -0.0500522113876544, -0.880426958760276, 0.346576747898886,
     0.280587305699797, 2, 0.31743153016038, 0.393377210558143, 53,
     44.0875445465443, 1.13217736944013, 0.0881264480534628, 0.992828443858037,
     1.01412869460115, 3, 0.341642698940364, 0.347392234026205, 47,
     47.450194065236))

  table <- results[["results"]][["clusteringTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.46, 162.89, 199.02, 3, 0.766965839400417, 150))

  plotName <- results[["results"]][["plot2dCluster"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-2_t-sne-cluster-plot")

})

