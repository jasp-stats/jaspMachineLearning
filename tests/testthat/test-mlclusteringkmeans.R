context("Machine Learning K-Means Clustering")

options <- jaspTools::analysisOptions("mlClusteringKMeans")
options$predictors <- list("Alcohol", "Malic", "Ash", "Alcalinity", "Magnesium", "Phenols", 
    "Flavanoids", "Nonflavanoids", "Proanthocyanins", "Color", 
    "Hue", "Dilution", "Proline")
options$tableClusterInfoSilhouette <- TRUE
options$tableClusterInfoCentroids <- TRUE
options$tableClusterInfoBetweenSumSquares <- TRUE
options$tableClusterInfoTotalSumSquares <- TRUE
options$clusterEvaluationMetrics <- TRUE
options$withinssPlot <- TRUE
options$plotClusterMeans <- TRUE
options$plot2dCluster <- TRUE
options$algorithm <- "Hartigan-Wong"
options$seedBox <- TRUE
options$modelOpt <- "validationOptimized"
options$addPredictions <- FALSE
options$predictionsColumn <- ""
set.seed(1)
results <- jaspTools::runAnalysis("mlClusteringKMeans", "wine.csv", options)


test_that("Evaluation Metrics table results match", {
	table <- results[["results"]][["clusterEvaluationMetrics"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list("Maximum diameter", 8.97000459794371, "Minimum separation", 1.60349035617523,
			 "Pearson's <unicode><unicode>", 0.622353795934006, "Dunn index",
			 0.178761375054681, "Entropy", 1.47556540322037, "Calinski-Harabasz index",
			 47.2866877681223))
})

test_that("Cluster Information table results match", {
	table <- results[["results"]][["clusterInfoTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(-0.730833814176345, -0.74707612935895, 0.17606244829255, 0.639563322424828,
			 -0.6995003034381, 0.153720814884363, -0.256477289296812, 0.0855876487085092,
			 -0.528568111653367, 0.714539559517063, 0.624134979374367, -0.602079907880078,
			 0.646893048915268, 1, 0.160931654126512, 0.0984645246472935,
			 26, 176.896648011844, -0.921299089995289, -0.916064607877969,
			 0.536798629417091, 0.0350708567135793, -0.769844680090664, -0.558048304218843,
			 -0.629189664097712, 0.18336923491786, -0.690602880280368, -0.526097546097289,
			 -0.309134678083177, 0.41625724048548, -0.464353646841332, 2,
			 0.247000239101422, 0.142777266079956, 41, 271.503543490686,
			 0.981705546386994, 0.255561956674198, 0.488357488677896, 0.772806160311654,
			 1.26108553315067, -0.394404612616799, 0.265825343740946, -0.82305585865629,
			 0.455044649627823, 0.908509053731672, 0.987969032048833, -0.614556122250903,
			 0.559460603458105, 3, 0.22127006271421, 0.34422711905128, 54,
			 243.220841865851, -0.669007419814319, -0.639735547737523, 0.656470813650849,
			 0.474745282591851, 0.0277279591190561, -0.504722091238357, 0.741707999252534,
			 0.772572814789805, 2.49490412001043, 0.279439717765542, 0.437479639145589,
			 -0.406609677680301, 1.07948205904972, 4, 0.0951516921201966,
			 0.00679859389123221, 8, 104.591079238431, 0.186018402285889,
			 0.985717694037202, -1.18794772162286, -1.29787849848707, -0.378975565101653,
			 0.902425818310064, 0.248509248850859, 0.582061558185055, -0.0504929601123107,
			 -0.985776240838559, -1.23271739808418, 0.714825280978949, -0.747498955315058,
			 5, 0.27564635193766, 0.320348147018739, 49, 302.991452856956
			))
})

test_that("All predictors plot matches", {
	plotName <- results[["results"]][["clusterMeans"]][["collection"]][["clusterMeans_oneFigure"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "all-predictors")
})

test_that("K-Means Clustering table results match", {
	table <- results[["results"]][["clusteringTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(0.24, 1229.2, 1436.02, 5, 0.522293104970114, 178))
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

context("Machine Learning K-Medians Clustering")

options <- jaspTools::analysisOptions("mlClusteringKMeans")
options$predictors <- list("Alcohol", "Malic", "Ash", "Alcalinity", "Magnesium", "Phenols", 
    "Flavanoids", "Nonflavanoids", "Proanthocyanins", "Color", 
    "Hue", "Dilution", "Proline")
options$tableClusterInfoSilhouette <- TRUE
options$tableClusterInfoCentroids <- TRUE
options$tableClusterInfoBetweenSumSquares <- TRUE
options$tableClusterInfoTotalSumSquares <- TRUE
options$clusterEvaluationMetrics <- TRUE
options$withinssPlot <- TRUE
options$plotClusterMeans <- TRUE
options$centers <- "medians"
options$seedBox <- TRUE
options$modelOpt <- "validationOptimized"
options$addPredictions <- FALSE
options$predictionsColumn <- ""
set.seed(1)
results <- jaspTools::runAnalysis("mlClusteringKMeans", "wine.csv", options)


test_that("Evaluation Metrics table results match", {
	table <- results[["results"]][["clusterEvaluationMetrics"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list("Maximum diameter", 8.51516001364652, "Minimum separation", 1.60632766808558,
			 "Pearson's <unicode><unicode>", 0.570325980213938, "Dunn index",
			 0.188643274525817, "Entropy", 1.36305780660858, "Calinski-Harabasz index",
			 53.7349408737258))
})

test_that("Cluster Information table results match", {
	table <- results[["results"]][["clusterInfoTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(-0.787973831989191, -0.821921669628806, 0.530773010236986, -0.0805575730926988,
			 -0.713865760078925, -0.553765128287757, -0.726598012728438,
			 -0.0359605164066285, -0.602766256172083, -0.508303564461616,
			 -0.320923378757268, 0.298385992711599, -0.445055759278768, 1,
			 0.336055246839718, 0.182454420501865, 53, 401.389760284746,
			 1.03588876253043, 0.279433788122583, 0.434242690697805, 0.797855375549058,
			 1.25147075609497, -0.328804813815894, 0.0896593337613188, -0.899978390422253,
			 0.449999033232327, 0.909246344252296, 1.00360662271484, -0.640433365174497,
			 0.639524522609801, 2, 0.183188099972413, 0.309145431831863,
			 47, 218.802795749876, 0.14647504112479, 0.810569883643298, -1.16110320796736,
			 -1.28943112185576, -0.418231489511521, 0.86593190868353, 0.211528918534768,
			 0.579828808398242, -0.0314790776378002, -1.02744782006489, -1.21743762866723,
			 0.527704078233325, -0.869664641448849, 3, 0.253673305985526,
			 0.339691943444652, 49, 302.991452856957, 0.00327133148196856,
			 -0.384707175875306, 0.693198071328121, 0.687916527934656, 0.407703241886847,
			 -0.28530544941901, 1.00144785536574, 0.130057657179639, 0.359961249782629,
			 0.639905468300686, 0.613046384559695, -0.380028830955218, 0.242674682193114,
			 4, 0.227083347202342, -0.0196777717959873, 29, 271.231980917946
			))
})

test_that("All predictors plot matches", {
	plotName <- results[["results"]][["clusterMeans"]][["collection"]][["clusterMeans_oneFigure"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "all-predictors-2")
})

test_that("K-Medians Clustering table results match", {
	table <- results[["results"]][["clusteringTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(0.23, 1298.42, 1463.87, 4, 0.470711342710878, 178))
})

test_that("Elbow Method Plot matches", {
	plotName <- results[["results"]][["optimPlot"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "elbow-method-plot-2")
})

context("Machine Learning K-Medoids Clustering")

options <- jaspTools::analysisOptions("mlClusteringKMeans")
options$predictors <- list("Alcohol", "Malic", "Ash", "Alcalinity", "Magnesium", "Phenols", 
    "Flavanoids", "Nonflavanoids", "Proanthocyanins", "Color", 
    "Hue", "Dilution", "Proline")
options$tableClusterInfoSilhouette <- TRUE
options$tableClusterInfoCentroids <- TRUE
options$tableClusterInfoBetweenSumSquares <- TRUE
options$tableClusterInfoTotalSumSquares <- TRUE
options$clusterEvaluationMetrics <- TRUE
options$withinssPlot <- TRUE
options$plotClusterMeans <- TRUE
options$centers <- "medoids"
options$seedBox <- TRUE
options$modelOpt <- "validationOptimized"
options$addPredictions <- FALSE
options$predictionsColumn <- ""
set.seed(1)
results <- jaspTools::runAnalysis("mlClusteringKMeans", "wine.csv", options)


test_that("Evaluation Metrics table results match", {
	table <- results[["results"]][["clusterEvaluationMetrics"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list("Maximum diameter", 8.32828431318771, "Minimum separation", 1.3319097118158,
			 "Pearson's <unicode><unicode>", 0.506782138443401, "Dunn index",
			 0.159926061806841, "Entropy", 1.58843873936718, "Calinski-Harabasz index",
			 43.9303597205423))
})

test_that("Cluster Information table results match", {
	table <- results[["results"]][["clusterInfoTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(1.50202286496714, 0.570210142780351, -0.076341246291495, 0.983554958559391,
			 0.708483474663071, -0.569619601201303, -0.242457832996962, -0.956694958673843,
			 1.278378998074, 1.44585144042711, 0.971839512074906, -0.818410597429238,
			 0.767177993092458, 1, 0.1238408435686, 0.217986939183351, 29,
			 141.366978410927, 0.627451799892053, -0.370139805643522, 0.623658333354924,
			 0.363828294405156, 1.10542546623392, -0.480105794355861, 1.03331269028942,
			 -0.148206129522007, 0.718252323225578, 0.0877008044513121, 0.501302481111386,
			 -0.577356400503032, -0.0889282576435221, 2, 0.235432859140199,
			 0.0239752948508857, 38, 268.751657015799, -0.87533228375809,
			 -0.930898949199043, 1.19240799181764, 0.180727234541404, -1.01265700078815,
			 -0.829209641053084, -1.40887659714451, -1.04652705080182, -1.03214353567575,
			 0.407265659975028, 0.471268202539246, -0.577356400503032, 0.312917533518265,
			 3, 0.198290738977986, 0.0761447463431758, 29, 226.353130424698,
			 -1.67599593488317, -0.974034267934083, 0.186158596075912, 0.194811931454,
			 -0.212421945781311, -0.247369896557712, 0.340751549076813, 0.630338668920501,
			 -1.1021593700318, -0.55142890659612, -0.339657318908522, 0.949320180029613,
			 -0.420887824255433, 4, 0.178926950604445, 0.164662146424061,
			 33, 204.248950785128, 0.393411655717031, 1.45017064497517, -1.78259022167964,
			 -1.39675881966938, -0.307688023758316, 0.8088930242185, 0.0491468580399253,
			 0.600394638211174, -0.542032695183377, -0.583385392148492, -1.27071995464485,
			 0.708265983103406, -0.595603385630123, 5, 0.26350860770877,
			 0.299304347024917, 49, 300.800726025614))
})

test_that("All predictors plot matches", {
	plotName <- results[["results"]][["clusterMeans"]][["collection"]][["clusterMeans_oneFigure"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "all-predictors-3")
})

test_that("K-Medoids Clustering table results match", {
	table <- results[["results"]][["clusteringTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(0.17, 1271.52, 1478.34, 5, 0.579748312323366, 178))
})

test_that("Elbow Method Plot matches", {
	plotName <- results[["results"]][["optimPlot"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "elbow-method-plot-3")
})