context("Machine Learning K-Means Clustering")

options <- jaspTools::analysisOptions("mlClusteringKMeans")
options$predictors <- list("Alcohol", "Malic", "Ash", "Alcalinity", "Magnesium", "Phenols", 
    "Flavanoids", "Nonflavanoids", "Proanthocyanins", "Color", 
    "Hue", "Dilution", "Proline")
options$tableClusterInformationSilhouetteScore <- TRUE
options$tableClusterInformationCentroids <- TRUE
options$tableClusterInformationBetweenSumOfSquares <- TRUE
options$tableClusterInformationTotalSumOfSquares <- TRUE
options$tableClusterEvaluationMetrics <- TRUE
options$elbowMethodPlot <- TRUE
options$clusterMeanPlot <- TRUE
options$tsneClusterPlot <- TRUE
options$algorithm <- "Hartigan-Wong"
options$randomSeed <- TRUE
options$clusterDeterminationMethod <- "optimized"
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
options$tableClusterInformationSilhouetteScore <- TRUE
options$tableClusterInformationCentroids <- TRUE
options$tableClusterInformationBetweenSumOfSquares <- TRUE
options$tableClusterInformationTotalSumOfSquares <- TRUE
options$tableClusterEvaluationMetrics <- TRUE
options$elbowMethodPlot <- TRUE
options$clusterMeanPlot <- TRUE
options$centers <- "medians"
options$randomSeed <- TRUE
options$clusterDeterminationMethod <- "optimized"
options$addPredictions <- FALSE
options$predictionsColumn <- ""
set.seed(1)
results <- jaspTools::runAnalysis("mlClusteringKMeans", "wine.csv", options)


test_that("Evaluation Metrics table results match", {
	table <- results[["results"]][["clusterEvaluationMetrics"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list("Maximum diameter", 7.54064200105901, "Minimum separation", 1.50897550103816,
			 "Pearson's <unicode><unicode>", 0.592852725815786, "Dunn index",
			 0.200112338024566, "Entropy", 1.57187361499907, "Calinski-Harabasz index",
			 39.453078864832))
})

test_that("Cluster Information table results match", {
	table <- results[["results"]][["clusterInfoTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(-0.909333712594198, -0.555286362421824, -0.410647308117152, -1.18980055515824,
			 -0.427430413546411, 0.457000505588578, 0.657295156536076, 0.600394638211174,
			 1.40211753711865, -1.02944824750672, -0.933589885376394, -1.6600968993946,
			 -0.693415931872898, 1, 0.0241351830024829, 0.231597537771501,
			 6, 25.8676545218679, 0.202571872537633, 0.891173835871273, -1.13377174117665,
			 -1.1978668766662, -0.479459698153808, 1.18716062309179, 0.410393737785899,
			 0.78647358291341, -0.290039548498121, -0.88922767084512, -1.17213245826513,
			 0.883705255279883, -0.689170060861989, 2, 0.227305519588028,
			 0.19399041860386, 43, 243.621962634875, -0.861369907004078,
			 -0.771658428370357, 0.586195489583433, 0.59901436340363, -0.881056025012055,
			 -0.578928104188156, -1.01877880473882, -0.260714249220329, -0.762826454023543,
			 0.398604797603305, 0.376484372049327, -0.640626617284001, 0.315591390341046,
			 3, 0.23831120988857, 0.103012707668149, 36, 255.417663311345,
			 -0.430165632508692, -0.431168130587258, 0.0710713296334867,
			 -0.923308186503816, -0.479121134667263, -0.431913378191017,
			 -0.801213995055997, -0.454241752375995, 0.049409255755503, -0.849317081052027,
			 -0.828643281620597, 0.352008120515292, -1.17537943045464, 4,
			 0.0615049834355613, 0.145695729291229, 11, 65.9199336802472,
			 0.889132732399879, 0.144569305778644, 0.522865177028873, 0.776482777455151,
			 1.14313690841256, -0.30521978594598, 0.325685208327842, -0.689628807712809,
			 0.436166358075998, 0.833709713152964, 0.92088928865172, -0.54379342910268,
			 0.52499406644893, 5, 0.324065107745409, 0.313338315110425, 61,
			 347.327146800077, -1.22427114209035, -1.02187409744718, 0.805043907807248,
			 0.111802819494604, -0.766640776500708, -0.564809717860675, 0.157680184917129,
			 0.881448863884847, -1.03465272653572, -0.666512916302385, -0.301822051437334,
			 0.967699160483516, -0.141909945833682, 6, 0.124677996339949,
			 0.166544069846377, 21, 133.627631307735))
})

test_that("All predictors plot matches", {
	plotName <- results[["results"]][["clusterMeans"]][["collection"]][["clusterMeans_oneFigure"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "all-predictors-2")
})

test_that("K-Medians Clustering table results match", {
	table <- results[["results"]][["clusteringTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(0.21, 1227.78, 1475.96, 6, 0.53859950101177, 178))
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
options$tableClusterInformationSilhouetteScore <- TRUE
options$tableClusterInformationCentroids <- TRUE
options$tableClusterInformationBetweenSumOfSquares <- TRUE
options$tableClusterInformationTotalSumOfSquares <- TRUE
options$tableClusterEvaluationMetrics <- TRUE
options$elbowMethodPlot <- TRUE
options$clusterMeanPlot <- TRUE
options$centers <- "medoids"
options$randomSeed <- TRUE
options$clusterDeterminationMethod <- "optimized"
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
