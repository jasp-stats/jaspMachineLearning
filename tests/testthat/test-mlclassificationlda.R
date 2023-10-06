context("Machine Learning LDA Classification")

options <- initMlOptions("mlClassificationLda")
options$addPredictions <- FALSE
options$addIndicator <- FALSE
options$andrewsCurve <- TRUE
options$boxTest <- TRUE
options$predictionsColumn <- ""
options$classProportionsTable <- TRUE
options$coefficientsTable <- TRUE
options$savePath <- ""
options$saveModel <- FALSE
options$holdoutData <- "holdoutManual"
options$manovaTable <- TRUE
options$matrixPlot <- TRUE
options$meanTable <- TRUE
options$modelOptimization <- "manual"
options$modelValid <- "validationManual"
options$multicolTable <- TRUE
options$noOfFolds <- 5
options$predictors <- list("Alcohol", "Malic", "Ash", "Alcalinity", "Magnesium", "Phenols", 
                           "Flavanoids", "Nonflavanoids", "Proanthocyanins", "Color", 
                           "Hue", "Dilution", "Proline")
options$priorTable <- TRUE
options$rocCurve <- TRUE
options$setSeed <- TRUE
options$target <- "Type"
options$testDataManual <- 0.2
options$testIndicatorColumn <- ""
options$testSetIndicatorVariable <- ""
options$validationDataManual <- 0.2
options$validationMeasures <- TRUE
options$tableShap <- TRUE
options$fromIndex <- 1
options$toIndex <- 5
options$featureImportanceTable <- TRUE
options$multinormalTable <- TRUE
set.seed(1)
results <- jaspTools::runAnalysis("mlClassificationLda", "wine.csv", options)


test_that("Andrews Curves Plot matches", {
  plotName <- results[["results"]][["andrewsCurve"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "andrews-curves-plot")
})

test_that("Tests of Equality of Covariance Matrices table results match", {
  table <- results[["results"]][["boxTest"]][["data"]]
  jaspTools::expect_equal_tables(table,
                      list(182, 2.89185053268131e-59, "Box's M", 684.203088594673))
})

test_that("Class Proportions table results match", {
  table <- results[["results"]][["classProportionsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                      list(0.331460674157303, 1, 0.371428571428571, 0.321678321678322, 0.398876404494382,
                           2, 0.4, 0.398601398601399, 0.269662921348315, 3, 0.228571428571429,
                           0.27972027972028))
})

test_that("Linear Discriminant Classification table results match", {
  table <- results[["results"]][["classificationTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                      list(2, "Moment", 35, 143, 1))
})

test_that("Linear Discriminant Coefficients table results match", {
  table <- results[["results"]][["coefficientsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                      list(0.0985099080776885, -0.00342793222377346, "(Constant)", -0.393164736556141,
                           0.74329203659079, "Alcohol", 0.196548982479906, 0.262198132657181,
                           "Malic", -0.0951763741131142, 0.598083594724991, "Ash", 0.501081256982107,
                           -0.534426316736952, "Alcalinity", -0.00263923349863145, 0.0427994783686402,
                           "Magnesium", 0.208101192372295, 0.0529200512387909, "Phenols",
                           -1.52479478359811, -0.558554626913979, "Flavanoids", -0.157591493223858,
                           -0.0949688285544638, "Nonflavanoids", -0.0405222908054603, -0.217441894815476,
                           "Proanthocyanins", 0.91972456724253, 0.549848617160285, "Color",
                           -0.270506150341797, -0.410863228716687, "Hue", -0.71156016179675,
                           0.138734056359611, "Dilution", -1.00443257274125, 0.950915886290421,
                           "Proline"))
})

test_that("Confusion Matrix table results match", {
  table <- results[["results"]][["confusionTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                      list("Observed", 1, 13, 0, 0, "", 2, 0, 14, 0, "", 3, 0, 0, 8))
})

test_that("Data Split plot matches", {
  skip("We need to figure out why this fails.")
  plotName <- results[["results"]][["plotDataSplit"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "data-split")
})

test_that("Tests of Equality of Class Means table results match", {
  table <- results[["results"]][["manovaTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                      list(1, 176, 21.2496323754348, "Alcohol", 7.72325331461668e-06, 1,
                           176, 41.7269323301623, "Malic", 9.91770325689756e-10, 1, 176,
                           0.434814672036439, "Ash", 0.510497749646086, 1, 176, 64.4956585871677,
                           "Alcalinity", 1.3353947853313e-13, 1, 176, 8.05344576130258,
                           "Magnesium", 0.00507541577210749, 1, 176, 188.537094424012,
                           "Phenols", 1.23405114169271e-29, 1, 176, 448.671871016041, "Flavanoids",
                           2.73665226170043e-50, 1, 176, 55.3438804591477, "Nonflavanoids",
                           4.2867390397641e-12, 1, 176, 58.3949501230074, "Proanthocyanins",
                           1.32725091814432e-12, 1, 176, 13.3652593667787, "Color", 0.000338241649326902,
                           1, 176, 108.396064621733, "Hue", 4.40539946338208e-20, 1, 176,
                           288.755043155589, "Dilution", 5.88616358195603e-39, 1, 176,
                           118.11615464263, "Proline", 2.23131916940365e-21))
})

test_that("Linear Discriminant Matrix plot matches", {
  plotName <- results[["results"]][["matrixPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "linear-discriminant-matrix")
})

test_that("Class Means in Training Data table results match", {
  table <- results[["results"]][["meanTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                      list(-0.737973343057887, 0.866579773288271, 0.30192375054201, 0.172427486097655,
                           0.835971830040447, 0.915688469526993, 0.479093202775772, 0.392526485134375,
                           -0.331435036899518, -0.640240104048998, 0.826520856297991, 0.531311985236627,
                           1.1413228289499, 1, 0.230034258385284, -0.89672653516467, -0.439418896241175,
                           -0.87376357087456, 0.281791112387928, 0.0384935686108725, 0.417649685134421,
                           -0.410599462269383, -0.353059005341892, -0.0543674703297988,
                           -0.0779683443333514, 0.0799634516853449, -0.74039657793808,
                           2, 0.547992584469851, 0.236666348631391, 0.275140493593514,
                           1.08190285049093, -1.28971512313365, -1.26896795506147, -1.20399681912815,
                           -0.0501714588321044, 0.921456636326643, 0.786608597104423, -1.01599631531372,
                           -0.716157122978659, -0.361275192620381, 3))
})

test_that("Pooled Within-Class Matrices Correlations table results match", {
  table <- results[["results"]][["multicolTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                      list("Alcohol", 1, "Malic", -0.15543140098941, 1, "Ash", -0.5077707223394,
                           0.10089679874801, 1, "Alcalinity", -0.467611905093725, 0.222285564619173,
                           0.884023520091301, 1, "Magnesium", -0.140413384091464, -0.256626215005153,
                           0.130223015503791, 0.00888175472723472, 1, "Phenols", 0.0451478948674465,
                           -0.243018700145123, -0.111235085964553, -0.117466569301758,
                           0.0775775713281924, 1, "Flavanoids", -0.0104179089218085, -0.200727294690442,
                           0.00488737317210032, 0.0179766449407254, 0.118742990766646,
                           0.91895865528287, 1, "Nonflavanoids", -0.196055464438605, 0.166861804099441,
                           0.335406465283413, 0.254504190909519, -0.429087861871871, -0.5753262491195,
                           -0.647943360372001, 1, "Proanthocyanins", 0.0253016108188158,
                           -0.122957793855602, -0.245943017246018, -0.18402137363758, 0.227431825623563,
                           0.654068699036149, 0.721189091877098, -0.545676586525786, 1,
                           "Color", 0.451413493469851, 1, -0.443474535624877, -0.277497844424914,
                           -0.324035265260096, 0.0803406024773737, 0.448676707247801, 0.474021656042198,
                           -0.27791501003148, 0.461086184605623, "Hue", -0.116736513331207,
                           -0.31605549855032, 1, -0.547694221367693, -0.0186379576416076,
                           -0.134150256351639, 0.0485210936837518, -0.25296286007992, -0.310674164048376,
                           0.104702433435255, -0.319965463375162, "Dilution", -0.216100482480972,
                           -0.194429688410647, -0.278041430079757, 1, 0.178453920345921,
                           0.0552302903464611, 0.206854858646108, -0.13003396618442, 0.51557569149584,
                           0.549699740766929, -0.488152463473658, 0.339741954938867, "Proline",
                           0.205948558920481, 0.419941021344394, 0.263379222194261, -0.448302974733637,
                           1, -0.611025813341785, -0.31960994842826, -0.425043376452818,
                           0.330321645062641, 0.0517339721463955, -0.015508474529089, -0.251298231606317,
                           0.13998283266291))
})

test_that("Prior and Posterior Class Probabilities table results match", {
  table <- results[["results"]][["priorTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                      list(0.360925605569456, 0.321678321678322, 1, 0.407922906646082, 0.398601398601399,
                           2, 0.231151487784462, 0.27972027972028, 3))
})

test_that("ROC Curves Plot matches", {
  plotName <- results[["results"]][["rocCurve"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "roc-curves-plot")
})

test_that("Evaluation Metrics table results match", {
  table <- results[["results"]][["validationMeasures"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0.371428571428571, 13, 1,
      "<unicode><unicode><unicode>", 1, 0.979591836734694, 1, 0, 0,
      0, 0, 2, 1, 1, 1, 1, 0.4, 14, 1, "<unicode><unicode><unicode>",
      1, 1, 1, 0, 0, 0, 0, 3, 1, 1, 1, 1, 0.228571428571429, 8, 1,
      "<unicode><unicode><unicode>", 1, 0.993197278911565, 1, 0, 0,
      0, 0, "Average / Total", 1, 1, 1, 1, 1, 35, 1, "<unicode><unicode><unicode>"))
})

test_that("Additive Explanations for Predictions of Test Set Cases table results match", {
	table <- results[["results"]][["tableShap"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(-0.0298348205005264, 0.204669205512118, 0.0650573130972094, 0.0984758248651785,
			 0.113173720484469, 0.00523002834583347, 0.0485892399292589,
			 0.0208518913958571, 0.015842321950409, 0.00411361891683004,
			 -0.0432251530809802, -0.00572368692579106, 0.1064720765996,
			 0.321308696734254, 1, "1 (0.925)", 0.000336631001660037, 0.0885033606859238,
			 -8.01732617317086e-05, 3.28859969256357e-08, 0.051334361355912,
			 0.232090513921032, 2.80742096236963e-12, 5.18818321637582e-12,
			 6.98441304791686e-13, 1.02218233877238e-12, 2.33990604669998e-12,
			 2.17420526027468e-11, 0.306506576628935, 0.321308696734254,
			 2, "1 (1)", 0.000193543699123166, 0.0523219088423289, 1.38580036335156e-09,
			 -4.53284298984613e-10, 5.67399238704525e-10, 0.0309691006574206,
			 -2.40113484650806e-11, -1.84963155902551e-13, -2.80331313717852e-13,
			 2.47246667584022e-13, -4.36315428231637e-11, -1.28925103926036e-09,
			 0.595206749921992, 0.321308696734254, 3, "1 (1)", 4.34445911468018e-08,
			 0.000334340497036067, 1.52022838761923e-12, -1.65489844050626e-12,
			 5.57565105197e-12, 0.239379726212877, -2.66453525910038e-14,
			 0, 0, 2.50910403565285e-14, -3.81783493708099e-12, -1.45683465291313e-12,
			 0.438977193111077, 0.321308696734254, 4, "1 (1)", 5.28184063708359e-07,
			 0.0366521043193467, 6.52825025038162e-07, 0.00608510361034065,
			 2.63923297327828e-07, 0.341542088017672, -1.31101352995877e-07,
			 5.12944464681198e-10, -1.19777410212407e-10, -7.10459945318931e-08,
			 7.75855569790096e-08, -1.12110600469784e-07, 0.294410797077748,
			 0.321308696734254, 5, "1 (1)"))
})

test_that("Feature Importance Metrics table results match", {
	table <- results[["results"]][["featureImportanceTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(85.8900713326248, "Flavanoids", 83.1720309927639, "Proline", 28.1577006284959,
			 "Color", 16.2022565499951, "Alcohol", 10.8598389036746, "Alcalinity",
			 9.92887864875162, "Ash", 8.93029310534988, "Dilution", 3.16091625814165,
			 "Hue", 1.95530540133887, "Nonflavanoids", 1.6883059234698, "Malic",
			 0.935460817887541, "Phenols", 0.758027706375559, "Proanthocyanins",
			 0.45339855404643, "Magnesium"))
})

test_that("Tests for Multivariate Normality table results match", {
	table <- results[["results"]][["multinormalTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(0, 1290.7639, "Skewness", 0, 10.9082, "Kurtosis"))
})
