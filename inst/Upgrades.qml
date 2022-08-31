
import QtQuick		2.12
import JASP.Module	1.0

Upgrades
{
	Upgrade
	{

		functionName:		"mlRegressionBoosting"
		fromVersion:		"0.16.4"
		toVersion:			"0.17.0"

		ChangeRename { from: "classBoostRelInfTable";					to: "relativeInfluenceTable"		}

		ChangeRename { from: "plotOOBChangeDev";						to: "outOfBagImprovementPlot"		}
		ChangeRename { from: "plotDeviance";							to: "deviancePlot"					}
		ChangeRename { from: "plotRelInf";								to: "relativeInfluencePlot"			}
		ChangeRename { from: "intDepth";								to: "interactionDepth"				}
		ChangeRename { from: "nNode";									to: "minObservationsInNode"			}
		ChangeRename { from: "bagFrac";									to: "baggingFraction"				}
		ChangeRename { from: "scaleEqualSD";							to: "scaleVariables"				}

		ChangeRename { from: "seedBox";									to: "setSeed"						}
		ChangeRename { from: "modelOpt";								to: "modelOptimization"				}

	}

	Upgrade
	{

		functionName:		"mlClassificationBoosting"
		fromVersion:		"0.16.4"
		toVersion:			"0.17.0"

		ChangeRename { from: "classBoostRelInfTable";					to: "relativeInfluenceTable"		}
		ChangeRename { from: "plotOOBChangeDev";						to: "outOfBagImprovementPlot"		}
		ChangeRename { from: "plotDeviance";							to: "deviancePlot"					}
		ChangeRename { from: "plotRelInf";								to: "relativeInfluencePlot"			}
		ChangeRename { from: "intDepth";								to: "interactionDepth"				}
		ChangeRename { from: "nNode";									to: "minObservationsInNode"			}
		ChangeRename { from: "bagFrac";									to: "baggingFraction"				}
		ChangeRename { from: "scaleEqualSD";							to: "scaleVariables"				}
		ChangeRename { from: "seedBox";									to: "setSeed"						}
		ChangeRename { from: "modelOpt";								to: "modelOptimization"				}

		ChangeRename { from: "plotLegend";								to: "legendShown"					}
		ChangeRename { from: "plotPoints";								to: "pointsShown"					}

	}

	Upgrade
	{

		functionName:		"mlRegressionDecisionTree"
		fromVersion:		"0.16.4"
		toVersion:			"0.17.0"

		ChangeRename { from: "intDepth";								to: "interactionDepth"				}
		ChangeRename { from: "nNode";									to: "minObservationsInNode"			}
		ChangeRename { from: "scaleEqualSD";							to: "scaleVariables"				}
		ChangeRename { from: "seedBox";									to: "setSeed"						}
		ChangeRename { from: "modelOpt";								to: "modelOptimization"				}

		ChangeRename { from: "plotDecisionTree";						to: "decisionTreePlot"				}
		ChangeRename { from: "cp";										to: "complexityParameter"			}
		ChangeRename { from: "nSplit";									to: "minObservationsForSplit"		}

		ChangeRename { from: "tableVariableImportance";					to: "variableImportanceTable"		}
		ChangeRename { from: "tableSplitsTree";							to: "splitsTreeTable"				}
		ChangeRename { from: "tableSplits";								to: "splitsTable"					}

	}

	Upgrade
	{

		functionName:		"mlClassificationDecisionTree"
		fromVersion:		"0.16.4"
		toVersion:			"0.17.0"

		ChangeRename { from: "intDepth";								to: "interactionDepth"				}
		ChangeRename { from: "nNode";									to: "minObservationsInNode"			}
		ChangeRename { from: "scaleEqualSD";							to: "scaleVariables"				}
		ChangeRename { from: "seedBox";									to: "setSeed"						}
		ChangeRename { from: "modelOpt";								to: "modelOptimization"				}

		ChangeRename { from: "plotLegend";								to: "legendShown"					}
		ChangeRename { from: "plotPoints";								to: "pointsShown"					}

		ChangeRename { from: "plotDecisionTree";						to: "decisionTreePlot"				}
		ChangeRename { from: "cp";										to: "complexityParameter"			}
		ChangeRename { from: "nSplit";									to: "minObservationsForSplit"		}

		ChangeRename { from: "tableVariableImportance";					to: "variableImportanceTable"		}
		ChangeRename { from: "tableSplitsTree";							to: "splitsTreeTable"				}
		ChangeRename { from: "tableSplits";								to: "splitsTable"					}

	}

	Upgrade
	{

		functionName:		"mlRegressionRandomForest"
		fromVersion:		"0.16.4"
		toVersion:			"0.17.0"

		ChangeRename { from: "bagFrac";									to: "baggingFraction"				}
		ChangeRename { from: "seedBox";									to: "setSeed"						}
		ChangeRename { from: "modelOpt";								to: "modelOptimization"				}

		ChangeRename { from: "tableVariableImportance";					to: "variableImportanceTable"		}

		ChangeRename { from: "plotTreesVsModelError";					to: "treesVsModelErrorPlot"			}
		ChangeRename { from: "plotDecreaseAccuracy";					to: "accuracyDecreasePlot"			}
		ChangeRename { from: "plotIncreasePurity";						to: "purityIncreasePlot"			}

		ChangeRename { from: "scaleEqualSD";							to: "scaleVariables"				}

	}


	Upgrade
	{

		functionName:		"mlClassificationRandomForest"
		fromVersion:		"0.16.4"
		toVersion:			"0.17.0"

		ChangeRename { from: "bagFrac";									to: "baggingFraction"				}
		ChangeRename { from: "scaleEqualSD";							to: "scaleVariables"				}
		ChangeRename { from: "seedBox";									to: "setSeed"						}
		ChangeRename { from: "modelOpt";								to: "modelOptimization"				}

		ChangeRename { from: "plotLegend";								to: "legendShown"					}
		ChangeRename { from: "plotPoints";								to: "pointsShown"					}

		ChangeRename { from: "tableVariableImportance";					to: "variableImportanceTable"		}

		ChangeRename { from: "plotTreesVsModelError";					to: "treesVsModelErrorPlot"			}
		ChangeRename { from: "plotDecreaseAccuracy";					to: "accuracyDecreasePlot"			}
		ChangeRename { from: "plotIncreasePurity";						to: "purityIncreasePlot"			}


	}

	Upgrade
	{

		functionName:		"mlRegressionKnn"
		fromVersion:		"0.16.4"
		toVersion:			"0.17.0"

		ChangeRename { from: "scaleEqualSD";							to: "scaleVariables"				}
		ChangeRename { from: "seedBox";									to: "setSeed"						}
		ChangeRename { from: "modelOpt";								to: "modelOptimization"				}

		ChangeRename { from: "plotWeights";								to: "weightsPlot"					}
		ChangeRename { from: "plotErrorVsK";							to: "errorVsKPlot"					}
		ChangeRename { from: "maxK";									to: "maxNearestNeighbors"			}

	}

	Upgrade
	{

		functionName:		"mlClassificationKnn"
		fromVersion:		"0.16.4"
		toVersion:			"0.17.0"

		ChangeRename { from: "scaleEqualSD";							to: "scaleVariables"				}
		ChangeRename { from: "seedBox";									to: "setSeed"						}
		ChangeRename { from: "modelOpt";								to: "modelOptimization"				}

		ChangeRename { from: "plotLegend";								to: "legendShown"					}
		ChangeRename { from: "plotPoints";								to: "pointsShown"					}

		ChangeRename { from: "plotWeights";								to: "weightsPlot"					}
		ChangeRename { from: "plotErrorVsK";							to: "errorVsKPlot"					}
		ChangeRename { from: "maxK";									to: "maxNearestNeighbors"			}


	}

	Upgrade
	{

		functionName:		"mlRegressionNeuralNetwork"
		fromVersion:		"0.16.4"
		toVersion:			"0.17.0"

		ChangeRename { from: "scaleEqualSD";							to: "scaleVariables"				}
		ChangeRename { from: "seedBox";									to: "setSeed"						}
		ChangeRename { from: "modelOpt";								to: "modelOptimization"				}

		ChangeRename { from: "plotError";								to: "meanSquaredErrorPlot"			}
		ChangeRename { from: "actFuncPlot";								to: "activationFunctionPlot"		}
		ChangeRename { from: "errfct";									to: "lossFunction"					}
		ChangeRename { from: "stepMax";									to: "maxTrainingRepetitions"		}
		ChangeRename { from: "genSize";									to: "populationSize"				}
		ChangeRename { from: "maxGen";									to: "maxGenerations"				}
		ChangeRename { from: "elitismProp";								to: "elitismProportion"				}

		ChangeJS
		{
			name: "lossFunction"
			jsFunction: function(options)
			{
				switch (options["lossFunction"])
				{
					case "sse":		return "sumOfSquares";
					case "ce":		return "crossEntropy";
				}
			}
		}

	}

	Upgrade
	{

		functionName:		"mlClassificationNeuralNetwork"
		fromVersion:		"0.16.4"
		toVersion:			"0.17.0"

		ChangeRename { from: "scaleEqualSD";							to: "scaleVariables"				}
		ChangeRename { from: "seedBox";									to: "setSeed"						}
		ChangeRename { from: "modelOpt";								to: "modelOptimization"				}

		ChangeRename { from: "plotLegend";								to: "legendShown"					}
		ChangeRename { from: "plotPoints";								to: "pointsShown"					}

		ChangeRename { from: "plotError";								to: "meanSquaredErrorPlot"			}
		ChangeRename { from: "actFuncPlot";								to: "activationFunctionPlot"		}
		ChangeRename { from: "errfct";									to: "lossFunction"					}
		ChangeRename { from: "stepMax";									to: "maxTrainingRepetitions"		}
		ChangeRename { from: "genSize";									to: "populationSize"				}
		ChangeRename { from: "maxGen";									to: "maxGenerations"				}
		ChangeRename { from: "elitismProp";								to: "elitismProportion"				}

		ChangeJS
		{
			name: "lossFunction"
			jsFunction: function(options)
			{
				switch (options["lossFunction"])
				{
					case "sse":		return "sumOfSquares";
					case "ce":		return "crossEntropy";
				}
			}
		}

	}

	Upgrade
	{

		functionName:		"mlRegressionSvm"
		fromVersion:		"0.16.4"
		toVersion:			"0.17.0"

		ChangeRename { from: "scaleEqualSD";							to: "scaleVariables"				}
		ChangeRename { from: "seedBox";									to: "setSeed"						}
		ChangeRename { from: "modelOpt";								to: "modelOptimization"				}

		ChangeRename { from: "cp";										to: "complexityParameter"			}

		ChangeRename { from: "tableSupportVectors";						to: "supportVectorsTable"			}


	}
	
	Upgrade
	{

		functionName:		"mlClassificationSvm"
		fromVersion:		"0.16.4"
		toVersion:			"0.17.0"

		ChangeRename { from: "scaleEqualSD";							to: "scaleVariables"				}
		ChangeRename { from: "seedBox";									to: "setSeed"						}
		ChangeRename { from: "modelOpt";								to: "modelOptimization"				}

		ChangeRename { from: "plotLegend";								to: "legendShown"					}
		ChangeRename { from: "plotPoints";								to: "pointsShown"					}

		ChangeRename { from: "cp";										to: "complexityParameter"			}

		ChangeRename { from: "tableSupportVectors";						to: "supportVectorsTable"			}

	}
	
	Upgrade
	{

		functionName:		"mlRegressionRegularized"
		fromVersion:		"0.16.4"
		toVersion:			"0.17.0"

		ChangeRename { from: "scaleEqualSD";							to: "scaleVariables"				}
		ChangeRename { from: "seedBox";									to: "setSeed"						}
		ChangeRename { from: "modelOpt";								to: "modelOptimization"				}

		ChangeRename { from: "thresh";									to: "convergenceThreshold"			}

	}

	Upgrade
	{

		functionName:		"mlClassificationLda"
		fromVersion:		"0.16.4"
		toVersion:			"0.17.0"

		ChangeRename { from: "scaleEqualSD";							to: "scaleVariables"				}
		ChangeRename { from: "seedBox";									to: "setSeed"						}
		ChangeRename { from: "modelOpt";								to: "modelOptimization"				}

		ChangeRename { from: "plotLegend";								to: "legendShown"					}
		ChangeRename { from: "plotPoints";								to: "pointsShown"					}

		ChangeRename { from: "matrixplot";								to: "matrixPlot"					}

	}

	// Renaming Clustering Density-Based 
	Upgrade
	{

		functionName:		"mlClusteringDensityBased"
		fromVersion:		"0.16.3"
		toVersion:			"0.16.4"

		ChangeRename { from: "tableClusterInfoWSS";									to: "tableClusterInformationWithinSumOfSquares"}
		ChangeRename { from: "tableClusterInfoSilhouette";							to: "tableClusterInformationSilhouetteScore"}
		ChangeRename { from: "tableClusterInfoBetweenSumSquares";					to: "tableClusterInformationBetweenSumOfSquares"}
		ChangeRename { from: "tableClusterInfoTotalSumSquares";						to: "tableClusterInformationTotalSumOfSquares"}
		ChangeRename { from: "clusterEvaluationMetrics";							to: "tableClusterEvaluationMetrics"}
		ChangeRename { from: "k-distplot";											to: "kDistancePlot"}
		ChangeRename { from: "plotClusterMeans";									to: "clusterMeanPlot"}
		ChangeRename { from: "showBars";											to: "clusterMeanPlotBarPlot"}
		ChangeRename { from: "oneFigure";											to: "clusterMeanPlotSingleFigure"}
		ChangeRename { from: "plotClusterDensities";								to: "clusterDensityPlot"}
		ChangeRename { from: "oneFigureDensity";									to: "clusterDensityPlotSingleFigure"}
		ChangeRename { from: "plot2dCluster";										to: "tsneClusterPlot"}
		ChangeRename { from: "legend";												to: "tsneClusterPlotLegend"}
		ChangeRename { from: "labels";												to: "tsneClusterPlotLabels"}
		ChangeRename { from: "eps";													to: "epsilonNeighborhoodSize"}
		ChangeRename { from: "minPts";												to: "minCorePoints"}

		ChangeJS
		{
			name: "distance"
			jsFunction: function(options)
			{
				switch options["distance"]
				{
					case "Normal densities"											return "normalDensities";
					case "Correlated densities"										return "correlatedDensities";
				}
			}
		}

		ChangeRename { from: "scaleEqualSD";										to: "equalSdScale"}
		ChangeRename { from: "seedBox";												to: "randomSeed"}
		ChangeRename { from: "seed";												to: "randomSeedValue"}
		ChangeRename { from: "modelOpt";											to: "clusterDeterminationMethod"}

		ChangeJS
		{
			name: "clusterDeterminationMethod"
			jsFunction: function(options)
			{
				switch options["clusterDeterminationMethod"]
				{
					case "validationManual"											return "manual";
				}
			}
		}
	}

	//Renaming Clustering Fuzzy C-Means
	Upgrade
	{

		functionName:		"mlClusteringFuzzyCMeans"
		fromVersion:		"0.16.3"
		toVersion:			"0.16.4"

		ChangeRename { from: "tableClusterInfoWSS";									to: "tableClusterInformationWithinSumOfSquares"}
		ChangeRename { from: "tableClusterInfoSilhouette";							to: "tableClusterInformationSilhouetteScore"}
		ChangeRename { from: "tableClusterInfoCentroids";							to: "tableClusterInformationCentroids"}
		ChangeRename { from: "tableClusterInfoBetweenSumSquares";					to: "tableClusterInformationBetweenSumOfSquares"}
		ChangeRename { from: "tableClusterInfoTotalSumSquares";						to: "tableClusterInformationTotalSumOfSquares"}
		ChangeRename { from: "clusterEvaluationMetrics";							to: "tableClusterEvaluationMetrics"}
		ChangeRename { from: "withinssPlot";										to: "elbowMethodPlot"}
		ChangeRename { from: "plotClusterMeans";									to: "clusterMeanPlot"}
		ChangeRename { from: "showBars";											to: "clusterMeanPlotBarPlot"}
		ChangeRename { from: "oneFigure";											to: "clusterMeanPlotSingleFigure"}
		ChangeRename { from: "plotClusterDensities";								to: "clusterDensityPlot"}
		ChangeRename { from: "oneFigureDensity";									to: "clusterDensityPlotSingleFigure"}
		ChangeRename { from: "plot2dCluster";										to: "tsneClusterPlot"}
		ChangeRename { from: "legend";												to: "tsneClusterPlotLegend"}
		ChangeRename { from: "labels";												to: "tsneClusterPlotLabels"}
		ChangeRename { from: "noOfIterations";										to: "maxNumberIterations"}
		ChangeRename { from: "m";													to: "fuzzinessParameter"}
		ChangeRename { from: "scaleEqualSD";										to: "equalSdScale"}
		ChangeRename { from: "seedBox";												to: "randomSeed"}
		ChangeRename { from: "seed";												to: "randomSeedValue"}
		ChangeRename { from: "modelOpt";											to: "clusterDeterminationMethod"}

		ChangeJS
		{
			name: "clusterDeterminationMethod"
			jsFunction: function(options)
			{
				switch options["clusterDeterminationMethod"]
				{
					case "validationManual"											return "manual";
					case "validationOptimized"										return "optimized";
				}
			}
		}

		ChangeRename { from: "noOfClusters";										to: "clusterDeterminationMethodManualNumberOfClusters"}
		ChangeRename { from: "optimizationCriterion";								to: "clusterDeterminationMethodOptimizedTypeOptimization"}

		ChangeJS
		{
			name: "clusterDeterminationMethodOptimizedTypeOptimization"
			jsFunction: function(options)
			{
				switch options["clusterDeterminationMethodOptimizedTypeOptimization"]
				{
					case "validationAIC"											return "aic";
					case "validationBIC"											return "bic";
					case "validationSilh"											return "silhouette";
				}
			}
		}

		ChangeRename { from: "maxClusters";											to: "clusterDeterminationMethodOptimizedMaxNumberOfClusters"} 
	}

	//Renaming Clustering Hierarchical
	Upgrade
	{

		functionName:		"mlClusteringHierarchical"
		fromVersion:		"0.16.3"
		toVersion:			"0.16.4"

		ChangeRename { from: "tableClusterInfoWSS";									to: "tableClusterInformationWithinSumOfSquares"}
		ChangeRename { from: "tableClusterInfoSilhouette";							to: "tableClusterInformationSilhouetteScore"}
		ChangeRename { from: "tableClusterInfoBetweenSumSquares";					to: "tableClusterInformationBetweenSumOfSquares"}
		ChangeRename { from: "tableClusterInfoTotalSumSquares";						to: "tableClusterInformationTotalSumOfSquares"}
		ChangeRename { from: "clusterEvaluationMetrics";							to: "tableClusterEvaluationMetrics"}
		ChangeRename { from: "withinssPlot";										to: "elbowMethodPlot"}
		ChangeRename { from: "plotClusterMeans";									to: "clusterMeanPlot"}
		ChangeRename { from: "showBars";											to: "clusterMeanPlotBarPlot"}
		ChangeRename { from: "oneFigure";											to: "clusterMeanPlotSingleFigure"}
		ChangeRename { from: "plotClusterDensities";								to: "clusterDensityPlot"}
		ChangeRename { from: "oneFigureDensity";									to: "clusterDensityPlotSingleFigure"}
		ChangeRename { from: "plot2dCluster";										to: "tsneClusterPlot"}
		ChangeRename { from: "legend";												to: "tsneClusterPlotLegend"}
		ChangeRename { from: "labels";												to: "tsneClusterPlotLabels"}

		ChangeJS
		{
			name: "distance"
			jsFunction: function(options)
			{
				switch options["distance"]
				{
					case "Euclidean"											return "euclidean";
					case "Pearson correlation"									return "pearsonCorrelation";
				}
			}
		}

		ChangeJS
		{
			name: "linkage"
			jsFunction: function(options)
			{
				switch options["linkage"]
				{
					case "ward.D"											return "wardD";
					case "ward.D2"											return "wardD2";
				}
			}
		}

		ChangeRename { from: "scaleEqualSD";										to: "equalSdScale"}
		ChangeRename { from: "seedBox";												to: "randomSeed"}
		ChangeRename { from: "seed";												to: "randomSeedValue"}
		ChangeRename { from: "modelOpt";											to: "clusterDeterminationMethod"}

		ChangeJS
		{
			name: "clusterDeterminationMethod"
			jsFunction: function(options)
			{
				switch options["clusterDeterminationMethod"]
				{
					case "validationManual"											return "manual";
					case "validationOptimized"										return "optimized";
				}
			}
		}

		ChangeRename { from: "noOfClusters";										to: "clusterDeterminationMethodManualNumberOfClusters"}
		ChangeRename { from: "optimizationCriterion";								to: "clusterDeterminationMethodOptimizedTypeOptimization"}

		ChangeJS
		{
			name: "clusterDeterminationMethodOptimizedTypeOptimization"
			jsFunction: function(options)
			{
				switch options["clusterDeterminationMethodOptimizedTypeOptimization"]
				{
					case "validationAIC"											return "aic";
					case "validationBIC"											return "bic";
					case "validationSilh"											return "silhouette";
				}
			}
		}

		ChangeRename { from: "maxClusters";											to: "clusterDeterminationMethodOptimizedMaxNumberOfClusters"} 
	}

	//Renaming Clustering Neighborhood-Based
	Upgrade
	{

		functionName:		"mlClusteringKMeans"
		fromVersion:		"0.16.3"
		toVersion:			"0.16.4"

		ChangeRename { from: "tableClusterInfoWSS";									to: "tableClusterInformationWithinSumOfSquares"}
		ChangeRename { from: "tableClusterInfoSilhouette";							to: "tableClusterInformationSilhouetteScore"}
		ChangeRename { from: "tableClusterInfoCentroids";							to: "tableClusterInformationCentroids"}
		ChangeRename { from: "tableClusterInfoBetweenSumSquares";					to: "tableClusterInformationBetweenSumOfSquares"}
		ChangeRename { from: "tableClusterInfoTotalSumSquares";						to: "tableClusterInformationTotalSumOfSquares"}
		ChangeRename { from: "clusterEvaluationMetrics";							to: "tableClusterEvaluationMetrics"}
		ChangeRename { from: "withinssPlot";										to: "elbowMethodPlot"}
		ChangeRename { from: "plotClusterMeans";									to: "clusterMeanPlot"}
		ChangeRename { from: "showBars";											to: "clusterMeanPlotBarPlot"}
		ChangeRename { from: "oneFigure";											to: "clusterMeanPlotSingleFigure"}
		ChangeRename { from: "plotClusterDensities";								to: "clusterDensityPlot"}
		ChangeRename { from: "oneFigureDensity";									to: "clusterDensityPlotSingleFigure"}
		ChangeRename { from: "plot2dCluster";										to: "tsneClusterPlot"}
		ChangeRename { from: "legend";												to: "tsneClusterPlotLegend"}
		ChangeRename { from: "labels";												to: "tsneClusterPlotLabels"}
		ChangeRename { from: "noOfIterations";										to: "maxNumberIterations"}
		ChangeRename { from: "m";													to: "fuzzinessParameter"}
		ChangeRename { from: "scaleEqualSD";										to: "equalSdScale"}
		ChangeRename { from: "seedBox";												to: "randomSeed"}
		ChangeRename { from: "seed";												to: "randomSeedValue"}
		ChangeRename { from: "modelOpt";											to: "clusterDeterminationMethod"}

		ChangeJS
		{
			name: "clusterDeterminationMethod"
			jsFunction: function(options)
			{
				switch options["clusterDeterminationMethod"]
				{
					case "validationManual"											return "manual";
					case "validationOptimized"										return "optimized";
				}
			}
		}

		ChangeRename { from: "noOfClusters";										to: "clusterDeterminationMethodManualNumberOfClusters"}
		ChangeRename { from: "optimizationCriterion";								to: "clusterDeterminationMethodOptimizedTypeOptimization"}

		ChangeJS
		{
			name: "clusterDeterminationMethodOptimizedTypeOptimization"
			jsFunction: function(options)
			{
				switch options["clusterDeterminationMethodOptimizedTypeOptimization"]
				{
					case "validationAIC"											return "aic";
					case "validationBIC"											return "bic";
					case "validationSilh"											return "silhouette";
				}
			}
		}

		ChangeRename { from: "maxClusters";											to: "clusterDeterminationMethodOptimizedMaxNumberOfClusters"} 
	}

	//Renaming Clustering Random Forest
	Upgrade
	{
		functionName:		"mlClusteringRandomForest"
		fromVersion:		"0.16.3"
		toVersion:			"0.16.4"

		ChangeRename { from: "tableClusterInfoWSS";									to: "tableClusterInformationWithinSumOfSquares"}
		ChangeRename { from: "tableClusterInfoSilhouette";							to: "tableClusterInformationSilhouetteScore"}
		ChangeRename { from: "tableClusterInfoBetweenSumSquares";					to: "tableClusterInformationBetweenSumOfSquares"}
		ChangeRename { from: "tableClusterInfoTotalSumSquares";						to: "tableClusterInformationTotalSumOfSquares"}
		ChangeRename { from: "clusterEvaluationMetrics";							to: "tableClusterEvaluationMetrics"}
		ChangeRename { from: "importanceTable";										to: "featureImportanceTable"}
		ChangeRename { from: "withinssPlot";										to: "elbowMethodPlot"}
		ChangeRename { from: "plotClusterMeans";									to: "clusterMeanPlot"}
		ChangeRename { from: "showBars";											to: "clusterMeanPlotBarPlot"}
		ChangeRename { from: "oneFigure";											to: "clusterMeanPlotSingleFigure"}
		ChangeRename { from: "plotClusterDensities";								to: "clusterDensityPlot"}
		ChangeRename { from: "oneFigureDensity";									to: "clusterDensityPlotSingleFigure"}
		ChangeRename { from: "plot2dCluster";										to: "tsneClusterPlot"}
		ChangeRename { from: "legend";												to: "tsneClusterPlotLegend"}
		ChangeRename { from: "labels";												to: "tsneClusterPlotLabels"}
		ChangeRename { from: "noOfTrees";											to: "numberOfTrees"}

		ChangeRename { from: "scaleEqualSD";										to: "equalSdScale"}
		ChangeRename { from: "seedBox";												to: "randomSeed"}
		ChangeRename { from: "seed";												to: "randomSeedValue"}
		ChangeRename { from: "modelOpt";											to: "clusterDeterminationMethod"}

		ChangeJS
		{
			name: "clusterDeterminationMethod"
			jsFunction: function(options)
			{
				switch options["clusterDeterminationMethod"]
				{
					case "validationManual"											return "manual";
					case "validationOptimized"										return "optimized";
				}
			}
		}

		ChangeRename { from: "noOfClusters";										to: "clusterDeterminationMethodManualNumberOfClusters"}
		ChangeRename { from: "optimizationCriterion";								to: "clusterDeterminationMethodOptimizedTypeOptimization"}

		ChangeJS
		{
			name: "clusterDeterminationMethodOptimizedTypeOptimization"
			jsFunction: function(options)
			{
				switch options["clusterDeterminationMethodOptimizedTypeOptimization"]
				{
					case "validationAIC"											return "aic";
					case "validationBIC"											return "bic";
					case "validationSilh"											return "silhouette";
				}
			}
		}

		ChangeRename { from: "maxClusters";											to: "clusterDeterminationMethodOptimizedMaxNumberOfClusters"} 
	}

	//Renaming Clustering Random Forest
	Upgrade
	{

		functionName:		"mlPrediction"
		fromVersion:		"0.16.3"
		toVersion:			"0.16.4"

		ChangeRename { from: "loadPath";											to: "trainedModelFilePath"}
		ChangeRename { from: "scaleEqualSD";										to: "equalSdScale"}
		ChangeRename { from: "addPredictors";										to: "predictionsTableFeatures"}
		ChangeRename { from: "pfrom";												to: "predictionsTableFeaturesFromValue"}
		ChangeRename { from: "pto";													to: "predictionsTableFeaturesToValue"}
	}


}
