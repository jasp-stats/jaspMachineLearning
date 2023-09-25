
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

		ChangeJS
		{
			name: "modelOptimization"
			jsFunction: function(options)
			{
				switch (options["modelOptimization"])
				{
					case "optimizationManual":		return "manual";
					case "optimizationOOB":			return "optimized";
					default:						return options["modelOptimization"];
				}
			}
		}

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

		ChangeJS
		{
			name: "modelOptimization"
			jsFunction: function(options)
			{
				switch (options["modelOptimization"])
				{
					case "optimizationManual":		return "manual";
					case "optimizationOOB":			return "optimized";
					default:						return options["modelOptimization"];
				}
			}
		}

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

		ChangeJS
		{
			name: "modelOptimization"
			jsFunction: function(options)
			{
				switch (options["modelOptimization"])
				{
					case "optimizationManual":		return "manual";
					default:						return options["modelOptimization"];
				}
			}
		}

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

		ChangeJS
		{
			name: "modelOptimization"
			jsFunction: function(options)
			{
				switch (options["modelOptimization"])
				{
					case "optimizationManual":		return "manual";
					default:						return options["modelOptimization"];
				}
			}
		}

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

		ChangeJS
		{
			name: "modelOptimization"
			jsFunction: function(options)
			{
				switch (options["modelOptimization"])
				{
					case "optimizationManual":		return "manual";
					case "optimizationError":		return "optimized";
					default:						return options["modelOptimization"];
				}
			}
		}

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

		ChangeJS
		{
			name: "modelOptimization"
			jsFunction: function(options)
			{
				switch (options["modelOptimization"])
				{
					case "optimizationManual":		return "manual";
					case "optimizationError":		return "optimized";
					default:						return options["modelOptimization"];
				}
			}
		}

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

		ChangeJS
		{
			name: "modelOptimization"
			jsFunction: function(options)
			{
				switch (options["modelOptimization"])
				{
					case "optimizationManual":		return "manual";
					case "optimizationError":		return "optimized";
					default:						return options["modelOptimization"];
				}
			}
		}

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

		ChangeJS
		{
			name: "modelOptimization"
			jsFunction: function(options)
			{
				switch (options["modelOptimization"])
				{
					case "optimizationManual":		return "manual";
					case "optimizationError":		return "optimized";
					default:						return options["modelOptimization"];
				}
			}
		}

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

		ChangeJS
		{
			name: "modelOptimization"
			jsFunction: function(options)
			{
				switch (options["modelOptimization"])
				{
					case "optimizationManual":		return "manual";
					case "optimizationError":		return "optimized";
					default:						return options["modelOptimization"];
				}
			}
		}

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

		ChangeJS
		{
			name: "modelOptimization"
			jsFunction: function(options)
			{
				switch (options["modelOptimization"])
				{
					case "optimizationManual":		return "manual";
					case "optimizationError":		return "optimized";
					default:						return options["modelOptimization"];
				}
			}
		}

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

		ChangeJS
		{
			name: "modelOptimization"
			jsFunction: function(options)
			{
				switch (options["modelOptimization"])
				{
					case "optimizationManual":		return "manual";
					default:						return options["modelOptimization"];
				}
			}
		}

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

		ChangeJS
		{
			name: "modelOptimization"
			jsFunction: function(options)
			{
				switch (options["modelOptimization"])
				{
					case "optimizationManual":		return "manual";
					default:						return options["modelOptimization"];
				}
			}
		}

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

		ChangeJS
		{
			name: "modelOptimization"
			jsFunction: function(options)
			{
				switch (options["modelOptimization"])
				{
					case "optimizationManual":		return "manual";
					default:						return options["modelOptimization"];
				}
			}
		}

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

		ChangeJS
		{
			name: "modelOptimization"
			jsFunction: function(options)
			{
				switch (options["modelOptimization"])
				{
					case "optimizationManual":		return "manual";
					default:						return options["modelOptimization"];
				}
			}
		}

		ChangeRename { from: "plotLegend";								to: "legendShown"					}
		ChangeRename { from: "plotPoints";								to: "pointsShown"					}

		ChangeRename { from: "matrixplot";								to: "matrixPlot"					}

	}

	// Renaming Clustering Density-Based 
	Upgrade
	{

		functionName:		"mlClusteringDensityBased"
		fromVersion:		"0.16.4"
		toVersion:			"0.17.0"

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
				switch (options["distance"])
				{
					case "Normal densities":		return "normalDensities";
					case "Correlated densities":	return "correlatedDensities";
					default:						return options["distance"];									
				}
			}
		}

		ChangeRename { from: "scaleEqualSD";										to: "scaleVariables"}
		ChangeRename { from: "seedBox";												to: "setSeed"}
		ChangeRename { from: "modelOpt";											to: "modelOptimization"}

		ChangeJS
		{
			name: "modelOptimization"
			jsFunction: function(options)
			{
				switch (options["modelOptimization"])
				{
					case "validationManual":			return "manual";
					default:							return options["modelOptimization"];
				}
			}
		}
	}

	//Renaming Clustering Fuzzy C-Means
	Upgrade
	{

		functionName:		"mlClusteringFuzzyCMeans"
		fromVersion:		"0.16.4"
		toVersion:			"0.17.0"

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
		ChangeRename { from: "scaleEqualSD";										to: "scaleVariables"}
		ChangeRename { from: "seedBox";												to: "setSeed"}
		ChangeRename { from: "modelOpt";											to: "modelOptimization"}

		ChangeJS
		{
			name: "modelOptimization"
			jsFunction: function(options)
			{
				switch (options["modelOptimization"])
				{
					case "validationManual":		return "manual";
					case "validationOptimized":		return "optimized";
					default:						return options["modelOptimization"]
				}
			}
		}

		ChangeRename { from: "noOfClusters";										to: "manualNumberOfClusters"}
		ChangeRename { from: "optimizationCriterion";								to: "modelOptimizationMethod"}

		ChangeJS
		{
			name: "modelOptimizationMethod"
			jsFunction: function(options)
			{
				switch (options["modelOptimizationMethod"])
				{
					case "validationAIC":			return "aic";
					case "validationBIC":			return "bic";
					case "validationSilh":			return "silhouette";
				}
			}
		}

		ChangeRename { from: "maxClusters";											to: "maxNumberOfClusters"} 
	}

	//Renaming Clustering Hierarchical
	Upgrade
	{

		functionName:		"mlClusteringHierarchical"
		fromVersion:		"0.16.4"
		toVersion:			"0.17.0"

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
				switch (options["distance"])
				{
					case "Euclidean":				return "euclidean";
					case "Pearson correlation":		return "pearsonCorrelation";
				}
			}
		}

		ChangeJS
		{
			name: "linkage"
			jsFunction: function(options)
			{
				switch (options["linkage"])
				{
					case "ward.D":				return "wardD";
					case "ward.D2":				return "wardD2";
					default:					return options["linkage"];
				}
			}
		}

		ChangeRename { from: "scaleEqualSD";										to: "scaleVariables"}
		ChangeRename { from: "seedBox";												to: "setSeed"}
		ChangeRename { from: "modelOpt";											to: "modelOptimization"}

		ChangeJS
		{
			name: "modelOptimization"
			jsFunction: function(options)
			{
				switch (options["modelOptimization"])
				{
					case "validationManual":					return "manual";
					case "validationOptimized":					return "optimized";
				}
			}
		}

		ChangeRename { from: "noOfClusters";										to: "manualNumberOfClusters"}
		ChangeRename { from: "optimizationCriterion";								to: "modelOptimizationMethod"}

		ChangeJS
		{
			name: "modelOptimizationMethod"
			jsFunction: function(options)
			{
				switch (options["modelOptimizationMethod"])
				{
					case "validationAIC":		return "aic";
					case "validationBIC":		return "bic";
					case "validationSilh":		return "silhouette";
				}
			}
		}

		ChangeRename { from: "maxClusters";											to: "maxNumberOfClusters"} 
	}

	//Renaming Clustering Neighborhood-Based
	Upgrade
	{

		functionName:		"mlClusteringKMeans"
		fromVersion:		"0.16.4"
		toVersion:			"0.17.0"

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
		ChangeRename { from: "scaleEqualSD";										to: "scaleVariables"}
		ChangeRename { from: "seedBox";												to: "setSeed"}
		ChangeRename { from: "modelOpt";											to: "modelOptimization"}

		ChangeJS
		{
			name: "modelOptimization"
			jsFunction: function(options)
			{
				switch (options["modelOptimization"])
				{
					case "validationManual":		return "manual";
					case "validationOptimized":		return "optimized";
				}
			}
		}

		ChangeRename { from: "noOfClusters";								to: "manualNumberOfClusters"}
		ChangeRename { from: "optimizationCriterion";								to: "modelOptimizationMethod"}

		ChangeJS
		{
			name: "modelOptimizationMethod"
			jsFunction: function(options)
			{
				switch (options["modelOptimizationMethod"])
				{
					case "validationAIC":											return "aic";
					case "validationBIC":											return "bic";
					case "validationSilh":											return "silhouette";
				}
			}
		}

		ChangeRename { from: "maxClusters";											to: "maxNumberOfClusters"} 
	}

	//Renaming Clustering Random Forest
	Upgrade
	{
		functionName:		"mlClusteringRandomForest"
		fromVersion:		"0.16.4"
		toVersion:			"0.17.0"

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

		ChangeRename { from: "scaleEqualSD";										to: "scaleVariables"}
		ChangeRename { from: "seedBox";												to: "setSeed"}
		ChangeRename { from: "modelOpt";											to: "modelOptimization"}

		ChangeJS
		{
			name: "modelOptimization"
			jsFunction: function(options)
			{
				switch (options["modelOptimization"])
				{
					case "validationManual":		return "manual";
					case "validationOptimized":		return "optimized";
				}
			}
		}

		ChangeRename { from: "noOfClusters";								to: "manualNumberOfClusters"}
		ChangeRename { from: "optimizationCriterion";						to: "modelOptimizationMethod"}

		ChangeJS
		{
			name: "modelOptimizationMethod"
			jsFunction: function(options)
			{
				switch (options["modelOptimizationMethod"])
				{
					case "validationAIC":		return "aic";
					case "validationBIC":		return "bic";
					case "validationSilh":		return "silhouette";
				}
			}
		}

		ChangeRename { from: "maxClusters";											to: "maxNumberOfClusters"} 
	}

	//Renaming Clustering Random Forest
	Upgrade
	{

		functionName:		"mlPrediction"
		fromVersion:		"0.16.4"
		toVersion:			"0.17.0"

		ChangeRename { from: "loadPath";											to: "trainedModelFilePath"}
		ChangeRename { from: "scaleEqualSD";										to: "scaleVariables"}
		ChangeRename { from: "addPredictors";										to: "predictionsTableFeatures"}
		ChangeRename { from: "pfrom";												to: "fromIndex"}
		ChangeRename { from: "pto";													to: "toIndex"}
	}

	// 11-06-2023
	//Renaming Boosting Classification
	Upgrade
	{

		functionName:		"mlClassificationBoosting"
		fromVersion:		"0.17.2.1"
		toVersion:			"0.17.3"

		ChangeRename { from: "relativeInfluenceTable";									to: "featureImportanceTable"}
	}

	//Renaming Boosting Regression
	Upgrade
	{

		functionName:		"mlRegressionBoosting"
		fromVersion:		"0.17.2.1"
		toVersion:			"0.17.3"

		ChangeRename { from: "relativeInfluenceTable";									to: "featureImportanceTable"}
	}

	//Renaming Decision Tree Classification
	Upgrade
	{

		functionName:		"mlClassificationDecisionTree"
		fromVersion:		"0.17.2.1"
		toVersion:			"0.17.3"

		ChangeRename { from: "variableImportanceTable";									to: "featureImportanceTable"}
	}

	//Renaming Decision Tree Regression
	Upgrade
	{

		functionName:		"mlRegressionDecisionTree"
		fromVersion:		"0.17.2.1"
		toVersion:			"0.17.3"

		ChangeRename { from: "variableImportanceTable";									to: "featureImportanceTable"}
	}

	//Renaming Random Forest Classification
	Upgrade
	{

		functionName:		"mlClassificationRandomForest"
		fromVersion:		"0.17.2.1"
		toVersion:			"0.17.3"

		ChangeRename { from: "variableImportanceTable";									to: "featureImportanceTable"}
	}

	//Renaming Random Forest Regression
	Upgrade
	{

		functionName:		"mlRegressionRandomForest"
		fromVersion:		"0.17.2.1"
		toVersion:			"0.17.3"

		ChangeRename { from: "variableImportanceTable";									to: "featureImportanceTable"}
	}

	//Renaming Density Based Clustering
	Upgrade
	{

		functionName:		"mlClusteringDensityBased"
		fromVersion:		"0.17.2.1"
		toVersion:			"0.17.3"

		ChangeRename { from: "tableClusterEvaluationMetrics";							to: "validationMeasures"}
	}

	//Renaming Fuzzy C Means Clustering
	Upgrade
	{

		functionName:		"mlClusteringFuzzyCMeans"
		fromVersion:		"0.17.2.1"
		toVersion:			"0.17.3"

		ChangeRename { from: "tableClusterEvaluationMetrics";							to: "validationMeasures"}
	}

	//Renaming Hierarchical Clustering
	Upgrade
	{

		functionName:		"mlClusteringHierarchical"
		fromVersion:		"0.17.2.1"
		toVersion:			"0.17.3"

		ChangeRename { from: "tableClusterEvaluationMetrics";							to: "validationMeasures"}
	}

	//Renaming K Means Clustering
	Upgrade
	{

		functionName:		"mlClusteringKMeans"
		fromVersion:		"0.17.2.1"
		toVersion:			"0.17.3"

		ChangeRename { from: "tableClusterEvaluationMetrics";							to: "validationMeasures"}
	}

	//Renaming Random Forest Clustering
	Upgrade
	{

		functionName:		"mlClusteringRandomForest"
		fromVersion:		"0.17.2.1"
		toVersion:			"0.17.3"

		ChangeRename { from: "tableClusterEvaluationMetrics";							to: "validationMeasures"}
	}
}
