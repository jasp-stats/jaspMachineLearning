
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

	Upgrade
	{

		functionName:		"mlPrediction"
		fromVersion:		"0.16.4"
		toVersion:			"0.17.0"

		ChangeRename { from: "scaleEqualSD";							to: "scaleVariables"				}
		ChangeRename { from: "seedBox";									to: "setSeed"						}

		ChangeRename { from: "pfrom";									to: "fromIndex"						}
		ChangeRename { from: "pto";										to: "toIndex"						}

	}


}