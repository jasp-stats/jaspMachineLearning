//
// Copyright (C) 2013-2021 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//

import QtQuick									2.8
import QtQuick.Layouts							1.3
import JASP.Controls							1.0
import JASP.Widgets								1.0

import "./common" as ML

Form 
{

	VariablesForm
	{
		AvailableVariablesList
		{
			name:								"variables"
		}
		
		AssignedVariablesList
		{
			id:									predictors
			name:								"predictors"
			title:								qsTr("Features")
			allowedColumns:						["scale"]
			allowAnalysisOwnComputedColumns:	false
		}
	}

	Group
	{
		title:									qsTr("Tables")

		CheckBox
		{
			text:								qsTr("Cluster means")
			name:								"tableClusterMeans"
		}

		CheckBox
		{
			id:									clusterInfo
			text:								qsTr("Cluster information")
			name:								"tableClusterInformation"
			checked:							true

			CheckBox
			{
				text:							qsTr("Within sum of squares")
				name:							"tableClusterInformationWithinSumOfSquares"
				checked:						true
			}

			CheckBox
			{
				text:							qsTr("Silhouette score")
				name:							"tableClusterInformationSilhouetteScore"
			}

			CheckBox
			{
				text:							qsTr("Centroids")
				name:							"tableClusterInformationCentroids"
			}

			CheckBox
			{
				text:							qsTr("Between sum of squares")
				name:							"tableClusterInformationBetweenSumOfSquares"
			}

			CheckBox
			{
				text:							qsTr("Total sum of squares")
				name:							"tableClusterInformationTotalSumOfSquares"
			}
		}

		CheckBox
		{
			text:								qsTr("Evaluation metrics")
			name:								"tableClusterEvaluationMetrics"
		}
	}

	Group
	{
		title:									qsTr("Plots")

		CheckBox
		{
			text:								qsTr("Elbow method")
			name:								"elbowMethodPlot"
			enabled:							!validationManual.checked
		}

		CheckBox
		{
			name:								"matrixPlot"
			text:								qsTr("Cluster matrix plot")
		}

		CheckBox
		{
			text:								qsTr("Cluster means")
			name:								"clusterMeanPlot"

			CheckBox
			{
				text:							qsTr("Display barplot")
				name:							"clusterMeanPlotBarPlot"
				checked:						true
			}

			CheckBox
			{
				text:							qsTr("Group into one figure")
				name:							"clusterMeanPlotSingleFigure"
				checked:						true
			}
		}

		CheckBox
		{
			text:								qsTr("Cluster densities")
			name:								"clusterDensityPlot"

			CheckBox
			{
				text:							qsTr("Group into one figure")
				name:							"clusterDensityPlotSingleFigure"
				checked:						true
			}
		}

		CheckBox
		{
			text:								qsTr("t-SNE cluster plot")
			name:								"tsneClusterPlot"

			Row
			{

				CheckBox
				{
					text:						qsTr("Legend")
					name:						"tsneClusterPlotLegend"
					checked:					true
				}

				CheckBox
				{
					text:						qsTr("Labels")
					name:						"tsneClusterPlotLabels"
				}
			}
		}
	}

	ML.ExportResults
	{
		enabled:								predictors.count > 1
		showSave:								false
	}

	Section
	{
		title:									qsTr("Training Parameters")

		Group
		{
			title:								qsTr("Algorithmic Settings")

			IntegerField
			{
				name:							"maxNumberIterations"
				text:							qsTr("Max. iterations")
				defaultValue:					25
				min:							1
				max:							999999
			}

			DoubleField
			{
				name:							"fuzzinessParameter"
				text:							qsTr("Fuzziness parameter")
				defaultValue:					2
				min:							1
				max:							1000
				decimals:						2
			}

			CheckBox
			{
				text:							qsTr("Scale variables")
				name:							"scaleVariables"
				checked:						true
			}

			CheckBox
			{
				name:							"setSeed"
				text:							qsTr("Set seed")
				childrenOnSameRow:				true

				IntegerField
				{
					name:						"seed"
					defaultValue:				1
					min:						-999999
					max:						999999
					fieldWidth:					60
				}
			}
		}

		RadioButtonGroup
		{
			title:								qsTr("Cluster Determination")
			name:								"modelOptimization"

			RadioButton
			{
				id:								validationManual
				text:							qsTr("Fixed")
				name:							"manual"

				IntegerField
				{
					name:						"manualNumberOfClusters"
					text:						qsTr("Clusters")
					defaultValue:				3
					min:						2
					max:						5000
					enabled:					validationManual.checked
					fieldWidth:					60
				}
			}

			RadioButton
			{
				text:							qsTr("Optimized according to")
				name:							"optimized"
				childrenOnSameRow:				true
				checked:						true

				DropDown
				{
					name:						"modelOptimizationMethod"
					indexDefaultValue:			1

					values:
						[
						{ label: "AIC",			value: "aic"},
						{ label: "BIC",			value: "bic"},
						{ label: "Silhouette", 	value: "silhouette"}
					]
				}
			}

			IntegerField
			{
				name:							"maxNumberOfClusters"
				text:							qsTr("Max. clusters")
				defaultValue:					10
				min:							2
				max:							5000
				enabled:						!validationManual.checked
				Layout.leftMargin:				20
				fieldWidth:						60
			}
		}
	}
}
