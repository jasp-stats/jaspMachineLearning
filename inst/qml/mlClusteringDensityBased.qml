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
				name:							"tableClusterInfoWSS"
				checked:						true
			}

			CheckBox
			{
				text:							qsTr("Silhouette score")
				name:							"tableClusterInfoSilhouette"
			}

			CheckBox
			{
				text:							qsTr("Between sum of squares")
				name:							"tableClusterInfoBetweenSumSquares"
			}

			CheckBox
			{
				text:							qsTr("Total sum of squares")
				name:							"tableClusterInfoTotalSumSquares"
			}
		}

		CheckBox
		{
			text:								qsTr("Evaluation metrics")
			name:								"clusterEvaluationMetrics"
		}

	}

	Group
	{
		title:									qsTr("Plots")

		CheckBox
		{
			text:								qsTr("K-distance plot")
			name:								"k-distplot"
		}

		CheckBox
		{
			text:								qsTr("Cluster means")
			name:								"plotClusterMeans"

			CheckBox
			{
				text:							qsTr("Display barplot")
				name:							"showBars"
				checked:						true
			}

			CheckBox
			{
				text:							qsTr("Group into one figure")
				name:							"oneFigure"
				checked:						true
			}
		}

		CheckBox
		{
			text:								qsTr("Cluster densities")
			name:								"plotClusterDensities"

			CheckBox
			{
				text:							qsTr("Group into one figure")
				name:							"oneFigureDensity"
				checked:						true
			}
		}

		CheckBox
		{
			text:								qsTr("t-SNE cluster plot")
			name:								"plot2dCluster"

			Row
			{
				CheckBox
				{
					text:						qsTr("Legend")
					name:						"legend"
					checked:					true
				}

				CheckBox
				{
					text:						qsTr("Labels")
					name:						"labels"
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

			DoubleField
			{
				name:							"eps"
				text:							qsTr("Epsilon neighborhood size")
				decimals:						2
				defaultValue:					2
				min:							0.01
				max:							999999
				enabled:						validationManual.checked
			}

			IntegerField
			{
				name:							"minPts"
				text:							qsTr("Min. core points")
				defaultValue:					5
				min:							2
				max:							50000
			}

			DropDown
			{
				name:							"distance"
				indexDefaultValue:				0
				label:							qsTr("Distance")
				values:
					[
					{ label: qsTr("Normal"), 	value: "Normal densities"},
					{ label: qsTr("Correlated"),value: "Correlated densities"}
				]
			}

			CheckBox
			{
				text:							qsTr("Scale variables")
				name:							"scaleEqualSD"
				checked:						true
			}

			CheckBox
			{
				name:							"seedBox"
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
			title:								qsTr("Model Optimization")
			name:								"modelOpt"
			visible:							false

			RadioButton
			{
				id:								validationManual
				text:							qsTr("Fixed")
				name:							"validationManual"
			}
		}
	}
}
