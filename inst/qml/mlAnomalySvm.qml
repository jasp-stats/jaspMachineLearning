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
			allowedColumns:						["scale", "ordinal", "nominal", "nominalText"]
			allowAnalysisOwnComputedColumns:	false
		}
	}

	Group
	{
		title:								qsTr("Tables")

		CheckBox
		{
			text:							qsTr("Anomalies")
			name:							"tableAnomalyScores"

			CheckBox
			{
				text:						qsTr("Show standard data")
				name:						"tableAnomalyScoresObs"
			}

			CheckBox
			{
				text:						qsTr("Add features")
				name:						"tableAnomalyScoresFeatures"
			}
		}
	}

	Group
	{
		title:									qsTr("Plots")

		CheckBox
		{
			text:								qsTr("Anomaly matrix plot")
			name:								"matrixPlot"

			CheckBox
			{
				id:							matrixPlotPoints
				text:						qsTr("Add data points")
				name:						"matrixPlotPoints"
				checked:					true
			}

			CheckBox
			{
				text:						qsTr("Label anomalies")
				name:						"matrixPlotLabels"
				enabled:					matrixPlotPoints.checked
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

			DropDown
			{
				name:							"scoringMetric"
				indexDefaultValue:				0
				label:							qsTr("Scoring metric")
				values:
					[
					{ label: "Depth", 			value: "depth"},
					{ label: "Density", 		value: "density"},
					{ label: "Adjusted depth", 	value: "adj_depth"},
					{ label: "Adjusted density", value: "adj_density"},
					{ label: "Boxed ratio", 	value: "boxed_ratio"},
					{ label: "Boxed density", value: "boxed_density"},
					{ label: "Boxed density 2", value: "boxed_density2"}
				]
			}

			DoubleField 
			{
				Layout.leftMargin:					15 * preferencesModel.uiScale
				text:								qsTr("Threshold")
				name:								"cutoff"
				min:								0
				decimals:							2
				defaultValue:						0.5
			}

			IntegerField
			{
				text:							qsTr("Number of trees")
				name:							"nTrees"
				min:							1
				defaultValue:					500
				max:							10000
			}

			PercentField
			{
				text:							qsTr("Data used per tree")
				name:							"sampleSize"
				defaultValue:					100
			}

			IntegerField
			{
				text:							qsTr("Features per split")
				name:							"numberOfPredictors"
				defaultValue:					1
				min:							1
				max:							5000
			}

			CheckBox
			{
				text:							qsTr("Scale features")
				name:							"scaleVariables"
				checked:						true
			}
		}
	}
}
