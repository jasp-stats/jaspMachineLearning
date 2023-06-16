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
			text:							qsTr("Detected anomalies")
			name:							"tableAnomalyScores"

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
				text:						qsTr("Label anomalies")
				name:						"matrixPlotLabels"
				enabled:					matrixPlotPoints.checked
			}
		}
	}

	ML.ExportResults
	{
		enabled:								predictors.count > 0
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
				text:							qsTr("Max. depth")
				name:							"maxDepth"
				min:							1
				defaultValue:					4
				max:							10000
			}

			CheckBox
			{
				text:							qsTr("Scale features")
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
	}
}
