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
			id:									target
			name:								"target"
			title:								qsTr("Target")
			singleVariable:						true
			allowedColumns:						["scale"]
		}

		AssignedVariablesList
		{
			id:									predictors
			name:								"predictors"
			title:								qsTr("Predictors")
			allowedColumns:						["scale", "nominal", "nominalText", "ordinal"]
			allowAnalysisOwnComputedColumns:	false
		}
	}

	Group
	{
		title:									qsTr("Tables")

		CheckBox
		{
			text:								qsTr("Evaluation metrics")
			name:								"validationMeasures"
		}

		CheckBox
		{
			text:								qsTr("Variable importance")
			name:								"tableVariableImportance"
		}

		CheckBox
		{
			text:								qsTr("Attempted splits")
			name:								"tableSplits"

			CheckBox
			{
				text:							qsTr("Only show splits in tree")
				name:							"tableSplitsTree"
				checked:						true
			}
		}
	}

	Group
	{
		title:									qsTr("Plots")

		CheckBox
		{
			text:								qsTr("Data split")
			name:								"dataSplitPlot"
			checked:							true
		}

		CheckBox
		{
			text:								qsTr("Predictive performance")
			name:								"predictedPerformancePlot"
		}

		CheckBox
		{
			text:								qsTr("Decision tree")
			name:								"plotDecisionTree"
		}
	}

	ML.ExportResults
	{
		enabled:								predictors.count > 0 && target.count > 0
	}

	ML.DataSplit
	{
		trainingValidationSplit:				false
	}

	Section
	{
		title:									qsTr("Training Parameters")

		Group
		{
			title:								qsTr("Algorithmic Settings")

			IntegerField
			{
				text:							qsTr("Min. observations for split")
				name:							"nSplit"
				min:							1
				defaultValue:					20
			}

			IntegerField
			{
				text:							qsTr("Min. observations in terminal")
				name:							"nNode"
				min:							1
				defaultValue:					7
			}

			IntegerField
			{
				text:							qsTr("Max. interaction depth")
				name:							"intDepth"
				min:							1
				defaultValue:					30
				max:							30
			}

			DoubleField
			{
				text:							qsTr("Complexity parameter")
				name:							"cp"
				min:							0
				defaultValue:					0.01
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
			name:								"modelOpt"
			visible:							false

			RadioButton
			{
				name:							"optimizationManual"
				checked:						true
			}
		}
	}
}
