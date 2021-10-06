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
			allowedColumns:						["ordinal", "nominal", "nominalText"]
		}

		AssignedVariablesList
		{
			id:									predictors
			name:								"predictors"
			title:								qsTr("Predictors")
			allowedColumns:						["scale", "ordinal", "nominal", "nominalText"]
			allowAnalysisOwnComputedColumns:	false
		}
	}

	Group
	{
		title:									qsTr("Tables")

		CheckBox
		{
			text:								qsTr("Confusion matrix")
			name:								"confusionTable"
			checked:							true

			CheckBox
			{
				text:							qsTr("Display proportions")
				name:							"confusionProportions"
			}
		}

		CheckBox
		{
			text:								qsTr("Class proportions")
			name:								"classProportionsTable"
		}

		CheckBox
		{
			text:								qsTr("Evaluation metrics")
			name:								"validationMeasures"
		}

		CheckBox
		{
			text:								qsTr("Support vectors")
			name:								"tableSupportVectors"
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
			name:								"rocCurve"
			text:								qsTr("ROC curves")
		}

		CheckBox
		{
			name:								"andrewsCurve"
			text:								qsTr("Andrews curves")
		}

		CheckBox
		{
			name:								"decisionBoundary"
			text:								qsTr("Decision boundary matrix")

			Row
			{

				CheckBox
				{
					name:						"plotLegend"
					text:						qsTr("Legend")
					checked:					true
				}

				CheckBox
				{
					name:						"plotPoints"
					text:						qsTr("Points")
					checked:					true
				}
			}
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

			DropDown
			{
				name:							"weights"
				indexDefaultValue:				0
				label:							qsTr("Kernel")
				values:
					[
					{ label: qsTr("Linear"),	value: "linear"},
					{ label: qsTr("Radial"),	value: "radial"},
					{ label: qsTr("Polynomial"),value: "polynomial"},
					{ label: qsTr("Sigmoid"),	value: "sigmoid"}
				]
			}

			DoubleField
			{
				name:							"cost"
				text:							qsTr("Cost of contraints violation")
				defaultValue:					1
				min:							0
			}

			DoubleField
			{
				name:							"tolerance"
				text:							qsTr("Tolerance of termination criterion")
				defaultValue:					0.001
				min:							0
			}

			DoubleField
			{
				name:							"epsilon"
				text:							qsTr("Epsilon")
				defaultValue:					0.01
				min:							0
			}

			CheckBox
			{
				text:							qsTr("Scale predictors")
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
