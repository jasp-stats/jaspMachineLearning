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

import QtQuick			2.8
import QtQuick.Layouts	1.3
import JASP.Controls	1.0
import JASP.Widgets		1.0

import "./common/ui" as UI
import "./common/tables" as TAB
import "./common/figures" as FIG

Form
{

	UI.VariablesFormClassification { }

	Group
	{
		title:									qsTr("Tables")

		TAB.ConfusionMatrix { }
		TAB.ClassProportions { }
		TAB.ModelPerformance { }
		TAB.ExplainPredictions { }

		CheckBox
		{
			text:								qsTr("Support vectors")
			name:								"supportVectorsTable"
		}
	}

	Group
	{
		title:									qsTr("Plots")

		FIG.DataSplit { }
		FIG.RocCurve { }
		FIG.AndrewsCurve { }
		FIG.DecisionBoundary { }
	}

	UI.ExportResults
	{
		enabled:								predictors.count > 0 && target.count > 0
	}

	UI.DataSplit
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
				id:								weights
				name:							"weights"
				indexDefaultValue:				0
				label:							qsTr("Weights")
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
				name:							"degree"
				text:							qsTr("Degree")
				defaultValue:					3
				min:							1
				enabled:						weights.value == "polynomial"
				Layout.leftMargin:				10 * preferencesModel.uiScale
			}

			DoubleField
			{
				name:							"gamma"
				text:							qsTr("Gamma parameter")
				defaultValue:					1
				min:							0
				enabled:						weights.value != "linear"
				Layout.leftMargin:				10 * preferencesModel.uiScale
			}

			DoubleField
			{
				name:							"complexityParameter"
				text:							qsTr("r parameter")
				defaultValue:					0
				min:							0
				enabled:						weights.value == "polynomial" | weights.value == "sigmoid"
				Layout.leftMargin:				10 * preferencesModel.uiScale
			}

			DoubleField
			{
				name:							"cost"
				text:							qsTr("Cost of constraints violation")
				defaultValue:					1
				min:							0.001
			}

			DoubleField
			{
				name:							"tolerance"
				text:							qsTr("Tolerance of termination criterion")
				defaultValue:					0.001
				min:							0.001
			}

			DoubleField
			{
				name:							"epsilon"
				text:							qsTr("Epsilon")
				defaultValue:					0.01
				min:							0.001
			}

			UI.ScaleVariables { }
			UI.SetSeed { }
		}

		RadioButtonGroup
		{
			name:								"modelOptimization"
			visible:							false

			RadioButton
			{
				name:							"manual"
				checked:						true
			}
		}
	}
}
