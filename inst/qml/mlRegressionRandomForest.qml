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

	UI.VariablesFormRegression { }

	Group
	{
		title:									qsTr("Tables")

		TAB.ModelPerformance { }
		TAB.FeatureImportance { }
		TAB.ExplainPredictions { }
	}

	Group
	{
		title:									qsTr("Plots")

		FIG.DataSplit { }
		FIG.PredictivePerformance { }

		CheckBox
		{
			name:								"treesVsModelErrorPlot"
			text:								qsTr("Out-of-bag error")
		}

		CheckBox
		{
			name:								"accuracyDecreasePlot"
			text:								qsTr("Mean decrease in accuracy")
		}

		CheckBox
		{
			name:								"purityIncreasePlot"
			text:								qsTr("Total increase in node purity")
		}
	}

	UI.ExportResults
	{
		enabled:								predictors.count > 1 && target.count > 0
	}

	UI.DataSplit
	{
		leaveOneOutVisible:						false
		kFoldsVisible:							false
		trainingValidationSplit:				optimizeModel.checked
	}

	Section
	{
		title:									qsTr("Training Parameters")

		Group
		{
			title:								qsTr("Algorithmic Settings")

			PercentField
			{
				name:							"baggingFraction"
				text:							qsTr("Training data used per tree")
				defaultValue:					50
				min:							5
				max:							95
			}

			RowLayout
			{
				DropDown
				{
					id:							noOfPredictors
					name:						"noOfPredictors"
					indexDefaultValue:			0
					label:						qsTr("Features per split")
					values:
						[
						{ label: qsTr("Auto"), 	value: "auto"},
						{ label: qsTr("Manual"),value: "manual"}
					]
				}

				IntegerField
				{
					name:						"numberOfPredictors"
					defaultValue:				1
					min:						1
					max:						5000
					visible:					noOfPredictors.currentIndex == 1
				}
			}

			UI.ScaleVariables { }
			UI.SetSeed { }
		}

		RadioButtonGroup
		{
			title:									qsTr("Number of Trees")
			name:									"modelOptimization"

			RadioButton
			{
				text:								qsTr("Fixed")
				name:								"manual"

				IntegerField
				{
					name:							"noOfTrees"
					text:							qsTr("Trees")
					defaultValue:					100
					min:							1
					max:							500000
					fieldWidth:						60
				}
			}

			RadioButton
			{
				id:									optimizeModel
				text:								qsTr("Optimized")
				name:								"optimized"
				checked:							true

				IntegerField
				{
					name:							"maxTrees"
					text:							qsTr("Max. trees")
					defaultValue:					100
					min:							1
					max:							500000
					fieldWidth:						60
				}
			}
		}
	}
}
