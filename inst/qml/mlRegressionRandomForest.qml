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
			title:								qsTr("Features")
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
			text:								qsTr("Explain predictions (SHAP)")
			name:								"tableShap"
			id:									tableShap

			Row
			{	
				spacing:							5 * preferencesModel.uiScale
				enabled:							tableShap.checked
			
				IntegerField
				{
					name:							"shapFrom"
					text:							qsTr("From")
					defaultValue:					1
					min:							1
					max:							dataSetModel.rowCount()
				}

				IntegerField
				{
					name:							"shapTo"
					text:							qsTr("to")
					defaultValue:					20
					max:							dataSetModel.rowCount()
					min:							1
				}
			}
		}

		CheckBox
		{
			name:								"variableImportanceTable"
			text:								qsTr("Feature importance")
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
			name:								"treesVsModelErrorPlot"
			text:								qsTr("Out-of-bag error")
		}

		CheckBox
		{
			name:								"predictedPerformancePlot"
			text:								qsTr("Predictive performance")
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

	ML.ExportResults
	{
		enabled:								predictors.count > 1 && target.count > 0
	}

	ML.DataSplit
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

			CheckBox 
			{
				text:							qsTr("Scale variables")
				name:							"scaleVariables"
				checked:						true
			}

		CheckBox
		{
			name:								"setSeed"
			text:								qsTr("Set seed")
			childrenOnSameRow:					true

			IntegerField
			{
				name:							"seed"
				defaultValue:					1
				min:							-999999
				max:							999999
				fieldWidth:						60
			}
		}
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
