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
			name:								"relativeInfluenceTable"
			text:								qsTr("Relative influence")
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
			name:								"outOfBagImprovementPlot"
			text:								qsTr("Out-of-bag improvement")
		}

		CheckBox
		{
			text:								qsTr("Predictive performance")
			name:								"predictedPerformancePlot"
		}

		CheckBox
		{
			name:								"deviancePlot"
			text:								qsTr("Deviance")
		}

		CheckBox
		{
			name:								"relativeInfluencePlot"
			text:								qsTr("Relative influence")
		}
	}

	ML.ExportResults
	{
		enabled:								predictors.count > 1 && target.count > 0
	}

	ML.DataSplit
	{
		leaveOneOutVisible:						false
		trainingValidationSplit:				optimizeModel.checked
	}

	Section
	{
		title:									qsTr("Training Parameters")

		Group
		{
			title:								qsTr("Algorithmic Settings")

			DoubleField
			{
				name:							"shrinkage"
				text:							qsTr("Shrinkage")
				defaultValue:					0.1
				min:							0
				max:							1
			}

			IntegerField
			{
				name:							"interactionDepth"
				text:							qsTr("Interaction depth")
				defaultValue:					1
				min:							1
				max:							99
			}

			IntegerField
			{
				name:							"minObservationsInNode"
				text:							qsTr("Min. observations in node")
				defaultValue:					10
				min:							1
				max:							50000
			}

			PercentField 
			{
				name:							"baggingFraction"
				text:							qsTr("Training data used per tree")
				defaultValue:					50
			}

			DropDown
			{
				name:							"distance"
				indexDefaultValue:				0
				label:							qsTr("Loss function")

				values:
					[
					{ label: "Gaussian",value: "gaussian"},
					{ label: "Laplace", value: "laplace"},
					{ label: "t", 		value: "tdist"}
				]
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
			title:								qsTr("Number of Trees")
			name:								"modelOptimization"

			RadioButton
			{
				text:							qsTr("Fixed")
				name:							"optimizationManual"

				IntegerField 
				{
					name:						"noOfTrees"
					text:						qsTr("Trees")
					defaultValue:				100
					min:						1
					max:						500000
					fieldWidth:					60
				}
			}

			RadioButton
			{
				id:								optimizeModel
				text:							qsTr("Optimized")
				name:							"optimizationOOB"
				checked:						true

				IntegerField
				{
					name:						"maxTrees"
					text:						qsTr("Max. trees")
					defaultValue:				100
					min:						3
					max:						500000
					fieldWidth:					60
				}
			}
		}
	}
}
