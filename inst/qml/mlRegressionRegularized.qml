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
			allowedColumns:						["scale", "ordinal"]
			allowAnalysisOwnComputedColumns:	false
		}

		AssignedVariablesList
		{
			name:								"weights"
			title:								qsTr("Weights")
			singleVariable:						true
			allowedColumns:						["scale"]
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
			name:								"coefTable"
			text:								qsTr("Regression coefficients")
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
			name:								"predictedPerformancePlot"
			text:								qsTr("Predictive performance")
		}

		CheckBox
		{
			name:								"variableTrace"
			text:								qsTr("Variable trace")

			CheckBox
			{
				name:							"variableTraceLegend"
				text:							qsTr("Legend")
				checked:						true
			}
		}

		CheckBox
		{
			name:								"lambdaEvaluation"
			text:								qsTr("\u03BB evaluation")

			CheckBox
			{
				name:							"lambdaEvaluationLegend"
				text:							qsTr("Legend")
				checked:						true
			}
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
		trainingValidationSplit:				!fixedModel.checked
	}

	Section
	{
		text:									qsTr("Training Parameters")

		Group
		{
			title:								qsTr("Algorithmic Settings")

			DoubleField
			{
				name:							"convergenceThreshold"
				text:							qsTr("Convergence threshold")
				defaultValue:					1e-7
				min:							1e-999
				max:							1
				fieldWidth:						60
				visible:						false
			}

			DropDown
			{
				id:								penalty
				name:							"penalty"
				indexDefaultValue:				0
				label:							qsTr("Penalty")
				values:
					[
					{ label: qsTr("Lasso"),		value:"lasso"},
					{ label: qsTr("Ridge"),		value:"ridge"},
					{ label: qsTr("Elastic net"),value:"elasticNet"}
				]
			}

			DoubleField
			{
				name:							"alpha"
				text:							qsTr("Elastic net parameter (\u03B1)")
				defaultValue:					0.5
				min:							0
				max:							1
				visible:						penalty.currentIndex == 2
			}

			CheckBox
			{
				name:							"intercept"
				text:							qsTr("Fit intercept")
				checked:						true
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
			title:								qsTr("Lambda (\u03BB)")
			name:								"modelOptimization"

			RadioButton
			{
				id:								fixedModel
				text:							qsTr("Fixed")
				name:							"manual"

				DoubleField
				{
					name:						"lambda"
					label:						"\u03BB = "
					defaultValue:				1
					min:						0
					max:						999999
				}
			}

			RadioButton
			{
				text:							qsTr("Optimized")
				name:							"optMin"
				checked:						true
			}

			RadioButton
			{
				text:							qsTr("Largest \u03BB within 1 SE of min")
				name:							"opt1SE"
			}
		}
	}
}
