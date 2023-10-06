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
import "./common/analyses/regularized" as REGU

Form 
{
	info: qsTr("Regularized linear regression is an adaptation of linear regression in which the coefficients are shrunken towards 0. This is done by applying a penalty (e.g., ridge, lasso, or elastic net). The parameter λ controls the degree to which parameters are shrunken.\n### Assumptions\n- The target variable is a continuous variable.\n- The feature variables consist of continuous, nominal, or ordinal variables.")

	REGU.VariablesFormRegularizedRegression { id: vars; allow_nominal: false }

	Group
	{
		title:									qsTr("Tables")

		TAB.ModelPerformance { }
		TAB.FeatureImportance { }
		TAB.ExplainPredictions { }
		REGU.CoefficientTable { }
	}

	Group
	{
		title:									qsTr("Plots")

		FIG.DataSplit { }
		FIG.PredictivePerformance { }

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

	UI.ExportResults
	{
		enabled:								vars.predictorCount > 1 && vars.targetCount > 0
	}

	UI.DataSplit
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

			REGU.Intercept { }
			UI.ScaleVariables { }
			UI.SetSeed { }
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
