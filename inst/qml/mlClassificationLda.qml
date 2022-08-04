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
			allowedColumns:						["nominal", "nominalText", "ordinal"]
		}

		AssignedVariablesList
		{
			id:									predictors
			name:								"predictors"
			title:								qsTr("Features")
			allowedColumns:						["scale", "ordinal"]
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
			name:								"coefficientsTable"
			text:								qsTr("Coefficients")
		}

		CheckBox
		{
			name:								"priorTable"
			text:								qsTr("Prior and posterior probabilities")
		}

		CheckBox
		{
			name:								"meanTable"
			text:								qsTr("Class means training data")
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
			name:								"matrixPlot"
			text:								qsTr("Linear discriminant matrix")

			Row
			{
				CheckBox
				{
					name:						"plotDensities"
					text:						qsTr("Densities")
					checked:					true
				}

				CheckBox
				{
					name:						"plotStatistics"
					text:						qsTr("Scatter plots")
					checked:					true
				}
			}
		}

		CheckBox
		{
			name:								"decisionBoundary"
			text:								qsTr("Decision boundary matrix")

			Row
			{
				CheckBox
				{
					name:						"legendShown"
					text:						qsTr("Legend")
					checked:					true
				}

				CheckBox
				{
					name:						"pointsShown"
					text:						qsTr("Points")
					checked:					true
				}
			}
		}
	}

	Column
	{
		spacing:								10 * preferencesModel.uiScale

		Group
		{
			title:								qsTr("Assumption Checks")

			CheckBox
			{
				name:							"manovaTable"
				text:							qsTr("Equality of class means")
			}

			CheckBox
			{
				name:							"boxTest"
				text:							qsTr("Equality of covariance matrices")
			}

			CheckBox
			{
				name:							"multicolTable"
				text:							qsTr("Multicollinearity")
			}
		}

		ML.ExportResults
		{
			enabled:							predictors.count > 1 && target.count > 0
		}
	}

	ML.DataSplit
	{
		leaveOneOutVisible:						false
		kFoldsVisible:							false
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
				name:							"estimationMethod"
				indexDefaultValue:				0
				label:							qsTr("Estimation method")
				values:
					[
					{ label: "Moment",	value: "moment"},
					{ label: "MLE",		value: "mle"},
					{ label: "MVE",		value: "covMve"},
					{ label: "t",		value: "t"},
				]
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

		RadioButtonGroup
		{
			name:								"modelOptimization"
			visible:							false

			RadioButton
			{
				name:							"optimizationManual"
				checked:						true
			}
		}
	}
}

