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

	UI.VariablesFormClassification { allow_nominal: false }

	Group
	{
		title:						qsTr("Tables")

		TAB.ConfusionMatrix { }
		TAB.ClassProportions { }
		TAB.ModelPerformance { }
		TAB.ExplainPredictions { }

		CheckBox
		{
			name:					"coefficientsTable"
			text:					qsTr("Coefficients")
		}

		CheckBox
		{
			name:					"priorTable"
			text:					qsTr("Prior and posterior probabilities")
		}

		CheckBox
		{
			name:					"meanTable"
			text:					qsTr("Class means training data")
		}
	}

	Group
	{
		title:						qsTr("Plots")

		FIG.DataSplit { }
		FIG.RocCurve { }
		FIG.AndrewsCurve { }
		FIG.DecisionBoundary { }

		CheckBox
		{
			name:					"matrixPlot"
			text:					qsTr("Linear discriminant matrix")

			Row
			{
				CheckBox
				{
					name:			"plotDensities"
					text:			qsTr("Densities")
					checked:		true
				}

				CheckBox
				{
					name:			"plotStatistics"
					text:			qsTr("Scatter plots")
					checked:		true
				}
			}
		}
	}

	Column
	{
		spacing:					10 * preferencesModel.uiScale

		Group
		{
			title:					qsTr("Assumption Checks")

			CheckBox
			{
				name:				"manovaTable"
				text:				qsTr("Equality of class means")
			}

			CheckBox
			{
				name:				"boxTest"
				text:				qsTr("Equality of covariance matrices")
			}

			CheckBox
			{
				name:				"multicolTable"
				text:				qsTr("Multicollinearity")
			}
		}

		UI.ExportResults
		{
			enabled:				predictors.count > 1 && target.count > 0
		}
	}

	UI.DataSplit
	{
		leaveOneOutVisible:			false
		kFoldsVisible:				false
		trainingValidationSplit:	false
	}

	Section
	{
		title:						qsTr("Training Parameters")

		Group
		{
			title:					qsTr("Algorithmic Settings")

			DropDown
			{
				name:				"estimationMethod"
				indexDefaultValue:	0
				label:				qsTr("Estimation method")
				values:
					[
					{ label: "Moment",	value: "moment"},
					{ label: "MLE",		value: "mle"},
					{ label: "MVE",		value: "covMve"},
					{ label: "t",		value: "t"},
				]
			}

			UI.ScaleVariables { }
			UI.SetSeed { }
		}

		RadioButtonGroup
		{
			name:					"modelOptimization"
			visible:				false

			RadioButton
			{
				name:				"manual"
				checked:			true
			}
		}
	}
}

