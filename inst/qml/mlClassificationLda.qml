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

import QtQuick
import QtQuick.Layouts
import JASP.Controls

import "./common/ui" as UI
import "./common/tables" as TAB
import "./common/figures" as FIG

Form 
{
	info: qsTr("Linear Discriminant Analysis (LDA) is a method of classification that aims to find *p - 1* components that discriminate best between the classes in the target variable. LDA is a linear classifier, meaning that the decision boundaries between classes are linear.\n### Assumptions\n- The target variable is a nominal or ordinal variable.\n- The feature variables consist of continuous, nominal, or ordinal variables.\n- Equality of class means: The class means should be equal, can be checked with the corresponding table.\n- Equality of covariance matrices: The covariance matrices should be equal, can be checked with the corresponding table.\n- Multicollinearity: The classes should not correlate within each other, can be checked with the corresponding table.")

	UI.VariablesFormClassification { id: vars; allow_nominal: false }

	Group
	{
		title:						qsTr("Tables")

		TAB.ConfusionMatrix { }
		TAB.ClassProportions { }
		TAB.ModelPerformance { }
		TAB.FeatureImportance { }
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

		FIG.DecisionBoundary { }
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

			CheckBox
			{
				name:				"multinormalTable"
				text:				qsTr("Multivariate normality")
			}
		}

		UI.ExportResults
		{
			enabled:				vars.predictorCount > 1 && vars.targetCount > 0
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

