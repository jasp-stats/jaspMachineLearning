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
	info: qsTr("Logistic regression is a statistical method used to model the relationship between a binary target variable (with two possible outcomes) and one or more feature variables. It predicts the probability of a specific outcome by using a logistic function, which ensures that the predicted probabilities are between 0 and 1. Multinomial regression extends logistic regression to handle target variables with more than two categories. Instead of predicting binary outcomes, multinomial regression is used for scenarios where the target variable has three or more unordered categories.")

	UI.VariablesFormClassification { id: vars }

	Group
	{
		title: qsTr("Tables")

		TAB.ConfusionMatrix { }
		TAB.ClassProportions { }
		TAB.ModelPerformance { }
		TAB.FeatureImportance { }
		TAB.ExplainPredictions { }
		REGU.CoefficientTable { confint: true }
	}

	Group
	{
		title: qsTr("Plots")

		FIG.DataSplit { }
		FIG.RocCurve { }
		FIG.AndrewsCurve { }
		FIG.DecisionBoundary { }
	}

	UI.ExportResults { enabled: vars.predictorCount > 0 && vars.targetCount > 0 }
	UI.DataSplit { trainingValidationSplit: false }

	Section
	{
		title: qsTr("Training Parameters")

		Group
		{
			title: qsTr("Algorithmic Settings")
			
			DropDown
			{
				name:										"link"
				indexDefaultValue:							0
				label:										qsTr("Link function (for binary classification)")
				values:
					[
					{ label: qsTr("Logit"),					value: "logit"},
					{ label: qsTr("Probit"),				value: "probit"},
					{ label: qsTr("Cauchit"),				value: "cauchit"},
					{ label: qsTr("Complimentary log-log"),	value: "cloglog"},
					{ label: qsTr("Log"),					value: "log"}
				]
			}
			REGU.Intercept { }
			UI.ScaleVariables { }
			UI.SetSeed { }
		}

		RadioButtonGroup
		{
			name:			"modelOptimization"
			visible:		false

			RadioButton
			{
				name:		"manual"
				checked:	true
			}
		}
	}
}
