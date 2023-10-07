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
	info: qsTr("Naive Bayes computes the conditional posterior probabilities of a categorical class variable given independent predictor variables using the Bayes rule.\n### Assumptions\n- The target variable is a nominal or ordinal variable.\n- The features are independent.\n- The features are normally distributed given the target class.")

	UI.VariablesFormClassification { id: vars }

	Group
	{
		title: qsTr("Tables")

		TAB.ConfusionMatrix { }
		TAB.ClassProportions { }
		TAB.ModelPerformance { }
		TAB.FeatureImportance { }
		TAB.ExplainPredictions { }
		CheckBox
		{
			name:	"tablePosterior"
			label:	qsTr("Posterior statistics")
			info:	qsTr("Show tables with the posterior statistics. For numeric features, the table contains the mean and standard deviation of the feature given the target class. For categorical features, the table displays the conditional probabilities given the target class.")
		}
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

			DoubleField
			{
				name:			"smoothingParameter"
				label:			qsTr("Smoothing parameter")
				defaultValue:	0
				min:			0
				info:			qsTr("A positive double controlling the amount of Laplace smoothing applied. The default (0) disables Laplace smoothing alltogether.")
			}
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
