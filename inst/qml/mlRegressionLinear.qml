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
import "./common/analyses/regularized" as REGU

Form 
{
	info: qsTr("Linear regression allows the user to model a linear relationship between one or more features (predictors) and a continuous dependent (target) variable.")

	REGU.VariablesFormRegularizedRegression { id: vars; allow_nominal: true }

	Group
	{
		title:									qsTr("Tables")

		TAB.ModelPerformance { }
		TAB.FeatureImportance { }
		TAB.ExplainPredictions { }
		REGU.CoefficientTable { confint: true }
	}

	Group
	{
		title:									qsTr("Plots")

		FIG.DataSplit { }
		FIG.PredictivePerformance { }
	}

	UI.ExportResults { enabled:	vars.predictorCount > 1 > 0 && vars.targetCount > 0 }
	UI.DataSplit { trainingValidationSplit: false }

	Section
	{
		title:									qsTr("Training Parameters")

		Group
		{
			title:								qsTr("Algorithmic Settings")

			REGU.Intercept { }
			UI.ScaleVariables { }
			UI.SetSeed { }
		}

		RadioButtonGroup
		{
			name:								"modelOptimization"
			visible:							false

			RadioButton
			{
				name:							"manual"
				checked:						true
			}
		}
	}
}
