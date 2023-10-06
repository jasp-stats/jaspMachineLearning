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
	info: qsTr("Linear regression allows the user to model a linear relationship between one or more features (predictors) and a continuous dependent (target) variable.")

	UI.VariablesFormRegression { id: vars }

	Group
	{
		title: qsTr("Tables")

		TAB.ModelPerformance { }
		TAB.ExplainPredictions { }

		CheckBox
		{
			name: "coefTable"
			text: qsTr("Regression coefficients")

			CheckBox 
			{
				name: "coefTableConfInt"
				text: qsTr("Confidence interval")
				childrenOnSameRow: true

				CIField
				{ 
					name: "coefTableConfIntLevel"
					defaultValue: 95
				}
			}
		}
	}

	Group
	{
		title: qsTr("Plots")

		FIG.DataSplit { }
		FIG.PredictivePerformance { }
	}

	UI.ExportResults { enabled:	vars.predictorCount > 0 && vars.targetCount > 0 }
	UI.DataSplit { trainingValidationSplit: false }

	Section
	{
		title: qsTr("Training Parameters")

		Group
		{
			title: qsTr("Algorithmic Settings")

			CheckBox 
			{ 
				name: "intercept"
				label: qsTr("Include intercept")
				checked: true 
				info: qsTr("Whether to include an intercept.")
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
