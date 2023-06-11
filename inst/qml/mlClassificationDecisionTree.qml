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

	UI.VariablesFormClassification { }

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
			text:					qsTr("Attempted splits")
			name:					"splitsTable"

			CheckBox
			{
				text:				qsTr("Only show splits in tree")
				name:				"splitsTreeTable"
				checked:			true
			}
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
			text:					qsTr("Decision tree")
			name:					"decisionTreePlot"
		}
	}

	UI.ExportResults
	{
		enabled:					predictors.count > 0 && target.count > 0
	}

	UI.DataSplit
	{
		trainingValidationSplit:	false
	}

	Section
	{
		title:						qsTr("Training Parameters")

		Group
		{
			title:					qsTr("Algorithmic Settings")

			IntegerField
			{
				text:				qsTr("Min. observations for split")
				name:				"minObservationsForSplit"
				min:				1
				defaultValue:		20
			}

			IntegerField
			{
				text:				qsTr("Min. observations in terminal")
				name:				"minObservationsInNode"
				min:				1
				defaultValue:		7
			}

			IntegerField
			{
				text:				qsTr("Max. interaction depth")
				name:				"interactionDepth"
				min:				1
				defaultValue:		30
				max:				30
			}

			DoubleField
			{
				text:				qsTr("Complexity penalty")
				name:				"complexityParameter"
				min:				0
				defaultValue:		0.01
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
