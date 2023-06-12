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

	UI.VariablesFormClustering { }

	Group
	{
		title:									qsTr("Tables")

		TAB.ClusterInfo { }
		TAB.ModelPerformance {}
		TAB.ClusterMeans { }
	}

	Group
	{
		title:									qsTr("Plots")

		CheckBox
		{
			text:								qsTr("K-distance plot")
			name:								"kDistancePlot"
		}

		FIG.Tsne { }
		FIG.ClusterMatrix { }
		FIG.ClusterMeans { }
		FIG.ClusterDensity { }
	}

	UI.ExportResults
	{
		enabled:								predictors.count > 1
		showSave:								false
	}

	Section
	{
		title:									qsTr("Training Parameters")

		Group
		{
			title:								qsTr("Algorithmic Settings")

			DoubleField
			{
				name:							"epsilonNeighborhoodSize"
				text:							qsTr("Epsilon neighborhood size")
				decimals:						2
				defaultValue:					2
				min:							0.01
				max:							999999
				enabled:						validationManual.checked
			}

			IntegerField
			{
				name:							"minCorePoints"
				text:							qsTr("Min. core points")
				defaultValue:					5
				min:							2
				max:							50000
			}

			DropDown
			{
				name:							"distance"
				indexDefaultValue:				0
				label:							qsTr("Distance")
				values:
					[
					{ label: qsTr("Normal"), 	value: "normalDensities"},
					{ label: qsTr("Correlated"),value: "correlatedDensities"}
					]
			}

			UI.ScaleVariables { }
			UI.SetSeed { }
		}

		RadioButtonGroup
		{
			title:								qsTr("Model Optimization")
			name:								"modelOptimization"
			visible:							false

			RadioButton
			{
				id:								validationManual
				text:							qsTr("Fixed")
				name:							"manual"
			}
		}
	}
}
