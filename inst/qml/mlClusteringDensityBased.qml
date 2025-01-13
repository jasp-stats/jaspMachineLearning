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
	info: qsTr("Density-based clustering is a soft clustering method where clusters are constructed as maximal sets of points that are connected to points whose density exceeds some threshold. The density is produced by the concept that for each point within a cluster, the neighborhood within a given radius has to contain at least a minimum amount of points, that results in the density of that neighborhood to exceed a certain threshold. A density-based cluster is recognized by points having a higher density than points outside of the cluster. The set of all high-density points is called the density level. The points that do not exceed a density level are identified as outliers. The density level influences the amount of generated clusters.\n### Assumptions\n- The data consists of continuous variables.\n- (Normally distributed data aids the clustering process).")

	UI.VariablesFormClustering { id: vars }

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
		enabled:								vars.predictorCount > 1
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
