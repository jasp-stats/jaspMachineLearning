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
	info: qsTr("Hierarchical clustering is a hard partitioning algorithm which aims to partition data into several clusters, where each observation belongs to only one group. The data is divided in such a way that the degree of similarity between two data observations is maximal if they belong to the same group and minimal if not.\n### Assumptions\n- The data consists of continuous variables.\n- (Normally distributed data aids the clustering process).")

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

		FIG.ElbowMethod { enable: !optim.isManual }
		FIG.Tsne { }
		FIG.ClusterMatrix { }
		FIG.ClusterMeans { }
		FIG.ClusterDensity { }

		CheckBox
		{
			text:								qsTr("Dendrogram")
			name:								"dendrogram"
		}
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

			DropDown
			{
				name:							"distance"
				indexDefaultValue:				0
				label:							qsTr("Distance")
				values:
					[
					{ label: qsTr("Euclidean"),	value: "euclidean"},
					{ label: qsTr("Pearson"),	value: "pearsonCorrelation"}
				]
			}

			DropDown
			{
				name:							"linkage"
				indexDefaultValue:				0
				label:							qsTr("Linkage")
				values:
					[
					{ label: qsTr("Average"), 	value: "average"},
					{ label: qsTr("Single"), 	value: "single"},
					{ label: qsTr("Complete"), 	value: "complete"},
					{ label: qsTr("Centroid"), 	value: "centroid"},
					{ label: qsTr("Median"), 	value: "median"},
					{ label: qsTr("Ward.D"), 	value: "wardD"},
					{ label: qsTr("Ward.D2"), 	value: "wardD2"},
					{ label: qsTr("McQuitty"), 	value: "mcquitty"}
				]
			}

			UI.ScaleVariables { }
			UI.SetSeed { }
		}

		UI.ClusterDetermination { id: optim }
	}
}
