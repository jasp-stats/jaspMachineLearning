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

		TAB.ClusterMeans { }
		TAB.ClusterInfo { show_centroids: true; k_means: true}
		TAB.ModelPerformance {}
	}

	Group
	{
		title:									qsTr("Plots")

		FIG.ElbowMethod { enable: !optim.isManual }
		FIG.ClusterMatrix { }
		FIG.ClusterMeans { }
		FIG.ClusterDensity { }
		FIG.Tsne { }
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

			DropDown
			{
				id:								centers
				name:							"centers"
				indexDefaultValue:				0
				label:							qsTr("Center type")
				values:
					[
					{ label: "Means", 			value: "means"},
					{ label: "Medians", 		value: "medians"},
					{ label: "Medoids", 		value: "medoids"}
				]
			}

			DropDown
			{
				id:								algorithm
				name:							"algorithm"
				indexDefaultValue:				0
				label:							qsTr("Algorithm")
				enabled:						centers.value  != "medians"
				values: centers.value == "medoids" ? 
					[
						{ label: "PAM", 	value: "pam"},
						{ label: "CLaRA", 	value: "clara"}
					] : 
					[
						{ label: "Hartigan-Wong", 	value: "Hartigan-Wong"},
						{ label: "Lloyd-Forgy", 	value: "Lloyd"},
						{ label: "MacQueen", 		value: "MacQueen"}
					]
			}

			DropDown
			{
				name:							"distance"
				indexDefaultValue:				0
				label:							qsTr("Distance")
				enabled:						centers.value == "medoids"
				values:
					[
					{ label: qsTr("Euclidean"), value: "euclidean"},
					{ label: qsTr("Manhattan"), value: "manhattan"}
				]
			}

			IntegerField
			{
				name:							"maxNumberIterations"
				text:							qsTr("Max. iterations")
				defaultValue:					25
				min:							1
				max:							999999
				enabled:						!(centers.value == "medoids" & algorithm.value == "pam")
			}

			IntegerField
			{
				name:							"noOfRandomSets"
				text:							qsTr("Random sets")
				defaultValue:					25
				min:							1
				max:							999999
				enabled:						!(centers.value == "medoids" & algorithm.value == "clara")
			}

			UI.ScaleVariables { }
			UI.SetSeed { }
		}

		UI.ClusterDetermination { id: optim }
	}
}
