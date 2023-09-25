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
	info: qsTr("Random Forest clustering is a hard partitioning algorithm which aims to partition data into several clusters, where each observation belongs to only one group. This clustering method uses the Random Forest algorithm in an unsupervised way, with the outcome variable 'y' set to NULL. The Random Forest algorithm generates a proximity matrix which gives an estimate of the distance between observations based on the frequency of observations ending up in the same leaf node.\n### Assumptions\n- The data consists of continuous variables.\n- (Normally distributed data aids the clustering process).")

	UI.VariablesFormClustering { id: vars }

	Group
	{
		title:									qsTr("Tables")

		TAB.ClusterInfo { }
		TAB.ModelPerformance {}
		TAB.ClusterMeans { }
		TAB.FeatureImportance { }
	}

	Group
	{
		title:									qsTr("Plots")

		FIG.ElbowMethod { enable: !optim.isManual }
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

			IntegerField
			{
				name:							"numberOfTrees"
				text:							qsTr("Trees")
				defaultValue:					1000
				min:							1
				max:							50000
				fieldWidth:						60
			}

			UI.ScaleVariables { }
			UI.SetSeed { }
		}

		UI.ClusterDetermination { id: optim }
	}
}
