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
		title:					qsTr("Tables")

		TAB.ClusterInfo { show_centroids: true }
		TAB.ModelPerformance {}
		TAB.ClusterMeans { }
	}

	Group
	{
		title:					qsTr("Plots")

		FIG.ElbowMethod { enable: !optim.isManual }
		FIG.Tsne { }
		FIG.ClusterMatrix { }
		FIG.ClusterMeans { }
		FIG.ClusterDensity { }
	}

	UI.ExportResults
	{
		enabled:				predictors.count > 1
		showSave:				false
	}

	Section
	{
		title:					qsTr("Training Parameters")

		Group
		{
			title:				qsTr("Algorithmic Settings")

			IntegerField
			{
				name:			"maxNumberIterations"
				text:			qsTr("Max. iterations")
				defaultValue:	25
				min:			1
				max:			999999
			}

			DoubleField
			{
				name:			"fuzzinessParameter"
				text:			qsTr("Fuzziness parameter")
				defaultValue:	2
				min:			1
				max:			1000
				decimals:		2
			}

			UI.ScaleVariables { }
			UI.SetSeed { }
		}

		UI.ClusterDetermination { id: optim }
	}
}
