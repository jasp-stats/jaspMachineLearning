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
	info: qsTr("Model-based clustering is based on parameterized finite Gaussian mixture models. The models are estimated by EM algorithm initialized by hierarchical model-based agglomerative clustering.")

	UI.VariablesFormClustering { id: vars }

	Group
	{
		title:						qsTr("Tables")

		TAB.ClusterInfo { }
		TAB.ModelPerformance {}
		TAB.ClusterMeans { }
		CheckBox
		{
			name:					"tableModelParameters"
			text:					qsTr("Parameter estimates")
			info:					qsTr("Shows tables containing the model parameters for each cluster and feature variable.")
		}
	}

	Group
	{
		title:						qsTr("Plots")

		FIG.ElbowMethod { enable: !optim.isManual }
		FIG.Tsne { }
		FIG.ClusterMatrix { }
		FIG.ClusterMeans { }
		FIG.ClusterDensity { }
	}

	UI.ExportResults
	{
		enabled:					vars.predictorCount > 1
		showSave:					false
	}

	Section
	{
		title:						qsTr("Training Parameters")

		Group
		{
			title:					qsTr("Algorithmic Settings")

			DropDown
			{
				id:					modelName
				name:				"modelName"
				indexDefaultValue:	0
				label:				qsTr("Model")
				values:
					[
					{ label: qsTr("Auto"),	value: "auto"},
					{ label: qsTr("EII"),	value: "EII"},
					{ label: qsTr("VII"),	value: "VII"},
					{ label: qsTr("EEI"),	value: "EEI"},
					{ label: qsTr("VEI"),	value: "VEI"},
					{ label: qsTr("EVI"),	value: "EVI"},
					{ label: qsTr("VII"),	value: "VVI"},
					{ label: qsTr("EEE"),	value: "EEE"},
					{ label: qsTr("VEE"),	value: "VEE"},
					{ label: qsTr("EVE"),	value: "EVE"},
					{ label: qsTr("VVE"),	value: "VVE"},
					{ label: qsTr("EEV"),	value: "EEV"},
					{ label: qsTr("VEV"),	value: "VEV"},
					{ label: qsTr("EVV"),	value: "EVV"},
					{ label: qsTr("VVV"),	value: "VVV"},
				]
				info:				qsTr("Choose the model to be fitted in the EM step of the clustering.")
			}

			IntegerField
			{
				name:				"maxNumberIterations"
				text:				qsTr("Max. iterations")
				defaultValue:		25
				min:				1
				max:				999999
				enabled:			modelName.value == "auto" | modelName.value == "VEI" | modelName.value == "VEE" | modelName.value == "EVE" | modelName.value == "VVE" | modelName.value == "VEV"
				info:				qsTr("The maximum number of iterations for the M-step in the algorithm.")
			}

			UI.ScaleVariables { }
			UI.SetSeed { }
		}

		UI.ClusterDetermination { id: optim }
	}
}
