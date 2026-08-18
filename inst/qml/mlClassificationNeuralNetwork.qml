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
import "./common/analyses/neuralnetwork" as NN

Form 
{
	info: qsTr("Feedforward neural networks are predictive algorithms inspired by the biological neural networks that constitute brains. A neuron (node) that receives a signal then processes it and can send signals to neurons connected to it. The signal at a node is a real number, and the output of each node is computed by sending the signal trough the activation function. The number of layers and nodes in the network is intrinsincly linked to model complexity, as high numbers increase the flexibility of the model.\n### Assumptions\n- The target is a nominal or ordinal variable.\n- The feature variables consist of continuous variables.")

	UI.VariablesFormClassification { id: vars; allow_nominal: false }

	Group
	{
		title: qsTr("Tables")

		TAB.ConfusionMatrix { }
		TAB.ClassProportions { }
		TAB.ModelPerformance { }
		TAB.FeatureImportance { }
		TAB.ExplainPredictions { }
		NN.Coefficients { }
	}

	Group
	{
		title: qsTr("Plots")

		FIG.DataSplit { }
		FIG.RocCurve { }
		FIG.AndrewsCurve { }
		NN.OptimPlot { regression: false; enable: !optim.isManual }
		NN.ActivationFunctionPlot { }
		NN.NetworkPlot { }
		FIG.DecisionBoundary { }
	}

	UI.ExportResults { enabled: vars.predictorCount > 0 && vars.targetCount > 0 }
	UI.DataSplit { leaveOneOutVisible: false; kFoldsVisible: false; balanceTargetClasses: false }

	Section
	{
		title: qsTr("Training Parameters")

		Group
		{
			title: qsTr("Algorithmic Settings")

			NN.AlgorithmicSettings { }
			UI.ScaleVariables { }
			UI.SetSeed { }
		}

		NN.ModelOptimization { id: optim }
	}
}
