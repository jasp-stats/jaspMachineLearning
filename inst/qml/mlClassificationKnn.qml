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
import "./common/analyses/knn" as KNN

Form
{
	info: qsTr("K-nearest neighbors is a method of classification that looks at the *k* number of feature observations that are most similar to new observations to make a prediction for their class assignments. The number of nearest neighbors is intrinsincly linked to model complexity, as small numbers increase the flexibility of the model.\n### Assumptions\n- The target is a nominal or ordinal variable.\n- The feature variables consist of continuous, nominal, or ordinal variables.")

	UI.VariablesFormClassification { id: vars }

	Group
	{
		title: qsTr("Tables")

		TAB.ConfusionMatrix { }
		TAB.ClassProportions { }
		TAB.ModelPerformance { }
		TAB.FeatureImportance { }
		TAB.ExplainPredictions { }
	}

	Group
	{
		title: qsTr("Plots")

		FIG.DataSplit { }
		FIG.RocCurve { }
		FIG.AndrewsCurve { }
		KNN.OptimPlot { regression: false; enable: !optim.isManual }
		KNN.WeightFunction { }
		FIG.DecisionBoundary { }
	}

	UI.ExportResults { enabled: vars.predictorCount > 0 && vars.targetCount > 0 }
	UI.DataSplit { trainingValidationSplit:	!optim.isManual }

	Section
	{
		title: qsTr("Training Parameters")

		Group
		{
			title: qsTr("Algorithmic Settings")

			KNN.AlgorithmicSettings { }
			UI.ScaleVariables { }
			UI.SetSeed { }
		}

		KNN.ModelOptimization { id: optim }
	}
}
