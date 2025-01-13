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
import "./common/analyses/boosting" as BOOSTING

Form
{
	info: qsTr("Boosting works by sequentially adding features to an decision tree ensemble, each one correcting its predecessor. However, instead of changing the weights for every incorrect classified observation at every iteration, Boosting method tries to fit the new feature to the residual errors made by the previous feature.\n### Assumptions\n- The target variable is a nominal or ordinal variable.\n- The feature variables consist of continuous, nominal, or ordinal variables.")

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
		BOOSTING.Oob { }
		BOOSTING.Deviance { }
		BOOSTING.RelativeInfluence { }
		FIG.DecisionBoundary { }
	}

	UI.ExportResults { enabled: vars.predictorCount > 1 && vars.targetCount > 0 }
	UI.DataSplit { leaveOneOutVisible: false; trainingValidationSplit: !optim.isManual }

	Section
	{
		title: qsTr("Training Parameters")

		Group
		{
			title: qsTr("Algorithmic Settings")

			BOOSTING.AlgorithmicSettings { }
			UI.ScaleVariables { }
			UI.SetSeed { }
		}

		BOOSTING.ModelOptimization { id: optim }
	}
}
