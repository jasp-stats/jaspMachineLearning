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
import "./common/analyses/decisiontree" as DT

Form 
{
	info: qsTr("Decision Trees is a supervised learning algorithm that uses a decision tree as a predictive model to go from observations about an item (represented in the roots of the tree) to conclusions about the item's target value (represented in the endpoints of the tree).\n### Assumptions\n- The target variable is a continuous variable.\n- The feature variables consist of continuous, nominal, or ordinal variables.")

	UI.VariablesFormRegression { id: vars }

	Group
	{
		title: qsTr("Tables")

		TAB.ModelPerformance { }
		TAB.FeatureImportance { }
		TAB.ExplainPredictions { }
		DT.AttemptedSplits { }
	}

	Group
	{
		title: qsTr("Plots")

		FIG.DataSplit { }
		FIG.PredictivePerformance { }
		DT.OptimPlot { regression: true; enable: !optim.isManual }
		DT.TreePlot { }
	}

	UI.ExportResults { enabled:	vars.predictorCount > 0 && vars.targetCount > 0 }
	UI.DataSplit { leaveOneOutVisible: false; kFoldsVisible: false; trainingValidationSplit: !optim.isManual }

	Section
	{
		title: qsTr("Training Parameters")

		Group
		{
			title: qsTr("Algorithmic Settings")

			DT.AlgorithmicSettings { }
			UI.ScaleVariables { }
			UI.SetSeed { }
		}

		DT.ModelOptimization { id: optim }
	}
}
