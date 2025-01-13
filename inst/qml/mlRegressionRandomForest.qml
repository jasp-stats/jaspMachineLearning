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
import "./common/analyses/randomforest" as RF

Form 
{
	info: qsTr("Random Forest is a method of regression that creates a set of decision trees that consists of a large number of individual trees which operate as an ensemble.\n### Assumptions\n- The target variable is a continuous variable.\n- The feature variables consist of continuous, nominal, or ordinal variables.")

	UI.VariablesFormRegression { id: vars }

	Group
	{
		title: qsTr("Tables")

		TAB.ModelPerformance { }
		TAB.FeatureImportance { }
		TAB.ExplainPredictions { }
	}

	Group
	{
		title: qsTr("Plots")

		FIG.DataSplit { }
		FIG.PredictivePerformance { }
		RF.Oob { regression: true }
		RF.AccuracyDecrease { }
		RF.NodePurity { }
	}

	UI.ExportResults { enabled: vars.predictorCount > 1 && vars.targetCount > 0 }
	UI.DataSplit { leaveOneOutVisible: false; kFoldsVisible: false; trainingValidationSplit: !optim.isManual }

	Section
	{
		title: qsTr("Training Parameters")

		Group
		{
			title: qsTr("Algorithmic Settings")

			RF.AlgorithmicSettings { }
			UI.ScaleVariables { }
			UI.SetSeed { }
		}

		RF.ModelOptimization { id: optim }
	}
}
