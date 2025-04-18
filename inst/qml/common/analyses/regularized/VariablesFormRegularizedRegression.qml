//
// Copyright (C) 2013-2018 University of Amsterdam
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

VariablesForm
{
	property bool	allow_nominal:	true
	property alias 	targetCount:	target.count
	property alias 	predictorCount: predictors.count
	property alias 	weightCount: 	weights.count

	AvailableVariablesList
	{
		name:								"variables"
	}

	AssignedVariablesList
	{
		id:									target
		name:								"target"
		title:								qsTr("Target")
		singleVariable:						true
		allowedColumns:						["scale"]
		info:								qsTr("In this box, the variable that needs to be predicted should be entered.")
	}

	AssignedVariablesList
	{
		id:									predictors
		name:								"predictors"
		title:								qsTr("Features")
		allowedColumns:						allow_nominal ? ["scale", "nominal"] : ["scale"]
		allowAnalysisOwnComputedColumns:	false
		info:								qsTr("In this box, the variables that provide information about the target variable should be entered.")
	}

	AssignedVariablesList
	{
		id:									weights
		name:								"weights"
		title:								qsTr("Weights")
		singleVariable:						true
		allowedColumns:						["scale"]
		info:								qsTr("In this box, an optional variable containing case weights can be entered.")
	}
}
