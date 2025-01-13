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

Group
{
	DropDown
	{
		id:					weights
		name:				"weights"
		indexDefaultValue:	0
		label:				qsTr("Weights")
		values:
			[
			{ label: qsTr("Linear"),	value: "linear"},
			{ label: qsTr("Radial"),	value: "radial"},
			{ label: qsTr("Polynomial"),value: "polynomial"},
			{ label: qsTr("Sigmoid"),	value: "sigmoid"}
		]
		info:				qsTr("The kernel used in training and predicting. Possible kernels are 'linear', 'radial', 'polynomial', and 'sigmoid'.")
	}

	DoubleField
	{
		name:				"degree"
		text:				qsTr("Degree")
		defaultValue:		3
		min:				1
		enabled:			weights.value == "polynomial"
		Layout.leftMargin:	10 * preferencesModel.uiScale
		info:				qsTr("The degree of polynomial used.")
	}

	DoubleField
	{
		name:				"gamma"
		text:				qsTr("Gamma parameter")
		defaultValue:		1
		min:				0
		enabled:			weights.value != "linear"
		Layout.leftMargin:	10 * preferencesModel.uiScale
		info:				qsTr("The gamma parameter used.")
	}

	DoubleField
	{
		name:				"complexityParameter"
		text:				qsTr("r parameter")
		defaultValue:		0
		min:				0
		enabled:			weights.value == "polynomial" || weights.value == "sigmoid"
		Layout.leftMargin:	10 * preferencesModel.uiScale
		info:				qsTr("The complexity parameter used.")
	}

	DoubleField
	{
		name:				"tolerance"
		text:				qsTr("Tolerance of termination criterion")
		defaultValue:		0.001
		min:				0.001
		info:				qsTr("The tolerance of termination criterion.")
	}

	DoubleField
	{
		name:				"epsilon"
		text:				qsTr("Epsilon")
		defaultValue:		0.01
		min:				0.001
		info:				qsTr("The epsilon parameter in the insensitive-loss function.")
	}
}
