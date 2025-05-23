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

RadioButtonGroup
{
	readonly property bool isManual: !optimizeModel.checked

	name:					"modelOptimization"
	title:					qsTr("Tree Complexity")
	info:					qsTr("Choose how to optimize the model.")

	RadioButton
	{
		name:				"manual"
		text:				qsTr("Fixed")
		info:				qsTr("Enables you to use a user-specified complexity penalty.")

		DoubleField 
		{
			name:			"complexityParameter"
			text:			qsTr("Complexity penalty")
			defaultValue:	0.01
			min:			0
			info:			qsTr("The complexity penalty to be used. Any split that does not decrease the overall lack of fit by a factor of this parameter is not attempted.")
		}
	}

	RadioButton
	{
		id:					optimizeModel
		name:				"optimized"
		text:				qsTr("Optimized")
		checked:			true
		info:				qsTr("Enables you to optimize the prediction error on a validation data set with respect to the complexity penalty.")

		DoubleField
		{
			name:			"maxComplexityParameter"
			text:			qsTr("Max. complexity penalty")
			defaultValue:	1
			min:			0.01
			info:			qsTr("Sets the maximum value of the complexity penalty to be considered. At default, this is set to 1.")
		}
	}
}
