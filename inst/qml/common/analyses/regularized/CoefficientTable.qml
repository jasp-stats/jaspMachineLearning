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

import QtQuick			2.8
import QtQuick.Layouts	1.3
import JASP.Controls	1.0
import JASP.Widgets		1.0

CheckBox
{
	property bool confint: 	false

	name:					"coefTable"
	text:					qsTr("Regression coefficients")
	info:					qsTr("Shows a table containing the regression coefficients.")

	CheckBox 
	{
		name:				"coefTableConfInt"
		text:				qsTr("Confidence interval")
		childrenOnSameRow:	true
		info:				qsTr("Display confidence intervals around estimated regression coefficients.")
		visible:			confint

		CIField
		{ 
			name:			"coefTableConfIntLevel"
			defaultValue:	95
			info:			qsTr("The confidence level for the interval.")
		}
	}
}
