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
	name:				"decisionBoundary"
	text:				qsTr("Decision boundary matrix")
	info:				qsTr("Creates a *n* x *n* plot that visualizes how every observation would be classified if predicted through the current model. Boundaries between classes are visualized. Can only be made for numeric features.")

	Row
	{
		CheckBox
		{
			name:		"legendShown"
			text:		qsTr("Legend")
			checked:	true
			visible:	false
			info:		qsTr("Show a legend next to the figure.")
		}

		CheckBox
		{
			name:		"pointsShown"
			text:		qsTr("Points")
			checked:	true
			info:		qsTr("Show the observations in the data set as points in the plot.")
		}
	}
}