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
	IntegerField
	{
		name:			"minObservationsForSplit"
		text:			qsTr("Min. observations for split")
		min:			1
		defaultValue:	20
		info:			qsTr("The minimum number of observations that must exist in a node in order for a split to be attempted.")
	}

	IntegerField
	{
		name:			"minObservationsInNode"
		text:			qsTr("Min. observations in terminal")
		min:			1
		defaultValue:	7
		info:			qsTr("The minimum number of observations in any terminal node.")
	}

	IntegerField
	{
		name:			"interactionDepth"
		text:			qsTr("Max. interaction depth")
		min:			1
		defaultValue:	30
		max:			30
		info:			qsTr("Set the maximum depth of any node of the final tree.")
	}
}
