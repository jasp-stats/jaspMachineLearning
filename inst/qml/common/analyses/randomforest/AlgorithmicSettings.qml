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

Group
{
	PercentField
	{
		name:					"baggingFraction"
		text:					qsTr("Training data used per tree")
		defaultValue:			50
		min:					5
		max:					95
		info:					qsTr("Select the percentage of training data that is used to train each individual tree.")
	}

	RowLayout
	{
		DropDown
		{
			id:					noOfPredictors
			name:				"noOfPredictors"
			indexDefaultValue:	0
			label:				qsTr("Features per split")
			values:
				[
				{ label: qsTr("Auto"), 		value: "auto"},
				{ label: qsTr("Manual"), 	value: "manual"}
			]
			info:				qsTr("Set the number of feature variables that is used within each split in the decision trees. Defaults to auto.")
		}

		IntegerField
		{
			name:				"numberOfPredictors"
			defaultValue:		1
			min:				0
			max:				10000
			visible:			noOfPredictors.currentIndex == 1
			info:				qsTr("The number of feature variables in each split.")
		}
	}
}