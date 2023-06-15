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
	DoubleField
	{
		name:			"shrinkage"
		text:			qsTr("Shrinkage")
		defaultValue:	0.1
		min:			0
		max:			1
		info:			qsTr("A shrinkage parameter applied to each tree in the expansion. Also known as the learning rate or step-size reduction 0.001 to 0.1 usually work, but a smaller learning rate typically requires more trees.")
	}

	IntegerField
	{
		name:			"interactionDepth"
		text:			qsTr("Interaction depth")
		defaultValue:	1
		min:			1
		max:			99
		info:			qsTr("Integer specifying the maximum depth of each tree (i.e., the highest level of variable interactions allowed. A value of 1 implies an additive model, a value of 2 implies a model with up to 2-way interactions, etc. Default is 1.")
	}

	IntegerField
	{
		name:			"minObservationsInNode"
		text:			qsTr("Min. observations in node")
		defaultValue:	10
		min:			1
		max:			50000
		info:			qsTr("Integer specifying the minimum number of observations in the terminal nodes of the trees. Note that this is the actual number of observations, not the total weight.")
	}

	PercentField 
	{
		name:			"baggingFraction"
		text:			qsTr("Training data used per tree")
		defaultValue:	50
		info:			qsTr("Select the percentage of training data that is used to train each individual tree.")
	}
}