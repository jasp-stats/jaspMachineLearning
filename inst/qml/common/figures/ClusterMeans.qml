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

CheckBox
{
	name:			"clusterMeanPlot"
	text:			qsTr("Cluster means")
	info:			qsTr("Creates a plot that visualizes and compares the mean of the feature variables in each cluster.")

	CheckBox
	{
		name:		"clusterMeanPlotBarPlot"
		text:		qsTr("Display barplot")
		checked:	true
		info:		qsTr("Transform the cluster mean figure into a barplot.")
	}

	CheckBox
	{
		name:		"clusterMeanPlotSingleFigure"
		text:		qsTr("Group into one figure")
		checked:	true
		info:		qsTr("Group the plots per feature into a single figure.")
	}
}
