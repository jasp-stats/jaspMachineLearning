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
	name:				"tsneClusterPlot"
	text:				qsTr("t-SNE cluster plot")
	info:				qsTr("Generates a t-SNE plot of the clustering output. t-SNE plots are used for visualizing high-dimensional data in a low-dimensional space of two dimensions aiming to illustrate the relative distances between data observations. The t-SNE two-dimensional space makes the axes uninterpretable. A t-SNE plot seeks to give an impression of the relative distances between observations and clusters. To recreate the same t-SNE plot across several clustering analyses you can set their seed to the same value, as the t-SNE algorithm uses random starting values.")

	Row
	{
		CheckBox
		{
			name:		"tsneClusterPlotLegend"
			text:		qsTr("Legend")
			checked:	true
			visible:	false
			info:		qsTr("Show a legend next to the figure.")
		}

		CheckBox
		{
			name:		"tsneClusterPlotLabels"
			text:		qsTr("Labels")
			info:		qsTr("Add the row numbers of the observations in the data set as labels to the plot.")
		}
	}
}