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
	property bool show_centroids: false

	name:			"tableClusterInformation"
	text:			qsTr("Cluster information")
	checked:		true

	CheckBox
	{
		name:		"tableClusterInformationWithinSumOfSquares"
		text:		qsTr("Within sum of squares")
		checked:	true
	}

	CheckBox
	{
		name:		"tableClusterInformationSilhouetteScore"
		text:		qsTr("Silhouette score")
	}

	CheckBox
	{
		name:		"tableClusterInformationCentroids"
		text:		qsTr("Centers")
		visible:	show_centroids
	}

	CheckBox
	{
		name:		"tableClusterInformationBetweenSumOfSquares"
		text:		qsTr("Between sum of squares")
	}

	CheckBox
	{
		name:		"tableClusterInformationTotalSumOfSquares"
		text:		qsTr("Total sum of squares")
	}
}