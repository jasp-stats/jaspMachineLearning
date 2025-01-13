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
	property bool show_centroids: false

	name:			"tableClusterInformation"
	text:			qsTr("Cluster information")
	checked:		true
	info:			qsTr("Displays the size of each cluster and the explained proportion of within-cluster heterogeneity. The latter is the cluster within sum of squares divided by its total over the various clusters. These outputs are shown by default.")

	CheckBox
	{
		name:		"tableClusterInformationWithinSumOfSquares"
		text:		qsTr("Within sum of squares")
		checked:	true
		info:		qsTr("Adds a row with the within sum of squares of each cluster to the table. This option is selected by default.")
	}

	CheckBox
	{
		name:		"tableClusterInformationSilhouetteScore"
		text:		qsTr("Silhouette score")
		info:		qsTr("Adds a row with the silhouette score of each cluster to the table.")
	}

	CheckBox
	{
		name:		"tableClusterInformationCentroids"
		text:		qsTr("Centers")
		visible:	show_centroids
		info:		qsTr("Adds a row with the center per feature of each cluster to the table. The center can be the mean, median or mode depending on the clustering algorithm.")
	}

	CheckBox
	{
		name:		"tableClusterInformationBetweenSumOfSquares"
		text:		qsTr("Between sum of squares")
		info:		qsTr("Adds a note with the between sum of squares of the cluster model to the table.")
	}

	CheckBox
	{
		name:		"tableClusterInformationTotalSumOfSquares"
		text:		qsTr("Total sum of squares")
		info:		qsTr("Adds a note with the total sum of squares of the cluster model to the table.")
	}
}
