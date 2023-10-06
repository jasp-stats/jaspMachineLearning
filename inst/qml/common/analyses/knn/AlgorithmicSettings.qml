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
	DropDown
	{
		name:				"weights"
		indexDefaultValue:	0
		label:				qsTr("Weights")
		values:
			[
			{ label: qsTr("Rectangular"), 	value: "rectangular"},
			{ label: qsTr("Triangular"), 	value: "triangular"},
			{ label: qsTr("Epanechnikov"), 	value: "epanechnikov"},
			{ label: qsTr("Biweight"), 		value: "biweight"},
			{ label: qsTr("Triweight"), 	value: "triweight"},
			{ label: qsTr("Cosine"), 		value: "cos"},
			{ label: qsTr("Inverse"), 		value: "inv"},
			{ label: qsTr("Gaussian"), 		value: "gaussian"},
			{ label: qsTr("Rank"), 			value: "rank"},
			{ label: qsTr("Optimal"), 		value: "optimal"}
		]
		info:				qsTr("Sets the weighting scheme for the nearest neighbors. The default rectangular option results in standard knn, while the other options expand the algorithm by weighing the nearest neighbors. See also the kknn package.")
	}

	DropDown
	{
		name:				"distanceParameterManual"
		indexDefaultValue:	0
		label:				qsTr("Distance")
		values:
			[
			{ label:qsTr("Euclidian"),	value:"2"},
			{ label:qsTr("Manhattan"),	value:"1"}
		]
		info:				qsTr("The distance metric to be used when determining the similarity between nearest neighbors. Can be either Euclidean or Manhattan distance.")
	}
}