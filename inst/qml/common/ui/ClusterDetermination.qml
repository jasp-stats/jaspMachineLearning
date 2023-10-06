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

RadioButtonGroup
{
	property bool isManual: validationManual.checked

	title:						qsTr("Cluster Determination")
	name:						"modelOptimization"
	info:						qsTr("Choose how to determine the number of clusters in the model.")

	RadioButton
	{
		id:						validationManual
		text:					qsTr("Fixed")
		name:					"manual"
		info:					qsTr("Enables you to generate a fixed amount of clusters. This allows you to generate your own specified number of clusters, and thus, optimize manually.")

		IntegerField
		{
			name:				"manualNumberOfClusters"
			text:				qsTr("Clusters")
			defaultValue:		3
			min:				2
			max:				5000
			enabled:			validationManual.checked
			fieldWidth:			60
			info:				qsTr("The number of clusters to be fitted.")
		}
	}

	RadioButton
	{
		text:					qsTr("Optimized according to")
		name:					"optimized"
		childrenOnSameRow:		true
		checked:				true
		info:					qsTr("Enables you to choose an optimization method. BIC optimization is set as default.")

		DropDown
		{
			name:				"modelOptimizationMethod"
			indexDefaultValue:	1
			info:				qsTr("The method of optimization. The options are AIC, BIC, and silhouette. The AIC uses the within sum of squares (within-cluster variation), the number of generated clusters and the number of dimensions for optimizing the clustering output. The BIC uses the within sum of squares (within-cluster variation), the number of generated clusters, the number of dimensions, and the sample size for optimizing the clustering output. The silhouette value uses the similarity of observations within a cluster and their dissimilarity to other clusters for optimizing the clustering output.")

			values:
				[
				{ label: "AIC", 		value: "aic"},
				{ label: "BIC", 		value: "bic"},
				{ label: "Silhouette",	value: "silhouette"}
			]
		}
	}

	IntegerField
	{
		name:					"maxNumberOfClusters"
		text:					qsTr("Max. clusters")
		defaultValue:			10
		min:					2
		max:					5000
		enabled:				!validationManual.checked
		Layout.leftMargin:		20
		fieldWidth:				60
		info:					qsTr("Sets the maximum number of possible clusters to be generated. At default, this is set to 10.")
	}
}