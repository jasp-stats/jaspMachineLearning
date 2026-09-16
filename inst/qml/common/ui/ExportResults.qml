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

Section
{
	property alias showSave:				saveGroup.visible
	property bool showProbs:				false
	property bool showSoftMemberships:		false
	property bool showTsne:					false

	title:									qsTr("Export Results")
	columns:								1

	CheckBox
	{
		id:									addPredictions
		name:								"addPredictions"
		text:								qsTr("Add predictions to data")
		info:								qsTr("Adds a column with the predicted cluster membership (clustering) or predicted values (regression/classification) to the dataset.")

		ComputedColumnField
		{
			id:								predictionsColumn
			name:							"predictionsColumn"
			text:							qsTr("Column name")
			placeholderText:				qsTr("e.g., predicted")
			fieldWidth:						120
			enabled:						addPredictions.checked
			info:							qsTr("The column name for the predicted values.")
		}

		CheckBox
		{
			id:								probabilities
			name:							"addProbabilities"
			text:							qsTr("Add probabilities (classification only)")
			visible:						showProbs
			info:							qsTr("In classification analyses, append the predicted probabilities for each class to the data. For neural networks, this option provides the output of the final layer.")
		}
	}

	CheckBox
	{
		id:									addSoftMemberships
		name:								"addSoftMemberships"
		text:								qsTr("Add soft memberships / posteriors")
		visible:							showSoftMemberships
		info:								qsTr("Adds one column per cluster with the soft membership or posterior probability of each observation. The column name is used as a prefix (e.g., membership_1, membership_2).")

		TextField
		{
			name:							"softMembershipsColumn"
			text:							qsTr("Column name prefix")
			placeholderText:				qsTr("e.g., membership")
			fieldWidth:						120
			enabled:						addSoftMemberships.checked
			info:							qsTr("Prefix for the soft membership columns. One column is created per cluster.")
		}
	}

	CheckBox
	{
		id:									addTsneCoordinates
		name:								"addTsneCoordinates"
		text:								qsTr("Add t-SNE coordinates")
		visible:							showTsne
		info:								qsTr("Adds the two-dimensional t-SNE embedding (dimension 1 and 2) for each observation. These are the same coordinates used in the t-SNE cluster plot. The column name is used as a prefix (e.g., tsne_1, tsne_2).")

		TextField
		{
			name:							"tsneCoordinatesColumn"
			text:							qsTr("Column name prefix")
			placeholderText:				qsTr("e.g., tsne")
			fieldWidth:						120
			enabled:						addTsneCoordinates.checked
			info:							qsTr("Prefix for the t-SNE coordinate columns (suffixes _1 and _2 are added).")
		}
	}

	Group
	{
		id:									saveGroup

		CheckBox
		{
			name:							"saveModel"
			text:							qsTr("Save trained model")
			info:							qsTr("When clicked, the model is exported to the specified file path.")

			FileSelector
			{
				name:						"savePath"
				label:						qsTr("Save as")
				placeholderText:			qsTr("e.g., location/model.jaspML")
				filter:						"*.jaspML"
				save:						true
				fieldWidth:					180 * preferencesModel.uiScale
				info:						qsTr("The file path for the saved model.")
			}
		}
	}
}
