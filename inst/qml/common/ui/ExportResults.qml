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
	property alias enabled:			exportSection.enabled
	property alias showSave:		saveGroup.visible
	property bool showProbs:		false

	id:								exportSection
	title:							qsTr("Export Results")

	CheckBox 
	{
		id:							addPredictions
		name:						"addPredictions"
		text:						qsTr("Add predictions to data")
		info:						qsTr("Generates a new column in your dataset with the values of your regression result. This gives you the option to inspect, cluster, or predict the generated values.")

		ComputedColumnField 
		{
			id:						predictionsColumn
			name:					"predictionsColumn"
			text:					qsTr("Column name")
			placeholderText:		qsTr("e.g., predicted")
			fieldWidth:				120
			enabled:				addPredictions.checked
			info:					qsTr("The column name for the predicted values.")
		}

		CheckBox
		{
			id:						probabilities
			name:					"addProbabilities"
			text:					qsTr("Add probabilities (classification only)")
			visible:				showProbs
			info:					qsTr("In classification analyses, append the predicted probabilities for each class to the data. For neural networks, this option provides the output of the final layer.")
		}
	}

	Group
	{
		id:							saveGroup

		CheckBox
		{
			name:					"saveModel"
			text:					qsTr("Save trained model")
			info:					qsTr("When clicked, the model is exported to the specified file path.")

			FileSelector
			{
				name:				"savePath"
				label:				qsTr("Save as")
				placeholderText:	qsTr("e.g., location/model.jaspML")
				filter:				"*.jaspML"
				save:				true
				fieldWidth:			180 * preferencesModel.uiScale
				info:				qsTr("The file path for the saved model.")
			}
		}
	}
}
