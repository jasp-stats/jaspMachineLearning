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
	property alias enabled:			exportSection.enabled
	property alias showSave:		saveGroup.visible

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
	}

	Group
	{
		id:							saveGroup

		FileSelector
		{
			id:						savePath
			name:					"savePath"
			label:					qsTr("Save as")
			placeholderText:		qsTr("e.g., location/model.jaspML")
			filter:					"*.jaspML"
			save:					true
			fieldWidth:				180 * preferencesModel.uiScale
			info:					qsTr("The file path for the saved model.")
		}

		CheckBox
		{
			id:						saveModel
			name:					"saveModel"
			text:					qsTr("Save trained model")
			enabled:				showSave && savePath.value != ""
			Layout.leftMargin:		10 * preferencesModel.uiScale
			info:					qsTr("When clicked, the model is exported to the specified file path.")
		}
	}
}
