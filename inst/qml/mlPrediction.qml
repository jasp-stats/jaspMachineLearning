//
// Copyright (C) 2013-2021 University of Amsterdam
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

import QtQuick									2.8
import QtQuick.Layouts							1.3
import JASP.Controls							1.0
import JASP.Widgets								1.0

import "./common" as ML

Form 
{

	FileSelector
	{
		id:										trainedModelFilePath
		name:									"trainedModelFilePath"
		label:									qsTr("Trained model")
		placeholderText:						qsTr("e.g., location/model.jaspML")
		filter:									"*.jaspML"
		save:									false
		fieldWidth:								180 * preferencesModel.uiScale
	}

	VariablesForm
	{
		enabled:								trainedModelFilePath.value != ""

		AvailableVariablesList
		{
			name:								"variables"
		}

		AssignedVariablesList
		{
			id:									predictors
			name:								"predictors"
			title:								qsTr("Features")
			allowedColumns:						["scale", "ordinal", "nominal", "nominalText"]
			allowAnalysisOwnComputedColumns:	false
		}
	}

	Group
	{
		title:									qsTr("Algorithmic Settings")

		CheckBox
		{
			text:								qsTr("Scale features")
			name:								"scaleVariables"
			checked:							true
		}
	}

	Group
	{
		title:									qsTr("Tables")

		CheckBox
		{
			id:									predictionsTable
			name:								"predictionsTable"
			label:				 				"Predictions for new data"
			checked:							true

			CheckBox
			{
				name:							"predictionsTableFeatures"
				label:							"Add features"
			}

			Row
			{	
				spacing:							5 * preferencesModel.uiScale
				enabled:							predictionsTable.checked
			
			IntegerField
			{
				name:							"fromIndex"
				text:							qsTr("From")
				defaultValue:					1
				min:							1
				max:							dataSetModel.rowCount()
			}

			IntegerField
			{
				name:							"toIndex"
				text:							qsTr("to")
				defaultValue:					20
				max:							dataSetModel.rowCount()
				min:							1
			}
		}
	}

	ML.ExportResults {
		enabled:								predictors.count > 1
		showSave:								false
	}
}
