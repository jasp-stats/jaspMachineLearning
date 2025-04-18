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

import QtQuick
import QtQuick.Layouts
import JASP.Controls

import "./common/ui" as UI
import "./common/tables" as TAB

Form 
{
	info: qsTr("The prediction analysis enables you to load a trained machine learning model and apply it to new data. It is important that the features in the new dataset have the same names as in the original dataset used for training.")

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
			allowedColumns:						["scale", "nominal"]
			allowAnalysisOwnComputedColumns:	false
		}
	}

	Group
	{
		title:									qsTr("Tables")

		CheckBox
		{
			id:									predictionsTable
			name:								"predictionsTable"
			label:				 				qsTr("Predictions for new data")
			checked:							true

			Row
			{	
				spacing:						5 * preferencesModel.uiScale
				enabled:						predictionsTable.checked
			
				IntegerField
				{
					name:						"fromIndex"
					text:						qsTr("Cases")
					defaultValue:				1
					min:						1
					max:						dataSetInfo.rowCount
				}

				IntegerField
				{
					name:						"toIndex"
					text:						qsTr("to")
					defaultValue:				20
					max:						dataSetInfo.rowCount
					min:						1
				}
			}

			CheckBox
			{
				name:							"predictionsTableFeatures"
				label:							qsTr("Add features")
			}
		}

		CheckBox
		{
			text:								qsTr("Explain predictions")
			name:								"tableShap"
		}
	}

	UI.ExportResults {
		enabled:								predictors.count > 1
		showSave:								false
		showProbs:								true
	}
}
