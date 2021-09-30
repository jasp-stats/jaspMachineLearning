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

import QtQuick									2.8
import QtQuick.Layouts							1.3
import JASP.Controls							1.0
import JASP.Widgets								1.0

import "./common" as ML

Form 
{
	columns: 									1

	FileSelector
	{
		id:										file
		name:									"file"
		label:  								qsTr("Trained model:")
		filter:									"*.rds"
		save:									true
		fieldWidth:								180 * preferencesModel.uiScale 
		visible:								modelSave.checked
	}

    VariablesForm 
	{
		enabled: 								file.value != ""
        
		AvailableVariablesList 
		{ 
			name: 								"variables" 
		}

        AssignedVariablesList 
		{
            id: 								predictors
            name: 								"predictors"
            title: 								qsTr("New predictors")
			allowedColumns: 					["scale", "ordinal", "nominal", "nominalText"]
			allowAnalysisOwnComputedColumns: 	false
        }
    }

	GridLayout
	{
		GroupBox
		{
			title: 								qsTr("Algorithmic Settings")

			CheckBox 
			{ 
				text: 							qsTr("Scale predictors") 
				name: 							"scaleEqualSD"
				checked: 						true
			}
		}

		GroupBox 
		{
			title: 								qsTr("Tables")

			CheckBox
			{
				name: 							"predictionsTable"
				label:				 			"Predictions for new data"
				checked:						true

				CheckBox
				{
					name: 						"addPredictors"
					label:						"Add predictors"
				}
			}
		}
	}

	CheckBox 
	{
		id: 									addClasses
		name: 									"addClasses"
		text: 									qsTr("Add predicted outcomes to data")
		enabled:    							predictors.count > 0 & file.value != ""

		ComputedColumnField 
		{ 
			id: 								classColumn
			name: 								"classColumn"
			text: 								qsTr("Column name: ")
			fieldWidth: 						120
			visible:    						addClasses.checked
		}
	}
}
