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


Group
{
	title:									qsTr("Explain predictions")

	CheckBox
	{
		text:								qsTr("Additive explanations (SHAP)")
		name:								"tableShap"
		id:									tableShap

		Row
		{	
			spacing:							5 * preferencesModel.uiScale
			enabled:							tableShap.checked
		
			IntegerField
			{
				name:							"shapFrom"
				text:							qsTr("From")
				defaultValue:					1
				min:							1
				max:							dataSetModel.rowCount()
			}

			IntegerField
			{
				name:							"shapTo"
				text:							qsTr("to")
				defaultValue:					20
				max:							dataSetModel.rowCount()
				min:							1
			}
		}
	}
}
