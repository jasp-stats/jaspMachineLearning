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
    title: qsTr("Balance Target Preferences")

    CheckBox 
	{ 
		name: "balanceLabels"
		label: qsTr("Balance sample size of target levels")
		info: qsTr("When clicked, the dataset is balanced to have the same sample size for all levels of the target variable. This is done either through over- or under-sampling")

		RadioButtonGroup
		
		{
			name: "balanceSamplingMethod"

			RadioButton{ value: "minSample"; label: qsTr("Balance using undersampling"); checked: true}
			RadioButton{ value: "maxSample"; label: qsTr("Balance using oversampling")}
		}
	}
}
