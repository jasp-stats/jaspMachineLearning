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
	property alias leaveOneOutVisible:			leaveOneOut.visible
	property alias kFoldsVisible:				kFolds.visible
	property alias trainingValidationSplit:		trainingValidationSplit.visible
	property alias balanceTargetClasses: 		balanceTargetClasses.visible

	title: 										qsTr("Data Split Preferences")

	RadioButtonGroup
	{
		name: 									"holdoutData"
		title: 									qsTr("Holdout Test Data")
		info:									qsTr("Choose how to create the test set.")

		RadioButton
		{
			id:									holdoutManual
			name:								"holdoutManual"
			childrenOnSameRow:					true
			text:								qsTr("Sample")
			checked:							true
			info:								qsTr("Choose a percentage to randomly sample from your data to derive prediction error. Generates an internal indicator variable that indicates whether the observation is included (1) or excluded (0) from the test set.")

			Row
			{
				PercentField
				{
					name:						"testDataManual"
					defaultValue:				20
					min:						5
					max:						95
					afterLabel:					qsTr("% of all data")
					info:						qsTr("The percentage of observations to use for the test set.")
				}
			}
		}

		CheckBox
		{
			id:									addIndicator
			name:								"addIndicator"
			text:								qsTr("Add generated indicator to data")
			Layout.leftMargin:					20
			enabled:							holdoutManual.checked
			info:								qsTr("Add the generated test set indicator from the option above to your data set.")

			ComputedColumnField
			{
				name:							"testIndicatorColumn"
				text:							qsTr("Column name")
				placeholderText:				qsTr("e.g., testSet")
				fieldWidth:						120
				visible:						addIndicator.checked
				info:							qsTr("The column name for the generated test set indicator.")
			}
		}

		RadioButton
		{
			name: 								"testSetIndicator"
			label: 								qsTr("Test set indicator")
			childrenOnSameRow: 					true
			info:								qsTr("Use an indicator variable to select data for the test set. This indicator should be a column of type scale in your data that consists of only 0 (excluded from the test set) and 1 (included in the test set). The data will then be split into a training (and validation if requested) set (0), and a test set (1) according to your indicator.")

			DropDown
			{
				id: 							testSetIndicatorVariable
				name: 							"testSetIndicatorVariable"
				showVariableTypeIcon: 			true
				addEmptyValue: 					true
				placeholderText: 				qsTr("None")
				info:							qsTr("The variable in the data set that is used as the test set indicator.")
				allowedColumns:					"scale"
			}
		}
	}

	RadioButtonGroup
	{
		id: 									trainingValidationSplit
		title: 									qsTr("Training and Validation Data")
		name: 									"modelValid"
		info:									qsTr("Choose how to create the validation set.")

		RadioButton
		{
			name: 								"validationManual"
			childrenOnSameRow: 					true
			checked: 							true
			text: 								qsTr("Sample")
			info:								qsTr("Randomly sample a percentage from the remaining training data (after selecting the test set).")

			Row 
			{
				PercentField 
				{
					name: 						"validationDataManual"
					defaultValue: 				20
					min: 						5
					max: 						95
					afterLabel: 				qsTr("% for validation data")
					info:						qsTr("The percentage of observations to use for the validation set.")
				}
			}
		}

		RadioButton
		{
			id:									kFolds
			name:								"validationKFold"
			childrenOnSameRow:					true
			text:								qsTr("K-fold with")
			info:								qsTr("Partition the remaining data in *k* parts.")

			Row
			{
				IntegerField 
				{
					name:						"noOfFolds"
					defaultValue:				5
					min:						2
					max:						999
					fieldWidth:					30
					afterLabel:					qsTr("folds")
					info:						qsTr("The number of folds to be used.")
				}
			}
		}

		RadioButton 
		{
			id:									leaveOneOut
			text:								qsTr("Leave-one-out")
			name:								"validationLeaveOneOut"
			info:								qsTr("Partition the remaining data in *n* parts.")
		}
	}

    CheckBox 
	{ 
		id: balanceTargetClasses
		name: "balanceLabels"
		label: qsTr("Balance sample size of target classes")
		info: qsTr("When clicked, the dataset is balanced to have the same sample size for all classes of the target variable. This is done either through over- or undersampling")

		RadioButtonGroup
		{
			name: "balanceSamplingMethod"

			RadioButton
			{ 
				value: "minSample" 
				label: qsTr("Undersample") 
				checked: true 
				info: qsTr("Balances the target classes by undersampling to match the size of the smallest class.")
			}
			
			RadioButton
			{ 
				value: "maxSample" 
				label: qsTr("Oversample") 
				info: qsTr("Balances the target classes by oversampling to match the size of the largest class. This is done by sampling with replacement for smaller classes.")
			}
		}
	}
}
