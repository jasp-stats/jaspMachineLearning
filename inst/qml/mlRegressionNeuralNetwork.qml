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

import "./common" as ML

Form 
{

	VariablesForm
	{
		AvailableVariablesList
		{
			name: "variables"
		}

		AssignedVariablesList
		{
			id: target
			name: "target"
			title: qsTr("Target")
			singleVariable: true
			allowedColumns: ["scale"]
		}

		AssignedVariablesList
		{
			id: predictors
			name: "predictors"
			title: qsTr("Predictors")
			allowedColumns: ["scale"]
			allowAnalysisOwnComputedColumns: false
		}
	}

	GroupBox
	{
		title: qsTr("Tables")

		CheckBox
		{
			text: qsTr("Evaluation metrics")
			name: "validationMeasures"
		}

		CheckBox
		{
			name: "coefficientsTable"
			text: qsTr("Network weights")
		}

	}

	GroupBox
	{
		title: qsTr("Plots")

		CheckBox
		{
			text: qsTr("Data split")
			name: "dataSplitPlot"
			checked: true
		}

		CheckBox
		{
			text: qsTr("Predictive performance")
			name: "predictedPerformancePlot"
		}

		CheckBox
		{
			name: "actFuncPlot"
			text: qsTr("Activation function")
		}

		CheckBox
		{
			name: "networkGraph"
			text: qsTr("Network structure")
		}
	}

	ML.DataSplit
	{
		leaveOneOutVisible: false;
		kFoldsVisible: false
		trainingValidationSplit: false
	}

	Section
	{
		title: qsTr("Training Parameters")

		GroupBox
		{
			title: qsTr("Algorithmic Settings")

			DropDown
			{
				name: "actfct"
				indexDefaultValue: 2
				label: qsTr("Activation function")
				values:
					[
					{ label: qsTr("Linear"), value: "linear"},
					{ label: qsTr("Binary"), value: "binary"},
					{ label: qsTr("Logistic sigmoid"), value: "sigmoid"},
					{ label: qsTr("Sine"), value: "sin"},
					{ label: qsTr("Cosine"), value: "cosin"},
					{ label: qsTr("Inverse tangent"), value: "arctan"},
					{ label: qsTr("Hyperbolic tangent"), value: "tanh"},
					{ label: qsTr("Rectified linear unit (ReLU)"), value: "relu"},
					{ label: qsTr("Softplus"), value: "softplus"},
					{ label: qsTr("Softsign"), value: "softsign"},
					{ label: qsTr("Exponential linear units (ELU)"), value: "elu"},
					{ label: qsTr("Leaky rectified linear unit (LReLU)"), value: "lrelu"},
					{ label: qsTr("Sigmoid linear unit (SiLU)"), value: "silu"},
					{ label: qsTr("Mish"), value: "mish"},
					{ label: qsTr("Gaussian"), value: "gaussian"},
					{ label: qsTr("Gaussian error linear unit (GeLU)"), value: "gelu"}
				]
			}

			DropDown
			{
				id: algorithm
				name: "algorithm"
				indexDefaultValue: 1
				label: qsTr("Algorithm")
				values:
					[
					{ label: qsTr("Backpropagation"), value: "backprop"},
					{ label: qsTr("rprop+"), value: "rprop+"},
					{ label: qsTr("rprop-"), value: "rprop-"},
					{ label: qsTr("grprop-sag"), value: "sag"},
					{ label: qsTr("grprop-slr"), value: "slr"}
				]
			}

			DoubleField
			{
				Layout.leftMargin: 15
				name: "learningRate"
				label: qsTr("Learning rate")
				defaultValue: 0.05
				min: 0
				enabled: algorithm.value == "backprop"
				fieldWidth: 60
			}

			DropDown
			{
				name: "errfct"
				debug: true
				indexDefaultValue: 0
				label: qsTr("Loss function")
				values:
					[
					{ label: qsTr("Sum of squares"), value: "sse"},
					{ label: qsTr("Cross-entropy"), value: "ce"}
				]
			}

			DoubleField
			{
				name: "threshold"
				label: qsTr("Stopping criteria loss function")
				defaultValue: 1
				min: 0
				fieldWidth: 60
			}

			IntegerField
			{
				name: "stepMax"
				label: qsTr("Max. training repetitions")
				defaultValue: 100000
				min: 1
				fieldWidth: 60
			}

			CheckBox
			{
				text: qsTr("Scale variables")
				name: "scaleEqualSD"
				checked: true
			}

			CheckBox
			{
				name: "seedBox"
				text: qsTr("Set seed:")
				childrenOnSameRow: true

				DoubleField
				{
					name: "seed"
					defaultValue: 1
					min: -999999
					max: 999999
					fieldWidth: 60
				}
			}

			RadioButtonGroup
			{
				name: "modelOpt"
				visible: false

				RadioButton
				{
					name: "optimizationManual"
					checked: true
				}
			}
		}

		GroupBox
		{
			title: qsTr("Network Structure")

			RowLayout
			{
				Label
				{
					text: qsTr("Nodes")
					Layout.leftMargin: 130 * preferencesModel.uiScale
					Layout.preferredWidth: 70 * preferencesModel.uiScale
				}
			}

			ComponentsList
			{
				name:					"layers"
				defaultValues: 			[1]
				minimumItems:			1
				rowComponent: 			RowLayout
				{
					Row
					{
						spacing:				4 * preferencesModel.uiScale
						Layout.preferredWidth:	110 * preferencesModel.uiScale
						Label
						{
							text: 				qsTr("Hidden layer ") + (rowIndex + 1)
						}
					}
					Row
					{
						spacing: 4 * preferencesModel.uiScale
						Layout.preferredWidth: 50 * preferencesModel.uiScale
						IntegerField
						{
							id: nodes
							name: "nodes"
							useExternalBorder: true
							min: 1
							defaultValue: 1
						}
					}
				}
			}
		}
	}

	Item
	{
		Layout.preferredHeight: addValues.height*2
		Layout.fillWidth: 	true
		Layout.columnSpan: 2

		CheckBox
		{
			id: addValues
			name: "addValues"
			text: qsTr("Add predicted values to data")
			enabled:    predictors.count > 0 && target.count > 0
			anchors.top: parent.top

			ComputedColumnField
			{
				id: 		valueColumn
				name: 		"valueColumn"
				text: 		qsTr("Column name: ")
				fieldWidth: 120
				visible:    addValues.checked
			}

		}

		Button
		{
			id: 			saveModel
			anchors.right: 	parent.right
			text: 			qsTr("<b>Save Model</b>")
			enabled: 		predictors.count > 0 && target.count > 0
			onClicked:
			{

			}
			debug: true
		}
	}
}
