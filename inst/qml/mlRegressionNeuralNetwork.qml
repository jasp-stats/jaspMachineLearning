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

        CheckBox {
            text: qsTr("Mean squared error")
            name: "plotError"
            enabled: optimizeModel.checked
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

	ML.ExportResults {
		enabled: predictors.count > 0 && target.count > 0
	}

    ML.DataSplit
    {
        leaveOneOutVisible: false
        kFoldsVisible: false
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
                    { label: qsTr("ReLU"), value: "relu"},
                    { label: qsTr("Softplus"), value: "softplus"},
                    { label: qsTr("Softsign"), value: "softsign"},
                    { label: qsTr("ELU"), value: "elu"},
                    { label: qsTr("LReLU"), value: "lrelu"},
                    { label: qsTr("SiLU"), value: "silu"},
                    { label: qsTr("Mish"), value: "mish"},
                    { label: qsTr("Gaussian"), value: "gaussian"},
                    { label: qsTr("GeLU"), value: "gelu"}
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
                Layout.leftMargin: 15 * preferencesModel.uiScale
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
                text: qsTr("Set seed")
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
        }

        GroupBox
        {
            title: qsTr("Network Topology")

            RadioButtonGroup
            {
                name: "modelOpt"

                RadioButton {
                    name: "optimizationManual"
                    text: qsTr("Manual")
                    checked: true

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

                RadioButton {
                    id: optimizeModel
                    text: qsTr("Optimized")
                    name: "optimizationError"
                    checked: true

                    GroupBox
                    {
                        IntegerField {
                            id: genSize
                            name: "genSize"
                            text: qsTr("Population size")
                            defaultValue: 20
                            min: 2
                            max: 50000
                        }

                        IntegerField {
                            name: "maxGen"
                            text: qsTr("Generations")
                            defaultValue: 20
                            min: 1
                            max: 50000
                        }

                        IntegerField {
                            name: "maxLayers"
                            text: qsTr("Max. layers:")
                            defaultValue: 10
                            min: 1
                            max: 50000
                            visible: false
                        }

                        IntegerField {
                            name: "maxNodes"
                            text: qsTr("Max. nodes:")
                            defaultValue: 10
                            min: 1
                            max: 50000
                            visible: false
                        }

                        DropDown
                        {
                            id: selectionMethod
                            name: "selectionMethod"
                            indexDefaultValue: 0
                            label: qsTr("Parent selection")
                            values:
                                [
                                { label: qsTr("Roulette wheel"), value: "roulette"},
                                { label: qsTr("Universal"), value: "universal"},
                                { label: qsTr("Rank"), value: "rank"},
                                { label: qsTr("Tournament"), value: "tournament"},
                                { label: qsTr("Random"), value: "random"}
                            ]
                        }

                        IntegerField {
                            Layout.leftMargin: 15 * preferencesModel.uiScale
                            name: "candidates"
                            text: qsTr("Candidates")
                            defaultValue: 5
                            min: 1
                            max: genSize.value
                            enabled: selectionMethod.value == "tournament"
                        }

                        DropDown
                        {
                            name: "crossoverMethod"
                            indexDefaultValue: 0
                            label: qsTr("Crossover method")
                            values:
                                [
                                { label: qsTr("Uniform"), value: "uniform"},
                                { label: qsTr("One-point"), value: "onepoint"},
                                { label: qsTr("Multi-point"), value: "multipoint"}
                            ]
                        }

                        DropDown
                        {
                            name: "mutationMethod"
                            indexDefaultValue: 0
                            label: qsTr("Mutations")
                            values:
                                [
                                { label: qsTr("Reset"), value: "random"},
                                { label: qsTr("Swap"), value: "swap"},
                                { label: qsTr("Scramble"), value: "scramble"},
                                { label: qsTr("Inversion"), value: "inversion"}
                            ]
                        }

                        PercentField
                        {
                            Layout.leftMargin: 15 * preferencesModel.uiScale
                            name: "mutationRate"
                            text: qsTr("Probability")
                            defaultValue: 10
                        }

                        DropDown
                        {
                            name: "survivalMethod"
                            indexDefaultValue: 0
                            label: qsTr("Survival method")
                            values:
                                [
                                { label: qsTr("Fitness-based"), value: "fitness"},
                                { label: qsTr("Age-based"), value: "age"},
                                { label: qsTr("Random"), value: "random"}
                            ]
                        }

                        CheckBox
                        {
                            Layout.leftMargin: 15 * preferencesModel.uiScale
                            id: elitism
                            name: "elitism"
                            label: qsTr("Elitism")
                            checked: true
                            childrenOnSameRow: true

                            PercentField {
                                name: "elitismProp"
                                defaultValue: 10
                                enabled: elitism.checked
                            }
                        }
                    }
                }
            }
        }
    }
}
