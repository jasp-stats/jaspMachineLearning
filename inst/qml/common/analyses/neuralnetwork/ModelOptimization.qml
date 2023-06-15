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
	readonly property bool isManual: !optimizeModel.checked

	title:										qsTr("Network Topology")

	RadioButtonGroup
	{
		name:									"modelOptimization"
		info:									qsTr("Choose how to optimize the model.")

		RadioButton
		{
			name:								"manual"
			text:								qsTr("Manual")
			info:								qsTr("Specify the nodes in each hidden layer of the neural network.")

			RowLayout
			{
				Label
				{
					text:						qsTr("Nodes")
					Layout.leftMargin:			130 * preferencesModel.uiScale
					Layout.preferredWidth:		70 * preferencesModel.uiScale
				}
			}

			ComponentsList
			{
				name:							"layers"
				defaultValues:					[1]
				minimumItems:					1
				rowComponent:					RowLayout
				{
					Row
					{
						spacing:				4 * preferencesModel.uiScale
						Layout.preferredWidth:	110 * preferencesModel.uiScale

						Label
						{
							text:				qsTr("Hidden layer ") + (rowIndex + 1)
						}
					}
					
					RowLayout
					{
						spacing:				4 * preferencesModel.uiScale
						Layout.preferredWidth:	50 * preferencesModel.uiScale
						
						IntegerField
						{
							id:					nodes
							name:				"nodes"
							useExternalBorder:	true
							min:				1
							defaultValue:		1
						}
					}
				}
			}
		}

		RadioButton
		{
			id:									optimizeModel
			text:								qsTr("Optimized")
			name:								"optimized"
			checked:							true
			info:								qsTr("Optimize the topology of the network using a genetic algorithm.")

			Group
			{
				IntegerField
				{
					id:							populationSize
					name:						"populationSize"
					text:						qsTr("Population size")
					defaultValue:				20
					min:						2
					max:						50000
					info:						qsTr("Size of population used in genetic optimization.")
				}

				IntegerField
				{
					name:						"maxGenerations"
					text:						qsTr("Generations")
					defaultValue:				10
					min:						1
					max:						50000
					info:						qsTr("Number of generations used in genetic optimization.")
				}

				IntegerField
				{
					name:						"maxLayers"
					text:						qsTr("Max. layers")
					defaultValue:				10
					min:						1
					max:						50000
					visible:					false
				}

				IntegerField
				{
					name:						"maxNodes"
					text:						qsTr("Max. nodes")
					defaultValue:				10
					min:						1
					max:						50000
					visible:					false
				}

				DropDown
				{
					id:							selectionMethod
					name:						"selectionMethod"
					indexDefaultValue:			0
					label:						qsTr("Parent selection")
					values:
						[
						{ label:qsTr("Roulette wheel"),	value: "roulette"},
						{ label:qsTr("Universal"), 		value: "universal"},
						{ label:qsTr("Rank"), 			value: "rank"},
						{ label: qsTr("Tournament"), 	value: "tournament"},
						{ label:qsTr("Random"),			value: "random"}
					]
					info:						qsTr("How to select suviving networks.")
				}

				IntegerField
				{
					Layout.leftMargin:			15 * preferencesModel.uiScale
					name:						"candidates"
					text:						qsTr("Candidates")
					defaultValue:				5
					min:						1
					max:						populationSize.value - 1
					enabled:					selectionMethod.value == "tournament"
					info:						qsTr("Number of candidates for tournament selection")
				}

				DropDown
				{
					name:						"crossoverMethod"
					indexDefaultValue:			0
					label:						qsTr("Crossover method")
					values:
						[
						{ label:qsTr("Uniform"), 		value: "uniform"},
						{ label:qsTr("One-point"),		value: "onepoint"},
						{ label:qsTr("Multi-point"),	value: "multipoint"}
					]
					info:						qsTr("How to crossover two candidate networks.")
				}

				DropDown
				{
					name:						"mutationMethod"
					indexDefaultValue:			0
					label:						qsTr("Mutations")
					values:
						[
						{ label:qsTr("Reset"), 		value: "random"},
						{ label:qsTr("Swap"), 		value: "swap"},
						{ label:qsTr("Scramble"), 	value: "scramble"},
						{ label:qsTr("Inversion"), 	value: "inversion"}
					]
					info:						qsTr("How to mutate a network.")
				}

				PercentField
				{
					Layout.leftMargin:			15 * preferencesModel.uiScale
					name:						"mutationRate"
					text:						qsTr("Probability")
					defaultValue:				10
					info:						qsTr("The mutation probability of a random network in each generation.")
				}

				DropDown
				{
					name:						"survivalMethod"
					indexDefaultValue:			0
					label:						qsTr("Survival method")
					values:
						[
						{ label:qsTr("Fitness-based"),	value: "fitness"},
						{ label:qsTr("Age-based"),		value: "age"},
						{ label:qsTr("Random"),			value: "random"}
					]
					info:						qsTr("How to choose which networks survive and die in a generation.")
				}

				CheckBox
				{
					Layout.leftMargin:			15 * preferencesModel.uiScale
					id:							elitism
					name:						"elitism"
					label:						qsTr("Elitism")
					checked:					true
					childrenOnSameRow:			true
					info:						qsTr("Keep top networks from dying out.")

					PercentField
					{
						name:					"elitismProportion"
						defaultValue:			10
						info:					qsTr("Percentage of top networks to keep.")
					}
				}
			}
		}
	}
}