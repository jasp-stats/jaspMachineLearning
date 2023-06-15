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
	DropDown
	{
		name:				"actfct"
		indexDefaultValue:	2
		label:				qsTr("Activation function")
		values:
			[
			{ label: qsTr("Linear"), 			value: "linear"},
			{ label: qsTr("Binary"), 			value: "binary"},
			{ label: qsTr("Logistic sigmoid"), 	value: "sigmoid"},
			{ label: qsTr("Sine"), 				value: "sin"},
			{ label: qsTr("Cosine"), 			value: "cosin"},
			{ label: qsTr("Inverse tangent"), 	value: "arctan"},
			{ label: qsTr("Hyperbolic tangent"),value: "tanh"},
			{ label: qsTr("ReLU"), 				value: "relu"},
			{ label: qsTr("Softplus"), 			value: "softplus"},
			{ label: qsTr("Softsign"), 			value: "softsign"},
			{ label: qsTr("ELU"), 				value: "elu"},
			{ label: qsTr("LReLU"),				value: "lrelu"},
			{ label: qsTr("SiLU"), 				value: "silu"},
			{ label: qsTr("Mish"), 				value: "mish"},
			{ label: qsTr("Gaussian"), 			value: "gaussian"},
			{ label: qsTr("GeLU"), 				value: "gelu"}
		]
		info:				qsTr("Sets the activation function for the signal in each hidden layer. Available options are:\n- linear: *f(x) = x*\n- Binary: *f(x) = 0 if x < 0, 1 if x > 0\n- Logistic sigmoid: *f(x) = 1 / (1 + e^(-x))*\n- Sine: *f(x) = sin(x)*\n- Cosine: *f(x) = cos(x)*\n- Inverse tangent: *f(x) = arctan(x)*\n- Hyperbolic tangent: *f(x) = tanh(x)*\n- ReLU: *f(x) =  0 if x < 0, x if x > 0*\n- Softplus: *f(x) = log(1 + e^x)*\n- Softsign: *f(x) = x / (abs(x) + 1)*\n- ELU: *f(x) = e^x - 1 if x <= 0, x if x > 0*\n- LReLU: *f(x) = 0.01 * x if x < 0, x if x > 0*\n- SiLU: *f(x) = x / (1 + e^(-x))*\n- Mish: *f(x) = x * tanh(log(1 + e^x))*\n- Gaussian: *f(x) = e * (-x^2)*\n- GeLU: *f(x) = 0.5 * x * (1 + tanh(sqrt(2 / pi) * (x + 0.044715 * x^3)))*")
	}

	DropDown
	{
		id:					algorithm
		name:				"algorithm"
		indexDefaultValue:	1
		label:				qsTr("Algorithm")
		values:
			[
			{ label: qsTr("Backpropagation"), 	value: "backprop"},
			{ label: qsTr("rprop+"), 			value: "rprop+"},
			{ label: qsTr("rprop-"), 			value: "rprop-"},
			{ label: qsTr("grprop-sag"), 		value: "sag"},
			{ label: qsTr("grprop-slr"), 		value: "slr"}
		]
		info:				qsTr("Sets the algorithm for the network training. The backpropagation option is standard for training neural networks, but other options are `rprop+` (default) for resilient backpropagation with backtracing, `rprop-` for resilient backpropagation without backtracing, `gprop-sag` for the globally convergent algorithm that modifies the learning rate associated with the smallest absolute gradient, or `gprop-slr` for the globally convergent algorithm that modifies the learning rate associated with the smallest learning rate itself.")
	}

	DoubleField
	{
		Layout.leftMargin:	15 * preferencesModel.uiScale
		name:				"learningRate"
		label:				qsTr("Learning rate")
		defaultValue:		0.05
		min:				0
		enabled:			algorithm.value == "backprop"
		info:				qsTr("The learning rate used by the backpropagation algorithm.")
	}

	DropDown
	{
		name:				"lossFunction"
		debug:				true
		indexDefaultValue:	0
		label:				qsTr("Loss function")
		values:
			[
			{ label: qsTr("Sum of squares"),	value: "sumOfSquares"},
			{ label: qsTr("Cross-entropy"),		value: "crossEntropy"}
		]
		info:				qsTr("The loss function used.")
	}

	DoubleField
	{
		name:			"threshold"
		label:			qsTr("Stopping criteria loss function")
		defaultValue:	1
		min:			0
		info:			qsTr("The threshold for the partial derivatives of the error function as stopping criteria.")
	}

	IntegerField
	{
		name:			"maxTrainingRepetitions"
		label:			qsTr("Max. training repetitions")
		defaultValue:	100000
		min:			1
		fieldWidth:		60
		info:			qsTr("The maximum number of repetitions used in training the network.")
	}
}