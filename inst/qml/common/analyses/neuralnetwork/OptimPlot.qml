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

CheckBox
{
	property bool regression: true
	property bool enable: false

	text:		regression ? qsTr("Mean squared error") : qsTr("Classification accuracy")
	name:		"meanSquaredErrorPlot"
	enabled:	enable
	info:		qsTr("For regression, plots the average mean squared error of the population of neural networks against the number of generations in the evoluationary optimization algorithm. For classification, plots the average classification accuracy of the population of neural networks against the number of generations in the evoluationary optimization algorithm. Accuracy is assessed for the training (and validation) set.")
}