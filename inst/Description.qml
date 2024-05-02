import QtQuick 		2.12
import JASP.Module 	1.0

Description
{
	title :			qsTr("Machine Learning")
	name :			"jaspMachineLearning"
	description:	qsTr("Explore the relation between variables using data-driven methods for regression, classification, and clustering")
	version			: "0.19.0"
	author:			"JASP Team"
	maintainer:		"JASP Team <info@jasp-stats.org>"
	website:		"www.jasp-stats.org"
	license:		"GPL (>= 2)"
	icon:			"analysis-ml-ribbon.svg"

	GroupTitle
	{
		title:	qsTr("Regression")
		icon: 	"analysis-ml-regression.svg"
	}
	Analysis
	{
		menu:	qsTr("Boosting")
		title:	qsTr("Boosting Regression")
		func:	"mlRegressionBoosting"
	}
	Analysis
	{
		menu:	qsTr("Decision Tree")
		title:	qsTr("Decision Tree Regression")
		func:	"mlRegressionDecisionTree"
	}
	Analysis
	{
		menu:	qsTr("K-Nearest Neighbors")
		title:	qsTr("K-Nearest Neighbors Regression")
		func:	"mlRegressionKnn"
	}
	Analysis
	{
		menu:	qsTr("Linear")
		title:	qsTr("Linear Regression")
		func:	"mlRegressionLinear"
	}
	Analysis
	{
		menu:	qsTr("Neural Network")
		title:	qsTr("Neural Network Regression")
		func:	"mlRegressionNeuralNetwork"
	}
	Analysis
	{
		menu:	qsTr("Random Forest")
		title:	qsTr("Random Forest Regression")
		func:	"mlRegressionRandomForest"
	}
	Analysis
	{
		menu:	qsTr("Regularized Linear")
		title:	qsTr("Regularized Linear Regression")
		func:	"mlRegressionRegularized"
	}
	Analysis
	{
		menu:	qsTr("Support Vector Machine")
		title:	qsTr("Support Vector Machine Regression")
		func:	"mlRegressionSvm"
	}


	GroupTitle
	{
		title:	qsTr("Classification")
		icon: 	"analysis-ml-classification.svg"
	}
	Analysis
	{
		menu:	qsTr("Boosting")
		title:	qsTr("Boosting Classification")
		func:	"mlClassificationBoosting"
	}
	Analysis
	{
		menu:	qsTr("Decision Tree")
		title:	qsTr("Decision Tree Classification")
		func:	"mlClassificationDecisionTree"
	}
	Analysis
	{
		menu:	qsTr("K-Nearest Neighbors")
		title:	qsTr("K-Nearest Neighbors Classification")
		func:	"mlClassificationKnn"
	}
	Analysis
	{
		menu:	qsTr("Linear Discriminant")
		title:	qsTr("Linear Discriminant Classification")
		func: 	"mlClassificationLda"
	}
	Analysis
	{
		menu:	qsTr("Naive Bayes")
		title:	qsTr("Naive Bayes Classification")
		func: 	"mlClassificationNaiveBayes"
	}
	Analysis
	{
		menu:	qsTr("Neural Network")
		title:	qsTr("Neural Network Classification")
		func: 	"mlClassificationNeuralNetwork"
	}
	Analysis
	{
		menu:	qsTr("Random Forest")
		title:	qsTr("Random Forest Classification")
		func:	"mlClassificationRandomForest"
	}
	Analysis
	{
		menu:	qsTr("Support Vector Machine")
		title:	qsTr("Support Vector Machine Classification")
		func:	"mlClassificationSvm"
	}


	GroupTitle
	{
		title:	qsTr("Clustering")
		icon: 	"analysis-ml-clustering.svg"
	}
	Analysis
	{
		menu:	qsTr("Density-Based")
		title:	qsTr("Density-Based Clustering")
		func:	"mlClusteringDensityBased"
	}
	Analysis
	{
		menu:	qsTr("Fuzzy C-Means")
		title:	qsTr("Fuzzy C-Means Clustering")
		func:	"mlClusteringFuzzyCMeans"
	}
	Analysis
	{
		menu:	qsTr("Hierarchical")
		title:	qsTr("Hierarchical Clustering")
		func:	"mlClusteringHierarchical"
	}
	Analysis
	{
		menu:	qsTr("Model-Based")
		title:	qsTr("Model-Based Clustering")
		func:	"mlClusteringModelBased"
	}
	Analysis
	{
		menu:	qsTr("Neighborhood-Based")
		title:	qsTr("Neighborhood-Based Clustering")
		func:	"mlClusteringKMeans"
	}
	Analysis
	{
		menu:	qsTr("Random Forest")
		title:	qsTr("Random Forest Clustering")
		func:	"mlClusteringRandomForest"
	}

	GroupTitle
	{
		title:	qsTr("Prediction")
		icon: 	"analysis-ml-prediction.svg"
	}
	Analysis
	{
		menu:	qsTr("Prediction")
		title:	qsTr("Prediction")
		func:	"mlPrediction"
	}
}
