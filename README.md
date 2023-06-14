[![R_build_status](https://github.com/jasp-stats/jaspAudit/workflows/unit-tests/badge.svg)](https://github.com/jasp-stats/jaspAudit/actions)

# The Machine Learning Module <img src='https://github.com/jasp-stats/jaspMachineLearning/raw/master/inst/icons/analysis-ml-ribbon.svg' width='149' height='173' align='right'/>

Explore the relation between variables using data-driven methods for regression, classification, and clustering. The Machine Learning module provides a standardized graphical interface that unifies various `R` packages dedicated to machine learning. It enables training of predictive models, evaluation of their performance on separate holdout data and prediction of new data. This module encompasses an wide range of supervised and unsupervised learning models, and it streamlines the process of parameter optimization for nearly all models. Furthermore, it simplifies the task of dividing data into training, testing, and validation sets by providing a variety of data splitting methods. Finally, the module aims to provide insight into 'black box' machine learning models via model-agnostic exploration and explanation features.

## Blog posts

- 26/04/2022 - [How to Predict with Machine Learning Models in JASP: Classification](https://jasp-stats.org/2022/04/26/how-to-predict-with-machine-learning-models-in-jasp-classification/)
- 19/11/2019 - [How to Train a Machine Learning Model in JASP: Clustering](https://jasp-stats.org/2019/11/19/how-to-train-a-machine-learning-model-in-jasp-clustering/)
- 07/10/2019 - [How to Train a Machine Learning Model in JASP: Classification](https://jasp-stats.org/2019/10/07/how-to-train-a-machine-learning-model-in-jasp-classification/)

## Module

The organization of the analyses within the Machine Learning module in JASP is as follows:

```
--- Machine Learning
    -- Regression
       - Boosting
       - Decision Tree
       - K-Nearest Neighbors
       - Neural Network
       - Random Forest
       - Regularized Linear
       - Support Vector Machine
    -- Classification
       - Boosting
       - Decision Tree
       - K-Nearest Neighbors
       - Linear Discriminant
       - Neural Network
       - Random Forest
       - Support Vector Machine
    -- Clustering
       - Density-Based
       - Fuzzy C-Means
       - Hierarchical
       - Neighborhood-Based
       - Random Forest
    -- Prediction
```

### Translations

The JASP translation project is located at the following [link](https://hosted.weblate.org/projects/jasp/). The Machine Learning module is referred to as `jaspMachineLearning` and consists of two distinct components: `jaspMachineLearning-QML` and `jaspMachineLearning-R`, both of which require translation. The participation of any individual who is interested would be highly valued. As demonstrated below, we have made substantial progress in the translation of the Audit module.

| Interface | Results |
| :---: | :---: |
| [![image](https://hosted.weblate.org/widgets/jasp/-/jaspmachinelearning-qml/multi-auto.svg)](https://hosted.weblate.org/engage/jasp/) | [![image](https://hosted.weblate.org/widgets/jasp/-/jaspmachinelearning-r/multi-auto.svg)](https://hosted.weblate.org/engage/jasp/) |
