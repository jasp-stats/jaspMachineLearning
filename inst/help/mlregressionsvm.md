Support Vector Machine Regression
===

Support Vector Machines is a supervised learning algorithm that maps training examples to points in space so as to maximise the width of the gap between the two categories. New examples are then mapped into that same space and predicted to belong to a category based on which side of the gap they fall.

### Assumptions
- The target variable is a continuous variable.
- The feature variables consist of continuous, nominal, or ordinal variables.

### Input 
-------
#### Assignment Box 
- Target: In this box, the variable that needs to be predicted should be entered. 
- Features: In this box, the variables that provide information about the target variable should be entered. 

#### Tables  
- Model performance: Shows commonly used classification evaluation metrics like mean squared error (MSE), root mean squared error (RMSE) and R<sup>2</sup>.
- Feature importance: Shows the available feature importance metrics for the fitted model.
  - Permutations: Sets the number of permutations on which the mean dropout loss is based.
- Explain predictions: Shows the decomposition of the model’s prediction into contributions that can be attributed to different explanatory variables.
- Support vectors: Shows a table containing the data (points) indicated as support vectors by the algorithm.

#### Plots
- Data split: Shows how the data is split into training (and validation), and test set.
- Mean squared error: Plots the number of nearest neighbors against the MSE of the model. Accuracy is assessed for the training (and validation) set.
- Predictive performance: Shows the selected test set observations against their predicted values.

### Data Split Preferences
#### Holdout Test Data
- Sample *x*% of all data: Choose a percentage to randomly sample from your data to derive prediction error. Generates an internal indicator variable that indicates whether the observation is included (1) or excluded (0) from the test set.
- Add generated indicator to data: Add the generated test set indicator from the option above to your data set. Requires a column name.
- Test set indicator: Use an indicator variable to select data for the test set. This indicator should be a column in your data that consists of only 0 (excluded from the test set) and 1 (included in the test set). The data will then be split into a training (and validation if requested) set (0), and a test set (1) according to your indicator.

### Training Parameters 
#### Algorithmic Settings
- Kernel: The kernel used in training and predicting. Possible kernels are 'linear', 'radial', 'polynomial', and 'sigmoid'.
- Cost of constraints violation: the 'C'-constant of the regularization term.
- Tolerance of information criterion: The tolerance of termination criterion.
- Epsilon: The epsilon parameter in the insensitive-loss function.
- Scale features: Standardizes the continuous features in the dataset. Standardization ensures that values of features from different scales range into a specific similar scale. As a result, standardizing provides numerical stability. JASP uses the Z-score standardization of a mean of 0 and a standard deviation of 1. This option is selected by default.
- Set seed: Gives the option to set a seed for your analysis. Setting a seed will exclude random processes influencing an analysis. For example, setting a seed makes it possible to re-run analyses with the same data splits.

#### Add Predicted Values to Data
Generates a new column in your dataset with the values of your regression result. This gives you the option to inspect, cluster, or predict the generated values.

### Output
-------

#### Support Vector Machine Regression Model Table
- The first column shows the number of support vectors.
- n(Train): The number of observations in the training set.
- n(Test): The number of observations in the test set.
- Test set MSE: The MSE on the test set.

#### Evaluation Metrics
- MSE: The mean squared error of the model.
- MSE(scaled): The mean squared error calculated using the scaled target variable and the scaled predictions.
- RMSE: The root mean squared error of the model.
- MAE / MAD: The mean absolute error of the model.
- MAPE: The mean absolute percentage error of the model.
- R<sup>2</sup>: The proportion of the variance for a dependent variable that's explained by an independent variable or variables.

### References
-------
- James, G., Witten, D., Hastie, T., & Tibshirani, R. (2013). An introduction to statistical learning. Springer New York.

### R-packages 
--- 
- e1071
