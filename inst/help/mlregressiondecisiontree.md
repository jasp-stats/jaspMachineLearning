Decision Tree Regression
===

Decision Trees is a supervised learning algorithm that uses a decision tree as a predictive model to go from observations about an item (represented in the roots of the tree) to conclusions about the item's target value (represented in the endpoints of the tree).

### Assumptions
- The target variable is a continuous variable.
- The predictor variables consist of continuous, nominal, or ordinal variables.

### Input 
-------
#### Assignment Box 
- Target: In this box, the variable that needs to be predicted should be entered. 
- Features: In this box, the variables that provide information about the target variable should be entered. 

#### Tables  
- Evaluation metrics: Shows commonly used classification evaluation metrics like mean squared error (MSE), root mean squared error (RMSE) and R<sup>2</sup>.
- Splits in tree: Shows the split variables, their split point, and the number of observations (which are not missing and are of positive weight) sent left or right by the split. It also shows the improvement in deviance given by this split.
- Variable importance: Shows the relative importance of the features.

#### Plots
- Data split: Shows how the data is split into training (and validation), and test set.
- Mean squared error: Plots the number of nearest neighbors against the MSE of the model. Accuracy is assessed for the training (and validation) set.
- Decision tree: Creates a plot that visualizes the decision tree and its leafs.
- Predictive performance: Shows the selected test set observations against their predicted values.

### Data Split Preferences
#### Holdout Test Data
- Sample *x*% of all data: Choose a percentage to randomly sample from your data to derive prediction error. Generates an internal indicator variable that indicates whether the observation is included (1) or excluded (0) from the test set.
- Add generated indicator to data: Add the generated test set indicator from the option above to your data set. Requires a column name.
- Test set indicator: Use an indicator variable to select data for the test set. This indicator should be a column in your data that consists of only 0 (excluded from the test set) and 1 (included in the test set). The data will then be split into a training (and validation if requested) set (0), and a test set (1) according to your indicator.

### Training Parameters 
#### Algorithmic Settings
- Min. observations per split: The minimum number of observations that must exist in a node in order for a split to be attempted.
- Min. observations in terminal: The minimum number of observations in any terminal node.
- Max. interaction depth: Set the maximum depth of any node of the final tree.
- Complexity parameter: Any split that does not decrease the overall lack of fit by a factor of this parameter is not attempted.
- Scale variables: Scales the continuous variables. Standardization ensures that values of variables from different scales range into a specific similar scale. As a result, standardizing provides numerical stability, which improves the clustering output. JASP uses the Z-score standardization of a mean of 0 and a standard deviation of 1. This option is selected by default.
- Set seed: Gives the option to set a seed for your analysis. Setting a seed will exclude random processes influencing an analysis. For example, setting a seed makes it possible to re-run analyses with the same data splits.

#### Add Predicted Values to Data
Generates a new column in your dataset with the values of your regression result. This gives you the option to inspect, cluster, or predict the generated values.

### Output
-------

#### Decision Tree Regression Model Table
- The first column shows the number of splits in the tree.
- n(Train): The number of observations in the training set.
- n(Test): The number of observations in the test set.
- Test set MSE: The MSE on the test set.

#### Evaluation Metrics
- MSE: The mean squared error of the model.
- RMSE: The root mean squared error of the model.
- MAE / MAD: The mean absolute error of the model.
- MAPE: The mean absolute percentage error of the model.
- R<sup>2</sup>: The proportion of the variance for a dependent variable that's explained by an independent variable or variables.

### References
-------
- James, G., Witten, D., Hastie, T., & Tibshirani, R. (2013). An introduction to statistical learning. Springer New York.

### R-packages 
--- 
- rpart
