K-Nearest Neighbors Regression
===

K-nearest neighbors is a method of regression that looks at the *k* number of feature observations that are most similar to new observations to make a prediction for their values. The number of nearest neighbors is intrinsincly linked to model complexity, as small numbers increase the flexibility of the model.

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
- Explain predictions: Shows the decomposition of the model’s prediction into contributions that can be attributed to different explanatory variables. This feature uses the breakdown algoritm from the `ibreakdown` R package. For more details about this method, see Gosiewska and Biecek (2019).

#### Plots
- Data split: Shows how the data is split into training (and validation), and test set.
- Weight function: Shows how the weights are assigned as a function of the distance.
- Mean squared error: Plots the number of nearest neighbors against the MSE of the model. Accuracy is assessed for the training (and validation) set.
- Predictive performance: Shows the selected test set observations against their predicted values.

### Data Split Preferences
#### Holdout Test Data
- Sample *x*% of all data: Choose a percentage to randomly sample from your data to derive prediction error. Generates an internal indicator variable that indicates whether the observation is included (1) or excluded (0) from the test set.
- Add generated indicator to data: Add the generated test set indicator from the option above to your data set. Requires a column name.
- Test set indicator: Use an indicator variable to select data for the test set. This indicator should be a column in your data that consists of only 0 (excluded from the test set) and 1 (included in the test set). The data will then be split into a training (and validation if requested) set (0), and a test set (1) according to your indicator.

#### Training and Validation Data
- Sample *x*% for validation data: Randomly sample a percentage from the remaining data (after selecting the test set).
- K-fold with *k* folds: Partition the remaining data in *k* parts.
- Leave-one-out: Partition the remaining data in *n* parts.

### Training Parameters 
#### Algorithmic Settings
- Weights: Sets the weighting scheme for the nearest neighbors. The default rectangular option results in standard knn, while the other options expand the algorithm by weighing the nearest neighbors. See also the kknn package.
- Distance: The distance metric to be used when determining the similarity between nearest neighbors. Can be either Euclidean or Manhattan distance.
- Scale features: Standardizes the continuous features in the dataset. Standardization ensures that values of features from different scales range into a specific similar scale. As a result, standardizing provides numerical stability. JASP uses the Z-score standardization of a mean of 0 and a standard deviation of 1. This option is selected by default.
- Set seed: Gives the option to set a seed for your analysis. Setting a seed will exclude random processes influencing an analysis. For example, setting a seed makes it possible to re-run analyses with the same data splits.

#### Number of Nearest Neighbors
- Fixed: Enables you to use a user-specified number of nearest neighbors. 
- Optimized: Enables you to optimize the prediction error on a validation data set with respect to the number of nearest neighbors. 
- Max. number of nearest neighbors: Sets the maximum number of possible nearest neighbors to be considered. At default, this is set to 10.

#### Add Predicted Values to Data
Generates a new column in your dataset with the values of your regression result. This gives you the option to inspect, cluster, or predict the generated values.

### Output
-------

#### K-Nearest Neighbors Regression Model Table
- The first column shows the number of nearest neighbors.
- Weights: The weighting scheme.
- Distance: The distance used.
- n(Train): The number of observations in the training set.
- n(Validation): The number of observations in the validation set (enabled when model is optimized).
- n(Test): The number of observations in the test set.
- Validation MSE: The MSE on the validation set (enabled when model is optimized).
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
- Gosiewska, A. & Biecek, P. (2019). Do Not Trust Additive Explanations. <i>ArXiv</i>. https://doi.org/10.48550/arXiv.1903.11420

### R-packages 
--- 
- kknn

### Example 
--- 
- For an example data set go to `Open` --> `Data Library` --> `Machine Learning` --> `Student Grades`.  

