Linear Discriminant Classification
===

Linear Discriminant Analysis (LDA) is a method of classification that aims to find *p - 1* components that discriminate best between the classes in the target variable. LDA is a linear classifier, meaning that the decision boundaries between classes are linear.

### Assumptions
- The target variable is a nominal or ordinal variable.
- The feature variables consist of continuous, nominal, or ordinal variables.
- Equality of class means: The class means should be equal, can be checked with the corresponding table.
- Equality of covariance matrices: The covariance matrices should be equal, can be checked with the corresponding table.
- Multicollinearity: The classes should not correlate within each other, can be checked with the corresponding table.

### Input 
-------
#### Assignment Box 
- Target: In this box, the variable that needs to be predicted should be entered. 
- Features: In this box, the variables that provide information about the target variable should be entered. 

#### Tables  
- Confusion matrix: Displays a table that shows the observed classes against the predicted classes. Used to assess model accuracy.
- Class proportions: Displays a table that shows the proportions of each class in the data set, training (and validaton), and test set.
- Model performance: Shows commonly used classification evaluation metrics like precision, recall, the F1-score, support and AUC (area under the ROC curve).
- Feature importance: Shows the available feature importance metrics for the fitted model.
  - Permutations: Sets the number of permutations on which the mean dropout loss is based.
- Explain predictions: Shows the decomposition of the model’s prediction into contributions that can be attributed to different explanatory variables. This feature uses the breakdown algoritm from the `ibreakdown` R package. For more details about this method, see Gosiewska and Biecek (2019).
- Coefficients: Shows the coefficients for the linear discriminants. 
- Prior and posterior probabilities: Shows the prior and posterior group probabilities. Prior probabilities are the proportions in the training set.
- Class means training data: Shows the means on every variable for every class in the training data.

#### Plots
- Data split: Shows how the data is split into training (and validation), and test set.
- ROC curves: Displays ROC curves for each class predicted against all other classes.
- Andrews curves: is a way to visualize structure in high-dimensional data. Lines that cluster are observations that are more alike. 
- Linear discriminant matrix: Creates a matrix plot that visualizes the densities on the linear discriminants along with a scatterplot of variables on these discriminants.
- Decision boundary matrix: Creates a *n* x *n* plot that visualizes how every observation would be classified if predicted through the current model. Boundaries between classes are visualized. Can only be made for numeric features.

### Data Split Preferences
#### Holdout Test Data
- Sample *x*% of all data: Choose a percentage to randomly sample from your data to derive prediction error. Generates an internal indicator variable that indicates whether the observation is included (1) or excluded (0) from the test set.
- Add generated indicator to data: Add the generated test set indicator from the option above to your data set. Requires a column name.
- Test set indicator: Use an indicator variable to select data for the test set. This indicator should be a column in your data that consists of only 0 (excluded from the test set) and 1 (included in the test set). The data will then be split into a training (and validation if requested) set (0), and a test set (1) according to your indicator.

### Training Parameters 
#### Algorithmic Settings
- Estimation method: Sets the estimator for the algorithm. Can be one of moment" for standard estimators of the mean and variance "mle", "mve", or "t" for robust estimates based on a t distribution. See also the MASS package.
- Scale features: Standardizes the continuous features in the dataset. Standardization ensures that values of features from different scales range into a specific similar scale. As a result, standardizing provides numerical stability. JASP uses the Z-score standardization of a mean of 0 and a standard deviation of 1. This option is selected by default.
- Set seed: Gives the option to set a seed for your analysis. Setting a seed will exclude random processes influencing an analysis. For example, setting a seed makes it possible to re-run analyses with the same data splits.

#### Add Predicted Classes to Data
Generates a new column in your dataset with the class labels of your classification result. This gives you the option to inspect, classify, or predict the generated class labels.

### Output
-------

#### Linear Discirminant Classification Model Table
- The first column shows the number of linear discriminants.
- Method: The estimation method.
- n(Train): The number of observations in the training set.
- n(Test): The number of observations in the test set.
- Test set accuracy: The classification accuracy on the test set.

#### Evaluation Metrics
- Support: The number of class observations in the test set.
- Accuracy: The ratio of correct predictions to the total predictions.
- Precision (Positive Predictive Value): The ratio of correct positive predictions to the total predicted positives.
- Recall (True Positive Rate): The ratio of correct positive predictions to the total positive classes.
- False Positive Rate: The ratio of incorrect positive predictions to the total negative classes.
- False Discovery Rate: The ratio of incorrect positive predictions to the predicted positive classes.
- F1 Score: The harmonic mean of the precision and recall scores.
- Matthews Correlation coefficient: An alternative to F1 or accuracy that is more reliable for imbalanced datasets.
	see https://bmcgenomics.biomedcentral.com/articles/10.1186/s12864-019-6413-7
- Area Under Curve (AUC): Area under the ROC curve. See also ROC curves.
- Negative Predictive Value: The ratio of correct negative predictions to the total predicted negatives.
- True Negative Rate: The ratio of correct negative predictions to the total negative classes.
- False Negative Rate: The ratio of incorrect negative predictions to the total positive classes.
- False Omission Rate: The ratio of incorrect negative predictions to the predicted negative classes.
- Threat Score: The ratio of correctly predicted positives to all predicted and true positives.
- Statistical Parity: The ratio of predicted positives to all predictions.

### References
-------
- James, G., Witten, D., Hastie, T., & Tibshirani, R. (2013). An introduction to statistical learning. Springer New York.
- Gosiewska, A. & Biecek, P. (2019). Do Not Trust Additive Explanations. <i>ArXiv</i>. https://doi.org/10.48550/arXiv.1903.11420

### R-packages 
--- 
- MASS
- ROCR

### Example 
---
- For an example data set go to `Open` --> `Data Library` --> `Machine Learning` --> `Wine Types`.
