Decision Tree Classification
===

Decision Trees is a supervised learning algorithm that uses a decision tree as a predictive model to go from observations about an item (represented in the roots of the tree) to conclusions about the item's target value (represented in the endpoints of the tree).

### Assumptions
- The target is a nominal or ordinal variable. 
- The predictor variables consist of continuous, nominal, or ordinal variables.

### Input 
-------
#### Assignment Box 
- Target: In this box, the variable that needs to be predicted should be entered. 
- Features: In this box, the variables that provide information about the target variable should be entered. 

#### Tables  
- Confusion matrix: Displays a table that shows the observed classes against the predicted classes. Used to assess model accuracy.
- Class proportions: Displays a table that shows the proportions of each class in the data set, training (and validaton), and test set.
- Evaluation metrics: Shows commonly used classification evaluation metrics like precision, recall, the F1-score, support and AUC (area under the ROC curve).
- Splits in tree: Shows the split variables, their split point, and the number of observations (which are not missing and are of positive weight) sent left or right by the split. It also shows the improvement in deviance given by this split.
- Variable importance: Shows the relative importance of the features.

#### Plots
- Data split: Shows how the data is split into training (and validation), and test set.
- ROC curves: Displays ROC curves for each class predicted against all other classes.
- Andrews curves: is a way to visualize structure in high-dimensional data. Lines that cluster are observations that are more alike.
- Decision tree: Creates a plot that visualizes the decision tree and its leafs.
- Decision boundary matrix: Creates a *n* x *n* plot that visualizes how every observation would be classified if predicted through the current model. Boundaries between classes are visualized. Can only be made for numeric features.

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

#### Add Predicted Classes to Data
Generates a new column in your dataset with the class labels of your classification result. This gives you the option to inspect, classify, or predict the generated class labels.

### Output
-------

#### Decision Tree Classification Model Table
- The first column shows the number of splits in the tree.
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

### R-packages 
--- 
- rpart
- ROCR
