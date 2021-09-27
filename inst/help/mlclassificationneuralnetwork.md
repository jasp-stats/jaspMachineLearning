Neural Network Classification
==========================

Feedforward neural networks are predictive algorithms inspired by the biological neural networks that constitute brains. A neuron (node) that receives a signal then processes it and can send signals to neurons connected to it. The signal at a node is a real number, and the output of each node is computed by sending the signal trough the activation function. The number of nodes in the network is intrinsincly linked to model complexity, as high numbers increase the flexibility of the model.

### Assumptions
- The target is a nominal or ordinal variable. 
- The predictor variables consist of continuous variables.

### Input 
-------
#### Assignment Box 
- Target: In this box, the variable that needs to be predicted should be entered. 
- Predictors: In this box the variables that provide information about the target variable should be entered. 

#### Tables  
- Confusion matrix: Displays a table that shows the observed classes against the predicted classes. Used to assess model accuracy.
- Class proportions: Displays a table that shows the proportions of each class in the data set, training (and validaton), and test set.
- Evaluation metrics: Shows commonly used classification evaluation metrics like precision, recall, the F1-score, support and AUC (area under the ROC curve).
- Network weights: Shows the connections in the neural network together with their weights.

#### Plots
- Data split: Shows how the data is split into training (and validation), and test set.
- Classification accuracy: Plots the number of nearest neighbors against the classification accuracy of the model. Accuracy is assessed for the training (and validation) set.
- ROC curves: Displays ROC curves for each class predicted against all other classes.
- Andrews curves: Is a way to visualize structure in high-dimensional data. Lines that cluster are observations that are more alike. 
- Network structure: Creates a plot that visualizes the structure (nodes and edges) of the network.
- Decision boundary matrix: Creates a *n* x *n* plot that visualizes how every observation would be classified if predicted through the current model. Boundaries between classes are visualized. Can only be made for numeric predictors.

### Data Split Preferences
#### Holdout Test Data
- Sample *x*% of all data: Choose a percentage to randomly sample from your data to derive prediction error. Generates an internal indicator variable that indicates whether the observation is included (1) or excluded (0) from the test set.
- Add generated indicator to data: Add the generated test set indicator from the option above to your data set. Requires a column name.
- Test set indicator: Use an indicator variable to select data for the test set. This indicator should be a column in your data that consists of only 0 (excluded from the test set) and 1 (included in the test set). The data will then be split into a training (and validation if requested) set (0), and a test set (1) according to your indicator.

### Training Parameters 
#### Algorithmic Settings
- Activation function: Sets the activation function for the signal in each hidden layer. 
- Algorithm: Sets the algorithm for the network training. The backpropagation option is standard for training neural networks, but other options are `rprop+` (default) for resilient backpropagation with backtracing, `rprop-` for resilient backpropagation without backtracing, `gprop-sag` for the globally convergent algorithm that modifies the learning rate associated with the smallest absolute gradient, or `gprop-slr` for the globally convergent algorithm that modifies the learning rate associated with the smallest learning rate itself.
- Learning rate: The learning rate used by the backpropagation algorithm.
- Stopping criteria loss function: The threshold for the partial derivatives of the error function as stopping criteria.
- Max. training repetitions: The maximum number of repetitions used in training the network.
- Scale variables: Scales the continuous variables. Standardization ensures that values of variables from different scales range into a specific similar scale. As a result, standardizing provides numerical stability, which improves the clustering output. JASP uses the Z-score standardization of a mean of 0 and a standard deviation of 1. This option is selected by default.
- Set seed: Gives the option to set a seed for your analysis. Setting a seed will exclude random processes influencing an analysis. For example, setting a seed makes it possible to re-run analyses with the same data splits.

#### Network Structure
- Nodes: Specify the nodes in each hidden layer of the neural network.

#### Add Predicted Classes to Data
Generates a new column in your dataset with the class labels of your classification result. This gives you the option to inspect, classify, or predict the generated class labels.

### Output
-------

#### Neural Network Classification Model Table
- The first column shows the number of hidden layers.
- Nodes: The total number of nodes in the network.
- n(Train): The number of observations in the training set.
- n(Test): The number of observations in the test set.
- Test set accuracy: The classification accuracy on the test set.

#### Evaluation Metrics
- Precision: The ratio of correct positive predictions to the total predicted positives.
- Recall: Ratio of correct positive predictions to the total positive observations.
- F1 Score: The harmonic mean of the precision and recall scores.
- Support: The number of class observations in the test set.
- AUC: Area under the ROC curve. Every class is predicted against all other classes. See also ROC curves.

#### Network Weights
- Node: The name of the sending or receiving node.
- Layer: The layer in which the sending or receiving node is residing.
- Weight: The weight assigned to the connection between the sending and receiving nodes.

### References
-------
- James, G., Witten, D., Hastie, T., & Tibshirani, R. (2013). An introduction to statistical learning. Springer New York.

### R-packages 
--- 
- neuralnet
- ROCR