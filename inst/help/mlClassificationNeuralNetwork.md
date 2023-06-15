Neural Network Classification
===

Feedforward neural networks are predictive algorithms inspired by the biological neural networks that constitute brains. A neuron (node) that receives a signal then processes it and can send signals to neurons connected to it. The signal at a node is a real number, and the output of each node is computed by sending the signal trough the activation function. The number of layers and nodes in the network is intrinsincly linked to model complexity, as high numbers increase the flexibility of the model.

### Assumptions
- The target is a nominal or ordinal variable. 
- The feature variables consist of continuous variables.

### Input 
-------
#### Assignment Box 
- Target: In this box, the variable that needs to be predicted should be entered. 
- Features: In this box the variables that provide information about the target variable should be entered. 

#### Tables  
- Confusion matrix: Displays a table that shows the observed classes against the predicted classes. Used to assess model accuracy.
- Class proportions: Displays a table that shows the proportions of each class in the data set, training (and validaton), and test set.
- Model performance: Shows commonly used classification evaluation metrics like precision, recall, the F1-score, support and AUC (area under the ROC curve).
- Feature importance: Shows the available feature importance metrics for the fitted model.
- Explain predictions: Shows the decomposition of the model’s prediction into contributions that can be attributed to different explanatory variables.
- Network weights: Shows the connections in the neural network together with their weights.

#### Plots
- Data split: Shows how the data is split into training (and validation), and test set.
- Classification accuracy: Plots the average classification accuracy of the population of neural networks against the number of generations in the evoluationary optimization algorithm. Accuracy is assessed for the training (and validation) set.
- ROC curves: Displays ROC curves for each class predicted against all other classes.
- Andrews curves: Visualizes structure in high-dimensional data. Lines that cluster are observations that are more alike.
- Network structure: Creates a plot that visualizes the structure (nodes and edges) of the network.
- Decision boundary matrix: Creates an *n* x *n* plot that visualizes how every observation would be classified if predicted through the current model. Boundaries between classes are visualized. Can only be made for numeric features.

### Data Split Preferences
#### Holdout Test Data
- Sample *x*% of all data: Choose a percentage (*x*%) to randomly sample from your data to derive prediction error. Generates an internal indicator variable that indicates whether the observation is included (1) or excluded (0) from the test set.
- Add generated indicator to data: Add the generated test set indicator from the option above to your data set. Requires a column name.
- Test set indicator: Use an indicator variable to select data for the test set. This indicator should be a column in your data that consists of only 0 (excluded from the test set) and 1 (included in the test set). The data will then be split into a training (and validation if requested) set (0), and a test set (1) according to your indicator.

#### Training and Validation Data
- Sample *x*% for validation data: Randomly sample a percentage from the remaining data (after selecting the test set).

### Training Parameters 
#### Algorithmic Settings
- Activation function: Sets the activation function for the signal in each hidden layer. Available options are:
  - linear: *f(x) = x*
  - Binary: *f(x) = 0 if x < 0, 1 if x > 0
  - Logistic sigmoid: *f(x) = 1 / (1 + e^(-x))*
  - Sine: *f(x) = sin(x)*
  - Cosine: *f(x) = cos(x)*
  - Inverse tangent: *f(x) = arctan(x)*
  - Hyperbolic tangent: *f(x) = tanh(x)*
  - ReLU: *f(x) =  0 if x < 0, x if x > 0*
  - Softplus: *f(x) = log(1 + e^x)*
  - Softsign: *f(x) = x / (abs(x) + 1)*
  - ELU: *f(x) = e^x - 1 if x <= 0, x if x > 0*
  - LReLU: *f(x) = 0.01 * x if x < 0, x if x > 0*
  - SiLU: *f(x) = x / (1 + e^(-x))*
  - Mish: *f(x) = x * tanh(log(1 + e^x))*
  - Gaussian: *f(x) = e * (-x^2)*
  - GeLU: *f(x) = 0.5 * x * (1 + tanh(sqrt(2 / pi) * (x + 0.044715 * x^3)))*
- Algorithm: Sets the algorithm for the network training. The backpropagation option is standard for training neural networks, but other options are `rprop+` (default) for resilient backpropagation with backtracing, `rprop-` for resilient backpropagation without backtracing, `gprop-sag` for the globally convergent algorithm that modifies the learning rate associated with the smallest absolute gradient, or `gprop-slr` for the globally convergent algorithm that modifies the learning rate associated with the smallest learning rate itself.
- Learning rate: The learning rate used by the backpropagation algorithm.
- Stopping criteria loss function: The threshold for the partial derivatives of the error function as stopping criteria.
- Max. training repetitions: The maximum number of repetitions used in training the network.
- Scale features: Standardizes the continuous features in the dataset. Standardization ensures that values of features from different scales range into a specific similar scale. As a result, standardizing provides numerical stability. JASP uses the Z-score standardization of a mean of 0 and a standard deviation of 1. This option is selected by default.
- Set seed: Gives the option to set a seed for your analysis. Setting a seed will exclude random processes influencing an analysis. For example, setting a seed makes it possible to re-run analyses with the same data splits.

#### Network Topology
- Manual: Specify the nodes in each hidden layer of the neural network.
- Optimized: Optimize the topology of the network using a genetic algorithm.

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
