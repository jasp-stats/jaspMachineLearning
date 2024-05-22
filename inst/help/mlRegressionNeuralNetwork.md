Neural Network Regression
===

Feedforward neural networks are predictive algorithms inspired by the biological neural networks that constitute brains. A neuron (node) that receives a signal then processes it and can send signals to neurons connected to it. The signal at a node is a real number, and the output of each node is computed by sending the signal trough the activation function. The number of layers and nodes in the network is intrinsincly linked to model complexity, as high numbers increase the flexibility of the model.

### Assumptions
- The target variable is a continuous variable.
- The feature variables consist of continuous.

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
- Network weights: Shows the connections in the neural network together with their weights.

#### Plots
- Data split: Shows how the data is split into training (and validation), and test set.
- Predictive performance: Shows the selected test set observations against their predicted values.
- Network structure: Creates a plot that visualizes the structure (nodes and edges) of the network.

### Data Split Preferences
#### Holdout Test Data
- Sample *x*% of all data: Choose a percentage to randomly sample from your data to derive prediction error. Generates an internal indicator variable that indicates whether the observation is included (1) or excluded (0) from the test set.
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

#### Add Predicted Values to Data
Generates a new column in your dataset with the values of your regression result. This gives you the option to inspect, cluster, or predict the generated values.

### Output
-------

#### Neural Network Regression Model Table
- The first column shows the number of hidden layers.
- Nodes: The total number of nodes in the network.
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

#### Network Weights
- Node: The name of the sending or receiving node.
- Layer: The layer in which the sending or receiving node is residing.
- Weight: The weight assigned to the connection between the sending and receiving nodes.

### References
-------
- James, G., Witten, D., Hastie, T., & Tibshirani, R. (2013). An introduction to statistical learning. Springer New York.
- Gosiewska, A. & Biecek, P. (2019). Do Not Trust Additive Explanations. <i>ArXiv</i>. https://doi.org/10.48550/arXiv.1903.11420

### R-packages 
--- 
- neuralnet