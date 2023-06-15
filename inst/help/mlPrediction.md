Prediction
===

### Input

#### Trained Model
This field can load a trained machine learning model. Note that the model must be created in JASP and be saved with the extension *.jaspML*.

#### Assignment Box 
- Features: In this box, the features that need to be considered for the predictions should be entered.

#### Algorithmic Setting
- Scale features: Standardizes the continuous features. Standardization ensures that values of variables from different scales range into a specific similar scale. As a result, standardizing provides numerical stability, which improves the clustering output. JASP uses the Z-score standardization of a mean of 0 and a standard deviation of 1. This option is selected by default.

#### Tables:
- Predictions for new data: Displays the predicted outcomes for the data.
  - Add features: Adds the values of the features to the table.

### Export Results
- Add predicted outcomes to data: Creates a new variable in the data set that contains the predicted values for the data.

### Output

#### Summary Table
- Method: The method used in the model.
- Specific informtion per model.
- n(Train): Number of observations in training data.
- n(New): Number of observations in new data.

#### Predictions for New Data
- Row: The row number.
- Predicted: The predicted value / label.
