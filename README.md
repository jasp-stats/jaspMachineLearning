# The Machine Learning Module

Explore the relation between variables using data-driven methods for regression, classification, and clustering.

## Core Functionality

The Machine Learning module bundles several `R` packages for machine learning into a general interface for training a predictive model and assessing its performance on holdout data. The module offers a variety of supervised and unsupervised learning methods whose parameters can be adjusted. Moreover, the module facilitates different data splitting methods for dividing data into a training, testing, and validation set.

## Module Structure

The analyses in the Machine Learning module are structured in JASP in the following way:

```
--- Machine Learning
    -- Regression
       - Boosting
       - K-Nearest Neighbors
       - Neural Network
       - Random Forest
       - Regularized Linear
    -- Classification
       - Boosting
       - K-Nearest Neighbors
       - Linear Discriminant
       - Neural Network
       - Random Forest
    -- Clustering
       - Density-Based
       - Fuzzy C-Means
       - Hierarchical
       - K-Means
       - Random Forest
```