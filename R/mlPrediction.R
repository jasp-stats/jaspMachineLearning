#
# Copyright (C) 2013-2021 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

mlPrediction <- function(jaspResults, dataset, options, ...) {

  # Preparatory work
  model <- .mlPredictionReadModel(options)
  dataset <- .mlPredictionReadData(options)

  # Check if analysis is ready
  ready <- .mlPredictionReady(model, dataset, options)

  # Create a table summarizing the loaded model
  .mlPredictionModelSummaryTable(model, dataset, options, jaspResults, ready, position = 1)

  # Create a table containing the predicted values and predictors
  .mlPredictionsTable(model, dataset, options, jaspResults, ready, position = 2)

  # Add predicted outcomes to data set
  .mlPredictionsAddPredictions(model, dataset, options, jaspResults, ready)

  # Create the shap table
  .mlTableShap(dataset, options, jaspResults, ready, position = 3, purpose = "prediction", model)
}

is.jaspMachineLearning <- function(x) {
  inherits(x, "jaspMachineLearning")
}

# S3 method to identify the model type
.mlPredictionGetModelType <- function(model) {
  UseMethod(".mlPredictionGetModelType", model)
}
.mlPredictionGetModelType.kknn <- function(model) {
  gettext("K-nearest neighbors")
}
.mlPredictionGetModelType.lda <- function(model) {
  gettext("Linear discriminant")
}
.mlPredictionGetModelType.lm <- function(model) {
  gettext("Linear")
}
.mlPredictionGetModelType.gbm <- function(model) {
  gettext("Boosting")
}
.mlPredictionGetModelType.randomForest <- function(model) {
  gettext("Random forest")
}
.mlPredictionGetModelType.cv.glmnet <- function(model) {
  gettext("Regularized linear regression")
}
.mlPredictionGetModelType.nn <- function(model) {
  gettext("Neural network")
}
.mlPredictionGetModelType.rpart <- function(model) {
  gettext("Decision tree")
}
.mlPredictionGetModelType.svm <- function(model) {
  gettext("Support vector machine")
}
.mlPredictionGetModelType.naiveBayes <- function(model) {
  gettext("Naive Bayes")
}

# S3 method to make predictions using the model
.mlPredictionGetPredictions <- function(model, dataset) {
  UseMethod(".mlPredictionGetPredictions", model)
}
.mlPredictionGetPredictions.kknn <- function(model, dataset) {
  if (inherits(model, "jaspClassification")) {
    as.character(kknn:::predict.train.kknn(model[["predictive"]], dataset))
  } else if (inherits(model, "jaspRegression")) {
    as.numeric(kknn:::predict.train.kknn(model[["predictive"]], dataset))
  }
}
.mlPredictionGetPredictions.lda <- function(model, dataset) {
  as.character(MASS:::predict.lda(model, newdata = dataset)$class)
}
.mlPredictionGetPredictions.lm <- function(model, dataset) {
  as.numeric(predict(model, newdata = dataset))
}
.mlPredictionGetPredictions.gbm <- function(model, dataset) {
  if (inherits(model, "jaspClassification")) {
    tmp <- gbm:::predict.gbm(model, newdata = dataset, n.trees = model[["n.trees"]], type = "response")
    as.character(colnames(tmp)[apply(tmp, 1, which.max)])
  } else if (inherits(model, "jaspRegression")) {
    as.numeric(gbm:::predict.gbm(model, newdata = dataset, n.trees = model[["n.trees"]], type = "response"))
  }
}
.mlPredictionGetPredictions.randomForest <- function(model, dataset) {
  if (inherits(model, "jaspClassification")) {
    as.character(randomForest:::predict.randomForest(model, newdata = dataset))
  } else if (inherits(model, "jaspRegression")) {
    as.numeric(randomForest:::predict.randomForest(model, newdata = dataset))
  }
}
.mlPredictionGetPredictions.cv.glmnet <- function(model, dataset) {
  as.numeric(glmnet:::predict.cv.glmnet(model, newx = data.matrix(dataset)))
}
.mlPredictionGetPredictions.nn <- function(model, dataset) {
  if (inherits(model, "jaspClassification")) {
    as.character(levels(factor(model[["data"]][, 1]))[max.col(neuralnet:::predict.nn(model, newdata = dataset))])
  } else if (inherits(model, "jaspRegression")) {
    as.numeric(neuralnet:::predict.nn(model, newdata = dataset))
  }
}
.mlPredictionGetPredictions.rpart <- function(model, dataset) {
  if (inherits(model, "jaspClassification")) {
    as.character(levels(factor(model[["data"]][, 1]))[max.col(rpart:::predict.rpart(model, newdata = dataset))])
  } else if (inherits(model, "jaspRegression")) {
    as.numeric(rpart:::predict.rpart(model, newdata = dataset))
  }
}
.mlPredictionGetPredictions.svm <- function(model, dataset) {
  if (inherits(model, "jaspClassification")) {
    as.character(levels(factor(model[["data"]][, 1]))[e1071:::predict.svm(model, newdata = dataset)])
  } else if (inherits(model, "jaspRegression")) {
    as.numeric(e1071:::predict.svm(model, newdata = dataset))
  }
}
.mlPredictionGetPredictions.naiveBayes <- function(model, dataset) {
  as.character(e1071:::predict.naiveBayes(model, newdata = dataset, type = "class"))
}

# S3 method to make find out number of observations in training data
.mlPredictionGetTrainingN <- function(model) {
  UseMethod(".mlPredictionGetTrainingN", model)
}
.mlPredictionGetTrainingN.kknn <- function(model) {
  nrow(model[["predictive"]]$data)
}
.mlPredictionGetTrainingN.lda <- function(model) {
  model[["N"]]
}
.mlPredictionGetTrainingN.lm <- function(model) {
  nrow(model[["model"]])
}
.mlPredictionGetTrainingN.gbm <- function(model) {
  model[["nTrain"]]
}
.mlPredictionGetTrainingN.randomForest <- function(model) {
  length(model[["y"]])
}
.mlPredictionGetTrainingN.cv.glmnet <- function(model) {
  model[["glmnet.fit"]][["nobs"]]
}
.mlPredictionGetTrainingN.nn <- function(model) {
  nrow(model[["data"]])
}
.mlPredictionGetTrainingN.rpart <- function(model) {
  nrow(model[["x"]])
}
.mlPredictionGetTrainingN.svm <- function(model) {
  length(model[["fitted"]])
}
.mlPredictionGetTrainingN.naiveBayes <- function(model) {
  nrow(model[["data"]])
}

# S3 method to decode the model variables in the result object
# so that they can be matched to variables in the prediction analysis
.decodeJaspMLobject <- function(model) {
  UseMethod(".decodeJaspMLobject", model)
}
.decodeJaspMLobject.kknn <- function(model) {
  formula <- formula(paste(decodeColNames(as.character(model$terms)[2]), "~", paste0(decodeColNames(strsplit(as.character(model$terms)[3], split = " + ", fixed = TRUE)[[1]]), collapse = " + ")))
  model[["predictive"]]$terms <- stats::terms(formula)
  colnames(model[["predictive"]]$data) <- decodeColNames(colnames(model[["predictive"]]$data))
  return(model)
}
.decodeJaspMLobject.lda <- function(model) {
  formula <- formula(paste(decodeColNames(as.character(model$terms)[2]), "~", paste0(decodeColNames(strsplit(as.character(model$terms)[3], split = " + ", fixed = TRUE)[[1]]), collapse = " + ")))
  model$terms <- stats::terms(formula)
  return(model)
}
.decodeJaspMLobject.lm <- function(model) {
  formula <- formula(paste(decodeColNames(as.character(model$terms)[2]), "~", paste0(decodeColNames(strsplit(as.character(model$terms)[3], split = " + ", fixed = TRUE)[[1]]), collapse = " + ")))
  model$terms <- stats::terms(formula)
  return(model)
}
.decodeJaspMLobject.gbm <- function(model) {
  model[["var.names"]] <- decodeColNames(model[["var.names"]])
  return(model)
}
.decodeJaspMLobject.randomForest <- function(model) {
  rownames(model$importance) <- decodeColNames(rownames(model$importance))
  names(model$forest$xlevels) <- decodeColNames(names(model$forest$xlevels))
  formula <- formula(paste("DOESNOTMATTER", "~", paste0(decodeColNames(names(model$forest$xlevels)), collapse = " + ")))
  model$terms <- stats::terms(formula)
  class(model) <- c(class(model), "randomForest.formula")
  return(model)
}
.decodeJaspMLobject.cv.glmnet <- function(model) {
  rownames(model[["glmnet.fit"]][["beta"]]) <- decodeColNames(rownames(model[["glmnet.fit"]][["beta"]]))
  return(model)
}
.decodeJaspMLobject.nn <- function(model) {
  model[["model.list"]]$variables <- decodeColNames(model[["model.list"]]$variables)
  return(model)
}
.decodeJaspMLobject.rpart <- function(model) {
  formula <- formula(paste(decodeColNames(as.character(model$terms)[2]), "~", paste0(decodeColNames(strsplit(as.character(model$terms)[3], split = " + ", fixed = TRUE)[[1]]), collapse = " + ")))
  model$terms <- stats::terms(formula)
  model$frame$var <- decodeColNames(model$frame$var)
  rownames(model$splits) <- decodeColNames(rownames(model$splits))
  return(model)
}
.decodeJaspMLobject.svm <- function(model) {
  formula <- formula(paste(decodeColNames(as.character(model$terms)[2]), "~ 0 +", paste0(decodeColNames(strsplit(as.character(model$terms)[3], split = " + ", fixed = TRUE)[[1]]), collapse = " + ")))
  model$terms <- stats::terms(formula)
  return(model)
}
.decodeJaspMLobject.naiveBayes <- function(model) {
  names(model[["isnumeric"]]) <- decodeColNames(names(model[["isnumeric"]]))
  names(model[["tables"]]) <- decodeColNames(names(model[["tables"]]))
  return(model)
}

.mlPredictionReadModel <- function(options) {
  if (options[["trainedModelFilePath"]] != "") {
    model <- try({
      readRDS(options[["trainedModelFilePath"]])
    })
    if (!is.jaspMachineLearning(model)) {
      jaspBase:::.quitAnalysis(gettext("Error: The trained model is not created in JASP."))
    }
    if (!(any(c("kknn", "lda", "gbm", "randomForest", "cv.glmnet", "nn", "rpart", "svm", "lm", "naiveBayes") %in% class(model)))) {
      jaspBase:::.quitAnalysis(gettextf("The trained model (type: %1$s) is currently not supported in JASP.", paste(class(model), collapse = ", ")))
    }
    if (model[["jaspVersion"]] != .baseCitation) {
      jaspBase:::.quitAnalysis(gettext("Error: The trained model is created using a different version of JASP."))
    }
  } else {
    model <- NULL
  }
  return(model)
}

# also define methods for other objects
.mlPredictionReady <- function(model, dataset, options) {
  if (!is.null(model)) {
    modelVars <- model[["jaspVars"]]
    presentVars <- decodeColNames(colnames(dataset))
    ready <- all(modelVars %in% presentVars)
  } else {
    ready <- FALSE
  }
  return(ready)
}

.mlPredictionReadData <- function(options) {
  dataset <- .readDataSetToEnd(columns = options[["predictors"]], exclude.na.listwise = options[["predictors"]])
  if (options[["scaleVariables"]] && length(unlist(options[["predictors"]])) > 0) {
    dataset <- .scaleNumericData(dataset)
  }
  return(dataset)
}

.mlPredictionsState <- function(model, dataset, options, jaspResults, ready) {
  if (!is.null(jaspResults[["predictions"]])) {
    return(jaspResults[["predictions"]]$object)
  } else {
    if (ready) {
      colnames(dataset) <- decodeColNames(colnames(dataset))
      jaspResults[["predictions"]] <- createJaspState(.mlPredictionGetPredictions(model, dataset))
      jaspResults[["predictions"]]$dependOn(options = c("loadPath", "predictors", "scaleVariables"))
      return(jaspResults[["predictions"]]$object)
    } else {
      return(NULL)
    }
  }
}

.mlPredictionModelSummaryTable <- function(model, dataset, options, jaspResults, ready, position) {
  if (is.null(model)) {
    table <- createJaspTable(gettext("Loaded Model"))
  } else {
    if (inherits(model, "jaspClassification")) {
      purpose <- gettext("Classification")
    }
    if (inherits(model, "jaspRegression")) {
      purpose <- gettext("Regression")
    }
    table <- createJaspTable(gettextf("Loaded Model: %1$s", purpose))
  }
  table$dependOn(options = c("predictors", "trainedModelFilePath"))
  table$position <- position
  table$addColumnInfo(name = "model", title = gettext("Method"), type = "string")
  jaspResults[["modelSummaryTable"]] <- table
  if (is.null(model)) {
    return()
  }
  modelVars <- model[["jaspVars"]]
  presentVars <- decodeColNames(colnames(dataset))
  if (!all(modelVars %in% presentVars)) {
    missingVars <- modelVars[which(!(modelVars %in% presentVars))]
    table$addFootnote(gettextf("The trained model is not applied because the the following features are missing: <i>%1$s</i>.", paste0(missingVars, collapse = ", ")))
  }
  if (!all(presentVars %in% modelVars)) {
    unusedVars <- presentVars[which(!(presentVars %in% modelVars))]
    table$addFootnote(gettextf("The following features are unused because they are not a feature variable in the trained model: <i>%1$s</i>.", paste0(unusedVars, collapse = ", ")))
  }
  if (inherits(model, "kknn")) {
    table$addColumnInfo(name = "nn", title = gettext("Nearest Neighbors"), type = "integer")
  } else if (inherits(model, "lda")) {
    table$addColumnInfo(name = "ld", title = gettext("Linear Discriminants"), type = "integer")
  } else if (inherits(model, "gbm")) {
    table$addColumnInfo(name = "trees", title = gettext("Trees"), type = "integer")
    table$addColumnInfo(name = "shrinkage", title = gettext("Shrinkage"), type = "number")
  } else if (inherits(model, "randomForest")) {
    table$addColumnInfo(name = "trees", title = gettext("Trees"), type = "integer")
    table$addColumnInfo(name = "mtry", title = gettext("Features per split"), type = "integer")
  } else if (inherits(model, "cv.glmnet")) {
    table$addColumnInfo(name = "lambda", title = "\u03BB", type = "number")
  }
  table$addColumnInfo(name = "ntrain", title = gettext("n(Train)"), type = "integer")
  table$addColumnInfo(name = "nnew", title = gettext("n(New)"), type = "integer")
  row <- list()
  row[["model"]] <- .mlPredictionGetModelType(model)
  row[["ntrain"]] <- .mlPredictionGetTrainingN(model)
  if (inherits(model, "kknn")) {
    row[["nn"]] <- model[["predictive"]]$best.parameters$k
  } else if (inherits(model, "lda")) {
    row[["ld"]] <- ncol(model[["scaling"]])
  } else if (inherits(model, "gbm")) {
    row[["trees"]] <- model[["n.trees"]]
    row[["shrinkage"]] <- model[["shrinkage"]]
  } else if (inherits(model, "randomForest")) {
    row[["trees"]] <- model[["ntree"]]
    row[["mtry"]] <- model[["mtry"]]
  } else if (inherits(model, "cv.glmnet")) {
    row[["lambda"]] <- model[["lambda.min"]]
  }
  if (length(presentVars) > 0) {
    row[["nnew"]] <- nrow(dataset)
  }
  table$addRows(row)
}

.mlPredictionsTable <- function(model, dataset, options, jaspResults, ready, position) {
  if (!is.null(jaspResults[["predictionsTable"]]) || !options[["predictionsTable"]]) {
    return()
  }
  table <- createJaspTable(gettext("Predictions for New Data"))
  table$dependOn(options = c("predictors", "loadPath", "predictionsTable", "predictionsTableFeatures", "scaleVariables", "fromIndex", "toIndex"))
  table$position <- position
  table$addColumnInfo(name = "row", title = gettext("Case"), type = "integer")
  if (!is.null(model) && inherits(model, "jaspClassification")) {
    table$addColumnInfo(name = "pred", title = gettext("Predicted"), type = "string")
  } else {
    table$addColumnInfo(name = "pred", title = gettext("Predicted"), type = "number")
  }
  jaspResults[["predictionsTable"]] <- table
  if (!ready) {
    return()
  }
  predictions <- .mlPredictionsState(model, dataset, options, jaspResults, ready)
  indexes <- options[["fromIndex"]]:options[["toIndex"]]
  selection <- predictions[indexes]
  cols <- list(row = indexes, pred = selection)
  if (options[["predictionsTableFeatures"]]) {
    for (i in encodeColNames(model[["jaspVars"]])) {
      if (.columnIsNominal(i)) {
        table$addColumnInfo(name = i, title = i, type = "string")
        var <- levels(dataset[[i]])[dataset[[i]]]
      } else {
        table$addColumnInfo(name = i, title = i, type = "number")
        var <- dataset[[i]]
      }
      var <- var[indexes]
      cols[[i]] <- var
    }
  }
  table$setData(cols)
}

.mlPredictionsAddPredictions <- function(model, dataset, options, jaspResults, ready) {
  if (options[["addPredictions"]] && is.null(jaspResults[["predictionsColumn"]]) && options[["predictionsColumn"]] != "" && ready) {
    predictionsColumn <- rep(NA, max(as.numeric(rownames(dataset))))
    predictionsColumn[as.numeric(rownames(dataset))] <- .mlPredictionsState(model, dataset, options, jaspResults, ready)
    jaspResults[["predictionsColumn"]] <- createJaspColumn(columnName = options[["predictionsColumn"]])
    jaspResults[["predictionsColumn"]]$dependOn(options = c("predictionsColumn", "predictors", "loadPath", "scaleVariables", "addPredictions"))
    if (inherits(model, "jaspClassification")) jaspResults[["predictionsColumn"]]$setNominal(predictionsColumn)
    if (inherits(model, "jaspRegression")) jaspResults[["predictionsColumn"]]$setScale(predictionsColumn)
  }
}
