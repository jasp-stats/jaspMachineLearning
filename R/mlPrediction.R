#
# Copyright (C) 2017 University of Amsterdam
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
}

is.jaspMachineLearning <- function(x) {
  inherits(x, "jaspMachineLearning")
}

# S3 method to identify the model type
.mlPredictionGetModelType <- function(model) {
  UseMethod(".mlPredictionGetModelType", model)
}
.mlPredictionGetModelType.lda <- function(model) {
  gettext("Linear discriminant")
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

# S3 method to identify the variables used for training the model
.mlPredictionGetModelVars <- function(model) {
  UseMethod(".mlPredictionGetModelVars", model)
}
.mlPredictionGetModelVars.lda <- function(model) {
  attr(model[["terms"]], "term.labels")
}
.mlPredictionGetModelVars.gbm <- function(model) {
  attr(model[["Terms"]], "term.labels")
}
.mlPredictionGetModelVars.randomForest <- function(model) {
  rownames(model[["importance"]])
}
.mlPredictionGetModelVars.cv.glmnet <- function(model) {
  rownames(model[["glmnet.fit"]][["beta"]])
}

# S3 method to make predictions using the model
.mlPredictionGetPredictions <- function(model, dataset) {
  UseMethod(".mlPredictionGetPredictions", model)
}
.mlPredictionGetPredictions.lda <- function(model, dataset) {
  as.character(MASS:::predict.lda(model, newdata = dataset)$class)
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

# S3 method to make find out number of observations in training data
.mlPredictionGetTrainingN <- function(model) {
  UseMethod(".mlPredictionGetTrainingN", model)
}
.mlPredictionGetTrainingN.lda <- function(model) {
  model[["N"]]
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

.mlPredictionReadModel <- function(options) {
  if (options[["file"]] != "") {
    model <- try({
      readRDS(options[["file"]])
    })
    if (!is.jaspMachineLearning(model)) {
      jaspBase:::.quitAnalysis(gettext("Error: The imported model is not created in JASP."))
    }
    if (!(any(c("lda", "gbm", "randomForest", "cv.glmnet") %in% class(model)))) { # Predictions for knn are not supported
      jaspBase:::.quitAnalysis(gettextf("The imported model (type: %1$s) is currently not supported in JASP.", paste(class(model), collapse = ", ")))
    }
    if (model[["jaspVersion"]] != .baseCitation) {
      jaspBase:::.quitAnalysis(gettext("Error: The imported model is created using a different version of JASP."))
    }
  } else {
    model <- NULL
  }
  return(model)
}

# also define methods for other objects
.mlPredictionReady <- function(model, dataset, options) {
  if (!is.null(model)) {
    modelVars <- .mlPredictionGetModelVars(model)
    presentVars <- colnames(dataset)
    ready <- all(modelVars %in% presentVars)
  } else {
    ready <- FALSE
  }
  return(ready)
}

.mlPredictionReadData <- function(options) {
  dataset <- .readDataSetToEnd(columns = options[["predictors"]], exclude.na.listwise = options[["predictors"]])
  if (options[["scaleEqualSD"]] && length(unlist(options[["predictors"]])) > 0) {
    dataset <- .scaleNumericData(dataset)
  }
  return(dataset)
}

.mlPredictionsState <- function(model, dataset, options, jaspResults, ready) {
  if (!is.null(jaspResults[["predictions"]])) {
    return(jaspResults[["predictions"]]$object)
  } else {
    if (ready) {
      jaspResults[["predictions"]] <- createJaspState(.mlPredictionGetPredictions(model, dataset))
      jaspResults[["predictions"]]$dependOn(options = c("file", "predictors", "scaleEqualSD"))
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
  table$dependOn(options = c("predictors", "file"))
  table$position <- position
  table$addColumnInfo(name = "model", title = gettext("Method"), type = "string")
  jaspResults[["modelSummaryTable"]] <- table

  if (is.null(model)) {
    return()
  }

  modelVars <- .mlPredictionGetModelVars(model)
  presentVars <- colnames(dataset)
  if (!all(modelVars %in% presentVars)) {
    missingVars <- modelVars[which(!(modelVars %in% presentVars))]
    table$addFootnote(gettextf("The trained model is not applied because the the following predictors are missing: <i>%1$s</i>.", paste0(missingVars, collapse = ", ")))
  }
  if (!all(presentVars %in% modelVars)) {
    unusedVars <- presentVars[which(!(presentVars %in% modelVars))]
    table$addFootnote(gettextf("The following predictors are unused because they are not a predictor variable in the trained model: <i>%1$s</i>.", paste0(unusedVars, collapse = ", ")))
  }

  if (inherits(model, "lda")) {
    table$addColumnInfo(name = "ld", title = gettext("Linear Discriminants"), type = "integer")
  } else if (inherits(model, "gbm")) {
    table$addColumnInfo(name = "trees", title = gettext("Trees"), type = "integer")
    table$addColumnInfo(name = "shrinkage", title = gettext("Shrinkage"), type = "number")
  } else if (inherits(model, "randomForest")) {
    table$addColumnInfo(name = "trees", title = gettext("Trees"), type = "integer")
    table$addColumnInfo(name = "mtry", title = gettext("Predictors per split"), type = "integer")
  } else if (inherits(model, "cv.glmnet")) {
    table$addColumnInfo(name = "lambda", title = "\u03BB", type = "number")
  }

  table$addColumnInfo(name = "ntrain", title = gettext("n(Train)"), type = "integer")
  table$addColumnInfo(name = "nnew", title = gettext("n(New)"), type = "integer")

  row <- list()
  row[["model"]] <- .mlPredictionGetModelType(model)
  row[["ntrain"]] <- .mlPredictionGetTrainingN(model)

  if (inherits(model, "lda")) {
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
  table$dependOn(options = c("predictors", "file", "predictionsTable", "addPredictors", "scaleEqualSD", "pfrom", "pto"))
  table$position <- position
  table$addColumnInfo(name = "row", title = gettext("Row"), type = "integer")

  if (!is.null(model)) {
    if (inherits(model, "jaspClassification")) {
      table$addColumnInfo(name = "pred", title = gettext("Predicted"), type = "string")
    }
    if (inherits(model, "jaspRegression")) {
      table$addColumnInfo(name = "pred", title = gettext("Predicted"), type = "number")
    }
  } else {
    table$addColumnInfo(name = "pred", title = gettext("Predicted"), type = "number")
  }

  jaspResults[["predictionsTable"]] <- table

  if (!ready) {
    return()
  }

  predictions <- .mlPredictionsState(model, dataset, options, jaspResults, ready)
  selection <- predictions[options[["pfrom"]]:options[["pto"]]]
  modelVars <- .mlPredictionGetModelVars(model)

  if (options[["addPredictors"]]) {
    for (i in 1:length(modelVars)) {
      columnName <- as.character(modelVars[i])
      if (is.numeric(dataset[[columnName]])) {
        table$addColumnInfo(name = columnName, title = columnName, type = "number")
      } else {
        table$addColumnInfo(name = columnName, title = columnName, type = "string")
      }
    }
  }

  rows <- list(row = options[["pfrom"]]:options[["pto"]], pred = selection)
  if (options[["addPredictors"]]) {
    for (i in modelVars) {
      var <- if (is.numeric(dataset[[i]])) dataset[[i]] else levels(dataset[[i]])[dataset[[i]]]
      var <- var[options[["pfrom"]]:options[["pto"]]]
      rows[[i]] <- var
    }
  }

  table$setData(rows)
}

.mlPredictionsAddPredictions <- function(model, dataset, options, jaspResults, ready) {
  if (options[["addClasses"]] && is.null(jaspResults[["classColumn"]]) && options[["classColumn"]] != "" && ready) {
    classColumn <- rep(NA, max(as.numeric(rownames(dataset))))
    classColumn[as.numeric(rownames(dataset))] <- .mlPredictionsState(model, dataset, options, jaspResults, ready)
    jaspResults[["classColumn"]] <- createJaspColumn(columnName = options[["classColumn"]])
    jaspResults[["classColumn"]]$dependOn(options = c("classColumn", "predictors", "file", "scaleEqualSD", "addClasses"))
    print(classColumn)
    if (inherits(model, "jaspClassification")) jaspResults[["classColumn"]]$setNominal(classColumn)
    if (inherits(model, "jaspRegression")) jaspResults[["classColumn"]]$setScale(classColumn)
  }
}