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
  model 	<- .mlPredictionReadModel(options)
  dataset 	<- .mlPredictionReadData(options)
  type 		<- .mlPredictionModelType(model)
  
  # Check if analysis is ready
  ready <- .mlPredictionReady(model, dataset, options)
  
  # Create a table summarizing the loaded model
  .mlPredictionModelSummaryTable(model, dataset, options, jaspResults, ready, position = 1)
  
  # Create a table containing the predicted values and predictors
  .mlPredictionsTable(model, dataset, options, type, jaspResults, ready, position = 2)
  
  # Add predicted outcomes to data set
  .mlPredictionsAddPredictions(model, dataset, options, jaspResults, ready)
  
}

.mlPredictionReadModel <- function(options){
	  if(options[["file"]] != ""){
    model <- try({ readRDS(options[["file"]]) })
    if(!(class(model) %in% c("lda", "gbm", "randomForest", "cv.glmnet")))
      jaspBase:::.quitAnalysis(gettextf("The imported model (type: %1$s) is currently not supported in JASP.", class(model)))
  } else {
    model <- NULL
  }
  return(model)
}

.mlPredictionReadData <- function(options){
	  dataset <- .readDataSetToEnd(columns = options[["predictors"]], exclude.na.listwise = options[["predictors"]])
  if(options[["scaleEqualSD"]] && length(unlist(options[["predictors"]])) > 0)
    dataset <- .scaleNumericData(dataset)
	return(dataset)
}

.mlPredictionModelType <- function(model){
  type <- NULL
  if(!is.null(model))
	type <- switch(class(model),
						"lda" = "classification",
						"gbm" = switch(model$distribution$name, "gaussian" = "regression", "multinomial" = "classification"),
						"randomForest" = model[["type"]],
						"cv.glmnet" = "regression")
	return(type)
}

.mlPredictionReady <- function(model, dataset, options){
  ready <- TRUE
  if(!is.null(model)){
  modelVars <- switch(class(model),
                      "lda" = decodeColNames(attr(model[["terms"]], "term.labels")),
                      "gbm" = decodeColNames(attr(model[["Terms"]], "term.labels")),
                      "randomForest" = decodeColNames(rownames(model[["importance"]])),
					  "cv.glmnet" = decodeColNames(rownames(model$glmnet.fit$beta)))
    presentVars <- decodeColNames(colnames(dataset))
    if(!all(modelVars %in% presentVars)){
      ready <- FALSE
    }
  } else {
    ready <- FALSE
  }
  return(ready)
}

.mlPredictionsState <- function(model, dataset, options, jaspResults, ready){
  if(!is.null(jaspResults[["predictions"]])){
    return(jaspResults[["predictions"]]$object)
  } else {
    if(ready){
		if(class(model) == "lda"){
			predictions <- MASS:::predict.lda(model, newdata = dataset)$class
		} else if(class(model) == "gbm"){
			if(model$distribution$name == "multinomial"){
				tmp <- gbm:::predict.gbm(model, newdata = dataset, n.trees = model[["n.trees"]], type = "response")
				predictions <- colnames(tmp)[apply(tmp, 1, which.max)]
			} else {
				predictions <- gbm:::predict.gbm(model, newdata = dataset, n.trees = model[["n.trees"]], type = "response")
			}
		} else if(class(model) == "randomForest"){
			predictions <- randomForest:::predict.randomForest(model, newdata = dataset)
		} else if(class(model) == "cv.glmnet"){
			predictions <- glmnet:::predict.cv.glmnet(model, newx = data.matrix(dataset))
			print(predictions)
		}
	  jaspResults[["predictions"]] <- createJaspState(predictions)
      jaspResults[["predictions"]]$dependOn(options = c("file", "predictors", "scaleEqualSD"))
      return(jaspResults[["predictions"]]$object)
    } else {
      return(NULL)
    }
  }
}

.mlPredictionModelSummaryTable <- function(model, dataset, options, jaspResults, ready, position){
  
  if(is.null(model)){
    table <- createJaspTable(gettext("Loaded Model"))
  } else {
    modelTitle <- switch(class(model),
                         "lda" = "Linear Discriminant Classification",
                         "gbm" = switch(model$distribution$name,
						 				"gaussian" = "Boosting Regression",
										 "multinomial" = "Boosting Classification"),
                         "randomForest" = switch(model[["type"]], 
                                                 "classification" = "Random Forest Classification",
                                                 "regression" = "Random Forest Regression"),
						  "cv.glmnet" = "Regularized Linear Regression")
    table <- createJaspTable(gettextf("Loaded Model: %1$s", modelTitle))
  }
  
  table$dependOn(options = c("predictors", "file"))
  table$position <- position
  
  table$addColumnInfo(name = "model", title = "", type = 'string')
  
  jaspResults[["modelSummaryTable"]] <- table
  
  if(is.null(model))
    return()
  
  modelName <- switch(class(model),
                      "lda" = "LDA",
                      "gbm" = "Boosting",
                      "randomForest" = "Random forest",
					  "cv.glmnet" = "Regularized")
  ntrain <- switch(class(model),
                   "lda" = model[["N"]],
                   "gbm" = model[["nTrain"]],
                   "randomForest" = length(model[["y"]]),
				   "cv.glmnet" = model$glmnet.fit$nobs)
  modelVars <- switch(class(model),
                      "lda" = decodeColNames(attr(model[["terms"]], "term.labels")),
                      "gbm" = decodeColNames(attr(model[["Terms"]], "term.labels")),
                      "randomForest" = decodeColNames(rownames(model[["importance"]])),
					  "cv.glmnet" = decodeColNames(rownames(model$glmnet.fit$beta)))
  presentVars <- decodeColNames(colnames(dataset))
  if(!all(modelVars %in% presentVars)){
    missingVars <- modelVars[which(!(modelVars %in% presentVars))]
    table$addFootnote(gettextf("The trained model is not applied because the the following predictors are missing: <i>%1$s</i>.", paste0(missingVars, collapse = ", ")))
  }
  
  if(class(model) == "lda"){
    table$addColumnInfo(name = "ld", title = "Linear Discriminants", type = 'integer')
  } else if(class(model) == "gbm"){
    table$addColumnInfo(name = "trees", title = "Trees", type = 'integer')
    table$addColumnInfo(name = "shrinkage", title = "Shrinkage", type = 'number')
  } else if(class(model) == "randomForest"){
    table$addColumnInfo(name = "trees", title = "Trees", type = 'integer')
    table$addColumnInfo(name = "mtry", title = "Predictors per split", type = 'integer')
  } else if(class(model) == "cv.glmnet"){
	table$addColumnInfo(name = "lambda", title = "\u03BB", type = 'number')
  }
  
  table$addColumnInfo(name = "ntrain", title = "n(Train)", type = 'integer')
  table$addColumnInfo(name = "nnew", title = "n(New)", type = 'integer')
  
  row <- list()
  row[["model"]] <- modelName
  row[["ntrain"]] <- ntrain
  if(class(model) == "lda"){
    row[["ld"]] <- ncol(model[["scaling"]])
  } else if(class(model) == "gbm"){
    row[["trees"]] <- model[["n.trees"]]
    row[["shrinkage"]] <- model[["shrinkage"]]
  } else if(class(model) == "randomForest"){
    row[["trees"]] <- model[["ntree"]]
    row[["mtry"]] <- model[["mtry"]]
  } else if(class(model) == "cv.glmnet"){
	  row[["lambda"]] <- model[["lambda.min"]]
  }
  if(length(presentVars) > 0)
	row[["nnew"]] <- nrow(dataset)

  table$addRows(row)
}

.mlPredictionsTable <- function(model, dataset, options, type, jaspResults, ready, position){
  
  if(!is.null(jaspResults[["predictionsTable"]]) || !options[["predictionsTable"]]) 
    return()
  
  table <- createJaspTable(gettext("Predictions for new data"))
  table$dependOn(options = c("predictors", "file", "predictionsTable", "addPredictors", "scaleEqualSD"))
  table$position <- position
  
  table$addColumnInfo(name = "row", title = "Row number", type = 'integer')
  if(!is.null(type)){
	if(type == "classification")
		table$addColumnInfo(name = "pred", title = "Predicted", type = 'string')
	if(type == "regression")
		table$addColumnInfo(name = "pred", title = "Predicted", type = 'number')
  } else {
	 table$addColumnInfo(name = "pred", title = "Predicted", type = 'number') 
  }
  
  jaspResults[["predictionsTable"]] <- table
  
  if(!ready)
    return() 
  
  predictions <- .mlPredictionsState(model, dataset, options, jaspResults, ready)
  
  if(options[["addPredictors"]]){
  modelVars <- switch(class(model),
                      "lda" = decodeColNames(attr(model[["terms"]], "term.labels")),
                      "gbm" = decodeColNames(attr(model[["Terms"]], "term.labels")),
                      "randomForest" = decodeColNames(rownames(model[["importance"]])),
					  "cv.glmnet" = decodeColNames(rownames(model$glmnet.fit$beta)))
    for(i in 1:length(modelVars)){
      columnName <- as.character(modelVars[i])
      if(is.numeric(dataset[, encodeColNames(columnName)])){
        table$addColumnInfo(name = columnName, title = columnName, type = "number")
      } else {
        table$addColumnInfo(name = columnName, title = columnName, type = "string")
      }
    }
  }
  
  rows <- data.frame(row = 1:nrow(dataset),
                     pred = predictions)
  if(options[["addPredictors"]]){
    rows <- cbind(rows, data.frame(dataset[, encodeColNames(modelVars)]))
    colnames(rows) <- c("row", "pred", modelVars)
  }
  
  table$addRows(rows)
}

.mlPredictionsAddPredictions <- function(model, dataset, options, jaspResults, ready){
  if(options[["addClasses"]] && is.null(jaspResults[["classColumn"]]) && options[["classColumn"]] != "" && ready){
    classColumn <- rep(NA, max(as.numeric(rownames(dataset))))
    predictions <- .mlPredictionsState(model, dataset, options, jaspResults, ready)
    classColumn[as.numeric(rownames(dataset))] <- predictions
    classColumn <- factor(classColumn)
    jaspResults[["classColumn"]] <- createJaspColumn(columnName = options[["classColumn"]])
    jaspResults[["classColumn"]]$dependOn(options = c("classColumn", "predictors", "file", "scaleEqualSD", "addClasses"))
    jaspResults[["classColumn"]]$setNominal(classColumn)
  }  
}

