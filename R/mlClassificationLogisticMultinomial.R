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

mlClassificationLogisticMultinomial <- function(jaspResults, dataset, options, ...) {

  # Preparatory work
  dataset <- .mlClassificationReadData(dataset, options)
  .mlClassificationErrorHandling(dataset, options, type = "logistic")

  # Check if analysis is ready to run
  ready <- .mlClassificationReady(options, type = "logistic")

  # Compute results and create the model summary table
  .mlClassificationTableSummary(dataset, options, jaspResults, ready, position = 1, type = "logistic")

  # If the user wants to add the classes to the data set
  .mlClassificationAddPredictionsToData(dataset, options, jaspResults, ready)

  # Add test set indicator to data
  .mlAddTestIndicatorToData(options, jaspResults, ready, purpose = "classification")

  # Create the data split plot
  .mlPlotDataSplit(dataset, options, jaspResults, ready, position = 2, purpose = "classification", type = "logistic")

  # Create the confusion table
  .mlClassificationTableConfusion(dataset, options, jaspResults, ready, position = 3)

  # Create the class proportions table
  .mlClassificationTableProportions(dataset, options, jaspResults, ready, position = 4)

  # Create the validation measures table
  .mlClassificationTableMetrics(dataset, options, jaspResults, ready, position = 5)

  # Create the variable importance table
  .mlTableFeatureImportance(options, jaspResults, ready, position = 6, purpose = "classification")

  # Create the shap table
  .mlTableShap(dataset, options, jaspResults, ready, position = 7, purpose = "classification")

  .mlClassificationLogisticTableCoef(options, jaspResults, ready, position = 8)

  # Create the ROC curve
  .mlClassificationPlotRoc(dataset, options, jaspResults, ready, position = 10, type = "logistic") # position + 1 for regression equation

  # Create the Andrews curves
  .mlClassificationPlotAndrews(dataset, options, jaspResults, ready, position = 11)

  # Decision boundaries
  .mlClassificationPlotBoundaries(dataset, options, jaspResults, ready, position = 12, type = "logistic")
}

.logisticMultinomialClassification <- function(dataset, options, jaspResults, ready) {
  # Import model formula from jaspResults
  formula <- jaspResults[["formula"]]$object
  # Split the data into training and test sets
  if (options[["holdoutData"]] == "testSetIndicator" && options[["testSetIndicatorVariable"]] != "") {
    # Select observations according to a user-specified indicator (included when indicator = 1)
    trainingIndex <- which(dataset[, options[["testSetIndicatorVariable"]]] == 0)
  } else {
    # Sample a percentage of the total data set
    trainingIndex <- sample.int(nrow(dataset), size = ceiling((1 - options[["testDataManual"]]) * nrow(dataset)))
  }
  trainingSet <- dataset[trainingIndex, ]
  # Create the generated test set indicator
  testIndicatorColumn <- rep(1, nrow(dataset))
  testIndicatorColumn[trainingIndex] <- 0
  # Just create a train and a test set (no optimization)
  testSet <- dataset[-trainingIndex, ]
  # Create the formula
  if (options[["intercept"]]) {
    formula <- formula(paste(options[["target"]], "~ 1 + ", paste(options[["predictors"]], collapse = " + ")))
  } else {
    formula <- formula(paste(options[["target"]], "~ 0 + ", paste(options[["predictors"]], collapse = " + ")))
  }
  if (nlevels(trainingSet[[options[["target"]]]]) == 2) {
    family = "binomial"
    trainingFit <- glm(formula, data = trainingSet, family = family)
    # Use the specified model to make predictions for dataset
    testPredictions <- .mlClassificationLogisticPredictions(trainingSet, options, predict(trainingFit, newdata = testSet, type = "response"))
    dataPredictions <- .mlClassificationLogisticPredictions(trainingSet, options, predict(trainingFit, newdata = dataset, type = "response"))
  } else {
    family <- "multinomial"
    trainingFit <- VGAM::vglm(formula, data = trainingSet, family = family)
    # Use the specified model to make predictions for dataset
    testPredictions <- .mlClassificationMultinomialPredictions(trainingSet, options, predict(trainingFit, newdata = testSet))
    dataPredictions <- .mlClassificationMultinomialPredictions(trainingSet, options, predict(trainingFit, newdata = dataset))
  }
  # Create results object
  result <- list()
  result[["formula"]] <- formula
  result[["family"]] <- family
  if (family == "binomial") {
    result[["model"]] <- trainingFit
  } else {
    model <- lapply(slotNames(trainingFit), function(x) slot(trainingFit, x))
    names(model) <- slotNames(trainingFit)
    model[["original"]] <- trainingFit
    model[["target"]] <- trainingSet[[options[["target"]]]]
    class(model) <- "vglm"
    result[["model"]] <- model
  }
  result[["confTable"]] <- table("Pred" = testPredictions, "Real" = testSet[, options[["target"]]])
  result[["testAcc"]] <- sum(diag(prop.table(result[["confTable"]])))
  result[["auc"]] <- .classificationCalcAUC(testSet, trainingSet, options, "logisticClassification")
  result[["ntrain"]] <- nrow(trainingSet)
  result[["ntest"]] <- nrow(testSet)
  result[["testReal"]] <- testSet[, options[["target"]]]
  result[["testPred"]] <- testPredictions
  result[["train"]] <- trainingSet
  result[["test"]] <- testSet
  result[["testIndicatorColumn"]] <- testIndicatorColumn
  result[["classes"]] <- dataPredictions
  if (family == "binomial") {
    result[["explainer"]] <- DALEX::explain(result[["model"]], type = "classification", data = result[["train"]], y = result[["train"]][, options[["target"]]], predict_function = function(model, data) data.frame(1 - predict(model, newdata = data, type = "response"), predict(model, newdata = data, type = "response")))
    result[["explainer_fi"]] <- DALEX::explain(result[["model"]], type = "classification", data = result[["train"]], y = as.numeric(result[["train"]][, options[["target"]]]) - 1, predict_function = function(model, data) round(predict(model, newdata = data, type = "response"), 0) + 1)
  } else {
    # TODO
    result[["explainer"]] <- DALEX::explain(result[["model"]][["original"]], type = "multiclass", data = result[["train"]], y = result[["train"]][, options[["target"]]], predict_function = function(model, data) VGAM::predict(model, data, type = "response"))
    result[["explainer_fi"]] <- result[["explainer"]]
  }
  return(result)
}

.mlClassificationLogisticTableCoef <- function(options, jaspResults, ready, position) {
  if (!is.null(jaspResults[["coefTable"]]) || !options[["coefTable"]]) {
    return()
  }
  table <- createJaspTable(gettext("Regression Coefficients"))
  table$position <- position
  table$dependOn(options = c("coefTable", "coefTableConfInt", "coefTableConfIntLevel", "formula", .mlClassificationDependencies()))
  table$addColumnInfo(name = "var", title = "", type = "string")
  table$addColumnInfo(name = "coefs", title = gettextf("Coefficient (%s)", "\u03B2"), type = "number")
  table$addColumnInfo(name = "se", title = gettext("Standard Error"), type = "number")
  table$addColumnInfo(name = "t", title = gettext("t"), type = "number")
  table$addColumnInfo(name = "p", title = gettext("p"), type = "pvalue")
  if (options[["coefTableConfInt"]]) {
    overtitle <- gettextf("%1$s%% Confidence interval", round(options[["coefTableConfIntLevel"]] * 100, 3))
    table$addColumnInfo(name = "lower", title = gettext("Lower"), type = "number", overtitle = overtitle)
    table$addColumnInfo(name = "upper", title = gettext("Upper"), type = "number", overtitle = overtitle)
  }
  if (options[["scaleVariables"]]) {
    table$addFootnote(gettext("The regression coefficients for numeric features are standardized."))
  } else {
    table$addFootnote(gettext("The regression coefficients are unstandardized."))
  }
  jaspResults[["coefTable"]] <- table
  if (!ready) {
    if (options[["target"]] == "" && length(unlist(options[["predictors"]])) > 0) {
      table[["var"]] <- c(if (options[["intercept"]]) "(Intercept)" else NULL, options[["predictors"]])
    }
    return()
  }
  classificationResult <- jaspResults[["classificationResult"]]$object
  model <- classificationResult[["model"]]
  if (classificationResult[["family"]] == "binomial") {
    coefs <- summary(model)$coefficients
    conf_int <- confint(model, level = options[["coefTableConfIntLevel"]])
    coefs <- cbind(coefs, lower = conf_int[, 1], upper = conf_int[, 2])
    colnames(coefs) <- c("est", "se", "t", "p", "lower", "upper")
    vars <- rownames(coefs)
    for (i in seq_along(vars)) {
      if (!(vars[i] %in% options[["predictors"]]) && vars[i] != "(Intercept)") {
        for (j in options[["predictors"]]) {
          vars[i] <- gsub(pattern = j, replacement = paste0(j, " ("), x = vars[i])
        }
        vars[i] <- paste0(vars[i], ")")
      }
    }
    rownames(coefs) <- vars
  } else {
    coefs <- cbind(model$coefficients, confint(model[["original"]], level = options[["coefTableConfIntLevel"]]))
    colnames(coefs) <- c("est", "lower", "upper")
    vars <- rownames(coefs)
    for (i in seq_along(vars)) {
      for (j in c("(Intercept)", options[["predictors"]])) {
        if (!grepl(j, vars[i])) {
          next
        }
        splitvar <- strsplit(vars[i], split = ":")[[1]]
        if (grepl(paste0(j, "[A-Za-z]+:"), vars[i])) {
          repl_part1 <- paste0(gsub(pattern = j, replacement = paste0(j, " ("), x = splitvar[1]), ")")
        } else {
          repl_part1 <- j
        }
        repl_part2 <- levels(factor(classificationResult[["train"]][[options[["target"]]]]))[as.numeric(splitvar[2])]
        vars[i] <- paste0(repl_part1, " : ", repl_part2)
      }
    }
    rownames(coefs) <- vars
  }
  table[["var"]] <- rownames(coefs)
  table[["coefs"]] <- as.numeric(coefs[, "est"])
  if (classificationResult[["family"]] == "binomial") {
    table[["se"]] <- as.numeric(coefs[, "se"])
    table[["t"]] <- as.numeric(coefs[, "t"])
    table[["p"]] <- as.numeric(coefs[, "p"])
  } else {
    table[["se"]] <- rep(".", nrow(coefs))
    table[["t"]] <- rep(".", nrow(coefs))
    table[["p"]] <- rep(".", nrow(coefs))
    table$addFootnote(gettext("Standard errors, t-values and p-values are not available for multinomial regression coefficients."))
  }
  if (options[["coefTableConfInt"]]) {
    table[["lower"]] <- coefs[, "lower"]
    table[["upper"]] <- coefs[, "upper"]
  }
  if (options[["formula"]]) { # TODO FOR MULTINOMIAL
    if (classificationResult[["family"]] == "binomial") {
      one_cat <- levels(factor(classificationResult[["train"]][[options[["target"]]]]))[2]
      if (options[["intercept"]]) {
        regform <- paste0("logit(p<sub>", options[["target"]], " = ", one_cat, "</sub>) = ", round(as.numeric(coefs[, 1])[1], 3))
        start <- 2
      } else {
        regform <- paste0("logit(p<sub>", options[["target"]], " = ", one_cat, "</sub>) = ")
        start <- 1
      }
      for (i in start:nrow(coefs)) {
        regform <- paste0(regform, if (round(as.numeric(coefs[, 1])[i], 3) < 0) " - " else " + ", abs(round(as.numeric(coefs[, 1])[i], 3)), " x ", rownames(coefs)[i])
      }
    } else {
      regform <- NULL
      nlevs <- nlevels(classificationResult[["train"]][[options[["target"]]]])
      baseline_cat <- levels(classificationResult[["train"]][[options[["target"]]]])[nlevs]
      for (i in seq_len(nlevs - 1)) {
        current_cat <- levels(classificationResult[["train"]][[options[["target"]]]])[i]
        if (options[["intercept"]]) {
          part <- paste0("log(p<sub>", options[["target"]], " = ", current_cat, "</sub> / p<sub>", options[["target"]], " = ", baseline_cat, "</sub>) = ", round(as.numeric(coefs[, 1])[i], 3))
          start <- nlevs - 1 + i
        } else {
          part <- paste0("log(p<sub>", options[["target"]], " = ", current_cat, "</sub> / p<sub>", options[["target"]], " = ", baseline_cat, "</sub>) = ")
          start <- i
        }
        for (j in seq(start, nrow(coefs), by = nlevs - 1)) {
          part <- paste0(part, if (round(as.numeric(coefs[, 1])[j], 3) < 0) " - " else " + ", abs(round(as.numeric(coefs[, 1])[j], 3)), " x ", strsplit(rownames(coefs)[j], " : ")[[1]][1])
        }
        if (i == 1) {
          regform <- paste0(regform, part, "\n\n")
        } else {
          regform <- paste0(regform, part)
        }
      }
    }
    formula <- createJaspHtml(gettextf("<b>Regression equation:</b>\n%1$s", regform), "p")
    formula$position <- position + 1
    formula$dependOn(options = c("coefTable", "formula"), optionsFromObject = jaspResults[["classificationResult"]])
    jaspResults[["regressionFormula"]] <- formula
  }
}

.mlClassificationLogisticPredictions <- function(trainingSet, options, probabilities) {
  categories <- levels(trainingSet[[options[["target"]]]])
  predicted_categories <- factor(categories[round(probabilities, 0) + 1], levels = levels(trainingSet[[options[["target"]]]]))
  return(predicted_categories)
}

.mlClassificationMultinomialPredictions <- function(trainingSet, options, logodds) {
  ncategories <- ncol(logodds) + 1
  probabilities <- matrix(0, nrow = nrow(logodds), ncol = ncategories)
  for (i in seq_len(ncategories - 1)) {
    probabilities[, i] <- exp(logodds[, i])
  }
  probabilities[, ncategories] <- 1
  row_sums <- rowSums(probabilities)
  probabilities <- probabilities / row_sums
  predicted_columns <- apply(probabilities, 1, which.max)
  categories <- levels(trainingSet[[options[["target"]]]])
  predicted_categories <- factor(categories[predicted_columns], levels = levels(trainingSet[[options[["target"]]]]))
  return(predicted_categories)
}
