initMlOptions <- function(analysis) {
  options <- c(
    jaspTools::analysisOptions(analysis),
    mlOptions(analysis)
  )

  return(options)
}

mlOptions <- function(analysis) {
  path <- testthat::test_path("..", "..", "inst", "qml", "common", "ui")
  files <- list.files(path, full.names = TRUE)
  files <- c(
    files,
    list.files(testthat::test_path("..", "..", "inst", "qml", "common", "tables"), full.names = TRUE),
    list.files(testthat::test_path("..", "..", "inst", "qml", "common", "figures"), full.names = TRUE)
  )
  if (analysis %in% c("mlClassificationBoosting", "mlRegressionBoosting")) {
    files <- c(files, list.files(testthat::test_path("..", "..", "inst", "qml", "common", "analyses", "boosting"), full.names = TRUE))
  } else if (analysis %in% c("mlClassificationDecisionTree", "mlRegressionDecisionTree")) {
    files <- c(files, list.files(testthat::test_path("..", "..", "inst", "qml", "common", "analyses", "decisiontree"), full.names = TRUE))
  } else if (analysis %in% c("mlClassificationKnn", "mlRegressionKnn")) {
    files <- c(files, list.files(testthat::test_path("..", "..", "inst", "qml", "common", "analyses", "knn"), full.names = TRUE))
  } else if (analysis %in% c("mlClassificationNeuralNetwork", "mlRegressionNeuralNetwork")) {
    files <- c(files, list.files(testthat::test_path("..", "..", "inst", "qml", "common", "analyses", "neuralnetwork"), full.names = TRUE))
  } else if (analysis %in% c("mlClassificationRandomForest", "mlRegressionRandomForest")) {
    files <- c(files, list.files(testthat::test_path("..", "..", "inst", "qml", "common", "analyses", "randomforest"), full.names = TRUE))
  } else if (analysis %in% c("mlClassificationSvm", "mlRegressionSvm")) {
    files <- c(files, list.files(testthat::test_path("..", "..", "inst", "qml", "common", "analyses", "svm"), full.names = TRUE))
  }
  options <- lapply(files, jaspTools:::readQML) |>
    lapply(function(x) {x$plotWidth <- NULL; x$plotHeight <- NULL; return(x)}) |>
    (function(x) { do.call(c, x)})()

  return(options)
}
