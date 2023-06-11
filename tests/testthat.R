library(jaspTools)
library(testthat)

initMlOptions <- function(analysis) {
  options <- c(
    jaspTools::analysisOptions(analysis),
    mlOptions()
  )

  return(options)
}

mlOptions <- function() {
  path <- testthat::test_path("..", "..", "inst", "qml", "common", "ui")
  files <- list.files(path, full.names = TRUE)
  files <- c(
    files,
	list.files(testthat::test_path("..", "..", "inst", "qml", "common", "tables"), full.names = TRUE),
	list.files(testthat::test_path("..", "..", "inst", "qml", "common", "figures"), full.names = TRUE)
  )
  options <- lapply(files, jaspTools:::readQML) |>
    lapply(function(x) {x$plotWidth <- NULL; x$plotHeight <- NULL; return(x)}) |>
    (function(x) { do.call(c, x)})()

  return(options)
}

jaspTools::runTestsTravis(module = getwd())
