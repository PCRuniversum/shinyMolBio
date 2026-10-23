# Given a vector or list, drop all the NULL items in it
.dropNulls <- function(x) {
  x[!vapply(x, is.null, FUN.VALUE = logical(1))]
}

# Add historical aliases for the RDML7 1.0.2 camelCase table schema used by
# the widgets and their JavaScript bindings. Keeping both forms at the package
# boundary lets callers use either the native API or compatibility wrappers.
.normaliseRDML7Columns <- function(x) {
  x <- data.table::copy(x)
  aliases <- c(
    fdataName = "fdata.name",
    expId = "exp.id",
    runId = "run.id",
    reactId = "react.id",
    targetDyeId = "target.dyeId",
    sampleType = "sample.type"
  )
  for (source in names(aliases)) {
    destination <- aliases[[source]]
    if (source %in% names(x) && !destination %in% names(x)) {
      x[, (destination) := get(source)]
    }
  }
  x
}

utils::globalVariables(c(
  ".", "V1", "V2", "V3", "V4", "color", "curveName", "fdata.name",
  "highlightCurve", "legendGroup", "linetype", "markers", "pointName",
  "position", "quantFluor", "sample.type", "target", "target.dyeId"
))
