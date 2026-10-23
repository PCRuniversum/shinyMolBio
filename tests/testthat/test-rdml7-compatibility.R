test_that("RDML7 tables render as PCR plates and amplification curves", {
  rdml <- RDML7::readRDML(
    system.file("extdata", "test.rdml", package = "shinyMolBio"),
    showProgress = FALSE
  )
  description <- RDML7::asTable(rdml)
  fluorescence <- RDML7::getFData(rdml, longTable = TRUE)
  format <- rdml@experiment[[1]]@run[[1]]@pcrFormat

  expect_s3_class(
    pcrPlateInput("plate", plateDescription = description, pcrFormat = format),
    "pcrPlate"
  )
  expect_s3_class(
    renderAmpCurves("curves", ampCurves = fluorescence),
    "pcrCurves"
  )
})

test_that("RDML7 allelic-discrimination data retains both requested channels", {
  rdml <- RDML7::readRDML(
    system.file("extdata", "test.rdml", package = "shinyMolBio"),
    showProgress = FALSE
  )
  description <- RDML7::asTable(
    rdml,
    endPointRFU = mean(tail(data$adp$fpoints$fluor, 5))
  )
  description[, genotype := "unknown"]

  plot <- renderADplot(
    "ad", adData = description, xAxisTarget = "FAM", yAxisTarget = "HEX"
  )

  expect_s3_class(plot, "adPlot")
  widget <- plotly::plotly_build(plot[[2]][[3]][[2]])
  expect_gt(length(widget$x$data[[1]]$x), 0L)
  expect_gt(length(widget$x$data[[1]]$y), 0L)
})
