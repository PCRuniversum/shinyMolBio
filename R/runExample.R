#' Run shinyMolBio Example Applications
#'
#' Launch shinyMolBio example applications
#'
#' @param example The name of the example to run, or NA (the default) to list the available examples.
#'
#' @author Konstantin A. Blagodatskikh <k.blag@@yandex.ru>
#'
#' @examples
#' ## Only run this example in interactive R sessions
#' if (interactive()) {
#'  # List all available examples
#'  runExample()
#'
#'  # Run one of the examples
#'  runExample("pcrPlateInput")
#'
#'  # Print the directory containing the code for all examples
#'  system.file("shiny-examples", package="shinyMolBio")
#' }
#' @export
runExample <- function(example) {
  # locate all the shiny app examples that exist
  validExamples <- list.files(system.file("shiny-examples", package = "shinyMolBio"))

  validExamplesMsg <-
    paste0(
      "Valid examples are: '",
      paste(validExamples, collapse = "', '"),
      "'")

  # if an invalid example is given, throw an error
  if (missing(example) || !nzchar(example) ||
      !example %in% validExamples) {
    stop(
      'Please run `runExample()` with a valid example app as an argument.\n',
      validExamplesMsg,
      call. = FALSE)
  }

  # find and launch the app
  appDir <- system.file("shiny-examples", example, package = "shinyMolBio")
  shiny::runApp(appDir, display.mode = "normal")
}
