#' Create a PCR curves viewer input control
#'
#' Create an input control for representing PCR plate and dynamically selecting
#' wells inside it.
#'
#' @param inputId The \code{input} slot that will be used to access the selected
#'   wells positions.
#' @param label Display label for the control, or \code{NULL} for no label.
#' @param plateDescription Plate description - basicly output from \code{RDML
#'   AsTable()} function.
#' @param pcrFormat PCR plate parametrs. Should be \code{pcrFormatType}.
#' @param selection Set preselected wells (e.g. \code{c("A01", "A02") or \code{c(1, 2)}})
#' @param wellLabelTemplate Template of the well label.
#' @param onHoverWellTextTemplate Template of the text on hover.
#' @param wellClassTemplate Template of the well class (css class).
#' @param wellStyleTemplate Template of the well style (css).
#' @param wellGroupTemplate Template of the well group for selecting.
#' @param cssFile path to the css styles file.
#' @param cssText css styles as text.
#' @param plateLegend plate legend (any HTML content).
#' @return A PCR plate control that can be added to a UI definition.
#'
#' @author Konstantin A. Blagodatskikh <k.blag@@yandex.ru>
#' @keywords PCR RDML Shiny Input
#' @import RDML shiny tidyverse whisker stringr checkmate
#'
#' @family input elements
#' @seealso \code{\link{updatePcrPlateInput}}
#'
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#' ui <- fluidPage(
#'     pcrPlateInput("plate1",
#'                    "Plate 1",
#'                    RDML$new(system.file("/extdata/stepone_std.rdml", package = "RDML"))$AsTable(),
#'                   pcrFormatType$new(8,12,labelFormatType$new("ABC"),
#'                                          labelFormatType$new("123"))),
#'    verbatimTextOutput("selected")
#'  )
#'  server <- function(input, output) {
#'    output$selected <- renderText({ input$plate1 })
#'  }
#'  shinyApp(ui, server)
#' }
#' @export
pcrCurvesInput <- function(inputId,
                          label = NULL,
                          pcrCurves,
                          selected = NULL,
                          colorBy = "none", shapeBy = "none",
                          logScale = FALSE,
                          showCq = FALSE,
                          showBaseline = FALSE,
                          cssFile = system.file("/css/pcrCurvesInputStyle.css",
                                                package = "shinyMolBio"),
                          cssText = NULL,
                          interactive = base::interactive()) {
  ns <- NS(inputId)

  if (!is.null(selected)){
    pcrCurves <- pcrCurves %>%
      filter(position %in% selected)
  }

  p <-
    ggplot(pcrCurves) +
    geom_line(aes_string(x = "cyc", y = "fluor",
                         group = "fdata.name",
                         color = {
                           if (colorBy == "none")
                             NULL
                           else
                             colorBy
                         },
                         linetype = {
                           if (shapeBy == "none")
                             NULL
                           else
                             shapeBy
                         }),
              size = 0.5)

  css <- tags$style(type = "text/css",
                    paste0(whisker.render(
                      suppressWarnings(readLines(cssFile, warn = FALSE, encoding = "UTF-8")) %>%
                        paste0( collapse = ""),
                      list(id = inputId)
                    ),
                    whisker.render(cssText, list(id = inputId))
                    )
  )

  tagList(
    if (interactive) {
      tags$head(
        singleton(
          includeScript(system.file("/js/pcrPlate-input-bindings.js", package = "shinyMolBio"))
        ),
        singleton(css)
      )
    } else {
      css
    },
    div(id = inputId, class = "pcr-plate",
        tags$label(label, `for` = inputId),
        ggplotly(p)
    )
  )
}
