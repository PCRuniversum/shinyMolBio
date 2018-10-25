#' Create a PCR curves viewer input control
#'
#' Create an input control for representing PCR plate and dynamically selecting
#' wells inside it.
#'
#'
#' @author Konstantin A. Blagodatskikh <k.blag@@yandex.ru>
#' @keywords PCR RDML Shiny Input
#' @import plotly
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
meltCurvesInput <- function(inputId,
                          label = NULL,
                          meltCurves,
                          selected = NULL,
                          colorBy = "none", shapeBy = "none",
                          diffCurves = TRUE,
                          inverted = FALSE,
                          cssFile = system.file("/css/meltCurvesInputStyle.css",
                                                package = "shinyMolBio"),
                          cssText = NULL,
                          interactive = base::interactive()) {
  ns <- NS(inputId)

  if (!is.null(selected)){
    meltCurves <- meltCurves %>%
      filter(position %in% selected)
  }

  p <-
    ggplot(meltCurves) +
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
        singleton(css)
      )
    } else {
      css
    },
    div(id = inputId, class = "melt-curves",
        tags$label(label, `for` = inputId),
        ggplotly(p)
    )
  )
}
