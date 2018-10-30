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
pcrCurvesInput <- function(inputId,
                          label = NULL,
                          pcrCurves,
                          selected = NULL,
                          ggplotCode = NULL,
                          colorBy = "none", shapeBy = "none",
                          logScale = FALSE,
                          showCq = TRUE,
                          showBaseline = FALSE,
                          cssFile = system.file("/css/pcrCurvesInputStyle.css",
                                                package = "shinyMolBio"),
                          cssText = NULL,
                          interactive = base::interactive()
                          ) {
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
              size = 0.5) +
              {
                if (!showCq)
                  NULL
                else
                  geom_point(aes_string(x = "cyc", y = "fluor",
                                        group = "fdata.name",
                                        color = {
                                          if (colorBy == "none")
                                            NULL
                                          else
                                            colorBy
                                        },
                                        shape = {
                                          NULL
                                          # if (input$shapeqPCRby == "none")
                                          #   NULL
                                          # else
                                          #   input$shapeqPCRby
                                        }
                                        # , size = "Tm"
                  ), data = pcrCurves %>%
                    filter(Cq == TRUE),
                  size = 1)
              } +
    ggplotCode

  css <- tags$style(type = "text/css",
                    paste0(whisker.render(
                      suppressWarnings(readLines(cssFile, warn = FALSE, encoding = "UTF-8")) %>%
                        paste0(collapse = ""),
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
    div(id = inputId, class = "pcr-curves",
        tags$label(label, `for` = inputId),
        ggplotly(p)
    )
  )
}
