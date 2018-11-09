#' Create a PCR curves viewer input control
#'
#' Create an input control for viewing PCR curves as plot.
#'
#'
#' @author Konstantin A. Blagodatskikh <k.blag@@yandex.ru>
#' @keywords PCR RDML Shiny Input
#' @import plotly
#'
#' @family input elements
#' @seealso \code{\link{updatePcrCurvesInput}}
#'
#' @examples
#' @export
pcrCurvesInput <- function(inputId,
                          label = NULL,
                          pcrCurves,
                          selected = NULL,
                          ggplotCode = NULL,
                          colorBy = NULL, shapeBy = NULL,
                          logScale = FALSE,
                          showCq = FALSE,
                          showBaseline = FALSE,
                          cssFile = system.file("/css/pcrCurvesInputStyle.css",
                                                package = "shinyMolBio"),
                          cssText = NULL,
                          interactive = base::interactive()
                          ) {
  ns <- NS(inputId)
  p <- plot_ly(pcrCurves,
               x = ~cyc, y = ~fluor,
               color = {
                 if (!is.null(colorBy)) {
                   ~get(colorBy)
                 } else {
                   NULL
                 }
               },
               split = ~fdata.name,
               type = "scatter", mode = "lines") %>%
    plotly::layout(showlegend = FALSE)

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
        singleton(
          includeScript(system.file("/js/pcrCurves-input-bindings.js", package = "shinyMolBio"))
        ),
        singleton(css)
      )
    } else {
      css
    },
    div(id = inputId, class = "pcr-curves",
        tags$label(label, `for` = inputId),
        p #ggplotly(p)
    )
  )
}

#' Change the value of a PCR curve input control on the client
#'
#' Change the value of a PCR plate input control on the client
#'
#' @param session The \code{session} object passed to function given to \code{shinyServer}.
#' @param inputId The id of the \code{input} object.
#' @param label The label to set for the input object.
#' @param selection The positions of the wells to be selected.
#'
#' @author Konstantin A. Blagodatskikh <k.blag@@yandex.ru>
#' @keywords PCR RDML Shiny Input
#' @examples
#' @export
updatePcrCurvesInput <- function(session, inputId,
                                 label = NULL,
                                 hideCurves = NULL) {
  assertClass(session, "ShinySession")
  assertString(inputId)
  assertString(label, null.ok = TRUE)
  assertInteger(hideCurves,
                any.missing = FALSE, null.ok = TRUE)
  message <- .dropNulls(list(label = label, hideCurves = hideCurves - 1L))
  session$sendInputMessage(inputId, message)
}
