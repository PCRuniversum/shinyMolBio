#' Renders a amplification curves viewer
#'
#' Create amplification curves plot.
#'
#' @author Konstantin A. Blagodatskikh <k.blag@@yandex.ru>
#' @keywords PCR RDML Shiny Input
#' @import plotly
#'
#' @family render elements
#' @seealso \code{\link{updateCurvesInput}}
#'
#' @examples
#' # TODO
#' @export
renderAmpCurves <- function(inputId,
                            label = NULL,
                            ampCurves,
                            # plotlyCode = NULL,
                            colorBy = NULL,
                            shapeBy = NULL,
                            logScale = FALSE,
                            showCq = FALSE,
                            showBaseline = FALSE,
                            cssFile = system.file("/css/curvesInputStyle.css",
                                                  package = "shinyMolBio"),
                            cssText = NULL,
                            interactive = base::interactive()) {
  assertNames(colnames(ampCurves),
              must.include = c("fdata.name", "cyc", "fluor"))
  assertLogical(logScale)
  ampCurves <- ampCurves %>%
    rename(x = cyc,
           y = fluor)
  if (showCq) {
    assertNames(colnames(ampCurves),
                must.include = c("cq"))
    ampCurves <- ampCurves %>%
      rename(markers = cq)
  }

  renderCurves(inputId,
               label = label,
               curves = ampCurves,
               xAxisTitle = "Cycles",
               yAxisTitle = if (logScale) "log(RFU)" else "RFU",
               # plotlyCode = NULL,
               colorBy = colorBy,
               shapeBy = shapeBy,
               logScale = logScale,
               showMarkers = showCq,
               # showBaseline = showBaseline,
               cssFile = cssFile,
               cssText = cssText,
               interactive = interactive)
}

#' Renders a melting curves viewer
#'
#' Create melting curves plot.
#'
#' @author Konstantin A. Blagodatskikh <k.blag@@yandex.ru>
#' @keywords PCR RDML Shiny Input
#' @import plotly
#'
#' @family render elements
#' @seealso \code{\link{updateCurvesInput}}
#'
#' @examples
#' # TODO
#' @export
renderMeltCurves <- function(inputId,
                            label = NULL,
                            meltCurves,
                            # plotlyCode = NULL,
                            colorBy = NULL,
                            shapeBy = NULL,
                            showTm = FALSE,
                            cssFile = system.file("/css/curvesInputStyle.css",
                                                  package = "shinyMolBio"),
                            cssText = NULL,
                            interactive = base::interactive()) {
  assertNames(colnames(meltCurves),
              must.include = c("fdata.name", "tmp", "fluor"))
  meltCurves <- meltCurves %>%
    rename(x = tmp,
           y = fluor)
  if (showTm) {
    assertNames(colnames(meltCurves),
                must.include = c("tm"))
    meltCurves <- meltCurves %>%
      rename(markers = tm)
  }

  renderCurves(inputId,
               label = label,
               curves = meltCurves,
               xAxisTitle = "Temperature",
               yAxisTitle = "-∆(RFU)/∆T",
               # plotlyCode = NULL,
               colorBy = colorBy,
               shapeBy = shapeBy,
               logScale = FALSE,
               showMarkers = showTm,
               # showBaseline = showBaseline,
               cssFile = cssFile,
               cssText = cssText,
               interactive = interactive)
}


renderCurves <- function(inputId,
                         label = NULL,
                         curves,
                         xAxisTitle,
                         yAxisTitle,
                         # plotlyCode = NULL,
                         colorBy = NULL,
                         shapeBy = NULL,
                         logScale = FALSE,
                         showMarkers = FALSE,
                         # showBaseline = FALSE,
                         cssFile = system.file("/css/curvesInputStyle.css",
                                               package = "shinyMolBio"),
                         cssText = NULL,
                         interactive = base::interactive()) {
  assertString(inputId)
  assertString(label, null.ok = TRUE)
  assertDataFrame(curves)
  assertString(xAxisTitle)
  assertString(yAxisTitle)
  # assertString(plotlyCode, null.ok = TRUE)
  assertString(colorBy, null.ok = TRUE)
  assertString(shapeBy, null.ok = TRUE)
  assertNames(colnames(curves),
              must.include = c("fdata.name", "x", "y", colorBy, shapeBy))
  assertLogical(logScale)
  assertLogical(showMarkers)

  if (showMarkers) {
    assertNames(colnames(curves),
                must.include = c("markers"))
    # prepare markers
    maxX <- max(curves$x, na.rm = TRUE)
    # replace all NA cq with max cycle
    curves[is.na(curves$markers), "markers"] <- maxX
    curves <- curves %>%
      group_by(fdata.name) %>%
      mutate(isMarker = replace(rep(FALSE, length(x)),
                                sapply(unique(markers), # set TRUE to closest cyc
                                       function(marker) which.min(abs(x - marker))), TRUE))
  }

  # assertLogical(showBaseline)
  assertString(cssFile)
  assertString(cssText, null.ok = TRUE)
  assertLogical(interactive)

  ns <- NS(inputId)



  # assign colors to curves
  if (suppressWarnings(is.null(curves$curveColor))) {
    if (!is.null(colorBy)) {
      colorNames <- unique(curves[[colorBy]])
      needNColors <- length(colorNames)
      curvesColors <- tryCatch(
        RColorBrewer::brewer.pal(needNColors, "Set2"),
        warning = function(w)
          colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))(needNColors)
      )
      names(curvesColors) <- colorNames
      curves$curveColor <- curvesColors[curves[[colorBy]]]
    } else {
      curves$curveColor <- "black"
    }
  }
  p <- plot_ly() %>%
    add_trace(data = curves,
              split = ~fdata.name,
              x = ~x, y = ~y,
              color = ~curveColor,
              linetype = {
                if (is.null(shapeBy)) NULL
                else ~get(shapeBy)
              },
              type = "scatter", mode = "lines"
    ) %>%
    plotly::layout(showlegend = FALSE,
                   xaxis = list(title = xAxisTitle),
                   yaxis = list(title = yAxisTitle,
                                type = if (logScale) "log" else "linear"))

  if (showMarkers) {
    cqs <- curves %>%
      filter(isMarker == TRUE)
    p <- add_trace(p,
                   data = cqs,
                   x = ~x, y = ~y,
                   color = ~curveColor,
                   split = ~fdata.name,
                   type = "scatter", mode = "markers"
    )
  }

  # if (!is.null(plotlyCode))
  # p <- p %>% eval(parse(plotlyCode))

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
          # includeScript(system.file("/js/renderCurves-bindings.js", package = "shinyMolBio"))
          includeScript("~/Dropbox/R/shinyMolBio/inst/js/renderCurves-bindings.js")
        ),
        singleton(css)
      )
    } else {
      css
    },
    div(id = inputId, class = "pcr-curves",
        "data-ncurves" = curves$fdata.name %>% unique() %>% length(),
        "data-showmarkers" = if (showMarkers) "true" else "false",
        # "data-showbaseline" = if (showBaseline) "true" else "false",
        tags$label(label, `for` = inputId),
        p #ggplotly(p)
    )
  )
}

#' Change the value of a render PCR curves control on the client
#'
#' Change the value of a render PCR curves control on the client
#'
#' @param session The \code{session} object passed to function given to \code{shinyServer}.
#' @param inputId The id of the \code{input} object.
#' @param label The label to set for the input object.
#' @param selection The positions of the wells to be selected.
#'
#' @author Konstantin A. Blagodatskikh <k.blag@@yandex.ru>
#' @keywords PCR RDML Shiny Input
#' @examples
#' # TODO
#' @export
updateCurves <- function(session, inputId,
                         label = NULL,
                         hideCurves = NULL) {
  assertClass(session, "ShinySession")
  assertString(inputId)
  assertString(label, null.ok = TRUE)
  assertInteger(hideCurves,
                any.missing = FALSE, null.ok = TRUE)
  message <- .dropNulls(list(label = label, hideCurves = hideCurves))# - 1L)
  session$sendInputMessage(inputId, message)
}
