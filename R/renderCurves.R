#' Renders an amplification curves viewer
#'
#' Renders a reactive PCR amplification plot that is suitable for assigning to
#' an \code{UI output} slot.
#'
#' @usage renderAmpCurves(inputId, label = NULL, ampCurves, colorBy = NULL,
#'   linetypeBy = NULL, logScale = FALSE, showCq = FALSE, showLegend = FALSE,
#'   thBy = NULL, plotlyCode = NULL, cssFile = NULL, cssText = NULL,
#'   interactive = TRUE)
#'
#' @param inputId The \code{input} slot that will be used to modify plot.
#' @param label Display label for the control, or \code{NULL} for no label.
#' @param ampCurves Amplification curves data with
#'   \code{RDML$GetFData(long.table = TRUE)} format.
#' @param colorBy Column name that contains color levels data.
#' @param linetypeBy Column name that contains linetype levels data.
#' @param logScale Converts plot to \code{log(RFU)}.
#' @param showCq Shows Cq with dots (\code{cq} column have to be provided!).
#' @param showLegend Show plot legend.
#' @param thBy Column name that separates threshold values (\code{quantFluor}
#'   column have to be provided!).
#' @param plotlyCode Your quoted custom plotly code.
#' @param cssFile Path to the css styles file.
#' @param cssText CSS styles as text.
#' @param interactive Should be this \code{pcrPlate} interactive or not.
#'
#' @author Konstantin A. Blagodatskikh <k.blag@@yandex.ru>
#' @keywords PCR RDML Shiny Input
#' @import plotly RColorBrewer
#' @importFrom grDevices colorRampPalette
#'
#' @family render elements
#' @seealso \code{\link{updateCurves}}
#'
#' @export
#' @examples
#' library(RDML)
#' rdml <- RDML$new(system.file("/extdata/test.rdml", package = "shinyMolBio"))
#' curves <- renderAmpCurves("curves1", ampCurves = rdml$GetFData(long.table = TRUE))
#' curves[[2]][[3]][[2]]
renderAmpCurves <- function(inputId,
                            label = NULL,
                            ampCurves,
                            colorBy = NULL,
                            linetypeBy = NULL,
                            logScale = FALSE,
                            showCq = FALSE,
                            showLegend = FALSE,
                            thBy = NULL,
                            plotlyCode = NULL,
                            cssFile = NULL,
                            cssText = NULL,
                            interactive = TRUE) {
  assertNames(colnames(ampCurves),
              must.include = c("fdata.name", "cyc", "fluor"))
  assertLogical(logScale)
  ampCurves <- ampCurves %>%
    rename(x = .data$cyc,
           y = .data$fluor)
  if (showCq) {
    assertNames(colnames(ampCurves),
                must.include = c("cq"))
    ampCurves <- ampCurves %>%
      rename(markers = .data$cq)
  }

  renderCurves(inputId,
               label = label,
               curves = ampCurves,
               xAxisTitle = "Cycles",
               yAxisTitle = if (logScale) "log(RFU)" else "RFU",
               colorBy = colorBy,
               linetypeBy = linetypeBy,
               logScale = logScale,
               showMarkers = showCq,
               showLegend = showLegend,
               thBy = thBy,
               plotlyCode = plotlyCode,
               cssFile = cssFile,
               cssText = cssText,
               interactive = interactive)
}

#' Renders a melting curves viewer
#'
#' Renders a reactive melting plot that is suitable for assigning to an \code{UI
#' output} slot.
#'
#' @usage renderMeltCurves(inputId, label = NULL, meltCurves, colorBy = NULL,
#'   linetypeBy = NULL, showTm = FALSE, showLegend = FALSE, plotlyCode = NULL,
#'   cssFile = NULL, cssText = NULL, interactive = TRUE)
#'
#' @param inputId The \code{input} slot that will be used to modify plot.
#' @param label Display label for the control, or \code{NULL} for no label.
#' @param meltCurves Melting curves data with \code{RDML$GetFData(dp.type =
#'   "mdp", long.table = TRUE)} format.
#' @param colorBy Column name that contains color levels data.
#' @param linetypeBy Column name that contains linetype levels data.
#' @param showTm Shows Tm with dots (\code{tm} column have to be provided!)
#' @param showLegend Show plot legend.
#' @param plotlyCode Your quoted custom plotly code.
#' @param cssFile Path to the css styles file.
#' @param cssText CSS styles as text.
#' @param interactive Should be this \code{pcrPlate} interactive or not.
#'
#' @author Konstantin A. Blagodatskikh <k.blag@@yandex.ru>
#' @keywords PCR RDML Shiny Input
#'
#' @family render elements
#' @seealso \code{\link{updateCurves}}
#'
#' @export
#' @examples
#' library(RDML)
#' rdml <- RDML$new(system.file("/extdata/test.rdml", package = "shinyMolBio"))
#' curves <- renderMeltCurves("curves1", meltCurves = rdml$GetFData(dp.type = "mdp",
#'  long.table = TRUE))
#' curves[[2]][[3]][[2]]
renderMeltCurves <- function(inputId,
                            label = NULL,
                            meltCurves,
                            colorBy = NULL,
                            linetypeBy = NULL,
                            showTm = FALSE,
                            showLegend = FALSE,
                            plotlyCode = NULL,
                            cssFile = NULL,
                            cssText = NULL,
                            interactive = TRUE) {
  assertNames(colnames(meltCurves),
              must.include = c("fdata.name", "tmp", "fluor"))
  meltCurves <- meltCurves %>%
    rename(x = .data$tmp,
           y = .data$fluor)
  if (showTm) {
    assertNames(colnames(meltCurves),
                must.include = c("tm"))
    meltCurves <- meltCurves %>%
      rename(markers = .data$tm)
  }

  renderCurves(inputId,
               label = label,
               curves = meltCurves,
               xAxisTitle = "Temperature",
               yAxisTitle = "-d(RFU)/dT",
               colorBy = colorBy,
               linetypeBy = linetypeBy,
               logScale = FALSE,
               showMarkers = showTm,
               showLegend = showLegend,
               plotlyCode = plotlyCode,
               cssFile = cssFile,
               cssText = cssText,
               interactive = interactive)
}


renderCurves <- function(inputId,
                         label = NULL,
                         curves,
                         xAxisTitle,
                         yAxisTitle,
                         colorBy = NULL,
                         linetypeBy = NULL,
                         logScale = FALSE,
                         showMarkers = FALSE,
                         thBy = NULL,
                         showLegend = FALSE,
                         plotlyCode = NULL,
                         cssFile = NULL,
                         cssText = NULL,
                         interactive = TRUE) {
  assertString(inputId)
  assertString(label, null.ok = TRUE)
  assertDataFrame(curves)
  assertString(xAxisTitle)
  assertString(yAxisTitle)
  assertString(colorBy, null.ok = TRUE)
  assertString(linetypeBy, null.ok = TRUE)
  assertString(thBy, null.ok = TRUE)
  assertNames(colnames(curves),
              must.include = c("fdata.name", "x", "y",
                               colorBy, linetypeBy, thBy))
  assertLogical(logScale)
  assertLogical(showMarkers)
  assertLogical(showLegend)

  # assertLogical(showBaseline)
  assertString(cssFile, null.ok = TRUE)
  assertString(cssText, null.ok = TRUE)
  assertLogical(interactive)

  curves <- curves %>%
    mutate(curveName = sprintf("%s %s %s %s", .data$position,
                               .data$target.dyeId,
                               .data$sample,
                               .data$sample.type)) %>%
    group_by(.data$fdata.name, .data$x) %>%
    mutate(legendGroup = paste(if (!is.null(colorBy)) get(colorBy),
                               if (!is.null(linetypeBy)) get(linetypeBy),
                               collapse = " ")) %>%
    ungroup()

  # ns <- NS(inputId)

  # assign colors to curves
  if (!("color" %in% colnames(curves))) {
    if (!is.null(colorBy)) {
      colorNames <- unique(curves[[colorBy]])
      needNColors <- length(colorNames)
      curvesColors <- tryCatch(
        brewer.pal(needNColors, "Set2"),
        warning = function(w)
          colorRampPalette(brewer.pal(8, "Set2"))(needNColors)
      )
      names(curvesColors) <- colorNames
      curves$color <- curvesColors[curves[[colorBy]]]
    } else {
      curves$color <- "black"
    }
  }

  p <- plot_ly() %>%
    add_trace(data = curves,
              split = ~fdata.name,
              name = ~curveName,
              customdata = ~fdata.name,
              hoverinfo = "x+y+name",
              legendgroup = ~legendGroup,
              x = ~x, y = ~y,
              line = list(color = curves$color),
              linetype = {
                if (is.null(linetypeBy)) NULL
                else ~get(linetypeBy)
              },
              type = "scatter", mode = "lines"
    ) %>%
    plotly::layout(showlegend = showLegend,
                   xaxis = list(title = xAxisTitle),
                   yaxis = list(title = yAxisTitle,
                                type = if (logScale) "log" else "linear"))

  if (showMarkers) {
    assertNames(colnames(curves),
                must.include = c("markers"))
    # prepare markers
    maxX <- max(curves$x, na.rm = TRUE)
    # replace all NA cq with max cycle
    curves[is.na(curves$markers), "markers"] <- maxX
    curves <- curves %>%
      group_by(.data$fdata.name) %>%
      mutate(isMarker =
               replace(rep(FALSE, length(.data$x)),
                       sapply(unique(.data$markers), # set TRUE to closest cyc
                              function(marker)
                                which.min(abs(.data$x - marker))), TRUE))
    cqs <- curves %>%
      filter(.data$isMarker == TRUE)
    p <- add_trace(p,
                   data = cqs,
                   split = ~fdata.name,
                   name = ~curveName,
                   customdata = ~fdata.name,
                   hoverinfo = "x+y+name",
                   legendgroup = ~legendGroup,
                   x = ~x, y = ~y,
                   marker = list(color = cqs$color,
                                 size = 7),
                   type = "scatter", mode = "markers",
                   showlegend = FALSE
    )
  }

  if (!is.null(thBy)) {
    assertNames(colnames(curves),
                must.include = c("quantFluor"))
    maxX <- max(curves$x)
    minX <- min(curves$x)
    ths <- curves %>%
      select(.data$quantFluor, .data[[thBy]]) %>%
      distinct()
    p <- add_segments(p,
                      data = ths,
                      x = minX, xend = maxX,
                      y = ~quantFluor, yend = ~quantFluor,
                      name = ~get(thBy),
                      color = ~get(thBy),
                      hoverinfo = "y+name")

  }

  css <-
    tags$style(type = "text/css",
                    paste0(
                      if (!is.null(cssFile)) {
                        whisker.render(
                          suppressWarnings(
                            readLines(cssFile,
                                      warn = FALSE, encoding = "UTF-8")) %>%
                            paste0(collapse = ""),
                          list(id = inputId)
                        )} else {
                          ""
                        },
                      whisker.render(cssText, list(id = inputId))
                    )
    )

  if (!is.null(plotlyCode)) {
    p <- eval(plotlyCode)
  }

  tagList(
    if (interactive) {
      tags$head(
        singleton(
          includeScript(system.file("/js/renderCurves-bindings.js",
                                    package = "shinyMolBio"))
        ),
        singleton(css)
      )
    } else {
      css
    },
    div(id = inputId, class = "pcr-curves",
        tags$label(label, `for` = inputId),
        p
    )
  )
}

#' Change the value of a render PCR curves control on the client
#'
#' Change the value of a render PCR curves control on the client
#'
#' @param session The \code{session} object passed to function given to
#'   \code{shinyServer}.
#' @param inputId The id of the \code{input} object.
#' @param label The label to set for the input object.
#' @param hideCurves The \code{fdata.names} of the curves to be hiden.
#' @param highlightCurves The \code{fdata.names} of the curves to be
#' highlighted.
#'
#' @author Konstantin A. Blagodatskikh <k.blag@@yandex.ru>
#' @keywords PCR RDML Shiny Input
#' @export
updateCurves <- function(session, inputId,
                         label = NULL,
                         hideCurves = NULL,
                         highlightCurves = NULL) {
  assertClass(session, "ShinySession")
  assertString(inputId)
  assertString(label, null.ok = TRUE)
  assertCharacter(hideCurves, any.missing = FALSE, null.ok = TRUE)
  assertCharacter(highlightCurves, any.missing = FALSE, null.ok = TRUE)
  message <- .dropNulls(list(label = label,
                             hideCurves = hideCurves,
                             highlightCurves = highlightCurves))
  session$sendInputMessage(inputId, message)
}
