#' Renders an allelic discrimination viewer
#'
#' Renders a reactive allelic discrimination plot that is suitable for
#' assigning to an \code{UI output} slot.
#'
#' @usage renderADplot(inputId, label = NULL, adData,
#'         targetColumn = "target.dyeId", xAxisTarget = "FAM",
#'         yAxisTarget = "HEX", valueColumn = "endPointRFU",
#'         colorBy = "genotype", polar = FALSE, showLegend = FALSE,
#'         plotlyCode = NULL, cssFile = NULL, cssText = NULL,
#'         interactive = TRUE)
#'
#' @param inputId The \code{input} slot that will be used to modify plot.
#' @param label Display label for the control, or \code{NULL} for no label.
#' @param adData Allelic descrimination data with
#'   \code{RDML$AsTable()} format.
#' @param targetColumn Column name that contains axis splitting (dye or target).
#' @param xAxisTarget X axis target (dye) name.
#' @param yAxisTarget X axis target (dye) name.
#' @param valueColumn Column name that contains discrimination value (RFU or cq).
#' @param colorBy Column name that contains color levels data.
#' @param polar Enables polar coordinates.
#' @param showLegend Show plot legend.
#' @param plotlyCode Your quoted custom plotly code.
#' @param cssFile Path to the css styles file.
#' @param cssText CSS styles as text.
#' @param interactive Should be this \code{adPLot} interactive or not.
#'
#' @author Konstantin A. Blagodatskikh <k.blag@@yandex.ru>
#' @keywords PCR RDML Shiny Input
#' @importFrom tidyr spread
#'
#' @family render elements
#' @seealso \code{\link{updateADplot}}
#'
#' @export
#' @examples
#' library(RDML)
#' rdml <- RDML$new(system.file("/extdata/test.rdml", package = "shinyMolBio"))
#' tbl <- rdml$AsTable(endPointRFU = mean(tail(data$adp$fpoints$fluor, 5)))
#' tbl <- group_by(tbl, position)
#' tbl <- mutate(tbl,
#'               genotype = paste(
#'                 if (endPointRFU[1] > 400 && endPointRFU[2] > 400) "AG"
#'                 else if (endPointRFU[1] > 400) "AA"
#'                 else if (endPointRFU[2] > 400) "GG"
#'                 else "NA"
#'               ))
#' renderADplot("f", "aa", tbl, polar = TRUE, showLegend = TRUE)

renderADplot <- function(inputId,
                         label = NULL,
                         adData,
                         targetColumn = "target.dyeId",
                         xAxisTarget = "FAM",
                         yAxisTarget = "HEX",
                         valueColumn = "endPointRFU",
                         colorBy = "genotype",
                         polar = FALSE,
                         showLegend = FALSE,
                         plotlyCode = NULL,
                         cssFile = NULL,
                         cssText = NULL,
                         interactive = TRUE) {
  assertString(inputId)
  assertString(label, null.ok = TRUE)
  assertDataFrame(adData)
  assertString(xAxisTarget)
  assertString(yAxisTarget)
  assertString(colorBy, null.ok = TRUE)
  assertNames(colnames(adData),
              must.include = c("position", targetColumn,
                               valueColumn,
                               colorBy))
  assert(all(c(xAxisTarget, yAxisTarget) %in%
               adData[[targetColumn]]))
  assertLogical(polar)
  assertLogical(showLegend)
  assertString(cssFile, null.ok = TRUE)
  assertString(cssText, null.ok = TRUE)
  assertLogical(interactive)

  # adData <- adData %>%
  #   filter(get(targetColumn) == xAxisTarget |
  #            get(targetColumn) == yAxisTarget) %>%
  #   mutate(pointName = sprintf("%s %s %s", .data$position,
  #                              .data$sample,
  #                              .data$sample.type))
  adData <- as.data.table(adData)[targetColumn == xAxisTarget |
                     targetColumn == yAxisTarget,
                   pointName := sprintf("%s %s %s", position,
                                       sample,
                                       sample.type)]

  adData2 <- data.frame(
    position = adData$position,
    pointName = adData$pointName,
    targetColumn = adData[[targetColumn]],
    valueColumn = adData[[valueColumn]]
  )
  if (!is.null(colorBy))
    adData2$colorBy <- adData[[colorBy]]
  if (!is.null(adData[["color"]]))
    adData2$color <- adData[["color"]]
  adData2 <- adData2 %>%
    spread(targetColumn, valueColumn) #%>%
  # group_by(.data$fdata.name) %>%
  # mutate(legendGroup = if (!is.null(colorBy)) color
  #        else "") %>%
  # ungroup()

  # ns <- NS(inputId)
  # assign colors to points
  if (is.null(adData2[["color"]])) {
    if (!is.null(colorBy)) {
      colorNames <- unique(adData2[["colorBy"]])
      needNColors <- length(colorNames)
      pointsColors <- tryCatch(
        brewer.pal(needNColors, "Set2"),
        warning = function(w)
          colorRampPalette(brewer.pal(8, "Set2"))(needNColors)
      )
      names(pointsColors) <- colorNames
      adData2$color <- pointsColors[adData2[["colorBy"]]]
      # browser()
    } else {
      adData2$color <- "black"
    }
  }

  if (polar) {
    adData2$distance <- sqrt(adData2[[xAxisTarget]]^2 +
                               adData2[[yAxisTarget]]^2)
    adData2$angle <-
      abs(atan(adData2[[yAxisTarget]] /
                 adData2[[xAxisTarget]]) * 180 / pi)
  }

  p <- plot_ly() %>%
    add_markers(data = adData2,
              # split = ~pointName,
              name = ~pointName,
              # legendgroup = ~colorBy,
              customdata = ~position,
              # hoverinfo = "x+y+name",
              x = if (polar) ~angle
              else ~get(xAxisTarget),
              y = if (polar) ~distance
              else ~get(yAxisTarget),
              opacity = 1,
              visible = TRUE,
              marker = list(color = adData2$color,
                            size = 7),
              type = "scatter", mode = "markers"
    ) %>%
    plotly::layout(showlegend = showLegend,
                   xaxis = list(title = if (polar) "Angle"
                                else xAxisTarget),
                   yaxis = list(title = if (polar) "Distance"
                                else yAxisTarget))

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

  tl <- tagList(
    if (interactive) {
      tags$head(
        singleton(
          includeScript(system.file("js/renderADplot-bindings.js",
                                    package = "shinyMolBio"))
        ),
        singleton(css)
      )
    } else {
      css
    },
    div(id = inputId, class = "ad-plot",
        tags$label(label, `for` = inputId),
        p
    )
  )
  class(tl) <- c("adPlot", class(tl))
  tl
}

#' Printing adPlot
#'
#' Print a \code{adPlot}
#'
#' @usage ## S3 method for class 'adPlot'
#' print(x)
#'
#' @param x object of class \code{adPlot}
#'
#' @author Konstantin A. Blagodatskikh <k.blag@@yandex.ru>
#' @keywords PCR RDML Shiny Input
#'
#' @seealso \code{\link{renderAmpCurves}}, \code{\link{renderMeltCurves}}
#'
#' @export
print.adPlot <- function(curves)
  print(curves[[2]][[3]][[2]])

#' Change the value of a render ADplot control on the client
#'
#' Change the value of a render ADplot control on the client
#'
#' @param session The \code{session} object passed to function given to
#'   \code{shinyServer}.
#' @param inputId The id of the \code{input} object.
#' @param label The label to set for the input object.
#' @param hidePoints The \code{position} of the points to be hiden.
#' @param highlightPoints The \code{position} of the points to be
#' highlighted.
#'
#' @author Konstantin A. Blagodatskikh <k.blag@@yandex.ru>
#' @keywords PCR RDML Shiny Input
#' @export
updateADplot <- function(session, inputId,
                         label = NULL,
                         hidePoints = NULL,
                         highlightPoints = NULL) {
  assertClass(session, "ShinySession")
  assertString(inputId)
  assertString(label, null.ok = TRUE)
  assertCharacter(hidePoints, any.missing = FALSE, null.ok = TRUE)
  assertCharacter(highlightPoints, any.missing = FALSE, null.ok = TRUE)
  message <- .dropNulls(list(label = label,
                             hidePoints = hidePoints,
                             highlightPoints = highlightPoints))
  session$sendInputMessage(inputId, message)
}

