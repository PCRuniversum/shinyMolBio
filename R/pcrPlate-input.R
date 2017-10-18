#' Create a PCR plate input control
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
#' @import RDML shiny tidyverse whisker stringr
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
pcrPlateInput <- function(inputId,
                          label = "lbl",
                          plateDescription,
                          pcrFormat,
                          wellLabelTemplate = "{{sample}}",
                          onHoverWellTextTemplate = "{{position}}\u000A{{sample}}\u000A{{target}}",
                          wellClassTemplate = NULL,
                          wellStyleTemplate = NULL,
                          wellGroupTemplate = "{{sample}}-{{target}}",
                          cssFile = system.file("/css/styles.css", package = "pcrPlate"),
                          cssText = NULL,
                          plateLegend = NULL) {
  ns <- NS(inputId)
  plateDescription <- plateDescription %>%
    ungroup() %>%
    distinct(react.id, .keep_all = TRUE)

  plateDescription <- plateDescription %>%
    group_by(position) %>%
    summarise_all(funs(first)) %>%
    group_by(fdata.name) %>%
    mutate(wellID = ns(sprintf("well_%s", position)))

  htmlPlate <-
    sprintf(paste0('<table id="', ns("pcrPlateTbl"),
                   '" class="pcr-plate-tbl">',
                   '<thead><tr><th id="', ns("toggleall"),
                   '" class="toggle-all"></th>%s</tr></thead>',
                   '<tbody>%s</tbody></table>'
                   # ,'<script>addOnClick("', ns("pcrPlateTbl"),'");</script>'
    ),
    map(1:pcrFormat$columns,
        function(col) {
          sprintf("<th id='%s'>%s</th>",
                  ns(sprintf("col_%02i", col)),
                  col)}) %>%
      paste(collapse = ""),
    map(LETTERS[1:pcrFormat$rows],
        function(row){
          sprintf("<tr><th id='%s' class='%s'>%s</th>%s</tr>",
                  ns(sprintf("row_%s", row)),
                  {
                    if (as.integer(charToRaw(row)) %% 2 == 0) "even-row"
                    else "odd-row"
                  },
                  row,
                  map(1:pcrFormat$columns,
                      function(col) {
                        well <- sprintf("%s%02i", row, col)

                        trow <- plateDescription %>%
                          filter(position == well)
                        if (!length(trow$fdata.name))
                          return("<td class='empty-well'></td>")
                        # paste0(
                        sprintf("<td id='%s' title='%s' group='%s' class='%s' style='%s'>%s</td>",
                                trow$position,
                                whisker.render(onHoverWellTextTemplate,
                                               trow),
                                whisker.render(wellGroupTemplate,
                                               trow) %>%
                                  str_replace_all("[[:punct:]]", ""),
                                whisker.render(wellClassTemplate,
                                               trow),
                                whisker.render(wellStyleTemplate,
                                               trow),
                                whisker.render(wellLabelTemplate,
                                               trow)
                        )
                      }) %>%
                    paste(collapse = ""))
        }) %>%
      paste(collapse = "")) %>%
    HTML
  # print(values)
  tagList(
    tags$head(
      # singleton(
        includeScript(system.file("/js/pcrPlate-input-bindings.js", package = "pcrPlate"))
      # ),
      ,
      # singleton(
        tags$style(type = "text/css",
                   paste0(whisker.render(
                     suppressWarnings(readLines(cssFile, warn = FALSE, encoding = "UTF-8")) %>%
                       paste0( collapse = ""),
                     list(id = inputId)
                   ),
                   whisker.render(cssText, list(id = inputId))
                   )
        ))
    # ),
    ,
    div(id = inputId, class = "pcr-plate",
        tags$label(label, `for` = inputId),
        htmlPlate,
        plateLegend
    )
  )
}

#' Create a PCR plate input control
#'
#' Create an input control for representing PCR plate and dynamically selecting
#' wells inside it.
#'
#' @param session The \code{session} object passed to function given to \code{shinyServer}.
#' @param inputId The id of the \code{input} object.
#' @param label The label to set for the input object.
#' @param selection The positions of the wells to be selected.
#'
#' @author Konstantin A. Blagodatskikh <k.blag@@yandex.ru>
#' @keywords PCR RDML Shiny Input
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#' ui <- fluidPage(
#'     pcrPlateInput("plate1",
#'                    "Plate 1",
#'                    RDML$new(system.file("/extdata/stepone_std.rdml", package = "RDML"))$AsTable(),
#'                   pcrFormatType$new(8,12,labelFormatType$new("ABC"),
#'                                          labelFormatType$new("123"))),
#'    verbatimTextOutput("selected"),
#'    actionButton("selectWellBtn",
#'                 "Select Well A01-A03")
#'  )
#'  server <- function(input, output) {
#'    output$selected <- renderText({ input$plate1 })
#'
#'    observeEvent(
#'                input$selectRandomWellBtn,
#'                {
#'            updatePcrPlateInput(session,
#'            "plate1",
#'            selection = c("A01", "A02", "A03"))
#'   })
#'  }
#'  shinyApp(ui, server)
#' }
#' @export
updatePcrPlateInput <- function(session, inputId,
                                label = NULL, selection = NULL) {
  message <- .dropNulls(list(label = label, selection = selection))
  session$sendInputMessage(inputId, message)
}

# Given a vector or list, drop all the NULL items in it
.dropNulls <- function(x) {
  x[!vapply(x, is.null, FUN.VALUE = logical(1))]
}