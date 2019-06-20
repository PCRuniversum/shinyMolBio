#' Create a PCR plate input control
#'
#' Create an input control for representing PCR plate and dynamically selecting
#' wells inside it.
#'
#' @usage pcrPlateInput(inputId,
#'        label = NULL,
#'        plateDescription,
#'        pcrFormat = pcrFormatType$new(8, 12, labelFormatType$new("ABC"),
#'         labelFormatType$new("123")),
#'        selection = NULL,
#'        wellLabelTemplate = "{{sample}}",
#'        onHoverWellTextTemplate = "{{position}}\u000A{{sample}}\u000A{{targets}}",
#'        wellClassTemplate = NULL,
#'        wellStyleTemplate = NULL,
#'        wellGroupTemplate = "{{sample}}-{{targets}}",
#'        cssFile = system.file("/css/pcrPlateInputStyle.css",
#'                  package = "shinyMolBio"),
#'        cssText = NULL,
#'        legend = NULL,
#'        interactive = TRUE)
#'
#' @param inputId The \code{input} slot that will be used to access the selected
#'   wells positions.
#' @param label Display label for the control, or \code{NULL} for no label.
#' @param plateDescription Plate description - basicly output from \code{RDML
#'   AsTable()} function.
#' @param pcrFormat PCR plate parametrs. Should be \code{pcrFormatType}.
#' @param selection Set preselected wells (e.g. \code{c("A01", "A02")} or \code{c(1, 2)})
#' @param wellLabelTemplate Template of the well label.
#' @param onHoverWellTextTemplate Template of the text on hover.
#' @param wellClassTemplate Template of the well class (css class).
#' @param wellStyleTemplate Template of the well style (css).
#' @param wellGroupTemplate Template of the well group for selecting.
#' @param cssFile Path to the css styles file.
#' @param cssText CSS styles as text.
#' @param legend Plate legend (any HTML content).
#' @param interactive Should be this \code{pcrPlate} interactive or not.
#' @return A PCR plate control that can be added to a UI definition.
#'
#' @author Konstantin A. Blagodatskikh <k.blag@@yandex.ru>
#' @keywords PCR RDML Shiny Input
#' @import RDML shiny dplyr stringr checkmate
#' @importFrom purrr map
#' @importFrom  whisker whisker.render
#'
#' @family input elements
#' @seealso \code{\link{updatePcrPlateInput}}
#'
#' @examples
#' ## Only run examples in interactive R sessions
#' library(RDML)
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
                          label = NULL,
                          plateDescription,
                          pcrFormat = pcrFormatType$new(8, 12, labelFormatType$new("ABC"), labelFormatType$new("123")),
                          selection = NULL,
                          wellLabelTemplate = "{{sample}}",
                          onHoverWellTextTemplate = "{{position}}\u000A{{sample}}\u000A{{targets}}",
                          wellClassTemplate = NULL,
                          wellStyleTemplate = NULL,
                          wellGroupTemplate = "{{sample}}-{{targets}}",
                          cssFile = system.file("/css/pcrPlateInputStyle.css",
                                                package = "shinyMolBio"),
                          cssText = NULL,
                          legend = NULL,
                          interactive = TRUE) {
  assertString(inputId)
  assertString(label, null.ok = TRUE)
  assertDataFrame(plateDescription)
  assertClass(pcrFormat, "pcrFormatType")
  assert(checkNull(selection),
         checkNumeric(selection),
         checkCharacter(selection))
  assertString(wellLabelTemplate, null.ok = TRUE)
  assertString(onHoverWellTextTemplate, null.ok = TRUE)
  assertString(wellClassTemplate, null.ok = TRUE)
  assertString(wellStyleTemplate, null.ok = TRUE)
  assertString(wellGroupTemplate, null.ok = TRUE)
  assertString(cssFile)
  assertString(cssText, null.ok = TRUE)
  assert(checkNull(legend),
         checkClass(legend, "shiny.tag"))
  assertFlag(interactive)

  ns <- NS(inputId)


  plateDescription <- plateDescription %>%
    group_by(.data$position) %>%
    mutate(targets = paste(.data$target, collapse = "; "),
           target.dyeIds = paste(.data$target.dyeId, collapse = "; ")) %>%
    distinct(.data$react.id, .keep_all = TRUE)

  selectionColumn <- {
    if (is.numeric(selection))
      "react.id"
    else
      "position"
  }

  plateDescription[, "selection"] <- ""
  plateDescription[plateDescription[[selectionColumn]] %in% selection,
                   "selection"] <- " selected-well"

  htmlPlate <-
    sprintf(paste0('<table id="', ns("pcrPlateTbl"),
                   '" class="pcr-plate-tbl',
                   {
                     if (interactive)
                       ' interactive'
                   },
                   '"><thead><tr><th id="', ns("toggleall"),
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

                        trow <-
                          filter(plateDescription, .data$position == well)
                        if (!length(trow$fdata.name))
                          return("<td class='empty-well'></td>")
                        # paste0(
                        sprintf("<td id='%s' title='%s' group='%s' class='%s%s' style='%s'>%s</td>",
                                trow$position,
                                whisker.render(onHoverWellTextTemplate,
                                               trow),
                                gsub(pattern = "[[:punct:]]",
                                     whisker.render(wellGroupTemplate, trow),
                                     replacement = ""),
                                whisker.render(wellClassTemplate,
                                               trow),
                                trow$selection,
                                whisker.render(wellStyleTemplate,
                                               trow),
                                whisker.render(wellLabelTemplate,
                                               trow)
                        )
                      }) %>%
                    paste(collapse = ""))
        }) %>%
      paste(collapse = "")) %>%
    HTML()
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
        htmlPlate,
        legend
    )
  )
}

#' Change the value of a PCR plate input control on the client
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
#' @export
#' @examples
#' ## Only run examples in interactive R sessions
#' library(RDML)
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
#'  server <- function(input, output, session) {
#'    output$selected <- renderText({ input$plate1 })
#'
#'    observeEvent(
#'                input$selectWellBtn,
#'                {
#'            updatePcrPlateInput(session,
#'            "plate1",
#'            selection = c("A01", "A02", "A03"))
#'   })
#'  }
#'  shinyApp(ui, server)
#' }
updatePcrPlateInput <- function(session, inputId,
                                label = NULL,
                                selection = NULL) {
  assertClass(session, "ShinySession")
  assertString(inputId)
  assertString(label, null.ok = TRUE)
  assert(checkNull(selection),
         checkNumeric(selection),
         checkCharacter(selection))
  message <- .dropNulls(list(label = label, selection = selection))
  session$sendInputMessage(inputId, message)
}