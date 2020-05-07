library(shiny)
library(RDML)
library(shinyMolBio)
library(tidyverse)
library(DT)

ui <- fluidPage(
  fluidRow(
    tags$h1("Demo"),
    column(3, fileInput("rdmlFile", "RDML")),
    column(3, actionButton("exmplFile", "Example File"))
  ),
  fluidRow(
    column(6,
           uiOutput("showDyesUI"),
           uiOutput("ampCurvesUI")),
    column(6,
           checkboxInput("polarCoord",
                         "Polar Coordinates"),
           uiOutput("adPlotUI"))
  ),
  fluidRow(
    column(6, uiOutput("ampPlateUI")),
    column(6, dataTableOutput("resultsTable"))
  )
)

server <- function(input, output, session) {
  values <- reactiveValues()

  observeEvent(input$exmplFile, {
    values$path <- system.file("/extdata/test.rdml", package = "shinyMolBio")
  })

  observeEvent(input$rdmlFile$datapath, {
    values$path <- input$rdmlFile$datapath
  })

  rdmlFile <- reactive({
    req(values$path)
    rdml <- RDML$new(values$path)
    # expId <- as.character(rdml$experiment[[1]]$id)
    # runId <- as.character(rdml$experiment[[expId]]$run[[1]]$id)
    list(table =
           rdml$AsTable(
             cq = data$cq,
             endPointRFU = round(mean(tail(data$adp$fpoints$fluor, 5)))) %>%
           mutate(cq = round(cq)) %>%
           group_by(position) %>%
           mutate(genotype = paste(
             if (endPointRFU[1] > 400 && endPointRFU[2] > 400) "AG"
             else if (endPointRFU[1] > 400) "AA"
             else if (endPointRFU[2] > 400) "GG"
             else "NA"
           )),# %>%
         # filter(exp.id == expId
         # &
         # run.id == runId
         # )
         format = rdml$experiment[[1]]$run[[1]]$pcrFormat,
         rdml = rdml)
  })

  output$ampPlateUI <- renderUI({
    req(rdmlFile())
    pcrPlateInput("pcrPlate", "PCR Plate",
                  rdmlFile()$table,
                  pcrFormat = rdmlFile()$format)
  })

  output$ampCurvesUI <- renderUI({
    req(rdmlFile())#, input$pcrPlate2)
    renderAmpCurves("ampCurves", "Amplification Curves",
                    rdmlFile()$rdml$GetFData(rdmlFile()$table,
                                             long.table = TRUE) %>%
                      mutate(quantFluor = 150),
                    # plotlyCode = plotly::layout(yaxis = list(title = "Fluorescence")),
                    colorBy = "sample",
                    linetypeBy = "target.dyeId",
                    showCq = TRUE)
  })

  output$showDyesUI <- renderUI({
    req(rdmlFile())
    selectInput("showDyes", "Dyes",
                choices = rdmlFile()$table$target.dyeId %>% unique(),
                selected = rdmlFile()$table$target.dyeId %>% unique(),
                multiple = TRUE)
  })

  output$adPlotUI <- renderUI({
    req(rdmlFile())#, input$pcrPlate2)
    renderADplot("adPlot", "Allelic Discrimination Plot",
                 rdmlFile()$table,
                 polar = input$polarCoord)
  })

  observeEvent(
    input$pcrPlate,
    {
      toHideCurves <-
        rdmlFile()$table %>%
        filter(!(position %in% input$pcrPlate))

      updateCurves(session,
                   "ampCurves",
                   hideCurves = toHideCurves %>%
                     pull(fdata.name))
      updateADplot(session,
                   "adPlot",
                   hidePoints = toHideCurves %>%
                     pull(position))
    })

  observeEvent(
    input$pcrPlate_hover,
    {
      fdataNames <- rdmlFile()$table %>%
        filter(position %in% input$pcrPlate_hover) %>%
        pull(fdata.name)
      updateCurves(session,
                   "ampCurves",
                   highlightCurves = fdataNames)
      updateADplot(session,
                   "adPlot",
                   highlightPoints = input$pcrPlate_hover)
    }
  )

  output$resultsTable <- renderDataTable({
    req(rdmlFile())
    DT::datatable(
      rdmlFile()$table %>%
        select(position, sample, dye = target.dyeId,
               cq, RFU = endPointRFU, genotype),
      rownames = FALSE,
      options = list(
        rowCallback = DT::JS('function(row, data) {
                             $(row).mouseenter(function(){
                             Shiny.onInputChange("hoverfDataName", data[0]);
                             });
                             $(row).mouseout(function(){
                             Shiny.onInputChange("hoverfDataName", "");
                             });}'))
    )
  })
}

shinyApp(ui, server)