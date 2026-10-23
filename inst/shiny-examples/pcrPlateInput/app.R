library(shiny)
library(RDML7)
library(shinyMolBio)
library(tidyverse)
library(DT)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      fileInput("rdmlFile", "RDML"),
      actionButton("exmplFile", "Example File")
    ),
    mainPanel(
      uiOutput("plate1"),
      textOutput("plate1Selected"),
      fluidRow(
        column(6,
               uiOutput("showDyesUI"),
               uiOutput("plate2"),
               textOutput("plate2Selected"),
               actionButton("selectRandomWellBtn",
                            "Select Random Well")
        ),
        column(6,
               checkboxInput("logScale", "Log Scale"),
               uiOutput("curves1")
        ),
        dataTableOutput("table"),
        verbatimTextOutput("hoverfDataName")
      ),
      uiOutput("mcurves1")
    )
  )
)

server <- function(input, output, session) {
  values <- reactiveValues()

  observeEvent(input$exmplFile, {
    values$path <-
      system.file("/extdata/BioRad_qPCR_melt.rdml",
                  package = "RDML7")
      #system.file("/extdata/test.rdml", package = "shinyMolBio")
  })

  observeEvent(input$rdmlFile$datapath, {
    values$path <- input$rdmlFile$datapath
  })

  rdmlFile <- reactive({
    req(values$path)
    rdml <- readRDML(values$path)
    expId <- as.character(rdml@experiment[[1]]@id)
    # runId <- as.character(rdml$experiment[[expId]]$run[[1]]$id)
    list(table =  asTable(rdml, cq = data$cq) %>%
           filter(.data$expId == .env$expId
                  # &
                  # run.id == runId
           ),
         format = rdml@experiment[[expId]]@run[[1]]@pcrFormat,
         rdml = rdml)
  })

  output$plate1 <- renderUI({
    req(rdmlFile())
    pcrPlateInput("pcrPlate1", "Plate 1",
                  rdmlFile()$table,
                  pcrFormat = rdmlFile()$format)
  })

  output$plate1Selected <- renderText({
    req(input$pcrPlate1)
    paste("Selected wells:",
          paste(input$pcrPlate1, collapse = ", "))
  })

  output$plate2 <- renderUI({
    req(rdmlFile())
    isolate({
      pcrPlateInput("pcrPlate2", "Plate 2",
                    rdmlFile()$table %>%
                      group_by(position) %>%
                      mutate(sampleType = sampleType,
                             cq = {
                               ifelse(is.na(cq), 100, cq) },
                             mark = {
                               if (any(cq <= 30)) "<span class='filled-circle1'></span>"
                               else if (any(cq >= 35)) "<span class='filled-circle2'></span>"
                               else ""
                             }),
                    pcrFormat = rdmlFile()$format,
                    wellLabelTemplate = "{{{mark}}}{{sample}}",
                    wellClassTemplate = "{{sampleType}}",
                    cssFile = "",
                    cssText = "#{{id}} td.selected-well{border: 2px solid red !important;}
               #{{id}} .ntc{background-color: Plum ;}
               #{{id}} .unkn{background-color: LightGrey ;}
               #{{id}} .pos{background-color: PaleGreen ;}
               #{{id}} .neg{background-color: Salmon ;}
               #{{id}} .std{background-color: DeepSkyBlue ;}
               #{{id}} .filled-circle1 {padding: 2px 11px;
                  border-radius: 100%; background-color: Maroon;}
               #{{id}} .filled-circle2 {padding: 2px 11px;
                  border-radius: 100%; background-color: Orange;}",
                    # tags$div(tags$span(class = ".filled-circle1"), "Sdsf")
                    legend =
                      tags$div(
                        tags$span(class = "filled-circle1"), "Cq <= 30",
                        tags$br(),
                        tags$span(class = "filled-circle2"), "Cq >= 35",
                        tags$br(),
                        tags$span(
                          tags$span(class = "ntc", "NTC"),
                          tags$span(class = "unkn", "Unknown"),
                          tags$span(class = "pos", "Positive"),
                          tags$span(class = "neg", "Negative"),
                          tags$span(class = "std", "Standard")
                        )
                      )
      )
    })
  })

  observeEvent(
    input$selectRandomWellBtn,
    {
      updatePcrPlateInput(
        session,
        "pcrPlate2",
        selection = sample(unique(rdmlFile()$table$position), 1))
    })

  output$curves1 <- renderUI({
    req(rdmlFile())#, input$pcrPlate2)
    renderAmpCurves("pcrCurves1", "curves1",
                    getFData(rdmlFile()$rdml, rdmlFile()$table,
                                             longTable = TRUE) %>%
                      mutate(quantFluor = 150),
                    # plotlyCode = plotly::layout(yaxis = list(title = "Fluorescence")),
                    colorBy = "sample",
                    linetypeBy = "targetDyeId",
                    showCq = TRUE,
                    showLegend = TRUE,
                    logScale = input$logScale)
  })

  output$mcurves1 <- renderUI({
    req(rdmlFile())#, input$pcrPlate2)
    renderMeltCurves("meltCurves1", "curves1",
                     getFData(
                       rdmlFile()$rdml, rdmlFile()$table,
                       dpType = "mdp",
                       longTable = TRUE) [,
                                           fluor := c(NA, diff(fluor)),
                                           by = fdataName],
                     # plotlyCode = plotly::layout(yaxis = list(title = "Fluorescence")),
                     colorBy = "sample",
                     linetypeBy = "targetDyeId")
  })

  output$showDyesUI <- renderUI({
    req(rdmlFile())
    selectInput("showDyes", "Dyes",
                choices = rdmlFile()$table$targetDyeId %>% unique(),
                selected = rdmlFile()$table$targetDyeId %>% unique(),
                multiple = TRUE)
  })

  output$plate2Selected <- renderText({
    req(input$pcrPlate2)
    input$showDyes
    isolate({
      toHideCurves <-
        rdmlFile()$table %>%
        filter(!(position %in% input$pcrPlate2) |
                 !(targetDyeId %in% input$showDyes)) %>%
        .$fdataName
      updateCurves(session,
                   "pcrCurves1",
                   hideCurves = toHideCurves)
      updateCurves(session,
                   "meltCurves1",
                   hideCurves = toHideCurves)
      paste("Selected wells:",
            paste(input$pcrPlate2, collapse = ", "))
    })
  })

  output$hoverfDataName <- renderText({
    hoverfDataName <- input$hoverfDataName
    updateCurves(session,
                 "pcrCurves1",
                 highlightCurves = hoverfDataName)
    updateCurves(session,
                 "meltCurves1",
                 highlightCurves = hoverfDataName)
    updatePcrPlateInput(
      session,
      "pcrPlate2",
      highlighting = str_sub(hoverfDataName, end = 3))
    paste("Highlighted:", hoverfDataName)
  })

  observeEvent(
    input$pcrPlate2_hover,
    {
      fdataNames <- rdmlFile()$table %>%
        filter(position %in% input$pcrPlate2_hover) %>%
        .$fdataName
      updateCurves(session,
                   "pcrCurves1",
                   highlightCurves = fdataNames)

      updateCurves(session,
                   "meltCurves1",
                   highlightCurves = fdataNames)
    }
  )

  output$table <- renderDataTable({
    req(rdmlFile())
    DT::datatable(rdmlFile()$table,
                  rownames = FALSE,
                  options = list(
                    rowCallback = DT::JS('function(row, data) {
                                           $(row).mouseenter(function(){
                                           Shiny.onInputChange("hoverfDataName", data[0]);
                                           });
                                           $(row).mouseout(function(){
                                           Shiny.onInputChange("hoverfDataName", "");
                                           });
  }')
                  )
    )
  })
}

shinyApp(ui, server)
