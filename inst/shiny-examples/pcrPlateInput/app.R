library(shiny)
library(RDML)
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
    values$path <- system.file("/extdata/test.rdml", package = "shinyMolBio")
  })

  observeEvent(input$rdmlFile$datapath, {
    values$path <- input$rdmlFile$datapath
  })

  rdmlFile <- reactive({
    req(values$path)
    rdml <- RDML$new(values$path)
    expId <- as.character(rdml$experiment[[1]]$id)
    # runId <- as.character(rdml$experiment[[expId]]$run[[1]]$id)
    list(table =  rdml$AsTable(cq = data$cq) %>%
           filter(exp.id == expId
                  # &
                  # run.id == runId
           ),
         format = rdml$experiment[[expId]]$run[[1]]$pcrFormat,
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
                      filter(target.dyeId == rdmlFile()$table$target.dyeId[1]) %>%
                      group_by(fdata.name) %>%
                      mutate(sampleType = sample.type,  #whisker does not support dots!
                             cq = {
                               if (is.na(cq)) 100
                               else cq },
                             mark = {
                               if (cq < 30) "<span class='filled-circle1'></span>"
                               else if (cq > 35) "<span class='filled-circle2'></span>"
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
                        tags$span(class = "filled-circle1"), "Cq < 30",
                        tags$br(),
                        tags$span(class = "filled-circle2"), "Cq > 35",
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
                    rdmlFile()$rdml$GetFData(rdmlFile()$table,
                                             long.table = TRUE) %>%
                      mutate(quantFluor = 150),
                    # plotlyCode = plotly::layout(yaxis = list(title = "Fluorescence")),
                    colorBy = "sample",
                    linetypeBy = "target.dyeId",
                    showCq = TRUE,
                    showLegend = TRUE,
                    logScale = input$logScale)
  })

  output$mcurves1 <- renderUI({
    req(rdmlFile())#, input$pcrPlate2)
    renderMeltCurves("meltCurves1", "curves1",
                     rdmlFile()$rdml$GetFData(rdmlFile()$table,
                                              dp.type = "mdp",
                                              long.table = TRUE) %>%
                       group_by(fdata.name) %>%
                       mutate(fluor = c(NA, diff(fluor))),
                     # plotlyCode = plotly::layout(yaxis = list(title = "Fluorescence")),
                     colorBy = "sample",
                     linetypeBy = "target.dyeId")
  })

  output$showDyesUI <- renderUI({
    req(rdmlFile())
    selectInput("showDyes", "Dyes",
                choices = rdmlFile()$table$target.dyeId %>% unique(),
                selected = rdmlFile()$table$target.dyeId %>% unique(),
                multiple = TRUE)
  })

  output$plate2Selected <- renderText({
    req(input$pcrPlate2)
    input$showDyes
    isolate({
      toHideCurves <-
        rdmlFile()$table %>%
        filter(!(position %in% input$pcrPlate2) |
                 !(target.dyeId %in% input$showDyes)) %>%
        .$fdata.name
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
        .$fdata.name
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