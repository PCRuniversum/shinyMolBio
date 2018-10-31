library(shiny)
library(RDML)
library(shinyMolBio)
library(tidyverse)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      fileInput("rdmlFile", "RDML"),
      actionButton("exmplFile", "Example File")
    ),
    mainPanel(
      uiOutput("plate1"),
      textOutput("plate1Selected"),
      uiOutput("plate2"),
      textOutput("plate2Selected"),
      actionButton("selectRandomWellBtn",
                   "Select Random Well"),
      uiOutput("curves1")
    )
  )
)

server <- function(input, output, session) {
  values <- reactiveValues()

  observeEvent(input$exmplFile, {
    values$path <- system.file("/extdata/stepone_std.rdml", package = "RDML")
  })

  observeEvent(input$rdmlFile$datapath, {
    values$path <- input$rdmlFile$datapath
  })

  rdmlFile <- reactive({
    req(values$path)
    rdml <- RDML$new(values$path)
    expId <- as.character(rdml$experiment[[1]]$id)
    runId <- as.character(rdml$experiment[[expId]]$run[[1]]$id)
    list(table =  rdml$AsTable(cq = data$cq) %>%
           filter(exp.id == expId &
                    run.id == runId),
         format = rdml$experiment[[expId]]$run[[runId]]$pcrFormat,
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
    pcrPlateInput("pcrPlate2", "Plate 2",
                  rdmlFile()$table %>%
                    filter(target.dyeId == "FAM") %>%
                    group_by(fdata.name) %>%
                    mutate(sampleType = sample.type,  #whisker does not support dots!
                           cq = {
                             if(is.na(cq)) 100
                             else cq },
                           mark = {if (cq < 30) return("<span class='filled-circle1'></span>")
                             if (cq > 35) return("<span class='filled-circle2'></span>")
                             else ""}),
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
                  plateLegend =
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

  observeEvent(
    input$selectRandomWellBtn,
    {
      updatePcrPlateInput(
        session,
        "pcrPlate2",
        selection = sample(unique(rdmlFile()$table$position), 1))
    })

  output$plate2Selected <- renderText({
    req(input$pcrPlate2)
    paste("Selected wells:",
          paste(input$pcrPlate2, collapse = ", "))
  })

  output$curves1 <- renderUI({
    req(rdmlFile(), input$pcrPlate2)
    pcrCurvesInput("pcrCurves1", "curves1",
                   rdmlFile()$rdml$GetFData(long.table = TRUE),
                   selected = input$pcrPlate2,
                   showCq = FALSE)
  })

}

shinyApp(ui, server)