## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
options(knitr.table.format = "html") 

## ------------------------------------------------------------------------
library(shinyMolBio)
library(tidyverse)
library(RDML)
# load RDML file
rdml <- RDML$new(system.file("/extdata/stepone_std.rdml", package = "RDML"))

## ---- results='asis'-----------------------------------------------------
# create Shiny control
pcrPlateInput(inputId = "firstLook", # Shiny input ID
              label = "Example", # optional plate label 
              plateDescription = rdml$AsTable(), # plate description (wells content)
              selection = c("A01", "C08") # optional preselected wells
) 

## ---- results='asis'-----------------------------------------------------
pcrPlateInput(inputId = "testPlateSel", 
              label = "Selection",
              plateDescription = rdml$AsTable(),
              selection = 1:12 # optional preselected wells
) 

## ----eval=FALSE----------------------------------------------------------
#  /* Global plate style */
#    table.pcr-plate-tbl{
#      width: 100%;
#      border-collapse: separate;
#      border-spacing: 1px;
#    }
#  /* Global plate style */
#    table.pcr-plate-tbl td, table.pcr-plate-tbl th{
#      width: 4rem;
#      height: 2rem;
#      border: 2px solid #ccc;
#      text-align: center;
#    }
#  /* Plate well style */
#    table.pcr-plate-tbl td{
#      max-width: 4rem;
#      word-wrap: break-word;
#    }
#  /* Plate header */
#    table.pcr-plate-tbl th{
#      border-color: white;
#      color: white;
#    }
#  /* Plate header even column*/
#    table.pcr-plate-tbl thead th:nth-child(even){
#      background-color: #3CA9E8;
#    }
#  /* Plate header odd column */
#    table.pcr-plate-tbl thead th:nth-child(odd){
#      background-color: #178ACC;
#    }
#  /* Plate header odd row */
#    th.odd-row{
#      background-color: #3CA9E8;
#    }
#  /* Plate header even row */
#    th.even-row{
#      background-color: #178ACC;
#    }
#  /* Selected well */
#    td.selected-well{
#      border: 2px solid black !important;
#    }
#  /* Toggle all sign */
#    th.toggle-all {
#      background: transparent !important;
#      position: relative;
#    }
#  /* Toggle all sign */
#    th.toggle-all:after {
#      content: "";
#      position: absolute;
#      bottom: 0;
#      right: 0;
#      width: 0;
#      height: 0;
#      display: block;
#      border-left: 1em solid transparent;
#      border-bottom: 1em solid transparent;
#      border-bottom: 1em solid grey;
#    }

## ---- results='asis'-----------------------------------------------------
pcrPlateInput(inputId = "customCSS", # Shiny input ID
              label = "Custom CSS: red selection, yellow background",
              plateDescription = rdml$AsTable(),
              selection = c("A01", "C08"), # optional preselected wells
              cssFile = "",
              cssText = "#{{id}} td.selected-well{border: 2px solid red !important;}
                        #{{id}} table.pcr-plate-tbl td{background-color: #ffffe0;}
              "
) 

## ---- results='asis'-----------------------------------------------------
library(shiny) # to use tags

plateDescription <- rdml$AsTable(cq = round(data$cq)) %>%
  group_by(fdata.name) %>%
  mutate(sampleType = sample.type,  # whisker does not support dots!
         dyeId = target.dyeId,   # whisker does not support dots!
         mark = {if (cq < 30) return("<span class='filled-circle1'></span>")
           if (cq > 35) return("<span class='filled-circle2'></span>")
           else ""})

uniqSamples <- unique(plateDescription$sample)
sampleColors <- topo.colors(length(uniqSamples), alpha = 0.3)
names(sampleColors) <- uniqSamples
plateDescription <- plateDescription %>% 
  group_by(sample) %>% 
  mutate(color = sampleColors[sample] %>%
           col2rgb() %>%
           .[,1] %>% c(0.2) %>% 
           paste(collapse = ","))

pcrPlateInput("customLabel", "Custom labels and marks",
              plateDescription,
              wellLabelTemplate = "{{{mark}}}{{sample}} Cq={{cq}}",
              onHoverWellTextTemplate = "{{target}}: {{dyeId}}",
              wellClassTemplate = "{{sampleType}}",
              wellStyleTemplate = "background-color:rgba({{color}});",
              cssFile = "",
              cssText = "#{{id}} td.selected-well{border: 2px solid red !important;}
               #{{id}} .ntc{border: 3px solid Plum;}
               #{{id}} .unkn{border: 3px solid Salmon;}
               #{{id}} .pos{border: 3px solid PaleGreen ;}
               #{{id}} .neg{border: 3px solid LightGrey ;}
               #{{id}} .std{border: 3px solid DeepSkyBlue ;}
               #{{id}} .filled-circle1 {padding: 2px 11px;
                  border-radius: 100%; background-color: Maroon;}
               #{{id}} .filled-circle2 {padding: 2px 11px;
                  border-radius: 100%; background-color: Orange;}",
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

