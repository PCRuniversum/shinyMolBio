## ---- results='hide', message=FALSE, warning=FALSE-----------------------
library(shinyMolBio)
library(tidyverse)
library(RDML)
# load RDML file
rdml <- RDML$new(system.file("/extdata/stepone_std.rdml", package = "RDML"))

## ---- results='asis'-----------------------------------------------------
# create Shiny UI
renderAmpCurves(inputId = "firstLook", # Shiny input ID
              label = "Example", # optional plot label 
              ampCurves = rdml$GetFData(long.table = TRUE), # Amplification curves
              interactive = FALSE
) 

## ---- results='asis'-----------------------------------------------------
# create Shiny UI
renderAmpCurves(inputId = "color1",
              ampCurves = rdml$GetFData(long.table = TRUE),
              colorBy = "sample", # sample name will define color
              interactive = FALSE
) 

renderAmpCurves(inputId = "color2",
              ampCurves = rdml$GetFData(long.table = TRUE) %>% 
                mutate(color = "red"), # All curves will be red
              interactive = FALSE
)

## ---- results='asis'-----------------------------------------------------
# create Shiny UI
renderAmpCurves(inputId = "linetype",
              ampCurves = rdml$GetFData(long.table = TRUE),
              linetypeBy = "sample.type", # sample.type will define color
              interactive = FALSE
) 

## ---- results='asis'-----------------------------------------------------
# create Shiny UI
renderAmpCurves(inputId = "cq", 
              ampCurves = rdml$GetFData(
                rdml$AsTable(cq = data$cq), # Get Cq values from file
                long.table = TRUE), 
              showCq = TRUE, # Add Cq markers to curves
              colorBy = "sample",
              interactive = FALSE
) 

