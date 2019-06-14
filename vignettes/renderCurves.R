## ---- results='hide', message=FALSE, warning=FALSE-----------------------
library(shinyMolBio)
library(tidyverse)
library(RDML)
library(chipPCR)
# load RDML file
rdml <- RDML$new(system.file("/extdata/stepone_std.rdml", package = "RDML"))

## ---- results='asis'-----------------------------------------------------
renderAmpCurves(inputId = "firstLook", # Shiny input ID
              label = "Example", # optional plot label 
              ampCurves = rdml$GetFData(long.table = TRUE), # Amplification curves
              interactive = FALSE
) 

## ---- results='asis'-----------------------------------------------------
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
renderAmpCurves(inputId = "linetype",
              ampCurves = rdml$GetFData(long.table = TRUE),
              linetypeBy = "sample.type", # sample.type will define color
              interactive = FALSE
) 

## ---- results='asis'-----------------------------------------------------
renderAmpCurves(inputId = "cq", 
              ampCurves = rdml$GetFData(
                rdml$AsTable(cq = data$cq), # Get Cq values from file
                long.table = TRUE), 
              showCq = TRUE, # Add Cq markers to curves
              colorBy = "sample",
              interactive = FALSE
) 

## ---- results='asis'-----------------------------------------------------
# Create function for curves preprocessing
dataType$set("public", "Process",
             function(thValue) {
               # Subtract background
               private$.adp$fpoints$fluor <- 
                 CPP(self$adp$fpoints$cyc,
                     self$adp$fpoints$fluor,
                     bg.range = c(10,20))$y.norm
               # Calc Cq by threshold method
               self$cq <- th.cyc(self$adp$fpoints$cyc, self$adp$fpoints$fluor, r = thValue)[1, 1]
               # Write threshold value
               self$quantFluor <- thValue
             },
             overwrite = TRUE)

rdml <- RDML$new(system.file("/extdata/lc96_bACTXY.rdml", package = "RDML"))

# Manual threshold values for different targets
thValues <- c("bACT" = 0.03, "X" = 0.05, "Y" = 0.04, "IPC" = 0.01)

# Preprocess every curve
for (react in rdml$experiment[[1]]$run[[1]]$react) {
  for (fdata in react$data) {
    fdata$Process(thValues[fdata$tar$id])
  }
}

renderAmpCurves("th", "th", 
                rdml$GetFData(
                  # Add threshold values to table
                  rdml$AsTable(quantFluor = data$quantFluor),
                  long.table = TRUE), 
                colorBy = "target",
                thBy = "target") # Add threshold lines (separated by targets)

