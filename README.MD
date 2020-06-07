## Tabiri Biashara
## https://tabiri.shinyapps.io/tabiriApp/

#Pre requisities
- R
- Rstudio

# install Required Packages
library(shiny)
require(gridExtra)
library(datasets)
library(fpp2)
library(ggplot2)
library(shinythemes)

library(tidyr)
library(dplyr)


File Structure

TabiriApp
    App.R
    Data
        - Template
    README.MD
    www
        - tabiri.png
    .RData


##Architecure

app.R
    ui
        - Download Template .xlx/csv
        - Upload File
        - Select What to see [All, Sales, Sales Refunds, Purchases, Canceled Purchase orders]
        - Select Graph type []
        - Select Forecast Type []
        - Select Number of Days to forecast
        - Drill Down [by Location, by Service, By Custom Category]
    server
        - Forecast
        - Graph Summary
        - summary
        - Dataset
    shinyapp
    

