# Predictive Analytics Framework For Small and Medium Enterprises
# Sales and Purchases Forecasting
# Kenya Methodist University
# Research By John Mburu
# ***********************************************************************************#
# Load Required Libraries
# ***********************************************************************************#
library(shiny)
library(shinythemes)

# ***********************************************************************************#
# Tabiri App Presentation Layer
# ***********************************************************************************#
shinyUI(fluidPage(
    theme = shinytheme("yeti"),
    tags$div(class="Header",style="font-family: Montserrat;",
             tags$h2("TABIRI - Sales & Purchase Forecasting")),
    sidebarLayout(
        sidebarPanel(
            #tags$a (href="/tabiriApp/data/template.csv", "Download Template here!"),
            downloadButton("downloadData", "Download Dataset Template"),
                        fileInput('file1', 'Choose file to upload',
                      accept = c(
                          '.csv',
                          '.xls',
                          '.xlsx'
                      )
            ),
            helpText("Default max. file size is 10MB"),
            h5(helpText("Data Preparations")),
                        radioButtons("disp", "Display",
                         choices = c(All = "all",
                                     Head = "head"),
                         selected = "all"),
            
            tags$hr(),
            br(),
            radioButtons(inputId = 'sep', label = 'Separator', choices = c(Comma=',',Semicolon=';',Tab='\t', Space=''), selected = ','),
            
            tags$hr(),
            # SELECT FORECAST OPTIONS
            selectInput("forecast_item", "Select what to forecast:",
                        choices =  c(
                            "Sales" = "sales",
                            "Sales Refund" = "sales_refund",
                            "Purchases" = "purchases",
                            "Cancelled POs" = "purchase_cancelation", selected = NULL)),
            
            #Allow users to Drill down using custom categorization column [this column can be used to identify products or outlets]
            radioButtons("drill", "Do You Want to Enable use of Custom Categorization?",
                         choices = c(
                             "No" = "no",
                             "Yes" = "yes",
                             selected = NULL)),
            
            selectInput(
                "item",
                "By Custom Categorization:",
                "choices"),
            selectInput(
                "location",
                "By Location",
                "choices"),
            
            selectInput(
                "service",
                "By Service Type",
                "choices"),
            
            
            tags$hr()   
            
        ),
        mainPanel(
            uiOutput("tb")
        )
        
    )
))