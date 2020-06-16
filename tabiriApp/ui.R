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
            #Download the Template .csv file

            
            #FILE INPUT
            fileInput('file1', 'Choose CSV File',
                      accept=c('text/csv', 
                               'text/comma-separated-values,text/plain', 
                               '.csv')),
            helpText("Max File size is 10MB"),
            #Uploaded data setup
            #Input: Select number of rows to display ----
            radioButtons("disp", "Display",
                         choices = c(Head = "head",
                                     All = "all"),
                         selected = "head"),
            # Input: Select separator ----
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ","),
            
            # Input: Select quotes ----
            #radioButtons("quote", "Quote",
            #            choices = c(None = "",
            #                       "Double Quote" = '"',
            #                      "Single Quote" = "'"),
            #         selected = '"'), 
            tags$hr(),
            
            # SELECT FORECAST OPTIONS
            selectInput("forecast_item", "Select what to forecast:",
                        choices =  c(
                            "Sales" = "sales",
                            "Sale Refunds" = "sale_refunds",
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
            
            
            
            tags$hr(),
            
            
            radioButtons("weekends", "Include Weekends?",
                         choices = c(
                             "No" = "no",
                             "Yes" = "yes",
                             selected = NULL)),
            
        ),
        mainPanel(
            uiOutput("tb") 
        )
    )))