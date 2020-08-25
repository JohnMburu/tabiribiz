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
    tags$head(HTML(
        "<script async src='https://www.googletagmanager.com/gtag/js?id=UA-175900486-1'></script>
            <script>
            window.dataLayer = window.dataLayer || [];
        function gtag(){dataLayer.push(arguments);}
        gtag('js', new Date());
        
        gtag('config', 'UA-175900486-1');
        </script>"
    )),
    

    
    tags$div(class="Header",style="font-family: Montserrat;",
             tags$h2("TABIRI - Demand and Supply Forecasting")),
    sidebarLayout(
        sidebarPanel(
            downloadButton("downloadData", "Download Dataset Template"),
                        fileInput('file1', 'Choose file to upload',
                      accept = c(
                          '.csv'
                      )
            ),
            helpText("Default max. file size is 10MB"),
            h5(helpText("Data Preparations")),
                        radioButtons("disp", "Display",
                         choices = c(All = "all",
                                     Head = "head"),
                         selected = "all"),
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
            selectInput(
                "sector",
                "By Sector",
                "choices"),
            tags$hr()   
            
        ),
        mainPanel(
            uiOutput("tb")
        )
        
    )
))