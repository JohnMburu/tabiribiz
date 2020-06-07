# Predictive Analytics Framework For Small and Medium Enterprises
# Sales and Purchases Forecasting
# Kenya Methodist University
# Research By John Mburu
# ***********************************************************************************#
# Load Required Libraries
# ***********************************************************************************#

library(shiny)
require(gridExtra)
library(datasets)
library(fpp2)
library(ggplot2)
library(shinythemes)
library(DT)
library(tidyr)
library(dplyr)
library(readxl)

# ***********************************************************************************#
# User Interface 
# ***********************************************************************************#
ui <- fluidPage(
  theme = shinytheme("yeti"),
  
  
  tags$div(class="Header",style="font-family: Montserrat;",
           tags$h2("TABIRI - Sales & Purchase Forecasting")),
  
  sidebarLayout(
    sidebarPanel(
      #Download the Template .csv file
      tags$a(href='./data/template.csv', target='blank', 'Download the Template to use for Prediction', download = 'template.csv'),
      
      #FILE INPUT
      fileInput('file1', 'Choose file to upload',
                accept = c(
                  '.csv',
                  '.xls',
                  '.xlsx'
                )
      ),
      
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
      tags$hr()
      
      
    ),
    mainPanel(
      tabsetPanel(
        #Forecat Tab
        tabPanel("Forecast",plotOutput("forecast_view"),
                 
                 selectInput(
                   "fplot",
                   "Chose a Forecast algorithm:",
                   c("seasonal naive","snaive","Simple Expotential Smoothing")),
                 # SLIDER. NUMBER OF DAYS TO FORECAST
                 sliderInput("forecast_days",
                             "Number of Days to Forecast:",
                             value = 14,
                             min = 2,
                             max = 31)
                 
                 ),
        
        # Graph Tab
        tabPanel("Graphs",plotOutput("graphs_view"),
                 selectInput(
                   "plot",
                   "Chose your desired plot type:",
                   c("Trend Line  (with Smoothened trend line)","Area Plot","Scatter Plot","Regression Line"))
                 
                 ),
        tabPanel("Summary",
                 tags$div(class="header",align="center"),
                 h3(textOutput("caption")),
                 verbatimTextOutput("summary")),
        tabPanel("Dataset",dataTableOutput("data_set"),
                 
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
                 radioButtons("quote", "Quote",
                              choices = c(None = "",
                                          "Double Quote" = '"',
                                          "Single Quote" = "'"),
                              selected = '"') 
                 
                 
                 
                 )
        
        
      )
    )))

# ***********************************************************************************#
# Server Side Logic
# ***********************************************************************************#

server <- function(input, output,session) {
  #Read Uploaded dataset Logic
  
  dataset2 <- reactive({
    if (stringr::str_detect(input$file1, "\\.csv")){
      return(read_csv(input$file1))
    } else if (stringr::str_detect(input$file1, "\\.tsv")){
      return(read_tsv(input$file1))
    } else if (stringr::str_detect(input$file1, "\\.xls")){
      return(read_xls(input$file1))
    } else if (stringr::str_detect(input$file1, "\\.xlsx")){
      return(read_xls(input$file1))
    } else {
      return(NULL)
    }
  })
  
  
  mydata <- reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    
    data <-read.csv(file = inFile$datapath)
    data$Date <- as.Date(paste(data$Date, sep = ""), format = "%d-%b-%y")
    
      return(data)
    data
    
    
    
  })
  
  dataset <- read.csv(file = './data/template.csv')
  dataset$Date <- as.Date(paste(dataset$Date, sep = ""), format = "%d-%b-%y")
  

  # Data Preparation Logic
  # This logic Allows users to drill down using the Custom category, Location and Service offered by SMES 
  data <- reactive({
    if (input$drill == 'yes'){
      itemdata_sr <- subset(dataset,Custom_Category==input$item)
      itemdata_sr_loc <- subset(itemdata_sr,Location==input$location)
      itemdata <- subset(itemdata_sr_loc,Service==input$service)
      return(itemdata) 
      itemdata
    } else {
      itemdata <- subset(dataset)
      return(itemdata)
      itemdata
    }
    
    
  })
  
  # Update Inputs with data from the uploaded file
  observe({
    updateSelectInput(session, "item", choices = unique(dataset$Custom_Category))
    updateSelectInput(session, "location", choices = unique(dataset$Location))
    updateSelectInput(session, "service", choices = unique(dataset$Service))
  })
  
  
  #DECLARE TIME SERIES DATA
  #sALES
  ts_Sales <- reactive({
    itemdata <- ts(data()[,c("Sales")],start=c(2020,4),frequency=365)
    ts
    return(itemdata)
  })
  
  #sALES REFUND
  ts_Sale_Refunds <- reactive({
    itemdata <- ts(data()[,c("Sale_Refunds")],start=c(2020,4),frequency=365)
    ts
    return(itemdata)
  })
  
  #PURCHASES
  ts_Purchases <- reactive({
    itemdata <- ts(data()[,c("Purchases")],start=c(2020,4),frequency=365)
    ts
    return(itemdata)
  })
  
  #PURCHASE CANCELATION
  ts_Purchase_cancelation <- reactive({
    itemdata <- ts(data()[,c("Purchase_cancelation")],start=c(2020,4),frequency=365)
    ts
    return(itemdata)
  })
  
  
  output$caption <- renderText({
    input$item
  })
  
  output$summary <- renderPrint({
    itemdata <- data()
    summary(itemdata)
  })
  
  
  # ***********************************************************************************#
  # Graphs Section 
  # ***********************************************************************************#
  
  output$graphs_view <- renderPlot({
    # Sales Plot
    if (input$forecast_item == 'sales'){
      Sales_Quantities <- ts_Sales()
      print(Sales_Quantities)
      if (input$plot == 'Trend Line  (with Smoothened trend line)') {
        
        p <- ggplot(data(), aes(x = Date, y = Sales)) + 
          geom_line(color = "#00AFBB", size = 1)
        p
        
        p + stat_smooth(
          color = "#FC4E07", fill = "#FC4E07",
          method = "loess"
        )
        
        #autoplot(Sales_Quantities)
      }
      else if(input$plot == 'Area Plot'){
        
        # Area plot
        ggplot(data(), aes(x = Date, y = Sales)) + 
          geom_area(aes(color = Custom_Category, fill = Custom_Category), 
                    alpha = 0.5, position = position_dodge(0.8)) 
      }
      else if(input$plot == 'Scatter Plot'){
        ggplot(data(), aes(x=Date, y=Sales)) + geom_point() + 
          geom_point() +
          geom_smooth(method=lm)
      }
      else if(input$plot == 'Regression Line'){
        ggplot(data(), aes(x = Date, y = Sales)) + 
          geom_smooth(aes(color = "Sales"), size = 1, method = "loess") +
          theme_minimal()
        
      }
      
    }
    # Sales Refund Plot
    else if (input$forecast_item == 'sale_refunds'){
      Refund_Quantities <- ts_Sale_Refunds()
      print(Refund_Quantities)
      if (input$plot == 'Trend Line  (with Smoothened trend line)') {
        
        
        p <- ggplot(data(), aes(x = Date, y = Sale_Refunds)) + 
          geom_line(color = "#00AFBB", size = 1)
        p
        
        p + stat_smooth(
          color = "#FC4E07", fill = "#FC4E07",
          method = "loess")
        
        
      }
      else if(input$plot == 'Area Plot'){
        # Area plot
        ggplot(data(), aes(x = Date, y = Sales_Refunds)) + 
          geom_area(aes(color = Custom_Category, fill = Custom_Category), 
                    alpha = 0.5, position = position_dodge(0.8)) 
      }
      else if(input$plot == 'Scatter Plot'){
        ggplot(data(), aes(x=Date, y=Sale_Refunds)) + geom_point() + 
          geom_point() +
          geom_smooth(method=lm)
      }
      else if(input$plot == 'Regression Line'){
        ggplot(data(), aes(x = Date, y = Sale_Refunds)) + 
          geom_smooth(aes(color = "Sale_Refunds"), size = 1, method = "loess") +
          #scale_color_manual(values = c("#00AFBB", "#E7B800")) +
          theme_minimal()
        
      }
      
    }
    
    # Purchase Plot
    else if (input$forecast_item == 'purchases'){
      Purchase_Quantities <- ts_Purchases()
      print(Purchase_Quantities)
      if (input$plot == 'Trend Line  (with Smoothened trend line)') {
        p <- ggplot(data(), aes(x = Date, y = Purchases)) + 
          geom_line(color = "#00AFBB", size = 1)
        p
        
        p + stat_smooth(
          color = "#FC4E07", fill = "#FC4E07",
          method = "loess")
      }
      else if(input$plot == 'Area Plot'){
        # Area plot
        ggplot(data(), aes(x = Date, y = Purchases)) + 
          geom_area(aes(color = Custom_Category, fill = Custom_Category), 
                    alpha = 0.5, position = position_dodge(0.8)) 
      }
      else if(input$plot == 'Scatter Plot'){
        ggplot(data(), aes(x=Date, y=Purchases)) + geom_point() + 
          geom_point() +
         geom_smooth(method=lm)
    

      }
      else if(input$plot == 'Regression Line'){
        ggplot(data(), aes(x = Date, y = Purchases)) + 
          geom_smooth(aes(color = "Purchases"), size = 1, method = "loess") +
          #scale_color_manual(values = c("#00AFBB", "#E7B800")) +
          theme_minimal()
      }
      
    }
    # Purchase Cancellation Plots
    else if (input$forecast_item == 'purchase_cancelation'){
      Cancelled_POs_Quantities <- ts_Purchase_cancelation()
      print(Cancelled_POs_Quantities)
      if (input$plot == 'Trend Line  (with Smoothened trend line)') {
        p <- ggplot(data(), aes(x = Date, y = Purchase_cancelation)) + 
          geom_line(color = "#00AFBB", size = 1)
        p
        
        p + stat_smooth(
          color = "#FC4E07", fill = "#FC4E07",
          method = "loess")
      }
      else if(input$plot == 'Area Plot'){
        # Area plot
        ggplot(data(), aes(x = Date, y = Purchase_cancelation)) + 
          geom_area(aes(color = Custom_Category, fill = Custom_Category), 
                    alpha = 0.5, position = position_dodge(0.8)) 
      }
      else if(input$plot == 'Scatter Plot'){
        ggplot(data(), aes(x=Date, y=Purchase_cancelation)) + geom_point() + 
          geom_point() +
          geom_smooth(method=lm)
      }
      else if(input$plot == 'Regression Line'){
        ggplot(data(), aes(x = Date, y = Purchase_cancelation)) + 
          geom_smooth(aes(color = "Purchase_cancelation"), size = 1, method = "loess") +
          #scale_color_manual(values = c("#00AFBB", "#E7B800")) +
          theme_minimal()
      }
     
      
    }
    
    
  })
  # ***********************************************************************************#
  # Forecasting
  # ***********************************************************************************#
  
  output$forecast_view <- renderPlot({
    # Sales Forecasting
    if (input$forecast_item == 'sales'){
      SALES_QUANTITIES <- ts_Sales()
      if (input$fplot == 'seasonal naive'){
        fcnv <- naive(SALES_QUANTITIES, h = input$forecast_days, format = "%d-%b-%y")
        
        
        autoplot(fcnv)
      }
      else if(input$fplot=='snaive'){
        fcsnv <- snaive(SALES_QUANTITIES, h = input$forecast_days)
        autoplot(fcsnv)
      }
      else if(input$fplot == "Simple Expotential Smoothing"){
        fcses <- ses(SALES_QUANTITIES, h = input$forecast_days)
        autoplot(fcses)}   
    }
    
    # Sales Refund Forecasting
    else if (input$forecast_item == 'sale_refunds'){
      SLAE_REFUND_QUANTITIES <- ts_Sale_Refunds()
      if (input$fplot == 'seasonal naive'){
        fcnv <- naive(SLAE_REFUND_QUANTITIES, h = input$forecast_days)
        autoplot(fcnv)
      }
      else if(input$fplot=='snaive'){
        fcsnv <- snaive(SLAE_REFUND_QUANTITIES, h = input$forecast_days)
        autoplot(fcsnv)
      }
      else if(input$fplot == "Simple Expotential Smoothing"){
        fcses <- ses(SLAE_REFUND_QUANTITIES, h = input$forecast_days)
        autoplot(fcses)} 
    }
    
    # Purchase Forecasting
    else if (input$forecast_item == 'purchases'){
      PURCHASE_QUANTITIES <- ts_Purchases()
      if (input$fplot == 'seasonal naive'){
        fcnv <- naive(PURCHASE_QUANTITIES, h = input$forecast_days)
        autoplot(fcnv)
      }
      else if(input$fplot=='snaive'){
        fcsnv <- snaive(PURCHASE_QUANTITIES, h = input$forecast_days)
        autoplot(fcsnv)
      }
      else if(input$fplot == "Simple Expotential Smoothing"){
        fcses <- ses(PURCHASE_QUANTITIES, h = input$forecast_days)
        autoplot(fcses)} 
      
    }
    # Purchase Forecasting
    else if (input$forecast_item == 'purchase_cancelation'){
      PURCHASE_ORDER_CANCELATION_QUANTITIES <- ts_Purchase_cancelation()
      if (input$fplot == 'seasonal naive'){
        fcnv <- naive(PURCHASE_ORDER_CANCELATION_QUANTITIES, h = input$forecast_days)
        autoplot(fcnv)
      }
      else if(input$fplot=='snaive'){
        fcsnv <- snaive(PURCHASE_ORDER_CANCELATION_QUANTITIES, h = input$forecast_days)
        autoplot(fcsnv)
      }
      else if(input$fplot == "Simple Expotential Smoothing"){
        fcses <- ses(PURCHASE_ORDER_CANCELATION_QUANTITIES, h = input$forecast_days)
        autoplot(fcses)} 
      
    }
    
    
  }
  
  )
  # Uploaded data set output
  output$data_set <- DT::renderDataTable({
    
    DT::datatable(data())   
    
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)