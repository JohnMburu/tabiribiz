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
library(DT)
library(tidyr)
library(dplyr)
library(readxl)
library(zoo)
library(xts)

#arima
library(quantmod)
library(tseries)
library(timeSeries)
library(forecast)
# ***********************************************************************************#
# Server Side Logic
# ***********************************************************************************#
#File Size Limitation
options(shiny.maxRequestSize = 9*1024^2)
shinyServer(function(input, output,session) {

  #read in reference CSV for public holidays
  holidays <- reactive({
    myholidays <- read.csv(file = './data/public_holidays.csv')
    return(myholidays)
  })
  
  #read template
  template <- reactive({
    mytemplate <- read.csv(file = './data/template.csv')
    return(mytemplate)
  })
  
 # Read uploaded file 
  mydata <- reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    data <-read.csv(inFile$datapath, sep=input$sep)
    data
    
    #outputing the head or all data only
    if (input$disp == "head") {
      return(head(data))
    }
    else {
      return(data)
    }
    
    
  })

  #Apply Filters
     data <- reactive({
       if (input$drill == 'yes'){
           itemdata_sr <- subset(mydata(),Custom_Category==input$item)
           itemdata_sr_loc <- subset(itemdata_sr,Location==input$location)
           itemdata <- subset(itemdata_sr_loc,Service==input$service)
           itemdata$Date <- as.Date(paste(itemdata$Date, sep = ""), format = "%d-%b-%y")
           return(itemdata) 
           itemdata
       } else {
           itemdata <- subset(mydata())
           itemdata$Date <- as.Date(paste(itemdata$Date, sep = ""), format = "%d-%b-%y")
           return(itemdata)
           itemdata
       }
   })

# Update Inputs with data from the uploaded file
 observe({
   updateSelectInput(session, "item", choices = unique(mydata()[,c("Custom_Category")] ))
   updateSelectInput(session, "location", choices = unique(mydata()[,c("Location")]))
   updateSelectInput(session, "service", choices = unique(mydata()[,c("Service")]))
 })
  
  
  #DECLARE TIME SERIES DATA
  #Sales
  ts_Sales <- reactive({
    itemdata <- ts(data()[,c("Sales")],start=c(2020,4),frequency=365)
    return(itemdata)
  })
  #Sales_Refunds
    ts_Sales_Refund <- reactive({
    itemdata <- ts(data()[,c("Sales_Refund")],start=c(2020,4),frequency=365)
    return(itemdata)
  })
  #Purchases
    ts_Purchases <- reactive({
    itemdata <- ts(data()[,c("Purchases")],start=c(2020,4),frequency=365)
    return(itemdata)
  })
  #Purchase_cancelation
    ts_Purchase_Cancelation <- reactive({
    itemdata <- ts(data()[,c("Purchase_Cancelation")],start=c(2020,4),frequency=365)
    return(itemdata)
  })
  
    # ***********************************************************************************#
    # Download Dataset Template
    # ***********************************************************************************#
    
    # Download CSV template
    # Downloadable csv of selected dataset ----
    output$downloadData <- downloadHandler(
      filename = function() {
        paste(input$template, "template.csv", sep = "")
      },
      content = function(file) {
        write.csv(template(), file, row.names = FALSE)
      }
    )
    
  
  # ***********************************************************************************#
  # Text Section Section 
  # ***********************************************************************************#
  
  output$caption <- renderText({
    
  })
  


  
  # ***********************************************************************************#
  # Summary Section 
  # ***********************************************************************************#
  
  output$summary <- renderPrint({
      summary(data())
  })
  
  
  # ***********************************************************************************#
  # Graphs Section 
  # ***********************************************************************************#
  output$graphs_view <- renderPlot({
    # Sales Plot
    if (input$forecast_item == 'sales'){
      
      if (input$plot == 'Trend Line') {
        p <- ggplot(data(), aes(x = Date, y = Sales)) + 
          geom_line(color = "#00AFBB", size = 1)
        p
        
        p + stat_smooth(
          color = "#FC4E07", fill = "#FC4E07",
          method = "loess"
        )
        
      }
      else if(input$plot == 'Area Plot'){
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
    else if (input$forecast_item == 'sales_refund'){
      if (input$plot == 'Trend Line') {
        p <- ggplot(data(), aes(x = Date, y = Sales_Refund)) + 
          geom_line(color = "#00AFBB", size = 1)
        p
        
        p + stat_smooth(
          color = "#FC4E07", fill = "#FC4E07",
          method = "loess"
        )
        
      }
      else if(input$plot == 'Area Plot'){
          ggplot(data(), aes(x = Date, y = Sales_Refund)) + 
          geom_area(aes(color = Custom_Category, fill = Custom_Category), 
          alpha = 0.5, position = position_dodge(0.8))

      }
      else if(input$plot == 'Scatter Plot'){
          ggplot(data(), aes(x=Date, y=Sales_Refund)) + geom_point() + 
          geom_point() +
          geom_smooth(method=lm)

      }
      else if(input$plot == 'Regression Line'){
          ggplot(data(), aes(x = Date, y = Sales_Refund)) + 
          geom_smooth(aes(color = "Sales"), size = 1, method = "loess") +
          theme_minimal()
      }
      
    }
    
    # Purchase Plot
    else if (input$forecast_item == 'purchases'){

      if (input$plot == 'Trend Line') {
        p <- ggplot(data(), aes(x = Date, y = Purchases)) + 
          geom_line(color = "#00AFBB", size = 1)
        p
        
        p + stat_smooth(
          color = "#FC4E07", fill = "#FC4E07",
          method = "loess"
        )
        
      }
      else if(input$plot == 'Area Plot'){
          ggplot(data(), aes(x = Date, y = Purchases)) + 
          geom_area(aes(color = Custom_Category, fill = Custom_Category), 
          alpha = 0.5, position = position_dodge(0.8))

      }
      else if(input$plot == 'Scatter Plot'){
          ggplot(data(), aes(x=Date, y=Sales)) + geom_point() + 
          geom_point() +
          geom_smooth(method=lm)

      }
      else if(input$plot == 'Regression Line'){
          ggplot(data(), aes(x = Date, y = Purchases)) + 
          geom_smooth(aes(color = "Sales"), size = 1, method = "loess") +
          theme_minimal()
      }
      
    }
    
    # Purchase Cancellation Plots
    else if (input$forecast_item == 'purchase_cancelation'){
      if (input$plot == 'Trend Line') {
        p <- ggplot(data(), aes(x = Date, y = Purchase_Cancelation)) + 
          geom_line(color = "#00AFBB", size = 1)
        p
        
        p + stat_smooth(
          color = "#FC4E07", fill = "#FC4E07",
          method = "loess"
        )
        
      }
      else if(input$plot == 'Area Plot'){
          ggplot(data(), aes(x = Date, y = Purchase_Cancelation)) + 
          geom_area(aes(color = Custom_Category, fill = Custom_Category), 
          alpha = 0.5, position = position_dodge(0.8))

      }
      else if(input$plot == 'Scatter Plot'){
          ggplot(data(), aes(x=Date, y=Purchase_Cancelation)) + geom_point() + 
          geom_point() +
          geom_smooth(method=lm)

      }
      else if(input$plot == 'Regression Line'){
          ggplot(data(), aes(x = Date, y = Purchase_Cancelation)) + 
          geom_smooth(aes(color = "Sales"), size = 1, method = "loess") +
          theme_minimal()
      }
    }
    
  })
  # ***********************************************************************************#
  # Forecasting
  # Logic for Sales, Sales Refund, Purchases and Purchase Refunds
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
        autoplot(fcses)
      }   
      else if(input$fplot == "Mean Average Forecast"){
        fcmean <- meanf(SALES_QUANTITIES, h = input$forecast_days)
        autoplot(fcmean)
      } 
      else if(input$fplot == "Drift Forecast"){
        fcdrf <- rwf(SALES_QUANTITIES, h = input$forecast_days)
        autoplot(fcdrf)
      } 
 
    }
    
    # Sales Refund Forecasting
    else if (input$forecast_item == 'sales_refund'){
      SALES_REFUND_QUANTITIES <- ts_Sales_Refund()
      if (input$fplot == 'seasonal naive'){
        fcnv <- naive(SALES_REFUND_QUANTITIES, h = input$forecast_days)
        autoplot(fcnv)
      }
      else if(input$fplot=='snaive'){
        fcsnv <- snaive(SALES_REFUND_QUANTITIES, h = input$forecast_days)
        autoplot(fcsnv)
      }
      else if(input$fplot == "Simple Expotential Smoothing"){
        fcses <- ses(SALES_REFUND_QUANTITIES, h = input$forecast_days)
        autoplot(fcses)
      }
      else if(input$fplot == "Mean Average Forecast"){
        fcmean <- meanf(SALES_REFUND_QUANTITIES, h = input$forecast_days)
        autoplot(fcmean)
      } 
      else if(input$fplot == "Drift Forecast"){
        fcdrf <- rwf(SALES_REFUND_QUANTITIES, h = input$forecast_days)
        autoplot(fcdrf)
      }  
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
        autoplot(fcses)
      } 
      else if(input$fplot == "Mean Average Forecast"){
        fcmean <- meanf(PURCHASE_QUANTITIES, h = input$forecast_days)
        autoplot(fcmean)
      } 
      else if(input$fplot == "Drift Forecast"){
        fcdrf <- rwf(PURCHASE_QUANTITIES, h = input$forecast_days)
        autoplot(fcdrf)
      } 
      
    }
    # Purchase Forecasting
    else if (input$forecast_item == 'purchase_cancelation'){
      PURCHASE_ORDER_CANCELATION_QUANTITIES <- ts_Purchase_Cancelation()
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
        autoplot(fcses)
      }
      else if(input$fplot == "Mean Average Forecast"){
        fcmean <- meanf(PURCHASE_ORDER_CANCELATION_QUANTITIES, h = input$forecast_days)
        autoplot(fcmean)
      } 
      else if(input$fplot == "Drift Forecast"){
        fcdrf <- rwf(PURCHASE_ORDER_CANCELATION_QUANTITIES, h = input$forecast_days)
        autoplot(fcdrf)
      }  
      
    }
  })
  
  # ***********************************************************************************#
  # Dataset view
  # View Uploaded Dataset
  # ***********************************************************************************#
  output$data_set <- DT::renderDataTable({
   DT::datatable(mydata(),editable = TRUE) 
  })
  
  # ***********************************************************************************#
  # Holidays Plot
  # View Holidays
  # ***********************************************************************************# 
  output$holidays <- DT::renderDataTable({
    DT::datatable(holidays(),editable = TRUE)  
  })
  

  # ***********************************************************************************#
  # Comparison
  # ***********************************************************************************#
    
  output$compare <- renderPlot({
    if (input$forecast_item == 'sales'){
      SALES_QUANTITIES <- ts_Sales()
      fcnv <- naive(SALES_QUANTITIES, h = input$forecast_days)
      fcsnv <- snaive(SALES_QUANTITIES, h = input$forecast_days)
      fcses <- ses(SALES_QUANTITIES, h = input$forecast_days)
      par(mfrow=c(3,1))
      autoplot(fcsnv)
      autoplot(fcses)
      autoplot(fcnv)
      
      

    } else if (input$forecast_item == 'sales_refund') {
      SALES_REFUND_QUANTITIES <- ts_Sales_Refund()

    }else if (input$forecast_item == 'purchases'){
      PURCHASE_QUANTITIES <- ts_Purchases()

    }else if (input$forecast_item == 'purchase_cancelation'){
      PURCHASE_ORDER_CANCELATION_QUANTITIES <- ts_Purchase_Cancelation()

    }

  })
  
  
  # ***********************************************************************************#
  # Dynamically Render the Presentation Layer Tabs After Data Upload
  # ***********************************************************************************#
  output$tb <- renderUI({
    if(is.null(mydata()))
      h5(tags$img(src='tabiri.png', heigth=900, width=900))
    else
      tabsetPanel(
        
        #Forecat Tab
        tabPanel("Forecast",plotOutput("forecast_view"),
                 
                 selectInput(
                   "fplot",
                   "Chose a Forecast algorithm:",
                   c("seasonal naive","snaive","Simple Expotential Smoothing","Mean Average Forecast","Drift Forecast")),
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
                   c("Trend Line","Area Plot","Scatter Plot","Regression Line"))      
        ),
        tabPanel("Summary",
                 tags$div(class="header",align="center"),
                 h3(textOutput("caption")),
                 verbatimTextOutput("summary")),
        tabPanel("Dataset",dataTableOutput("data_set")
        ),
        tabPanel("Holidays", dataTableOutput("holidays") ) ,
        tabPanel("Model Comparison", plotOutput("compare") )      
                 
      )
  }) 
})