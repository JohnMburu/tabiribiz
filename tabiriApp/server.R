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
library(readr)
library(scales)


#arima
library(quantmod)
library(tseries)
library(timeSeries)
library(forecast)
library(plotly)
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
           itemdata_sr_loc_ser <- subset(itemdata_sr_loc,Service==input$service)
           itemdata <- subset(itemdata_sr_loc_ser,Sector==input$sector)
           itemdata$Date <- as.Date(paste(itemdata$Date, sep = ""), format = "%d-%b-%y")
           
           #Remove N/A Dates
           itemdata <- itemdata %>% filter(!is.na(Date))
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
   updateSelectInput(session, "item", choices = unique(mydata()[,c(1)])) # first column
   updateSelectInput(session, "location", choices = unique(mydata()[,c(7)])) # 7th column
   updateSelectInput(session, "service", choices = unique(mydata()[,c(8)])) # 8th Column
   updateSelectInput(session, "sector", choices = unique(mydata()[,c(10)])) # 8th Column
 })
  
  
  #DECLARE TIME SERIES DATA
  #Sales
  ts_Sales <- reactive({
    itemdata <- ts(data()[,c(3)],start=c(2020,4),frequency=365)
    return(itemdata)
  })
  #Sales_Refunds
    ts_Sales_Refund <- reactive({
    itemdata <- ts(data()[,c(4)],start=c(2020,4),frequency=365)
    return(itemdata)
  })
  #Purchases
    ts_Purchases <- reactive({
    itemdata <- ts(data()[,c(5)],start=c(2020,4),frequency=365)
    return(itemdata)
  })
  #Purchase_cancelation
    ts_Purchase_Cancelation <- reactive({
    itemdata <- ts(data()[,c(6)],start=c(2020,4),frequency=365)
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
  # Display Forecasting accuracy
  # ***********************************************************************************#
  #"seasonal naive","Naive","Simple Exponential Smoothing","Mean Average Forecast","Drift Forecast"
  output$summary <- renderPrint({
    #data Prep
        f_ts_Sales <- ts_Sales()
        f_ts_Sales_Refund <- ts_Sales_Refund()
        f_ts_Purchases<- ts_Purchases()
        f_ts_Purchase_Cancelation <- ts_Purchase_Cancelation()
        
        
        
        
        #Seasonal Naive Accuracy
        if (input$f_accuracy=='Seasonal Naive'){
          if (input$accuracy_item=='sales'){
            fcsnv <- snaive(f_ts_Sales, h = input$forecast_days_acc)
            accuracy(fcsnv)
            
          }
          else if (input$accuracy_item=='sales_refund'){
            fcsnv <- snaive(f_ts_Sales_Refund, h = input$forecast_days_acc)
            accuracy(fcsnv)
          }
          
          else if (input$accuracy_item=='purchases'){
            fcsnv <- snaive(f_ts_Purchases, h = input$forecast_days_acc)
            accuracy(fcsnv)
          }
          else if (input$accuracy_item=='purchase_cancelation'){
            fcsnv <- snaive(f_ts_Purchase_Cancelation, h = input$forecast_days_acc)
            accuracy(fcsnv)
          }

          
        }
        #Naive Accuracy
        else if (input$f_accuracy=='Naive'){
            if (input$accuracy_item=='sales'){
              fcnv <- naive(f_ts_Sales, h = input$forecast_days_acc)
              accuracy(fcnv)
              
            }
            else if (input$accuracy_item=='sales_refund'){
              fcnv <- naive(f_ts_Sales_Refund, h = input$forecast_days_acc)
              accuracy(fcnv)
            }
            
            else if (input$accuracy_item=='purchases'){
              fcnv <- naive(f_ts_Purchases, h = input$forecast_days_acc)
              accuracy(fcnv)
            }
            else if (input$accuracy_item=='purchase_cancelation'){
              fcnv <- naive(f_ts_Purchase_Cancelation, h = input$forecast_days_acc)
              accuracy(fcnv)
            }
          
          
        }
        
        #Exponential smoothing accuracy
        else if (input$f_accuracy=='Simple Exponential Smoothing'){
          
          if (input$accuracy_item=='sales'){
            fcses <- ses(f_ts_Sales, h = input$forecast_days_acc)
            accuracy(fcses)
            
          }
          else if (input$accuracy_item=='sales_refund'){
            fcses <- ses(f_ts_Sales_Refund, h = input$forecast_days_acc)
            accuracy(fcses)
          }
          
          else if (input$accuracy_item=='purchases'){
            fcses <- ses(f_ts_Purchases, h = input$forecast_days_acc)
            accuracy(fcses)
          }
          else if (input$accuracy_item=='purchase_cancelation'){
            fcses <- ses(f_ts_Purchase_Cancelation, h = input$forecast_days_acc)
            accuracy(fcses)
          }
          
          
          
          
        }
        #Mean Average Accuracy
        else if (input$f_accuracy=='Mean Average Forecast'){
          if (input$accuracy_item=='sales'){
            fcmean <- meanf(f_ts_Sales, h = input$forecast_days_acc)
            accuracy(fcmean)
            
          }
          else if (input$accuracy_item=='sales_refund'){
            fcmean <- meanf(f_ts_Sales_Refund, h = input$forecast_days_acc)
            accuracy(fcmean)
          }
          
          else if (input$accuracy_item=='purchases'){
            fcmean <- meanf(f_ts_Purchases, h = input$forecast_days_acc)
            accuracy(fcmean)
          }
          else if (input$accuracy_item=='purchase_cancelation'){
            fcmean <- meanf(f_ts_Purchase_Cancelation, h = input$forecast_days_acc)
            accuracy(fcmean)
          }
          
          
        }
        
        #Drift Accuracy
        else if (input$f_accuracy=='Drift Forecast'){
          
          if (input$accuracy_item=='sales'){
            fcdrf <- rwf(f_ts_Sales, h = input$forecast_days_acc)
            accuracy(fcdrf)
            
          }
          else if (input$accuracy_item=='sales_refund'){
            fcdrf <- rwf(f_ts_Sales_Refund, h = input$forecast_days_acc)
            accuracy(fcdrf)
          }
          
          else if (input$accuracy_item=='purchases'){
            fcdrf <- rwf(f_ts_Purchases, h = input$forecast_days_acc)
            accuracy(fcdrf)
          }
          else if (input$accuracy_item=='purchase_cancelation'){
            fcdrf <- rwf(f_ts_Purchase_Cancelation, h = input$forecast_days_acc)
            accuracy(fcdrf)
          }
          
          
        }
        #Autoregressive integrated moving average (ARIMA) Accuracy
        
        else if (input$f_accuracy=='Autoregressive integrated moving average (ARIMA)'){
          autoarimaSales <- auto.arima(ts_Sales())
          autoarimaRefunds <- auto.arima(ts_Sales_Refund())
          autoarimaPurchases <- auto.arima(ts_Purchases())
          autoarimaPurchaseCancelation <- auto.arima(ts_Purchase_Cancelation())
          
          if (input$accuracy_item=='sales'){
            fcarima <- forecast(autoarimaSales, h = input$forecast_days)
            autoplot(fcarima)
            
          }
          else if (input$accuracy_item=='sales_refund'){
            fcarima <- forecast(autoarimaRefunds, h = input$forecast_days_acc)
            accuracy(fcarima)
          }
          
          else if (input$accuracy_item=='purchases'){
            fcarima <- forecast(autoarimaPurchases, h = input$forecast_days_acc)
            accuracy(fcarima)
          }
          else if (input$accuracy_item=='purchase_cancelation'){
            fcarima <- forecast(autoarimaPurchaseCancelation, h = input$forecast_days_acc)
            accuracy(fcarima)
          }
          
          
        }        
        
        
        
        
        
        #Summary
        else if (input$f_accuracy=='Summary'){
          
          
          
      if (input$accuracy_item=='sales'){
            summary(f_ts_Sales)
            
          }
          else if (input$accuracy_item=='sales_refund'){
            summary(f_ts_Sales_Refund)

          }
          
          else if (input$accuracy_item=='purchases'){
            summary(f_ts_Purchases)

          }
          else if (input$accuracy_item=='purchase_cancelation'){
            summary(f_ts_Purchase_Cancelation)

          }
          
        }
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
          geom_smooth(aes(color = "Sales Refund"), size = 1, method = "loess") +
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
          geom_smooth(aes(color = "Purchases"), size = 1, method = "loess") +
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
          geom_smooth(aes(color = "Purchase Cancelation"), size = 1, method = "loess") +
          theme_minimal()
      }
    }
    
  })
  # ***********************************************************************************#
  # Forecasting
  # Logic for Sales, Sales Refund, Purchases and Purchase Refunds
  # Plot Logic
  # ***********************************************************************************#
  
  output$forecast_view <- renderPlot({
    # Sales Forecasting
    if (input$forecast_item == 'sales'){
      SALES_QUANTITIES <- ts_Sales()
      autoarima1 <- auto.arima(ts_Sales())
      if (input$fplot == 'Seasonal Naive'){
        fcsnv <- snaive(SALES_QUANTITIES, h = input$forecast_days, format = "%d-%b-%y")
        autoplot(fcsnv)
      }
      else if(input$fplot=='Naive'){
        fcnv <- naive(SALES_QUANTITIES, h = input$forecast_days)
        autoplot(fcnv)
      }
      else if(input$fplot == "Simple Exponential Smoothing"){
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
      else if(input$fplot == "Autoregressive integrated moving average (ARIMA)"){
        fcarima <- forecast(autoarima1, h = input$forecast_days)
        autoplot(fcarima)
      } 
 
    }
    
    # Sales Refund Forecasting
    else if (input$forecast_item == 'sales_refund'){
      SALES_REFUND_QUANTITIES <- ts_Sales_Refund()
      autoarima1 <- auto.arima(ts_Sales_Refund())
      if (input$fplot == 'Seasonal Naive'){
        fcsnv <- snaive(SALES_REFUND_QUANTITIES, h = input$forecast_days)
        autoplot(fcsnv)
      }
      else if(input$fplot=='Naive'){
        fcnv <- naive(SALES_REFUND_QUANTITIES, h = input$forecast_days)
        autoplot(fcnv)
      }
      else if(input$fplot == "Simple Exponential Smoothing"){
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
      else if(input$fplot == "Autoregressive integrated moving average (ARIMA)"){
        fcarima <- forecast(autoarima1, h = input$forecast_days)
        autoplot(fcarima)
      } 
    }
    
    # Purchase Forecasting
    else if (input$forecast_item == 'purchases'){
      PURCHASE_QUANTITIES <- ts_Purchases()
      autoarima1 <- auto.arima(ts_Purchases())
      if (input$fplot == 'Seasonal Naive'){
        fcsnv <- snaive(PURCHASE_QUANTITIES, h = input$forecast_days)
        autoplot(fcsnv)
      }
      else if(input$fplot=='Naive'){
        fcnv <- naive(PURCHASE_QUANTITIES, h = input$forecast_days)
        autoplot(fcnv)
      }
      else if(input$fplot == "Simple Exponential Smoothing"){
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
      else if(input$fplot == "Autoregressive integrated moving average (ARIMA)"){
        fcarima <- forecast(autoarima1, h = input$forecast_days)
        autoplot(fcarima)
      } 
      
    }
    # Purchase Forecasting
    else if (input$forecast_item == 'purchase_cancelation'){
      PURCHASE_ORDER_CANCELATION_QUANTITIES <- ts_Purchase_Cancelation()
      autoarima1 <- auto.arima(ts_Purchase_Cancelation())
      if (input$fplot == 'Seasonal Naive'){
        fcsnv <- snaive(PURCHASE_ORDER_CANCELATION_QUANTITIES, h = input$forecast_days)
        autoplot(fcsnv)
      }
      else if(input$fplot=='Naive'){
        fcnv <- naive(PURCHASE_ORDER_CANCELATION_QUANTITIES, h = input$forecast_days)
        autoplot(fcnv)
      }
      else if(input$fplot == "Simple Exponential Smoothing"){
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
      else if(input$fplot == "Autoregressive integrated moving average (ARIMA)"){
        fcarima <- forecast(autoarima1, h = input$forecast_days)
        autoplot(fcarima)
      } 
      
    }
  })
  # ***********************************************************************************#
  # Forecasting
  # Logic for Sales, Sales Refund, Purchases and Purchase Refunds
  # Plot forecasting Data
  # ***********************************************************************************#
  output$forecast_accuracy <- renderTable({
    # Sales Forecasting
    if (input$forecast_item == 'sales'){
      SALES_QUANTITIES <- ts_Sales()
      autoarima1 <- auto.arima(ts_Sales())
      if (input$fplot == 'Seasonal Naive'){
        fcsnv <- snaive(SALES_QUANTITIES, h = input$forecast_days, format = "%d-%b-%y")
        return(fcsnv)
      }
      else if(input$fplot=='Naive'){
        fcnv <- naive(SALES_QUANTITIES, h = input$forecast_days)
        return(fcnv)
      }
      else if(input$fplot == "Simple Exponential Smoothing"){
        fcses <- ses(SALES_QUANTITIES, h = input$forecast_days)
        return(fcses)
      }   
      else if(input$fplot == "Mean Average Forecast"){
        fcmean <- meanf(SALES_QUANTITIES, h = input$forecast_days)
        return(fcmean)
      } 
      else if(input$fplot == "Drift Forecast"){
        fcdrf <- rwf(SALES_QUANTITIES, h = input$forecast_days)
        return(fcdrf)
      } 
      else if(input$fplot == "Autoregressive integrated moving average (ARIMA)"){
        fcarima <- forecast(autoarima1, h = input$forecast_days)
        return(fcarima)
      } 
      
    }
    
    # Sales Refund Forecasting
    else if (input$forecast_item == 'sales_refund'){
      SALES_REFUND_QUANTITIES <- ts_Sales_Refund()
      autoarima1 <- auto.arima(ts_Sales_Refund())
      if (input$fplot == 'Seasonal Naive'){
        fcsnv <- snaive(SALES_REFUND_QUANTITIES, h = input$forecast_days)
        return(fcsnv)
      }
      else if(input$fplot=='Naive'){
        fcnv <- naive(SALES_REFUND_QUANTITIES, h = input$forecast_days)
        return(fcnv)
      }
      else if(input$fplot == "Simple Exponential Smoothing"){
        fcses <- ses(SALES_REFUND_QUANTITIES, h = input$forecast_days)
        return(fcses)
      }
      else if(input$fplot == "Mean Average Forecast"){
        fcmean <- meanf(SALES_REFUND_QUANTITIES, h = input$forecast_days)
        return(fcmean)
      } 
      else if(input$fplot == "Drift Forecast"){
        fcdrf <- rwf(SALES_REFUND_QUANTITIES, h = input$forecast_days)
        return(fcdrf)
      }
      else if(input$fplot == "Autoregressive integrated moving average (ARIMA)"){
        fcarima <- forecast(autoarima1, h = input$forecast_days)
        return(fcarima)
      } 
    }
    
    # Purchase Forecasting
    else if (input$forecast_item == 'purchases'){
      PURCHASE_QUANTITIES <- ts_Purchases()
      autoarima1 <- auto.arima(ts_Purchases())
      if (input$fplot == 'Seasonal Naive'){
        fcsnv <- snaive(PURCHASE_QUANTITIES, h = input$forecast_days)
        return(fcsnv)
      }
      else if(input$fplot=='Naive'){
        fcnv <- naive(PURCHASE_QUANTITIES, h = input$forecast_days)
        return(fcnv)
      }
      else if(input$fplot == "Simple Exponential Smoothing"){
        fcses <- ses(PURCHASE_QUANTITIES, h = input$forecast_days)
        return(fcses)
      } 
      else if(input$fplot == "Mean Average Forecast"){
        fcmean <- meanf(PURCHASE_QUANTITIES, h = input$forecast_days)
        return(fcmean)
      } 
      else if(input$fplot == "Drift Forecast"){
        fcdrf <- rwf(PURCHASE_QUANTITIES, h = input$forecast_days)
        return(fcdrf)
      } 
      else if(input$fplot == "Autoregressive integrated moving average (ARIMA)"){
        fcarima <- forecast(autoarima1, h = input$forecast_days)
        return(fcarima)
      } 
      
    }
    # Purchase Forecasting
    else if (input$forecast_item == 'purchase_cancelation'){
      PURCHASE_ORDER_CANCELATION_QUANTITIES <- ts_Purchase_Cancelation()
      autoarima1 <- auto.arima(ts_Purchase_Cancelation())
      if (input$fplot == 'Seasonal Naive'){
        fcsnv <- snaive(PURCHASE_ORDER_CANCELATION_QUANTITIES, h = input$forecast_days)
        return(fcsnv)
      }
      else if(input$fplot=='Naive'){
        fcnv <- naive(PURCHASE_ORDER_CANCELATION_QUANTITIES, h = input$forecast_days)
        return(fcnv)
      }
      else if(input$fplot == "Simple Exponential Smoothing"){
        fcses <- ses(PURCHASE_ORDER_CANCELATION_QUANTITIES, h = input$forecast_days)
        return(fcses)
      }
      else if(input$fplot == "Mean Average Forecast"){
        fcmean <- meanf(PURCHASE_ORDER_CANCELATION_QUANTITIES, h = input$forecast_days)
        return(fcmean)
      } 
      else if(input$fplot == "Drift Forecast"){
        fcdrf <- rwf(PURCHASE_ORDER_CANCELATION_QUANTITIES, h = input$forecast_days)
        return(fcdrf)
      } 
      else if(input$fplot == "Autoregressive integrated moving average (ARIMA)"){
        fcarima <- forecast(autoarima1, h = input$forecast_days)
        return(fcarima)
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
  # Dynamically Render the Presentation Layer Tabs After Data Upload
  # ***********************************************************************************#
  output$tb <- renderUI({
    if(is.null(mydata()))
      h5(tags$img(src='tabiri.png', heigth=600, width=600),
         tags$hr(),
         h5('How to use Tabiri - Check out our ', a("Documentation", href="https://johnmburu.github.io/tabiribiz/"))

         
         )
    
    
    
    else
      tabsetPanel(
        #Forecat Tab
        tabPanel(("Forecast"),
                 column(6,h4("Forecast Plot"), plotOutput("forecast_view"),
                        selectInput(
                          "fplot",
                          "Chose a Forecast algorithm:",
                          c("Naive","Seasonal Naive","Simple Exponential Smoothing","Mean Average Forecast","Drift Forecast","Autoregressive integrated moving average (ARIMA)")),
                        # SLIDER. NUMBER OF DAYS TO FORECAST
                        sliderInput("forecast_days",
                                    "Number of Days to Forecast:",
                                    value = 14,
                                    min = 2,
                                    max = 31),
                        tags$hr(),
                        helpText (h5("Algorithyms definitions: ")),
                        
                        helpText("Seasonal Naive: A similar method is useful for highly seasonal data. In this case, we set each forecast to be equal to the last observed value from the same season of the year (e.g., the same month of the previous year)"),
                        tags$hr(),
                        helpText("Naive: Estimating technique in which the last period's actuals are used as this period's forecast, without adjusting them or attempting to establish causal factors"),
                        tags$hr(),
                        helpText("Simple Exponential Smoothing: Exponential smoothing is a rule of thumb technique for smoothing time series data using the exponential window function"),
                        tags$hr(),
                        helpText("Mean Average Forecast: Simple Moving Average is a method of time series smoothing and is actually a very basic forecasting technique. It does not need estimation of parameters, but rather is based on order selection"),
                        tags$hr(),
                        helpText("Drift Forecast: A variation on the naïve method is to allow the forecasts to increase or decrease over time, where the amount of change over time (called the drift) is set to be the average change seen in the historical data. Thus the forecast for time T +h is given by"),
                 ),
                 column(6, h4("Forecast Data"),tableOutput("forecast_accuracy")
                
                )
        ),
        
        
        # Graph Tab
        tabPanel("Analysis",plotOutput("graphs_view"),
                 selectInput(
                   "plot",
                   "Chose your desired plot type:",
                   c("Trend Line","Area Plot","Scatter Plot","Regression Line"))      
        ),
        tabPanel("Forecast Accuracy", verbatimTextOutput("summary"),
                 
                 selectInput(
                   "f_accuracy",
                   "Select a Forecast algorithm:",
                   c("Naive","Seasonal Naive","Simple Exponential Smoothing","Mean Average Forecast","Drift Forecast","Autoregressive integrated moving average (ARIMA)","Summary")),
                 
                 
                 selectInput("accuracy_item", "check accuracy for :",
                             choices =  c(
                               "Sales" = "sales",
                               "Sales Returns" = "sales_refund",
                               "Purchases" = "purchases",
                               "Cancelled POs" = "purchase_cancelation", selected = NULL)),
                 
                 # SLIDER. NUMBER OF DAYS TO FORECAST
                 sliderInput("forecast_days_acc",
                             "Number of Days to Forecast:",
                             value = 2,
                             min = 2,
                             max = 31)                        
                 ),
       
        tabPanel("Dataset",dataTableOutput("data_set")),
        
        tabPanel("Holidays", dataTableOutput("holidays") )      
                 
      )
  }) 
})