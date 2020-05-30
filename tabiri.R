# Predictive Analytics Framework For Small and Medium Enterprises in Kenya a Case for Sales and Purchase Forecasting
# John Mburu
# mburujr@gmail.com
# Kenya Methodist University
# Msc. Computer Information System

#Load Required Libraries
library(DT)
library(plotly)
library(shiny)
library(ggplot2)
library(shinythemes)
library(packrat)
library(rsconnect)






ui <- fluidPage(
  theme = shinytheme("yeti"),
  #Title of the Client Side
  titlePanel("Tabiri - Forecasting Supply & Demand"),
  
  # Sidebar Layout
  sidebarLayout(
    
    # Sidebar Panel
    sidebarPanel(
      
      #Download the Template .csv file
      hr(), 
      tags$a(href='./data/template.csv', target='blank', 'Download the Template to use for Prediction', download = 'template.csv'),
      
      #FILE INPUT
      fileInput('file1', 'Choose file to upload',
                accept = c(
                  '.csv',
                  '.xls',
                  '.xlsx'
                )
      ),
      
      checkboxInput("header", "Header", TRUE),
      
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head"),
      
      
      
      
      #ploting sales
      selectInput("x", "Select the first X variable:",
                  choices =  c("Sales" = "Sales",
                               "Purchases" = "Purchases"
                  )),
      
      selectInput("y", "Select the first Y variable:",
                  choices =  c(
                    "Sales Quantity" = "Sales_Quantity",
                    "Purchase Quantity" = "Purchase_Quantity")),
      
      #ploting the regression model
      actionButton(inputId = "click3", label = "Plot Regression Model Residuals"),
      hr(),
      #ploting the sales prediction
      actionButton(inputId = "click4", label = "Plot Sales Prediction"),
      hr(),
      
      #predict per Location-
      selectInput("pre", "Predicting by SME Location",
                  choices =  c("Urban Area" = "urban",
                               "Rural Area" = "rural",
                               "Urban & Rural" = "u_r")),
      actionButton(inputId = "click5", label = "Predict"),
      
      #predict per Sector
      selectInput("pre", "Predicting by SME Sector",
                  choices =  c( "Energy" = "energy",
                                "Materials" = "materials",
                                "Industrials" = "industrials",
                                "Consumer Discretionary" = "consumer_discretionary",
                                "Consumer Staples" = "consumer_staples",
                                "Health Care" = "health_care",
                                "Financials" = "financials",
                                "Information Technology" = "information_technology",
                                "Communication Services" = "communication_services",
                                "Utilities" = "utilities",
                                "Real Estate" = "real_estate")
      ),
      actionButton(inputId = "click6", label = "Predict"),     
      
      
      #DISTRIBUTION TYPE
      radioButtons("dist", "Select Your Desired Grapht type:",
                   choices = c(
                     "Line Graph" = "line",
                     "Bar Graph" = "bar",
                     "Histogram" = "hist",
                     "Box Plot" = "box",
                     "Density Plot" = "density"), selected = "Line Graph"),
      hr(),
      
      # Input: Slider for the number of observations to generate ----
      #sliderInput("n",
      #          "Number of observations:",
      #          value = 7,
      #          min = 1,
      #          max = 30)
      
    ),
    
    
    # Main panel for displaying outputs ----
    mainPanel(
      # DT::dataTableOutput('contents'),
      
      # Output: Tabset data, salesanalysis, and predictions ----
      tabsetPanel(type = "tabs",
                  tabPanel("Raw Uploaded Data", dataTableOutput('contents')),
                  tabPanel("Analysis", dataTableOutput("analysis")),
                  tabPanel("Predictions", plotOutput("predictions"))
                  
      )
      
    )
    
  )
  
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  mydata <- reactive({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    data <-read.csv(inFile$datapath, header = input$header)
    data
    
    #outputing the head or all data only
    if (input$disp == "head") {
      return(head(data))
    }
    else {
      return(data)
    }
    
    
  })
  output$contents <- DT::renderDataTable({
    DT::datatable(mydata())       
  })
  
  output$analysis<-renderText({
    switch(input$dist,
           "line" = "Line Graph",
           "box" 	= 	"Box Plot",
           "hist" =	"Histogram",
           "density" 	=	"Density",
           "bar" 		=	"Bar Graph")
  })
  
  
  output$salesanalysis <- renderPlot({
    #  Plot the table  
    matplot(data[, 1], data[, 2:21],  type="l", main="Matplot", xlab="x", ylab="y")    
    grid() 
  })
  
  
}


# Create Shiny object
shinyApp(ui = ui, server = server)