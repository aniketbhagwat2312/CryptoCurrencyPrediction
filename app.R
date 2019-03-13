# set working directory where cleaned_crypto_data.csv and utils.R file is present
# install.packages("shiny","shinydashboard","DT","ggplot2","plotly","ggfortify","forecast","tseries","gridExtra","readr","xts","anytime","e1071","prophet","DescTools")
# load required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(plotly)
library(ggfortify)
library(forecast)
library(tseries)
library(gridExtra)
library(readr)
library(xts)
library(anytime)
library(e1071)
library(prophet)
library(DescTools)

# load utils file for utility functions
source("utils.R")
set.seed(2312)

# read dataset from csv file and load into a dataframe 
crypto_data <-
  read.csv('cleaned_crypto_data.csv',
           header = TRUE)
# initialise mydata to null. This variable needs globle access so defining it globally
myData <- NULL

# define UI component of shiny application
ui <- dashboardPage(
  # dashboard title
  dashboardHeader(title = "Cryptocurrency’s price prediction using various time series techniques",
                  titleWidth = 650),
  dashboardSidebar(disable = TRUE),
  dashboardBody(fluidRow(
    column(
      width = 9,
      box(
        # define multiple tabs to plot graphs
        tabsetPanel(
          tabPanel("Data", plotOutput("forecaste_plot4")),
          tabPanel("Decomposition", plotOutput("forecaste_plot6")),
          tabPanel("Seasonality", plotOutput("forecaste_plot7")),
          tabPanel("Forecast", plotOutput("forecast_plot2")),
          tabPanel("Prediction", plotlyOutput("forecast_plot1")),
          tabPanel("Test vs prediction", plotlyOutput("forecaste_plot3"))
        ),
        width = NULL,
        collapsible = T,
        title = "Plots"
      ),
      # show text output to print summary
      box(
        verbatimTextOutput('summary'),
        width = NULL,
        title = "summary",
        collapsible = T
      )
    ),
    
    column(
      width = 3,
      box(
        # drop-down to select currency
        selectInput(
          "currency",
          "Select cryptocurrency",
          choices = c('Bitcoin', 'Ethereum', 'Ripple', 'Litecoin', 'IOTA'),
          selected = "Bitcoin"
        ),
        # drop-down for price variable selection
        selectInput(
          "type",
          "Select price variable",
          choices = c("close",
                      "high",
                      "low",
                      "open"),
          selected = "close"
        ),
        # drop-down for model selection
        selectInput(
          "model",
          "Select Model",
          choices = c('Arima',
                      'SVM',
                      'Neural network',
                      "My hybrid model",
                      'Prophet'),
          selected = "Arima"
        ),
        # Numeric input to divide data into train and test set
        numericInput(
          "train_percent",
          "Train data percentage",
          min = 10,
          max = 99,
          value = 75
        ),
        #),
        # Buttton to apply model
        actionButton("go", "Apply"),
        box(
          sliderInput(
            "stepsize",
            "Select stepsize",
            min = 2,
            max = 30,
            step = 1,
            value = 7
          ),
          width = NULL
        ),
        width = NULL,
        title = "Inputs"
      ),
      box(
        # accuracy table
        DT::dataTableOutput("accuracy_table"),
        width = NULL,
        title = "Accuracy table",
        collapsible = T
      )
      
    )
  ))
)

server <- function(input, output, session) {
  
  # this variables are created to store data in globaly so that other 
  # functions can also use same data
  applied_model <- reactiveValues()
  train_data <- reactiveValues()
  test_data <- reactiveValues()
  forecast_model <- reactiveValues()
  time_series_xts_object <- reactiveValues()
  time_series_ts_object <- reactiveValues()
  input_currency <- reactiveValues()
  input_column <- reactiveValues()
  input_model <- reactiveValues()
  
  
  # plot the forecast plot using plotly library
  output$forecast_plot1 <- renderPlotly({
    # create model as per the drop-down selection
    createmodel()
    if (input_model$data == "Prophet") {
      # if model is prophet call this method to save prediction in forecast_model global object  
      future <-
        calculate_future()
      # call method in utils.R file
      prophetPlotly(
        time_series_xts_object$data,
        # pass only required number of entries to function
        test_data$data$date[1:input$stepsize],
        forecast_model$data,
        input$stepsize
      )
    } else if (input_model$data == "SVM") {
      # if model is SVM then create new data frame
      nd <- test_data$data$date[1:input$stepsize]
      # calculate forecast
      forecast_model$data <-
        predict(applied_model$data, newdata = data.frame(x = nd))
      # call method in utils.R
      SVMPlotly(
        time_series_xts_object$data,
        test_data$data$date[1:input$stepsize],
        forecast_model$data,
        input$stepsize
      )
    } else if (input_model$data == "Neural network" ||
               input_model$data == "My hybrid model") {
      # id model is Neuralor hybrid then call this methos for prediction
      forecast_model$data <-
        forecast(applied_model$data, h = input$stepsize)
      neuralPlotly(
        time_series_xts_object$data,
        test_data$data$date[1:input$stepsize],
        forecast_model$data,
        input$stepsize
      )
    }
    else{
      # for ARIMA model
      forecast_model$data <-
        forecast(applied_model$data, h = input$stepsize)
      createPlotlyForecastGraph(time_series_xts_object$data,
                                test_data$data$date[1:input$stepsize],
                                forecast_model$data)
    }
  })
  
  output$forecast_plot2 <- renderPlot({
    createmodel()
    if (input_model$data == "Arima" ||
        input_model$data == "Neural network" ||
        input_model$data == "My hybrid model") {
      forecast_model$data <-
        forecast(applied_model$data, h = input$stepsize)
      plot(forecast_model$data)
    } else if (input_model$data == "SVM") {
      nd <- test_data$data$date[1:input$stepsize]
      forecast_model$data <-
        predict(applied_model$data, newdata = data.frame(x = nd))
      plot(forecast_model$data)
    } else if (input_model$data == "Prophet") {
      calculate_future()
      plot(applied_model$data, forecast_model$data)
    }
    
  })
  
  output$forecaste_plot3 <- renderPlotly({
    createmodel()
    if (input_model$data == "Arima" ||
        input_model$data == "Neural network" ||
        input_model$data == "My hybrid model") {
      forecast_model$data <-
        forecast(applied_model$data, h = input$stepsize)
    } else if (input_model$data == "SVM") {
      nd <- test_data$data$date[1:input$stepsize]
      forecast_model$data <-
        predict(applied_model$data, newdata = data.frame(x = nd))
    } else if (input_model$data == "Prophet") {
      calculate_future()
    }
    createComparisonGraph(
      test_data$data,
      input$stepsize,
      forecast_model$data,
      input_column$data,
      input_model$data
    )
  })
  
  output$forecaste_plot4 <- renderPlot({
    createmodel()
    plot_time_series(
      time_series_xts_object$data,
      input_currency$data,
      input_model$data,
      input_column$data
    )
  })
  
  
  output$forecaste_plot6 <- renderPlot({
    createmodel()
    plot_decomp(time_series_ts_object$data,
                input_currency$data,
                input_column$data)
  })
  
  output$forecaste_plot7 <- renderPlot({
    createmodel()
    plot_seasonal(time_series_ts_object$data,
                  input_currency$data,
                  input_column$data)
  })
  
  # print summary of model
  output$summary <- renderPrint({
    createmodel()
    if (input_model$data == "Neural network" ||
        input_model$data == "My hybrid model") {
      summary.neural(applied_model$data)
    } else if (input_model$data == "Prophet") {
      summary.prophet(applied_model$data)
    } else{
      summary(applied_model$data)
    }
  })
  
  # print accuracy table
  output$accuracy_table = DT::renderDataTable({
    createmodel()
    if (input_model$data == "Arima" ||
        input_model$data == "Neural network" ||
        input_model$data == "My hybrid model") {
      forecast_model$data <-
        forecast(applied_model$data, h = input$stepsize)
      acc_arima <-
        round(accuracy(forecast_model$data, test_data$data[[input$type]][1:input$stepsize]), 4)
      t(acc_arima[, c(2, 5)])
    } else if (input_model$data == "SVM") {
      nd <- test_data$data$date[1:input$stepsize]
      forecast_model$data <-
        predict(applied_model$data, newdata = data.frame(x = nd))
      acc_svm <-
        round(SVMAccuracy(test_data$data[[input$type]], forecast_model$data, input$stepsize),
              4)
      t(acc_svm[, c(2, 5)])
    } else if (input_model$data == "Prophet") {
      calculate_future()
      acc_proph <-
        round(prophetAccuracy(test_data$data[[input$type]], forecast_model$data, input$stepsize),
              4)
      t(acc_proph[, c(2, 5)])
    }
  })
  
  # call each models create function based on drop-down selection
  createmodel <- eventReactive(input$go, {
    # store values in global variable so that unless button is clicked no change will reflect on UI
    input_currency$data <- input$currency
    input_column$data <- input$type
    input_model$data <- input$model
    switch (
      input$model,
      "Arima" = createArimaModel(),
      "Exponential Time smoothening" = createEtsModel(),
      "Neural network" = createNeuralModel(),
      "My hybrid model" = createMyHybridModel(),
      "SVM" = createSVMModel(),
      "Prophet" = createProphetModel()
    )
  })
  
  calculate_future <- reactive({
    if (input_model$data == "Prophet") {
      future <-
        make_future_dataframe(
          applied_model$data,
          periods = input$stepsize,
          include_history = T
        )
      forecast_model$data <- predict(applied_model$data, future)
    }
  })
  
  # apply arima model on dataset
  createArimaModel <- reactive({
    # select cureency specif data from dataframe
    myData <- crypto_data[crypto_data$name == input$currency,]
    # decide how many rows for train data and remaing for test data set
    n <- getDatasetDivideValue(input$train_percent, nrow(myData))
    # devide data into train set and test set
    train_data$data <- myData[c (1:(nrow(myData) - n)),]
    test_data$data <-
      myData[c ((nrow(myData) - n):nrow(myData)),]
    train_data$data$date <- as.Date(anytime(train_data$data$date))
    test_data$data$date <- as.Date(anytime(test_data$data$date))
    # create xtsr objectto convert data into time series format
    xtsr <-
      xts(train_data$data[[input$type]], order.by = as.Date(train_data$data$date))
    tsr <-
      ts(
        train_data$data[[input$type]],
        frequency = 365.25,
        start = getDateAsVector(train_data$data$date[1])
      )
    # save both objects globaly
    # some graph requires ts object and some require xts so save both
    time_series_xts_object$data <- xtsr
    time_series_ts_object$data <- tsr
    applied_model$data <- auto.arima(xtsr)
  })
  
  createEtsModel <- reactive({
    myData <- crypto_data[crypto_data$name == input$currency,]
    xtsr <-
      xts(train_data$data[[input$type]], order.by = as.Date(train_data$data$date))
    tsr <-
      ts(
        train_data$data[[input$type]],
        frequency = 365,
        start = getDateAsVector(train_data$data$date[1])
      )
    time_series_ts_object$data <- tsr
    time_series_xts_object$data <- xtsr
    applied_model$data <- ets(xtsr)
  })
  
  # Create feed forward neural network
  createNeuralModel <- reactive({
    myData <- crypto_data[crypto_data$name == input$currency,]
    n <- getDatasetDivideValue(input$train_percent, nrow(myData))
    train_data$data <- myData[c (1:(nrow(myData) - n)),]
    test_data$data <- myData[c ((nrow(myData) - n):nrow(myData)),]
    train_data$data$date <- as.Date(anytime(train_data$data$date))
    test_data$data$date <- as.Date(anytime(test_data$data$date))
    xtsr <-
      xts(train_data$data[[input$type]], order.by = as.Date(train_data$data$date))
    tsr <-
      ts(
        train_data$data[[input$type]],
        frequency = 365,
        start = getDateAsVector(train_data$data$date[1])
      )
    time_series_xts_object$data <- xtsr
    time_series_ts_object$data <- tsr
    applied_model$data <- nnetar(tsr, lambda = 1)
  })
  
  createMyHybridModel <- reactive({
    myData <- crypto_data[crypto_data$name == input$currency,]
    n <- getDatasetDivideValue(input$train_percent, nrow(myData))
    train_data$data <- myData[c (1:(nrow(myData) - n)),]
    test_data$data <- myData[c ((nrow(myData) - n):nrow(myData)),]
    train_data$data$date <- as.Date(anytime(train_data$data$date))
    test_data$data$date <- as.Date(anytime(test_data$data$date))
    xtsr <-
      xts(train_data$data[[input$type]], order.by = as.Date(train_data$data$date))
    tsr <-
      ts(
        train_data$data[[input$type]],
        frequency = 365,
        start = getDateAsVector(train_data$data$date[1])
      )
    time_series_xts_object$data <- xtsr
    time_series_ts_object$data <- tsr
    new_train <-
      data.frame(date = train_data$data$date, close = train_data$data[[input$type]])
    arimaModel <- auto.arima(xtsr)
    svm_pred <- fitted(arimaModel)
    # apply actual model to data store model in global variable
    applied_model$data <- nnetar(svm_pred, lambda = 0.5)
  })
  
  # create svm model
  createSVMModel <- reactive({
    myData <- crypto_data[crypto_data$name == input$currency,]
    n <- getDatasetDivideValue(input$train_percent, nrow(myData))
    train_data$data <- myData[c (1:(nrow(myData) - n)),]
    test_data$data <- myData[c ((nrow(myData) - n):nrow(myData)),]
    train_data$data$date <- as.Date(anytime(train_data$data$date))
    test_data$data$date <- as.Date(anytime(test_data$data$date))
    xtsr <-
      xts(train_data$data[[input$type]], order.by = as.Date(train_data$data$date))
    tsr <-
      ts(
        train_data$data[[input$type]],
        frequency = 365,
        start = getDateAsVector(train_data$data$date[1])
      )
    time_series_xts_object$data <- xtsr
    time_series_ts_object$data <- tsr
    new_train <-
      data.frame(date = train_data$data$date, close = train_data$data[[input$type]])
    applied_model$data <- svm(
      tsr ~ train_data$data$date,
      data = new_train,
      type = "eps-regression",
      kernel = "radial",
      cost = 10000,
      gamma = 100
    )
  })
  
  #create prophet model
  createProphetModel <- reactive({
    myData <- crypto_data[crypto_data$name == input$currency,]
    n <- getDatasetDivideValue(input$train_percent, nrow(myData))
    train_data$data <- myData[c (1:(nrow(myData) - n)),]
    test_data$data <- myData[c ((nrow(myData) - n):nrow(myData)),]
    train_data$data$date <- as.Date(anytime(train_data$data$date))
    test_data$data$date <- as.Date(anytime(test_data$data$date))
    xtsr <-
      xts(train_data$data[[input$type]], order.by = as.Date(train_data$data$date))
    tsr <-
      ts(
        train_data$data[[input$type]],
        frequency = 365,
        start = getDateAsVector(train_data$data$date[1])
      )
    time_series_xts_object$data <- xtsr
    time_series_ts_object$data <- tsr
    ds <- train_data$data$date
    y <- train_data$data[[input$type]]
    new_train <- data.frame(ds, y = log(y))
    applied_model$data <- prophet(new_train)
  })
}

# start shiny app with UI and server
shinyApp(ui, server)
