library(shiny) 
library(DT)
library(shinyWidgets) 
library(ggplot2)

ui <- fluidPage(
  titlePanel("Gas Price Prediction"),
  sidebarPanel(
    
    fileInput(
      inputId = "filedata",
      label = "Upload Gas_Price_Data_Merged. csv",
      multiple = FALSE,
      accept = c(".csv"),
      buttonLabel = "Choosing ...",
      placeholder = "No files selected yet"
    ),
    uiOutput("xvariable"),
    uiOutput("yvariable"),
    sliderInput(
      "Slider1",
      label = h3("Train/Test Split %"),
      min = 0,
      max = 100,
      value = 80
    ),
    textOutput("cntTrain"),
    textOutput("cntTest"),
    br()
    
  ), #sidebarpanel
  
  mainPanel( #DTOutput("tb1"), 
    fluidRow(column(10, verbatimTextOutput('lmSummary')), column(8, plotOutput('scatterPlotTrain')), 
             column(8, plotOutput('scatterPlotTest')))
  )
) #fluidpage


server <- function(input, output) {
  
  data <- reactive({
    req(input$filedata)
    inData <- input$filedata
    if (is.null(inData)){ return(NULL) }
    mydata <- read.csv(inData$datapath, header = TRUE, sep=",")
  })
  output$tb1 <- renderDT(data())
  
  output$xvariable <- renderUI({
    req(data())
    xa<-colnames(data())
    pickerInput(inputId = 'xvar',
                label = 'Select independent variable',
                choices = c(xa[1:length(xa)]), selected=xa[2],
                options = list(`style` = "btn-info"),
                multiple = TRUE)
    
  })
  output$yvariable <- renderUI({
    req(data())
    ya<-colnames(data()) 
    pickerInput(inputId = 'yvar',
                label = 'Select dependent variable',
                choices = c(ya[1:length(ya)]), selected=ya[1],
                options = list(`style` = "btn-info"),
                multiple = FALSE)
    
  })
  
  splitSlider <- reactive({
    input$Slider1 / 100
  })
  set.seed(100)  # setting seed to reproduce results of random sampling
  trainingRowIndex <-
    reactive({
      sample(1:nrow(data()),
             splitSlider() * nrow(data()))
    })# row indices for training data
  
  trainingData <- reactive({
    tmptraindt <- data()
    tmptraindt[trainingRowIndex(), ]
  })
  
  testData <- reactive({
    tmptestdt <- data()
    tmptestdt[-trainingRowIndex(),]
  })
  
  output$cntTrain <-
    renderText(paste("Train Data:", NROW(trainingData()), "records"))
  output$cntTest <-
    renderText(paste("Test Data:", NROW(testData()), "records"))
  
  lmModel <- reactive({
    tmp_training_data <- trainingData()
    tmp_training_data$Date <- as.numeric(as.Date(tmp_training_data$Date,  format="%m/%d/%Y"))
    tmp_training_data$Gas_Price_Date <- as.numeric(as.Date(tmp_training_data$Gas_Price_Date,  format="%m/%d/%Y"))
    req(data(),input$xvar,input$yvar)
    x <- as.numeric(data()[[as.name(input$xvar)]])
    y <- as.numeric(data()[[as.name(input$yvar)]])
    current_formula <- paste0(input$yvar, " ~ ", paste0(input$xvar, collapse = " + "))
    current_formula <- as.formula(current_formula)
    model <- lm(current_formula, data = tmp_training_data, na.action=na.exclude)
    return(model)
  })
  
  scatter_train <- reactive({
    tmp_train_data <- trainingData()
    tmp_lm_model <- lmModel()
    
    scatter_best_train <- data.frame(gasprice_actual = tmp_train_data[,"Gas_price_per_gallon"],
                                     best_fitted_values = tmp_lm_model$fitted.values)
    print(scatter_best_train)
  })
  
  scatter_test <- reactive({
    tmp_test_data <- testData()
    predict_lm <- predict(lmModel(), newdata = testData())
    
    scatter_best_test <- data.frame(gasprice_actual = tmp_test_data[,"Gas_price_per_gallon"],
                                    best_pred = predict_lm)
    print(scatter_best_test)
  })
  
  output$lmSummary <- renderPrint({
    req(lmModel())
    summary(lmModel())
  })
  
  # output$diagnosticPlot <- renderPlot({
  #   req(lmModel())
  #   par(mfrow = c(2,2))
  #   plot(lmModel())
  # })
  
  output$scatterPlotTrain<-renderPlot({
    ggplot(scatter_train()) + 
      geom_point(aes(x=best_fitted_values, y=gasprice_actual)) +
      geom_abline(color='red') + ggtitle('Scatter Plot of Gas Price Actual Vs Fitted Values', subtitle = 'Train Data')})
  
  output$scatterPlotTest<-renderPlot({
    ggplot(scatter_test()) + 
      geom_point(aes(x=best_pred, y=gasprice_actual)) +
  geom_abline(color='red') + ggtitle('Scatter Plot of Gas Price Actual Vs Predicted', subtitle = 'Test Data')})
 
}

shinyApp(ui = ui, server = server)
