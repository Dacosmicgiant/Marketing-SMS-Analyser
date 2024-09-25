
# Required packages
library(shiny)
library(tidyverse)
library(tm)
library(ggplot2)
library(quanteda)
library(plotly)
library(caret)

# UI
ui <- fluidPage(
  titlePanel("SMS Offer Analysis"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose CSV File",
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      selectInput("category", "Select Product Category",
                  choices = c("All", "Electronics", "Fashion", "Food"))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Best Companies", plotlyOutput("bestCompaniesPlot")),
        tabPanel("Offer Types", plotOutput("offerTypesPlot")),
        tabPanel("Data Table", dataTableOutput("dataTable"))
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive value to store the processed data
  processed_data <- reactiveVal()
  
  # Function to preprocess the data
  preprocess_data <- function(data) {
    # Remove duplicates
    data <- distinct(data)
    
    # Clean text (remove punctuation, convert to lowercase)
    data$clean_text <- tolower(removePunctuation(data$text))
    
    # Create a document-feature matrix using quanteda
    corpus <- corpus(data$clean_text)
    dfm <- dfm(corpus, tolower = TRUE, remove_punct = TRUE) %>%
      dfm_trim(min_termfreq = 5, verbose = FALSE)
    
    # Convert to a data frame for model training
    dfm_data <- as.data.frame(as.matrix(dfm))
    dfm_data$company <- factor(ifelse(grepl("amazon", data$clean_text), "Amazon",
                                      ifelse(grepl("walmart", data$clean_text), "Walmart", 
                                             ifelse(grepl("target", data$clean_text), "Target", "Other"))))
    
    return(list(dfm_data = dfm_data, dfm_matrix = dfm))
  }
  
  # Train a Machine Learning model
  train_model <- function(dfm_data) {
    # Split the data into training and testing sets
    set.seed(123)
    training_indices <- createDataPartition(dfm_data$company, p = 0.8, list = FALSE)
    training_data <- dfm_data[training_indices, ]
    testing_data <- dfm_data[-training_indices, ]
    
    # Train a Naive Bayes model
    model <- train(company ~ ., data = training_data, method = "nb",
                   trControl = trainControl(method = "cv", number = 5))
    
    return(list(model = model, testing_data = testing_data))
  }
  
  # Predict using the trained model
  predict_company <- function(model, dfm_matrix) {
    predictions <- predict(model, dfm_matrix)
    return(predictions)
  }
  
  # Read and process the uploaded file
  observeEvent(input$file, {
    req(input$file)
    data <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
    
    # Preprocess the data
    processed <- preprocess_data(data)
    dfm_data <- processed$dfm_data
    
    # Train the model
    model_info <- train_model(dfm_data)
    model <- model_info$model
    
    # Predict company for each SMS
    predictions <- predict_company(model, processed$dfm_matrix)
    data$predicted_company <- predictions
    
    processed_data(data)
  })
  
  # Filter data based on selected category
  filtered_data <- reactive({
    req(processed_data())
    data <- processed_data()
    if (input$category != "All") {
      data <- data %>% filter(category == input$category)
    }
    return(data)
  })
  
  # Generate best companies plot
  output$bestCompaniesPlot <- renderPlotly({
    req(filtered_data())
    
    company_scores <- filtered_data() %>%
      group_by(predicted_company) %>%
      summarise(score = n())
    
    plot_ly(company_scores, x = ~predicted_company, y = ~score, type = "bar") %>%
      layout(title = "Best Companies by Offer Classification",
             xaxis = list(title = "Company"),
             yaxis = list(title = "Score"))
  })
  
  # Generate offer types plot
  output$offerTypesPlot <- renderPlot({
    req(filtered_data())
    
    offer_counts <- filtered_data() %>%
      count(offer_type)
    
    ggplot(offer_counts, aes(x = offer_type, y = n, fill = offer_type)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(title = "Distribution of Offer Types",
           x = "Offer Type", y = "Count")
  })
  
  # Generate data table
  output$dataTable <- renderDataTable({
    req(filtered_data())
    filtered_data() %>%
      select(predicted_company, offer_type, category, text)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
