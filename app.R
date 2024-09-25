# Required packages
library(shiny)
library(tidyverse)
library(tm)
library(ggplot2)
library(quanteda)
library(plotly)

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
    
    # Classify company (example logic, replace with your own)
    data$company <- sapply(data$clean_text, function(text) {
      if (grepl("amazon", text)) return("Amazon")
      if (grepl("walmart", text)) return("Walmart")
      if (grepl("target", text)) return("Target")
      return("Other")
    })
    
    # Classify offer type (example logic, replace with your own)
    data$offer_type <- sapply(data$clean_text, function(text) {
      if (grepl("discount", text)) return("Discount")
      if (grepl("promotion", text)) return("Promotion")
      if (grepl("sale", text)) return("Sale")
      return("Other")
    })
    
    # Classify product category (example logic, replace with your own)
    data$category <- sapply(data$clean_text, function(text) {
      if (grepl("electronics|phone|laptop", text)) return("Electronics")
      if (grepl("fashion|clothing|shoes", text)) return("Fashion")
      if (grepl("food|grocery|restaurant", text)) return("Food")
      return("Other")
    })
    
    return(data)
  }
  
  # Read and process the uploaded file
  observeEvent(input$file, {
    req(input$file)
    data <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
    processed <- preprocess_data(data)
    processed_data(processed)
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
      group_by(company) %>%
      summarise(score = n() * mean(as.numeric(factor(offer_type))))
    
    plot_ly(company_scores, x = ~company, y = ~score, type = "bar") %>%
      layout(title = "Best Companies by Offer Quality and Frequency",
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
      select(company, offer_type, category, text)
  })
}

# Run the application
shinyApp(ui = ui, server = server)