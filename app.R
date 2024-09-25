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
                  choices = c("All", "Electronics", "Fashion", "Food")),
      # New download buttons
      downloadButton("downloadProcessed", "Download Processed Data"),
      downloadButton("downloadAnalysis", "Download Analysis Results")
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
  
  # Function to preprocess the data (same as before)
  preprocess_data <- function(data) {
    # ... (preprocessing logic remains the same)
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
  
  # Download handler for processed data
  output$downloadProcessed <- downloadHandler(
    filename = function() {
      paste("processed_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(processed_data(), file, row.names = FALSE)
    }
  )
  
  # Download handler for analysis results
  output$downloadAnalysis <- downloadHandler(
    filename = function() {
      paste("analysis_results_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # Generate analysis results
      company_scores <- filtered_data() %>%
        group_by(company) %>%
        summarise(
          total_offers = n(),
          avg_score = mean(as.numeric(factor(offer_type))),
          score = total_offers * avg_score
        )
      
      offer_counts <- filtered_data() %>%
        count(offer_type) %>%
        mutate(percentage = n / sum(n) * 100)
      
      # Combine results
      analysis_results <- list(
        company_scores = company_scores,
        offer_counts = offer_counts
      )
      
      # Write results to a CSV file
      write.csv(analysis_results$company_scores, file, row.names = FALSE)
      write.csv(analysis_results$offer_counts, file, append = TRUE, row.names = FALSE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)