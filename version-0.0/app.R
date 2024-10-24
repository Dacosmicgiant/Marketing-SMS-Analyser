library(shiny)
library(data.table)
library(tm)
library(ggplot2)
library(plotly)
library(DT)

# UI
ui <- fluidPage(
  titlePanel("SMS Offer Analysis"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose CSV File",
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      selectInput("category", "Select Product Category",
                  choices = c("All", "Electronics", "Fashion", "Food", "Sports")),
      # Add download buttons
      downloadButton("downloadProcessed", "Download Processed Data"),
      downloadButton("downloadAnalysis", "Download Analysis Results")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Best Companies", plotlyOutput("bestCompaniesPlot", height = "600px")),
        tabPanel("Offer Types", plotlyOutput("offerTypesPlot", height = "600px")),
        tabPanel("Data Table", DTOutput("dataTable"))
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive value to store the processed data
  processed_data <- reactiveVal()
  
  # Function to preprocess the data using data.table
  preprocess_data <- function(data) {
    # Convert data to data.table
    data <- as.data.table(data)
    
    # Remove duplicates
    data <- unique(data)
    
    # Clean text (remove punctuation, convert to lowercase)
    data[, message := tolower(removePunctuation(message))]
    
    return(data)
  }
  
  # Read and process the uploaded file
  observeEvent(input$file, {
    req(input$file)
    data <- fread(input$file$datapath, stringsAsFactors = FALSE)
    processed <- preprocess_data(data)
    processed_data(processed)
  })
  
  # Filter data based on selected category
  filtered_data <- reactive({
    req(processed_data())
    data <- processed_data()
    
    # Filter using data.table
    if (input$category != "All") {
      data <- data[product_category == input$category]
    }
    return(data)
  })
  
  # Generate best companies plot
  output$bestCompaniesPlot <- renderPlotly({
    req(filtered_data())
    
    # Summarize using data.table
    company_scores <- filtered_data()[, .(score = .N), by = company]
    
    plot_ly(company_scores, x = ~reorder(company, -score), y = ~score, type = "bar", 
            text = ~score, textposition = 'auto') %>%
      layout(title = list(text = "Best Companies by Offer Frequency", font = list(size = 20)),
             xaxis = list(title = list(text = "Company", font = list(size = 14)), 
                          tickfont = list(size = 12),
                          tickangle = 45),
             yaxis = list(title = list(text = "Frequency", font = list(size = 14)), 
                          tickfont = list(size = 12)),
             margin = list(b = 100),
             hoverlabel = list(font = list(size = 12)),
             showlegend = FALSE) %>%
      config(scrollZoom = TRUE, displayModeBar = TRUE)
  })
  
  # Generate offer types plot
  output$offerTypesPlot <- renderPlotly({
    req(filtered_data())
    
    # Count using data.table
    offer_counts <- filtered_data()[, .N, by = message][order(-N)]
    
    plot_ly(offer_counts, x = ~reorder(message, -N), y = ~N, type = "bar", 
            text = ~N, textposition = 'auto') %>%
      layout(title = list(text = "Distribution of Offer Types", font = list(size = 20)),
             xaxis = list(title = list(text = "Offer Type", font = list(size = 14)), 
                          tickfont = list(size = 12),
                          tickangle = 45),
             yaxis = list(title = list(text = "Count", font = list(size = 14)), 
                          tickfont = list(size = 12)),
             margin = list(b = 100),
             hoverlabel = list(font = list(size = 12)),
             showlegend = FALSE) %>%
      config(scrollZoom = TRUE, displayModeBar = TRUE)
  })
  
  # Generate data table
  output$dataTable <- renderDT({
    req(filtered_data())
    datatable(filtered_data()[, .(company, product_category, message)],
              options = list(pageLength = 15, 
                             scrollX = TRUE, 
                             scrollY = "400px",
                             dom = 'ltipr'),
              rownames = FALSE)
  })
  
  # Download handler for processed data
  output$downloadProcessed <- downloadHandler(
    filename = function() {
      paste("processed_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      fwrite(processed_data(), file)
    }
  )
  
  # Download handler for analysis results
  output$downloadAnalysis <- downloadHandler(
    filename = function() {
      paste("analysis_results_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # Generate analysis results
      company_scores <- filtered_data()[, .(score = .N), by = company]
      offer_counts <- filtered_data()[, .N, by = message]
      
      # Combine results
      results <- list(company_scores = company_scores, offer_counts = offer_counts)
      
      # Write results to a CSV file
      fwrite(results$company_scores, file)
      fwrite(results$offer_counts, file, append = TRUE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)