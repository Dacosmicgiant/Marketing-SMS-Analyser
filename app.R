library(shiny)
library(data.table)
library(tm)
library(ggplot2)
library(plotly)

# UI
ui <- fluidPage(
  titlePanel("SMS Offer Analysis"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose CSV File",
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      selectInput("category", "Select Product Category",
                  choices = c("All", "Electronics", "Fashion", "Food", "Sports"))
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
    
    plot_ly(company_scores, x = ~company, y = ~score, type = "bar") %>%
      layout(title = "Best Companies by Offer Frequency",
             xaxis = list(title = "Company"),
             yaxis = list(title = "Frequency"))
  })
  
  # Generate offer types plot
  output$offerTypesPlot <- renderPlot({
    req(filtered_data())
    
    # Count using data.table
    offer_counts <- filtered_data()[, .N, by = message]
    
    ggplot(offer_counts, aes(x = message, y = N, fill = message)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(title = "Distribution of Offer Types", x = "Offer Type", y = "Count")
  })
  
  # Generate data table
  output$dataTable <- renderDataTable({
    req(filtered_data())
    filtered_data()[, .(company, product_category, message)]
  })
}

# Run the application
shinyApp(ui = ui, server = server)