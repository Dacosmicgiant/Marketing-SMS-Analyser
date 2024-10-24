# Load required libraries
library(data.table)
library(ggplot2)
library(plotly)
library(lubridate)
library(corrplot)
library(DT)
library(tm)

# 1. Data Extraction
# Read the generated dataset
sms_data <- fread("D:/VEDANT/projects/SMSAnalyser/mod/sms_marketing_data.csv")

# 2. Data Cleaning and Preparation
clean_sms_data <- function(dt) {
  # Create copy to avoid modifying original
  cleaned <- copy(dt)
  
  # Remove any duplicates
  cleaned <- unique(cleaned)
  
  # Convert date column to Date type if not already
  cleaned[, date := as.Date(date)]
  
  # Add derived features
  cleaned[, `:=`(
    month = month(date),
    weekday = wday(date, label = TRUE),
    week = week(date)
  )]
  
  # Clean text messages
  cleaned[, message_clean := tolower(removePunctuation(message))]
  
  return(cleaned)
}

# Clean the data
cleaned_data <- clean_sms_data(sms_data)

# 3. Exploratory Data Analysis

# a. Summary statistics by company
company_stats <- cleaned_data[, .(
  n_campaigns = .N,
  avg_discount = mean(discount_percentage),
  avg_response = mean(response_rate),
  total_response = sum(response_rate)
), by = .(company, product_category)]

# b. Summary statistics by category
category_stats <- cleaned_data[, .(
  n_campaigns = .N,
  avg_discount = mean(discount_percentage),
  avg_response = mean(response_rate),
  total_response = sum(response_rate)
), by = product_category]

# 4. Data Mining and Analysis

# a. Best performing companies
best_companies <- company_stats[order(-total_response)]

# b. Discount effectiveness analysis
discount_effectiveness <- cleaned_data[, .(
  avg_response = mean(response_rate),
  n_campaigns = .N
), by = .(discount_percentage, product_category)]

# c. Time-based analysis
time_analysis <- cleaned_data[, .(
  avg_response = mean(response_rate),
  n_campaigns = .N
), by = .(weekday, product_category)]

# 5. Visualization

# a. Company Performance Plot
company_plot <- ggplot(best_companies, 
                       aes(x = reorder(company, -total_response), 
                           y = total_response, 
                           fill = product_category)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Company Performance by Total Response",
       x = "Company",
       y = "Total Response Rate")

# b. Discount Effectiveness Plot
discount_plot <- ggplot(discount_effectiveness, 
                        aes(x = discount_percentage, 
                            y = avg_response, 
                            color = product_category)) +
  geom_smooth(method = "loess") +
  theme_minimal() +
  labs(title = "Discount Effectiveness by Category",
       x = "Discount Percentage",
       y = "Average Response Rate")

# c. Weekly Pattern Plot
weekday_plot <- ggplot(time_analysis, 
                       aes(x = weekday, 
                           y = avg_response, 
                           fill = product_category)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Response Rates by Weekday",
       x = "Day of Week",
       y = "Average Response Rate")

# Function to generate comprehensive report
generate_report <- function() {
  cat("SMS Marketing Analysis Report\n")
  cat("============================\n\n")
  
  cat("1. Top Performing Companies:\n")
  print(best_companies[1:5])
  
  cat("\n2. Category Performance:\n")
  print(category_stats)
  
  cat("\n3. Best Days for Marketing:\n")
  print(time_analysis[order(-avg_response)][1:5])
  
  cat("\n4. Optimal Discount Ranges:\n")
  discount_ranges <- discount_effectiveness[, .(
    avg_response = mean(avg_response)
  ), by = .(
    discount_range = cut(discount_percentage, 
                         breaks = seq(0, 70, by = 10))
  )][order(-avg_response)]
  print(discount_ranges)
}

# Save plots
ggsave("D:/VEDANT/projects/SMSAnalyser/mod/company_performance.png", company_plot)
ggsave("D:/VEDANT/projects/SMSAnalyser/mod/discount_effectiveness.png", discount_plot)
ggsave("D:/VEDANT/projects/SMSAnalyser/mod/weekday_pattern.png", weekday_plot)

# Generate and save report
sink("D:/VEDANT/projects/SMSAnalyser/mod/marketing_analysis_report.txt")
generate_report()
sink()
# Create interactive dashboard
if (interactive()) {
  library(shiny)
  library(shinydashboard)
  
  # Additional plots needed for dashboard
  category_plot <- ggplot(category_stats, 
                          aes(x = reorder(product_category, -total_response), 
                              y = total_response, 
                              fill = product_category)) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    labs(title = "Category Performance",
         x = "Product Category",
         y = "Total Response")
  
  month_plot <- ggplot(cleaned_data[, .(
    avg_response = mean(response_rate)
  ), by = .(month, product_category)],
  aes(x = factor(month), y = avg_response, fill = product_category)) +
    geom_bar(stat = "identity", position = "dodge") +
    theme_minimal() +
    labs(title = "Monthly Response Rates",
         x = "Month",
         y = "Average Response Rate")
  
  ui <- dashboardPage(
    dashboardHeader(title = "SMS Marketing Analysis"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Overview", tabName = "overview"),
        menuItem("Company Analysis", tabName = "company"),
        menuItem("Time Analysis", tabName = "time")
      )
    ),
    dashboardBody(
      tabItems(
        # Overview Tab
        tabItem(tabName = "overview",
                fluidRow(
                  box(width = 12, plotlyOutput("company_plot", height = "400px")),
                  box(width = 12, DTOutput("company_table"))
                )),
        # Company Analysis Tab
        tabItem(tabName = "company",
                fluidRow(
                  box(width = 6, plotlyOutput("discount_plot", height = "400px")),
                  box(width = 6, plotlyOutput("category_plot", height = "400px"))
                )),
        # Time Analysis Tab
        tabItem(tabName = "time",
                fluidRow(
                  box(width = 6, plotlyOutput("weekday_plot", height = "400px")),
                  box(width = 6, plotlyOutput("month_plot", height = "400px"))
                ))
      )
    )
  )
  
  server <- function(input, output) {
    # Overview tab outputs
    output$company_plot <- renderPlotly({
      ggplotly(company_plot) %>%
        layout(showlegend = TRUE)
    })
    
    output$company_table <- renderDT({
      datatable(best_companies,
                options = list(pageLength = 10,
                               scrollX = TRUE,
                               dom = 'Bfrtip'),
                rownames = FALSE)
    })
    
    # Company Analysis tab outputs
    output$discount_plot <- renderPlotly({
      ggplotly(discount_plot) %>%
        layout(showlegend = TRUE)
    })
    
    output$category_plot <- renderPlotly({
      ggplotly(category_plot) %>%
        layout(showlegend = TRUE)
    })
    
    # Time Analysis tab outputs
    output$weekday_plot <- renderPlotly({
      ggplotly(weekday_plot) %>%
        layout(showlegend = TRUE)
    })
    
    output$month_plot <- renderPlotly({
      ggplotly(month_plot) %>%
        layout(showlegend = TRUE)
    })
  }
  
  # Run the application
  shinyApp(ui, server)
}