# Load required libraries
library(data.table)
library(lubridate)

# Set seed for reproducibility
set.seed(123)

# Function to generate sample SMS marketing data
generate_sms_data <- function(n_records = 1000) {
  # Define possible values
  companies <- c("TechGiant", "FashionHub", "FoodExpress", "SportMaster", 
                 "ElectroWorld", "StyleCo", "QuickBite", "FitGear")
  
  categories <- c("Electronics", "Fashion", "Food", "Sports")
  
  # Create offer templates
  offer_templates <- c(
    "get {discount}% off on your next purchase",
    "flash sale: {discount}% discount today only",
    "buy one get one free offer",
    "special discount: save up to {discount}%",
    "exclusive member discount: {discount}% off",
    "clearance sale: extra {discount}% off",
    "seasonal sale: {discount}% off selected items",
    "weekend special: save {discount}%"
  )
  
  # Generate random data
  dt <- data.table(
    date = sample(seq(as.Date('2024-01-01'), as.Date('2024-03-31'), by = "day"), 
                  n_records, replace = TRUE),
    company = sample(companies, n_records, replace = TRUE),
    product_category = NA_character_,
    discount_percentage = sample(10:70, n_records, replace = TRUE),
    message_template = sample(offer_templates, n_records, replace = TRUE)
  )
  
  # Assign categories based on company
  dt[company %in% c("TechGiant", "ElectroWorld"), product_category := "Electronics"]
  dt[company %in% c("FashionHub", "StyleCo"), product_category := "Fashion"]
  dt[company %in% c("FoodExpress", "QuickBite"), product_category := "Food"]
  dt[company %in% c("SportMaster", "FitGear"), product_category := "Sports"]
  
  # Generate final messages
  dt[, message := gsub("\\{discount\\}", discount_percentage, message_template)]
  
  # Add response rate (synthetic engagement metric)
  dt[, response_rate := runif(n_records) * 
       (1 + (discount_percentage/100)) * # Higher discounts get better response
       (1 + ifelse(wday(date) %in% c(1,7), 0.2, 0)) * # Weekend boost
       (1 + ifelse(month(date) == 1, 0.1, 0))] # January boost
  
  # Remove template column and reorder
  dt[, message_template := NULL]
  setcolorder(dt, c("date", "company", "product_category", "message", 
                    "discount_percentage", "response_rate"))
  
  return(dt)
}

# Generate sample data
sms_data <- generate_sms_data(1000)

# Save to CSV
fwrite(sms_data, "D:/VEDANT/projects/SMSAnalyser/mod/sms_marketing_data.csv")