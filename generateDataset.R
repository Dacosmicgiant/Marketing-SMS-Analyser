library(stringr)

# Define companies, product categories, and message templates
companies <- c("Apple", "Samsung", "H&M", "Zara", "Domino's", "KFC", "Xiaomi", "Nike", "Adidas", "Levi's")
product_categories <- c("Electronics", "Fashion", "Food", "Sports")
message_templates <- c("Get {discount}% off on all {product}", 
                       "Buy one get one free on {product}", 
                       "Discounts on all {product}", 
                       "Free shipping on all {product}", 
                       "Trade-in offer on all {product}", 
                       "Limited time offer on {product}")

# Generate large dataset
set.seed(123)
sms_data <- data.frame(id = 1:5000, 
                       company = sample(companies, 5000, replace = TRUE), 
                       product_category = sample(product_categories, 5000, replace = TRUE), 
                       message = sapply(1:5000, function(i) {
                         company <- sample(companies, 1)
                         product_category <- sample(product_categories, 1)
                         message_template <- sample(message_templates, 1)
                         product <- paste0(product_category, "s")
                         discount <- sample(c(10, 20, 30, 40, 50), 1)
                         str_replace(message_template, 
                                     c("\\{discount\\}", "\\{product\\}"), 
                                     c(discount, product))
                       }))

# Write to CSV file
write.csv(sms_data, "D:/VEDANT/projects/SMSAnalyser/sms_data.csv", row.names = FALSE)