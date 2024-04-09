# Reusable functions

extract_tier_and_generation <- function(product_collection) {
  # Splitting the product_collection string by spaces
  product_info <- strsplit(product_collection, " ")[[1]]
  # Finding the tier (i3 to i9)
  tier <- grep("^i[0-9]$", product_info, value = TRUE)
  # Finding the generation
  generation <- grep("^[0-9]+(st|nd|rd|th)$", product_info, value = TRUE)
  # Return a named list with tier and generation
  result <- list(tier = tier, generation = generation)
  return(result)
}

is_valid <- function(value) {
  value <- tolower(value)
  return(!is.na(value)
         & !is.null(value)
         & !value == ""
         & !value == "N/A"
         & !trimws(value) == "-"
         & !grepl("missing", value)
         & !grepl("unknown", value))
  # Add your criteria
}

filtered_data <- function(data, valid_percentage=0.8) {
  selected_columns <- character(0) 
  
  for (col in colnames(data)) { 
    valid_count <- sum(is_valid(data[[col]])) 
    total_instances <- length(data[[col]]) 
    
    if ((valid_count / total_instances) >= valid_percentage) {
      selected_columns <- c(selected_columns, col)
    }
  }
  
  return(data[selected_columns])
}

month_to_quarter <- function(month) {
  quarter <- switch(month,
                    "Jan" = "1",
                    "Feb" = "1",
                    "Mar" = "1",
                    "Apr" = "2",
                    "May" = "2",
                    "Jun" = "2",
                    "Jul" = "3",
                    "Aug" = "3",
                    "Sep" = "3",
                    "Oct" = "4",
                    "Nov" = "4",
                    "Dec" = "4",
                    "Unknown")
  return(quarter)
}