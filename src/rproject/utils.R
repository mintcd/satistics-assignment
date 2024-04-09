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
  return(!is.na(value)
         & !is.null(value)
         & !value == ""
         & !value == "N/A"
         & !value == "-"
         & !value == "missing"
         & !value == "unknown")
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