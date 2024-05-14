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

select_data <- function(data, valid_thres=0.8) {
  selected_columns <- character(0) 
  
  for (col in colnames(data)) { 
    valid_count <- sum(is_valid(data[[col]])) 
    total_instances <- length(data[[col]]) 
    
    if ((valid_count / total_instances) >= valid_thres) {
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

inspect_valid = function(data) {
  # Count NA and empty strings
  na_counts <- colSums(sapply(data, 
                              function(x) 
                                is.na(x) | x == "N/A"))
  empty_counts <- colSums(sapply(data, 
                                 function(x) 
                                   is.character(x) & 
                                   (x == "" | x == "-")))
  # Get percentage of invalid values
  valid_percentage <- round(100 - (na_counts+empty_counts)/nrow(data)*100,1)
  
  # Display
  invalid_df <- 
    data.frame(Property = names(na_counts), 
               NA_Count = unname(na_counts),
               Empty_Count = unname(empty_counts),
               Valid_Percentage = unname(valid_percentage)) %>%
    arrange(desc(Valid_Percentage))
  
  kable(invalid_df, format = "html") %>% 
    kable_styling()
}

get_valid = function(data) {
  return (data[
    apply(data, 1, 
          function(row) 
            all(sapply(row, is_valid))), ])
}