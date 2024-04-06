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
         & !value == "missing"
         & !value == "unknown")
  # Add your criteria
}