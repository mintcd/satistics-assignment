# Count of NA values
na_counts <- colSums(is.na(cpu_data))

# Count of empty strings
empty_counts <- colSums(cpu_data == "")



# Create a data frame for properties with NA values
invalid_df <- 
  data.frame(Property = names(na_counts), 
             NA_Count = unname(na_counts),
             Empty_Count = unname(empty_counts[]))

kable(invalid_df, format = "html") %>%
  kable_styling()