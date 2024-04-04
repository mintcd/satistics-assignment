library(dplyr)

cpu_data <- read.csv("dataset/Intel_CPUs.csv")
gpu_data <- read.csv("dataset/All_GPUs.csv")

source("utils.R")

result_list <- lapply(matched_products, extract_tier_and_generation)

pattern = "[0-9]+(st|nd|rd|th) Generation Intel® Core™ i[3,5,7,9] Processors"

matched_products <- grep(pattern, products, value = TRUE)


# Extracting quarter and year
quarters <- as.integer(substring(cpu_data$Launch_Date, 2, 2))
years <- as.integer(substring(cpu_data$Launch_Date, 4, 5)) + 2000

# Creating a new column to store the release year and quarter
cpu_data$Launch_Year <- years
cpu_data$Launch_Quarter <- quarters

quarter_numeric <- c("Q1" = 1, "Q2" = 2, "Q3" = 3, "Q4" = 4)
cpu_data$Launch_Quarter <- quarters

# Sorting the dataset by release year and quarter
sorted_on_release <- cpu_data[order(cpu_data$Launch_Year, cpu_data$Launch_Quarter), ]
