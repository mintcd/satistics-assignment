library(readxl)
library(stringr)
library(tidyr)
library(dplyr)
library(zoo)
library(Metrics)
library(caret)
library(MASS)
library(ggplot2)
library(reshape2)
library(mltools)
library(DescTools)
library(plotly)


setwd("D:/School++/XSTK")

Intel_CPUs = read.csv("Intel_CPUs.csv",na.strings = c("", "N/A"))

#print(nrow(Intel_CPUs))
#print(apply(is.na(Intel_CPUs),2,sum) )
#

sum_non_na <- apply(is.na(Intel_CPUs), 2, sum)
column_names <- names(sum_non_na)
na_data <- data.frame(Column = column_names, Sum_NA = sum_non_na)

# Create a horizontal box barplot with sorted data
na_data %>%
  arrange(Sum_NA) %>%    # First sort by val. This sort the dataframe but NOT the factor levels
  mutate(Column=factor(Column, levels=Column)) %>%   # This trick update the factor levels
  ggplot( aes(x=Column, y=Sum_NA)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  theme_bw() +
  xlab("") +
  ylab("NA values") +
  labs(title = "Sum of NA values")


# CLEANING
Intel_clean = Intel_CPUs[,c("Product_Collection","Vertical_Segment","Processor_Number","Status","Launch_Date",
                            "Lithography","Recommended_Customer_Price","nb_of_Cores","nb_of_Threads",
                            "Processor_Base_Frequency","Cache","Instruction_Set","TDP","Max_Memory_Size",
                            "Memory_Types","Max_nb_of_Memory_Channels","Max_Memory_Bandwidth")]

#View(Intel_clean)
print(apply(is.na(Intel_clean),2,sum) )

# Launch_Date là chính
#Intel_clean <- Intel_clean[complete.cases(Intel_clean$Launch_Date),] 

Intel_clean <- separate(Intel_clean,Launch_Date,into = c("Launch_Quarter","Launch_Year"),sep="'") 
Intel_clean$Launch_Quarter <- as.integer(sub("Q", "", Intel_clean$Launch_Quarter))
Intel_clean$Launch_Year <- as.integer(Intel_clean$Launch_Year)
Intel_clean$Launch_Year <- ifelse(Intel_clean$Launch_Year>60, 1900 + Intel_clean$Launch_Year, 2000 + Intel_clean$Launch_Year)

# MEMORY là chính 
to_GB <-function(a){
  if (grepl(' TB',a)) {
    return (as.double(gsub(" TB","",a))*1024)
  }
  return (as.double(gsub(" GB","",a)))
}

Intel_clean$Max_Memory_Size <- sapply(Intel_clean$Max_Memory_Size,to_GB)
# fill Max_Memory_Size
Intel_clean$Memory_Types[is.na(Intel_clean$Memory_Types)] <- "MISSINGNAME"
for( i in unique(Intel_clean$Memory_Types) ){
  if (i == "MISSINGNAME") {
    next
  }
  subset <- Intel_clean[Intel_clean$Memory_Types==i,'Max_Memory_Size']
  fill_value <- median(subset, na.rm = TRUE)
  Intel_clean[Intel_clean$Memory_Types==i,'Max_Memory_Size'] = na.fill(subset,fill_value)
}

Intel_clean <- Intel_clean[complete.cases(Intel_clean$Max_Memory_Size),] 

# fill Max_nb_of_Memory_Channels
for( i in unique(Intel_clean$Memory_Types) ){
  if (i == "MISSINGNAME") {
    next
  }
  fill_value <- median(Intel_clean[which(Intel_clean$Memory_Types == i), 'Max_nb_of_Memory_Channels'], na.rm = TRUE)
  subset <- Intel_clean[Intel_clean$Memory_Types==i,'Max_nb_of_Memory_Channels']
  Intel_clean[Intel_clean$Memory_Types==i,'Max_nb_of_Memory_Channels'] = na.fill(subset,fill_value) 
}

# fill Max_Memory_Bandwidth
Intel_clean$Max_Memory_Bandwidth <-as.double(sub(" GB/s", "", Intel_clean$Max_Memory_Bandwidth))

for( i in unique(Intel_clean$Max_nb_of_Memory_Channels) ){
  fill_value =  median(Intel_clean[Intel_clean$Max_nb_of_Memory_Channels==i,'Max_Memory_Bandwidth'],na.rm = TRUE)
  subset <- Intel_clean[Intel_clean$Max_nb_of_Memory_Channels==i,'Max_Memory_Bandwidth']
  Intel_clean[Intel_clean$Max_nb_of_Memory_Channels==i,'Max_Memory_Bandwidth'] = na.fill(subset,fill_value) 
}
#print(apply(is.na(Intel_clean),2,sum) )

Intel_clean <- Intel_clean[complete.cases(Intel_clean$Launch_Year),]

# Product_Collection
PROD_COLL_type <- c('Legacy', 'Celeron', 'Pentium', 'Quark', 'Atom', 'Itanium', 'Xeon','Core')
for (i in PROD_COLL_type) {
  Intel_clean$Product_Collection <- ifelse(grepl(i, Intel_clean$Product_Collection), i, Intel_clean$Product_Collection)
}

# 6 Lithography
Intel_clean <- Intel_clean[order(Intel_clean$Launch_Year,Intel_clean$Launch_Quarter,Intel_clean$Product_Collection,Intel_clean$Vertical_Segment),]
Intel_clean$Lithography<- na.locf(Intel_clean$Lithography)
Intel_clean$Lithography <-as.double(sub("nm", "", Intel_clean$Lithography))

# Recommended price
recommend_price <- function(price_range) {
  if(grepl('-', price_range)) {
    range <- strsplit(price_range, "-")[[1]]
    return((as.double(range[1]) + as.double(range[2])) / 2)
  }
  return (price_range)
}

Intel_clean$Recommended_Customer_Price <- gsub("\\$", "", Intel_clean$Recommended_Customer_Price)
Intel_clean$Recommended_Customer_Price <- gsub(",", "", Intel_clean$Recommended_Customer_Price)
Intel_clean$Recommended_Customer_Price <- sapply(Intel_clean$Recommended_Customer_Price, recommend_price)
Intel_clean$Recommended_Customer_Price <- as.double(Intel_clean$Recommended_Customer_Price) 
#Intel_clean$Recommended_Customer_Price <- log(Intel_clean$Recommended_Customer_Price) 
Intel_clean <- Intel_clean %>%
  group_by(Product_Collection) %>%
  fill(Recommended_Customer_Price, .direction = "updown")


# 8 nb_thread = nb_core x 2
Intel_clean$nb_of_Threads <- ifelse(is.na(Intel_clean$nb_of_Threads), Intel_clean$nb_of_Cores*2, Intel_clean$nb_of_Threads)

# Processor_Base_Frequency
to_MHz <-function(a){
  if (grepl(' GHz',a)) {
    return (as.double(gsub(" GHz","",a))*1000)
  }
  return (as.double(gsub(" MHz","",a)))
}

Intel_clean$Processor_Base_Frequency <-as.integer( sapply(Intel_clean$Processor_Base_Frequency,to_MHz))
subset <- Intel_clean[Intel_clean$Vertical_Segment == "Mobile", "Processor_Base_Frequency"]
Intel_clean[Intel_clean$Vertical_Segment == "Mobile", "Processor_Base_Frequency"] <- na.locf(subset)  
# 11 Cache
to_KB <-function(a){
  if (grepl(' M',a)) {
    return (as.double(gsub(" M","",a))*1024)
  }
  return (as.double(gsub(" K","",a)))
}

Intel_clean <- separate(Intel_clean,Cache,into = c("Cache_Size","Cache_Type"),sep="B")
Intel_clean$Cache_Size <- sapply(Intel_clean$Cache_Size,to_KB)
#Intel_clean$Cache_Size <- log(Intel_clean$Cache_Size)
Intel_clean$Cache_Type <- ifelse(Intel_clean$Cache_Type == "", "Standard", sub(" ","",Intel_clean$Cache_Type))
# 13 TDP
Intel_clean$TDP <-as.double(sub(" W", "", Intel_clean$TDP))
Intel_clean[Intel_clean$Vertical_Segment == "Mobile", "TDP"] <- na.locf(Intel_clean[Intel_clean$Vertical_Segment == "Mobile", "TDP"])


# 35 Instruction_Set
Intel_clean$Instruction_Set <- na.fill(Intel_clean$Instruction_Set,"64-bit")

print(apply(is.na(Intel_clean),2,sum) )
print(str(Intel_clean))
#print(str(Intel_clean) )
#View(Intel_clean)
#write.csv(Intel_clean, file = "Intel_clean.csv", row.names = TRUE)


summary(Intel_clean)

#Histogram
plot_data <-function(data, feature_name, binVal){
  ggplot(data = Intel_clean, aes(x = data)) +
    geom_histogram(color = "black", fill = "skyblue", binwidth = binVal) +
    labs(title = paste("Histogram for" , feature_name), x = feature_name, y = "Frequency") +
    geom_vline(aes(xintercept = mean(data)),color = "red",lwd = 0.75, lty = "dashed") +
    geom_vline(aes(xintercept = median(data)),color = "green",lwd = 1, linetype = "dashed") +
    geom_vline(aes(xintercept = Mode(data)),color = "blue",lwd = 0.5, linetype = "dashed")
}

plot_data(Intel_clean$Max_Memory_Size, "Max Memory Size", 100)
plot_data(Intel_clean$Max_nb_of_Memory_Channels, "Max number of Memory Channels", 1)
plot_data(Intel_clean$Max_Memory_Bandwidth, "Max Memory Bandwidth", 10)
plot_data(Intel_clean$TDP, "TDP", 5)
plot_data(Intel_clean$Cache_Size, "Cache Size", 1000)

plot_data(Intel_clean$Processor_Base_Frequency, "Processor Base Frequency", 100)
plot_data(Intel_clean$nb_of_Threads, "Number of Threads",5)
plot_data(Intel_clean$nb_of_Cores, "Number of Cores", 1)
plot_data(Intel_clean$Lithography, "Lithography", 1)
plot_data(Intel_clean$Launch_Year, "Launch Year", 1)

#


numeric_Intel_clean <- Intel_clean[, sapply(Intel_clean, is.numeric)]
correlation_matrix <- cor(numeric_Intel_clean)
melted_corr_matrix <- melt(correlation_matrix)

ggplot(data = melted_corr_matrix, aes(x = Var1, y = Var2)) +
  geom_tile(aes(fill = value)) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1),
                       space = "Lab", name="Correlation") +
  geom_text(aes(label = round(value, 2)), color = "black") + # Add text annotations
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Correlation Heatmap of Numeric Columns")

# LIne plot of litho/year
Litho_year <- Intel_clean %>% 
  group_by(Launch_Year) %>%
  summarize(mean_litho = mean(Lithography),
            median_litho = median(Lithography),
    .groups = "drop"
  )
options(repr.plot.width = 15, repr.plot.height =8)
ggplot(Litho_year, aes(x = Launch_Year)) +
  geom_line(aes(y = mean_litho, color = "Mean")) +
  geom_line(aes(y = median_litho, color = "Median")) +
  scale_color_manual(values = c("Mean" = "blue", "Median" = "red")) +
  labs(x = "Year", y = "Lithography", title = "Mean and Median Lithography by Year") +
  scale_x_continuous(breaks = seq(min(Litho_year$Launch_Year), max(Litho_year$Launch_Year), by = 1)) +
  theme_minimal()

# box plot of status/year
options(repr.plot.width = 15, repr.plot.height =8) 
ggplot(data = Intel_clean, aes(y = Status, x = Launch_Year, fill = Status)) +
  geom_boxplot() +
  labs(x = "Year", y = "Status",title = "Boxplot of Status over Year") +
  scale_x_continuous(breaks = seq(min(Litho_year$Launch_Year), max(Litho_year$Launch_Year), by = 1))
#Vertical Segement/Year
options(repr.plot.width = 15, repr.plot.height =8) 
ggplot(data = Intel_clean, aes(y = Vertical_Segment, x = Launch_Year, fill = Vertical_Segment)) +
  geom_boxplot() +
  labs(x = "Year", y = "Vertical Segment",title = "Boxplot of Vertical Segment over Year") +
  scale_x_continuous(breaks = seq(min(Litho_year$Launch_Year), max(Litho_year$Launch_Year), by = 1))

options(repr.plot.width = 15, repr.plot.height =8) 
ggplot(data = Intel_clean, aes(y = Instruction_Set, x = Launch_Year, fill = Instruction_Set)) +
  geom_violin() +
  labs(x = "Launch Date", y = "Instruction set",title = "Violinplot of Instruction set over Year") +
  scale_x_continuous(breaks = seq(min(Litho_year$Launch_Year), max(Litho_year$Launch_Year), by = 1))

options(repr.plot.width = 10, repr.plot.height =10) 
ggplot(Intel_clean, aes(x = nb_of_Threads, y = nb_of_Cores)) +
  geom_point() +
  geom_smooth(method = "lm")+
  labs(x="number of Threads",y="number of Cores",title = "Analysis of the Relationship between Number of Threads and Number of Cores")

ggplot(Intel_clean,aes(Recommended_Customer_Price))+
  geom_histogram(aes(y=after_stat(density)))+
  geom_density() +
  labs(title = "Histogram of Recommended Customer Price")

options(repr.plot.width = 10, repr.plot.height =10) 
ggplot(Intel_clean,aes(log(Recommended_Customer_Price)))+
  geom_histogram(aes(y=after_stat(density)))+
  geom_density() +
  labs(title = "Histogram of Log of Recommended Customer Price")


#  ------------------------------------THIS IS GPU PART------------------------------------

All_GPUs <- read.csv("All_GPUs.csv",na.strings = c("", "N/A","Unknown Release Date","\n- "))
summary(All_GPUs)
#print(apply(is.na(All_GPUs),2,sum))
#View(All_GPUs)

sum_non_na <- apply(is.na(All_GPUs), 2, sum)
column_names <- names(sum_non_na)
na_data <- data.frame(Column = column_names, Sum_NA = sum_non_na)

# Create a horizontal box barplot with sorted data
na_data %>%
  arrange(Sum_NA) %>%    # First sort by val. This sort the dataframe but NOT the factor levels
  mutate(Column=factor(Column, levels=Column)) %>%   # This trick update the factor levels
  ggplot( aes(x=Column, y=Sum_NA)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  theme_bw() +
  xlab("") +
  ylab("NA values") +
  labs(title = "Sum of NA values")

key_columns <- c("Architecture","Best_Resolution", "Core_Speed", "Manufacturer", "Memory", "Memory_Bandwidth","Name", "Release_Date")
clean_GPUs = All_GPUs[,key_columns]

print(apply(is.na(clean_GPUs),2,sum))
#View(clean_GPUs)


# Release date
clean_GPUs <- clean_GPUs[!grepl("Unknown Release Date", clean_GPUs$Release_Date), ]
clean_GPUs <- separate(clean_GPUs,Release_Date,into = c("Release_Day","Release_Month","Release_Year"),sep="-")
clean_GPUs$Release_Year <- as.integer((clean_GPUs$Release_Year))
month_names <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
clean_GPUs$Release_Month <- match(clean_GPUs$Release_Month, month_names)
clean_GPUs$Release_Day <- as.integer((clean_GPUs$Release_Day))

year_counts <- table(clean_GPUs$Release_Year)
months_counts <- table(clean_GPUs$Release_Month)
#View(year_counts)

# Manufacturers

freq <- table(clean_GPUs$Manufacturer, clean_GPUs$Release_Year)
total_count <- colSums(freq)
percentage <- prop.table(freq, margin = 2) * 100


# Memory
clean_GPUs$Memory <- as.integer(gsub(" MB","",clean_GPUs$Memory))
clean_GPUs$Memory[is.na(clean_GPUs$Memory)] <- 0

to_GBs <-function(a){
  if (grepl('GB/s',a)) {
    return (as.double(gsub("GB/sec","",a)))
  }
  return (as.double(gsub("MB/sec","",a))/1024)
}
clean_GPUs$Memory_Bandwidth <- sapply(clean_GPUs$Memory_Bandwidth,to_GBs)
clean_GPUs$Memory_Bandwidth[is.na(clean_GPUs$Memory_Bandwidth)] <- 0
# Core speed
clean_GPUs$Core_Speed <- as.integer(gsub("MHz", "", clean_GPUs$Core_Speed))
clean_GPUs$Core_Speed <- na.aggregate(clean_GPUs$Core_Speed, FUN = median)
# Best_Resolution
clean_GPUs <- na.locf(clean_GPUs)


print(apply(is.na(clean_GPUs),2,sum) )
print(str(clean_GPUs))
summary(clean_GPUs)

memory_summary <- clean_GPUs %>%
  group_by(Release_Year) %>%
  summarise(mean_memory = mean(Memory),
            median_memory = median(Memory))


freq <- table(clean_GPUs$Manufacturer, clean_GPUs$Release_Year)
total_count <- colSums(freq)
percentage <- prop.table(freq, margin = 2) * 100

numeric_clean_GPUs <- clean_GPUs[, sapply(clean_GPUs, is.numeric)]
correlation_matrix <- cor(numeric_clean_GPUs)
melted_corr_matrix <- melt(correlation_matrix)

ggplot(data = melted_corr_matrix, aes(x = Var1, y = Var2)) +
  geom_tile(aes(fill = value)) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1),
                       space = "Lab", name="Correlation") +
  geom_text(aes(label = round(value, 2)), color = "black") + # Add text annotations
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Correlation Heatmap of Numeric Columns")

# HISTOGRAM
plot_data <-function(data, feature_name, binVal){
  ggplot(data = clean_GPUs, aes(x = data)) +
    geom_histogram(color = "black", fill = "skyblue", binwidth = binVal) +
    labs(title = paste("Histogram for" , feature_name), x = feature_name, y = "Frequency") +
    geom_vline(aes(xintercept = mean(data)),color = "red",lwd = 0.75, lty = "dashed") +
    geom_vline(aes(xintercept = median(data)),color = "green",lwd = 1, linetype = "dashed") +
    geom_vline(aes(xintercept = Mode(data)),color = "blue",lwd = 0.5, linetype = "dashed")
}

plot_data(clean_GPUs$Release_Year, "Release Year", 1)
plot_data(clean_GPUs$Memory_Bandwidth, "Memory Bandwidth", 50)
plot_data(clean_GPUs$Memory, "Memory",500)
plot_data(clean_GPUs$Core_Speed, "Core Speed", 50)

#

barplot(freq,
        legend.text = TRUE,
        main = "Number of GPUs by Year",
        xlab = "Year",
        ylab = "Number of GPUs",
        col = c("skyblue", "salmon", "lightgreen", "yellow"),
        border = "black")

barplot(percentage,
        legend.text = TRUE,
        main = "Market Share Percentage by Year",
        xlab = "Year",
        ylab = "Percentage",
        col = c("skyblue", "salmon", "lightgreen", "yellow"),
        border = "black")

scatter_plot <- ggplot(clean_GPUs, aes(x = Release_Year + Release_Month/12, y = Memory, color = Manufacturer)) +
  geom_point() +
  scale_color_manual(values = c("skyblue", "salmon", "lightgreen", "yellow")) +
  labs(x = "Year", y = "GPU Memory", title = "Scatter Plot of GPU Memory Size vs Year") +
  theme_minimal()
print(scatter_plot)

line_plot <- ggplot(memory_summary, aes(x = Release_Year)) +
  geom_line(aes(y = mean_memory, color = "Mean")) +
  geom_line(aes(y = median_memory, color = "Median")) +
  scale_color_manual(values = c("Mean" = "blue", "Median" = "red")) +
  labs(x = "Year", y = "Memory", title = "Mean and Median Memory Size by Year") +
  theme_minimal()
print(line_plot)

moore_law <- data.frame(
  Release_Year = seq(min(memory_summary$Release_Year), max(memory_summary$Release_Year), 1),
  Memory = 2^(0.5 * (seq(min(memory_summary$Release_Year), max(memory_summary$Release_Year), 1) - min(memory_summary$Release_Year - 8)))
)

memory_summary$log_mean_memory <- log(memory_summary$mean_memory)
memory_summary$log_median_memory <- log(memory_summary$median_memory)

line_plot <- ggplot(memory_summary, aes(x = Release_Year)) +
  geom_line(aes(y = log_mean_memory, color = "Logarithm of Mean")) +
  geom_line(aes(y = log_median_memory, color = "Logarithm of Median")) +
  geom_line(data = moore_law, aes(y = log(Memory), color = "Moore's Law"), linetype = "dashed") +
  scale_color_manual(values = c("Logarithm of Mean" = "blue", "Logarithm of Median" = "red", "Moore's Law" = "green4")) +
  labs(x = "Year", y = "Logarithm of Memory", title = "Logarithm of Mean and Median Memory Size by Year") +
  theme_minimal()

print(line_plot)
