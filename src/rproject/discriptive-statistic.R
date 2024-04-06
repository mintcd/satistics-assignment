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

Intel_clean = Intel_CPUs[,c("Product_Collection","Vertical_Segment","Processor_Number","Status","Launch_Date",
                            "Lithography","Recommended_Customer_Price","nb_of_Cores","nb_of_Threads",
                            "Processor_Base_Frequency","Cache","Instruction_Set","TDP","Max_Memory_Size",
                            "Memory_Types","Max_nb_of_Memory_Channels","Max_Memory_Bandwidth")]
print(apply(is.na(Intel_clean),2,sum) )
#Intel_clean = Intel_CPUs
#Intel_clean <- Intel_clean[, !(names(Intel_clean) %in% c("Processor_Graphics_","Support_4k","OpenGL_Support"))]

# CLEANING

# Product_Collection
PROD_COLL_type <- c('Legacy', 'Celeron', 'Pentium', 'Quark', 'Atom', 'Itanium', 'Xeon','Core')
for (i in PROD_COLL_type) {
  Intel_clean$Product_Collection <- ifelse(grepl(i, Intel_clean$Product_Collection), i, Intel_clean$Product_Collection)
}

# MEMORY STUFF

to_GB <-function(a){
  if (grepl(' TB',a)) {
    return (as.double(gsub(" TB","",a))*1024)
  }
  return (as.double(gsub(" GB","",a)))
}

Intel_clean$Max_Memory_Size <- sapply(Intel_clean$Max_Memory_Size,to_GB)

Intel_clean$Memory_Types[is.na(Intel_clean$Memory_Types)] <- -1
for( i in unique(Intel_clean$Memory_Types) ){
  if (i == -1) {
    next
  }
  fill_value <- median(Intel_clean[which(Intel_clean$Memory_Types == i), 'Max_Memory_Size'], na.rm = TRUE)
  subset <- Intel_clean[Intel_clean$Memory_Types==i,'Max_Memory_Size']
  Intel_clean[Intel_clean$Memory_Types==i,'Max_Memory_Size'] = na.fill(subset,fill_value) 
}
Intel_clean$Memory_Types[Intel_clean$Memory_Types == -1] <- NA


Intel_clean$Max_Memory_Bandwidth <-as.double(sub(" GB/s", "", Intel_clean$Max_Memory_Bandwidth))
Intel_clean$Max_nb_of_Memory_Channels[is.na(Intel_clean$Max_nb_of_Memory_Channels)] <- -1
for( i in unique(Intel_clean$Max_nb_of_Memory_Channels) ){
  if (i == -1) {
    next
  }
  fill_value =  median(Intel_clean[Intel_clean$Max_nb_of_Memory_Channels==i,'Max_Memory_Bandwidth'],na.rm = TRUE)
  subset <- Intel_clean[Intel_clean$Max_nb_of_Memory_Channels==i,'Max_Memory_Bandwidth']
  Intel_clean[Intel_clean$Max_nb_of_Memory_Channels==i,'Max_Memory_Bandwidth'] = na.fill(subset,fill_value) 
}
Intel_clean$Max_nb_of_Memory_Channels[Intel_clean$Max_nb_of_Memory_Channels == -1] <- NA

nrow(Intel_clean[is.na(Intel_clean$Max_Memory_Size) & is.na(Intel_clean$Launch_Date),])

print(apply(is.na(Intel_clean),2,sum) )
# 5 Launch_Date 
Intel_clean <- Intel_clean[complete.cases(Intel_clean$Launch_Date), ] 

Intel_clean <- separate(Intel_clean,Launch_Date,into = c("Launch_Quarter","Launch_Year"),sep="'") 
Intel_clean$Launch_Quarter <- as.integer(sub("Q", "", Intel_clean$Launch_Quarter))
Intel_clean$Launch_Year <- as.integer(Intel_clean$Launch_Year)
Intel_clean$Launch_Year <- ifelse(Intel_clean$Launch_Year>60, 1900 + Intel_clean$Launch_Year, 2000 + Intel_clean$Launch_Year)
Intel_clean <- Intel_clean[order(Intel_clean$Launch_Year,Intel_clean$Launch_Quarter,Intel_clean$Product_Collection,Intel_clean$Vertical_Segment), ]



# 6 Lithography
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
Intel_clean$Recommended_Customer_Price <- log(Intel_clean$Recommended_Customer_Price) 
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
# 10 Max_Turbo_Frequency
#Intel_clean$Max_Turbo_Frequency <-as.integer( sapply(Intel_clean$Max_Turbo_Frequency,to_MHz))
# 11 Cache
to_KB <-function(a){
  if (grepl(' M',a)) {
    return (as.double(gsub(" M","",a))*1024)
  }
  return (as.double(gsub(" K","",a)))
}

Intel_clean <- separate(Intel_clean,Cache,into = c("Cache_Size","Cache_Type"),sep="B")
Intel_clean$Cache_Size <- sapply(Intel_clean$Cache_Size,to_KB)
Intel_clean$Cache_Size <- log(Intel_clean$Cache_Size)
Intel_clean$Cache_Type <- ifelse(Intel_clean$Cache_Type == "", "Standard", sub(" ","",Intel_clean$Cache_Type))
# 13 TDP
Intel_clean$TDP <-as.double(sub(" W", "", Intel_clean$TDP))
Intel_clean[Intel_clean$Vertical_Segment == "Mobile", "TDP"] <- na.locf(Intel_clean[Intel_clean$Vertical_Segment == "Mobile", "TDP"])


# 35 Instruction_Set
Intel_clean$Instruction_Set <- na.fill(Intel_clean$Instruction_Set,"64-bit")

Intel_clean <- Intel_clean[complete.cases(Intel_clean$Cache_Type), ]
Intel_clean <- Intel_clean[complete.cases(Intel_clean$TDP), ]
Intel_clean <- Intel_clean[complete.cases(Intel_clean$Memory_Types), ]
Intel_clean <- Intel_clean[complete.cases(Intel_clean$Max_nb_of_Memory_Channels), ]
Intel_clean <- Intel_clean[complete.cases(Intel_clean$Max_Memory_Size), ]


print(apply(is.na(Intel_clean),2,sum) )
print(str(Intel_clean) )
View(Intel_clean)
write.csv(Intel_clean, file = "Intel_clean.csv", row.names = TRUE)



numerical_cols = c("Launch_Year","Lithography","Recommended_Customer_Price","nb_of_Cores","nb_of_Threads",
                   "Processor_Base_Frequency","TDP","Cache_Size","Max_Memory_Size","Max_nb_of_Memory_Channels","Max_Memory_Bandwidth")
categorical_cols = c("Product_Collection","Vertical_Segment","Status","Cache_Type","Instruction_Set")
summary_numeric_table <- data.frame(
  Staticstic=c("Count", "Mean", "STD", "Min", "First Quantile", "Median", "Third Quantile", "Max")
)

for (i in numerical_cols){
  count <- length(Intel_clean[[i]])
  mean<- mean(Intel_clean[[i]])
  std <- sd(Intel_clean[[i]])
  min <- min(Intel_clean[[i]])
  first_quantile <- sapply(Intel_clean[i], function(x) quantile(x, 0.25) )[[1]]
  median <- median(Intel_clean[[i]])
  third_quantile <- sapply(Intel_clean[i], function(x) quantile(x, 0.75))[[1]]
  max <- max(Intel_clean[[i]])
  summary_numeric_table <- cbind(summary_numeric_table,new_col=c(count,mean,std,min,first_quantile,median,third_quantile,max))
}
colnames(summary_numeric_table) <- c("",numerical_cols)

summary_categorical_table <- data.frame(
  Staticstic = c("Count","Unique","Mode","Freq")
)
for (i in categorical_cols) {
  count <- length(Intel_clean[[i]])
  unique <- length( unique(Intel_clean[[i]]))
  mode <- Mode(Intel_clean[[i]])
  freq <- attr(mode,"freq")
  summary_categorical_table <- cbind(summary_categorical_table,new_col=c(count,unique,mode,freq))
}
colnames(summary_categorical_table) <- c("",categorical_cols)


summary_numeric_table

cor(Intel_clean[numerical_cols])

summary_categorical_table

Litho_year <- Intel_clean %>% 
  group_by(Launch_Year) %>%
  summarize(
    mean_thick= mean(Lithography),
    .groups = "drop"
  )
options(repr.plot.width = 10, repr.plot.height =10) 
ggplot(Litho_year, aes(Launch_Year,mean_thick)) +
  geom_line() +
  labs(x = "Launch Date", y = "Thickness", title = "Line plot of thickness of CPU over year") +
  scale_x_continuous(breaks = seq(min(Litho_year$Launch_Year), max(Litho_year$Launch_Year), by = 1))


options(repr.plot.width = 15, repr.plot.height =8) 
ggplot(data = Intel_clean, aes(y = Status, x = Launch_Year, fill = Status)) +
  geom_boxplot() +
  labs(x = "Status", y = "Launch Date",title = "Boxplot of Status over Year") +
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

options(repr.plot.width = 10, repr.plot.height =10) 
ggplot(Intel_clean, aes(Max_nb_of_Memory_Channels,Max_Memory_Bandwidth)) +
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(x="Max number of Memory Channels",y="Max Memory Bandwith",title="Relationship of Max Number of Memory Channels and Max Memory BandWidth")

options(repr.plot.width = 15, repr.plot.height =8) 
ggplot(Intel_clean,aes(Processor_Base_Frequency,Vertical_Segment,fill=Vertical_Segment)) + 
  geom_violin() +
  labs(y="Vertical Segment",x="Frequency",title="violinplot with Vertical Segment and Frequency")

options(repr.plot.width = 15, repr.plot.height =8) 
ggplot(Intel_clean,aes(Vertical_Segment,Max_Memory_Size,fill=Vertical_Segment)) + 
  geom_boxplot() +
  labs(x="Vertical Segment",y="Max Memory Size",title="boxplot with Vertical Segment and Max Memory Size")

options(repr.plot.width = 10, repr.plot.height =10) 
ggplot(Intel_clean,aes(Recommended_Customer_Price))+
  geom_histogram(aes(y=after_stat(density)))+
  geom_density() +
  labs(title = "Histogram of Recommended Customer Price")

Price_year<- Intel_clean %>%
  group_by(Launch_Year, Product_Collection) %>%
  summarize(
    mean_price = mean(Recommended_Customer_Price),
    .groups = "drop",
    lower = if (length(Recommended_Customer_Price) > 5& sd(Recommended_Customer_Price) != 0) {
      t.test(Recommended_Customer_Price, conf.level = 0.95)$conf.int[1]
    } else {
      min(Recommended_Customer_Price)
    },
    upper = if (length(Recommended_Customer_Price) > 5 & sd(Recommended_Customer_Price) != 0) {
      t.test(Recommended_Customer_Price, conf.level = 0.95)$conf.int[2]
    } else {
      max(Recommended_Customer_Price)
    }
  )

options(repr.plot.width = 18, repr.plot.height =10) 
ggplot(Price_year, aes(Launch_Year, mean_price,colour = Product_Collection,fill=Product_Collection)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.4) +
  labs(x="Launch Date",y="Price",title="Lineplot of Recommended Customer Price of Product Collection over Year")+
  scale_x_continuous(breaks = seq(min(Litho_year$Launch_Year), max(Litho_year$Launch_Year), by = 1))

options(repr.plot.width = 15, repr.plot.height =8) 
ggplot(Intel_clean,aes(TDP,Recommended_Customer_Price,colour=Vertical_Segment))+
  geom_point() +
  labs(y="Price",title="Scatterplot of Price and TDP with Vertical Segment")

options(repr.plot.width = 15, repr.plot.height =8) 
ggplot(Intel_clean,aes(Recommended_Customer_Price,Cache_Size,colour=Cache_Type)) +
  geom_point() +
  labs(x="Price",y="Cache Size",title="Scatterplot of Price and Cache Size with Cache Type")

#  THIS IS GPU PART

All_GPUs = read.csv("All_GPUs.csv",na.strings = c("", "N/A","Unknown Release Date"))
summary(All_GPUs)
print(apply(is.na(All_GPUs),2,sum))
View(All_GPUs)
barplot(apply(!is.na(All_GPUs),2,sum))

key_columns <- c("Best_Resolution", "Core_Speed", "Manufacturer", "Memory", "Memory_Bandwidth","Name", "Release_Date")
clean_GPUs = All_GPUs[,key_columns]
print(apply(is.na(clean_GPUs),2,sum))
View(clean_GPUs)


# Release date
clean_GPUs <- clean_GPUs[!grepl("Unknown Release Date", clean_GPUs$Release_Date), ]
clean_GPUs <- separate(clean_GPUs,Release_Date,into = c("Release_Day","Release_Month","Release_Year"),sep="-")
clean_GPUs$Release_Year <- as.integer((clean_GPUs$Release_Year))
month_names <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
clean_GPUs$Release_Month <- match(clean_GPUs$Release_Month, month_names)
clean_GPUs$Release_Day <- as.integer((clean_GPUs$Release_Day))

year_counts <- table(clean_GPUs$Release_Year)
months_counts <- table(clean_GPUs$Release_Month)
View(year_counts)

# Manufacturers

freq <- table(clean_GPUs$Manufacturer, clean_GPUs$Release_Year)
total_count <- colSums(freq)
percentage <- prop.table(freq, margin = 2) * 100
View(total_count)
View(percentage)

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

memory_summary <- clean_GPUs %>%
  group_by(Release_Year) %>%
  summarise(mean_memory = mean(Memory),
            median_memory = median(Memory))



barplot(percentage,
        legend.text = TRUE,
        main = "Market Share Percentage by Year",
        xlab = "Year",
        ylab = "Percentage",
        col = c("skyblue", "salmon", "lightgreen", "yellow", "purple", "orange", "cyan", "pink"),
        border = "black")


barplot(year_counts,
        main = "Counts of Each Year", 
        xlab = "Year", 
        ylab = "Count",
        col = "steelblue",
        border = "black")

scatter_plot <- ggplot(clean_GPUs, aes(x = Release_Year + Release_Month/12, y = Memory, color = Manufacturer)) +
  geom_point() +
  labs(x = "Year", y = "GPU Memory", title = "Scatter Plot of GPU Memory vs Year") +
  theme_minimal()
print(scatter_plot)

line_plot <- ggplot(memory_summary, aes(x = Release_Year)) +
  geom_line(aes(y = mean_memory, color = "Mean")) +
  geom_line(aes(y = median_memory, color = "Median")) +
  scale_color_manual(values = c("Mean" = "blue", "Median" = "red")) +
  labs(x = "Year", y = "Memory", title = "Mean and Median Memory by Year") +
  theme_minimal()
print(line_plot)

moore_law <- data.frame(
  Release_Year = seq(min(memory_summary$Release_Year), max(memory_summary$Release_Year), 1),
  Memory = 2^(0.5 * (seq(min(memory_summary$Release_Year), max(memory_summary$Release_Year), 1) - min(memory_summary$Release_Year - 8)))
)

memory_summary$log_mean_memory <- log(memory_summary$mean_memory)
memory_summary$log_median_memory <- log(memory_summary$median_memory)

line_plot <- ggplot(memory_summary, aes(x = Release_Year)) +
  geom_line(aes(y = log_mean_memory, color = "Log Mean"), size = 1.1) +
  geom_line(aes(y = log_median_memory, color = "Log Median"), size = 1.1) +
  geom_line(data = moore_law, aes(y = log(Memory), color = "Moore's Law"), size = 1.1) +
  scale_color_manual(values = c("Log Mean" = "blue", "Log Median" = "red", "Moore's Law" = "green4")) +
  labs(x = "Year", y = "Log Memory", title = "Log of Mean and Median Memory by Year") +
  theme_minimal()

print(line_plot)
