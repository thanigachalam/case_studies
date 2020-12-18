#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------
#Retail-Giant Sales Forecasting : Case Study - Time Series
#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------
#1. Required package
#2. Importing Dataset
#3. Data Understanding
#4. Data preparation
#5. Model building
#6. Model evaluation
#7. Recommendations
#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------
#Required R Packages
#-----------------------------------------------------------------------------------
required_packages <- c("dplyr","tidyr","lubridate","stringr","ggplot2","gridExtra","corrplot","caTools","MASS","car",
                       "caret","pROC","forecast","tseries","graphics","fpp")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if (length(new_packages)) install.packages(new_packages)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(gridExtra)
library(corrplot)
library(caTools)
library(MASS)
library(car)

library(pROC)

library(ggpubr)

library(tidyr)
library(stringr)

require(graphics)
library(forecast)

library(tseries)
#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------
# Importing datasets and creating local variables
#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------
global_superstore_data <- read.csv("Global Superstore.csv")

#Storing imported data for reference purpose this will not be changed as part of the process
global_superstore_data_raw <- global_superstore_data

#-----------------------------------------------------------------------------------
# creating derive columns date and month number
#-----------------------------------------------------------------------------------
library(tidyr)
#Order.Date
global_superstore_data$Order.Date <- as.Date(global_superstore_data$Order.Date, "%d-%m-%Y")
global_superstore_data$Order.Month <- format(global_superstore_data$Order.Date,"%m")
global_superstore_data$Order.Year <- format(global_superstore_data$Order.Date,"%Y")
global_superstore_data$Order.Month <- as.factor(global_superstore_data$Order.Month)
global_superstore_data$Order.Year <- as.factor(global_superstore_data$Order.Year)
global_superstore_data$Order.Month.Name <- month.abb[global_superstore_data$Order.Month]
global_superstore_data$Order.Month.Name <- as.factor(global_superstore_data$Order.Month.Name)
global_superstore_data$Order.Month.Year <- paste(global_superstore_data$Order.Year,"-",global_superstore_data$Order.Month)
global_superstore_data$Order.Month.Year <- as.factor(global_superstore_data$Order.Month.Year)

#converting to factor
global_superstore_data$Quantity <- as.factor(global_superstore_data$Quantity)
#Adding Market.Segment column
global_superstore_data$Market.Segment <- paste(global_superstore_data$Market,global_superstore_data$Segment,sep="-")
global_superstore_data$Market.Segment <- as.factor(global_superstore_data$Market.Segment)
# Adding Market.Category column
global_superstore_data$Market.Category <- paste(global_superstore_data$Market,"-",global_superstore_data$Category)
global_superstore_data$Market.Category <- as.factor(global_superstore_data$Market.Category)
#-------------------------------------------------------------------------------
#validating for Missing Values or NA
#-----------------------------------------------------------------------------------
global_superstore_data_na_validation <- as.data.frame(sapply(global_superstore_data, function(x) sum(is.na(x))))
names(global_superstore_data_na_validation)[1] <- 'na_count'
names <- rownames(global_superstore_data_na_validation)
rownames(global_superstore_data_na_validation) <- NULL
global_superstore_data_na_validation <- cbind(names,global_superstore_data_na_validation)
#---------------------------------------------------------------------------------
# general dataset na validation
#----------------------------------------------------------------------------------
general_data_na_validation_chart <- ggplot(data = subset(global_superstore_data_na_validation, na_count > 0), aes(x = names, y = (na_count/51290)*100)) + 
  geom_bar(stat = "identity") +
  labs(title="General Dataset NA data validation"
       , x = "Column Name" 
       , y = "%")
general_data_na_validation_chart <- general_data_na_validation_chart + geom_text(aes(label = sprintf("%.02f",(na_count/51290)*100)), vjust = -0.3)
general_data_na_validation_chart

rm(global_superstore_data_na_validation)
rm(general_data_na_validation_chart)
#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
# EDA
# a.Market_Category
#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
#Analysis on Market and Category data
Market_Category_data <- ggplot(data = global_superstore_data, aes(x = global_superstore_data$Market.Category))

#Market-Category %
Market_Category_percentage_plot <- Market_Category_data +
  geom_bar(aes(y = (..count..)/sum(..count..)),fill = "#0073C2FF") +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  labs(title="Analysis on Market and Category data %"
       , x = "Market-Category" 
       , y = "Number of Sales") + theme_pubclean() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

Market_Category_percentage_plot
#------------------------------------------------------------------------------------
Market_Market_by_Category_data <- ggplot(data = global_superstore_data, aes(x = global_superstore_data$Market, fill = global_superstore_data$Category))

#Analysis on Market by Category data 
Market_Market_by_Category_data_percentage_plot <- Market_Market_by_Category_data +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  #geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  labs(title="Analysis on Market by Category data %"
       , x = "Market" 
       , y = "Number of Sales") + theme_pubclean() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

Market_Market_by_Category_data_percentage_plot
#------------------------------------------------------------------------------
#Analysis on Market and Category data by year %
Market_Market_by_Category_data <- ggplot(data = global_superstore_data, aes(x = global_superstore_data$Market, fill = global_superstore_data$Category))
Market_Market_and_Category_data_by_year_plot <- Market_Market_by_Category_data +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  labs(title="Analysis on Market and Category data by year %"
       , x = "Market-Category" 
       , y = "Number of Sales") + theme_pubclean() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + facet_wrap(~global_superstore_data$Order.Year)
Market_Market_and_Category_data_by_year_plot
#------------------------------------------------------------------------------------
#Analysis on Market data by month and year%
Market_Year_data <- ggplot(data = global_superstore_data, aes(x = global_superstore_data$Order.Month.Year))
#Market-Category %
Market_Year_percentage_plot <- Market_Year_data +
  geom_bar(aes(y = (..count..)/sum(..count..)),fill = "#0073C2FF") +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  labs(title="Analysis on Market data by month and year%"
       , x = "Month-Year" 
       , y = "Number of Sales") + theme_pubclean() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
Market_Year_percentage_plot
#--------------------------------------------------------------------------------
Market_data <- ggplot(data = global_superstore_data, aes(x = global_superstore_data$Order.Month))
#Market-Category %
Market_percentage_plot <- Market_data +
  geom_bar(aes(y = (..count..)/sum(..count..)),fill = "#0073C2FF") +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  labs(title="Analysis on Market data by month and year%"
       , x = "Month" 
       , y = "Number of Sales") + theme_pubclean() + facet_wrap(~global_superstore_data$Order.Year)
Market_percentage_plot
#------------------------------------------------------------------------------
#Analysis on Market data by month and year%"
Market_data <- ggplot(data = global_superstore_data, aes(x = global_superstore_data$Order.Month))

#Market-Category %
Market_percentage_plot <- Market_data +
  geom_bar(aes(y = (..count..)/sum(..count..), fill = factor(global_superstore_data$Order.Year))) +
  #geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  labs(title="Analysis on Market data by month and year%"
       , x = "Month" 
       , y = "Number of Sales") + theme_pubclean() + facet_wrap(~global_superstore_data$Market.Category)

Market_percentage_plot
#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------
# Profit Analysis by Market Category : Market and Category summary data
#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------
#summary of sales
global_superstore_Sales_Summary_data <- global_superstore_data %>% 
  group_by_(.dots=c("Order.Month.Year","Market","Category")) %>% 
  summarize(x=sum(Sales))
#summary of Profit
global_superstore_Profit_Summary_data <- global_superstore_data %>% 
  group_by_(.dots=c("Order.Month.Year","Market","Category")) %>% 
  summarize(x=sum(Profit))
#summary of quantity
global_superstore_Quantity_Summary_data <- global_superstore_data %>% 
  group_by_(.dots=c("Order.Month.Year","Market","Category")) %>% 
  summarize(x=sum(as.numeric(Quantity)))
#Renaming columns
colnames(global_superstore_Sales_Summary_data)[4] <- "Total.Sales"
colnames(global_superstore_Profit_Summary_data)[4] <- "Total.Profit"
colnames(global_superstore_Quantity_Summary_data)[4] <- "Total.Quantity"
#Mergeing all the data sets of  Market Category summary
global_superstore_Summary_data <- inner_join(global_superstore_Sales_Summary_data, global_superstore_Profit_Summary_data, by = c("Order.Month.Year" = "Order.Month.Year", "Market" = "Market", "Category" = "Category"))
global_superstore_Summary_data <- inner_join(global_superstore_Summary_data, global_superstore_Quantity_Summary_data, by = c("Order.Month.Year" = "Order.Month.Year", "Market" = "Market", "Category" = "Category"))
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Data set for Market: Africa and Category: Furniture
global_superstore_africa_data <- global_superstore_Summary_data[global_superstore_Summary_data$Market == "Africa",]
global_superstore_africa_Furniture_data <- global_superstore_africa_data[global_superstore_africa_data$Category == "Furniture",]
global_superstore_africa_Furniture_data <- arrange(global_superstore_africa_Furniture_data,Category,Order.Month.Year)
global_superstore_africa_Furniture_data$Order <- rank(global_superstore_africa_Furniture_data$Order.Month.Year,ties.method= "first")
# Data set for Market: Africa and Category: Office Supplies
global_superstore_africa_Office_Supplies_data <- global_superstore_africa_data[global_superstore_africa_data$Category == "Office Supplies",]
global_superstore_africa_Office_Supplies_data <- arrange(global_superstore_africa_Office_Supplies_data,Category,Order.Month.Year)
global_superstore_africa_Office_Supplies_data$Order <- rank(global_superstore_africa_Office_Supplies_data$Order.Month.Year,ties.method= "first")
# Data set for Market: Africa and Category: Technology
global_superstore_africa_Technology_data <- global_superstore_africa_data[global_superstore_africa_data$Category == "Technology",]
global_superstore_africa_Technology_data <- arrange(global_superstore_africa_Technology_data,Category,Order.Month.Year)
global_superstore_africa_Technology_data$Order <- rank(global_superstore_africa_Technology_data$Order.Month.Year,ties.method= "first")
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Data set for Market: APAC and Category: Furniture
global_superstore_APAC_data <- global_superstore_Summary_data[global_superstore_Summary_data$Market == "APAC",]
global_superstore_APAC_Furniture_data <- global_superstore_APAC_data[global_superstore_africa_data$Category == "Furniture",]
global_superstore_APAC_Furniture_data <- arrange(global_superstore_APAC_Furniture_data,Category,Order.Month.Year)
global_superstore_APAC_Furniture_data$Order <- rank(global_superstore_APAC_Furniture_data$Order.Month.Year,ties.method= "first")
# Data set for Market: APAC and Category: Office Supplies
global_superstore_APAC_Office_Supplies_data <- global_superstore_APAC_data[global_superstore_africa_data$Category == "Office Supplies",]
global_superstore_APAC_Office_Supplies_data <- arrange(global_superstore_APAC_Office_Supplies_data,Category,Order.Month.Year)
global_superstore_APAC_Office_Supplies_data$Order <- rank(global_superstore_APAC_Office_Supplies_data$Order.Month.Year,ties.method= "first")
# Data set for Market: APAC and Category: Technology
global_superstore_APAC_Technology_data <- global_superstore_APAC_data[global_superstore_africa_data$Category == "Technology",]
global_superstore_APAC_Technology_data <- arrange(global_superstore_APAC_Technology_data,Category,Order.Month.Year)
global_superstore_APAC_Technology_data$Order <- rank(global_superstore_APAC_Technology_data$Order.Month.Year,ties.method= "first")
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Data set for Market: Canada and Category: Furniture
global_superstore_Canada_data <- global_superstore_Summary_data[global_superstore_Summary_data$Market == "Canada",]
global_superstore_Canada_Furniture_data <- global_superstore_Canada_data[global_superstore_Canada_data$Category == "Furniture",]
global_superstore_Canada_Furniture_data <- arrange(global_superstore_Canada_Furniture_data,Category,Order.Month.Year)
global_superstore_Canada_Furniture_data$Order <- rank(global_superstore_Canada_Furniture_data$Order.Month.Year,ties.method= "first")
# Data set for Market: Canada and Category: Office Supplies
global_superstore_Canada_Office_Supplies_data <- global_superstore_Canada_data[global_superstore_Canada_data$Category == "Office Supplies",]
global_superstore_Canada_Office_Supplies_data <- arrange(global_superstore_Canada_Office_Supplies_data,Category,Order.Month.Year)
global_superstore_Canada_Office_Supplies_data$Order <- rank(global_superstore_Canada_Office_Supplies_data$Order.Month.Year,ties.method= "first")
# Data set for Market: Canada and Category: Technology
global_superstore_Canada_Technology_data <- global_superstore_Canada_data[global_superstore_Canada_data$Category == "Technology",]
global_superstore_Canada_Technology_data <- arrange(global_superstore_Canada_Technology_data,Category,Order.Month.Year)
global_superstore_Canada_Technology_data$Order <- rank(global_superstore_Canada_Technology_data$Order.Month.Year,ties.method= "first")
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Data set for Market: EMEA and Category: Furniture
global_superstore_EMEA_data <- global_superstore_Summary_data[global_superstore_Summary_data$Market == "EMEA",]
global_superstore_EMEA_Furniture_data <- global_superstore_EMEA_data[global_superstore_EMEA_data$Category == "Furniture",]
global_superstore_EMEA_Furniture_data <- arrange(global_superstore_EMEA_Furniture_data,Category,Order.Month.Year)
global_superstore_EMEA_Furniture_data$Order <- rank(global_superstore_EMEA_Furniture_data$Order.Month.Year,ties.method= "first")
# Data set for Market: EMEA and Category: Office Supplies
global_superstore_EMEA_Office_Supplies_data <- global_superstore_EMEA_data[global_superstore_EMEA_data$Category == "Office Supplies",]
global_superstore_EMEA_Office_Supplies_data <- arrange(global_superstore_EMEA_Office_Supplies_data,Category,Order.Month.Year)
global_superstore_EMEA_Office_Supplies_data$Order <- rank(global_superstore_EMEA_Office_Supplies_data$Order.Month.Year,ties.method= "first")
# Data set for Market: EMEA and Category: Technology
global_superstore_EMEA_Technology_data <- global_superstore_EMEA_data[global_superstore_EMEA_data$Category == "Technology",]
global_superstore_EMEA_Technology_data <- arrange(global_superstore_EMEA_Technology_data,Category,Order.Month.Year)
global_superstore_EMEA_Technology_data$Order <- rank(global_superstore_EMEA_Technology_data$Order.Month.Year,ties.method= "first")
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Data set for Market: EU and Category: Furniture
global_superstore_EU_data <- global_superstore_Summary_data[global_superstore_Summary_data$Market == "EU",]
global_superstore_EU_Furniture_data <- global_superstore_EU_data[global_superstore_EU_data$Category == "Furniture",]
global_superstore_EU_Furniture_data <- arrange(global_superstore_EU_Furniture_data,Category,Order.Month.Year)
global_superstore_EU_Furniture_data$Order <- rank(global_superstore_EU_Furniture_data$Order.Month.Year,ties.method= "first")
# Data set for Market: EU and Category: Office Supplies
global_superstore_EU_Office_Supplies_data <- global_superstore_EU_data[global_superstore_EU_data$Category == "Office Supplies",]
global_superstore_EU_Office_Supplies_data <- arrange(global_superstore_EU_Office_Supplies_data,Category,Order.Month.Year)
global_superstore_EU_Office_Supplies_data$Order <- rank(global_superstore_EU_Office_Supplies_data$Order.Month.Year,ties.method= "first")
# Data set for Market: EU and Category: Technology
global_superstore_EU_Technology_data <- global_superstore_EU_data[global_superstore_EU_data$Category == "Technology",]
global_superstore_EU_Technology_data <- arrange(global_superstore_EU_Technology_data,Category,Order.Month.Year)
global_superstore_EU_Technology_data$Order <- rank(global_superstore_EU_Technology_data$Order.Month.Year,ties.method= "first")
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Data set for Market: LATAM and Category: Furniture
global_superstore_LATAM_data <- global_superstore_Summary_data[global_superstore_Summary_data$Market == "LATAM",]
global_superstore_LATAM_Furniture_data <- global_superstore_LATAM_data[global_superstore_LATAM_data$Category == "Furniture",]
global_superstore_LATAM_Furniture_data <- arrange(global_superstore_LATAM_Furniture_data,Category,Order.Month.Year)
global_superstore_LATAM_Furniture_data$Order <- rank(global_superstore_LATAM_Furniture_data$Order.Month.Year,ties.method= "first")
# Data set for Market: LATAM and Category: Office Supplies
global_superstore_LATAM_Office_Supplies_data <- global_superstore_LATAM_data[global_superstore_LATAM_data$Category == "Office Supplies",]
global_superstore_LATAM_Office_Supplies_data <- arrange(global_superstore_LATAM_Office_Supplies_data,Category,Order.Month.Year)
global_superstore_LATAM_Office_Supplies_data$Order <- rank(global_superstore_LATAM_Office_Supplies_data$Order.Month.Year,ties.method= "first")
# Data set for Market: LATAM and Category: Technology
global_superstore_LATAM_Technology_data <- global_superstore_LATAM_data[global_superstore_LATAM_data$Category == "Technology",]
global_superstore_LATAM_Technology_data <- arrange(global_superstore_LATAM_Technology_data,Category,Order.Month.Year)
global_superstore_LATAM_Technology_data$Order <- rank(global_superstore_LATAM_Technology_data$Order.Month.Year,ties.method= "first")
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Data set for Market: US and Category: Furniture
global_superstore_US_data <- global_superstore_Summary_data[global_superstore_Summary_data$Market == "US",]
global_superstore_US_Furniture_data <- global_superstore_US_data[global_superstore_US_data$Category == "Furniture",]
global_superstore_US_Furniture_data <- arrange(global_superstore_US_Furniture_data,Category,Order.Month.Year)
global_superstore_US_Furniture_data$Order <- rank(global_superstore_US_Furniture_data$Order.Month.Year,ties.method= "first")
# Data set for Market: US and Category: Office Supplies
global_superstore_US_Office_Supplies_data <- global_superstore_US_data[global_superstore_US_data$Category == "Office Supplies",]
global_superstore_US_Office_Supplies_data <- arrange(global_superstore_US_Office_Supplies_data,Category,Order.Month.Year)
global_superstore_US_Office_Supplies_data$Order <- rank(global_superstore_US_Office_Supplies_data$Order.Month.Year,ties.method= "first")
# Data set for Market: US and Category: Technology
global_superstore_US_Technology_data <- global_superstore_US_data[global_superstore_US_data$Category == "Technology",]
global_superstore_US_Technology_data <- arrange(global_superstore_US_Technology_data,Category,Order.Month.Year)
global_superstore_US_Technology_data$Order <- rank(global_superstore_US_Technology_data$Order.Month.Year,ties.method= "first")
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Analysis on the trend for africa : profit , Sales, Quantity 
global_superstore_africa_Furniture_subset_data <- subset(global_superstore_africa_Furniture_data, select = c(7,4,5,6))
#https://stackoverflow.com/questions/13324004/plotting-multiple-time-series-in-ggplot
library(ggplot2)
library(reshape2)
global_superstore_africa_Furniture_subset_melt_data <- melt(global_superstore_africa_Furniture_subset_data,id="Order")
global_superstore_africa_Furniture_ts_plot <- ggplot(global_superstore_africa_Furniture_subset_melt_data,aes(x=Order, y=value,colour=variable,group=variable)) + geom_line() + 
  labs(title="Africa-Furniture category"
       , x = "Months") + theme_pubclean()
#
global_superstore_africa_Office_Supplies_subset_data <- subset(global_superstore_africa_Office_Supplies_data, select = c(7,4,5,6))
global_superstore_africa_Office_Supplies_melt_data <- melt(global_superstore_africa_Office_Supplies_subset_data,id="Order")
global_superstore_africa_Office_Supplies_ts_plot <- ggplot(global_superstore_africa_Office_Supplies_melt_data,aes(x=Order, y=value,colour=variable,group=variable)) + geom_line() +
  labs(title="Africa-Office Supplies Category"
       , x = "Months") + theme_pubclean()
#
global_superstore_africa_Technology_subset_data <- subset(global_superstore_africa_Technology_data, select = c(7,4,5,6))
global_superstore_africa_Technology_melt_data <- melt(global_superstore_africa_Technology_subset_data,id="Order")
global_superstore_africa_Technology_ts_plot <- ggplot(global_superstore_africa_Technology_melt_data,aes(x=Order, y=value,colour=variable,group=variable)) + geom_line() +
  labs(title="Africa-Technology Category"
       , x = "Months") + theme_pubclean()
#
#grid.arrange(global_superstore_africa_Furniture_ts_plot,
#             global_superstore_africa_Office_Supplies_ts_plot,
#             global_superstore_africa_Technology_ts_plot,
#             nrow=3)
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Analysis on the trend for APAC : profit , Sales, Quantity 
global_superstore_APAC_Furniture_subset_data <- subset(global_superstore_APAC_Furniture_data, select = c(7,4,5,6))
#
global_superstore_APAC_Furniture_subset_melt_data <- melt(global_superstore_APAC_Furniture_subset_data,id="Order")
global_superstore_APAC_Furniture_ts_plot <- ggplot(global_superstore_APAC_Furniture_subset_melt_data,aes(x=Order, y=value,colour=variable,group=variable)) + geom_line() +
  labs(title="APAC-Furniture Category"
       , x = "Months") + theme_pubclean()
#
global_superstore_APAC_Office_Supplies_subset_data <- subset(global_superstore_APAC_Office_Supplies_data, select = c(7,4,5,6))
global_superstore_APAC_Office_Supplies_melt_data <- melt(global_superstore_APAC_Office_Supplies_subset_data,id="Order")
global_superstore_APAC_Office_Supplies_ts_plot <- ggplot(global_superstore_APAC_Office_Supplies_melt_data,aes(x=Order, y=value,colour=variable,group=variable)) + geom_line() +
  labs(title="APAC-Office Supplies Category"
       , x = "Months") + theme_pubclean()
#
global_superstore_APAC_Technology_subset_data <- subset(global_superstore_APAC_Technology_data, select = c(7,4,5,6))
global_superstore_APAC_Technology_melt_data <- melt(global_superstore_APAC_Technology_subset_data,id="Order")
global_superstore_APAC_Technology_ts_plot <- ggplot(global_superstore_APAC_Technology_melt_data,aes(x=Order, y=value,colour=variable,group=variable)) + geom_line() +
  labs(title="APAC-Technology Category"
       , x = "Months") + theme_pubclean()
#
#grid.arrange(global_superstore_APAC_Furniture_ts_plot,
#             global_superstore_APAC_Office_Supplies_ts_plot,
#             global_superstore_APAC_Technology_ts_plot,
#             nrow=3)
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Analysis on the trend for Canada : profit , Sales, Quantity 
global_superstore_Canada_Furniture_subset_data <- subset(global_superstore_Canada_Furniture_data, select = c(7,4,5,6))
#
global_superstore_Canada_Furniture_subset_melt_data <- melt(global_superstore_Canada_Furniture_subset_data,id="Order")
global_superstore_Canada_Furniture_ts_plot <- ggplot(global_superstore_Canada_Furniture_subset_melt_data,aes(x=Order, y=value,colour=variable,group=variable)) + geom_line() +
  labs(title="Canada-Furniture Category"
       , x = "Months") + theme_pubclean()
#
global_superstore_Canada_Office_Supplies_subset_data <- subset(global_superstore_Canada_Office_Supplies_data, select = c(7,4,5,6))
global_superstore_Canada_Office_Supplies_melt_data <- melt(global_superstore_Canada_Office_Supplies_subset_data,id="Order")
global_superstore_Canada_Office_Supplies_ts_plot <- ggplot(global_superstore_Canada_Office_Supplies_melt_data,aes(x=Order, y=value,colour=variable,group=variable)) + geom_line() +
  labs(title="Canada-Office Supplies Category"
       , x = "Months") + theme_pubclean()
#
global_superstore_Canada_Technology_subset_data <- subset(global_superstore_Canada_Technology_data, select = c(7,4,5,6))
global_superstore_Canada_Technology_melt_data <- melt(global_superstore_Canada_Technology_subset_data,id="Order")
global_superstore_Canada_Technology_ts_plot <- ggplot(global_superstore_Canada_Technology_melt_data,aes(x=Order, y=value,colour=variable,group=variable)) + geom_line() +
  labs(title="Canada-Technology Category"
       , x = "Months") + theme_pubclean()
#
grid.arrange(global_superstore_africa_Furniture_ts_plot,
             global_superstore_africa_Office_Supplies_ts_plot,
             global_superstore_africa_Technology_ts_plot,
             global_superstore_APAC_Furniture_ts_plot,
             global_superstore_APAC_Office_Supplies_ts_plot,
             global_superstore_APAC_Technology_ts_plot,
             global_superstore_Canada_Furniture_ts_plot,
             global_superstore_Canada_Office_Supplies_ts_plot,
             global_superstore_Canada_Technology_ts_plot,
             nrow=3,ncol = 3)
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Analysis on the trend for EMEA : profit , Sales, Quantity 
global_superstore_EMEA_Furniture_subset_data <- subset(global_superstore_EMEA_Furniture_data, select = c(7,4,5,6))
global_superstore_EMEA_Furniture_subset_melt_data <- melt(global_superstore_EMEA_Furniture_subset_data,id="Order")
global_superstore_EMEA_Furniture_ts_plot <- ggplot(global_superstore_EMEA_Furniture_subset_melt_data,aes(x=Order, y=value,colour=variable,group=variable)) + geom_line() +
  labs(title="EMEA-Furniture Category"
       , x = "Months") + theme_pubclean()
#
global_superstore_EMEA_Office_Supplies_subset_data <- subset(global_superstore_EMEA_Office_Supplies_data, select = c(7,4,5,6))
global_superstore_EMEA_Office_Supplies_melt_data <- melt(global_superstore_EMEA_Office_Supplies_subset_data,id="Order")
global_superstore_EMEA_Office_Supplies_ts_plot <- ggplot(global_superstore_EMEA_Office_Supplies_melt_data,aes(x=Order, y=value,colour=variable,group=variable)) + geom_line() +
  labs(title="EMEA-Office Supplies Category"
       , x = "Months") + theme_pubclean()
#
global_superstore_EMEA_Technology_subset_data <- subset(global_superstore_EMEA_Technology_data, select = c(7,4,5,6))
global_superstore_EMEA_Technology_melt_data <- melt(global_superstore_EMEA_Technology_subset_data,id="Order")
global_superstore_EMEA_Technology_ts_plot <- ggplot(global_superstore_EMEA_Technology_melt_data,aes(x=Order, y=value,colour=variable,group=variable)) + geom_line() +
  labs(title="EMEA-Technology Category"
       , x = "Months") + theme_pubclean()
#
#grid.arrange(global_superstore_EMEA_Furniture_ts_plot,
#             global_superstore_EMEA_Office_Supplies_ts_plot,
#             global_superstore_EMEA_Technology_ts_plot,
#             nrow=3)
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Analysis on the trend for EU : profit , Sales, Quantity 
global_superstore_EU_Furniture_subset_data <- subset(global_superstore_EU_Furniture_data, select = c(7,4,5,6))
#
global_superstore_EU_Furniture_subset_melt_data <- melt(global_superstore_EU_Furniture_subset_data,id="Order")
global_superstore_EU_Furniture_ts_plot <- ggplot(global_superstore_EU_Furniture_subset_melt_data,aes(x=Order, y=value,colour=variable,group=variable)) + geom_line() +
  labs(title="EU-Furniture Category"
       , x = "Months") + theme_pubclean()
#
global_superstore_EU_Office_Supplies_subset_data <- subset(global_superstore_EU_Office_Supplies_data, select = c(7,4,5,6))
global_superstore_EU_Office_Supplies_melt_data <- melt(global_superstore_EU_Office_Supplies_subset_data,id="Order")
global_superstore_EU_Office_Supplies_ts_plot <- ggplot(global_superstore_EU_Office_Supplies_melt_data,aes(x=Order, y=value,colour=variable,group=variable)) + geom_line() +
  labs(title="EU-Office Supplies Category"
       , x = "Months") + theme_pubclean()
#
global_superstore_EU_Technology_subset_data <- subset(global_superstore_EU_Technology_data, select = c(7,4,5,6))
global_superstore_EU_Technology_melt_data <- melt(global_superstore_EU_Technology_subset_data,id="Order")
global_superstore_EU_Technology_ts_plot <- ggplot(global_superstore_EU_Technology_melt_data,aes(x=Order, y=value,colour=variable,group=variable)) + geom_line() +
  labs(title="EU-Technology Category"
       , x = "Months") + theme_pubclean()
#
#grid.arrange(global_superstore_EU_Furniture_ts_plot,
#             global_superstore_EU_Office_Supplies_ts_plot,
#             global_superstore_EU_Technology_ts_plot,
#             nrow=3)
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Analysis on the trend for LATAM : profit , Sales, Quantity 
global_superstore_LATAM_Furniture_subset_data <- subset(global_superstore_LATAM_Furniture_data, select = c(7,4,5,6))
#
global_superstore_LATAM_Furniture_subset_melt_data <- melt(global_superstore_LATAM_Furniture_subset_data,id="Order")
global_superstore_LATAM_Furniture_ts_plot <- ggplot(global_superstore_LATAM_Furniture_subset_melt_data,aes(x=Order, y=value,colour=variable,group=variable)) + geom_line() +
  labs(title="LATAM-Furniture Category"
       , x = "Months") + theme_pubclean()
#
global_superstore_LATAM_Office_Supplies_subset_data <- subset(global_superstore_LATAM_Office_Supplies_data, select = c(7,4,5,6))
global_superstore_LATAM_Office_Supplies_melt_data <- melt(global_superstore_LATAM_Office_Supplies_subset_data,id="Order")
global_superstore_LATAM_Office_Supplies_ts_plot <- ggplot(global_superstore_LATAM_Office_Supplies_melt_data,aes(x=Order, y=value,colour=variable,group=variable)) + geom_line() +
  labs(title="LATAM-Office Supplies Category"
       , x = "Months") + theme_pubclean()
#
global_superstore_LATAM_Technology_subset_data <- subset(global_superstore_LATAM_Technology_data, select = c(7,4,5,6))
global_superstore_LATAM_Technology_melt_data <- melt(global_superstore_LATAM_Technology_subset_data,id="Order")
global_superstore_LATAM_Technology_ts_plot <- ggplot(global_superstore_LATAM_Technology_melt_data,aes(x=Order, y=value,colour=variable,group=variable)) + geom_line() +
  labs(title="LATAM-Technology Category"
       , x = "Months") + theme_pubclean()
#
grid.arrange(global_superstore_EMEA_Furniture_ts_plot,
             global_superstore_EMEA_Office_Supplies_ts_plot,
             global_superstore_EMEA_Technology_ts_plot,
             global_superstore_EU_Furniture_ts_plot,
             global_superstore_EU_Office_Supplies_ts_plot,
             global_superstore_EU_Technology_ts_plot,
             global_superstore_LATAM_Furniture_ts_plot,
             global_superstore_LATAM_Office_Supplies_ts_plot,
             global_superstore_LATAM_Technology_ts_plot,
             nrow=3,ncol=3)
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Analysis on the trend for US : profit , Sales, Quantity 
global_superstore_US_Furniture_subset_data <- subset(global_superstore_US_Furniture_data, select = c(7,4,5,6))
global_superstore_US_Furniture_subset_melt_data <- melt(global_superstore_US_Furniture_subset_data,id="Order")
global_superstore_US_Furniture_ts_plot <- ggplot(global_superstore_US_Furniture_subset_melt_data,aes(x=Order, y=value,colour=variable,group=variable)) + geom_line() +
  labs(title="US-Furniture Category"
       , x = "Months") + theme_pubclean()
#
global_superstore_US_Office_Supplies_subset_data <- subset(global_superstore_US_Office_Supplies_data, select = c(7,4,5,6))
global_superstore_US_Office_Supplies_melt_data <- melt(global_superstore_US_Office_Supplies_subset_data,id="Order")
global_superstore_US_Office_Supplies_ts_plot <- ggplot(global_superstore_US_Office_Supplies_melt_data,aes(x=Order, y=value,colour=variable,group=variable)) + geom_line() +
  labs(title="US-Office Supplies Category"
       , x = "Months") + theme_pubclean()
#
global_superstore_US_Technology_subset_data <- subset(global_superstore_US_Technology_data, select = c(7,4,5,6))
global_superstore_US_Technology_melt_data <- melt(global_superstore_US_Technology_subset_data,id="Order")
global_superstore_US_Technology_ts_plot <- ggplot(global_superstore_US_Technology_melt_data,aes(x=Order, y=value,colour=variable,group=variable)) + geom_line() +
  labs(title="US-Technology Category"
       , x = "Months") + theme_pubclean()
#

grid.arrange(global_superstore_US_Furniture_ts_plot,
             global_superstore_US_Office_Supplies_ts_plot,
             global_superstore_US_Technology_ts_plot,
             nrow=1, ncol = 3)
#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------
# EDA
# b.Market.Segment
#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------
#summary of sales for market segment data
global_superstore_Sales_Summary_for_MS_data <- global_superstore_data %>% 
  group_by_(.dots=c("Order.Month.Year","Market.Segment")) %>% 
  summarize(x=sum(Sales))
#summary of Profit
global_superstore_Profit_Summary_for_MS_data <- global_superstore_data %>% 
  group_by_(.dots=c("Order.Month.Year","Market.Segment")) %>% 
  summarize(x=sum(Profit))
#summary of quantity
global_superstore_Quantity_Summary_for_MS_data <- global_superstore_data %>% 
  group_by_(.dots=c("Order.Month.Year","Market.Segment")) %>% 
  summarize(x=sum(as.numeric(Quantity)))
#
colnames(global_superstore_Sales_Summary_for_MS_data)[3] <- "Total.Sales"
colnames(global_superstore_Profit_Summary_for_MS_data)[3] <- "Total.Profit"
colnames(global_superstore_Quantity_Summary_for_MS_data)[3] <- "Total.Quantity"
#
global_superstore_Summary_for_MS_data <- inner_join(global_superstore_Sales_Summary_for_MS_data, global_superstore_Profit_Summary_for_MS_data, by = c("Order.Month.Year" = "Order.Month.Year", "Market.Segment" = "Market.Segment"))
global_superstore_Summary_for_MS_data <- inner_join(global_superstore_Summary_for_MS_data, global_superstore_Quantity_Summary_for_MS_data, by = c("Order.Month.Year" = "Order.Month.Year", "Market.Segment" = "Market.Segment"))
#
#head(global_superstore_Summary_for_MS_data)
#----------------------------------------------------------------
#----------------------------------------------------------------Africa-Consumer Segent
#1.Analysis on the trend for africa : profit , Sales, Quantity  : Market Segment 
global_superstore_africa_Furniture_MS_ds <- global_superstore_Summary_for_MS_data[global_superstore_Summary_for_MS_data$Market.Segment == "Africa-Consumer",]
global_superstore_africa_Furniture_MS_ds <- arrange(global_superstore_africa_Furniture_MS_ds,Order.Month.Year)
global_superstore_africa_Furniture_MS_ds$Order <- rank(global_superstore_africa_Furniture_MS_ds$Order.Month.Year,ties.method= "first")
global_superstore_africa_Furniture_MS_ds_subset <- subset(global_superstore_africa_Furniture_MS_ds, select = c(6,3,4,5))
global_superstore_africa_Furniture_MS_ds_subset_melt <- melt(global_superstore_africa_Furniture_MS_ds_subset,id="Order")
global_superstore_africa_Furniture_MS_ds_subset_melt_plot <- ggplot(global_superstore_africa_Furniture_MS_ds_subset_melt,aes(x=Order, y=value,colour=variable,group=variable)) + geom_line() +
  labs(title="Africa-Consumer Segment"
       , x = "Months") + theme_pubclean()
#Africa-Corporate Segent
global_superstore_africa_Corporate_MS_ds <- global_superstore_Summary_for_MS_data[global_superstore_Summary_for_MS_data$Market.Segment == "Africa-Corporate",]
global_superstore_africa_Corporate_MS_ds <- arrange(global_superstore_africa_Corporate_MS_ds,Order.Month.Year)
global_superstore_africa_Corporate_MS_ds$Order <- rank(global_superstore_africa_Corporate_MS_ds$Order.Month.Year,ties.method= "first")
global_superstore_africa_Corporate_MS_ds_subset <- subset(global_superstore_africa_Corporate_MS_ds, select = c(6,3,4,5))
global_superstore_africa_Corporate_MS_ds_subset_melt <- melt(global_superstore_africa_Corporate_MS_ds_subset,id="Order")
global_superstore_africa_Corporate_MS_ds_subset_melt_plot <- ggplot(global_superstore_africa_Corporate_MS_ds_subset_melt,aes(x=Order, y=value,colour=variable,group=variable)) + geom_line() +
  labs(title="Africa-Corporate Segent"
       , x = "Months") + theme_pubclean()
#Africa-Home Office Segent
global_superstore_africa_HO_MS_ds <- global_superstore_Summary_for_MS_data[global_superstore_Summary_for_MS_data$Market.Segment == "Africa-Home Office",]
global_superstore_africa_HO_MS_ds <- arrange(global_superstore_africa_HO_MS_ds,Order.Month.Year)
global_superstore_africa_HO_MS_ds$Order <- rank(global_superstore_africa_HO_MS_ds$Order.Month.Year,ties.method= "first")
global_superstore_africa_HO_MS_ds_subset <- subset(global_superstore_africa_HO_MS_ds, select = c(6,3,4,5))
global_superstore_africa_HO_MS_ds_subset_melt <- melt(global_superstore_africa_HO_MS_ds_subset,id="Order")
global_superstore_africa_HO_MS_ds_subset_melt_plot <- ggplot(global_superstore_africa_HO_MS_ds_subset_melt,aes(x=Order, y=value,colour=variable,group=variable)) + geom_line() +
  labs(title="Africa-Home Office Segent"
       , x = "Months") + theme_pubclean()
#
#----------------------------------------------------------------APAC-Consumer Segent
#2.Analysis on the trend for APAC : profit , Sales, Quantity  : Market Segment 
global_superstore_APAC_Furniture_MS_ds <- global_superstore_Summary_for_MS_data[global_superstore_Summary_for_MS_data$Market.Segment == "APAC-Consumer",]
global_superstore_APAC_Furniture_MS_ds <- arrange(global_superstore_APAC_Furniture_MS_ds,Order.Month.Year)
global_superstore_APAC_Furniture_MS_ds$Order <- rank(global_superstore_APAC_Furniture_MS_ds$Order.Month.Year,ties.method= "first")
global_superstore_APAC_Furniture_MS_ds_subset <- subset(global_superstore_APAC_Furniture_MS_ds, select = c(6,3,4,5))
global_superstore_APAC_Furniture_MS_ds_subset_melt <- melt(global_superstore_APAC_Furniture_MS_ds_subset,id="Order")
global_superstore_APAC_Furniture_MS_ds_subset_melt_plot <- ggplot(global_superstore_APAC_Furniture_MS_ds_subset_melt,aes(x=Order, y=value,colour=variable,group=variable)) + geom_line() +
  labs(title="APAC-Consumer Segment"
       , x = "Months") + theme_pubclean()
#APAC-Corporate Segent
global_superstore_APAC_Corporate_MS_ds <- global_superstore_Summary_for_MS_data[global_superstore_Summary_for_MS_data$Market.Segment == "APAC-Corporate",]
global_superstore_APAC_Corporate_MS_ds <- arrange(global_superstore_APAC_Corporate_MS_ds,Order.Month.Year)
global_superstore_APAC_Corporate_MS_ds$Order <- rank(global_superstore_APAC_Corporate_MS_ds$Order.Month.Year,ties.method= "first")
global_superstore_APAC_Corporate_MS_ds_subset <- subset(global_superstore_APAC_Corporate_MS_ds, select = c(6,3,4,5))
global_superstore_APAC_Corporate_MS_ds_subset_melt <- melt(global_superstore_APAC_Corporate_MS_ds_subset,id="Order")
global_superstore_APAC_Corporate_MS_ds_subset_melt_plot <- ggplot(global_superstore_APAC_Corporate_MS_ds_subset_melt,aes(x=Order, y=value,colour=variable,group=variable)) + geom_line() +
  labs(title="APAC-Corporate Segent"
       , x = "Months") + theme_pubclean()
#APAC-Home Office Segent
global_superstore_APAC_HO_MS_ds <- global_superstore_Summary_for_MS_data[global_superstore_Summary_for_MS_data$Market.Segment == "APAC-Home Office",]
global_superstore_APAC_HO_MS_ds <- arrange(global_superstore_APAC_HO_MS_ds,Order.Month.Year)
global_superstore_APAC_HO_MS_ds$Order <- rank(global_superstore_APAC_HO_MS_ds$Order.Month.Year,ties.method= "first")
global_superstore_APAC_HO_MS_ds_subset <- subset(global_superstore_APAC_HO_MS_ds, select = c(6,3,4,5))
global_superstore_APAC_HO_MS_ds_subset_melt <- melt(global_superstore_APAC_HO_MS_ds_subset,id="Order")
global_superstore_APAC_HO_MS_ds_subset_melt_plot <- ggplot(global_superstore_APAC_HO_MS_ds_subset_melt,aes(x=Order, y=value,colour=variable,group=variable)) + geom_line() +
  labs(title="APAC-Home Office Segent"
       , x = "Months") + theme_pubclean()
#
#grid.arrange(global_superstore_africa_Furniture_MS_ds_subset_melt_plot,
#             global_superstore_africa_Corporate_MS_ds_subset_melt_plot,
#             global_superstore_africa_HO_MS_ds_subset_melt_plot,
#             global_superstore_APAC_Furniture_MS_ds_subset_melt_plot,
#             global_superstore_APAC_Corporate_MS_ds_subset_melt_plot,
#             global_superstore_APAC_HO_MS_ds_subset_melt_plot,
#             nrow=3,ncol=3)
#----------------------------------------------------------------US-Consumer Segent
#3.Analysis on the trend for US : profit , Sales, Quantity  : Market Segment 
global_superstore_US_Furniture_MS_ds <- global_superstore_Summary_for_MS_data[global_superstore_Summary_for_MS_data$Market.Segment == "US-Consumer",]
global_superstore_US_Furniture_MS_ds <- arrange(global_superstore_US_Furniture_MS_ds,Order.Month.Year)
global_superstore_US_Furniture_MS_ds$Order <- rank(global_superstore_US_Furniture_MS_ds$Order.Month.Year,ties.method= "first")
global_superstore_US_Furniture_MS_ds_subset <- subset(global_superstore_US_Furniture_MS_ds, select = c(6,3,4,5))
global_superstore_US_Furniture_MS_ds_subset_melt <- melt(global_superstore_US_Furniture_MS_ds_subset,id="Order")
global_superstore_US_Furniture_MS_ds_subset_melt_plot <- ggplot(global_superstore_US_Furniture_MS_ds_subset_melt,aes(x=Order, y=value,colour=variable,group=variable)) + geom_line() +
  labs(title="US-Consumer Segment"
       , x = "Months") + theme_pubclean()
#US-Corporate Segent
global_superstore_US_Corporate_MS_ds <- global_superstore_Summary_for_MS_data[global_superstore_Summary_for_MS_data$Market.Segment == "US-Corporate",]
global_superstore_US_Corporate_MS_ds <- arrange(global_superstore_US_Corporate_MS_ds,Order.Month.Year)
global_superstore_US_Corporate_MS_ds$Order <- rank(global_superstore_US_Corporate_MS_ds$Order.Month.Year,ties.method= "first")
global_superstore_US_Corporate_MS_ds_subset <- subset(global_superstore_US_Corporate_MS_ds, select = c(6,3,4,5))
global_superstore_US_Corporate_MS_ds_subset_melt <- melt(global_superstore_US_Corporate_MS_ds_subset,id="Order")
global_superstore_US_Corporate_MS_ds_subset_melt_plot <- ggplot(global_superstore_US_Corporate_MS_ds_subset_melt,aes(x=Order, y=value,colour=variable,group=variable)) + geom_line() +
  labs(title="US-Corporate Segent"
       , x = "Months") + theme_pubclean()
#US-Home Office Segent
global_superstore_US_HO_MS_ds <- global_superstore_Summary_for_MS_data[global_superstore_Summary_for_MS_data$Market.Segment == "US-Home Office",]
global_superstore_US_HO_MS_ds <- arrange(global_superstore_US_HO_MS_ds,Order.Month.Year)
global_superstore_US_HO_MS_ds$Order <- rank(global_superstore_US_HO_MS_ds$Order.Month.Year,ties.method= "first")
global_superstore_US_HO_MS_ds_subset <- subset(global_superstore_US_HO_MS_ds, select = c(6,3,4,5))
global_superstore_US_HO_MS_ds_subset_melt <- melt(global_superstore_US_HO_MS_ds_subset,id="Order")
global_superstore_US_HO_MS_ds_subset_melt_plot <- ggplot(global_superstore_US_HO_MS_ds_subset_melt,aes(x=Order, y=value,colour=variable,group=variable)) + geom_line() +
  labs(title="US-Home Office Segent"
       , x = "Months") + theme_pubclean()
#
grid.arrange(global_superstore_africa_Furniture_MS_ds_subset_melt_plot,
             global_superstore_africa_Corporate_MS_ds_subset_melt_plot,
             global_superstore_africa_HO_MS_ds_subset_melt_plot,
             global_superstore_APAC_Furniture_MS_ds_subset_melt_plot,
             global_superstore_APAC_Corporate_MS_ds_subset_melt_plot,
             global_superstore_APAC_HO_MS_ds_subset_melt_plot,
             global_superstore_US_Furniture_MS_ds_subset_melt_plot,
             global_superstore_US_Corporate_MS_ds_subset_melt_plot,
             global_superstore_US_HO_MS_ds_subset_melt_plot,
             nrow=3,ncol=3)
#----------------------------------------------------------------EU-Consumer Segent
#4.Analysis on the trend for EU : profit , Sales, Quantity  : Market Segment 
global_superstore_EU_Furniture_MS_ds <- global_superstore_Summary_for_MS_data[global_superstore_Summary_for_MS_data$Market.Segment == "EU-Consumer",]
global_superstore_EU_Furniture_MS_ds <- arrange(global_superstore_EU_Furniture_MS_ds,Order.Month.Year)
global_superstore_EU_Furniture_MS_ds$Order <- rank(global_superstore_EU_Furniture_MS_ds$Order.Month.Year,ties.method= "first")
global_superstore_EU_Furniture_MS_ds_subset <- subset(global_superstore_EU_Furniture_MS_ds, select = c(6,3,4,5))
global_superstore_EU_Furniture_MS_ds_subset_melt <- melt(global_superstore_EU_Furniture_MS_ds_subset,id="Order")
global_superstore_EU_Furniture_MS_ds_subset_melt_plot <- ggplot(global_superstore_EU_Furniture_MS_ds_subset_melt,aes(x=Order, y=value,colour=variable,group=variable)) + geom_line() +
  labs(title="EU-Consumer Segment"
       , x = "Months") + theme_pubclean()
#EU-Corporate Segent
global_superstore_EU_Corporate_MS_ds <- global_superstore_Summary_for_MS_data[global_superstore_Summary_for_MS_data$Market.Segment == "EU-Corporate",]
global_superstore_EU_Corporate_MS_ds <- arrange(global_superstore_EU_Corporate_MS_ds,Order.Month.Year)
global_superstore_EU_Corporate_MS_ds$Order <- rank(global_superstore_EU_Corporate_MS_ds$Order.Month.Year,ties.method= "first")
global_superstore_EU_Corporate_MS_ds_subset <- subset(global_superstore_EU_Corporate_MS_ds, select = c(6,3,4,5))
global_superstore_EU_Corporate_MS_ds_subset_melt <- melt(global_superstore_EU_Corporate_MS_ds_subset,id="Order")
global_superstore_EU_Corporate_MS_ds_subset_melt_plot <- ggplot(global_superstore_EU_Corporate_MS_ds_subset_melt,aes(x=Order, y=value,colour=variable,group=variable)) + geom_line() +
  labs(title="EU-Corporate Segent"
       , x = "Months") + theme_pubclean()
#EU-Home Office Segent
global_superstore_EU_HO_MS_ds <- global_superstore_Summary_for_MS_data[global_superstore_Summary_for_MS_data$Market.Segment == "EU-Home Office",]
global_superstore_EU_HO_MS_ds <- arrange(global_superstore_EU_HO_MS_ds,Order.Month.Year)
global_superstore_EU_HO_MS_ds$Order <- rank(global_superstore_EU_HO_MS_ds$Order.Month.Year,ties.method= "first")
global_superstore_EU_HO_MS_ds_subset <- subset(global_superstore_EU_HO_MS_ds, select = c(6,3,4,5))
global_superstore_EU_HO_MS_ds_subset_melt <- melt(global_superstore_EU_HO_MS_ds_subset,id="Order")
global_superstore_EU_HO_MS_ds_subset_melt_plot <- ggplot(global_superstore_EU_HO_MS_ds_subset_melt,aes(x=Order, y=value,colour=variable,group=variable)) + geom_line() +
  labs(title="EU-Corporate Segent"
       , x = "Months") + theme_pubclean()
#
#grid.arrange(global_superstore_EU_Furniture_MS_ds_subset_melt_plot,
#             global_superstore_EU_Corporate_MS_ds_subset_melt_plot,
#             global_superstore_EU_HO_MS_ds_subset_melt_plot,
#             nrow=3)
#----------------------------------------------------------------EMEA-Consumer Segent
#5.Analysis on the trend for EMEA : profit , Sales, Quantity  : Market Segment 
global_superstore_EMEA_Furniture_MS_ds <- global_superstore_Summary_for_MS_data[global_superstore_Summary_for_MS_data$Market.Segment == "EMEA-Consumer",]
global_superstore_EMEA_Furniture_MS_ds <- arrange(global_superstore_EMEA_Furniture_MS_ds,Order.Month.Year)
global_superstore_EMEA_Furniture_MS_ds$Order <- rank(global_superstore_EMEA_Furniture_MS_ds$Order.Month.Year,ties.method= "first")
global_superstore_EMEA_Furniture_MS_ds_subset <- subset(global_superstore_EMEA_Furniture_MS_ds, select = c(6,3,4,5))
global_superstore_EMEA_Furniture_MS_ds_subset_melt <- melt(global_superstore_EMEA_Furniture_MS_ds_subset,id="Order")
global_superstore_EMEA_Furniture_MS_ds_subset_melt_plot <- ggplot(global_superstore_EMEA_Furniture_MS_ds_subset_melt,aes(x=Order, y=value,colour=variable,group=variable)) + geom_line() +
  labs(title="EMEA-Consumer Segent"
       , x = "Months") + theme_pubclean()
#EMEA-Corporate Segent
global_superstore_EMEA_Corporate_MS_ds <- global_superstore_Summary_for_MS_data[global_superstore_Summary_for_MS_data$Market.Segment == "EMEA-Corporate",]
global_superstore_EMEA_Corporate_MS_ds <- arrange(global_superstore_EMEA_Corporate_MS_ds,Order.Month.Year)
global_superstore_EMEA_Corporate_MS_ds$Order <- rank(global_superstore_EMEA_Corporate_MS_ds$Order.Month.Year,ties.method= "first")
global_superstore_EMEA_Corporate_MS_ds_subset <- subset(global_superstore_EMEA_Corporate_MS_ds, select = c(6,3,4,5))
global_superstore_EMEA_Corporate_MS_ds_subset_melt <- melt(global_superstore_EMEA_Corporate_MS_ds_subset,id="Order")
global_superstore_EMEA_Corporate_MS_ds_subset_melt_plot <- ggplot(global_superstore_EMEA_Corporate_MS_ds_subset_melt,aes(x=Order, y=value,colour=variable,group=variable)) + geom_line() +
  labs(title="EMEA-Corporate Segent"
       , x = "Months") + theme_pubclean()
#EMEA-Home Office Segent
global_superstore_EMEA_HO_MS_ds <- global_superstore_Summary_for_MS_data[global_superstore_Summary_for_MS_data$Market.Segment == "EMEA-Home Office",]
global_superstore_EMEA_HO_MS_ds <- arrange(global_superstore_EMEA_HO_MS_ds,Order.Month.Year)
global_superstore_EMEA_HO_MS_ds$Order <- rank(global_superstore_EMEA_HO_MS_ds$Order.Month.Year,ties.method= "first")
global_superstore_EMEA_HO_MS_ds_subset <- subset(global_superstore_EMEA_HO_MS_ds, select = c(6,3,4,5))
global_superstore_EMEA_HO_MS_ds_subset_melt <- melt(global_superstore_EMEA_HO_MS_ds_subset,id="Order")
global_superstore_EMEA_HO_MS_ds_subset_melt_plot <- ggplot(global_superstore_EMEA_HO_MS_ds_subset_melt,aes(x=Order, y=value,colour=variable,group=variable)) + geom_line() +
  labs(title="EMEA-Home Office Segent"
       , x = "Months") + theme_pubclean()
#
#grid.arrange(global_superstore_EU_Furniture_MS_ds_subset_melt_plot,
#             global_superstore_EU_Corporate_MS_ds_subset_melt_plot,
#             global_superstore_EU_HO_MS_ds_subset_melt_plot,
#             global_superstore_EMEA_Furniture_MS_ds_subset_melt_plot,
#             global_superstore_EMEA_Corporate_MS_ds_subset_melt_plot,
#             global_superstore_EMEA_HO_MS_ds_subset_melt_plot,
#             nrow=3)	   
#----------------------------------------------------------------LATAM-Consumer Segent
#6.Analysis on the trend for LATAM : profit , Sales, Quantity  : Market Segment 
global_superstore_LATAM_Furniture_MS_ds <- global_superstore_Summary_for_MS_data[global_superstore_Summary_for_MS_data$Market.Segment == "LATAM-Consumer",]
global_superstore_LATAM_Furniture_MS_ds <- arrange(global_superstore_LATAM_Furniture_MS_ds,Order.Month.Year)
global_superstore_LATAM_Furniture_MS_ds$Order <- rank(global_superstore_LATAM_Furniture_MS_ds$Order.Month.Year,ties.method= "first")
global_superstore_LATAM_Furniture_MS_ds_subset <- subset(global_superstore_LATAM_Furniture_MS_ds, select = c(6,3,4,5))
global_superstore_LATAM_Furniture_MS_ds_subset_melt <- melt(global_superstore_LATAM_Furniture_MS_ds_subset,id="Order")
global_superstore_LATAM_Furniture_MS_ds_subset_melt_plot <- ggplot(global_superstore_LATAM_Furniture_MS_ds_subset_melt,aes(x=Order, y=value,colour=variable,group=variable)) + geom_line() +
  labs(title="LATAM-Consumer Segent"
       , x = "Months") + theme_pubclean()
#LATAM-Corporate Segent
global_superstore_LATAM_Corporate_MS_ds <- global_superstore_Summary_for_MS_data[global_superstore_Summary_for_MS_data$Market.Segment == "LATAM-Corporate",]
global_superstore_LATAM_Corporate_MS_ds <- arrange(global_superstore_LATAM_Corporate_MS_ds,Order.Month.Year)
global_superstore_LATAM_Corporate_MS_ds$Order <- rank(global_superstore_LATAM_Corporate_MS_ds$Order.Month.Year,ties.method= "first")
global_superstore_LATAM_Corporate_MS_ds_subset <- subset(global_superstore_LATAM_Corporate_MS_ds, select = c(6,3,4,5))
global_superstore_LATAM_Corporate_MS_ds_subset_melt <- melt(global_superstore_LATAM_Corporate_MS_ds_subset,id="Order")
global_superstore_LATAM_Corporate_MS_ds_subset_melt_plot <- ggplot(global_superstore_LATAM_Corporate_MS_ds_subset_melt,aes(x=Order, y=value,colour=variable,group=variable)) + geom_line() +
  labs(title="LATAM-Corporate Segent"
       , x = "Months") + theme_pubclean()
#LATAM-Home Office Segent
global_superstore_LATAM_HO_MS_ds <- global_superstore_Summary_for_MS_data[global_superstore_Summary_for_MS_data$Market.Segment == "LATAM-Home Office",]
global_superstore_LATAM_HO_MS_ds <- arrange(global_superstore_LATAM_HO_MS_ds,Order.Month.Year)
global_superstore_LATAM_HO_MS_ds$Order <- rank(global_superstore_LATAM_HO_MS_ds$Order.Month.Year,ties.method= "first")
global_superstore_LATAM_HO_MS_ds_subset <- subset(global_superstore_LATAM_HO_MS_ds, select = c(6,3,4,5))
global_superstore_LATAM_HO_MS_ds_subset_melt <- melt(global_superstore_LATAM_HO_MS_ds_subset,id="Order")
global_superstore_LATAM_HO_MS_ds_subset_melt_plot <- ggplot(global_superstore_LATAM_HO_MS_ds_subset_melt,aes(x=Order, y=value,colour=variable,group=variable)) + geom_line() +
  labs(title="LATAM-Home Office Segent"
       , x = "Months") + theme_pubclean()
#
grid.arrange(global_superstore_EU_Furniture_MS_ds_subset_melt_plot,
             global_superstore_EU_Corporate_MS_ds_subset_melt_plot,
             global_superstore_EU_HO_MS_ds_subset_melt_plot,
             global_superstore_EMEA_Furniture_MS_ds_subset_melt_plot,
             global_superstore_EMEA_Corporate_MS_ds_subset_melt_plot,
             global_superstore_EMEA_HO_MS_ds_subset_melt_plot,
             global_superstore_LATAM_Furniture_MS_ds_subset_melt_plot,
             global_superstore_LATAM_Corporate_MS_ds_subset_melt_plot,
             global_superstore_LATAM_HO_MS_ds_subset_melt_plot,
             nrow=3,ncol=3)
#----------------------------------------------------------------Canada-Consumer Segent
#7.Analysis on the trend for Canada : profit , Sales, Quantity  : Market Segment 
global_superstore_Canada_Furniture_MS_ds <- global_superstore_Summary_for_MS_data[global_superstore_Summary_for_MS_data$Market.Segment == "Canada-Consumer",]
global_superstore_Canada_Furniture_MS_ds <- arrange(global_superstore_Canada_Furniture_MS_ds,Order.Month.Year)
global_superstore_Canada_Furniture_MS_ds$Order <- rank(global_superstore_Canada_Furniture_MS_ds$Order.Month.Year,ties.method= "first")
global_superstore_Canada_Furniture_MS_ds_subset <- subset(global_superstore_Canada_Furniture_MS_ds, select = c(6,3,4,5))
global_superstore_Canada_Furniture_MS_ds_subset_melt <- melt(global_superstore_Canada_Furniture_MS_ds_subset,id="Order")
global_superstore_Canada_Furniture_MS_ds_subset_melt_plot <- ggplot(global_superstore_Canada_Furniture_MS_ds_subset_melt,aes(x=Order, y=value,colour=variable,group=variable)) + geom_line() +
  labs(title="Canada-Consumer Segent"
       , x = "Months") + theme_pubclean()
#Canada-Corporate Segent
global_superstore_Canada_Corporate_MS_ds <- global_superstore_Summary_for_MS_data[global_superstore_Summary_for_MS_data$Market.Segment == "Canada-Corporate",]
global_superstore_Canada_Corporate_MS_ds <- arrange(global_superstore_Canada_Corporate_MS_ds,Order.Month.Year)
global_superstore_Canada_Corporate_MS_ds$Order <- rank(global_superstore_Canada_Corporate_MS_ds$Order.Month.Year,ties.method= "first")
global_superstore_Canada_Corporate_MS_ds_subset <- subset(global_superstore_Canada_Corporate_MS_ds, select = c(6,3,4,5))
global_superstore_Canada_Corporate_MS_ds_subset_melt <- melt(global_superstore_Canada_Corporate_MS_ds_subset,id="Order")
global_superstore_Canada_Corporate_MS_ds_subset_melt_plot <- ggplot(global_superstore_Canada_Corporate_MS_ds_subset_melt,aes(x=Order, y=value,colour=variable,group=variable)) + geom_line() +
  labs(title="Canada-Corporate Segent"
       , x = "Months") + theme_pubclean()
#Canada-Home Office Segent
global_superstore_Canada_HO_MS_ds <- global_superstore_Summary_for_MS_data[global_superstore_Summary_for_MS_data$Market.Segment == "Canada-Home Office",]
global_superstore_Canada_HO_MS_ds <- arrange(global_superstore_Canada_HO_MS_ds,Order.Month.Year)
global_superstore_Canada_HO_MS_ds$Order <- rank(global_superstore_Canada_HO_MS_ds$Order.Month.Year,ties.method= "first")
global_superstore_Canada_HO_MS_ds_subset <- subset(global_superstore_Canada_HO_MS_ds, select = c(6,3,4,5))
global_superstore_Canada_HO_MS_ds_subset_melt <- melt(global_superstore_Canada_HO_MS_ds_subset,id="Order")
global_superstore_Canada_HO_MS_ds_subset_melt_plot <- ggplot(global_superstore_Canada_HO_MS_ds_subset_melt,aes(x=Order, y=value,colour=variable,group=variable)) + geom_line() +
  labs(title="Canada-Home Office Segent"
       , x = "Months") + theme_pubclean()

#
grid.arrange(global_superstore_Canada_Furniture_MS_ds_subset_melt_plot,
             global_superstore_Canada_Corporate_MS_ds_subset_melt_plot,
             global_superstore_Canada_HO_MS_ds_subset_melt_plot,
             nrow=3)	
#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------
###################### Data preparation: ######################
#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------
#  You would need to first segment the whole dataset into the 21 subsets based on 
#the market and the customer segment level. Next, comes the most important data 
#preparation step. That is to convert the transaction-level data into a time series. 
#Thus, you would need to aggregate the 3 attributes  - Sales, Quantity & Profit, 
#over the Order Date to arrive at monthly values for these attributes. 
#Once, you arrive at these 3 time series for each of the 21 segments, 
#we need to find the 2 most profitable and consistently profitable segments. 
#For this, the metric that you can use is the coefficient of variation of the 
#Profit for all 21 market segments. If you wish to know more about the coefficient 
#of variation, you can read on it here.
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
#1. Africa-Consumer Segment dataset for building ts
#-----------------------------------------------------------------------------
par(mfrow=c(2,3))
gs_Africa_Consumer_MS_ds <- global_superstore_Summary_for_MS_data[global_superstore_Summary_for_MS_data$Market.Segment == "Africa-Consumer",]
#converting to time series data
ts_Africa_Consumer <- ts(gs_Africa_Consumer_MS_ds$Total.Profit, start=c(2011,1), end=c(2014,12), frequency = 12)
#ploting time series data
coeff=coefficients(lm(ts_Africa_Consumer ~ time(ts_Africa_Consumer)))
# equation of the line : 
eq = paste0("y = ", round(coeff[2],1), "*x ", round(coeff[1],1))
plot(ts_Africa_Consumer, main=eq)
#plot(ts_Africa_Consumer)
abline(reg=lm(ts_Africa_Consumer ~ time(ts_Africa_Consumer)), col="red")
#...................................
#2. #Africa-Corporate Segment dataset for building ts
#-----------------------------------------------------------------------------
gs_Africa_Corporate_MS_ds <- global_superstore_Summary_for_MS_data[global_superstore_Summary_for_MS_data$Market.Segment == "Africa-Corporate",]
#converting to time series data
ts_Africa_Corporate <- ts(gs_Africa_Corporate_MS_ds$Total.Profit, start=c(2011,1), end=c(2014,12), frequency = 12)
#ploting time series data
coeff=coefficients(lm(ts_Africa_Corporate ~ time(ts_Africa_Corporate)))
# equation of the line : 
eq = paste0("y = ", round(coeff[2],1), "*x ", round(coeff[1],1))
plot(ts_Africa_Corporate, main=eq)
abline(reg=lm(ts_Africa_Corporate ~ time(ts_Africa_Corporate)), col="red")
#...................................
#3. Africa-Home Office Segment dataset for building ts
#-----------------------------------------------------------------------------
gs_Africa_Home_Office_MS_ds <- global_superstore_Summary_for_MS_data[global_superstore_Summary_for_MS_data$Market.Segment == "Africa-Home Office",]
#converting to time series data
ts_gs_Africa_Home_Office <- ts(gs_Africa_Home_Office_MS_ds$Total.Profit, start=c(2011,1), end=c(2014,12), frequency = 12)
#ploting time series data
coeff=coefficients(lm(ts_gs_Africa_Home_Office ~ time(ts_gs_Africa_Home_Office)))
# equation of the line : 
eq = paste0("y = ", round(coeff[2],1), "*x ", round(coeff[1],1))
plot(ts_gs_Africa_Home_Office, main=eq)
abline(reg=lm(ts_gs_Africa_Home_Office ~ time(ts_gs_Africa_Home_Office)), col="red")
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#4. APAC-Consumer Segment dataset for building ts
#-----------------------------------------------------------------------------
gs_APAC_Consumer_MS_ds <- global_superstore_Summary_for_MS_data[global_superstore_Summary_for_MS_data$Market.Segment == "APAC-Consumer",]
#converting to time series data
ts_APAC_Consumer <- ts(gs_APAC_Consumer_MS_ds$Total.Profit, start=c(2011,1), end=c(2014,12), frequency = 12)
#ploting time series data
coeff=coefficients(lm(ts_APAC_Consumer ~ time(ts_APAC_Consumer)))
# equation of the line : 
eq = paste0("y = ", round(coeff[2],1), "*x ", round(coeff[1],1))
plot(ts_APAC_Consumer, main=eq)
abline(reg=lm(ts_APAC_Consumer ~ time(ts_APAC_Consumer)), col="red")
#...................................
#5. APAC-Corporate Segment dataset for building ts
#-----------------------------------------------------------------------------
gs_APAC_Corporate_MS_ds <- global_superstore_Summary_for_MS_data[global_superstore_Summary_for_MS_data$Market.Segment == "APAC-Corporate",]
#converting to time series data
ts_APAC_Corporate <- ts(gs_APAC_Corporate_MS_ds$Total.Profit, start=c(2011,1), end=c(2014,12), frequency = 12)
#ploting time series data
coeff=coefficients(lm(ts_APAC_Corporate ~ time(ts_APAC_Corporate)))
# equation of the line : 
eq = paste0("y = ", round(coeff[2],1), "*x ", round(coeff[1],1))
plot(ts_APAC_Corporate, main=eq)
abline(reg=lm(ts_APAC_Corporate ~ time(ts_APAC_Corporate)), col="red")
#...................................
#6. APAC-Home Office Segment dataset for building ts
#-----------------------------------------------------------------------------
gs_APAC_Home_Office_MS_ds <- global_superstore_Summary_for_MS_data[global_superstore_Summary_for_MS_data$Market.Segment == "APAC-Home Office",]
#converting to time series data
ts_gs_APAC_Home_Office <- ts(gs_APAC_Home_Office_MS_ds$Total.Profit, start=c(2011,1), end=c(2014,12), frequency = 12)
#ploting time series data
coeff=coefficients(lm(ts_gs_APAC_Home_Office ~ time(ts_gs_APAC_Home_Office)))
# equation of the line : 
eq = paste0("y = ", round(coeff[2],1), "*x ", round(coeff[1],1))
plot(ts_gs_APAC_Home_Office, main=eq)
abline(reg=lm(ts_gs_APAC_Home_Office ~ time(ts_gs_APAC_Home_Office)), col="red")
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#7. Canada-Consumer Segment dataset for building ts
#-----------------------------------------------------------------------------
gs_Canada_Consumer_MS_ds <- global_superstore_Summary_for_MS_data[global_superstore_Summary_for_MS_data$Market.Segment == "Canada-Consumer",]
#converting to time series data
ts_Canada_Consumer <- ts(gs_Canada_Consumer_MS_ds$Total.Profit, start=c(2011,1), end=c(2014,12), frequency = 12)
#ploting time series data
coeff=coefficients(lm(ts_Canada_Consumer ~ time(ts_Canada_Consumer)))
# equation of the line : 
eq = paste0("y = ", round(coeff[2],1), "*x ", round(coeff[1],1))
plot(ts_Canada_Consumer, main=eq)
abline(reg=lm(ts_Canada_Consumer ~ time(ts_Canada_Consumer)), col="red")
#...................................
#8. Canada-Corporate Segment dataset for building ts
#-----------------------------------------------------------------------------
gs_Canada_Corporate_MS_ds <- global_superstore_Summary_for_MS_data[global_superstore_Summary_for_MS_data$Market.Segment == "Canada-Corporate",]
#converting to time series data
ts_Canada_Corporate <- ts(gs_Canada_Corporate_MS_ds$Total.Profit, start=c(2011,1), end=c(2014,12), frequency = 12)
#ploting time series data
coeff=coefficients(lm(ts_Canada_Corporate ~ time(ts_Canada_Corporate)))
# equation of the line : 
eq = paste0("y = ", round(coeff[2],1), "*x ", round(coeff[1],1))
plot(ts_Canada_Corporate, main=eq)
abline(reg=lm(ts_Canada_Corporate ~ time(ts_Canada_Corporate)), col="red")
#...................................
#9. Canada-Home Office Segment dataset for building ts
#-----------------------------------------------------------------------------
gs_Canada_Home_Office_MS_ds <- global_superstore_Summary_for_MS_data[global_superstore_Summary_for_MS_data$Market.Segment == "Canada-Home Office",]
#converting to time series data
ts_gs_Canada_Home_Office <- ts(gs_Canada_Home_Office_MS_ds$Total.Profit, start=c(2011,1), end=c(2014,12), frequency = 12)
#ploting time series data
coeff=coefficients(lm(ts_gs_Canada_Home_Office ~ time(ts_gs_Canada_Home_Office)))
# equation of the line : 
eq = paste0("y = ", round(coeff[2],1), "*x ", round(coeff[1],1))
plot(ts_gs_Canada_Home_Office, main=eq)
abline(reg=lm(ts_gs_Canada_Home_Office ~ time(ts_gs_Canada_Home_Office)), col="red")
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#10. EMEA-Consumer Segment dataset for building ts
#-----------------------------------------------------------------------------
gs_EMEA_Consumer_MS_ds <- global_superstore_Summary_for_MS_data[global_superstore_Summary_for_MS_data$Market.Segment == "EMEA-Consumer",]
#converting to time series data
ts_EMEA_Consumer <- ts(gs_EMEA_Consumer_MS_ds$Total.Profit, start=c(2011,1), end=c(2014,12), frequency = 12)
#ploting time series data
coeff=coefficients(lm(ts_EMEA_Consumer ~ time(ts_EMEA_Consumer)))
# equation of the line : 
eq = paste0("y = ", round(coeff[2],1), "*x ", round(coeff[1],1))
plot(ts_EMEA_Consumer, main=eq)
abline(reg=lm(ts_EMEA_Consumer ~ time(ts_EMEA_Consumer)), col="red")
#...................................
#11. EMEA-Corporate Segment dataset for building ts
#-----------------------------------------------------------------------------
gs_EMEA_Corporate_MS_ds <- global_superstore_Summary_for_MS_data[global_superstore_Summary_for_MS_data$Market.Segment == "EMEA-Corporate",]
#converting to time series data
ts_EMEA_Corporate <- ts(gs_EMEA_Corporate_MS_ds$Total.Profit, start=c(2011,1), end=c(2014,12), frequency = 12)
#ploting time series data
coeff=coefficients(lm(ts_EMEA_Corporate ~ time(ts_EMEA_Corporate)))
# equation of the line : 
eq = paste0("y = ", round(coeff[2],1), "*x ", round(coeff[1],1))
plot(ts_EMEA_Corporate, main=eq)
abline(reg=lm(ts_EMEA_Corporate ~ time(ts_EMEA_Corporate)), col="red")
#...................................
#12. EMEA-Home Office Segment dataset for building ts
#-----------------------------------------------------------------------------
gs_EMEA_Home_Office_MS_ds <- global_superstore_Summary_for_MS_data[global_superstore_Summary_for_MS_data$Market.Segment == "EMEA-Home Office",]
#converting to time series data
ts_gs_EMEA_Home_Office <- ts(gs_EMEA_Home_Office_MS_ds$Total.Profit, start=c(2011,1), end=c(2014,12), frequency = 12)
#ploting time series data
coeff=coefficients(lm(ts_gs_EMEA_Home_Office ~ time(ts_gs_EMEA_Home_Office)))
# equation of the line : 
eq = paste0("y = ", round(coeff[2],1), "*x ", round(coeff[1],1))
plot(ts_gs_EMEA_Home_Office, main=eq)
abline(reg=lm(ts_gs_EMEA_Home_Office ~ time(ts_gs_EMEA_Home_Office)), col="red")
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#13. EU-Consumer Segment dataset for building ts
#-----------------------------------------------------------------------------
gs_EU_Consumer_MS_ds <- global_superstore_Summary_for_MS_data[global_superstore_Summary_for_MS_data$Market.Segment == "EU-Consumer",]
#converting to time series data
ts_EU_Consumer <- ts(gs_EU_Consumer_MS_ds$Total.Profit, start=c(2011,1), end=c(2014,12), frequency = 12)
#ploting time series data
coeff=coefficients(lm(ts_EU_Consumer ~ time(ts_EU_Consumer)))
# equation of the line : 
eq = paste0("y = ", round(coeff[2],1), "*x ", round(coeff[1],1))
plot(ts_EU_Consumer, main=eq)
abline(reg=lm(ts_EU_Consumer ~ time(ts_EU_Consumer)), col="red")
#...................................
#14. EU-Corporate Segment dataset for building ts
#-----------------------------------------------------------------------------
gs_EU_Corporate_MS_ds <- global_superstore_Summary_for_MS_data[global_superstore_Summary_for_MS_data$Market.Segment == "EU-Corporate",]
#converting to time series data
ts_EU_Corporate <- ts(gs_EU_Corporate_MS_ds$Total.Profit, start=c(2011,1), end=c(2014,12), frequency = 12)
#ploting time series data
coeff=coefficients(lm(ts_EU_Corporate ~ time(ts_EU_Corporate)))
# equation of the line : 
eq = paste0("y = ", round(coeff[2],1), "*x ", round(coeff[1],1))
plot(ts_EU_Corporate, main=eq)
abline(reg=lm(ts_EU_Corporate ~ time(ts_EU_Corporate)), col="red")
#...................................
#15. EU-Home Office Segment dataset for building ts
#-----------------------------------------------------------------------------
gs_EU_Home_Office_MS_ds <- global_superstore_Summary_for_MS_data[global_superstore_Summary_for_MS_data$Market.Segment == "EU-Home Office",]
#converting to time series data
ts_gs_EU_Home_Office <- ts(gs_EU_Home_Office_MS_ds$Total.Profit, start=c(2011,1), end=c(2014,12), frequency = 12)
#ploting time series data
coeff=coefficients(lm(ts_gs_EU_Home_Office ~ time(ts_gs_EU_Home_Office)))
# equation of the line : 
eq = paste0("y = ", round(coeff[2],1), "*x ", round(coeff[1],1))
plot(ts_gs_EU_Home_Office, main=eq)
abline(reg=lm(ts_gs_EU_Home_Office ~ time(ts_gs_EU_Home_Office)), col="red")
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#16. LATAM-Consumer Segment dataset for building ts
#-----------------------------------------------------------------------------
gs_LATAM_Consumer_MS_ds <- global_superstore_Summary_for_MS_data[global_superstore_Summary_for_MS_data$Market.Segment == "LATAM-Consumer",]
#converting to time series data
ts_LATAM_Consumer <- ts(gs_LATAM_Consumer_MS_ds$Total.Profit, start=c(2011,1), end=c(2014,12), frequency = 12)
#ploting time series data
coeff=coefficients(lm(ts_LATAM_Consumer ~ time(ts_LATAM_Consumer)))
# equation of the line : 
eq = paste0("y = ", round(coeff[2],1), "*x ", round(coeff[1],1))
plot(ts_LATAM_Consumer, main=eq)
abline(reg=lm(ts_LATAM_Consumer ~ time(ts_LATAM_Consumer)), col="red")
#...................................
#17. LATAM-Corporate Segment dataset for building ts
#-----------------------------------------------------------------------------
gs_LATAM_Corporate_MS_ds <- global_superstore_Summary_for_MS_data[global_superstore_Summary_for_MS_data$Market.Segment == "LATAM-Corporate",]
#converting to time series data
ts_LATAM_Corporate <- ts(gs_LATAM_Corporate_MS_ds$Total.Profit, start=c(2011,1), end=c(2014,12), frequency = 12)
#ploting time series data
coeff=coefficients(lm(ts_LATAM_Corporate ~ time(ts_LATAM_Corporate)))
# equation of the line : 
eq = paste0("y = ", round(coeff[2],1), "*x ", round(coeff[1],1))
plot(ts_LATAM_Corporate, main=eq)
abline(reg=lm(ts_LATAM_Corporate ~ time(ts_LATAM_Corporate)), col="red")
#...................................
#18. LATAM-Home Office Segment dataset for building ts
#-----------------------------------------------------------------------------
gs_LATAM_Home_Office_MS_ds <- global_superstore_Summary_for_MS_data[global_superstore_Summary_for_MS_data$Market.Segment == "LATAM-Home Office",]
#converting to time series data
ts_gs_LATAM_Home_Office <- ts(gs_LATAM_Home_Office_MS_ds$Total.Profit, start=c(2011,1), end=c(2014,12), frequency = 12)
#ploting time series data
coeff=coefficients(lm(ts_gs_LATAM_Home_Office ~ time(ts_gs_LATAM_Home_Office)))
# equation of the line : 
eq = paste0("y = ", round(coeff[2],1), "*x ", round(coeff[1],1))
plot(ts_gs_LATAM_Home_Office, main=eq)
abline(reg=lm(ts_gs_LATAM_Home_Office ~ time(ts_gs_LATAM_Home_Office)), col="red")
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#19. US-Consumer Segment dataset for building ts
#-----------------------------------------------------------------------------
par(mfrow=c(1,3))
gs_US_Consumer_MS_ds <- global_superstore_Summary_for_MS_data[global_superstore_Summary_for_MS_data$Market.Segment == "US-Consumer",]
#converting to time series data
ts_US_Consumer <- ts(gs_US_Consumer_MS_ds$Total.Profit, start=c(2011,1), end=c(2014,12), frequency = 12)
#ploting time series data
coeff=coefficients(lm(ts_US_Consumer ~ time(ts_US_Consumer)))
# equation of the line : 
eq = paste0("y = ", round(coeff[2],1), "*x ", round(coeff[1],1))
plot(ts_US_Consumer, main=eq)
abline(reg=lm(ts_US_Consumer ~ time(ts_US_Consumer)), col="red")
#...................................
#20. US-Corporate Segment dataset for building ts
#-----------------------------------------------------------------------------
gs_US_Corporate_MS_ds <- global_superstore_Summary_for_MS_data[global_superstore_Summary_for_MS_data$Market.Segment == "US-Corporate",]
#converting to time series data
ts_US_Corporate <- ts(gs_US_Corporate_MS_ds$Total.Profit, start=c(2011,1), end=c(2014,12), frequency = 12)
#ploting time series data
coeff=coefficients(lm(ts_US_Corporate ~ time(ts_US_Corporate)))
# equation of the line : 
eq = paste0("y = ", round(coeff[2],1), "*x ", round(coeff[1],1))
plot(ts_US_Corporate, main=eq)
abline(reg=lm(ts_US_Corporate ~ time(ts_US_Corporate)), col="red")
#...................................
#21. US-Home Office Segment dataset for building ts
#-----------------------------------------------------------------------------
gs_US_Home_Office_MS_ds <- global_superstore_Summary_for_MS_data[global_superstore_Summary_for_MS_data$Market.Segment == "US-Home Office",]
#converting to time series data
ts_gs_US_Home_Office <- ts(gs_US_Home_Office_MS_ds$Total.Profit, start=c(2011,1), end=c(2014,12), frequency = 12)
#ploting time series data
coeff=coefficients(lm(ts_gs_US_Home_Office ~ time(ts_gs_US_Home_Office)))
# equation of the line : 
eq = paste0("y = ", round(coeff[2],1), "*x ", round(coeff[1],1))
plot(ts_gs_US_Home_Office, main=eq)
abline(reg=lm(ts_gs_US_Home_Office ~ time(ts_gs_US_Home_Office)), col="red")
#-----------------------------------------------------------------------------
#.............................................................................
#
# COV of profit
#
#-----------------------------------------------------------------------------
#.............................................................................
global_superstore_Summary_for_MS_ds_cov <- aggregate.data.frame(global_superstore_Summary_for_MS_data$Total.Profit,by=list(global_superstore_Summary_for_MS_data$Market.Segment),FUN = function(x) {sd(x)/mean(x)})
#we will order coefficient of variation of profit by assending order 
#to see the 5 most profitable and consistently profitable segments.
colnames(global_superstore_Summary_for_MS_ds_cov) <- c("Market.Segment","COV")
global_superstore_Summary_for_MS_ds_cov_order <- global_superstore_Summary_for_MS_ds_cov[with(global_superstore_Summary_for_MS_ds_cov,order(COV)),]
#write.csv(global_superstore_Summary_for_MS_ds_cov_order,"global_superstore_Summary_for_MS_ds_cov_order.csv")
global_superstore_Summary_for_MS_ds_cov_order[1:5,]
#.................................
#.................................
#   Market.Segment       COV
#13    EU-Consumer 0.6243052
#4   APAC-Consumer 0.6321323
#16 LATAM-Consumer 0.6614828
#5  APAC-Corporate 0.6980869
#14   EU-Corporate 0.7638072
#.................................
#.................................
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Model building
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Once you arrive at the 2 most profitable segments, the next challenge is to 
#forecast the sales and quantity for the next 6 months. You are supposed to 
#use classical decomposition and auto ARIMA for forecasting. Also, it is advised 
#that you smoothen the data before you perform classical decomposition.
#----------------------------------------------------------
#----------------------------------------------------------
######################################################################################################################################## smothening
############################################ Model building##################################################

####################### EU Consumer Sales Model
gs_EU_Consumer_MS_ds <- (global_superstore_Summary_for_MS_data[global_superstore_Summary_for_MS_data$Market.Segment == "EU-Consumer",])
gs_EU_Consumer_MS_ds$Month <- (rank(gs_EU_Consumer_MS_ds$Order.Month.Year,ties.method= "first"))

#converting to time series data
ts_EU_Consumer_Sales <- ts(gs_EU_Consumer_MS_ds$Total.Sales)

plot(ts_EU_Consumer_Sales) + abline(reg=lm(ts_EU_Consumer_Sales ~time(ts_EU_Consumer_Sales))) # Increasing trend + seasonal


# creating the model using the first 42 rows.
#Then we can test the model on the remaining 6 rows later

plot(ts_EU_Consumer_Sales)
total_timeserEU_Consumer_Sales <- ts_EU_Consumer_Sales
indata_EU_Consumer <- gs_EU_Consumer_MS_ds[1:42,]
ts_indata_EU_Consumer_Sales <- ts(indata_EU_Consumer$Total.Sales)
plot(ts_indata_EU_Consumer_Sales)

plot(ts_indata_EU_Consumer_Sales)

#Smoothing the series - Moving Average Smoothing

w <-1
smoothedseries <- stats::filter(ts_indata_EU_Consumer_Sales, 
                                filter=rep(1/(2*w+1),(2*w+1)), 
                                method='convolution', sides=2)

#Smoothing left end of the time series

diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

#Smoothing right end of the time series

n <- length(ts_indata_EU_Consumer_Sales)
diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}


#Building a model on the smoothed time series using classical decomposition
# Convert the time series to a dataframe 

timevals_in <- (indata_EU_Consumer$Month)
smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
colnames(smootheddf) <- c('Month', 'Sales')
# to check cbind(smootheddf,indata_EU_Consumer$Total.Sales,global_pred)
# building global pattern

lmfit <- lm(Sales ~ sin(0.5*Month) * poly(Month,2) + cos(0.5*Month) * poly(Month,2)
            + Month, data=smootheddf)
summary(lmfit)
## Residual standard error: 4741 on 33 degrees of freedom
## Multiple R-squared:  0.7952,	Adjusted R-squared:  0.7456 

global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)
lines( global_pred, col='red', lwd=2)

# local predictor
local_pred <- ts_indata_EU_Consumer_Sales - ts(global_pred)

#plot.ts(local_pred)
acf(local_pred)      # stationary series or white noise
pacf(local_pred)     # Inside confidence 

adf.test(local_pred,alternative = "stationary") #  p-value = 0.01  which is stationary series 
kpss.test(local_pred)  # p-value = 0.1 which is stationary series

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

outdata <- gs_EU_Consumer_MS_ds[43:48,]
timevals_out <- (outdata$Month)
outdata_Sales <- as.data.frame(outdata$Total.Sales)
global_pred_out <- predict(lmfit,data.frame(Month = timevals_out))


fcast <- as.data.frame(global_pred_out)

MAPE_classical_decomposition <- accuracy(ts(fcast),ts(outdata_Sales))[5]
MAPE_classical_decomposition   ### 34.35913

# Plotting the predictions along with original values

class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(ts_EU_Consumer_Sales, col = "black")
lines(ts(class_dec_pred), col = "blue")

# Predict Next 6 Months Sales
timevals_next <- c(1:6)
global_pred_forcast <- predict(lmfit,data.frame(Month = timevals_next))
as.data.frame(global_pred_forcast)
class_dec_pred_forcast <- c(ts(global_pred),ts(global_pred_out),ts(global_pred_forcast))
plot(ts_EU_Consumer_Sales, col = "black")
lines(ts(class_dec_pred), col = "blue")

## Building Model with ARIMA

armafit <- auto.arima(local_pred)
par(mar=c(1,1,1,1))
tsdiag(armafit)
armafit

#Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)
c(fcast_auto_arima$pred)
MAPE_auto_arima <- accuracy(c(fcast_auto_arima$pred),ts(outdata[,3]))[5]
MAPE_auto_arima

#Lastly, let's plot the predictions along with original values, to




par(mfrow=c(1,1))
#################### End EU Consumer Sales ############################################

################### EU Consumer Quantity Model
gs_EU_Consumer_MS_ds <- (global_superstore_Summary_for_MS_data[global_superstore_Summary_for_MS_data$Market.Segment == "EU-Consumer",])
gs_EU_Consumer_MS_ds$Month <- (rank(gs_EU_Consumer_MS_ds$Order.Month.Year,ties.method= "first"))

#converting to time series data
ts_EU_Consumer_Quantity <- ts(gs_EU_Consumer_MS_ds$Total.Quantity)

plot(ts_EU_Consumer_Quantity) + abline(reg=lm(ts_EU_Consumer_Quantity ~time(ts_EU_Consumer_Quantity))) # Increasing trend + seasonal


# creating the model using the first 42 rows.
#Then we can test the model on the remaining 6 rows later

plot(ts_EU_Consumer_Quantity)
indata_EU_Consumer <- gs_EU_Consumer_MS_ds[1:42,]
ts_indata_EU_Consumer_Quantity<- ts(indata_EU_Consumer$Total.Quantity)
plot(ts_indata_EU_Consumer_Quantity)


#Smoothing the series - Moving Average Smoothing

w <-1
smoothedseries <- stats::filter(ts_indata_EU_Consumer_Quantity, 
                                filter=rep(1/(2*w+1),(2*w+1)), 
                                method='convolution', sides=2)

#Smoothing left end of the time series

diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

#Smoothing right end of the time series

n <- length(ts_indata_EU_Consumer_Quantity)
diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}


#Building a model on the smoothed time series using classical decomposition
# Convert the time series to a dataframe 

timevals_in <- (indata_EU_Consumer$Month)
smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
colnames(smootheddf) <- c('Month', 'Sales')

# building global pattern

lmfit <- lm(Quantity ~ sin(0.5*Month) * poly(Month,2) + cos(0.5*Month) * poly(Month,2)
            + Month, data=smootheddf)
summary(lmfit)
## Residual standard error: 4741 on 33 degrees of freedom
## Multiple R-squared:  0.8927,	Adjusted R-squared:  0.8666 

global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)
lines( global_pred, col='Blue', lwd=2)

# local predictor
local_pred <- ts_indata_EU_Consumer_Quantity - ts(global_pred)

#plot.ts(local_pred)
acf(local_pred)      # stationary series or white noise
pacf(local_pred)     # Inside confidence 

adf.test(local_pred,alternative = "stationary") #  p-value = 0.01  which is stationary series 
kpss.test(local_pred)  # p-value = 0.1 which is stationary series

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

outdata <- gs_EU_Consumer_MS_ds[43:48,]
timevals_out <- (outdata$Month)
outdata_Quantity <- as.data.frame(outdata$Total.Quantity)
global_pred_out <- predict(lmfit,data.frame(Month = timevals_out))

cbind(global_pred_out,outdata_Quantity,fcast)
fcast <- as.data.frame(global_pred_out)

MAPE_classical_decomposition <- accuracy(ts(fcast),ts(outdata_Quantity))[5]
MAPE_classical_decomposition   ### 33.24

# Plotting the predictions along with original values

class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(ts_EU_Consumer_Quantity, col = "black")
lines(ts(class_dec_pred), col = "blue")

# Predict Next 6 Months Sales
timevals_next <- c(1:6)
global_pred_forcast <- predict(lmfit,data.frame(Month = timevals_next))
as.data.frame(global_pred_forcast)
class_dec_pred_forcast <- c(ts(global_pred),ts(global_pred_out),ts(global_pred_forcast))
plot(ts_EU_Consumer_Quantity, col = "black")
lines(ts(class_dec_pred), col = "blue")

## Building Model with ARIMA

armafit <- auto.arima(local_pred)

tsdiag(armafit)
armafit



#Lastly, let's plot the predictions along with original values, to

par(mfrow = c(1,2))
class_dec_pred_forcast <- c(ts(global_pred),ts(global_pred_out),ts(global_pred_forcast))
plot(ts_EU_Consumer_Quantity, col = "black")
lines(ts(class_dec_pred_forcast), col = "blue")


##################################### ENd EU Consumer Quantity #############
#----------------------------------------------------------
#
#Building model : 1.a : APAC-Consumer Segemnt for sales and quantity
#
#----------------------------------------------------------
#----------------------------------------------------------
#----------------------------------------------------------
par(mfrow=c(1,1))

gs_APAC_Consumer_MS_ds$Month <- rank(gs_APAC_Consumer_MS_ds$Order.Month.Year,ties.method= "first")

#Builing time series data for Total Sales
TS_Total.Sales <- ts(gs_APAC_Consumer_MS_ds$Total.Sales,
         start=c(2011,1),
         end=c(2014,12),
         frequency = 12)

#Builing time series data for Total Quantity
TS_Total.Quantity <- ts(gs_APAC_Consumer_MS_ds$Total.Quantity,
         start=c(2011,1),
         end=c(2014,12),
         frequency = 12)

plot(TS_Total.Sales)
plot(TS_Total.Quantity)

#Descriptive Statistics
summary(TS_Total.Sales)
summary(TS_Total.Quantity)

mean.TS_Total.Sales <- mean(TS_Total.Sales)
sd.TS_Total.Sales <- sd(TS_Total.Sales)

mean.TS_Total.Quantity <- mean(TS_Total.Quantity)
sd.TS_Total.Quantity <- sd(TS_Total.Quantity)

plot(TS_Total.Sales, main="APAC Consumer Total Sales")
abline(h=mean.TS_Total.Sales, col="red")
abline(h=c(mean.TS_Total.Sales+sd.TS_Total.Sales,mean.TS_Total.Sales-sd.TS_Total.Sales), col="blue", lty=3)

plot(TS_Total.Quantity, main="APAC Consumer Total Quantity")
abline(h=mean.TS_Total.Quantity, col="red")
abline(h=c(mean.TS_Total.Quantity+sd.TS_Total.Quantity,mean.TS_Total.Quantity-sd.TS_Total.Quantity), col="blue", lty=3)

#in this we can see that the plot is stationary since the most of the data in between sd and mean

#stationarity
#A time series is stationary if the mean of the series over some 
#reasonable range does not change when different endpoints for that range are chosen. 


#monthly boxplot
boxplot(TS_Total.Sales ~ cycle(TS_Total.Sales), main="APAC Consumer Total Sales")
abline(h=mean.TS_Total.Sales, col="red")
abline(h=c(mean.TS_Total.Sales+sd.TS_Total.Sales,mean.TS_Total.Sales-sd.TS_Total.Sales), col="blue", lty=3)

boxplot(TS_Total.Quantity ~ cycle(TS_Total.Quantity), main="APAC Consumer Total Quantity")
abline(h=mean.TS_Total.Quantity, col="red")
abline(h=c(mean.TS_Total.Quantity+sd.TS_Total.Quantity,mean.TS_Total.Quantity-sd.TS_Total.Quantity), col="blue", lty=3)


#decomposition and correlogram
#decomposition into seasonal, trend, and random components

library(forecast)

#we can see the linear treand of increasing data
#apply the lm and see
#------------------------------------------------------------------
#linear trend
#------------------------------------------------------------------
#Finding linear trend for Total Sales
l.trend.Sales <- lm(TS_Total.Sales ~ time(TS_Total.Sales))

plot(TS_Total.Sales)
abline(l.trend.Sales, col="red")

#Finding linear trend for Total Quantity
l.trend.Quantity <- lm(TS_Total.Quantity ~ time(TS_Total.Quantity))

plot(TS_Total.Quantity)
abline(l.trend.Quantity, col="red")
#-------------------------------------------------------------------
#
#install.packages('tseries') 
library(tseries)
#
adf.test(TS_Total.Sales, alternative="stationary", k=0)
#
adf.test(TS_Total.Quantity, alternative="stationary", k=0)

par(mfrow=c(2,2))
acf(TS_Total.Sales)
pacf(TS_Total.Sales)

acf(TS_Total.Quantity)
pacf(TS_Total.Quantity)

#Clearly, ACF plot cuts off after the first lag. Hence, 
#we understood that value of p should be 0 as the ACF is 
#the curve getting a cut off. While value of q should be 1 or 2. 
#After a few iterations, we found that (0,1,1) as (p,d,q) 
#comes out to be the combination with least AIC and BIC.

#Arima fit for Total Sales
TS_Total.Sales.fit <- arima(TS_Total.Sales, c(0, 1, 1),seasonal = list(order = c(0, 1, 1), period = 12))
TS_Total.Sales.fit

#Arima fit for Total Sales
TS_Total.Quantity.fit <- arima(TS_Total.Quantity, c(0, 1, 1),seasonal = list(order = c(0, 1, 1), period = 12))
TS_Total.Quantity.fit

par(mfrow=c(1,1))
#HoltWinters Smoothing : Total Sales
#-------------------------------------------
TS_Total.Sales.fit.hw <- HoltWinters(TS_Total.Sales)
plot(TS_Total.Sales.fit.hw, main="Holt-Winters filtering for Sales")

TS_Total.Quantity.hw <- HoltWinters(TS_Total.Quantity)
plot(TS_Total.Quantity.hw, main="Holt-Winters filtering for Quantity")

#sum of square error
TS_Total.Sales.fit.hw$SSE
TS_Total.Quantity.hw$SSE

#-------------------------------------------
#Exponential smoothing forecasts
#-------------------------------------------
#install.packages("fpp")
library(fpp)

TS_Total.Sales.fit.esf <- hw(TS_Total.Sales,seasonal="additive")
#plot both the original time series and smoothed time series
plot.ts(TS_Total.Sales, main="Exponential smoothing forecasts for Total Sales")
lines(fitted(TS_Total.Sales.fit.esf), col="red")

TS_Total.Quantity.fit.esf <- hw(TS_Total.Quantity,seasonal="additive")
#plot both the original time series and smoothed time series
plot.ts(TS_Total.Quantity, main="Exponential smoothing forecasts for Total Quantity")
lines(fitted(TS_Total.Quantity.fit.esf), col="red")
#------------------------------------------
#anova(fit) produces an anova (analysis of variance) table, if one exists, for any object:
anova(l.trend.Sales)
anova(l.trend.Quantity)

#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
# Decomposition : decompose uses the moving average
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
#par(mfrow=c(1,1))
TS_Total.Sales.dec <- decompose(TS_Total.Sales)
plot(TS_Total.Sales.dec)

TS_Total.Quantity.dec <- decompose(TS_Total.Quantity)
plot(TS_Total.Quantity.dec)
#in the output trend and random componet do not cover the start and end point of entire range

#stl uses the lowested smothes
TS_Total.Sales.STL <- stl(TS_Total.Sales, "periodic")
plot(TS_Total.Sales.STL, main="stl : lowest smooth for Sales")

#stl uses the lowested smothes
TS_Total.Quantity.STL <- stl(TS_Total.Quantity, "periodic")
plot(TS_Total.Quantity.STL, main="stl : lowest smooth for Quantity")

#-------------------------------------------------------------------
#acf
#-------------------------------------------------------------------
#correlogram
acf(TS_Total.Sales)
acf(TS_Total.Sales.STL$time.series[,3], main="Random")
Acf(TS_Total.Sales.STL$time.series[,3], main="Random")

#...................................................
acf(TS_Total.Quantity)
#in this above chart dot line indicate significante correlation
#below the blue line data observation are not correlated

#This function produces the autocorrelation plot and stores all 
#information related to the autocorrelation function.
acf(TS_Total.Quantity.STL$time.series[,3], main="Random")

#same function from forecast package
Acf(TS_Total.Quantity.STL$time.series[,3], main="Random")

#in the above chart lag is in months
#-------------------------------------------------------
#corelation : TS_Total.Sales, TS_Total.Quantity for APAC-Consumer
#-------------------------------------------------------
#removed seasonal and tread using decompose, we can see the random correlation
Ccf(decompose(TS_Total.Sales)$random,
    decompose(TS_Total.Quantity)$random,
    lag.max = 48, na.action = na.omit, main="Random correlation - Sales and Quantity ")
#-------------------------------------------------------

#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
#Forecast : APAC-Consumer sales and quantity
#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
#Mean Forecast
APAC_Consumer.mean.forecast.Sales <- meanf(TS_Total.Sales)
plot(APAC_Consumer.mean.forecast.Sales, main="Forcasts from Mean - APAC_Consumer Sales")

APAC_Consumer.mean.forecast.Quantity <- meanf(TS_Total.Quantity)
plot(APAC_Consumer.mean.forecast.Quantity, main="Forcasts from Mean - APAC_Consumer Quantity")

#for stationary data forecasting mean will be good approach

#Navi Forecast
APAC_Consumer.navi.forecast.Sales <- naive(TS_Total.Sales)
plot(APAC_Consumer.navi.forecast.Sales, type = "l", main="Forcasts from Navi - APAC_Consumer Sales")

APAC_Consumer.navi.forecast.Quantity <- naive(TS_Total.Quantity)
plot(APAC_Consumer.navi.forecast.Quantity, type = "l", main="Forcasts from Navi - APAC_Consumer Quantity")

#Drift Method
APAC_Consumer.drift.Sales <- rwf(TS_Total.Sales, drift = T)
plot(APAC_Consumer.drift.Sales, main="Forecasts from Random walk with drift - APAC_Consumer Sales")

APAC_Consumer.drift.Quantity <- rwf(TS_Total.Quantity, drift = T)
plot(APAC_Consumer.drift.Quantity, main="Forecasts from Random walk with drift - APAC_Consumer Quantity")

#seasonal Naive
APAC_Consumer.snaive.forecast.Sales <- snaive(TS_Total.Sales)
plot(APAC_Consumer.snaive.forecast.Sales, type='l',main="Forecasts from Seasonal naive method - APAC_Consumer Sales")

APAC_Consumer.snaive.forecast.Quantity <- snaive(TS_Total.Quantity)
plot(APAC_Consumer.snaive.forecast.Quantity, type='l',main="Forecasts from Seasonal naive method - APAC_Consumer Quantity")

#ARIMA Model
TS_Total.Sales.arima.forecast <- auto.arima(TS_Total.Sales)
TS_Total.Sales.pred <- forecast(TS_Total.Sales.arima.forecast, h=6) #6 months in future
TS_Total.Sales.pred
plot(TS_Total.Sales.pred)

TS_Total.Quantity.arima.forecast <- auto.arima(TS_Total.Quantity)
TS_Total.Quantity.pred <- forecast(TS_Total.Quantity.arima.forecast, h=6) #6 months in future
TS_Total.Quantity.pred
plot(TS_Total.Quantity.pred)
#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
#Model evaluation: APAC_Consumer Sales and Quantity
#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------

#  Once you come up with a satisfactory model, the next step would be to forecast the 
#sales/demand for next 6 months using this model. To test the accuracy of your forecast, 
#you must initially separate out the last 6 months values from your dataset, after aggregating 
#the transaction level data into the monthly data. Then check your 6 months forecast using 
#the out-of-sample figures. You can use MAPE for this.

#Validation : APAC_Consumer Sales
#-----------------------------------------------
TS_Total.Sales.Training <- window(TS_Total.Sales, 
                   start=c(2011,1), 
                   end=c(2013,12))

TS_Total.Sales.Test <- window(TS_Total.Sales, start=c(2014,1))


#Fit ARIMA
TS_Total.Sales.arima.forecast <- auto.arima(TS_Total.Sales.Training, stepwise = F)

#Estimate the test set
TS_Total.Sales.pred.test <- forecast(TS_Total.Sales.arima.forecast, h=6)
plot(TS_Total.Sales.pred.test)
lines(TS_Total.Sales.Test)
#Validation : APAC_Consumer Quantity
#-----------------------------------------------
TS_Total.Quantity.Training <- window(TS_Total.Quantity, 
                                  start=c(2011,1), 
                                  end=c(2013,12))

TS_Total.Quantity.Test <- window(TS_Total.Quantity, start=c(2014,1))


#Fit ARIMA
TS_Total.Quantity.arima.forecast <- auto.arima(TS_Total.Quantity.Training, stepwise = F)

#Estimate the test set
TS_Total.Quantity.pred.test <- forecast(TS_Total.Quantity.arima.forecast, h=6)
plot(TS_Total.Quantity.pred.test)
lines(TS_Total.Quantity.Test)
#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
#




