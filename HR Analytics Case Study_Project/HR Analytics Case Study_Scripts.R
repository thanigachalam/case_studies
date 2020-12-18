#-----------------------------------------------------------------------------------
#Required R Packages
#-----------------------------------------------------------------------------------
required_packages <- c("dplyr","tidyr","lubridate","stringr","ggplot2","gridExtra","corrplot","caTools","MASS","car",
                       "caret","pROC","ModelMetrics",'caret',"RcppRoll","ddalpha","dimRed","gower","lava")
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
library(corrgram)
library(psych)

#-----------------------------------------------------------------------------------
# Importing datasets and creating local variables
#-----------------------------------------------------------------------------------
employee_survey_data <- read.csv("employee_survey_data.csv")
general_data <- read.csv("general_data.csv")
in_time <- read.csv("in_time.csv")
manager_survey_data <- read.csv("manager_survey_data.csv")
out_time <- read.csv("out_time.csv")

#-----------------------------------------------------------------------------------
#in_time, out_time - dataset is having X column which is refered 
#has EmployeeID to join with other employee datasets
names(in_time)[1] <- 'EmployeeID'
names(out_time)[1] <- 'EmployeeID'
#-----------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------
#validating Primary Key column for all the dataset.
#-----------------------------------------------------------------------------------
#length(unique(employee_survey_data$EmployeeID)) #4410
#length(unique(general_data$EmployeeID)) #4410
#length(unique(in_time$EmployeeID)) #4410
#length(unique(manager_survey_data$EmployeeID)) #4410
#length(unique(out_time$EmployeeID)) #4410

#we observed that all the dataset has EmployeeID column value and all are unique
#we can join all the dataset with EmployeeID column

#-----------------------------------------------------------------------------------
#validating for Missing Values or NA
#-----------------------------------------------------------------------------------
general_data_na_validation <- as.data.frame(sapply(general_data, function(x) sum(is.na(x))))
names(general_data_na_validation)[1] <- 'na_count'
names <- rownames(general_data_na_validation)
rownames(general_data_na_validation) <- NULL
general_data_na_validation <- cbind(names,general_data_na_validation)
theme_set(theme_pubr())
rm(names)
# general dataset na validation
#-----------------------------------
general_data_na_validation_chart <- ggplot(data = subset(general_data_na_validation, na_count > 0), aes(x = names, y = (na_count/4410)*100)) + 
  geom_bar(stat = "identity") +
  labs(title="General Dataset NA data validation"
       , x = "Column Name" 
       , y = "%") + theme_pubclean()
general_data_na_validation_chart <- general_data_na_validation_chart + geom_text(aes(label = sprintf("%.02f",(na_count/4410)*100)), vjust = -0.3)
general_data_na_validation_chart + theme(axis.text.x = element_text(angle = 90, hjust = 1))

rm(general_data_na_validation)
rm(general_data_na_validation_chart)
#-----------------------------------
#employee survey dataset
#-----------------------------------
employee_survey_data_na_validation <- as.data.frame(sapply(employee_survey_data, function(x) sum(is.na(x))))
names(employee_survey_data_na_validation)[1] <- 'na_count'
names <- rownames(employee_survey_data_na_validation)
rownames(employee_survey_data_na_validation) <- NULL
employee_survey_data_na_validation <- cbind(names,employee_survey_data_na_validation)

employee_survey_data_na_validation_chart <- ggplot(data = subset(employee_survey_data_na_validation, na_count > 0), aes(x = names, y = (na_count/4410)*100)) + 
  geom_bar(stat = "identity") +
  labs(title="Employee Survey Dataset NA data validation"
       , x = "Column Name" 
       , y = "%") + theme_pubclean()
employee_survey_data_na_validation_chart <- employee_survey_data_na_validation_chart + geom_text(aes(label = sprintf("%.02f",(na_count/4410)*100)), vjust = -0.3)
employee_survey_data_na_validation_chart + theme(axis.text.x = element_text(angle = 90, hjust = 1))

rm(names)
rm(employee_survey_data_na_validation)
rm(employee_survey_data_na_validation_chart)
#-----------------------------------
#manager survey dataset
#-----------------------------------
manager_survey_data_na_validation <- as.data.frame(sapply(manager_survey_data, function(x) sum(is.na(x))))
names(manager_survey_data_na_validation)[1] <- 'na_count'
names <- rownames(manager_survey_data_na_validation)
rownames(manager_survey_data_na_validation) <- NULL
manager_survey_data_na_validation <- cbind(names,manager_survey_data_na_validation)

#No NA records in manager server dataset.
rm(manager_survey_data_na_validation)
rm(names)
#-----------------------------------
#in time dataset
#-----------------------------------
in_time_data_na_validation <- as.data.frame(sapply(in_time, function(x) sum(is.na(x))))
names(in_time_data_na_validation)[1] <- 'na_count'
names <- rownames(in_time_data_na_validation)
rownames(in_time_data_na_validation) <- NULL
in_time_data_na_validation <- cbind(names,in_time_data_na_validation)
#add percentage column
in_time_data_na_validation$percentage <- round((in_time_data_na_validation$na_count/4410)*100)
#remove 100 % NA column, which will not make any sense.
in_time_data_na_validation <- subset(in_time_data_na_validation, percentage != 100)

unique(in_time_data_na_validation$percentage)
summary(as.factor(in_time_data_na_validation$percentage))

#Maximum we can see 5 to 6 percentage data in every column is having NA.
# I assume that this will be leave taken by employees.
# we can consider this data has a leave take by employees. 
rm(in_time_data_na_validation)
rm(names)
#-----------------------------------
#out time dataset
#-----------------------------------
out_time_data_na_validation <- as.data.frame(sapply(out_time, function(x) sum(is.na(x))))
names(out_time_data_na_validation)[1] <- 'na_count'
names <- rownames(out_time_data_na_validation)
rownames(out_time_data_na_validation) <- NULL
out_time_data_na_validation <- cbind(names,out_time_data_na_validation)

#add percentage column
out_time_data_na_validation$percentage <- round((out_time_data_na_validation$na_count/4410)*100)
#remove 100 % NA column, which will not make any sense.
out_time_data_na_validation <- subset(out_time_data_na_validation, percentage != 100)

unique(out_time_data_na_validation$percentage)
summary(as.factor(out_time_data_na_validation$percentage))

#This is same as in time dataset, same date we can see the missing data NA
#so we will be ignoring these data.
rm(out_time_data_na_validation)
rm(names)
#-----------------------------------------------------------------------------------
# creating derive tables based on in time and out time data
#-----------------------------------------------------------------------------------
#creating copy of the dataset intime and out time.
in_time_cp <- in_time
out_time_cp <- out_time

#removing column which is having same data for complete column, which will not give any insights
in_time_cp<- in_time_cp[vapply(in_time_cp, function(columnname) length(unique(columnname))>1, logical(1L))]
out_time_cp<- out_time_cp[vapply(out_time_cp, function(columnname) length(unique(columnname))>1, logical(1L))]

#below listed column has been removed from the analysis for in and out time,
#since these columns only contain NA values.
#"X2015.01.01" "X2015.01.14" "X2015.01.26" "X2015.03.05" "X2015.05.01" "X2015.07.17"
#"X2015.09.17" "X2015.10.02" "X2015.11.09" "X2015.11.10" "X2015.11.11" "X2015.12.25"

emp_hours_worked<- data.frame("EmployeeID"= in_time_cp$EmployeeID)

#formating date column to ensure all the columns date values in same formate (in_time, out_time)
#creating calculated column worked hour (intime - out time)
for(column_number in 2:ncol(in_time_cp)) {
  in_time_cp[,column_number]<- as.POSIXct(in_time_cp[,column_number], "%Y-%m-%d %H:%M:%S")
  out_time_cp[,column_number]<- as.POSIXct(out_time_cp[,column_number], "%Y-%m-%d %H:%M:%S")
  emp_hours_worked[,column_number]<- round(out_time_cp[,column_number]-in_time_cp[,column_number],1)
  colnames(emp_hours_worked)[column_number]<- colnames(out_time_cp)[column_number]
}

#remove variable
rm(column_number)

# creating derive columns date and month number
library(tidyr)
emp_hours_worked <- gather(emp_hours_worked, key = "Date", value = "logged_hours", 2:ncol(emp_hours_worked))

emp_hours_worked$Date <- substr(emp_hours_worked$Date, 2, 1000)
emp_hours_worked$Date <- as.Date(emp_hours_worked$Date, "%Y.%m.%d")
emp_hours_worked$month <- format(emp_hours_worked$Date,"%m")

#-----------------------------------------------------------------------------
#month wise working hours
#-----------------------------------------------------------------------------
library(reshape)
library(plyr)
detach(package:plyr)
#converting month number to month name
#Removing the data column
emp_hours_worked$Date <- NULL
#converting EmployeeID as a factor
emp_hours_worked$EmployeeID <- as.factor(emp_hours_worked$EmployeeID)
#using melt command to get the monthwise time entry data
#emp_hours_worked_melt <- melt(emp_hours_worked, id=c("EmployeeID","month"), na.rm=TRUE)

#hours worked by month - employee monthly time entry
#hours_worked_by_month <- cast(emp_hours_worked_melt,EmployeeID ~ month, sum)

#rm(emp_hours_worked_melt)
#------------------------------------------------------------------------------------
# employee wise summary of logged hours 
#------------------------------------------------------------------------------------

#na values in logged hours consider as a leave taken
library(dplyr)
library(tidyr)
emp_hours_worked_summary <-emp_hours_worked %>% group_by(EmployeeID) %>% 
  summarise(Total.logged.hours = sum(logged_hours, na.rm = TRUE),
            Average.logged.hours = round(mean(logged_hours, na.rm = TRUE),2),
            Total.leaves.taken = sum(is.na(logged_hours)),
            Total.excess.logged = sum(logged_hours - 8, na.rm = TRUE),
            Average.excess.logged = round(mean(logged_hours - 8, na.rm = TRUE),2))

rm(emp_hours_worked)
#------------------------------------------------------------------------------------
# Creating complete dataset by merging general_data, emp_hours_worked_summary
# employee_survey_data, manager_survey_data 
#------------------------------------------------------------------------------------
employee_full_dataset <- merge(x = general_data, y = emp_hours_worked_summary, by = "EmployeeID", all = TRUE)
employee_full_dataset <- merge(x = employee_full_dataset, y = employee_survey_data, by = "EmployeeID", all = TRUE)
employee_full_dataset <- merge(x = employee_full_dataset, y = manager_survey_data, by = "EmployeeID", all = TRUE)

rm(emp_hours_worked_summary)
#------------------------------------------------------------------------------------
# data cleanup activities
#converting all the character columns converting to upper case. to avoid any case issue.
#------------------------------------------------------------------------------------

employee_full_dataset$Attrition <- toupper(employee_full_dataset$Attrition)
employee_full_dataset$BusinessTravel <- toupper(employee_full_dataset$BusinessTravel)
employee_full_dataset$Department <- toupper(employee_full_dataset$Department)
employee_full_dataset$EducationField <- toupper(employee_full_dataset$EducationField)
employee_full_dataset$Gender <- toupper(employee_full_dataset$Gender)
employee_full_dataset$JobRole <- toupper(employee_full_dataset$JobRole)
employee_full_dataset$MaritalStatus <- toupper(employee_full_dataset$MaritalStatus)
employee_full_dataset$Over18 <- toupper(employee_full_dataset$Over18)

#------------------------------------------------------------------------------------
library(dplyr)

# Creating Age_range variable
employee_full_dataset %>% 
  plyr::mutate(Age_range =
                 dplyr::case_when(Age >= 18 &  Age < 20  ~ "18-20",
                                  Age >= 20 &  Age < 30  ~ "20-30",
                                  Age >= 30 &  Age < 40  ~ "30-40",
                                  Age >= 40 &  Age < 50  ~ "40-50",
                                  Age >= 50 &  Age < 60  ~ "50-60",
                                  Age >= 60  ~ "60+",
                                  TRUE                     ~ "undetermined"
                 )) -> employee_full_dataset

# Creating DistanceFromHome_range variable
employee_full_dataset %>% 
  plyr::mutate(DistanceFromHome_range =
                 dplyr::case_when(DistanceFromHome >= 0 &  DistanceFromHome < 5  ~ "0-5",
                                  DistanceFromHome >= 5 &  DistanceFromHome < 10  ~ "5-10",
                                  DistanceFromHome >= 10 &  DistanceFromHome < 15  ~ "10-15",
                                  DistanceFromHome >= 15 &  DistanceFromHome < 20  ~ "15-20",
                                  DistanceFromHome >= 20 &  DistanceFromHome < 25  ~ "20-25",
                                  DistanceFromHome >= 25  ~ "25+",
                                  TRUE                     ~ "undetermined"
                 )) -> employee_full_dataset

# Creating MonthlyIncome_Range variable
employee_full_dataset %>% 
  plyr::mutate(MonthlyIncome_Range =
                 dplyr::case_when(MonthlyIncome >= 10000 &  MonthlyIncome < 20000  ~ "10000-20000",
                                  MonthlyIncome >= 20000 &  MonthlyIncome < 30000  ~ "20000-30000",
                                  MonthlyIncome >= 30000 &  MonthlyIncome < 40000  ~ "30000-40000",
                                  MonthlyIncome >= 40000 &  MonthlyIncome < 50000  ~ "40000-50000",
                                  MonthlyIncome >= 50000 &  MonthlyIncome < 60000  ~ "50000-60000",
                                  MonthlyIncome >= 60000 &  MonthlyIncome < 70000  ~ "60000-70000",
                                  MonthlyIncome >= 70000 &  MonthlyIncome < 80000  ~ "70000-80000",
                                  MonthlyIncome >= 80000 &  MonthlyIncome < 90000  ~ "80000-90000",
                                  MonthlyIncome >= 90000  ~ "90000+",
                                  TRUE                     ~ "undetermined"
                 )) -> employee_full_dataset

# Creating PercentSalaryHike_range variable
employee_full_dataset %>% 
  plyr::mutate(PercentSalaryHike_range =
                 dplyr::case_when(PercentSalaryHike >= 10 &  PercentSalaryHike < 15  ~ "10-15",
                                  PercentSalaryHike >= 15 &  PercentSalaryHike < 20  ~ "15-20",
                                  PercentSalaryHike >= 20 &  PercentSalaryHike < 25  ~ "20-25",
                                  PercentSalaryHike >= 25  ~ "25+",
                                  TRUE                     ~ "undetermined"
                 )) -> employee_full_dataset

# Creating TotalWorkingYears_range variable
employee_full_dataset %>% 
  plyr::mutate(TotalWorkingYears_range =
                 dplyr::case_when(TotalWorkingYears >= 0 &  TotalWorkingYears < 5  ~ "0-5",
                                  TotalWorkingYears >= 5 &  TotalWorkingYears < 10  ~ "5-10",
                                  TotalWorkingYears >= 10 &  TotalWorkingYears < 15  ~ "10-15",
                                  TotalWorkingYears >= 15 &  TotalWorkingYears < 20  ~ "15-20",
                                  TotalWorkingYears >= 20 &  TotalWorkingYears < 25  ~ "20-25",
                                  TotalWorkingYears >= 25 &  TotalWorkingYears < 30  ~ "25-30",
                                  TotalWorkingYears >= 30 &  TotalWorkingYears < 35  ~ "30-35",
                                  TotalWorkingYears >= 35 &  TotalWorkingYears < 40  ~ "35-40",
                                  TotalWorkingYears >= 40  ~ "40+",
                                  TRUE                     ~ "undetermined"
                 )) -> employee_full_dataset

# Creating YearsAtCompany_range variable
employee_full_dataset %>% 
  plyr::mutate(YearsAtCompany_range =
                 dplyr::case_when(YearsAtCompany >= 0 &  YearsAtCompany < 5  ~ "0-5",
                                  YearsAtCompany >= 5 &  YearsAtCompany < 10  ~ "5-10",
                                  YearsAtCompany >= 10 &  YearsAtCompany < 15  ~ "10-15",
                                  YearsAtCompany >= 15 &  YearsAtCompany < 20  ~ "15-20",
                                  YearsAtCompany >= 20 &  YearsAtCompany < 25  ~ "20-25",
                                  YearsAtCompany >= 25 &  YearsAtCompany < 30  ~ "25-30",
                                  YearsAtCompany >= 30 &  YearsAtCompany < 35  ~ "30-35",
                                  YearsAtCompany >= 35 &  YearsAtCompany < 40  ~ "35-40",
                                  YearsAtCompany >= 40  ~ "40+",
                                  TRUE                     ~ "undetermined"
                 )) -> employee_full_dataset

# Creating YearsSinceLastPromotion_range variable
employee_full_dataset %>% 
  plyr::mutate(YearsSinceLastPromotion_range =
                 dplyr::case_when(YearsSinceLastPromotion >= 0 &  YearsSinceLastPromotion < 5  ~ "0-5",
                                  YearsSinceLastPromotion >= 5 &  YearsSinceLastPromotion < 10  ~ "5-10",
                                  YearsSinceLastPromotion >= 10 &  YearsSinceLastPromotion < 15  ~ "10-15",
                                  YearsSinceLastPromotion >= 15  ~ "15+",
                                  TRUE                     ~ "undetermined"
                 )) -> employee_full_dataset

# Creating YearsWithCurrManager_range variable
employee_full_dataset %>% 
  plyr::mutate(YearsWithCurrManager_range =
                 dplyr::case_when(YearsWithCurrManager >= 0 &  YearsWithCurrManager < 5  ~ "0-5",
                                  YearsWithCurrManager >= 5 &  YearsWithCurrManager < 10  ~ "5-10",
                                  YearsWithCurrManager >= 10 &  YearsWithCurrManager < 15  ~ "10-15",
                                  YearsWithCurrManager >= 15  ~ "15+",
                                  TRUE                     ~ "undetermined"
                 )) -> employee_full_dataset

# Creating Education variable
employee_full_dataset %>% 
  plyr::mutate(Education =
                 dplyr::case_when(Education == 1 ~ 'Below College',
                                  Education == 2 ~ 'College',
                                  Education == 3  ~ 'Bachelor',
                                  Education == 4  ~ 'Master',
                                  Education == 5  ~ 'Doctor',
                                  TRUE    ~ "undetermined"
                 )) -> employee_full_dataset
employee_full_dataset$Education <- toupper(employee_full_dataset$Education)

# Creating EnvironmentSatisfaction variable
employee_full_dataset %>% 
  plyr::mutate(EnvironmentSatisfaction =
                 dplyr::case_when(EnvironmentSatisfaction == 1 ~ 'Low',
                                  EnvironmentSatisfaction == 2 ~ 'Medium',
                                  EnvironmentSatisfaction == 3  ~ 'High',
                                  EnvironmentSatisfaction == 4  ~ 'Very High',
                                  TRUE    ~ "undetermined"
                 )) -> employee_full_dataset
employee_full_dataset$EnvironmentSatisfaction <- toupper(employee_full_dataset$EnvironmentSatisfaction)

# Creating JobInvolvement variable
employee_full_dataset %>% 
  plyr::mutate(JobInvolvement =
                 dplyr::case_when(JobInvolvement == 1 ~ 'Low',
                                  JobInvolvement == 2 ~ 'Medium',
                                  JobInvolvement == 3  ~ 'High',
                                  JobInvolvement == 4  ~ 'Very High',
                                  TRUE    ~ "undetermined"
                 )) -> employee_full_dataset
employee_full_dataset$JobInvolvement <- toupper(employee_full_dataset$JobInvolvement)

# Creating JobSatisfaction variable
employee_full_dataset %>% 
  plyr::mutate(JobSatisfaction =
                 dplyr::case_when(JobSatisfaction == 1 ~ 'Low',
                                  JobSatisfaction == 2 ~ 'Medium',
                                  JobSatisfaction == 3  ~ 'High',
                                  JobSatisfaction == 4  ~ 'Very High',
                                  TRUE    ~ "undetermined"
                 )) -> employee_full_dataset
employee_full_dataset$JobSatisfaction <- toupper(employee_full_dataset$JobSatisfaction)

# Creating PerformanceRating variable
employee_full_dataset %>% 
  plyr::mutate(PerformanceRating =
                 dplyr::case_when(PerformanceRating == 1 ~ 'Low',
                                  PerformanceRating == 2 ~ 'Good',
                                  PerformanceRating == 3  ~ 'Excellent',
                                  PerformanceRating == 4  ~ 'Outstanding',
                                  TRUE    ~ "undetermined"
                 )) -> employee_full_dataset
employee_full_dataset$PerformanceRating <- toupper(employee_full_dataset$PerformanceRating)


# Creating WorkLifeBalance variable
employee_full_dataset %>% 
  plyr::mutate(WorkLifeBalance =
                 dplyr::case_when(WorkLifeBalance == 1 ~ 'Bad',
                                  WorkLifeBalance == 2 ~ 'Good',
                                  WorkLifeBalance == 3  ~ 'Better',
                                  WorkLifeBalance == 4  ~ 'Best',
                                  TRUE    ~ "undetermined"
                 )) -> employee_full_dataset
employee_full_dataset$WorkLifeBalance <- toupper(employee_full_dataset$WorkLifeBalance)

#--------------------------------------------------------------------------------
# Converting to factor type for categorical variables.
employee_full_dataset$Attrition <- as.factor(employee_full_dataset$Attrition)
employee_full_dataset$BusinessTravel <- as.factor(employee_full_dataset$BusinessTravel)
employee_full_dataset$Department <- as.factor(employee_full_dataset$Department)
employee_full_dataset$Education <- as.factor(employee_full_dataset$Education)
employee_full_dataset$EducationField <- as.factor(employee_full_dataset$EducationField)
employee_full_dataset$Gender <- as.factor(employee_full_dataset$Gender)
employee_full_dataset$JobLevel <- as.factor(employee_full_dataset$JobLevel)
employee_full_dataset$JobRole <- as.factor(employee_full_dataset$JobRole)
employee_full_dataset$MaritalStatus <- as.factor(employee_full_dataset$MaritalStatus)
employee_full_dataset$NumCompaniesWorked <- as.factor(employee_full_dataset$NumCompaniesWorked)
employee_full_dataset$StockOptionLevel <- as.factor(employee_full_dataset$StockOptionLevel)
employee_full_dataset$TrainingTimesLastYear <- as.factor(employee_full_dataset$TrainingTimesLastYear)
employee_full_dataset$EnvironmentSatisfaction <- as.factor(employee_full_dataset$EnvironmentSatisfaction)
employee_full_dataset$JobSatisfaction <- as.factor(employee_full_dataset$JobSatisfaction)
employee_full_dataset$WorkLifeBalance <- as.factor(employee_full_dataset$WorkLifeBalance)
employee_full_dataset$JobInvolvement <- as.factor(employee_full_dataset$JobInvolvement)
employee_full_dataset$PerformanceRating <- as.factor(employee_full_dataset$PerformanceRating)
employee_full_dataset$Age_range <- as.factor(employee_full_dataset$Age_range)
employee_full_dataset$MonthlyIncome_Range <- as.factor(employee_full_dataset$MonthlyIncome_Range)
employee_full_dataset$PercentSalaryHike_range <- as.factor(employee_full_dataset$PercentSalaryHike_range)
employee_full_dataset$TotalWorkingYears_range <- as.factor(employee_full_dataset$TotalWorkingYears_range)
employee_full_dataset$YearsAtCompany_range <- as.factor(employee_full_dataset$YearsAtCompany_range)
employee_full_dataset$YearsSinceLastPromotion_range <- as.factor(employee_full_dataset$YearsSinceLastPromotion_range)
employee_full_dataset$YearsWithCurrManager_range <- as.factor(employee_full_dataset$YearsWithCurrManager_range)
employee_full_dataset$DistanceFromHome_range <- as.factor(employee_full_dataset$DistanceFromHome_range)
#------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------
# EDA Analysis on the complete data set.
#------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------
#Analysis on JobSatisfaction data
JobSatisfaction_data <- ggplot(data = employee_full_dataset, aes(x = JobSatisfaction))

#JobSatisfaction %
JobSatisfaction_percentage_plot <- JobSatisfaction_data +
  geom_bar(aes(y = (..count..)/sum(..count..)),fill = "#0073C2FF") +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  labs(title="JobSatisfaction %"
       , x = "Job Satisfaction" 
       , y = "Number of Employee") + theme_pubclean()

JobSatisfaction_percentage_plot
#-------------------------------------------------------------------------
#understanding co relation between variables 2 to 8
pairs.panels(employee_full_dataset[2:8])  # select columns 5-12
#..................................................................
col_list <- c("Attrition","NumCompaniesWorked","StockOptionLevel","EnvironmentSatisfaction","WorkLifeBalance","JobInvolvement","PerformanceRating")
#understanding co relation between "BusinessTravel","Department","JobRole","JobLevel","StockOptionLevel","EnvironmentSatisfaction"
pairs.panels(employee_full_dataset[col_list])
#...................................................................
col_list <- c("Attrition","JobInvolvement","Total.logged.hours","Average.logged.hours","Total.leaves.taken","Total.excess.logged","Average.excess.logged")
#understanding co relation between "BusinessTravel","Department","JobRole","JobLevel","StockOptionLevel","EnvironmentSatisfaction"
pairs.panels(employee_full_dataset[col_list])
#-------------------------------------------------------------------
#Analysis on JobInvolvement data
JobInvolvement_data <- ggplot(data = employee_full_dataset, aes(x = JobInvolvement))

#JobInvolvement %
JobInvolvement_percentage_plot <- JobInvolvement_data +
  geom_bar(aes(y = (..count..)/sum(..count..)),fill = "#0073C2FF") +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  labs(title="JobInvolvement % by Performance Rating"
       , x = "Job Involvement" 
       , y = "Number of Employee") + theme_pubclean() + facet_wrap(~PerformanceRating)

JobInvolvement_percentage_plot
#-------------------------------------------------------------------
#JobInvolvement % by Work Life Balance
JobInvolvement_percentage_plot <- JobInvolvement_data +
  geom_bar(aes(y = (..count..)/sum(..count..)),fill = "#0073C2FF") +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  labs(title="JobInvolvement % by Work Life Balance"
       , x = "Job Involvement" 
       , y = "Number of Employee") + theme_pubclean() + facet_wrap(~WorkLifeBalance)

JobInvolvement_percentage_plot
#-------------------------------------------------------------------------
#JobInvolvement % by Percent Salary Hike range
JobInvolvement_percentage_plot <- JobInvolvement_data +
  geom_bar(aes(y = (..count..)/sum(..count..)),fill = "#0073C2FF") +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  labs(title="JobInvolvement % by Percent Salary Hike range"
       , x = "Job Involvement" 
       , y = "Number of Employee") + theme_pubclean() + facet_wrap(~PercentSalaryHike_range)

JobInvolvement_percentage_plot
#-------------------------------------------------------------------------
#JobInvolvement % by Age_range
JobInvolvement_percentage_plot <- JobInvolvement_data +
  geom_bar(aes(y = (..count..)/sum(..count..)),fill = "#0073C2FF") +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  labs(title="JobInvolvement % by Age range"
       , x = "Job Involvement" 
       , y = "Number of Employee") + theme_pubclean() + facet_wrap(~Age_range)

JobInvolvement_percentage_plot
#-------------------------------------------------------------------------
#JobInvolvement % by YearsAtCompany_range and YearsWithCurrManager_range
JobInvolvement_percentage_plot <- JobInvolvement_data +
  geom_bar(aes(y = (..count..)/sum(..count..),fill = YearsWithCurrManager_range),position = "stack") +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  labs(title="JobInvolvement % by YearsAtCompany_range and YearsWithCurrManager_range"
       , x = "Job Involvement" 
       , y = "Number of Employee") + theme_pubclean() + facet_wrap(~YearsAtCompany_range)

JobInvolvement_percentage_plot
#-------------------------------------------------------------------------
#JobInvolvement % by YearsWithCurrManager_range and YearsSinceLastPromotion_range
JobInvolvement_percentage_plot <- JobInvolvement_data +
  geom_bar(aes(y = (..count..)/sum(..count..),fill = employee_full_dataset$YearsSinceLastPromotion_range),position = "stack") +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  labs(title="JobInvolvement % by YearsWithCurrManager_range and YearsSinceLastPromotion_range"
       , x = "Job Involvement" 
       , y = "Number of Employee") + theme_pubclean() + facet_wrap(~YearsWithCurrManager_range)

JobInvolvement_percentage_plot
#-------------------------------------------------------------------------
#JobInvolvement % by YearsWithCurrManager_range and YearsSinceLastPromotion_range
JobInvolvement_percentage_plot <- JobInvolvement_data +
  geom_bar(aes(y = (..count..)/sum(..count..),fill = MonthlyIncome_Range),position = "stack") +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  labs(title="JobInvolvement % by YearsWithCurrManager_range and YearsSinceLastPromotion_range"
       , x = "Job Involvement" 
       , y = "Number of Employee") + theme_pubclean() + facet_wrap(~employee_full_dataset$JobSatisfaction)

JobInvolvement_percentage_plot
#-------------------------------------------------------------------------
#Total Leaves Taken by MonthlyIncome_Range
ggplot(data = employee_full_dataset, aes(x = MonthlyIncome_Range,y = Total.leaves.taken))+
  geom_boxplot() +
  labs(title="Total Leaves Taken by MonthlyIncome_Range"
       , x = "MonthlyIncome_Range" 
       , y = "Total.leaves.taken") + theme_pubclean()
#------------------------------------------------------------------
#Total Leaves Taken by MonthlyIncome_Range and JobSatisfaction
ggplot(data = employee_full_dataset, aes(x = JobSatisfaction,y = Total.leaves.taken))+
  geom_boxplot() +
  labs(title="Total Leaves Taken by MonthlyIncome_Range and JobSatisfaction"
       , x = "MonthlyIncome_Range" 
       , y = "Total.leaves.taken") + theme_pubclean() + facet_wrap(~MonthlyIncome_Range) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
#------------------------------------------------------------------
#Analysis on Attrition data
Attrition_data <- ggplot(data = employee_full_dataset, aes(x = Attrition))

#Attrition %
Attrition_percentage_plot <- Attrition_data +
  geom_bar(aes(y = (..count..)/sum(..count..)),fill = "#0073C2FF") +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  labs(title="Attrition % by Performance Rating"
       , x = "Attrition" 
       , y = "Number of Employee") + theme_pubclean() + facet_wrap(~PerformanceRating)

Attrition_percentage_plot
#---------------------------------------------------------------------
#JobInvolvement % by Work Life Balance
Attrition_percentage_plot <- Attrition_data +
  geom_bar(aes(y = (..count..)/sum(..count..)),fill = "#0073C2FF") +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  labs(title="Attrition % by Work Life Balance"
       , x = "Attrition" 
       , y = "Number of Employee") + theme_pubclean() + facet_wrap(~WorkLifeBalance)

Attrition_percentage_plot
#-------------------------------------------------------------------------
#Attrition % by Percent Salary Hike range
Attrition_percentage_plot <- Attrition_data +
  geom_bar(aes(y = (..count..)/sum(..count..)),fill = "#0073C2FF") +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  labs(title="Attrition % by Percent Salary Hike range"
       , x = "Attrition" 
       , y = "Number of Employee") + theme_pubclean() + facet_wrap(~PercentSalaryHike_range)

Attrition_percentage_plot
#-------------------------------------------------------------------------
#Attrition % by Age_range
Attrition_percentage_plot <- Attrition_data +
  geom_bar(aes(y = (..count..)/sum(..count..)),fill = "#0073C2FF") +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  labs(title="Attrition % by Age range"
       , x = "Attrition" 
       , y = "Number of Employee") + theme_pubclean() + facet_wrap(~Age_range)

Attrition_percentage_plot
#-------------------------------------------------------------------------
#Attrition % by YearsAtCompany_range and YearsWithCurrManager_range
Attrition_percentage_plot <- Attrition_data +
  geom_bar(aes(y = (..count..)/sum(..count..),fill = YearsWithCurrManager_range),position = "stack") +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  labs(title="Attrition % by YearsAtCompany_range and YearsWithCurrManager_range"
       , x = "Attrition" 
       , y = "Number of Employee") + theme_pubclean() + facet_wrap(~YearsAtCompany_range)

Attrition_percentage_plot
#-------------------------------------------------------------------------
#Attrition % by YearsWithCurrManager_range and YearsSinceLastPromotion_range
Attrition_percentage_plot <- Attrition_data +
  geom_bar(aes(y = (..count..)/sum(..count..),fill = employee_full_dataset$YearsSinceLastPromotion_range),position = "stack") +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  labs(title="Attrition % by YearsWithCurrManager_range and YearsSinceLastPromotion_range"
       , x = "Attrition" 
       , y = "Number of Employee") + theme_pubclean() + facet_wrap(~YearsWithCurrManager_range)

Attrition_percentage_plot
#-------------------------------------------------------------------------
ggplot(data = employee_full_dataset, aes(x = MonthlyIncome_Range,y = Total.leaves.taken))+
  geom_boxplot() +
  labs(title="Total Leaves Taken by MonthlyIncome_Range and Attrition"
       , x = "MonthlyIncome_Range" 
       , y = "Total.leaves.taken") + theme_pubclean() + facet_wrap(~Attrition) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
#------------------------------------------------------------------
#------------------------------------------------------------------
#------------------------------------------------------------------
#------------------------------------------------------------------
# Outlier Treatment
#------------------------------------------------------------------
#------------------------------------------------------------------
#------------------------------------------------------------------
#------------------------------------------------------------------
boxplot(employee_full_dataset$MonthlyIncome)
#There are outliers on the upper end in Monthly Income
quantile(employee_full_dataset$MonthlyIncome, seq(0,1,0.01))
#modifying all Monthly incomes above 96% to 186437.6
employee_full_dataset$MonthlyIncome[which(employee_full_dataset$MonthlyIncome>186437.6)]<- 186438
#...............................................................
#records with missing Total Working Years data
boxplot(employee_full_dataset$TotalWorkingYears)
#There are Outliers on the Higher Side
quantile(employee_full_dataset$TotalWorkingYears, seq(0,1,0.01), na.rm = TRUE)
#modifying all Total Working Years above 98% to 32
employee_full_dataset$TotalWorkingYears[which(employee_full_dataset$TotalWorkingYears>32)]<- 32
#...............................................................
#
employee_full_dataset$TrainingTimesLastYear <- as.integer(employee_full_dataset$TrainingTimesLastYear)
boxplot(employee_full_dataset$TrainingTimesLastYear)
#There are outliers on both upper and lower ends of the boxplot
quantile(employee_full_dataset$TrainingTimesLastYear, seq(0,1,0.01))
#we have not found any high range so we will be avoid data clumping
#...............................................................
boxplot(employee_full_dataset$YearsAtCompany)
#There are significant outliers in the higher values of Years At Company
quantile(employee_full_dataset$YearsAtCompany, seq(0,1,0.01))
#Outlier Treatment: We will cap Years At 98% will be updating to 24 for above 98%
employee_full_dataset$YearsAtCompany[which(employee_full_dataset$YearsAtCompany>24)]<- 24
#...............................................................
#
summary(employee_full_dataset$YearsSinceLastPromotion)
boxplot(employee_full_dataset$YearsSinceLastPromotion)
#There are significant outliers on the upper values of YearsSinceLastPromotion
quantile(employee_full_dataset$YearsSinceLastPromotion, seq(0,1,0.01))
#modifying all YearsSinceLastPromotion after 97% updating to 11
employee_full_dataset$YearsSinceLastPromotion[which(employee_full_dataset$YearsSinceLastPromotion >11)]<-11
#...............................................................
#
summary(employee_full_dataset$YearsWithCurrManager)
boxplot(employee_full_dataset$YearsWithCurrManager)
#There are outliers on the higher values of YearsWithCurrManager
quantile(employee_full_dataset$YearsWithCurrManager, seq(0,1,0.01))
#updating all YearsWithCurrManager value above 99% update to 14
employee_full_dataset$YearsWithCurrManager[which(employee_full_dataset$YearsWithCurrManager>14)]<-14
#...............................................................
#
employee_full_dataset$Total.logged.hours<- as.numeric(employee_full_dataset$Total.logged.hours)
boxplot(employee_full_dataset$Total.logged.hours)
#There are significant outliers in the upper region of Tot.logged.hours
quantile(employee_full_dataset$Total.logged.hours, seq(0,1,0.01))
#updating all records for 98%>2589
employee_full_dataset$Total.logged.hours[which(employee_full_dataset$Total.logged.hours>2589)]<-2590

#-------------------------------------------------------------------
#------------------------------------------------------------------
#------------------------------------------------------------------------------------
# Removing columns 
#------------------------------------------------------------------------------------
#------------------------------------------------------------------

#Removed the following columns, not considered for analysis.
#EmployeeCount - looks like all are 1, we can remove this column -- remove column all the value is 1
#Over18 - All the employees in the dataset are over 18 so we can remove this column
#StandardHouras - All the values in this column is having only 8 and we can remove this column
employee_full_dataset$EmployeeCount <- NULL
employee_full_dataset$Over18 <- NULL
employee_full_dataset$StandardHours <- NULL

#------------------------------------------------------------------
employee_full_dataset$Age_range <- NULL
employee_full_dataset$DistanceFromHome_range <- NULL
employee_full_dataset$MonthlyIncome_Range <- NULL
employee_full_dataset$PercentSalaryHike_range <- NULL
employee_full_dataset$TotalWorkingYears_range <- NULL
employee_full_dataset$YearsAtCompany_range <- NULL
employee_full_dataset$YearsSinceLastPromotion_range <- NULL
employee_full_dataset$YearsWithCurrManager_range <- NULL
#------------------------------------------------------------------------------------
#removing the rows with NA values in columns
employee_full_dataset_without_na <- employee_full_dataset[complete.cases(employee_full_dataset),]

main_dataset <- employee_full_dataset_without_na
#------------------------------------------------------------------------------------
#converting categorical variables into dummies
#------------------------------------------------------------------------------------
main_dataset$Attrition <- ifelse(main_dataset$Attrition == "YES", 1,0)

main_dataset$Gender <- ifelse(main_dataset$Gender == "MALE", 1,0)

main_dataset$PerformanceRating <- ifelse(main_dataset$PerformanceRating == "OUTSTANDING", 1,0)

#------------------------------------------------------------------
#------------------------------------------------------------------
#------------------------------------------------------------------
#creating dummy variables
#------------------------------------------------------------------
#------------------------------------------------------------------
#------------------------------------------------------------------
main_dataset$NumCompaniesWorked <- as.numeric(main_dataset$NumCompaniesWorked)
#selecting all the factor columns to covert to dummies
factor_columns <- 
  colnames(main_dataset[sapply(main_dataset, function(x) class(x) == 'factor')])

#creating the dummy variables for all the factor variables         
dummies <- data.frame(sapply(main_dataset[factor_columns], 
                             function(x) data.frame(model.matrix(~x-1,data =main_dataset[factor_columns]))[,-1]))

#removing all the factor columns from the main_dataset
main_dataset[factor_columns] <- NULL
#adding the dummies created to the main data set
main_dataset <- cbind(main_dataset, dummies)
rm(dummies)


#Removing EmployeeID column
main_dataset$EmployeeID <- NULL

str(main_dataset)
main_dataset$Attrition <- as.factor(main_dataset$Attrition)

#Applying the scale function so that all the variables to make one scale
# Scale all numeric columns in a data frame.

performScaling <- TRUE  # Turn on/off for experimentation.

if (performScaling) {
  
  # Loop over each variable
  for (colName in names(main_dataset)) {
    
    # validating if the column contains numeric data.
    if(class(main_dataset[,colName]) == 'integer' | class(main_dataset[,colName]) == 'numeric') {
      
      # Scale this variable (scale() function applies z-scaling).
      main_dataset[,colName] <- scale(main_dataset[,colName])
    }
  }
}
#------------------------------------------------------------------
#including required libraries
library(cowplot)
library("car")
library("MASS")
library(caTools)
library(scales)
library(stats)
library(tidyr)

#------------------------------------------------------------------
#Creating Testing and Training Dataset for out Logistic Model
set.seed(100)

training_index<- sample.split(main_dataset$Attrition, SplitRatio = 0.7)

train<- main_dataset[training_index,]
test<- main_dataset[!training_index,]
#------------------------------------------------------------------
#------------------------------------------------------------------
#------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Model Building
#------------------------------------------------------------------
#------------------------------------------------------------------
#------------------------------------------------------------------
#-------------------------------------------------------------------------------
model_1<- glm(Attrition~., data = train, family = "binomial")
summary(model_1)

#Null deviance: 2704.5  on 3066  degrees of freedom
#Residual deviance: 1979.3  on 3004  degrees of freedom
#AIC: 2105.3

#Executing STEP AIC to eliminate insignificant variables
#?stepAIC
stepAIC(model_1,direction="both", steps = 1)
#-------------------------------------------------------------------------------
#model 2
#------------------------------------------------------------------
#removed :
#Average.logged.hours                  -1.985e+01  1.645e+02  -0.121 0.903946
#Average.excess.logged                  2.124e+01  1.645e+02   0.129 0.897266
#Education.xMASTER                      1.385e-03  6.512e-02   0.021 0.983034 
#JobRole.xMANAGER                      -1.724e-02  7.621e-02  -0.226 0.821020

model_2 <- glm(formula = Attrition ~ Age+DistanceFromHome+Gender+MonthlyIncome+NumCompaniesWorked+PercentSalaryHike+
                 TotalWorkingYears+TrainingTimesLastYear+YearsAtCompany+YearsSinceLastPromotion+
                 YearsWithCurrManager+Total.logged.hours+Total.leaves.taken+
                 Total.excess.logged+PerformanceRating+
                 BusinessTravel.xTRAVEL_FREQUENTLY+BusinessTravel.xTRAVEL_RARELY+
                 Department.xRESEARCH...DEVELOPMENT+Department.xSALES+Education.xBELOW.COLLEGE+
                 Education.xCOLLEGE+Education.xDOCTOR+EducationField.xLIFE.SCIENCES+
                 EducationField.xMARKETING+EducationField.xMEDICAL+EducationField.xOTHER+
                 EducationField.xTECHNICAL.DEGREE+JobLevel.x2+JobLevel.x3+JobLevel.x4+JobLevel.x5+
                 JobRole.xHUMAN.RESOURCES+JobRole.xLABORATORY.TECHNICIAN+
                 JobRole.xMANUFACTURING.DIRECTOR+JobRole.xRESEARCH.DIRECTOR+
                 JobRole.xRESEARCH.SCIENTIST+JobRole.xSALES.EXECUTIVE+JobRole.xSALES.REPRESENTATIVE+
                 MaritalStatus.xMARRIED+MaritalStatus.xSINGLE+StockOptionLevel.x1+StockOptionLevel.x2+
                 StockOptionLevel.x3+EnvironmentSatisfaction.xLOW+EnvironmentSatisfaction.xMEDIUM+
                 EnvironmentSatisfaction.xUNDETERMINED+EnvironmentSatisfaction.xVERY.HIGH+
                 JobSatisfaction.xLOW+JobSatisfaction.xMEDIUM+JobSatisfaction.xUNDETERMINED+
                 JobSatisfaction.xVERY.HIGH+WorkLifeBalance.xBEST+WorkLifeBalance.xBETTER+
                 WorkLifeBalance.xGOOD+WorkLifeBalance.xUNDETERMINED+JobInvolvement.xLOW+
                 JobInvolvement.xMEDIUM+JobInvolvement.xVERY.HIGH, family = "binomial", 
               data = train)

summary(model_2)

#Null deviance: 2704.5  on 3066  degrees of freedom
#Residual deviance: 1979.8  on 3008  degrees of freedom
#AIC: 2097.8

#------------------------------------------------------------------
#model 3
#------------------------------------------------------------------
#removed
#JobLevel.x3                           -0.017675   0.065212  -0.271  0.78636
#EnvironmentSatisfaction.xMEDIUM       -0.024038   0.069354  -0.347  0.72889
#JobSatisfaction.xMEDIUM                0.016878   0.066404   0.254  0.79937

model_3 <- glm(formula = Attrition ~ Age + DistanceFromHome + Gender + MonthlyIncome + 
                 NumCompaniesWorked + PercentSalaryHike + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsAtCompany + YearsSinceLastPromotion + 
                 YearsWithCurrManager + Total.logged.hours + Total.leaves.taken + 
                 Total.excess.logged + PerformanceRating + BusinessTravel.xTRAVEL_FREQUENTLY + 
                 BusinessTravel.xTRAVEL_RARELY + Department.xRESEARCH...DEVELOPMENT + 
                 Department.xSALES + Education.xBELOW.COLLEGE + Education.xCOLLEGE + 
                 Education.xDOCTOR + EducationField.xLIFE.SCIENCES + EducationField.xMARKETING + 
                 EducationField.xMEDICAL + EducationField.xOTHER + EducationField.xTECHNICAL.DEGREE + 
                 JobLevel.x2 + JobLevel.x4 + JobLevel.x5 + JobRole.xHUMAN.RESOURCES + 
                 JobRole.xLABORATORY.TECHNICIAN + JobRole.xMANUFACTURING.DIRECTOR + 
                 JobRole.xRESEARCH.DIRECTOR + JobRole.xRESEARCH.SCIENTIST + 
                 JobRole.xSALES.EXECUTIVE + JobRole.xSALES.REPRESENTATIVE + 
                 MaritalStatus.xMARRIED + MaritalStatus.xSINGLE + StockOptionLevel.x1 + 
                 StockOptionLevel.x2 + StockOptionLevel.x3 + EnvironmentSatisfaction.xLOW + 
                 EnvironmentSatisfaction.xUNDETERMINED + 
                 EnvironmentSatisfaction.xVERY.HIGH + JobSatisfaction.xLOW + 
                 JobSatisfaction.xUNDETERMINED + 
                 JobSatisfaction.xVERY.HIGH + WorkLifeBalance.xBEST + WorkLifeBalance.xBETTER + 
                 WorkLifeBalance.xGOOD + WorkLifeBalance.xUNDETERMINED + JobInvolvement.xLOW + 
                 JobInvolvement.xMEDIUM + JobInvolvement.xVERY.HIGH, family = "binomial", 
               data = train)

summary(model_3)

#Null deviance: 2704.5  on 3066  degrees of freedom
#Residual deviance: 1980.1  on 3011  degrees of freedom
#AIC: 2092.1

#------------------------------------------------------------------
#model 4
#------------------------------------------------------------------
#removed:
#DistanceFromHome                       0.030252   0.059031   0.512  0.60832
#JobRole.xHUMAN.RESOURCES              -0.027694   0.065868  -0.420  0.67416
#EnvironmentSatisfaction.xUNDETERMINED  0.023238   0.049253   0.472  0.63707

model_4 <-glm(formula = Attrition ~ Age + Gender + MonthlyIncome + 
                NumCompaniesWorked + PercentSalaryHike + TotalWorkingYears + 
                TrainingTimesLastYear + YearsAtCompany + YearsSinceLastPromotion + 
                YearsWithCurrManager + Total.logged.hours + Total.leaves.taken + 
                Total.excess.logged + PerformanceRating + BusinessTravel.xTRAVEL_FREQUENTLY + 
                BusinessTravel.xTRAVEL_RARELY + Department.xRESEARCH...DEVELOPMENT + 
                Department.xSALES + Education.xBELOW.COLLEGE + Education.xCOLLEGE + 
                Education.xDOCTOR + EducationField.xLIFE.SCIENCES + EducationField.xMARKETING + 
                EducationField.xMEDICAL + EducationField.xOTHER + EducationField.xTECHNICAL.DEGREE + 
                JobLevel.x2 + JobLevel.x4 + JobLevel.x5 + 
                JobRole.xLABORATORY.TECHNICIAN + JobRole.xMANUFACTURING.DIRECTOR + 
                JobRole.xRESEARCH.DIRECTOR + JobRole.xRESEARCH.SCIENTIST + 
                JobRole.xSALES.EXECUTIVE + JobRole.xSALES.REPRESENTATIVE + 
                MaritalStatus.xMARRIED + MaritalStatus.xSINGLE + StockOptionLevel.x1 + 
                StockOptionLevel.x2 + StockOptionLevel.x3 + EnvironmentSatisfaction.xLOW + 
                EnvironmentSatisfaction.xVERY.HIGH + 
                JobSatisfaction.xLOW + JobSatisfaction.xUNDETERMINED + JobSatisfaction.xVERY.HIGH + 
                WorkLifeBalance.xBEST + WorkLifeBalance.xBETTER + WorkLifeBalance.xGOOD + 
                WorkLifeBalance.xUNDETERMINED + JobInvolvement.xLOW + JobInvolvement.xMEDIUM + 
                JobInvolvement.xVERY.HIGH, family = "binomial", data = train)

summary(model_4)

#Null deviance: 2704.5  on 3066  degrees of freedom
#Residual deviance: 1980.7  on 3014  degrees of freedom
#AIC: 2086.7

#------------------------------------------------------------------
#model 5
#------------------------------------------------------------------
#removed
#JobRole.xSALES.REPRESENTATIVE      -0.043681   0.066860  -0.653 0.513548
#JobSatisfaction.xUNDETERMINED      -0.048475   0.071964  -0.674 0.500569

model_5 <- glm(formula = Attrition ~ Age + Gender + MonthlyIncome + NumCompaniesWorked + 
                 PercentSalaryHike + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsAtCompany + YearsSinceLastPromotion + YearsWithCurrManager + 
                 Total.logged.hours + Total.leaves.taken + Total.excess.logged + 
                 PerformanceRating + BusinessTravel.xTRAVEL_FREQUENTLY + BusinessTravel.xTRAVEL_RARELY + 
                 Department.xRESEARCH...DEVELOPMENT + Department.xSALES + 
                 Education.xBELOW.COLLEGE + Education.xCOLLEGE + Education.xDOCTOR + 
                 EducationField.xLIFE.SCIENCES + EducationField.xMARKETING + 
                 EducationField.xMEDICAL + EducationField.xOTHER + EducationField.xTECHNICAL.DEGREE + 
                 JobLevel.x2 + JobLevel.x4 + JobLevel.x5 + JobRole.xLABORATORY.TECHNICIAN + 
                 JobRole.xMANUFACTURING.DIRECTOR + JobRole.xRESEARCH.DIRECTOR + 
                 JobRole.xRESEARCH.SCIENTIST + JobRole.xSALES.EXECUTIVE + MaritalStatus.xMARRIED + 
                 MaritalStatus.xSINGLE + StockOptionLevel.x1 + StockOptionLevel.x2 + 
                 StockOptionLevel.x3 + EnvironmentSatisfaction.xLOW + EnvironmentSatisfaction.xVERY.HIGH + 
                 JobSatisfaction.xLOW + JobSatisfaction.xVERY.HIGH + 
                 WorkLifeBalance.xBEST + WorkLifeBalance.xBETTER + WorkLifeBalance.xGOOD + 
                 WorkLifeBalance.xUNDETERMINED + JobInvolvement.xLOW + JobInvolvement.xMEDIUM + 
                 JobInvolvement.xVERY.HIGH, family = "binomial", data = train)

summary(model_5)

#Null deviance: 2704.5  on 3066  degrees of freedom
#Residual deviance: 1981.6  on 3016  degrees of freedom
#AIC: 2083.6

#------------------------------------------------------------------
#model 6
#------------------------------------------------------------------
#removed
#PercentSalaryHike                   0.062380   0.093234   0.669 0.503450
#PerformanceRating                  -0.060483   0.092034  -0.657 0.511063

model_6 <- glm(formula = Attrition ~ Age + Gender + MonthlyIncome + NumCompaniesWorked + 
                 TotalWorkingYears + TrainingTimesLastYear + 
                 YearsAtCompany + YearsSinceLastPromotion + YearsWithCurrManager + 
                 Total.logged.hours + Total.leaves.taken + Total.excess.logged + 
                 BusinessTravel.xTRAVEL_FREQUENTLY + BusinessTravel.xTRAVEL_RARELY + 
                 Department.xRESEARCH...DEVELOPMENT + Department.xSALES + 
                 Education.xBELOW.COLLEGE + Education.xCOLLEGE + Education.xDOCTOR + 
                 EducationField.xLIFE.SCIENCES + EducationField.xMARKETING + 
                 EducationField.xMEDICAL + EducationField.xOTHER + EducationField.xTECHNICAL.DEGREE + 
                 JobLevel.x2 + JobLevel.x4 + JobLevel.x5 + JobRole.xLABORATORY.TECHNICIAN + 
                 JobRole.xMANUFACTURING.DIRECTOR + JobRole.xRESEARCH.DIRECTOR + 
                 JobRole.xRESEARCH.SCIENTIST + JobRole.xSALES.EXECUTIVE + 
                 MaritalStatus.xMARRIED + MaritalStatus.xSINGLE + StockOptionLevel.x1 + 
                 StockOptionLevel.x2 + StockOptionLevel.x3 + EnvironmentSatisfaction.xLOW + 
                 EnvironmentSatisfaction.xVERY.HIGH + JobSatisfaction.xLOW + 
                 JobSatisfaction.xVERY.HIGH + WorkLifeBalance.xBEST + WorkLifeBalance.xBETTER + 
                 WorkLifeBalance.xGOOD + WorkLifeBalance.xUNDETERMINED + JobInvolvement.xLOW + 
                 JobInvolvement.xMEDIUM + JobInvolvement.xVERY.HIGH, family = "binomial", 
               data = train)

summary(model_6)

#Null deviance: 2704.5  on 3066  degrees of freedom
#Residual deviance: 1982.1  on 3018  degrees of freedom
#AIC: 2080.1

#------------------------------------------------------------------
#model 7
#------------------------------------------------------------------
#removed
#Education.xBELOW.COLLEGE           -0.049362   0.064447  -0.766 0.443720    
#Education.xCOLLEGE                  0.044054   0.059190   0.744 0.456710

model_7 <- glm(formula = Attrition ~ Age + Gender + MonthlyIncome + NumCompaniesWorked + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsAtCompany + 
                 YearsSinceLastPromotion + YearsWithCurrManager + Total.logged.hours + 
                 Total.leaves.taken + Total.excess.logged + BusinessTravel.xTRAVEL_FREQUENTLY + 
                 BusinessTravel.xTRAVEL_RARELY + Department.xRESEARCH...DEVELOPMENT + 
                 Department.xSALES + 
                 Education.xDOCTOR + EducationField.xLIFE.SCIENCES + EducationField.xMARKETING + 
                 EducationField.xMEDICAL + EducationField.xOTHER + EducationField.xTECHNICAL.DEGREE + 
                 JobLevel.x2 + JobLevel.x4 + JobLevel.x5 + JobRole.xLABORATORY.TECHNICIAN + 
                 JobRole.xMANUFACTURING.DIRECTOR + JobRole.xRESEARCH.DIRECTOR + 
                 JobRole.xRESEARCH.SCIENTIST + JobRole.xSALES.EXECUTIVE + 
                 MaritalStatus.xMARRIED + MaritalStatus.xSINGLE + StockOptionLevel.x1 + 
                 StockOptionLevel.x2 + StockOptionLevel.x3 + EnvironmentSatisfaction.xLOW + 
                 EnvironmentSatisfaction.xVERY.HIGH + JobSatisfaction.xLOW + 
                 JobSatisfaction.xVERY.HIGH + WorkLifeBalance.xBEST + WorkLifeBalance.xBETTER + 
                 WorkLifeBalance.xGOOD + WorkLifeBalance.xUNDETERMINED + JobInvolvement.xLOW + 
                 JobInvolvement.xMEDIUM + JobInvolvement.xVERY.HIGH, family = "binomial", 
               data = train)

summary(model_7)

#Null deviance: 2704.5  on 3066  degrees of freedom
#Residual deviance: 1983.5  on 3020  degrees of freedom
#AIC: 2077.5

#------------------------------------------------------------------
#model 8
#------------------------------------------------------------------
#removed
#YearsAtCompany                      0.145385   0.147701   0.984  0.32496
#JobRole.xLABORATORY.TECHNICIAN      0.068420   0.069072   0.991  0.32190 
#JobInvolvement.xMEDIUM              0.058559   0.062325   0.940  0.34743

model_8 <- glm(formula = Attrition ~ Age + Gender + MonthlyIncome + NumCompaniesWorked + 
                 TotalWorkingYears + TrainingTimesLastYear +  
                 YearsSinceLastPromotion + YearsWithCurrManager + Total.logged.hours + 
                 Total.leaves.taken + Total.excess.logged + BusinessTravel.xTRAVEL_FREQUENTLY + 
                 BusinessTravel.xTRAVEL_RARELY + Department.xRESEARCH...DEVELOPMENT + 
                 Department.xSALES + Education.xDOCTOR + EducationField.xLIFE.SCIENCES + 
                 EducationField.xMARKETING + EducationField.xMEDICAL + EducationField.xOTHER + 
                 EducationField.xTECHNICAL.DEGREE + JobLevel.x2 + JobLevel.x4 + 
                 JobLevel.x5 + JobRole.xMANUFACTURING.DIRECTOR + 
                 JobRole.xRESEARCH.DIRECTOR + JobRole.xRESEARCH.SCIENTIST + 
                 JobRole.xSALES.EXECUTIVE + MaritalStatus.xMARRIED + MaritalStatus.xSINGLE + 
                 StockOptionLevel.x1 + StockOptionLevel.x2 + StockOptionLevel.x3 + 
                 EnvironmentSatisfaction.xLOW + EnvironmentSatisfaction.xVERY.HIGH + 
                 JobSatisfaction.xLOW + JobSatisfaction.xVERY.HIGH + WorkLifeBalance.xBEST + 
                 WorkLifeBalance.xBETTER + WorkLifeBalance.xGOOD + WorkLifeBalance.xUNDETERMINED + 
                 JobInvolvement.xLOW + JobInvolvement.xVERY.HIGH, 
               family = "binomial", data = train)

summary(model_8)

#Null deviance: 2704.5  on 3066  degrees of freedom
#Residual deviance: 1986.2  on 3023  degrees of freedom
#AIC: 2074.2

#------------------------------------------------------------------
#model 9
#------------------------------------------------------------------
#removed
#JobLevel.x4                         0.059759   0.058121   1.028 0.303868
#StockOptionLevel.x2                -0.059503   0.060966  -0.976 0.329057    
#StockOptionLevel.x3                -0.062997   0.061453  -1.025 0.305305

model_9 <- glm(formula = Attrition ~ Age + Gender + MonthlyIncome + NumCompaniesWorked + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + Total.logged.hours + Total.leaves.taken + 
                 Total.excess.logged + BusinessTravel.xTRAVEL_FREQUENTLY + 
                 BusinessTravel.xTRAVEL_RARELY + Department.xRESEARCH...DEVELOPMENT + 
                 Department.xSALES + Education.xDOCTOR + EducationField.xLIFE.SCIENCES + 
                 EducationField.xMARKETING + EducationField.xMEDICAL + EducationField.xOTHER + 
                 EducationField.xTECHNICAL.DEGREE + JobLevel.x2 +  
                 JobLevel.x5 + JobRole.xMANUFACTURING.DIRECTOR + JobRole.xRESEARCH.DIRECTOR + 
                 JobRole.xRESEARCH.SCIENTIST + JobRole.xSALES.EXECUTIVE + 
                 MaritalStatus.xMARRIED + MaritalStatus.xSINGLE + StockOptionLevel.x1 + 
                 EnvironmentSatisfaction.xLOW + 
                 EnvironmentSatisfaction.xVERY.HIGH + JobSatisfaction.xLOW + 
                 JobSatisfaction.xVERY.HIGH + WorkLifeBalance.xBEST + WorkLifeBalance.xBETTER + 
                 WorkLifeBalance.xGOOD + WorkLifeBalance.xUNDETERMINED + JobInvolvement.xLOW + 
                 JobInvolvement.xVERY.HIGH, family = "binomial", data = train)

summary(model_9)
#Null deviance: 2704.5  on 3066  degrees of freedom
#Residual deviance: 1989.0  on 3026  degrees of freedom
#AIC: 2071

#------------------------------------------------------------------
#model 10
#------------------------------------------------------------------
#removed
#Gender                              0.064703   0.058879   1.099 0.271800
#JobRole.xRESEARCH.SCIENTIST         0.064381   0.061768   1.042 0.297270

model_10 <- glm(formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + Total.logged.hours + Total.leaves.taken + 
                  Total.excess.logged + BusinessTravel.xTRAVEL_FREQUENTLY + 
                  BusinessTravel.xTRAVEL_RARELY + Department.xRESEARCH...DEVELOPMENT + 
                  Department.xSALES + Education.xDOCTOR + EducationField.xLIFE.SCIENCES + 
                  EducationField.xMARKETING + EducationField.xMEDICAL + EducationField.xOTHER + 
                  EducationField.xTECHNICAL.DEGREE + JobLevel.x2 + JobLevel.x5 + 
                  JobRole.xMANUFACTURING.DIRECTOR + JobRole.xRESEARCH.DIRECTOR + 
                  JobRole.xSALES.EXECUTIVE + 
                  MaritalStatus.xMARRIED + MaritalStatus.xSINGLE + StockOptionLevel.x1 + 
                  EnvironmentSatisfaction.xLOW + EnvironmentSatisfaction.xVERY.HIGH + 
                  JobSatisfaction.xLOW + JobSatisfaction.xVERY.HIGH + WorkLifeBalance.xBEST + 
                  WorkLifeBalance.xBETTER + WorkLifeBalance.xGOOD + WorkLifeBalance.xUNDETERMINED + 
                  JobInvolvement.xLOW + JobInvolvement.xVERY.HIGH, family = "binomial", 
                data = train)

summary(model_10)
#Null deviance: 2704.5  on 3066  degrees of freedom
#Residual deviance: 1991.3  on 3028  degrees of freedom
#AIC: 2069.3

#------------------------------------------------------------------
#model 11
#------------------------------------------------------------------
#removed
#JobRole.xSALES.EXECUTIVE            0.064741   0.059685   1.085 0.278049

model_11 <- glm(formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + Total.logged.hours + Total.leaves.taken + 
                  Total.excess.logged + BusinessTravel.xTRAVEL_FREQUENTLY + 
                  BusinessTravel.xTRAVEL_RARELY + Department.xRESEARCH...DEVELOPMENT + 
                  Department.xSALES + Education.xDOCTOR + EducationField.xLIFE.SCIENCES + 
                  EducationField.xMARKETING + EducationField.xMEDICAL + EducationField.xOTHER + 
                  EducationField.xTECHNICAL.DEGREE + JobLevel.x2 + JobLevel.x5 + 
                  JobRole.xMANUFACTURING.DIRECTOR + JobRole.xRESEARCH.DIRECTOR + 
                  MaritalStatus.xMARRIED + MaritalStatus.xSINGLE + 
                  StockOptionLevel.x1 + EnvironmentSatisfaction.xLOW + EnvironmentSatisfaction.xVERY.HIGH + 
                  JobSatisfaction.xLOW + JobSatisfaction.xVERY.HIGH + WorkLifeBalance.xBEST + 
                  WorkLifeBalance.xBETTER + WorkLifeBalance.xGOOD + WorkLifeBalance.xUNDETERMINED + 
                  JobInvolvement.xLOW + JobInvolvement.xVERY.HIGH, family = "binomial", 
                data = train)

summary(model_11)

#Null deviance: 2704.5  on 3066  degrees of freedom
#Residual deviance: 1992.5  on 3029  degrees of freedom
#AIC: 2068.5

#------------------------------------------------------------------
#model 12
#------------------------------------------------------------------
#removed
#Department.xSALES                  -0.227105   0.168038  -1.352 0.176532
#EducationField.xLIFE.SCIENCES      -0.368426   0.235935  -1.562 0.118392
#EducationField.xMEDICAL            -0.356612   0.221769  -1.608 0.107828
#JobLevel.x2                         0.090526   0.058119   1.558 0.119328
#MaritalStatus.xMARRIED              0.134999   0.088077   1.533 0.125339

model_12 <- glm(formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + Total.logged.hours + Total.leaves.taken + 
                  Total.excess.logged + BusinessTravel.xTRAVEL_FREQUENTLY + 
                  BusinessTravel.xTRAVEL_RARELY + Department.xRESEARCH...DEVELOPMENT + 
                  Education.xDOCTOR + EducationField.xMARKETING + EducationField.xOTHER + 
                  EducationField.xTECHNICAL.DEGREE + JobLevel.x5 + 
                  JobRole.xMANUFACTURING.DIRECTOR + JobRole.xRESEARCH.DIRECTOR + 
                  MaritalStatus.xSINGLE + StockOptionLevel.x1 + 
                  EnvironmentSatisfaction.xLOW + EnvironmentSatisfaction.xVERY.HIGH + 
                  JobSatisfaction.xLOW + JobSatisfaction.xVERY.HIGH + WorkLifeBalance.xBEST + 
                  WorkLifeBalance.xBETTER + WorkLifeBalance.xGOOD + WorkLifeBalance.xUNDETERMINED + 
                  JobInvolvement.xLOW + JobInvolvement.xVERY.HIGH, family = "binomial", 
                data = train)

summary(model_12)
#Null deviance: 2704.5  on 3066  degrees of freedom
#Residual deviance: 2011.1  on 3034  degrees of freedom
#AIC: 2077.1

#------------------------------------------------------------------
#model 13
#------------------------------------------------------------------
#removed
#EducationField.xOTHER              -0.085757   0.059209  -1.448 0.147510

model_13 <- glm(formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + Total.logged.hours + Total.leaves.taken + 
                  Total.excess.logged + BusinessTravel.xTRAVEL_FREQUENTLY + 
                  BusinessTravel.xTRAVEL_RARELY + Department.xRESEARCH...DEVELOPMENT + 
                  Education.xDOCTOR + EducationField.xMARKETING + 
                  EducationField.xTECHNICAL.DEGREE + JobLevel.x5 + JobRole.xMANUFACTURING.DIRECTOR + 
                  JobRole.xRESEARCH.DIRECTOR + MaritalStatus.xSINGLE + StockOptionLevel.x1 + 
                  EnvironmentSatisfaction.xLOW + EnvironmentSatisfaction.xVERY.HIGH + 
                  JobSatisfaction.xLOW + JobSatisfaction.xVERY.HIGH + WorkLifeBalance.xBEST + 
                  WorkLifeBalance.xBETTER + WorkLifeBalance.xGOOD + WorkLifeBalance.xUNDETERMINED + 
                  JobInvolvement.xLOW + JobInvolvement.xVERY.HIGH, family = "binomial", 
                data = train)

summary(model_13)

#Null deviance: 2704.5  on 3066  degrees of freedom
#Residual deviance: 2013.3  on 3035  degrees of freedom
#AIC: 2077.3

#------------------------------------------------------------------
#model 14
#------------------------------------------------------------------
#removed
#MonthlyIncome                      -0.105159   0.060361  -1.742 0.081476 
#JobLevel.x5                        -0.117950   0.062827  -1.877 0.060464 
#WorkLifeBalance.xUNDETERMINED      -0.150721   0.076944  -1.959 0.050132 

model_14 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + Total.logged.hours + Total.leaves.taken + 
                  Total.excess.logged + BusinessTravel.xTRAVEL_FREQUENTLY + 
                  BusinessTravel.xTRAVEL_RARELY + Department.xRESEARCH...DEVELOPMENT + 
                  Education.xDOCTOR + EducationField.xMARKETING + EducationField.xTECHNICAL.DEGREE + 
                  JobRole.xMANUFACTURING.DIRECTOR + JobRole.xRESEARCH.DIRECTOR + 
                  MaritalStatus.xSINGLE + StockOptionLevel.x1 + EnvironmentSatisfaction.xLOW + 
                  EnvironmentSatisfaction.xVERY.HIGH + JobSatisfaction.xLOW + 
                  JobSatisfaction.xVERY.HIGH + WorkLifeBalance.xBEST + WorkLifeBalance.xBETTER + 
                  WorkLifeBalance.xGOOD + JobInvolvement.xLOW + 
                  JobInvolvement.xVERY.HIGH, family = "binomial", data = train)

summary(model_14)

#Null deviance: 2704.5  on 3066  degrees of freedom
#Residual deviance: 2025.2  on 3038  degrees of freedom
#AIC: 2083.2

#------------------------------------------------------------------
#model 15
#------------------------------------------------------------------
#removed
#Total.leaves.taken                  0.69125    0.28892   2.393 0.016733   
#Total.excess.logged                -0.01494    0.00660  -2.264 0.023598 
#Department.xRESEARCH...DEVELOPMENT -0.14771    0.06520  -2.266 0.023476   
#Education.xDOCTOR                  -0.13329    0.06235  -2.138 0.032527 
#EducationField.xTECHNICAL.DEGREE   -0.14965    0.06892  -2.171 0.029899 
#JobInvolvement.xLOW                 0.10665    0.05198   2.052 0.040170   
#JobInvolvement.xVERY.HIGH           0.11023    0.05390   2.045 0.040841 

model_15 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  Total.logged.hours + BusinessTravel.xTRAVEL_FREQUENTLY + BusinessTravel.xTRAVEL_RARELY + 
                  EducationField.xMARKETING + JobRole.xMANUFACTURING.DIRECTOR + JobRole.xRESEARCH.DIRECTOR + 
                  MaritalStatus.xSINGLE + StockOptionLevel.x1 + EnvironmentSatisfaction.xLOW + 
                  EnvironmentSatisfaction.xVERY.HIGH + JobSatisfaction.xLOW + 
                  JobSatisfaction.xVERY.HIGH + WorkLifeBalance.xBEST + WorkLifeBalance.xBETTER + 
                  WorkLifeBalance.xGOOD, family = "binomial", data = train)

summary(model_15)

#Null deviance: 2704.5  on 3066  degrees of freedom
#Residual deviance: 2053.7  on 3045  degrees of freedom
#AIC: 2097.7

#------------------------------------------------------------------
#model 16
#------------------------------------------------------------------
#remove
#EducationField.xMARKETING          -0.08478    0.05716  -1.483 0.137977

model_16 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  Total.logged.hours + BusinessTravel.xTRAVEL_FREQUENTLY + 
                  BusinessTravel.xTRAVEL_RARELY + 
                  JobRole.xMANUFACTURING.DIRECTOR + JobRole.xRESEARCH.DIRECTOR + 
                  MaritalStatus.xSINGLE + StockOptionLevel.x1 + EnvironmentSatisfaction.xLOW + 
                  EnvironmentSatisfaction.xVERY.HIGH + JobSatisfaction.xLOW + 
                  JobSatisfaction.xVERY.HIGH + WorkLifeBalance.xBEST + WorkLifeBalance.xBETTER + 
                  WorkLifeBalance.xGOOD, family = "binomial", data = train)

summary(model_16)
#Null deviance: 2704.5  on 3066  degrees of freedom
#Residual deviance: 2056.0  on 3046  degrees of freedom
#AIC: 2098

#------------------------------------------------------------------
#model 17
#------------------------------------------------------------------
#removed
#JobRole.xRESEARCH.DIRECTOR          0.11817    0.04926   2.399 0.016450 
#StockOptionLevel.x1                -0.13809    0.05754  -2.400 0.016401 
#EnvironmentSatisfaction.xVERY.HIGH -0.16265    0.06364  -2.556 0.010598 
#WorkLifeBalance.xBEST              -0.19383    0.07910  -2.451 0.014262 

model_17 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  Total.logged.hours + BusinessTravel.xTRAVEL_FREQUENTLY + 
                  BusinessTravel.xTRAVEL_RARELY + JobRole.xMANUFACTURING.DIRECTOR + 
                  MaritalStatus.xSINGLE + EnvironmentSatisfaction.xLOW + 
                  JobSatisfaction.xLOW + JobSatisfaction.xVERY.HIGH + 
                  WorkLifeBalance.xBETTER + WorkLifeBalance.xGOOD, family = "binomial", 
                data = train)
summary(model_17)

#Null deviance: 2704.5  on 3066  degrees of freedom
#Residual deviance: 2080.3  on 3050  degrees of freedom
#AIC: 2114.3

#------------------------------------------------------------------
#model 19
#------------------------------------------------------------------
#removed
#WorkLifeBalance.xGOOD             -0.18313    0.07323  -2.501 0.012395  

model_19 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  Total.logged.hours + BusinessTravel.xTRAVEL_FREQUENTLY + 
                  BusinessTravel.xTRAVEL_RARELY + JobRole.xMANUFACTURING.DIRECTOR + 
                  MaritalStatus.xSINGLE + EnvironmentSatisfaction.xLOW + JobSatisfaction.xLOW + 
                  JobSatisfaction.xVERY.HIGH + WorkLifeBalance.xBETTER, 
                family = "binomial", data = train)

summary(model_19)

#Null deviance: 2704.5  on 3066  degrees of freedom
#Residual deviance: 2086.5  on 3051  degrees of freedom
#AIC: 2118.5

#------------------------------------------------------------------
#model 20
#------------------------------------------------------------------
#remove
#Age                               -0.22315    0.07766  -2.873 0.004062 
#BusinessTravel.xTRAVEL_RARELY      0.30262    0.10532   2.873 0.004062 

model_20 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  Total.logged.hours + BusinessTravel.xTRAVEL_FREQUENTLY + 
                  JobRole.xMANUFACTURING.DIRECTOR + 
                  MaritalStatus.xSINGLE + EnvironmentSatisfaction.xLOW + JobSatisfaction.xLOW + 
                  JobSatisfaction.xVERY.HIGH + WorkLifeBalance.xBETTER, family = "binomial", 
                data = train)

summary(model_20)

#Null deviance: 2704.5  on 3066  degrees of freedom
#Residual deviance: 2104.0  on 3053  degrees of freedom
#AIC: 2132

#------------------------------------------------------------------
#model 21
#------------------------------------------------------------------
#remove
#WorkLifeBalance.xBETTER           -0.16387    0.05539  -2.958  0.00309  

model_21 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  Total.logged.hours + BusinessTravel.xTRAVEL_FREQUENTLY + 
                  JobRole.xMANUFACTURING.DIRECTOR + MaritalStatus.xSINGLE + 
                  EnvironmentSatisfaction.xLOW + JobSatisfaction.xLOW + JobSatisfaction.xVERY.HIGH
                , family = "binomial", data = train)

summary(model_21)

#Null deviance: 2704.5  on 3066  degrees of freedom
#Residual deviance: 2112.7  on 3054  degrees of freedom
#AIC: 2138.7

#------------------------------------------------------------------
#model 22
#------------------------------------------------------------------
#removed
#TrainingTimesLastYear             -0.18137    0.05670  -3.199 0.001380 

model_22 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                     YearsSinceLastPromotion + YearsWithCurrManager + 
                     Total.logged.hours + BusinessTravel.xTRAVEL_FREQUENTLY + 
                     JobRole.xMANUFACTURING.DIRECTOR + MaritalStatus.xSINGLE + 
                     EnvironmentSatisfaction.xLOW + JobSatisfaction.xLOW + JobSatisfaction.xVERY.HIGH, 
                   family = "binomial", data = train)

summary(model_22)

#Null deviance: 2704.5  on 3066  degrees of freedom
#Residual deviance: 2123.2  on 3055  degrees of freedom
#AIC: 2147.2

#------------------------------------------------------------------
#model 23
#------------------------------------------------------------------
#removed
#JobRole.xMANUFACTURING.DIRECTOR   -0.21327    0.06565  -3.249  0.00116 

model_23 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + Total.logged.hours + 
                  BusinessTravel.xTRAVEL_FREQUENTLY + JobRole.xMANUFACTURING.DIRECTOR + 
                  MaritalStatus.xSINGLE + EnvironmentSatisfaction.xLOW + JobSatisfaction.xLOW + 
                  JobSatisfaction.xVERY.HIGH, family = "binomial", data = train)

summary(model_23)

#Null deviance: 2704.5  on 3066  degrees of freedom
#Residual deviance: 2123.2  on 3055  degrees of freedom
#AIC: 2147.2

#------------------------------------------------------------------
#final model
#------------------------------------------------------------------
#removed
#JobRole.xMANUFACTURING.DIRECTOR   -0.21327    0.06565  -3.249  0.00116 ** 

final_model <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + Total.logged.hours + 
                  BusinessTravel.xTRAVEL_FREQUENTLY + 
                  MaritalStatus.xSINGLE + EnvironmentSatisfaction.xLOW + JobSatisfaction.xLOW + 
                  JobSatisfaction.xVERY.HIGH, family = "binomial", data = train)

summary(final_model)

# All the P-values now come out to be significant #

#Null deviance: 2704.5  on 3066  degrees of freedom
#Residual deviance: 2135.0  on 3056  degrees of freedom
#AIC: 2157

#******************************************
#Coefficients:
#******************************************
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)                       -2.20686    0.07319 -30.151  < 2e-16 ***
#  NumCompaniesWorked                 0.25770    0.05668   4.547 5.44e-06 ***
#  TotalWorkingYears                 -0.74781    0.09001  -8.308  < 2e-16 ***
#  YearsSinceLastPromotion            0.50694    0.07627   6.647 3.00e-11 ***
#  YearsWithCurrManager              -0.49320    0.08661  -5.694 1.24e-08 ***
#  Total.logged.hours                 0.64084    0.05395  11.878  < 2e-16 ***
#  BusinessTravel.xTRAVEL_FREQUENTLY  0.33455    0.05033   6.647 2.98e-11 ***
#  MaritalStatus.xSINGLE              0.49690    0.05275   9.420  < 2e-16 ***
#  EnvironmentSatisfaction.xLOW       0.42791    0.05059   8.458  < 2e-16 ***
#  JobSatisfaction.xLOW               0.25880    0.05353   4.835 1.33e-06 ***
#  JobSatisfaction.xVERY.HIGH        -0.25696    0.06380  -4.027 5.64e-05 ***

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Model evaluation
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# predictions value
test$AttritionPredictedValue <- predict(final_model,type="response",newdata = test %>% dplyr::select(-Attrition))

# Minimum predicted Probability value
min(test$AttritionPredictedValue)
# 0.0005206165

# Maximum predicted Probability value
max(test$AttritionPredictedValue)
#0.9407626

#summary
summary(test$AttritionPredictedValue)

#Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#0.0005206 0.0403977 0.1021843 0.1583250 0.2130685 0.9407626 

# Lets validate for 500 different cut off value for finding closet prediction result
AttritionProbabilityCheckRange <- seq(min(test$AttritionPredictedValue),max(test$AttritionPredictedValue),length.out = 500)
CutOfAnalysis_dataframe <- data.frame(iteration=vector(mode="numeric"),
                                cutoff=vector(mode="numeric"),
                                TP=vector(mode="numeric"),
                                TN=vector(mode="numeric"),
                                FP=vector(mode="numeric"),
                                FN=vector(mode="numeric"))

#increment value
ivalue <- 1
probability_val <- txtProgressBar(min=1,max=500,style = 3)
for (cutoff in AttritionProbabilityCheckRange) {
  setTxtProgressBar(probability_val,ivalue)
  test_check <- test
  test_check$AttritionPredict <- sapply(test_check$AttritionPredictedValue,function(x) ifelse(x>cutoff,1,0))
  TP <- length(which(test_check$Attrition == 1 & test_check$AttritionPredict == 1))
  TN <- length(which(test_check$Attrition == 0 & test_check$AttritionPredict == 0))
  FP <- length(which(test_check$Attrition == 0 & test_check$AttritionPredict == 1))
  FN <- length(which(test_check$Attrition == 1 & test_check$AttritionPredict == 0))
  CutOfAnalysis_dataframe[ivalue,] <- c(ivalue,cutoff,TP,TN,FP,FN)
  ivalue <- ivalue + 1
}

#calulating accuracy, specificity, sensitivity etc
CutOfAnalysis_dataframe <- CutOfAnalysis_dataframe %>% mutate(P=FN+TP,N=TN+FP) %>% 
  mutate(Sn=TP/P, Sp=TN/N) %>%
  mutate(KS=abs((TP/P)-(FP/N))) %>%
  mutate(Accuracy=(TP+TN)/(P+N)) %>%
  mutate(Lift=(TP/P)/((TP+FP)/(P+N)))

#Building 3 cutoff values
CutOffValue1 <- CutOfAnalysis_dataframe[(which(abs(CutOfAnalysis_dataframe$Sn - CutOfAnalysis_dataframe$Sp) < 0.01))[2],'cutoff']
#CutOffValue1 : 0.1550293
CutOffValue2 <- CutOfAnalysis_dataframe[which(CutOfAnalysis_dataframe$Accuracy == max(CutOfAnalysis_dataframe$Accuracy))[1],'cutoff']
#CutOffValue2 : 0.5149215
CutOffValue3 <- CutOfAnalysis_dataframe[which(CutOfAnalysis_dataframe$KS == max(CutOfAnalysis_dataframe$KS)),'cutoff']
#CutOffValue3 : 0.1493766

library(plyr)
test_1 <- test %>% mutate(AttritionPredict=ifelse(AttritionPredictedValue>CutOffValue1,1,0))
test_2 <- test %>% mutate(AttritionPredict=ifelse(AttritionPredictedValue>CutOffValue2,1,0))
test_3 <- test %>% mutate(AttritionPredict=ifelse(AttritionPredictedValue>CutOffValue3,1,0))
detach(package:plyr)

#Plot for analyze cutoff values
comparision_plot <- ggplot(CutOfAnalysis_dataframe) +
  geom_line(aes(x=cutoff,y=Accuracy,color="Accuracy"),size=1) + 
  geom_line(aes(x=cutoff,y=Sp,color="Specificity"),size=1) + 
  geom_line(aes(x=cutoff,y=Sn,color="Sensitivity"),size=1) + 
  geom_line(aes(x=cutoff,y=KS,col="KS"),size=1) + 
  geom_vline(xintercept=CutOffValue1,size=1,col="black") +
  geom_text(aes(x=CutOffValue1+0.085,y=0.9,label=c("Acc/Sp/Sn CutOff = 0.1608")),inherit.aes = FALSE) +
  geom_vline(xintercept=CutOffValue2,size=1,col="black") +
  geom_text(aes(x=CutOffValue2+0.075,y=0.9,label=c("Max Acc CutOff = 0.6192")),inherit.aes = FALSE) +
  geom_vline(xintercept=CutOffValue3,size=1,col="black") +
  geom_text(aes(x=CutOffValue3-0.075,y=0.12,label=c("Max KS CutOff = 0.1465")),inherit.aes = FALSE) +
  scale_color_manual("",breaks=c("Accuracy","Specificity","Sensitivity","KS"),
                     values=c("blue","green","red","magenta")) +
  scale_x_continuous(breaks=seq(0,1,0.05)) +
  scale_y_continuous(breaks=seq(0,1,0.05)) +
  labs(title="Accuracy/Specificity/Sensitivity/KS comparison",x="CutOff",y="Accuracy/Specificity/Sensitivity/KS comparison")
grid.arrange(comparision_plot)

library(Rcpp)
library(ModelMetrics)
library(ddalpha)
library(RcppRoll)
library(dimRed)
library(gower)
library(caret)

#Building confusionMatrix 
#converting it into factor
test_1$AttritionPredict <- as.factor(test_1$AttritionPredict)
test_2$AttritionPredict <- as.factor(test_2$AttritionPredict)
test_3$AttritionPredict <- as.factor(test_3$AttritionPredict)

confusionMatrix_1 <- confusionMatrix(test_1$AttritionPredict,test_1$Attrition)
confusionMatrix_2 <- confusionMatrix(test_2$AttritionPredict,test_2$Attrition)
confusionMatrix_3 <- confusionMatrix(test_3$AttritionPredict,test_3$Attrition)

confusionMatrix_1$table
#Reference
#Prediction   0   1
#0          763  60
#1          340 152

confusionMatrix_2$table
#Reference
#Prediction    0    1
#0            1083  167
#1            20   45

confusionMatrix_3$table
#Reference
#Prediction   0   1
#0          763  60
#1          340 152

library(plyr)

buildOrderedRandomEmpData <- function(emp_ref.df,random) {
  if (random == 0) {
    emp_ref_ordered_prob.df <- emp_ref.df %>% arrange(desc(AttritionPredictedValue))
    return(emp_ref_ordered_prob.df)
  } else {
    set.seed(100)
    emp_ref_random_prob.df <- emp_ref.df[sample(c(1:nrow(emp_ref.df)),size=nrow(emp_ref.df)),]
    return(emp_ref_random_prob.df)
  }
}

buildGainChart <- function(df) {
  itr <- 1
  gainChart <- data.frame(numObs = vector(mode="numeric"),
                          attritionYes = vector(mode="numeric"),
                          cumAttritionYes = vector(mode="numeric"),
                          percCumAttritionYes = vector(mode="numeric"),
                          attritionNo = vector(mode="numeric"),
                          cumAttritionNo = vector(mode="numeric"),
                          percCumAttritionNo = vector(mode="numeric"),
                          KS = vector(mode="numeric"))
  startingRowNumber = 1
  groupsOf <- round(nrow(df)/10,0) - 1
  totalAttritionYes <- sum(df$AttritionPredict == 1)
  totalAttritionNo <- sum(df$AttritionPredict == 0)
  while (itr <= 10) {
    endingRowNumber = (startingRowNumber + groupsOf) - 1
    if (endingRowNumber > nrow(df)) {
      endingRowNumber <- nrow(df)
    }
    ref.df <- df[c(startingRowNumber:endingRowNumber),]
    numObs <- nrow(ref.df)
    attritionYes <- sum(ref.df$AttritionPredict == 1)
    attritionNo <- sum(ref.df$AttritionPredict == 0)
    cumAttrition_temp <- cumsum(c(gainChart$attritionYes,attritionYes))
    cumAttritionYes <- cumAttrition_temp[length(cumAttrition_temp)]
    percCumAttritionYes <- cumAttritionYes/totalAttritionYes
    cumAttrition_temp <- cumsum(c(gainChart$attritionNo,attritionNo))
    cumAttritionNo <- cumAttrition_temp[length(cumAttrition_temp)]
    percCumAttritionNo <- cumAttritionNo / totalAttritionNo
    KS <- percCumAttritionYes - percCumAttritionNo
    gainChart[itr,] <- c(numObs,attritionYes,cumAttritionYes,percCumAttritionYes,attritionNo,cumAttritionNo,percCumAttritionNo,KS)
    itr <- itr + 1
    startingRowNumber <- endingRowNumber + 1
  }
  return(gainChart)
}

#----------------------------------------------------------------------------------
# ROC : Based on test_1
#----------------------------------------------------------------------------------

# DataFrame to consider = test_1 --> This is based on CutOffValue1 = 0.1550
emp_ref_ordered_prob.df <- buildOrderedRandomEmpData(test_1,random=0)
gainChart_ordered_test_1 <- buildGainChart(emp_ref_ordered_prob.df)


# Building random data frame
emp_ref_random_prob.df <- buildOrderedRandomEmpData(test_1,random=1)
gainChart_random_test_1 <- buildGainChart(emp_ref_random_prob.df)

ROC_Curve_Plot<- ggplot() + 
  geom_line(data=gainChart_random_test_1,aes(x=percCumAttritionNo,y=percCumAttritionYes,color="Random"),size=1) +
  geom_line(data=gainChart_ordered_test_1,aes(x=percCumAttritionNo,y=percCumAttritionYes,color="Ordered"),size=1) +
  scale_color_manual("",breaks=c("Random","Ordered"),
                     values=c("blue","red")) +
  scale_x_continuous(breaks=seq(0,1,0.1)) +
  scale_y_continuous(breaks=seq(0,1,0.05)) +
  labs(title="ROC Curve for Prediction based on\nAccuracy,Specificity,Senstivity\nCutOff = 0.1550",
       x="Percentage Attrition = No",
       y="Percentage Attrition = Yes")
grid.arrange(ROC_Curve_Plot)


#----------------------------------------------------------------------------------
# ROC : Based on test_2
#----------------------------------------------------------------------------------

# DataFrame to consider = test_2 --> This is based on CutOffValue2 = 0.5149
emp_ref_ordered_prob.df <- buildOrderedRandomEmpData(test_2,random=0)
gainChart_ordered_test_2 <- buildGainChart(emp_ref_ordered_prob.df)


# Building random data frame
emp_ref_random_prob.df <- buildOrderedRandomEmpData(test_2,random=1)
gainChart_random_test_2 <- buildGainChart(emp_ref_random_prob.df)

ROC_Curve_Plot<- ggplot() + 
  geom_line(data=gainChart_random_test_2,aes(x=percCumAttritionNo,y=percCumAttritionYes,color="Random"),size=1) +
  geom_line(data=gainChart_ordered_test_2,aes(x=percCumAttritionNo,y=percCumAttritionYes,color="Ordered"),size=1) +
  scale_color_manual("",breaks=c("Random","Ordered"),
                     values=c("blue","red")) +
  scale_x_continuous(breaks=seq(0,1,0.1)) +
  scale_y_continuous(breaks=seq(0,1,0.05)) +
  labs(title="ROC Curve for Prediction based on\nAccuracy,Specificity,Senstivity\nCutOff = 0.5149",
       x="Percentage Attrition = No",
       y="Percentage Attrition = Yes")
grid.arrange(ROC_Curve_Plot)

#----------------------------------------------------------------------------------

# DataFrame to consider = test_3 --> This is based on CutOffValue3 = 0.1493
emp_ref_ordered_prob.df <- buildOrderedRandomEmpData(test_3,random=0)
gainChart_ordered_test_3 <- buildGainChart(emp_ref_ordered_prob.df)


# Building random data frame
emp_ref_random_prob.df <- buildOrderedRandomEmpData(test_3,random=1)
gainChart_random_test_3 <- buildGainChart(emp_ref_random_prob.df)

ROC_Curve_Plot<- ggplot() + 
  geom_line(data=gainChart_random_test_3,aes(x=percCumAttritionNo,y=percCumAttritionYes,color="Random"),size=1) +
  geom_line(data=gainChart_ordered_test_3,aes(x=percCumAttritionNo,y=percCumAttritionYes,color="Ordered"),size=1) +
  scale_color_manual("",breaks=c("Random","Ordered"),
                     values=c("blue","red")) +
  scale_x_continuous(breaks=seq(0,1,0.1)) +
  scale_y_continuous(breaks=seq(0,1,0.05)) +
  labs(title="ROC Curve for Prediction based on\nAccuracy,Specificity,Senstivity\nCutOff = 0.1493",
       x="Percentage Attrition = No",
       y="Percentage Attrition = Yes")
grid.arrange(ROC_Curve_Plot)

#------------------------------------------------------------------------------

library(ROCR)
ROCRpred_1 <- prediction(as.numeric(test_1$Attrition),as.numeric(test_1$AttritionPredict))
ROCRperf_1 <- performance(ROCRpred_1, 'tpr','fpr')
plot(ROCRperf_1, colorize = TRUE, text.adj = c(-0.2,1.7))

auc <- performance(ROCRpred_1, measure = "auc")
auc <- auc@y.values[[1]]
auc

#0.6180195

#..........................

ROCRpred_2 <- prediction(as.numeric(test_2$Attrition),as.numeric(test_2$AttritionPredict))
ROCRperf_2 <- performance(ROCRpred_2, 'tpr','fpr')
plot(ROCRperf_2, colorize = TRUE, text.adj = c(-0.2,1.7))

auc <- performance(ROCRpred_2, measure = "auc")
auc <- auc@y.values[[1]]
auc

#0.7793538

#..........................

ROCRpred_3 <- prediction(as.numeric(test_3$Attrition),as.numeric(test_3$AttritionPredict))
ROCRperf_3 <- performance(ROCRpred_3, 'tpr','fpr')
plot(ROCRperf_3, colorize = TRUE, text.adj = c(-0.2,1.7))

auc <- performance(ROCRpred_3, measure = "auc")
auc <- auc@y.values[[1]]
auc

#0.6180195

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

# From the above AUC we can conclude that "Accuracy : 0.7793538"

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

######## Inference on the above analysis #######
# Based on the model, the following significant variables have been considered for infer 
# co-efficients as follows

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)                       -2.20686    0.07319 -30.151  < 2e-16 ***
#  NumCompaniesWorked                 0.25770    0.05668   4.547 5.44e-06 ***
# Positive coefficient    : Employee frequently switches job frequently = higher attrition probability

#  TotalWorkingYears                 -0.74781    0.09001  -8.308  < 2e-16 ***
# Negative coefficient  : Employee with Higher experience = lesser the attrition probability

#  YearsSinceLastPromotion            0.50694    0.07627   6.647 3.00e-11 ***
# Positive coefficient    :   Lesser the promotion = higher attrition probability

#  YearsWithCurrManager              -0.49320    0.08661  -5.694 1.24e-08 ***
# Negative coefficient   : frequently changing the manager = higher attrition probability

#  Total.logged.hours                 0.64084    0.05395  11.878  < 2e-16 ***
# Positive coefficient    :   Lesser the logged hours = higher attrition probability

#  BusinessTravel.xTRAVEL_FREQUENTLY  0.33455    0.05033   6.647 2.98e-11 ***
# Positive coefficient    :   Higher Business Travel = higher attrition probability

#  MaritalStatus.xSINGLE              0.49690    0.05275   9.420  < 2e-16 ***
# Positive coefficient    :   Employees in Single status = higher attrition probability

#  EnvironmentSatisfaction.xLOW       0.42791    0.05059   8.458  < 2e-16 ***
# Positive coefficient    :   Lower the Employee Statisfaction = higher attrition probability

#  JobSatisfaction.xLOW               0.25880    0.05353   4.835 1.33e-06 ***
# Positive coefficient    :   Lower the Job Statisfaction = higher attrition probability

#  JobSatisfaction.xVERY.HIGH        -0.25696    0.06380  -4.027 5.64e-05 ***
# Negative coefficient   : Higher the Job Statisfaction = higher attrition probability

#------------------------------------------------------------------------------
# Model Evaluation Conclusions :
#------------------------------------------------------------------------------
# Based on the above comparisons, we have concluded that KS Statistics is a better
# metric for evaluating model. The confusionMatrix for KS Statistics states as follows

#confusionMatrix_2$table
#Reference
#Prediction    0    1
#0            1083  167
#1            20   45

#AUC we can conclude that "Accuracy : 0.7793538"


#------------------------------------------------------------------------------
#References: 
#  https://www.r-bloggers.com/how-to-perform-a-logistic-regression-in-r/
#  https://www.analyticsvidhya.com/blog/2015/11/beginners-guide-on-logistic-regression-in-r/
#  https://www.analyticsvidhya.com/blog/2016/02/7-important-model-evaluation-error-metrics/


