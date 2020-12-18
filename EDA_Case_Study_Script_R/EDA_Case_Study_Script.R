#importing loan data
raw_loan_data <- read.csv("loan.csv")
#--------------------------------------------------------------------------
#--------------------------------EDA Case Study ---------------------------
#--------------------------------------------------------------------------
#.....................................................
#Required Packages
#.....................................................
#Validating the required packages and installing if any package is not available
list.of.packages <- c("ggplot2", "ggpubr","maps","gridExtra","ggthemes")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(ggplot2)
library(ggpubr)
library(plyr)
library(dplyr)
library(tidyr)
library(stringr)
library(maps)
library(gridExtra)


#--------------------------------------------------------------------------
#.....................................................
#loading into stage table, cleaning data.
#.....................................................
#Removing % sign from int_rate for using for better analysis.
loan_data <- raw_loan_data
loan_data$int_rate <- gsub("%", "", loan_data$int_rate)
#--------------------------------------------------------------------------
#........................
# Handling Date columns.
#........................

loan_data <- loan_data %>%
  separate(issue_d, c("issued_d_month", "issued_d_year"), "-")

loan_data$issued_d_year <- paste("20",loan_data$issued_d_year,sep="")

#--------------------------------------------------------------------------
#..................
#derive columns
#..................
# Employee Length Range column
#Assumbtion is that < 1 year : cosidered as a 0.5 and 10+ years : 11 years 
#for the employee length column.
loan_data$emp_length_value <- gsub(" years", "", loan_data$emp_length)
loan_data$emp_length_value <- gsub(" year", "", loan_data$emp_length_value)
loan_data$emp_length_value <- gsub("10+", "11", loan_data$emp_length_value)
loan_data$emp_length_value <- str_replace_all(loan_data$emp_length_value, "[+]", "")
loan_data$emp_length_value <- gsub("< 1", "0.5", loan_data$emp_length_value)
loan_data$emp_length_value <- as.factor(loan_data$emp_length_value)

#
state_code <- c("AZ","GA","IL","CA","OR","NC","TX","VA","MO","CT","UT","FL","NY",
                "PA","MN","NJ","KY","OH","SC","RI","LA","MA","WA","WI","AL","CO","KS",
                "NV","AK","MD","WV","VT","MI","DC","SD","NH","AR","NM","MT","HI","WY",
                "OK","DE","MS","TN","IA","NE","ID","IN","ME")
                
region_value <- c("West","South","Midwest","West","West","South","South","South",
                  "Midwest","NorthEast","West","South","NorthEast","NorthEast",
                  "Midwest","NorthEast","South","Midwest","South","NorthEast","South",
                  "NorthEast","West","Midwest","South","West","Midwest","West","West",
                  "South","South","NorthEast","West","South","Midwest","NorthEast",
                  "South","West","West","West","West","South","South","South","South",
                  "Midwest","Midwest","West","Midwest","NorthEast")

state <- c("Arizona","Georgia","Illinois","California","Oregon","North Carolina",
           "Texas","Virginia","Missouri","Connecticut","Utah","Florida","New York",
           "Pennsylvania","Minnesota","New Jersey","Kentucky","Ohio","South Carolina",
           "Rhode Island","Louisiana","Massachusetts","Washington","Wisconsin",
           "Alabama","Colorado","Kansas","Nevada","Alaska","Maryland","West Virginia",
           "Vermont","Michigan","Washington DC","South Dakota","New Hampshire",
           "Arkansas","New Mexico","Montana","Hawaii","Wyoming","Oklahoma","Delaware",
           "Mississippi","Tennessee","Iowa","Nebraska","Idaho","Indiana","Maine") 
                
#preparing data frame with vector data for state code and region mapping
state_region_mapping <- data.frame(cbind(state_code, region_value, state))

loan_data <- merge(loan_data, state_region_mapping, by.x = "addr_state", by.y = "state_code")

#..........................................................................
#comparing the data for loan_amt and funded_amt, this looks like same data.
#library(dplyr)
#validating_data <- dplyr::filter(loan_data, loan_data$loan_amnt != loan_data$funded_amnt)  
# This is different data

#..........................
#Converted continuous variables to range of values to enhance interpretation of 
#results (E.g. loan_amt, int_rate, Annual_income, revol_util, 
#total_pymnt etc.)

#Lending Club enables borrowers to create unsecured personal loans between $1,000 and $40,000. 
#The standard loan period is three years.
#https://en.wikipedia.org/wiki/Lending_Club
#Based on the above details, creating range of 5000, so we can see the distribution


loan_data %>% 
  plyr::mutate(loan_amnt_range =
                 dplyr::case_when(loan_amnt >= 0 &  loan_amnt < 5000  ~ "0-5000",
                                  loan_amnt >= 5000 &  loan_amnt < 10000  ~ "5000-10000",
                                  loan_amnt >= 10000 &  loan_amnt < 15000  ~ "10000-15000",
                                  loan_amnt >= 15000 &  loan_amnt < 20000  ~ "15000-20000",
                                  loan_amnt >= 20000 &  loan_amnt < 25000  ~ "20000-25000",
                                  loan_amnt >= 25000 &  loan_amnt < 30000  ~ "25000-30000",
                                  loan_amnt >= 30000 &  loan_amnt < 35000  ~ "30000-35000",
                                  loan_amnt >= 35000 &  loan_amnt < 40000  ~ "35000-40000",
                                  TRUE                     ~ "undetermined"
                 )) -> loan_data

# creating a new column for intrest rate range [this is in percentage]
loan_data %>% 
  plyr::mutate(int_rate_bin =
                 dplyr::case_when(as.numeric(int_rate) >= 0 &  as.numeric(int_rate) < 5  ~ "5",
                                  as.numeric(int_rate) >= 5 &  as.numeric(int_rate) < 10  ~ "10",
                                  as.numeric(int_rate) >= 10 &  as.numeric(int_rate) < 15  ~ "15",
                                  as.numeric(int_rate) >= 15 &  as.numeric(int_rate) < 20  ~ "20",
                                  as.numeric(int_rate) >= 20 &  as.numeric(int_rate) < 25  ~ "25",
                                  as.numeric(int_rate) >= 25 &  as.numeric(int_rate) < 30  ~ "30",
                                  TRUE                     ~ "undetermined"
                 )) -> loan_data

#driving range column for intrest rate.
loan_data$int_rate_range <- round(as.numeric(loan_data$int_rate))

#creating annual income bin
loan_data %>% 
  plyr::mutate(annual_income_bin =
                 dplyr::case_when(annual_inc >= 10000 &  annual_inc < 20000  ~ "0-20K",
                                  annual_inc >= 20000 &  annual_inc < 30000  ~ "21-30K",
                                  annual_inc >= 30000 &  annual_inc < 40000  ~ "31-40K",
                                  annual_inc >= 40000 &  annual_inc < 50000  ~ "41-50K",
                                  annual_inc >= 50000 &  annual_inc < 60000  ~ "51-60K",
                                  annual_inc >= 60000 &  annual_inc < 70000  ~ "61-70K",
                                  annual_inc >= 70000 &  annual_inc < 80000  ~ "71-80K",
                                  annual_inc >= 80000 &  annual_inc < 90000  ~ "81-90K",
                                  annual_inc >= 90000 &  annual_inc < 100000  ~ "91-1M",
                                  annual_inc > 100000       ~ "Greater than 1M",
                                  TRUE                     ~ "Not Displayed"
                 )) -> loan_data

#
#removing column based on the analysis, the below columns not having any data
#and these columns will not 
droping_column_list <- c("mths_since_last_major_derog","annual_inc_joint","dti_joint",
           "verification_status_joint","tot_coll_amt","tot_cur_bal","open_acc_6m",
           "open_il_6m","open_il_12m","open_il_24m",
           "mths_since_rcnt_il","total_bal_il","il_util",
           "open_rv_12m","open_rv_24m",
           "max_bal_bc","all_util","total_rev_hi_lim","inq_fi","total_cu_tl","inq_last_12m",
           "acc_open_past_24mths","avg_cur_bal","bc_open_to_buy","bc_util",
           "chargeoff_within_12_mths","delinq_amnt","mo_sin_old_il_acct",
           "mo_sin_old_rev_tl_op","mo_sin_rcnt_rev_tl_op","mo_sin_rcnt_tl",
           "mort_acc","mths_since_recent_bc","mths_since_recent_bc_dlq",
           "mths_since_recent_inq","mths_since_recent_revol_delinq",
           "num_accts_ever_120_pd","num_actv_bc_tl","num_actv_rev_tl",
           "num_bc_sats","num_bc_tl","num_il_tl","num_op_rev_tl",
           "num_rev_accts","num_rev_tl_bal_gt_0","num_sats","num_tl_120dpd_2m",
           "num_tl_30dpd","num_tl_90g_dpd_24m","num_tl_op_past_12m",
           "pct_tl_nvr_dlq","percent_bc_gt_75","tax_liens",
           "tot_hi_cred_lim","total_bal_ex_mort","total_bc_limit",
           "total_il_high_credit_limit")

#only have the selected column in the dataframe, dropped the above listed column in this
loan_data <- loan_data[ , !(names(loan_data) %in% droping_column_list)]

#Similar Distributions:
#  We will start by exploring the distribution of the loan amounts and see when did the loan amount issued increased significantly.

#------------------------ Plots ---------------------------------
#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
theme_set(theme_pubr())
# loan count by grade
loan_count_by_grade <- count(loan_data, grade)
#Loan Count By Grade
loan_count_by_grade_chart <- ggplot(data = loan_count_by_grade, aes(x = grade, y = n)) + 
  geom_bar(stat = "identity" , aes(fill =grade)) + 
  labs(title="Loan Count By Grade"
       , x = "Grade" 
       , y = "Number of Loans") + geom_text(aes(label = n), vjust = -0.3) + theme_pubclean()
#---------------------------------------------------------------------------
#Loan Count By SubGrade
loan_count_by_subgrade <- count(loan_data, grade, sub_grade)
#Loan Count By SubGrade
loan_count_by_Subgrade_chart <- ggplot(data = loan_count_by_subgrade, aes(x = sub_grade, y = n)) + 
  geom_bar(stat = "identity" , aes(fill =grade)) + 
  labs(title="Loan Count By SubGrade"
       , subtitle="Chart explains how the distribution of Loan Count accross the Dataset"
       , x = "Sub Grade" 
       , y = "Number of Loans") + geom_text(aes(label = n), vjust = -0.3) + theme_pubclean()
#---------------------------------------------------------------------------
#--- with box plot : Loan Count Summary By Grade
loan_count_sumarry_by_grade_plot <- ggplot(data = loan_count_by_subgrade, aes(x = grade, y = n)) + 
  #geom_bar(stat = "identity", position = "dodge", aes(fill = loan_count_by_subgrade$sub_grade)) + 
  geom_boxplot(width = 0.9, aes(fill =grade)) +
  #geom_jitter(aes(color = sub_grade, shape = sub_grade), width = 0.1, size = 1) +
  labs(title="Loan Count Summary By Grade"
       , subtitle="Boxplot explains how the distribution of Loan Count accross Grade and Subgrade for the Dataset"
       , x = "Grade" 
       , y = "Number of Loans") + theme_pubclean() + 
  guides(fill=guide_legend(title="Grade"))
#-----------------------------------------------------------------------------
#Printing Above charts
library(gridExtra)

grid.arrange( loan_count_by_grade_chart, 
              loan_count_by_Subgrade_chart, 
              nrow=2)

loan_count_sumarry_by_grade_plot

dev.off()
#-----------------------------------------------------------------------------
#Loan Count By Year
loan_count_by_year <- count(loan_data, issued_d_year)
#Loan Count By Year Chart
loan_count_by_year_chart <- ggplot(data = loan_count_by_year, aes(x = issued_d_year, y = n)) + 
  geom_bar(stat = "identity" , fill = "#0073C2FF") + 
  labs(title="Loan Count By Year"
       , subtitle="Chart explains how the distribution of Loan Count accross the Dataset"
       , x = "Year" 
       , y = "Number of Loans") + geom_text(aes(label = n), vjust = -0.3) + theme_pubclean()

loan_count_by_year_chart
#-----------------------------------------------------------------------------
#Average Loan Amount By Year Chart
Average_loan_amount_by_year_chart <- ggplot(data = loan_data, aes(x = issued_d_year, y = mean(loan_amnt))) + 
  geom_bar(stat = "identity", fill = "#E7B800") + 
  labs(title="Average Loan Amount By Year"
       , subtitle="Chart explains how the distribution of Loan amount accross the Dataset"
       , x = "Year" 
       , y = "Average Loan Amount") + theme_pubclean()

Average_loan_amount_by_year_chart
#-----------------------------------------------------------------------------
#Loan Count By Year and Term
loan_count_by_year_and_term <- count(loan_data, issued_d_year, term)
#Loan Count By Year and Term Chart
loan_count_by_year_and_term_chart <- ggplot(data = loan_count_by_year_and_term, aes(x = issued_d_year, y = n, col = term)) + 
  geom_bar(stat = "identity", position = "dodge", aes(fill =term)) +
  labs(title="Loan Count By Year and Term"
       , subtitle="Chart explains how the distribution of Loan Count accross the Dataset"
       , x = "Year"
       , y = "Number of Loans"
       , fill = "term") + theme_pubclean()

loan_count_by_year_and_term_chart
#-----------------------------------------------------------------------------
#Loan Count By Purpose
loan_request_purpose <- count(loan_data, purpose)
#Loan Count By Purpose Chart
loan_request_purpose_chart <- ggplot(data = loan_request_purpose, aes(x = purpose, y = n)) + 
  geom_bar(stat = "identity", fill = "#0073C2FF") +
  labs(title="Loan Count By Purpose"
       , subtitle="Chart explains how the distribution of Loan Count accross the Dataset"
       , x = "Purpose"
       , y = "Number of Loans"
       ) + theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 11))

loan_request_purpose_chart
#-----------------------------------------------------------------------------------
#purpose and intrest rate
#Correlation between the purpose and the interest rate of loan using a box plot
loan_request_purpose_and_intrest_rate_plot <- ggplot(loan_data, aes(x=purpose, y=as.numeric(loan_data$int_rate), fill=as.numeric(loan_data$int_rate))) +
  geom_boxplot(aes(fill =purpose)) +
  labs(title="Correlation between the purpose and the interest rate of loan using a box plot"
       , x = "Purpose"
       , y = "Intrest Rate"
  ) + theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 11)) +
  guides(fill=guide_legend(title="Intrest Rate"))

loan_request_purpose_and_intrest_rate_plot
#-----------------------------------------------------------------------------------
# loan request purpose and status
loan_request_purpose_and_status <- count(loan_data, purpose, loan_status)
# Loan Count By Purpose and Status Chart
loan_request_purpose_and_status_chart <- ggplot(data = loan_request_purpose_and_status, aes(x = purpose, y = n, fill = loan_status)) + 
  geom_bar(stat = "identity", position="dodge") +
  labs(title="Loan Count By Purpose and Status"
       , subtitle="Chart explains how the distribution of Loan Count accross the Dataset"
       , x = "Purpose"
       , y = "Number of Loans"
  ) + theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 11))

loan_request_purpose_and_status_chart
#-----------------------------------------------------------------------------
#Loan count by occupation
loan_count_requester_occupations_loan <- count(loan_data, emp_title)

#replaceing Blank to NA
loan_count_requester_occupations_loan$emp_title[loan_count_requester_occupations_loan$emp_title==""] <- "NA"

#removing na values
library(tidyr)
loan_count_requester_occupations_loan <- loan_count_requester_occupations_loan %>% drop_na()

#Identify the top 10 Employee Titles 
loan_count_requester_occupations_loan_top30 <- dplyr::top_n(dplyr::arrange(loan_count_requester_occupations_loan, desc(n)), 30, n)

#Loan Count By Occupation Chart
loan_count_requester_occupations_loan_chart <- ggplot(data = loan_count_requester_occupations_loan_top30, aes(x = emp_title, y = n)) + 
  geom_bar(stat = "identity", fill = "#0073C2FF") +
  labs(title="Top 30 Loan Count By Occupation"
       , subtitle="Chart explains how the distribution of Loan Count accross the Dataset"
       , x = "Occupation"
       , y = "Number of Loans"
  ) + theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 11))

loan_count_requester_occupations_loan_chart
# ----------------------------------------------------------------------------
#Number of Loan Count By State
loan_request_by_state <- count(loan_data, issued_d_year, addr_state)
#Loan Count By State and Issued Year Chart
loan_request_by_state_chart <- ggplot(data = loan_request_by_state, aes(x = addr_state, y = n, fill = issued_d_year)) + 
  geom_bar(stat = "identity") + 
  labs(title="Loan Count By State and Issued Year"
       , subtitle="Chart explains how the distribution of Loan Count by State."
       , x = "State Code" 
       , y = "Number Of Loans") + theme_pubclean() + guides(fill=guide_legend(title="Issued Year"))

loan_request_by_state_chart
#----------------------------------------------------------------------
# Map shows Loan Count By State
#Number_of_Loan_By_State <- count(loan_data, addr_state)
#Number_of_Loan_By_State <- merge(Number_of_Loan_By_State, state_region_mapping, by.x = "addr_state", by.y = "state_code")
#Number_of_Loan_By_State$state <- tolower(Number_of_Loan_By_State$state)
#library(maps)
#library(ggplot2)
#map <- map_data("state")

#Number_of_Loan_By_State_Map <- ggplot(Number_of_Loan_By_State, aes(fill = n))
#Number_of_Loan_By_State_Map + geom_map(aes(map_id = state), map = map) +
#  expand_limits(x = map$long, y = map$lat)  + 
#  labs(title="Loan Count By State"
#       , subtitle="Map Shows, how the distribution of Loan Count by State.") + guides(fill=guide_legend(title="Number of Loans"))
#-----------------------------------------------------------------------
#Average Intrest Rate by Region and Issued Year
average_intrest <- loan_data %>%
  group_by(issued_d_year,region_value) %>%
  summarise(n = n(), 
            average_intrest_rate_amount = mean((as.numeric(int_rate)/100)*loan_amnt)
            )
#Average Intrest Amount by Region and Issued Year Chart
average_intrest_amount_by_region_and_year_chart <- ggplot(data = average_intrest, aes(x = region_value, y = average_intrest$average_intrest_rate_amount, fill = issued_d_year)) + 
  geom_bar(stat = "identity", position="dodge") + 
  labs(title="Average Intrest Amount by Region and Issued Year"
       , subtitle="Chart explains how the distribution of Average Intrest Amount by Region."
       , x = "Region" 
       , y = "Average Intrest Amount") + theme_pubclean() + guides(fill=guide_legend(title="Issued Year"))

average_intrest_amount_by_region_and_year_chart
#------------------------------------------------------Loan amount analysis by region
#average loan amount by region
Average_Loan_Amount_by_intrest_rate_and_region_chart <- ggplot(data = loan_data, aes(x = factor(int_rate_range), y = mean(loan_amnt), fill = loan_data$region_value)) + 
  geom_bar(stat = "identity", position="fill") + 
  labs(title="Average loan amount by intrest rate and Region"
       , subtitle="Chart explains how the distribution of Average Intrest Amount by Region."
       , x = "Intrest Rate" 
       , y = "Average Loan Amount"
       , size = .3) + theme_pubclean() + guides(fill=guide_legend(title="Region"))

Average_Loan_Amount_by_intrest_rate_and_region_chart
#-------------------------------------------------------------------------------------
#Average Invester Funded amount by intrest rate and Region Chart
Average_Invester_Funded_Amount_by_intrest_rate_and_region_chart <- ggplot(data = loan_data, aes(x = factor(int_rate_range), y = mean(funded_amnt_inv), fill = loan_data$region_value)) + 
  geom_bar(stat = "identity", position="fill") + 
  labs(title="Average Invester Funded amount by intrest rate and Region"
       , subtitle="Chart explains how the distribution of Average Invester Funded Amount by Region."
       , x = "Intrest Rate" 
       , y = "Average Invester Funded Amount") + guides(fill=FALSE)#theme_pubclean() + guides(fill=guide_legend(title="Region"))

Average_Invester_Funded_Amount_by_intrest_rate_and_region_chart 
#-------------------------------------------------------------------------------------
#Average Amount Invester Received amount by intrest rate and Region Chart
Average_Amount_Invester_Received_by_intrest_rate_and_region_chart <- ggplot(data = loan_data, aes(x = factor(int_rate_range), y = mean(loan_data$total_pymnt_inv), fill = loan_data$region_value)) + 
  geom_bar(stat = "identity", position="fill") + 
  labs(title="Average Amount Invester Received amount by intrest rate and Region"
       , subtitle="Chart explains how the distribution of Average Amount Invester Received Amount by Region."
       , x = "Intrest Rate" 
       , y = "Average Amount Received by Invester") + theme_pubclean() + guides(fill=FALSE)#guides(fill=guide_legend(title="Region"))

Average_Amount_Invester_Received_by_intrest_rate_and_region_chart 
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
library(gridExtra)
#The below chart displays the Loan Amount vs Inverster Funded Amount Vs Invester Received
# Amount by intrest rate and Region
grid.arrange( Average_Loan_Amount_by_intrest_rate_and_region_chart,
              Average_Invester_Funded_Amount_by_intrest_rate_and_region_chart,
              Average_Amount_Invester_Received_by_intrest_rate_and_region_chart,
              nrow=3)
#dev.off()
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#Average loan amount by intrest rate and Employee Length
Average_Loan_Amount_by_intrest_rate_and_employee_length_chart <- ggplot(data = loan_data, aes(x = factor(loan_data$int_rate_range), y = mean(loan_amnt), fill = loan_data$emp_length_value)) + 
  geom_bar(stat = "identity", position="fill") + 
  labs(title="Average loan amount by intrest rate and Employee Length"
       , subtitle="Chart explains how the distribution of Average Loan Amount by Employee Length"
       , x = "Intrest Rate" 
       , y = "Average Loan Amount"
       , size = .3) + theme_pubclean() + guides(fill=guide_legend(title="Employee Length"))

Average_Loan_Amount_by_intrest_rate_and_employee_length_chart
#--------------------------------------------------------------------------------------
#Average_Amount_Invester_Received_by_intrest_rate_and_employee_length_chart
Average_Amount_Invester_Received_by_intrest_rate_and_employee_length_chart <- ggplot(data = loan_data, aes(x = factor(int_rate_range), y = mean(loan_data$loan_amnt), fill = loan_data$emp_length_value)) + 
  geom_bar(stat = "identity", position="fill") + 
  labs(title="Average Amount Invester Received amount by intrest rate and Region"
       , subtitle="Chart explains how the distribution of Average Amount Invester Received Amount by Employee Length"
       , x = "Intrest Rate" 
       , y = "Average Amount Received by Invester") + theme_pubclean() + guides(fill=FALSE)#guides(fill=guide_legend(title="Region"))

Average_Amount_Invester_Received_by_intrest_rate_and_employee_length_chart
#--------------------------------------------------------------------------------------
#..................................................................................
grid.arrange( Average_Loan_Amount_by_intrest_rate_and_employee_length_chart,
              Average_Amount_Invester_Received_by_intrest_rate_and_employee_length_chart,
              nrow=2)
# .................................................................................
# Analysis on DTI (Dept To Income).................................................
#..................................................................................
loan_data$dti_range <- round(loan_data$dti)

#
#Average DTI based on Member Subgrade and anual income range.
Average_DTI_by_subgrade_and_anual_income_range_chart <- ggplot(data = loan_data, aes(x = factor(loan_data$dti_range), y = loan_data$annual_inc, fill = loan_data$sub_grade)) + 
  geom_bar(stat = "identity", position="fill") + 
  labs(title="Average DTI based on Member Subgrade and anual income range"
       , x = "DTI" 
       , y = "Annual Income"
       , size = .3) + theme_pubclean() + guides(fill=guide_legend(title="SubGrade"))

Average_DTI_by_subgrade_and_anual_income_range_chart
#---------------------------------------------------------------------------------------
#Average DTI based on Member Subgrade and anual income range
Average_DTI_by_status_and_anual_income_range_chart <- ggplot(data = loan_data, aes(x = factor(loan_data$dti_range), y = loan_data$annual_inc, fill = loan_data$loan_status)) + 
  geom_bar(stat = "identity", position="fill") + 
  labs(title="Average DTI based on Member Subgrade and anual income range"
       , x = "DTI" 
       , y = "Annual Income"
       , size = .3) + theme_pubclean() + guides(fill=guide_legend(title="Loan Status"))

Average_DTI_by_status_and_anual_income_range_chart
#----------------------------------------------------------------------------------------
#Correlation between the delinq of 2years and the interest rate of loan using a box plot
intrest_rate_vs_delinq_2yrs_plot <- ggplot(loan_data, aes(x=factor(loan_data$delinq_2yrs), y=as.numeric(loan_data$int_rate), fill=as.numeric(loan_data$int_rate))) +
  geom_boxplot(aes(fill =delinq_2yrs)) +
  labs(title="Correlation between the delinq of 2years and the interest rate of loan using a box plot"
       , x = "delinq_2yrs"
       , y = "Intrest Rate"
  ) + theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 11)) +
  guides(fill=guide_legend(title="Intrest Rate"))

intrest_rate_vs_delinq_2yrs_plot
#----------------------------------------------------------------------------------------
#Correlation between the inq_last_6mths and the interest rate of loan using a box plot
intrest_rate_vs_inq_last_6mths_plot <- ggplot(loan_data, aes(x=factor(loan_data$inq_last_6mths), y=as.numeric(loan_data$int_rate), fill=as.numeric(loan_data$int_rate))) +
  geom_boxplot(aes(fill =inq_last_6mths)) +
  labs(title="Correlation between the inq_last_6mths and the interest rate of loan using a box plot"
       , x = "inq_last_6mths"
       , y = "Intrest Rate"
  ) + theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 11)) +
  guides(fill=guide_legend(title="Intrest Rate"))

intrest_rate_vs_inq_last_6mths_plot
#-----------------------------------------------------------------------------------------
#income Range by loan count
Income_Range_By_Loan_Count <- count(loan_data, annual_income_bin)
#Loan Count By Annual Income Chart
Income_Range_By_Loan_Count <- ggplot(data = Income_Range_By_Loan_Count, aes(x = annual_income_bin, y = n)) + 
  geom_bar(stat = "identity" , fill = "#0073C2FF") + 
  labs(title="Loan Count By Annual Income"
       , subtitle="Chart explains how the distribution of Loan Count accross the Dataset"
       , x = "Annual Income Range" 
       , y = "Number of Loans") + geom_text(aes(label = n), vjust = -0.3) + theme_pubclean()

Income_Range_By_Loan_Count
#-----------------------------------------------------------------------------------------
#income Range by loan count
Income_Range_By_Loan_and_status_Count <- count(loan_data, loan_status, annual_income_bin)
#Correlation of Loan Count By Annual Income and Status Chart
Income_Range_By_Loan_Count_and_Status <- ggplot(data = Income_Range_By_Loan_and_status_Count, aes(x = annual_income_bin, y = n), fill=as.numeric(loan_status)) + 
  geom_bar(stat = "identity" , aes(fill =loan_status)) +
  labs(title="Correlation of Loan Count By Annual Income and Status Chart"
       , subtitle="Chart explains how the distribution of Loan Count accross and Status the Dataset"
       , x = "Annual Income Range" 
       , y = "Number of Loans") + theme_pubclean() + guides(fill=guide_legend(title="Loan Status"))

Income_Range_By_Loan_Count_and_Status
#-----------------------------------------------------------------------------------------
loan_status_Current_data <- dplyr::filter(loan_data, loan_status == 'Current')
#----------------------------------------------------------------------
#=========================================================================================
#=========================================================================================
#Correlations between "employment length", "rate" and "status" of loans
#=========================================================================================
#Average loan amount by intrest rate and Employee Length Chart
intrest_rate_and_employee_length_chart <- ggplot(data = loan_data, aes(x = factor(loan_data$int_rate_bin), y = mean(loan_amnt), fill = loan_data$emp_length_value)) + 
  geom_bar(stat = "identity", position="fill") + 
  labs(title="Average loan amount by intrest rate and Employee Length"
       , subtitle="Chart explains how the distribution of Average Loan Amount by Employee Length"
       , x = "Intrest Rate" 
       , y = "Average Loan Amount"
       , size = .3) + theme_pubclean() + guides(fill=guide_legend(title="Employee Length"))

intrest_rate_and_employee_length_chart

#=========================================================================================
#=========================================================================================

######################################## Code #####################################

#### Data set preparation #######
loan <- loan_data

loan_36term <- loan %>% 
  filter(term==" 36 months")
loan_60term <- loan %>% 
  filter(term==" 60 months") 



####################################### Plots #########################
###### Term Analysis #############

loan_36term_Chart1<-  ggplot(loan_36term, aes(x = term ))  + 
  geom_bar(aes(fill = loan_status),position = "fill") + 
  labs(title="    Term 36 By Status",  x = "Term 36" , y = "Loan Satus %") 

loan_60term_Chart1<- ggplot(loan_60term, aes(x = term ))  + 
  geom_bar(aes(fill = loan_status), position = "fill") + 
  labs(title="    Term 60 By Status",  x = "Term 60" , y = "Loan Satus %") 

grid.arrange( loan_36term_Chart1,loan_60term_Chart1,nrow=2)

###### Sub Grade Analysis ##################

subgrade_36<- ggplot(loan_36term, aes(x = sub_grade ))  + geom_bar(aes(fill = loan_status), position = "fill")+ labs(title="    Term 36 Sub Grade By Status",  x = "Sub Grade" , y = "Loan Satus %") 
subgrade_60<-ggplot(loan_60term, aes(x = sub_grade ))  + geom_bar(aes(fill = loan_status), position = "fill")+ labs(title="     Term 60 Sub Grade By Status",  x = "Sub Grade" , y = "Loan Satus %") 
grid.arrange( subgrade_36,subgrade_60,nrow=2) 

subgrade_36_stack<- ggplot(loan_36term, aes(x = sub_grade ))  + geom_bar(aes(fill = loan_status), position = "stack")+ labs(title="    Term 36 Sub Grade By Status",  x = "Sub Grade" , y = "No. Of Loan") 
subgrade_60_stack<-ggplot(loan_60term, aes(x = sub_grade ))  + geom_bar(aes(fill = loan_status), position = "stack")+ labs(title="     Term 60 Sub Grade By Status",  x = "Sub Grade" , y = "No. Of Loan") 
grid.arrange( subgrade_36_stack,subgrade_60_stack,nrow=2)                


####### Grade Analysis #################
grade_36<- ggplot(loan_36term, aes(x = grade ))  + geom_bar(aes(fill = loan_status), position = "fill")+ labs(title="    Term 36 Grade By Status",  x = "Grade" , y = "Loan Satus %") 
grade_60<-ggplot(loan_60term, aes(x = grade ))  + geom_bar(aes(fill = loan_status), position = "fill")+ labs(title="     Term 60 Grade By Status",  x = "Grade" , y = "Loan Satus %") 
grid.arrange( grade_36,grade_60,nrow=2) 


######## EMPloyee Length ###############

length_36<-ggplot(loan_36term, aes(x = emp_length ))  + geom_bar(aes(fill = loan_status), position = "fill") + labs(title="   Term 36 experience By Status",  x = "Employee Length" , y = "Loan Satus %") 
length_60<- ggplot(loan_60term, aes(x = emp_length ))  + geom_bar(aes(fill = loan_status), position = "fill") + labs(title="    Term 60 experience By Status",  x = "Employee Length" , y = "Loan Satus %") 
grid.arrange( length_36,length_60,nrow=2) 

######## Brower City ############ 36 NE
City_36<-ggplot(loan_36term, aes(x = addr_state ))  + geom_bar(aes(fill = loan_status), position = "dodge") + labs(title="   Term 36 State By Status",  x = "Borrower State" , y = "Number Of Loan Satus") 
City_60<- ggplot(loan_60term, aes(x = addr_state ))  + geom_bar(aes(fill = loan_status), position = "dodge") + labs(title="    Term 60 State By Status",  x = "Borrower State" , y = "Number Of Loan Satus") 
grid.arrange( City_36,City_60,nrow=2)

######## Home Ownership #######
home_36<-ggplot(loan_36term, aes(x = home_ownership))  + geom_bar(aes(fill = loan_status), position = "dodge") + labs(title="   Term 36 Home Ownership By Status",  x = "Home Ownership" , y = "Number Of Loan") 
home_60<- ggplot(loan_60term, aes(x = home_ownership ))  + geom_bar(aes(fill = loan_status), position = "dodge") + labs(title="    Term 60 Home Ownership By Status",  x = "Home Ownership" , y = "Number Of Loan") 
grid.arrange( home_36,home_60,nrow=2)

#########Purpose #####
purpose_36 <- ggplot(loan_36term, aes(x = purpose, fill = loan_status )) + geom_bar(position="stack") + labs(title="    Term 36 Purpose By Status",  x = "Pupose" , y = "Loan Satus %") 
purpose_60 <- ggplot(loan_60term, aes(x = purpose, fill = loan_status )) + geom_bar(position="stack") + labs(title="    Term 60 Purpose By Status",  x = "Purpose" , y = "Loan Satus %") 
grid.arrange( purpose_36,purpose_60,nrow=2)

###### verification Status #####
purpose_36 <- ggplot(loan_36term, aes(x = verification_status, fill = loan_status )) + geom_bar(position="fill") + labs(title="    Term 36 Verification Status By Loan Status",  x = "Verification Status" , y = "Loan Satus %") 
purpose_60 <- ggplot(loan_60term, aes(x = verification_status, fill = loan_status )) + geom_bar(position="fill") + labs(title="    Term 60 Verification Status By Loan Status",  x = "Verification Status" , y = "Loan Satus %") 
grid.arrange( purpose_36,purpose_60,nrow=2)

#=========================================================================================
#=========================================================================================
