#------------------------------------------------------------------
#importing required libraries
library(dplyr)
library(tidyr)
#--------------------------------------------------------------------
#Checkpoint 1: Data Cleaning 1
#------------------------------------------------------------------
#importing companies txt data
companies <- read.delim("companies.txt",header = TRUE, sep = "\t",stringsAsFactors = F)
#******************************************************
#write.csv(companies, "companies.csv")
#******************************************************
#importing round2 csv file
rounds2 <- read.csv("rounds2.csv",stringsAsFactors = F)
#-----------------------------------------------------------
#------------------------------------ preparing English Speaking country dataframe:
#continet vector
continent = c("Africa","Africa","Africa","Africa","Africa","Africa", 
              "Africa","Africa","Africa","Africa","Africa","Africa","Africa", 
              "Africa","Africa","Africa","Africa","Africa","Africa","Africa","Africa", 
              "Africa","Africa","Africa","Americas","Americas","Americas","Americas",
              "Americas","Americas","Americas","Americas","Americas","Americas","Americas",
              "Americas","Americas","Americas","Asia","Asia","Asia","Asia","Australia/Oceania",
              "Australia/Oceania","Australia/Oceania","Australia/Oceania","Australia/Oceania",
              "Australia/Oceania","Australia/Oceania","Australia/Oceania","Australia/Oceania",
              "Australia/Oceania","Australia/Oceania","Australia/Oceania","Australia/Oceania",
              "Australia/Oceania","Europe","Europe","Europe")
#country vector
country = c("Botswana","Cameroon","Ethiopia","Eritrea","Gambia","Ghana","Kenya",
            "Lesotho","Liberia","Malawi","Mauritius","Namibia","Nigeria","Rwanda","Seychelles",
            "Sierra Leone","South Africa","South Sudan","Sudan","Swaziland","Tanzania","Uganda",
            "Zambia","Zimbabwe","Antigua and Barbuda","Bahamas","Barbados","Belize","Canada",
            "Dominica","Grenada","Guyana","Jamaica","Saint Kitts and Nevis","Saint Lucia",
            "Saint Vincent and the Grenadines","Trinidad and Tobago","United States",
            "India","Pakistan","Philippines","Singapore","Australia","Fiji","Kiribati",
            "Marshall Islands","Federated States of Micronesia","Nauru","New Zealand",
            "Palau","Papua New Guinea","Samoa","Solomon Islands","Tonga","Tuvalu","Vanuatu",
            "Ireland","Malta","United Kingdom")
#country code vector
code = c("BWA","CMR","ETH","ERI","GMB","GHA","KEN","LSO","LBR","MWI","MUS",
         "NAM","NGA","RWA","SYC","SLE","ZAF","SSD","SDN","SWZ","TZA","UGA","ZMB",
         "ZWE","ATG","BHS","BRB","BLZ","CAN","DMA","GRD","GUY","JAM","KNA","LCA",
         "VCT","TTO","USA","IND","PAK","PHL","SGP","AUS","FJI","KIR","MHL","FSM",
         "NRU","NZL","PLW","PNG","WSM","SLB","TON","TUV","VUT","IRL","MLT","GBR")
#----
#preparing data frame with vector data for english speaking countries
english_countries <- data.frame(cbind(continent, country, code))
#-------------------------------
#How many unique companies are present in rounds2?
#updating the permalink for companies and rounds2 files to upper case
companies$permalink <- toupper(companies$permalink)
rounds2$company_permalink <- toupper(rounds2$company_permalink)
rounds2_unique_company <- merge(rounds2, companies, by.x = "company_permalink", by.y = "permalink")
rounds2_unique_company_count <- length(unique(rounds2_unique_company$name))
rounds2_unique_company_count
#How many unique companies are present in companies?
companies_unique_company_count <- length(unique(companies$name))
companies_unique_company_count
#In the companies data frame, which column can be used as the unique key for each company? Write the name of the column.
companies_unique_key <- length(unique(companies$permalink))
companies_unique_key
#Are there any companies in the rounds2 file which are not  present in companies ? Answer Y/N? : N
companies_not_in_rounds2 <- length(subset(rounds2$company_permalink, !(rounds2$company_permalink %in% companies$permalink))) > 0
companies_not_in_rounds2
#Merge the two data frames so that all  variables (columns) 
#in the companies frame are added to the rounds2 data frame.
#Name the merged frame master_frame. How many observations 
#are present in master_frame ?"
master_frame <- merge(rounds2
                      , companies
                      , by.x = "company_permalink"
                      , by.y = "permalink"
)
#--------------------------------------------------------------------
#Checkpoint 2: Funding Type Analysis
#--------------------------------------------------------------------
#This is the first of the three goals of data analysis - investment type analysis.
# cleaning up data
#updating NA with 0 for rounds2 dataset
master_frame$raised_amount_usd[is.na(master_frame$raised_amount_usd)] <- 0
#Calculate the average investment amount for each of the four 
#funding types (venture, angel, seed, and private equity) 
#and report the answers in Table 2.1
#identifying mean by funding round type
average_invesments <- aggregate(data = master_frame,
                                raised_amount_usd ~ funding_round_type,
                                FUN=mean ,
                                na.action = na.omit)
#filtering for "angel","seed","venture","private_equity"
#library(dplyr)
average_invesments <- dplyr::filter(average_invesments, average_invesments$funding_round_type %in% c("angel","seed","venture","private_equity"))  
dplyr::arrange(average_invesments,desc(raised_amount_usd))
#funding_round_type raised_amount_usd
#1     private_equity        62111788.2
#2            venture        10634054.4
#3              angel          764564.3
#4               seed          556606.7

#******************************************************
#write.csv(master_frame, "master_frame.csv")
#******************************************************
#Based on the average investment amount calculated above, 
#which investment type do you think is the most suitable for 
#Spark Funds?
print("we found that venture is suitable for Spark Funds")
#--------------------------------------------------------------------
#Checkpoint 3: Country Analysis
#--------------------------------------------------------------------
#This is the second goal of analysis - country analysis.

#Spark Funds wants to see the top nine countries 
#which have received the highest total funding 
#(across ALL sectors for the chosen investment type)

#For the chosen investment type, make a data frame 
#named top9 with the top nine countries 
#(based on the total investment amount each country has received)
#--------------------- venture
master_frame_venture <- dplyr::filter(master_frame, master_frame$funding_round_type %in% c("venture"))  
#importing English Countries having country name and  country code
#english_countries <- read.csv("english_countries.csv")
#adding country column
master_frame_venture <- merge(master_frame_venture, english_countries, by.x = "country_code", by.y = "code")
#filtering for english countries
#library(dplyr)
venture_eng_country <- dplyr::filter(master_frame_venture, master_frame_venture$country_code %in% english_countries$code)
#finding total invesment for english countries
venture_eng_country_total_invesment <- aggregate(data = venture_eng_country, raised_amount_usd ~ country, FUN=sum)
#names(data)
#Identify the top three English-speaking countries in 
#the data frame top9
top9 <- dplyr::top_n(dplyr::arrange(venture_eng_country_total_invesment, desc(raised_amount_usd)), 9, raised_amount_usd)
top9
#******************************************************
#write.csv(top9, "top9.csv")
#******************************************************
#Group.1            x
#1     USA 422510842796
#2     GBR  20245627416
#3     IND  14391858718
#4     CAN   9583332317
#5     SGP   2793917856
#6     IRL   1676131350
#7     AUS   1322934948
#8     NZL    448316383
#9     NGA    245440000
#----------------------------------------------------------------
#Top English speaking country
dplyr::slice(top9, 1:1)
#Second English speaking country
dplyr::slice(top9, 2:2)
#Third English speaking country
dplyr::slice(top9, 3:3)
#--------------------------------------------------------------------
#Checkpoint 4: Sector Analysis 1
#--------------------------------------------------------------------
#This is the third goal of analysis - sector analysis.

#reading mapping.csv file and storing into mapping variable
mapping <- read.csv("mapping.csv",stringsAsFactors = F)
#---------------------data cleaning steps
#------
#cleaning the data Fi0nce Technology' where 0 should be changed to na
#updating all the above patten data to valid text
mapping$category_list <- sub("0", "na", mapping$category_list)
#importing tidyr library
#library(tidyr)
# converting mapping dataset from wide to long formate for sectors 
mapping_data_long <- tidyr::gather(mapping, "main_sector", "main_sector_value", "Automotive...Sports":"Social..Finance..Analytics..Advertising")
#removing the 0 value rows from mapping_data (cleaning the long dataset)
#library(dplyr)
mapping_data_long <- dplyr::filter(mapping_data_long, main_sector_value > 0)
#-----------------------------------
# replacing the correct values to description
#unique(mapping_data_long$main_sector)
#
mapping_data_long$main_sector <- replace(
  mapping_data_long$main_sector,
  mapping_data_long$main_sector == "Automotive...Sports", "Automotive & Sports")
#
mapping_data_long$main_sector <- replace(
  mapping_data_long$main_sector,
  mapping_data_long$main_sector == "Cleantech...Semiconductors", "Cleantech / Semiconductors")
#
mapping_data_long$main_sector <- replace(
  mapping_data_long$main_sector,
  mapping_data_long$main_sector == "News..Search.and.Messaging", "News, Search and Messaging")
#
mapping_data_long$main_sector <- replace(
  mapping_data_long$main_sector,
  mapping_data_long$main_sector == "Social..Finance..Analytics..Advertising", "Social, Finance, Analytics, Advertising")
#-----------------------------------
#names(data)
#nrow(data)
#importing master to companies_data dataset
checkpoint4_master_frame <- venture_eng_country
#companies_data <- read.delim("companies.txt",header = TRUE, sep = "\t",stringsAsFactors = F)

#Extract the primary sector of each category list from the category_list column
primary_sector <- tidyr::extract(checkpoint4_master_frame, category_list, c("primary_sector") #, "sub_category1", "sub_category2","sub_category3"
                                 ,remove = F,  "([[^|]]+)|") #([[|^|]]+)|([[^|]]+)|([[^|]]+)

#Use the mapping file 'mapping.csv' to map each primary sector 
#to one of the eight main sectors (Note that 'Others' 
#is also considered one of the main sectors)

#Expected Results: Code for a merged data frame with each 
#primary sector mapped to its main sector 
#(the primary sector should be present in a separate column).
#-------------------------------

#library(dplyr)

primary_sector$primary_sector <- toupper(primary_sector$primary_sector)
mapping_data_long$category_list <- toupper(mapping_data_long$category_list)

#Data frame with each company's main sector
checkpoint4_master_frame <- dplyr::left_join(primary_sector, mapping_data_long,
                                             by = c("primary_sector" = "category_list"
                                             ))
#removing main_sector_value column
checkpoint4_master_frame$main_sector_value <- NULL
#*************************************
#write.csv(checkpoint4_master_frame, "checkpoint4_master_frame.csv")
#*************************************
# -------------------------------------------------------------
#-----------------------------------------------------------------
#Checkpoint 5: Sector Analysis 2
#-----------------------------------------------------------------
#Now you have a data frame with each company's main sector 
#(main_sector) mapped to it. When we say sector analysis, 
#we refer to one of the eight main sectors.
#------------------------------------------------ 
unique(checkpoint4_master_frame$funding_round_type)
#USA Data
D1 <- dplyr::filter(checkpoint4_master_frame, checkpoint4_master_frame$country_code == "USA" & checkpoint4_master_frame$raised_amount_usd >= 5000000  & checkpoint4_master_frame$raised_amount_usd <= 15000000 )
#GBR Data
D2 <- dplyr::filter(checkpoint4_master_frame, checkpoint4_master_frame$country_code == "GBR" & checkpoint4_master_frame$raised_amount_usd >= 5000000  & checkpoint4_master_frame$raised_amount_usd <= 15000000 )

#IND Data
D3 <- dplyr::filter(checkpoint4_master_frame, checkpoint4_master_frame$country_code == "IND" & checkpoint4_master_frame$raised_amount_usd >= 5000000  & checkpoint4_master_frame$raised_amount_usd <= 15000000 )

#Total number of Investments (count) : C1 : USA
C1_count_invesment <- length(unique(D1$company_permalink))
C1_count_invesment
#Total number of Investments (count) : GBR
C2_count_invesment <- length(unique(D2$company_permalink))
C2_count_invesment
#Total number of Investments (count) : IND
C3_count_invesment <- length(unique(D3$company_permalink))
C3_count_invesment

#-------------------------------------------
#Total amount of investment (USD) : USA
C1_total_invesment <- sum(D1$raised_amount_usd, na.rm = T)
C1_total_invesment
#Total amount of investment (USD) : GBR
C2_total_invesment <- sum(D2$raised_amount_usd, na.rm = T)
C2_total_invesment
#Total amount of investment (USD) : IND
C3_total_invesment <- sum(D3$raised_amount_usd, na.rm = T)
C3_total_invesment
#-------------------------------------------
#library(dplyr)
#Top Sector name (no. of investment-wise)
C1_top_sector_investment <- D1 %>% group_by(D1$main_sector)
C1_top_sector_investment_count <- setNames(C1_top_sector_investment %>% summarise(n = n()),c("main_sector","invesmentcount"))
C1_top_sector_investment_count <- dplyr::arrange(C1_top_sector_investment_count, desc(invesmentcount)) 
#Top 3 sector and Number of invesments in USA
head(C1_top_sector_investment_count,3)
#main_sector                             invesmentcount
#<chr>                                            <int>
#  1 Others                                            2950
#2 Social..Finance..Analytics..Advertising           2714
#3 Cleantech...Semiconductors                        2350
C2_top_sector_investment <- D2 %>% group_by(D2$main_sector)
C2_top_sector_investment_count <- setNames(C2_top_sector_investment %>% summarise(n = n()),c("main_sector","invesmentcount"))
C2_top_sector_investment_count <- dplyr::arrange(C2_top_sector_investment_count, desc(invesmentcount)) 
#Top 3 sector and Number of invesments in GBR
head(C2_top_sector_investment_count,3)
#main_sector                             invesmentcount
#<chr>                                            <int>
#  1 Others                                             147
#2 Social..Finance..Analytics..Advertising            133
#3 Cleantech...Semiconductors                         130
C3_top_sector_investment <- D3 %>% group_by(D3$main_sector)
C3_top_sector_investment_count <- setNames(C3_top_sector_investment %>% summarise(n = n()),c("main_sector","invesmentcount"))
C3_top_sector_investment_count <- dplyr::arrange(C3_top_sector_investment_count, desc(invesmentcount)) 
#Top 3 sector and Number of invesments in IND
head(C3_top_sector_investment_count,3)
#main_sector                             invesmentcount
#<chr>                                            <int>
#  1 Others                                             110
#2 Social..Finance..Analytics..Advertising             60
#3 News..Search.and.Messaging                          52
#-----------------------------------------------------------

#Top Sector name (no. of investment-wise)
C1_top_sector_investment <- D1 %>% group_by(D1$main_sector)
C1_top_sector_investment_count <- setNames(C1_top_sector_investment %>% summarise(n = n()),c("main_sector","invesmentcount"))
C1_top_sector_investment_count <- dplyr::arrange(C1_top_sector_investment_count, desc(invesmentcount)) 
#Top 3 sector and Number of invesments in USA
head(C1_top_sector_investment_count,3)

#----------------------------------------------------------
#For point 3 (top sector count-wise), which company received the highest investment?
C1_top_1_company <- dplyr::filter(D1, D1$main_sector == "Others")
C1_top_1_company <- dplyr::arrange(C1_top_1_company, desc(C1_top_1_company$raised_amount_usd))
C1_top_1_company[1,"name"]
#
C2_top_1_company <- dplyr::filter(D2, D2$main_sector == "Others")
C2_top_1_company <- dplyr::arrange(C2_top_1_company, desc(C2_top_1_company$raised_amount_usd))
C2_top_1_company[1,"name"]
#
C3_top_1_company <- dplyr::filter(D3, D3$main_sector == "Others")
C3_top_1_company <- dplyr::arrange(C3_top_1_company, desc(C3_top_1_company$raised_amount_usd))
C3_top_1_company[1,"name"]
#-------------------------------------------------------------
#For point 4 (second best sector count-wise), which company received the highest investment?
C1_top_2_company <- dplyr::filter(D1, D1$main_sector == "Social, Finance, Analytics, Advertising")
C1_top_2_company <- dplyr::arrange(C1_top_2_company, desc(C1_top_2_company$raised_amount_usd))
C1_top_2_company[2,"name"]
#
C2_top_2_company <- dplyr::filter(D2, D2$main_sector == "Social, Finance, Analytics, Advertising")
C2_top_2_company <- dplyr::arrange(C2_top_2_company, desc(C2_top_2_company$raised_amount_usd))
C2_top_2_company[2,"name"]
#
C3_top_2_company <- dplyr::filter(D3, D3$main_sector == "Social, Finance, Analytics, Advertising")
C3_top_2_company <- dplyr::arrange(C3_top_2_company, desc(C3_top_2_company$raised_amount_usd))
C3_top_2_company[2,"name"]
#--------------------------------------------------------------