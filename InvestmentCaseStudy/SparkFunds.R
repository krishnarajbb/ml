#-------------------------------------------------------------------------------------------------------------------------
# Investment Case Group Project
#
# Team: Krishnaraj Barvathaya
#       Prakash Kamatar
#       Hemanth Joshi
#       Anil Abraham
#-------------------------------------------------------------------------------------------------------------------------

##########################################################################################################################
setwd("D:\\Learning\\PGDDS\\InvestmentCaseGroup")
getwd()
# Checkpoint 1: Data Cleaning 1

  library(stringr)
  library(dplyr)
  library(tidyr)

  companies <- read.csv("companies.txt",sep="\t", na.strings = c("","NA"), header=TRUE, stringsAsFactors = FALSE)
  rounds2 <- read.csv("rounds2.csv", header=TRUE, na.strings = c("","NA"), stringsAsFactors = FALSE)
  
  companies$permalink <- toupper(companies$permalink)
  rounds2$company_permalink <- toupper(rounds2$company_permalink)
  
# Table-1.1
# Understand the Data Set

# 1	How many unique companies are present in rounds2?
  nunique_rnd2_company <- length(unique(rounds2$company_permalink))
  nunique_rnd2_company

# 2	How many unique companies are present in the companies file?
  nunique_company <- length(unique(companies$permalink))
  nunique_company

# 3 In the companies data frame, which column can be used as the unique key for each company? Write the name of the column.
  n_occur <- data.frame(table(companies$permalink))
  n_occure_more <- filter(n_occur, Freq > 1)
  n_occure_more
  # n_occure_more variable is resulting in zero rows, means no duplicate permalink value in the data frame and could be considred for unique key

# 4	Are there any companies in the rounds2 file which are not present in companies ? Answer Y/N.
  has_more_round2 <- if(length(setdiff(rounds2$company_permalink, companies$permalink)) > 0) "Y" else "N"
  has_more_round2 

# 5	Merge the two data frames so that all variables (columns) in the companies frame are added to the rounds2 data frame.
# Name the merged frame master_frame. How many observations are present in master_frame ?
  master_frame <- merge(x=companies, y=rounds2, by.x=c("permalink"),by.y=c("company_permalink"))
  #master_frame <- inner_join(companies, rounds2, by = c("permalink" = "company_permalink"))
  
  master_frame_obs <- nrow(master_frame)
  master_frame_obs


##########################################################################################################################
# Checkpoint 2: Funding Type Analysis

# Table-2.1
# Average Values of Investments for Each of these Funding Types		

  mean_for_round_type <- master_frame %>%  group_by(funding_round_type) %>% summarise(mean_amount =mean(raised_amount_usd, na.rm = TRUE))  
  

# 1	Average funding amount of venture type
  mean_venture_type <- filter(mean_for_round_type, funding_round_type == "venture")
  mean_venture_type

# 2	Average funding amount of angel type
  mean_angel_type <- filter(mean_for_round_type, funding_round_type == "angel")
  mean_angel_type

# 3	Average funding amount of seed type
  mean_seed_type <- filter(mean_for_round_type, funding_round_type == "seed")
  mean_seed_type

# 4	Average funding amount of private equity type
  mean_pe_type <- filter(mean_for_round_type, funding_round_type == "private_equity")
  mean_pe_type

# 5	Considering that Spark Funds wants to invest between 5 to 15 million USD per investment round, 
# which investment type is the most suitable for them?
  filtered_funding <- mean_for_round_type %>% filter(mean_amount > 5000000 & mean_amount < 15000000)
  best_round_type <- filtered_funding[which.max(filtered_funding$mean_amount),]
  best_round_type


##########################################################################################################################
# Checkpoint 3: Country Analysis

  # Identify all english speaking countries
  english_country <- read.csv("CountryList.csv", header = TRUE, stringsAsFactors = FALSE)

  library(countrycode)
  english_country$country_code <- sapply(english_country$Country, function(x) countrycode(x, 'country.name', 'iso3c'))


  # Fucntion to detect if the country is english speaking or not
  detect_enlglish_country <- function(x) 
  {
    if(is.na(x)) return(NA)
    else if (toupper(x) %in% english_country$country_code) return(1)
    else return(0)
  }
  
  # 1. Spark Funds wants to see the top nine countries which have received the highest total funding (across ALL sectors for the chosen investment type)
  # 2. For the chosen investment type, make a data frame named top9 with the top nine countries (based on the total investment amount each country has received)
  
  total_invest_by_country <- master_frame %>% filter(funding_round_type == best_round_type$funding_round_type & !is.na(country_code)) %>% group_by(country_code) %>% summarise(total_amount_raised = sum(raised_amount_usd, na.rm = TRUE)) %>% arrange(desc(total_amount_raised))
  top9 <- head(total_invest_by_country,9)
  
  # Identify the top three English-speaking countries in the data frame top9
  top9$is_english_speaking <- sapply(top9$country_code, detect_enlglish_country)
  top3_english_country <- head(top9 %>% filter(is_english_speaking == 1),3)

# Table -  3.1	
# Analysing the Top 3 English-Speaking Countries


# 1	Top English speaking country
  top_one_country_code <- top3_english_country[1,]$country_code
  top_english_country <- filter(english_country, country_code==top_one_country_code)[1,1]
  top_english_country

# 2	Second English speaking country
  top_two_country_code <- top3_english_country[2,]$country_code
  second_english_country <- filter(english_country, country_code==top_two_country_code)[1,1]
  second_english_country

# 3	Third English speaking country
  top_three_country_code <- top3_english_country[3,]$country_code
  third_english_country <- filter(english_country, country_code==top_three_country_code)[1,1]
  third_english_country


##########################################################################################################################

# Checkpoint 4: Sector Analysis 1 

  library(janitor)

  # Preparing mapping file
  mapping <- read.csv("mapping.csv", header=TRUE, stringsAsFactors = FALSE)
  mapping <- mapping %>% clean_names()
  colnames(mapping)[8] <- "news_search_messaging"
  
  mapping <- gather(mapping, main_sector, main_sector_val,  automotive_sports:social_finance_analytics_advertising)
  mapping <- mapping[(!(mapping$main_sector_val ==0) & !is.na(mapping$category_list) & mapping$category_list != ''),]
  mapping <- mapping[,!(names(mapping) %in% c("main_sector_val"))]
  
  colnames(mapping)[1] <- "primary_sector"
  mapping$primary_sector <- toupper(mapping$primary_sector)
  
  # Preparing and merging Main Sector  
  master_frame <- separate(master_frame, category_list, c("primary_sector", "sub_sector_1", "sub_sector_2"), "\\|")
  master_frame$primary_sector <- toupper(master_frame$primary_sector)
  master_frame <- merge(master_frame, mapping[, c("primary_sector", "main_sector")])

unique(master_frame$main_sector) <- as.factor(master_frame$main_sector)  

##########################################################################################################################

# Checkpoint 5: Sector Analysis 2

# Create three separate data frames D1, D2 and D3 for each of the three countries containing the observations of funding type FT 
# falling within the 5-15 million USD range. 
# The three data frames should contain:
#       All the columns of the master_frame along with the primary sector and the main sector
#       The total number (or count) of investments for each main sector in a separate column
#       The total amount invested in each main sector in a separate column 

  D1 <- master_frame %>%  filter(country_code == top_one_country_code & funding_round_type == best_round_type$funding_round_type & raised_amount_usd >= 5000000.00 & raised_amount_usd <= 15000000.00)
  D2 <- master_frame %>%  filter(country_code == top_two_country_code & funding_round_type == best_round_type$funding_round_type & raised_amount_usd >= 5000000.00 & raised_amount_usd <= 15000000.00)
  D3 <- master_frame %>%  filter(country_code == top_three_country_code & funding_round_type == best_round_type$funding_round_type & raised_amount_usd >= 5000000.00 & raised_amount_usd <= 15000000.00)
  
  D1 <- D1 %>% group_by(main_sector) %>% mutate(total_investment_num = n(), total_amount = sum(raised_amount_usd, na.rm = TRUE)) 
  D2 <- D2 %>% group_by(main_sector) %>% mutate(total_investment_num = n(), total_amount = sum(raised_amount_usd, na.rm = TRUE)) 
  D3 <- D3 %>% group_by(main_sector) %>% mutate(total_investment_num = n(), total_amount = sum(raised_amount_usd, na.rm = TRUE)) 

# Table - 5.1				
# Sector-wise Investment Analysis				

# 1	Total number of Investments (count)
  C1.1 <- nrow(D1)
  C1.1 
  
  C2.1 <- nrow(D2)
  C2.1 
  
  C3.1 <- nrow(D3)
  C3.1
  
# 2	Total amount of investment (USD)
  C1.2 <- sum(D1$raised_amount_usd)
  C1.2 
  
  C2.2 <- sum(D2$raised_amount_usd)
  C2.2 
  
  C3.3 <- sum(D3$raised_amount_usd)
  C3.3

# 3	Top Sector name (no. of investment-wise)
  
  C1.3 <-  (D1 %>% select(main_sector,country_code, total_investment_num) %>% group_by(main_sector) %>% arrange(desc(total_investment_num)) %>% distinct())[1,]
  C1.3$main_sector
  
  C2.3 <-  (D2 %>% select(main_sector,country_code, total_investment_num) %>% group_by(main_sector) %>% arrange(desc(total_investment_num)) %>% distinct())[1,]
  C2.3$main_sector
  
  C3.3 <-  (D3 %>% select(main_sector,country_code, total_investment_num) %>% group_by(main_sector) %>% arrange(desc(total_investment_num)) %>% distinct())[1,]
  C3.3$main_sector
  

# 4	Second Sector name (no. of investment-wise)
  C1.4 <-  (D1 %>% select(main_sector, country_code, total_investment_num) %>% group_by(main_sector) %>% arrange(desc(total_investment_num)) %>% distinct())[2,]
  C1.4$main_sector
  
  C2.4 <-  (D2 %>% select(main_sector,country_code, total_investment_num) %>% group_by(main_sector) %>% arrange(desc(total_investment_num)) %>% distinct())[2,]
  C2.4$main_sector
  
  C3.4 <-  (D3 %>% select(main_sector,country_code, total_investment_num) %>% group_by(main_sector) %>% arrange(desc(total_investment_num)) %>% distinct())[2,]
  C3.4$main_sector

# 5	Third Sector name (no. of investment-wise)
  C1.5 <-  (D1 %>% select(main_sector,country_code, total_investment_num) %>% group_by(main_sector) %>% arrange(desc(total_investment_num)) %>% distinct())[3,]
  C1.5$main_sector
  
  C2.5 <-  (D2 %>% select(main_sector,country_code, total_investment_num) %>% group_by(main_sector) %>% arrange(desc(total_investment_num)) %>% distinct())[3,]
  C2.5$main_sector
  
  C3.5 <-  (D3 %>% select(main_sector,country_code, total_investment_num) %>% group_by(main_sector) %>% arrange(desc(total_investment_num)) %>% distinct())[3,]
  C3.5$main_sector

# 6	Number of investments in top sector (3)
  C1.3$total_investment_num
  C2.3$total_investment_num
  C3.3$total_investment_num

# 7	Number of investments in second sector (4)
  C1.4$total_investment_num
  C2.4$total_investment_num
  C3.4$total_investment_num

# 8	Number of investments in third sector (5)
  C1.5$total_investment_num
  C2.5$total_investment_num
  C3.5$total_investment_num
  
# 9	For point 3 (top sector count-wise), which company received the highest investment?
  C1.9 <- head(D1 %>% filter(main_sector == C1.3$main_sector) %>% group_by(permalink) %>% summarise(amount = sum(raised_amount_usd, na.rm = TRUE)) %>% arrange(desc(amount)),1)
  C1.9$permalink
  
  C2.9 <- head(D2 %>% filter(main_sector == C2.3$main_sector) %>% group_by(permalink) %>% summarise(amount = sum(raised_amount_usd, na.rm = TRUE)) %>% arrange(desc(amount)),1)
  C2.9$permalink
  
  C3.9 <- head(D3 %>% filter(main_sector == C3.3$main_sector) %>% group_by(permalink) %>% summarise(amount = sum(raised_amount_usd, na.rm = TRUE)) %>% arrange(desc(amount)),1)
  C3.9$permalink 
  
# 10	For point 4 (second best sector count-wise), which company received the highest investment?
  C1.10 <- head(D1 %>% filter(main_sector == C1.4$main_sector) %>% group_by(permalink) %>% summarise(amount = sum(raised_amount_usd, na.rm = TRUE)) %>% arrange(desc(amount)),1)
  C1.10$permalink
  
  C2.10 <- head(D2 %>% filter(main_sector == C2.4$main_sector) %>% group_by(permalink) %>% summarise(amount = sum(raised_amount_usd, na.rm = TRUE)) %>% arrange(desc(amount)),1)
  C2.10$permalink
  
  C3.10 <- head(D3 %>% filter(main_sector == C3.4$main_sector) %>% group_by(permalink) %>% summarise(amount = sum(raised_amount_usd, na.rm = TRUE)) %>% arrange(desc(amount)),1)
  C3.10$permalink 
 
  

##########################################################################################################################

# Checkpoint 6: Plots    

  # Export in master_frame in csv format
  write.csv(master_frame, "spark_fund.csv")
  
  write.csv(top9, "top9.csv")
  
  top3 <- rbind(C1.3,C2.3,C3.3,C1.4,C2.4,C3.4,C1.5,C2.5,C3.5)
  write.csv(top3, "top3.csv")

##########################################################################################################################  
  