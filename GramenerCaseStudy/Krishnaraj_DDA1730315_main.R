#-------------------------------------------------------------------------------------------------------------------------
# Gramener Case Study
#
# Team: Krishnaraj Barvathaya
#       Prakash Kamatar
#       Hemanth Joshi
#-------------------------------------------------------------------------------------------------------------------------

##########################################################################################################################
library(ggplot2)
library(stringr)
library(stringi)
library(dplyr)
library(ggplot2)
library(cowplot)
library(tidyr)
library(lubridate)
library(corrplot)

rm(list=ls())

# LOAD loan dataset
loan_master <- read.csv("loan.csv", header = TRUE, stringsAsFactors = FALSE)

#--------------------------------------------------------------------------------------------------------------------------------  
# DATA CLEANING

# To check the duplicate entries in id,member_id
  dim(loan_master[duplicated(loan_master$id),])[1]
  dim(loan_master[duplicated(loan_master$member_id),])[1]

# To check the alpha numeric values in id,member_id
  which(is.na(as.numeric(as.character(loan_master[[1]]))))
  which(is.na(as.numeric(as.character(loan_master[[2]]))))

# To check how many NA values present in data set
  sapply(loan_master, function(x) sum(is.na(x)))

# Remove the column in which all values of column are NA.
  loan_master <- loan_master[,colSums(is.na(loan_master))<nrow(loan_master)]
  loan_master <- within(loan_master, rm('id', 'member_id','emp_title','title','zip_code', 'pymnt_plan', 'url', 'desc','initial_list_status','collections_12_mths_ex_med','policy_code','application_type',
                                      'acc_now_delinq','chargeoff_within_12_mths','delinq_amnt','tax_liens'))

# Convert below columns to Factor
  cols_to_factor <- c('term','grade','sub_grade','home_ownership','verification_status','loan_status')
  loan_master[, cols_to_factor] <- lapply(loan_master[,cols_to_factor], factor)

# Convert interest Rate column(int_rate) to numeric
  loan_master$int_rate <- str_replace_all(loan_master$int_rate,"%", "")
  loan_master$int_rate <- as.numeric(loan_master$int_rate)
  loan_master$revol_util <- str_replace_all(loan_master$revol_util,"%", "")	
  loan_master$revol_util <- as.numeric(loan_master$revol_util)

# convert Employement Lenth to integer
  loan_master$emp_length <- gsub("< 1 year", "0", loan_master$emp_length)  
  exp_emp_len <- gregexpr('[0-9]+', loan_master$emp_length)
  loan_master$emp_length <- regmatches(loan_master$emp_length,exp_emp_len)
  loan_master$emp_length <- as.integer(loan_master$emp_length)

# Format below date columns to date type
  loan_master$issue_d <- parse_date_time(loan_master$issue_d, c('%y%b','%b%y'))
  loan_master$earliest_cr_line <- parse_date_time(loan_master$earliest_cr_line, c('%y%b','%b%y'))
  loan_master$last_pymnt_d <- parse_date_time(loan_master$last_pymnt_d, c('%y%b','%b%y'))
  loan_master$next_pymnt_d <- parse_date_time(loan_master$next_pymnt_d, c('%y%b','%b%y'))
  loan_master$last_credit_pull_d <- parse_date_time(loan_master$last_credit_pull_d, c('%y%b','%b%y'))

  
defaulter <- subset(loan_master,loan_master$loan_status== "Charged Off")
completed <- subset(loan_master,loan_master$loan_status== "Fully Paid")

#--------------------------------------------------------------------------------------------------------------------------------  

# EDA


# Univariate Analysis
#----------------------
# Frequecy distrubution of loan based on tenure of employment
  sum(is.na(loan_master$emp_length))
  plot_tenure <- ggplot(loan_master[!is.na(loan_master$emp_length),], aes(x=emp_length)) + geom_histogram(binwidth = 1)+labs(x = "Tenure of employment", y= "Frequecy")
  #Observation: More loans taken by people with employment tenure 10 or more number of years
  
# Frequecy distrubution of loan based on loan status
  ggplot(loan_master,aes(x=loan_status)) + geom_bar() +labs(x = "Loan Status", y= "Frequecy")
  #Observation: Significantly high number of loan applicants are fully paid
  
# Frequecy distrubution of loan among loan grades
  plot_grades <- ggplot(loan_master,aes(x=grade)) + geom_bar()  +labs(x = "Grade", y= "Frequecy")
  #Observation: More number of B grade loans have been granted, followed by A and then rest of grades

# Frequecy distrubution of loan based on purpose
  plot_purpose <- ggplot(loan_master,aes(x=purpose)) + geom_bar()  +labs(x = "Purpose", y= "Frequecy") + coord_flip()
  #Observation: Significantly high number of people applied loan for debt consolidation
   
# Frequecy distrubution of loan among home ownership
  plot_ownership <- ggplot(loan_master,aes(x=home_ownership)) + geom_bar() +labs(x = "Home Ownsership", y= "Frequecy") + coord_flip()
  #Observation: Significantly high number of people who are on Rented or Mortgaged applied for loan compared to Own 

  plot_grid(plot_tenure,plot_grades,labels = "AUTO", ncol = 2, align = 'v')
  
  plot_grid(plot_purpose,plot_ownership, labels = "AUTO", ncol = 2, align = 'v')
# Bivariate and multivariate analysis
#-------------------------------------

  # Frequecy distrubution of loan based on term and grade
  ggplot(loan_master,aes(x=term, fill=grade)) + geom_bar()
  
# Frequecy distrubution of loan based on term and status
  ggplot(loan_master,aes(x=term, fill=loan_status)) + geom_bar()
  
  
# Analysis of univariate variables and comparing it with term variable.
  temp1 <-  defaulter %>% group_by(grade,term) %>% tally()
  names(temp1)[names(temp1) == 'n'] <- 'default'
  temp2 <-  completed %>% group_by(grade,term) %>% tally()
  names(temp2)[names(temp2) == 'n'] <- 'completed'
  
  final_grade <- merge(temp1,temp2,by=c("grade","term"))
  
  grade_graph <- ggplot(final_grade,aes(x=grade)) + 
    geom_point(aes(y=default,group=1),colour="Red")+geom_line(aes(y=default,group=1),colour="Red")+geom_text(aes(y=default,label=default),vjust=-.2)+
    geom_point(aes(y=completed,group=1),colour="Blue")+geom_line(aes(y=completed,group=1),colour="Blue")+geom_text(aes(y=completed,label=completed), vjust=-.2)+
    ggtitle("Graph showing Univariate variable GRADE")+
    labs(x="Grade", y="Count")+
    facet_grid(. ~ term )
  
  temp1 <-  defaulter %>% group_by(emp_length,term) %>% tally()
  names(temp1)[names(temp1) == 'n'] <- 'default'
  temp2 <- completed %>% group_by(emp_length,term) %>% tally()
  names(temp2)[names(temp2) == 'n'] <- 'completed'
  
  final_exp <- merge(temp1,temp2,by=c("emp_length","term"))
  
  
  exp_graph <- ggplot(final_exp,aes(x=emp_length)) + 
    geom_point(aes(y=default,group=1),colour="Red")+geom_line(aes(y=default,group=1),colour="Red")+geom_text(aes(y=default,label=default),vjust=-.2)+
    geom_point(aes(y=completed,group=1),colour="Blue")+geom_line(aes(y=completed,group=1),colour="Blue")+geom_text(aes(y=completed,label=completed), vjust=-.2)+
    ggtitle("Graph showing Univariate variable EMP EXP")+
    labs(x="Emp Length", y="Count")+
    facet_grid(. ~ term )
  
  temp1 <- defaulter %>% group_by(purpose,term) %>% tally()
  names(temp1)[names(temp1) == 'n'] <- 'default'
  temp2 <- completed %>% group_by(purpose,term) %>% tally()
  names(temp2)[names(temp2) == 'n'] <- 'completed'
  
  final_purpose <- merge(temp1,temp2,by=c("purpose","term"))
  
  purpose_graph <- ggplot(final_purpose,aes(x=purpose)) + 
    geom_point(aes(y=default,group=1),colour="Red")+geom_line(aes(y=default,group=1),colour="Red")+geom_text(aes(y=default,label=default),vjust=-.2)+
    geom_point(aes(y=completed,group=1),colour="Blue")+geom_line(aes(y=completed,group=1),colour="Blue")+geom_text(aes(y=completed,label=completed), vjust=-.2)+
    ggtitle("Graph showing Univariate variable LOAN PURPOSE")+
    xlab("Loan Purpose")+
    ylab("Frequency")+
    facet_grid(. ~ term ) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  
  temp1 <- defaulter %>% group_by(home_ownership,term) %>% tally()
  names(temp1)[names(temp1) == 'n'] <- 'default'
  temp2 <- completed %>% group_by(home_ownership,term) %>% tally()
  names(temp2)[names(temp2) == 'n'] <- 'completed'
  
  final_ownership <- merge(temp1,temp2,by=c("home_ownership","term"))
  
  ownership_graph <- ggplot(final_ownership,aes(x=home_ownership)) + 
    geom_point(aes(y=default,group=1),colour="Red")+geom_line(aes(y=default,group=1),colour="Red")+geom_text(aes(y=default,label=default),vjust=-.2)+
    geom_point(aes(y=completed,group=1),colour="Blue")+geom_line(aes(y=completed,group=1),colour="Blue")+geom_text(aes(y=completed,label=completed), vjust=-.2)+
    ggtitle("Graph showing Univariate variable HOUSE OWNERSHIP")+
    xlab("House Ownership")+
    ylab("Frequency")+
    facet_grid(. ~ term ) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  
  temp1 <- defaulter %>% group_by(verification_status,term) %>% tally()
  names(temp1)[names(temp1) == 'n'] <- 'default'
  temp2 <- completed %>% group_by(verification_status,term) %>% tally()
  names(temp2)[names(temp2) == 'n'] <- 'completed'
  
  final_verification <- merge(temp1,temp2,by=c("verification_status","term"))
  
  veri_status_graph <- ggplot(final_verification,aes(x=verification_status)) + 
    geom_point(aes(y=default,group=1),colour="Red")+geom_line(aes(y=default,group=1),colour="Red")+geom_text(aes(y=default,label=default),vjust=-.2)+
    geom_point(aes(y=completed,group=1),colour="Blue")+geom_line(aes(y=completed,group=1),colour="Blue")+geom_text(aes(y=completed,label=completed), vjust=-.2)+
    ggtitle("Graph showing Univariate variable for VERIFICATION STATUS")+
    xlab("Verification Status")+
    ylab("Frequency")+
    facet_grid(. ~ term ) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  plot_grid(grade_graph,exp_graph,purpose_graph,ownership_graph, labels = "AUTO", ncol = 2, align = 'v')

#Analysis of the same above used univariate variables in percentage
  #loan status by Term wise
  loan_status_by_term <- as.data.frame(with(loan_master, table(term, loan_master$loan_status)))
  names(loan_status_by_term)<-c("term","loan_status","frequency")
  loan_status_by_term2<-loan_status_by_term %>% group_by(loan_status_by_term$term) %>% summarise(sum(frequency))
  names(loan_status_by_term2)<- c("term","total")
  loan_status_by_term<-merge(loan_status_by_term,loan_status_by_term2, by = "term")
  loan_status_by_term$percentage<-format(round(loan_status_by_term$frequency/loan_status_by_term$total, 2), nsmall = 2)
  ggplot(loan_status_by_term, aes(x=term, y=percentage, group=loan_status ,color=loan_status))+ geom_line()+  ggtitle("Loan Status by Term") + labs(x="Term", y="Percentage")+ geom_text(aes(label=percentage))
  #conclusion: we notice that with 60 months term defaulters are more 
  #36 months: defaulters=11%
  #60 months: defaulters=23%
  
  #loan status by grade wise
  loan_status_by_grade<-as.data.frame(with(loan_master, table(grade, loan_master$loan_status)))
  names(loan_status_by_grade)<-c("grade","loan_status","frequency")
  loan_status_by_grade2<-loan_status_by_grade %>% group_by(loan_status_by_grade$grade) %>% summarise(sum(frequency))
  names(loan_status_by_grade2)<- c("grade","total")
  loan_status_by_grade<-merge(loan_status_by_grade,loan_status_by_grade2, by = "grade")
  loan_status_by_grade$percentage<-format(round(loan_status_by_grade$frequency/loan_status_by_grade$total, 2), nsmall = 2)
  ggplot(loan_status_by_grade, aes(x=grade, y=percentage, group=loan_status ,color=loan_status))+ geom_line()+  ggtitle("Loan Status by Grade") + labs(x="Grade", y="Percentage")+ geom_text(aes(label=percentage))
  #conclusion: we notice that as the grade increase from A to G defaulter % increases and fully paid % decreases.
  
  
  #loan status by  employee experience
  loan_status_by_length<-as.data.frame(with(loan_master, table(emp_length, loan_master$loan_status)))
  names(loan_status_by_length)<-c("emp_length","loan_status","frequency")
  loan_status_by_length2<-loan_status_by_length %>% group_by(loan_status_by_length$emp_length) %>% summarise(sum(frequency))
  names(loan_status_by_length2)<- c("emp_length","total")
  loan_status_by_length<-merge(loan_status_by_length,loan_status_by_length2, by = "emp_length")
  loan_status_by_length$percentage<-format(round(loan_status_by_length$frequency/loan_status_by_length$total, 2), nsmall = 2)
  ggplot(loan_status_by_length, aes(x=emp_length, y=percentage, group=loan_status ,color=loan_status))+ geom_line()+  ggtitle("Loan Status by Emp Length") + labs(x="Employee Exp.", y="Percentage")+ geom_text(aes(label=percentage))
  #conclusion: we notice that as defaulter rate lies between 13 to 15%
  
  #loan status by  Home ownership
  loan_status_by_home_ownership<-as.data.frame(with(loan_master, table(loan_master$home_ownership, loan_master$loan_status)))
  names(loan_status_by_home_ownership)<-c("home_ownership","loan_status","frequency")
  loan_status_by_home_ownership2<-loan_status_by_home_ownership %>% group_by(loan_status_by_home_ownership$home_ownership) %>% summarise(sum(frequency))
  names(loan_status_by_home_ownership2)<- c("home_ownership","total")
  loan_status_by_home_ownership<-merge(loan_status_by_home_ownership,loan_status_by_home_ownership2, by = "home_ownership")
  loan_status_by_home_ownership$percentage<-format(round(loan_status_by_home_ownership$frequency/loan_status_by_home_ownership$total, 2), nsmall = 2)
  ggplot(loan_status_by_home_ownership, aes(x=home_ownership, y=percentage, group=loan_status ,color=loan_status))+ geom_line()+  ggtitle("Loan Status by Home Ownership") + labs(x="Home Ownership", y="Percentage")+ geom_text(aes(label=percentage))
  #conclusion: we notice that as defaulter rate high for OTHER Category
  
  #loan status by  Purpose
  loan_status_by_purpose<-as.data.frame(with(loan_master, table(loan_master$purpose, loan_master$loan_status)))
  names(loan_status_by_purpose)<-c("purpose","loan_status","frequency")
  loan_status_by_purpose2<-loan_status_by_purpose %>% group_by(loan_status_by_purpose$purpose) %>% summarise(sum(frequency))
  names(loan_status_by_purpose2)<- c("purpose","total")
  loan_status_by_purpose<-merge(loan_status_by_purpose,loan_status_by_purpose2, by = "purpose")
  loan_status_by_purpose$percentage<-format(round(loan_status_by_purpose$frequency/loan_status_by_purpose$total, 2), nsmall = 2)
  purpose <- ggplot(loan_status_by_purpose, aes(x=purpose, y=percentage, group=loan_status ,color=loan_status))+ geom_line()+  ggtitle("Loan Status by purpose %") + labs(x="Purpose", y="Percentage")+ geom_text(aes(label=percentage))
  #conclusion: we notice that as defaulter rate high for SMALL Business,renewable Energy
  
  #purpose_2<-ggplot(loan_status_by_purpose, aes(x=purpose, y=percentage, fill=loan_status))+ geom_bar(stat="identity", position = "dodge")+  ggtitle("Loan Status by purpose %") + labs(x="Term", y="Percentage")+ geom_text(aes(label=percentage))
  purpose_2<-ggplot(loan_status_by_purpose, aes(x=purpose, y=loan_status_by_purpose$frequency, group=loan_status ,color=loan_status))+ geom_line()+  ggtitle("Loan Status by purpose") + labs(x="Term", y="Total")+ geom_text(aes(label=frequency))
  plot_grid(purpose,purpose_2, ncol = 1,align = 'v')
  
  #loan status by  Verification status
  loan_status_by_verification_status<-as.data.frame(with(loan_master, table(loan_master$verification_status, loan_master$loan_status)))
  names(loan_status_by_verification_status)<-c("verification_status","loan_status","frequency")
  loan_status_by_verification_status2<-loan_status_by_verification_status %>% group_by(loan_status_by_verification_status$verification_status) %>% summarise(sum(frequency))
  names(loan_status_by_verification_status2)<- c("verification_status","total")
  loan_status_by_verification_status<-merge(loan_status_by_verification_status,loan_status_by_verification_status2, by = "verification_status")
  loan_status_by_verification_status$percentage<-format(round(loan_status_by_verification_status$frequency/loan_status_by_verification_status$total, 2), nsmall = 2)
  ggplot(loan_status_by_verification_status, aes(x=verification_status, y=percentage, group=loan_status ,color=loan_status))+ geom_line()+  ggtitle("Loan Status by Verification Status") + labs(x="Verification Status", y="Percentage")+ geom_text(aes(label=percentage))
  #conclusion: we notice that as defaulter rate high for Verified staus: may be the verification process is not bullet proof.
  
  #loan status by  Number of derogatory public records
  loan_status_by_pub_rec<-as.data.frame(with(loan_master, table(loan_master$pub_rec, loan_master$loan_status)))
  names(loan_status_by_pub_rec)<-c("pub_rec","loan_status","frequency")
  loan_status_by_pub_rec2<-loan_status_by_pub_rec %>% group_by(loan_status_by_pub_rec$pub_rec) %>% summarise(sum(frequency))
  names(loan_status_by_pub_rec2)<- c("pub_rec","total")
  loan_status_by_pub_rec<-merge(loan_status_by_pub_rec,loan_status_by_pub_rec2, by = "pub_rec")
  loan_status_by_pub_rec$percentage<-format(round(loan_status_by_pub_rec$frequency/loan_status_by_pub_rec$total, 2), nsmall = 2)
  ggplot(loan_status_by_pub_rec, aes(x=pub_rec, y=percentage, group=loan_status ,color=loan_status))+ geom_line()+  ggtitle("Loan Status by derogatory public records") + labs(x="derogatory public records", y="Percentage")+ geom_text(aes(label=percentage))
  #conclusion: we notice that as defaulter rate high for pub_rec=1, may be we should not give loan for these guys
  
  
  loan_status_by_pub_rec_bankruptcies<-as.data.frame(with(loan_master, table(loan_master$pub_rec_bankruptcies, loan_master$loan_status)))
  names(loan_status_by_pub_rec_bankruptcies)<-c("pub_rec_bankruptcies","loan_status","frequency")
  loan_status_by_pub_rec_bankruptcies2<-loan_status_by_pub_rec_bankruptcies %>% group_by(loan_status_by_pub_rec_bankruptcies$pub_rec_bankruptcies) %>% summarise(sum(frequency))
  names(loan_status_by_pub_rec_bankruptcies2)<- c("pub_rec_bankruptcies","total")
  loan_status_by_pub_rec_bankruptcies<-merge(loan_status_by_pub_rec_bankruptcies,loan_status_by_pub_rec_bankruptcies2, by = "pub_rec_bankruptcies")
  loan_status_by_pub_rec_bankruptcies$percentage<-format(round(loan_status_by_pub_rec_bankruptcies$frequency/loan_status_by_pub_rec_bankruptcies$total, 2), nsmall = 2)
  ggplot(loan_status_by_pub_rec_bankruptcies, aes(x=pub_rec_bankruptcies, y=percentage, group=loan_status ,color=loan_status))+ geom_line()+  ggtitle("Loan Status by public record bankruptcies") + labs(x="Public record bankruptcies", y="Percentage")+ geom_text(aes(label=percentage))
  
  plot_grid(grade_graph,exp_graph,purpose_graph,ownership_graph,veri_status_graph, labels = "AUTO", ncol = 2, align = 'v')
  
  #loan status by year wise from 2007 to 2011
  loan_master$year<-as.numeric(format(loan_master$issue_d,"%Y"))
  loan_master$month<-format(loan_master$issue_d,"%b")
  loan_by_yearwise<-as.data.frame(with(loan_master, table(year, loan_master$loan_status)))
  names(loan_by_yearwise)<-c("year","loan_status","frequency")
  loan_by_yearwise2<-loan_by_yearwise %>% group_by(loan_by_yearwise$year) %>% summarise(sum(frequency))
  names(loan_by_yearwise2)<- c("year","total")
  loan_by_yearwise<-merge(loan_by_yearwise,loan_by_yearwise2, by = "year")
  loan_by_yearwise$percentage<-format(round(loan_by_yearwise$frequency/loan_by_yearwise$total, 2), nsmall = 2)
  ggplot(loan_by_yearwise, aes(x=year, y=percentage, group=loan_status ,color=loan_status))+ geom_line()+  ggtitle("Loan Status by Year Wise") + labs(x="Year", y="Percentage")+ geom_text(aes(label=percentage))
  #Conclusion: we notice that defaulter rate was high(18%) in 2007, it got reduced to 13% till 2010 then again increase to 15%
  #we have current active users only for the people who applied for loan in 2011.
  
#Bi variate analysis

# Purpose vs Loan amount with Loan status
  ggplot(loan_master, aes(x=purpose, y=loan_amnt, fill=loan_status)) + geom_boxplot()  + labs(title = "Purpose vs Loan amount", x = "Purpose", y= "Loan amount") + scale_fill_discrete(name = "Loan Status") + coord_flip()

# Grade vs Rate of Interest with Loan status
  ggplot(loan_master, aes(x=grade, y=int_rate, fill=loan_status)) + geom_boxplot() + labs(title = "Grade vs Interest", x = "Grade", y= "Rate of Interest") + scale_fill_discrete(name = "Loan Status")

# Loan amount vs Funded amount
  ggplot(loan_master, aes(x=loan_amnt, y=funded_amnt, col=loan_status)) + geom_point(alpha =0.2) + geom_smooth()
  
# Loan amount vs loan status
  ggplot(loan_master, aes(x=loan_amnt, fill=loan_status)) + geom_histogram(binwidth = 1000) 

  ggplot(loan_master, aes(x=loan_amnt, fill=loan_status)) + geom_density()

# DTI vs Loan status 
  ggplot(loan_master, aes(x=dti, fill=loan_status)) + geom_density()+labs(title = "DTI Density graph", x = "Debt-To-Income Ratio", y= "Density")

# Loan grade vs loan status
  ggplot(loan_master, aes(x=grade, fill=loan_status)) + geom_bar() +labs(title = "Different Grade v/s the loan status", x = "Loan Grades")
  

# Loan status vs loan request count with verification status
  ggplot(loan_master, aes(x=term, fill=loan_status)) + geom_bar() 
  ggplot(loan_master, aes(x=home_ownership, fill=loan_status)) + geom_bar() 

# Loan issue date vs loan amount

  ggplot(loan_master, aes(x=issue_d, y=loan_amnt, fill=loan_status)) + geom_bar(stat="identity")+labs(title = "Loan Status v/s Loan issue Date", x = "Issued Date", y = "Loan Amount")
  #OBSERVATION - Ratio of between charged off and rest is geting wider as year profresses

# Analysis of Debt Consolidation

  debt_cons_loan <- loan_master[loan_master$purpose == "debt_consolidation",]
  plot_home_ownership <- ggplot(debt_cons_loan %>% count(home_ownership, loan_status) %>% mutate(pct=n/sum(n)), aes(x=home_ownership, y=n, fill=factor(loan_status))) 
  plot_home_ownership <- plot_home_ownership + geom_bar(stat="identity") + geom_text(aes(label=paste0(sprintf("%1.1f", pct*100),"%")), position=position_stack(vjust=0.5))+labs(title = "Count affecting Home Ownership", x = "Ownership", y = "Count")
  
  plot_loan_amnt <- ggplot(debt_cons_loan %>% count(grade, loan_status) %>% mutate(pct=n/sum(n)), aes(x=grade, y=n, fill=factor(loan_status))) 
  plot_loan_amnt <- plot_loan_amnt + geom_bar(stat="identity") + geom_text(aes(label=paste0(sprintf("%1.1f", pct*100),"%")), position=position_stack(vjust=0.5))+labs(title = "Grade v/s Loan Status", x = "Grade", y = "Count")
  
  plot_grid(plot_home_ownership, plot_loan_amnt, labels = "AUTO") 


  numeric_cols<-c('loan_amnt','funded_amnt','funded_amnt_inv','term int_rate', 'installment', 'emp_length', 
                  'annual_inc', 'revol_bal', 'total_pymnt', 'total_pymnt_inv',  'total_rec_prncp', 
                  'total_rec_int', 'total_rec_late_fee','recoveries','collection_recovery_fee','last_pymnt_amnt')
  
  
  my_data<-defaulter[ , (names(defaulter) %in% numeric_cols)]
  corr_data<-cor(my_data, use = "complete.obs")
  corrplot(corr_data, method = "number")
  corrplot(corr_data, method="shade",shade.col=NA, tl.col="black", tl.srt=45)

rm(temp1,temp2,corr_data,exp_emp_len,exp_graph,
   final_exp,grade_graph,cols_to_factor,final_grade,final_ownership,
   final_purpose,final_verification,debt_cons_loan,completed,defaulter,loan_by_yearwise,
   loan_by_yearwise2,loan_status_by_home_ownership,
   loan_status_by_home_ownership2,loan_status_by_length,loan_status_by_grade,loan_status_by_grade2,
   loan_status_by_length2,loan_status_by_pub_rec,loan_status_by_pub_rec_bankruptcies,loan_status_by_pub_rec_bankruptcies2,
   loan_status_by_pub_rec2,loan_status_by_purpose,loan_status_by_purpose2,loan_status_by_term,loan_status_by_term2,
   loan_status_by_verification_status,loan_status_by_verification_status2,my_data,numeric_cols)

#-------------------------------------------------------------------------------------------------------------------------