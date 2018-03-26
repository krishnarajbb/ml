#-------------------------------------------------------------------------------------------------------------------------
# HR Analytics Case Study
#-------------------------------------------------------------------------------------------------------------------------

##########################################################################################################################
library(dplyr)
library(lubridate)
library(mice)
library(ggplot2)
library(cowplot)
library(GGally)
library(caTools)
library(MASS)
library(car)
library(e1071)
library(MASS)
library(caret)
library(stats)
library(ROCR)


  rm(list=ls())  
#--------------------------------------------------------------------------------------------------------------------------------  

# LOAD datasets
  general_data <- read.csv("general_data.csv", header = TRUE, stringsAsFactors = FALSE)
  employee_survey_data <- read.csv("employee_survey_data.csv", header = TRUE, stringsAsFactors = FALSE)
  manager_survey_data <- read.csv("manager_survey_data.csv", header = TRUE, stringsAsFactors = FALSE)
  in_time <- read.csv("in_time.csv", header = TRUE, stringsAsFactors = FALSE)
  out_time <- read.csv("out_time.csv", header = TRUE, stringsAsFactors = FALSE)
  
  
  str(general_data)         # 4410 obs. of  24  variables
  str(employee_survey_data) # 4410 obs. of  4   variables
  str(manager_survey_data)  # 4410 obs. of  3   variables
  str(in_time)              # 4410 obs. of  262 variables
  str(out_time)             # 4410 obs. of  262 variables


#--------------------------------------------------------------------------------------------------------------------------------  
# DATA CLEANING & PREPARATION
  colnames(in_time)[1] <- "EmployeeID"
  colnames(out_time)[1] <- "EmployeeID"

# Remove column having all NA
  in_time <- in_time[,colSums(is.na(in_time))<nrow(in_time)]
  out_time <- out_time[,colSums(is.na(out_time))<nrow(out_time)]

# Check even after removing columns having only NA, both the dataframes are identical
  identical(names(in_time), names(out_time))

# Calculate hours each employee spent in each day
  in_time[2:250] <- lapply(in_time[2:250], function(x) parse_date_time(x, c("ymd", "ymd HMS")))
  out_time[2:250] <- lapply(out_time[2:250], function(x) parse_date_time(x, c("ymd", "ymd HMS")))
  hours_burned <- cbind(in_time[1], out_time[-1] - in_time[-1])
  hours_burned[2:250] <- lapply(hours_burned[2:250], function(x) as.numeric(x, units="hours"))

# Summarizing employee attendance data
  hours_burned[is.na(hours_burned)] <- 0
  AvgWorkingHour <- rowMeans(hours_burned[,-1])
  LeavesTaken <- apply(hours_burned[-1] == 0, 1, sum)
  employee_attendance_summary <- cbind(hours_burned[1], AvgWorkingHour, LeavesTaken)


# Prepare general_data 
  sum(is.na(general_data))
  sum(is.na(general_data$NumCompaniesWorked))
  sum(is.na(general_data$TotalWorkingYears))

  impute_temp <- mice(general_data,m=5,maxit=50,meth='pmm',seed=500)
  summary(impute_temp)
  general_data <- mice::complete(impute_temp,1)

  sum(is.na(general_data))  

# Prepare employee_survey_data
  sum(is.na(employee_survey_data))
  sum(is.na(employee_survey_data$EnvironmentSatisfaction))  
  sum(is.na(employee_survey_data$JobSatisfaction))  
  sum(is.na(employee_survey_data$WorkLifeBalance))  
  
  impute_temp <- mice(employee_survey_data,m=5,maxit=50,meth='pmm',seed=500)
  summary(impute_temp)
  employee_survey_data <- mice::complete(impute_temp,1)
  
  sum(is.na(employee_survey_data))

# Prepare manager_survey_data
  sum(is.na(manager_survey_data))


  length(unique(tolower(general_data$EmployeeID)))                # 4410, confirming customerID is key 
  length(unique(tolower(employee_survey_data$EmployeeID)))        # 4410, confirming customerID is key 
  length(unique(tolower(manager_survey_data$EmployeeID)))         # 4410, confirming customerID is key 
  length(unique(tolower(employee_attendance_summary$EmployeeID))) # 4410, confirming customerID is key 
  
  setdiff(general_data$EmployeeID,employee_survey_data$EmployeeID)        # Identical customerID across these datasets
  setdiff(general_data$EmployeeID,manager_survey_data$EmployeeID)         # Identical customerID across these datasets
  setdiff(general_data$EmployeeID,employee_attendance_summary$EmployeeID) # Identical customerID across these datasets
  
  hrdata<- merge(general_data,employee_survey_data, by="EmployeeID", all = F)
  hrdata<- merge(hrdata,manager_survey_data, by="EmployeeID", all = F)
  hrdata<- merge(hrdata,employee_attendance_summary, by="EmployeeID", all = F)

  sum(is.na(hrdata))  
  
  str(hrdata) #4410 obs. of 31 variables;
  
  cols_to_factor <- c('Attrition','BusinessTravel','Department','Education', 'EducationField','Gender','JobLevel', 'JobRole', 
                      'MaritalStatus', 'Over18', 'StockOptionLevel','EnvironmentSatisfaction', 'JobSatisfaction', 
                      'WorkLifeBalance', 'JobInvolvement', 'PerformanceRating' )
  hrdata[, cols_to_factor] <- lapply(hrdata[,cols_to_factor], factor)
  
  str(hrdata) #4410 obs. of 31 variables;
  
  # Removing columns one factor and all row values same and do not add any value to analysis
  hrdata <- within(hrdata, rm('EmployeeCount', 'StandardHours','Over18', 'EmployeeID'))
  
  str(hrdata) #4410 obs. of 27 variables;
  
  View(hrdata) #master file

#--------------------------------------------------------------------------------------------------------------------------------  

# EDA
# Univariate Analysis
  
  bar_theme<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                     legend.position="right")
  

  plot_grid(ggplot(hrdata, aes(x=BusinessTravel,fill=Attrition))+ geom_bar()+bar_theme, 
            ggplot(hrdata, aes(x=Department,fill=Attrition))+ geom_bar()+bar_theme,
            ggplot(hrdata, aes(x=JobRole,fill=Attrition))+ geom_bar() +bar_theme, 
            ggplot(hrdata, aes(x=JobLevel,fill=Attrition))+ geom_bar()+bar_theme,
            align = "h")   
  
  plot_grid(ggplot(hrdata, aes(x=StockOptionLevel,fill=Attrition))+ geom_bar()+bar_theme,
            ggplot(hrdata, aes(x=PerformanceRating,fill=Attrition))+ geom_bar()+bar_theme
            +scale_x_discrete(labels=c("1" = "Low","2"="Good", "3"="Excellent", "4"="Outstanding")),
            align = "h")   
  
  plot_grid(ggplot(hrdata, aes(x=MaritalStatus,fill=Attrition))+ geom_bar()+bar_theme,
            ggplot(hrdata, aes(x=Gender,fill=Attrition))+ geom_bar()+bar_theme,
            ggplot(hrdata, aes(x=Education,fill=Attrition))+ geom_bar()+bar_theme
            +scale_x_discrete(labels=c("1" = "Below College","2"="College", "3"="Bachelor", "4"="Master", "4"="Doctor")),
            ggplot(hrdata, aes(x=EducationField,fill=Attrition))+ geom_bar()+bar_theme,
            align = "h")   
  # Observation: Who are single are more 
  
  plot_grid(ggplot(hrdata, aes(x=EnvironmentSatisfaction,fill=Attrition))+ geom_bar()+bar_theme
            +scale_x_discrete(labels=c("1" = "Low","2"="Medium", "3"="High", "4"="Very High")),
            ggplot(hrdata, aes(x=JobSatisfaction,fill=Attrition))+ geom_bar()+bar_theme
            +scale_x_discrete(labels=c("1" = "Low","2"="Medium", "3"="High", "4"="Very High")),
            ggplot(hrdata, aes(x=JobInvolvement,fill=Attrition))+ geom_bar()+bar_theme
            +scale_x_discrete(labels=c("1" = "Low","2"="Medium", "3"="High", "4"="Very High")),
            ggplot(hrdata, aes(x=WorkLifeBalance,fill=Attrition))+ geom_bar()+bar_theme
            +scale_x_discrete(labels=c("1" = "Bad","2"="Good", "3"="Better", "4"="Best")),
            align = "h")   
  
  
  ### Histogram and Boxplots for numeric variables 
  box_theme<- theme(axis.line=element_blank(),axis.title=element_blank(), 
                    axis.ticks=element_blank(), axis.text=element_blank())

  plot_grid(ggplot(hrdata, aes(Age))+ geom_histogram(binwidth = 5),
            ggplot(hrdata, aes(x="",y=Age))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
            align = "v",ncol = 1)
  
  plot_grid(ggplot(hrdata, aes(DistanceFromHome))+ geom_histogram(binwidth = 10),
            ggplot(hrdata, aes(x="",y=DistanceFromHome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
            align = "v",ncol = 1)
  
  plot_grid(ggplot(hrdata, aes(MonthlyIncome))+ geom_histogram(binwidth = 10000),
            ggplot(hrdata, aes(x="",y=MonthlyIncome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
            align = "v",ncol = 1) 
  
  plot_grid(ggplot(hrdata, aes(NumCompaniesWorked))+ geom_histogram(binwidth = 1),
            ggplot(hrdata, aes(x="",y=NumCompaniesWorked))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
            align = "v",ncol = 1) 
  
  plot_grid(ggplot(hrdata, aes(PercentSalaryHike))+ geom_histogram(binwidth = 1),
            ggplot(hrdata, aes(x="",y=PercentSalaryHike))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
            align = "v",ncol = 1) 
  
  plot_grid(ggplot(hrdata, aes(TotalWorkingYears))+ geom_histogram(),
            ggplot(hrdata, aes(x="",y=TotalWorkingYears))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
            align = "v",ncol = 1) 
  
  plot_grid(ggplot(hrdata, aes(TrainingTimesLastYear))+ geom_histogram(),
            ggplot(hrdata, aes(x="",y=TrainingTimesLastYear))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
            align = "v",ncol = 1)
  
  plot_grid(ggplot(hrdata, aes(YearsAtCompany))+ geom_histogram(),
            ggplot(hrdata, aes(x="",y=YearsAtCompany))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
            align = "v",ncol = 1) 
  
  plot_grid(ggplot(hrdata, aes(YearsSinceLastPromotion))+ geom_histogram(binwidth = 1),
            ggplot(hrdata, aes(x="",y=YearsSinceLastPromotion))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
            align = "v",ncol = 1) 
  
  plot_grid(ggplot(hrdata, aes(YearsWithCurrManager))+ geom_histogram(binwidth = 5),
            ggplot(hrdata, aes(x="",y=YearsWithCurrManager))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
            align = "v",ncol = 1)
  
  plot_grid(ggplot(hrdata, aes(AvgWorkingHour))+ geom_histogram(),
            ggplot(hrdata, aes(x="",y=AvgWorkingHour))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
            align = "v",ncol = 1)
  
  plot_grid(ggplot(hrdata, aes(LeavesTaken))+ geom_histogram(),
            ggplot(hrdata, aes(x="",y=LeavesTaken))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
            align = "v",ncol = 1)
  
  # OBSERVATION Outliers observed in attributes MonthlyIncome, NumCompaniesWorked, TotalWorkingYears, TrainingTimesLastYear,  
  #             YearsAtCompany, YearsSinceLastPromotion, YearsWithCurrManager, AvgWorkingHour
  
# Bivariate and multivariate analysis relative to attrition value
  box_theme_y<- theme(axis.line.y=element_blank(),axis.title.y=element_blank(), 
                      axis.ticks.y=element_blank(), axis.text.y=element_blank(),
                      legend.position="top")
  
  plot_grid(ggplot(hrdata, aes(x=Attrition,y=Age, fill=Attrition))+ geom_boxplot(width=0.2)+ 
              coord_flip() +box_theme_y,
            ggplot(hrdata, aes(x=Attrition,y=DistanceFromHome, fill=Attrition))+ geom_boxplot(width=0.2)+
              coord_flip() + box_theme_y,
            ggplot(hrdata, aes(x=Attrition,y=MonthlyIncome, fill=Attrition))+ geom_boxplot(width=0.2)+
              coord_flip() + box_theme_y,
            ggplot(hrdata, aes(x=Attrition,y=NumCompaniesWorked, fill=Attrition))+ geom_boxplot(width=0.2)+
              coord_flip() + box_theme_y,
            align = "v",nrow = 1)
  
  plot_grid(ggplot(hrdata, aes(x=Attrition,y=PercentSalaryHike, fill=Attrition))+ geom_boxplot(width=0.2)+
              coord_flip() + box_theme_y,
            ggplot(hrdata, aes(x=Attrition,y=TotalWorkingYears, fill=Attrition))+ geom_boxplot(width=0.2)+
              coord_flip() + box_theme_y,
            ggplot(hrdata, aes(x=Attrition,y=TrainingTimesLastYear, fill=Attrition))+ geom_boxplot(width=0.2)+ 
              coord_flip() +theme(legend.position="none"),
            ggplot(hrdata, aes(x=Attrition,y=YearsAtCompany, fill=Attrition))+ geom_boxplot(width=0.2)+
              coord_flip() + box_theme_y,
            align = "v",nrow = 1)
  
  plot_grid(ggplot(hrdata, aes(x=Attrition,y=YearsSinceLastPromotion, fill=Attrition))+ geom_boxplot(width=0.2)+
              coord_flip() + box_theme_y,
            ggplot(hrdata, aes(x=Attrition,y=YearsWithCurrManager, fill=Attrition))+ geom_boxplot(width=0.2)+
              coord_flip() + box_theme_y,
            ggplot(hrdata, aes(x=Attrition,y=AvgWorkingHour, fill=Attrition))+ geom_boxplot(width=0.2)+
              coord_flip() + box_theme_y,
            ggplot(hrdata, aes(x=Attrition,y=LeavesTaken, fill=Attrition))+ geom_boxplot(width=0.2)+
              coord_flip() + box_theme_y,
            align = "v",nrow = 1)
  
  str(hrdata)
  
  # Correlation between numeric attributes
  ggpairs(hrdata[, c("Age", "DistanceFromHome", "MonthlyIncome", "NumCompaniesWorked", "PercentSalaryHike", "TotalWorkingYears",
                     "TrainingTimesLastYear", "YearsAtCompany",  "YearsSinceLastPromotion", "YearsWithCurrManager", "AvgWorkingHour",
                     "LeavesTaken")])
  # OBSERVATION
  # YearsAtCompany and TotalWorkingYears  have corr = 0.627
  # YearsAtCompany and YearsWithCurrManager  have corr = 0.769
  # YearsAtCompany and YearsSinceLastPromotion have corr = 0.618
  # YearsAtCompany and Age have corr = 0.311
  # YearsWithCurrManager and YearsSinceLastPromotion  have corr = 0.51
  # YearsWithCurrManager and TotalWorkingYears  have corr = 0.459
  # TotalWorkingYears and YearsSinceLastPromotion have corr = 0.404
  # NumCompaniesWorked and Age have corr = 0.311
  # AvgWorkingHour and LeavesTaken are negetively corr = -0.368
  
  
  # Outlier treatment
  #MonthlyIncome, NumCompaniesWorked, TotalWorkingYears, TrainingTimesLastYear,  YearsAtCompany, YearsSinceLastPromotion, 
  # YearsWithCurrManager, AvgWorkingHour
  boxplot(hrdata$MonthlyIncome)
  quantile(hrdata$MonthlyIncome,seq(0,1,0.01))
  hrdata$MonthlyIncome[hrdata$MonthlyIncome > 164130.0] <- 164130.0
  boxplot(hrdata$MonthlyIncome)
  
  boxplot(hrdata$NumCompaniesWorked)
  quantile(hrdata$NumCompaniesWorked,seq(0,1,0.01))
  hrdata$NumCompaniesWorked[hrdata$NumCompaniesWorked > 8] <- 8
  boxplot(hrdata$NumCompaniesWorked)
  
  boxplot(hrdata$TotalWorkingYears)
  quantile(hrdata$TotalWorkingYears,seq(0,1,0.01))
  hrdata$TotalWorkingYears[hrdata$TotalWorkingYears > 28.0] <- 28.0
  boxplot(hrdata$TotalWorkingYears)
  
  boxplot(hrdata$TrainingTimesLastYear)
  quantile(hrdata$TrainingTimesLastYear,seq(0,1,0.01))
  hrdata$TrainingTimesLastYear[hrdata$TrainingTimesLastYear > 4] <- 4
  boxplot(hrdata$TrainingTimesLastYear)
  
  boxplot(hrdata$YearsAtCompany)
  quantile(hrdata$YearsAtCompany,seq(0,1,0.01))
  hrdata$YearsAtCompany[hrdata$YearsAtCompany > 17] <- 17
  boxplot(hrdata$YearsAtCompany)
  
  boxplot(hrdata$YearsSinceLastPromotion)
  quantile(hrdata$YearsSinceLastPromotion,seq(0,1,0.01))
  hrdata$YearsSinceLastPromotion[hrdata$YearsSinceLastPromotion > 7] <- 7
  boxplot(hrdata$YearsSinceLastPromotion)
  
  boxplot(hrdata$YearsWithCurrManager)
  quantile(hrdata$YearsWithCurrManager,seq(0,1,0.01))
  hrdata$YearsWithCurrManager[hrdata$YearsWithCurrManager > 14] <- 14
  boxplot(hrdata$YearsWithCurrManager)
  
  boxplot(hrdata$AvgWorkingHour)
  quantile(hrdata$AvgWorkingHour,seq(0,1,0.01))
  hrdata$AvgWorkingHour[hrdata$AvgWorkingHour > 10.103419] <- 10.103419
  boxplot(hrdata$AvgWorkingHour)

#--------------------------------------------------------------------------------------------------------------------------------   
# DATA PREPARATION FOR LOGISTIC REGRESSION
  
  hrdata_lg <- hrdata  
 
  # Standardising scales of continues variables
  hrdata_lg$Age<- scale(hrdata_lg$Age) 
  hrdata_lg$DistanceFromHome<- scale(hrdata_lg$DistanceFromHome) 
  hrdata_lg$MonthlyIncome<- scale(hrdata_lg$MonthlyIncome) 
  hrdata_lg$NumCompaniesWorked<- scale(hrdata_lg$NumCompaniesWorked) 
  hrdata_lg$PercentSalaryHike<- scale(hrdata_lg$PercentSalaryHike) 
  hrdata_lg$TotalWorkingYears<- scale(hrdata_lg$TotalWorkingYears) 
  hrdata_lg$TrainingTimesLastYear<- scale(hrdata_lg$TrainingTimesLastYear) 
  hrdata_lg$YearsAtCompany<- scale(hrdata_lg$YearsAtCompany) 
  hrdata_lg$YearsSinceLastPromotion<- scale(hrdata_lg$YearsSinceLastPromotion) 
  hrdata_lg$YearsWithCurrManager<- scale(hrdata_lg$YearsWithCurrManager) 
  hrdata_lg$AvgWorkingHour<- scale(hrdata_lg$AvgWorkingHour) 
  hrdata_lg$LeavesTaken<- scale(hrdata_lg$LeavesTaken) 
  
  
  str(hrdata_lg)
  # Dummy variable creation for categorical variables having 2 levels - Attrition, Gender
  levels(hrdata_lg$Attrition)<-c(0,1)
  hrdata_lg$Attrition <- as.numeric(levels(hrdata_lg$Attrition))[hrdata_lg$Attrition]
  
  levels(hrdata_lg$Gender)<-c(0,1)
  hrdata_lg$Gender <- as.numeric(levels(hrdata_lg$Gender))[hrdata_lg$Gender]
  
  # OBSERVATION - Though PerformanceRating can have 4 levels, current data set has only 2 levels, so will use the above technique 
  #               to convert this attriutes also to numeric
  levels(hrdata_lg$PerformanceRating)<-c(0,1)
  hrdata_lg$PerformanceRating <- as.numeric(levels(hrdata_lg$PerformanceRating))[hrdata_lg$PerformanceRating]
  
  # Dummy variable creation for categorical variables having more than 2 levels
  dummies<- data.frame(sapply(hrdata_lg, function(x) data.frame(model.matrix(~x-1,data =hrdata_lg))[,-1]))

  hrdata_lg <- cbind(hrdata_lg,dummies) 
  hrdata_lg <- dplyr::select(hrdata_lg,-c(BusinessTravel,Department,Education,EducationField,JobLevel,JobRole,MaritalStatus,StockOptionLevel,
                                    EnvironmentSatisfaction,JobSatisfaction,WorkLifeBalance,JobInvolvement))
  
  str(hrdata_lg)  #4410 obs. of  57 variables
  View(hrdata_lg) 
  
  # Attrition rate
  sum(hrdata_lg$Attrition)/nrow(hrdata_lg)
  # OBSERVATION - Attrition rate is 16.12%
  
  # Train and Test Data
  #------------------------------------------------------------------------------------------------------------------------- 
  set.seed(100)
  
  indices = sample.split(hrdata_lg$Attrition, SplitRatio = 0.7)
  
  hrdata.train = hrdata_lg[indices,]
  View(hrdata.train)
  
  hrdata.test = hrdata_lg[!(indices),]
  View(hrdata.test)
  
  # Data Modeling using Logistic Regression
  #-------------------------------------------------------------------------------------------------------------------------   
  
  # Initial model
  model_1 = glm(Attrition ~ ., data = hrdata.train, family = "binomial")
  summary(model_1)
  # OBSERVATION   Null deviance: 2728.0  on 3086  degrees of freedom
  #               Residual deviance: 2039  on 3030  degrees of freedom
  #               AIC: 2153
  
  # Stepwise selection
  model_2<- stepAIC(model_1, direction="both")
  
  summary(model_2)
  sort(vif(model_2), decreasing = TRUE)
  
  # BusinessTravel.xTravel_Frequently has low p value, cann't remove. So next option is to remove BusinessTravel.xTravel_Rarely
  model_3<- glm(formula = Attrition ~ Age + DistanceFromHome + MonthlyIncome + 
                  NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + AvgWorkingHour + 
                  BusinessTravel.xTravel_Frequently + 
                  Department.xResearch...Development + Department.xSales + 
                  Education.x3 + Education.x4 + Education.x5 + JobLevel.x2 + 
                  JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                  JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                  MaritalStatus.xSingle + StockOptionLevel.x1 + EnvironmentSatisfaction.x2 + 
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                  JobInvolvement.x3 + EducationField.xLife.Sciences, family = "binomial", 
                data = hrdata.train) 
  
  summary(model_3) 
  sort(vif(model_3), decreasing = TRUE)
  
  
  # WorkLifeBalance.x2 has higher p value, so remove
  model_4<- glm(formula = Attrition ~ Age + DistanceFromHome + MonthlyIncome + 
                  NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + AvgWorkingHour + 
                  BusinessTravel.xTravel_Frequently + 
                  Department.xResearch...Development + Department.xSales + 
                  Education.x3 + Education.x4 + Education.x5 + JobLevel.x2 + 
                  JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                  JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                  MaritalStatus.xSingle + StockOptionLevel.x1 + EnvironmentSatisfaction.x2 + 
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                  JobInvolvement.x3 + EducationField.xLife.Sciences, family = "binomial", 
                data = hrdata.train) 
  
  summary(model_4) 
  sort(vif(model_4), decreasing = TRUE)
  

  # Department.xSales has higher p value, so remove
  model_5<- glm(formula = Attrition ~ Age + DistanceFromHome + MonthlyIncome + 
                  NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + AvgWorkingHour + 
                  BusinessTravel.xTravel_Frequently + 
                  Department.xResearch...Development + 
                  Education.x3 + Education.x4 + Education.x5 + JobLevel.x2 + 
                  JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                  JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                  MaritalStatus.xSingle + StockOptionLevel.x1 + EnvironmentSatisfaction.x2 + 
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                  JobInvolvement.x3 + EducationField.xLife.Sciences, family = "binomial", 
                data = hrdata.train) 
  
  summary(model_5) 
  sort(vif(model_5), decreasing = TRUE)
  
  # Removing TotalWorkingYears is increasing the AIC. So retaining it though VIF is 2.3
  # No high VIF, so start removing less significant p values, Department.xResearch...Development
  model_6<- glm(formula = Attrition ~ Age + DistanceFromHome + MonthlyIncome + 
                  NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + AvgWorkingHour + 
                  BusinessTravel.xTravel_Frequently + 
                  Education.x3 + Education.x4 + Education.x5 + JobLevel.x2 + 
                  JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                  JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                  MaritalStatus.xSingle + StockOptionLevel.x1 + EnvironmentSatisfaction.x2 + 
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                  JobInvolvement.x3 + EducationField.xLife.Sciences, family = "binomial", 
                data = hrdata.train) 
  
  summary(model_6) 
  sort(vif(model_6), decreasing = TRUE)
  
  # No high VIF, so start removing less significant p values, WorkLifeBalance.x4
  model_7<- glm(formula = Attrition ~ Age + DistanceFromHome + MonthlyIncome + 
                  NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + AvgWorkingHour + 
                  BusinessTravel.xTravel_Frequently + 
                  Education.x3 + Education.x4 + Education.x5 + JobLevel.x2 + 
                  JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                  JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                  MaritalStatus.xSingle + StockOptionLevel.x1 + EnvironmentSatisfaction.x2 + 
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  WorkLifeBalance.x3 + 
                  JobInvolvement.x3 + EducationField.xLife.Sciences, family = "binomial", 
                data = hrdata.train) 
  
  summary(model_7) 
  sort(vif(model_7), decreasing = TRUE)
  
  # No high VIF, so start removing less significant p values, EducationField.xLife.Sciences
  model_8<- glm(formula = Attrition ~ Age + DistanceFromHome + MonthlyIncome + 
                  NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + AvgWorkingHour + 
                  BusinessTravel.xTravel_Frequently + 
                  Education.x3 + Education.x4 + Education.x5 + JobLevel.x2 + 
                  JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                  JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                  MaritalStatus.xSingle + StockOptionLevel.x1 + EnvironmentSatisfaction.x2 + 
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  WorkLifeBalance.x3 + 
                  JobInvolvement.x3, family = "binomial", 
                data = hrdata.train) 
  
  summary(model_8) 
  sort(vif(model_8), decreasing = TRUE)
  
  # No high VIF, so start removing less significant p values, DistanceFromHome
  model_9<- glm(formula = Attrition ~ Age + MonthlyIncome + 
                  NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + AvgWorkingHour + 
                  BusinessTravel.xTravel_Frequently + 
                  Education.x3 + Education.x4 + Education.x5 + JobLevel.x2 + 
                  JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                  JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                  MaritalStatus.xSingle + StockOptionLevel.x1 + EnvironmentSatisfaction.x2 + 
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  WorkLifeBalance.x3 + 
                  JobInvolvement.x3, family = "binomial", 
                data = hrdata.train) 
  
  summary(model_9) 
  sort(vif(model_9), decreasing = TRUE)
  
  
  # No high VIF, so start removing less significant p values, Education.x3
  model_10<- glm(formula = Attrition ~ Age + MonthlyIncome + 
                  NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + AvgWorkingHour + 
                  BusinessTravel.xTravel_Frequently + 
                  Education.x4 + Education.x5 + JobLevel.x2 + 
                  JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                  JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                  MaritalStatus.xSingle + StockOptionLevel.x1 + EnvironmentSatisfaction.x2 + 
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  WorkLifeBalance.x3 + 
                  JobInvolvement.x3, family = "binomial", 
                data = hrdata.train) 
  

  summary(model_10) 
  sort(vif(model_10), decreasing = TRUE)
  
  # No high VIF, so start removing less significant p values, Education.x4
  model_11<- glm(formula = Attrition ~ Age + MonthlyIncome + 
                   NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                   YearsSinceLastPromotion + YearsWithCurrManager + AvgWorkingHour + 
                   BusinessTravel.xTravel_Frequently + 
                   Education.x5 + JobLevel.x2 + 
                   JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                   JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                   MaritalStatus.xSingle + StockOptionLevel.x1 + EnvironmentSatisfaction.x2 + 
                   EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                   JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                   WorkLifeBalance.x3 + 
                   JobInvolvement.x3, family = "binomial", 
                 data = hrdata.train) 
  
  
  summary(model_11) 
  sort(vif(model_11), decreasing = TRUE)
  
  # No high VIF, so start removing less significant p values, Education.x5
  model_12<- glm(formula = Attrition ~ Age + MonthlyIncome + 
                   NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                   YearsSinceLastPromotion + YearsWithCurrManager + AvgWorkingHour + 
                   BusinessTravel.xTravel_Frequently + 
                   JobLevel.x2 + 
                   JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                   JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                   MaritalStatus.xSingle + StockOptionLevel.x1 + EnvironmentSatisfaction.x2 + 
                   EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                   JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                   WorkLifeBalance.x3 + 
                   JobInvolvement.x3, family = "binomial", 
                 data = hrdata.train) 
  
  
  summary(model_12) 
  sort(vif(model_12), decreasing = TRUE)
  
  
  # No high VIF, so start removing less significant p values, StockOptionLevel.x1
  model_13<- glm(formula = Attrition ~ Age + MonthlyIncome + 
                   NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                   YearsSinceLastPromotion + YearsWithCurrManager + AvgWorkingHour + 
                   BusinessTravel.xTravel_Frequently + 
                   JobLevel.x2 + 
                   JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                   JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                   MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + 
                   EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                   JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                   WorkLifeBalance.x3 + 
                   JobInvolvement.x3, family = "binomial", 
                 data = hrdata.train) 
  
  
  summary(model_13) 
  sort(vif(model_13), decreasing = TRUE)
  
  # No high VIF, so start removing less significant p values, JobInvolvement.x3 
  model_14<- glm(formula = Attrition ~ Age + MonthlyIncome + 
                   NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                   YearsSinceLastPromotion + YearsWithCurrManager + AvgWorkingHour + 
                   BusinessTravel.xTravel_Frequently + 
                   JobLevel.x2 + 
                   JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                   JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                   MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + 
                   EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                   JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                   WorkLifeBalance.x3 , family = "binomial", 
                 data = hrdata.train) 
  
  
  summary(model_14) 
  sort(vif(model_14), decreasing = TRUE)
  
  
  # No high VIF, so start removing less significant p values, JobLevel.x2
  model_15<- glm(formula = Attrition ~ Age + MonthlyIncome + 
                   NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                   YearsSinceLastPromotion + YearsWithCurrManager + AvgWorkingHour + 
                   BusinessTravel.xTravel_Frequently + 
                   JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                   JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                   MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + 
                   EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                   JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                   WorkLifeBalance.x3 , family = "binomial", 
                 data = hrdata.train) 
  
  
  summary(model_15) 
  sort(vif(model_15), decreasing = TRUE)
  
  # No high VIF, so start removing less significant p values, MonthlyIncome
  model_16<- glm(formula = Attrition ~ Age +
                   NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                   YearsSinceLastPromotion + YearsWithCurrManager + AvgWorkingHour + 
                   BusinessTravel.xTravel_Frequently + 
                   JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                   JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                   MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + 
                   EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                   JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                   WorkLifeBalance.x3 , family = "binomial", 
                 data = hrdata.train) 
  
  
  summary(model_16) 
  sort(vif(model_16), decreasing = TRUE)
  
  # No high VIF, so start removing less significant p values, JobRole.xLaboratory.Technician
  model_17<- glm(formula = Attrition ~ Age +
                   NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                   YearsSinceLastPromotion + YearsWithCurrManager + AvgWorkingHour + 
                   BusinessTravel.xTravel_Frequently + 
                   JobRole.xResearch.Director + 
                   JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                   MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + 
                   EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                   JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                   WorkLifeBalance.x3 , family = "binomial", 
                 data = hrdata.train) 
  
  
  summary(model_17) 
  sort(vif(model_17), decreasing = TRUE)
  
  # No high VIF, so start removing less significant p values, JobRole.xResearch.Scientist
  model_18<- glm(formula = Attrition ~ Age +
                   NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                   YearsSinceLastPromotion + YearsWithCurrManager + AvgWorkingHour + 
                   BusinessTravel.xTravel_Frequently + 
                   JobRole.xResearch.Director + 
                   JobRole.xSales.Executive + 
                   MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + 
                   EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                   JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                   WorkLifeBalance.x3 , family = "binomial", 
                 data = hrdata.train) 
  
  
  summary(model_18) 
  sort(vif(model_18), decreasing = TRUE)
  
  # No high VIF, so start removing less significant p values, JobRole.xResearch.Director
  model_19<- glm(formula = Attrition ~ Age +
                   NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                   YearsSinceLastPromotion + YearsWithCurrManager + AvgWorkingHour + 
                   BusinessTravel.xTravel_Frequently + 
                   JobRole.xSales.Executive + 
                   MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + 
                   EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                   JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                   WorkLifeBalance.x3 , family = "binomial", 
                 data = hrdata.train) 
  
  
  summary(model_19) 
  sort(vif(model_19), decreasing = TRUE)
  
  # No high VIF, so start removing less significant p values, JobRole.xSales.Executive
  model_20<- glm(formula = Attrition ~ Age +
                   NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                   YearsSinceLastPromotion + YearsWithCurrManager + AvgWorkingHour + 
                   BusinessTravel.xTravel_Frequently + 
                   MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + 
                   EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                   JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                   WorkLifeBalance.x3 , family = "binomial", 
                 data = hrdata.train) 
  
  
  summary(model_20) 
  sort(vif(model_20), decreasing = TRUE)
  
  # No high VIF, so start removing less significant p values, TrainingTimesLastYear
  model_21<- glm(formula = Attrition ~ Age +
                   NumCompaniesWorked + TotalWorkingYears + 
                   YearsSinceLastPromotion + YearsWithCurrManager + AvgWorkingHour + 
                   BusinessTravel.xTravel_Frequently + 
                   MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + 
                   EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                   JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                   WorkLifeBalance.x3 , family = "binomial", 
                 data = hrdata.train) 
  
  
  summary(model_21) 
  sort(vif(model_21), decreasing = TRUE)
  
  # No high VIF, so start removing less significant p values, WorkLifeBalance.x3
  model_22<- glm(formula = Attrition ~ Age +
                   NumCompaniesWorked + TotalWorkingYears + 
                   YearsSinceLastPromotion + YearsWithCurrManager + AvgWorkingHour + 
                   BusinessTravel.xTravel_Frequently + 
                   MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + 
                   EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                   JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4, family = "binomial", 
                 data = hrdata.train) 
  
  
  summary(model_22) 
  sort(vif(model_22), decreasing = TRUE)
  
  
  # No high VIF, so start removing less significant p values, JobSatisfaction.x3
  model_22<- glm(formula = Attrition ~ Age +
                   NumCompaniesWorked + TotalWorkingYears + 
                   YearsSinceLastPromotion + YearsWithCurrManager + AvgWorkingHour + 
                   BusinessTravel.xTravel_Frequently + 
                   MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + 
                   EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                   JobSatisfaction.x2 + JobSatisfaction.x4, family = "binomial", 
                 data = hrdata.train) 
  
  
  summary(model_22) 
  sort(vif(model_22), decreasing = TRUE)
  
  # No high VIF, so start removing less significant p values, JobSatisfaction.x2
  model_23<- glm(formula = Attrition ~ Age +
                   NumCompaniesWorked + TotalWorkingYears + 
                   YearsSinceLastPromotion + YearsWithCurrManager + AvgWorkingHour + 
                   BusinessTravel.xTravel_Frequently + 
                   MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + 
                   EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                   JobSatisfaction.x4, family = "binomial", 
                 data = hrdata.train) 
  
  
  summary(model_23) 
  sort(vif(model_23), decreasing = TRUE)
  
  # Compared to all other attribute Age has higher P value, can be removed
  model_24<- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                   YearsSinceLastPromotion + YearsWithCurrManager + AvgWorkingHour + 
                   BusinessTravel.xTravel_Frequently + 
                   MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + 
                   EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                   JobSatisfaction.x4, family = "binomial", 
                 data = hrdata.train) 
  
  
  summary(model_24) 
  sort(vif(model_24), decreasing = TRUE)
  
  # Removed all least significant and hight p value attributes, now model has 11 attributes
  
  # Deviance Residuals: 
  #   Min       1Q   Median       3Q      Max  
  # -1.6448  -0.5868  -0.3827  -0.2124   3.5528  
  # 
  # Coefficients:
  #   Estimate Std. Error z value Pr(>|z|)    
  # (Intercept)                       -1.51313    0.12285 -12.317  < 2e-16 ***
  #   NumCompaniesWorked                 0.32457    0.05513   5.887 3.93e-09 ***
  #   TotalWorkingYears                 -0.67418    0.08261  -8.161 3.33e-16 ***
  #   YearsSinceLastPromotion            0.40300    0.07178   5.615 1.97e-08 ***
  #   YearsWithCurrManager              -0.41957    0.08416  -4.985 6.18e-07 ***
  #   AvgWorkingHour                     0.53835    0.05212  10.328  < 2e-16 ***
  #   BusinessTravel.xTravel_Frequently  0.74103    0.12622   5.871 4.34e-09 ***
  #   MaritalStatus.xSingle              1.03462    0.11023   9.386  < 2e-16 ***
  #   EnvironmentSatisfaction.x2        -0.94931    0.16532  -5.742 9.35e-09 ***
  #   EnvironmentSatisfaction.x3        -0.93577    0.14716  -6.359 2.03e-10 ***
  #   EnvironmentSatisfaction.x4        -1.20205    0.15216  -7.900 2.80e-15 ***
  #   JobSatisfaction.x4                -0.76953    0.12886  -5.972 2.35e-09 ***
  
  final_model<- model_24
  
  # Model Evaluation
  #-------------------------------------------------------------------------------------------------------------------------   

  #predicted probabilities of Churn 1 for test data
  
  test_pred = predict(final_model, type = "response", newdata = hrdata.test[,-1])
 
  summary(test_pred)
  
  hrdata.test$prob <- test_pred
  View(hrdata.test)
  
  # Probability cutoff of 50%.
  test_pred_attrition <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
  test_actual_attrition <- factor(ifelse(hrdata.test$Attrition==1,"Yes","No"))
  
  
  table(test_actual_attrition,test_pred_attrition)
  
  #                         test_pred_attrition
  # test_actual_attrition   No      Yes
  #                     No  1091    19
  #                     Yes  168    45
  
  
  #######################################################################
  test_pred_attrition <- factor(ifelse(test_pred >= 0.40, "Yes", "No"))
  
  ### install.packages("e1071")
  
  test_conf <- confusionMatrix(test_pred_attrition, test_actual_attrition, positive = "Yes")
  test_conf
  
  # Confusion Matrix and Statistics
  # 
  # Reference
  # Prediction   No  Yes
  # 		No  1065  141
  # 		Yes   45   72
  # 
  # Accuracy : 0.8594          
  # 95% CI : (0.8395, 0.8777)
  # No Information Rate : 0.839           
  # P-Value [Acc > NIR] : 0.02228         
  # 
  # Kappa : 0.3637          
  # Mcnemar's Test P-Value : 3.267e-12       
  # 
  # Sensitivity : 0.33803         
  # Specificity : 0.95946         
  # Pos Pred Value : 0.61538         
  # Neg Pred Value : 0.88308         
  # Prevalence : 0.16100         
  # Detection Rate : 0.05442         
  # Detection Prevalence : 0.08844         
  # Balanced Accuracy : 0.64874         
  # 
  # 'Positive' Class : Yes             
  

  ### Let's Choose the cutoff value. 
  ### Let's find out the optimal probalility cutoff 
  
  perform_fn <- function(cutoff) 
  {
    predicted_attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
    conf <- confusionMatrix(predicted_attrition, test_actual_attrition, positive = "Yes")
    acc <- conf$overall[1]
    sens <- conf$byClass[1]
    spec <- conf$byClass[2]
    out <- t(as.matrix(c(sens, spec, acc))) 
    colnames(out) <- c("sensitivity", "specificity", "accuracy")
    return(out)
  }
  
  
  ### Summary of test probability
  
  summary(test_pred)
  
  s = seq(.01,.80,length=100)
  
  OUT = matrix(0,100,3)
  
  
  for(i in 1:100)
  {
    OUT[i,] = perform_fn(s[i])
  } 
  
  
  plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
  axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
  axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
  lines(s,OUT[,2],col="darkgreen",lwd=2)
  lines(s,OUT[,3],col=4,lwd=2)
  box()
  legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))
  
  
  cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]
  
  
  ### Let's choose a cutoff value of 0.169596 for final model
  
  test_cutoff_attrition <- factor(ifelse(test_pred >=0.169596, "Yes", "No"))
  
  conf_final <- confusionMatrix(test_cutoff_attrition, test_actual_attrition, positive = "Yes")
  
  acc <- conf_final$overall[1]
  
  sens <- conf_final$byClass[1]
  
  spec <- conf_final$byClass[2]
  
  acc
  
  sens
  
  spec
  
  View(hrdata.test)
  
  ### KS -statistic - Test Data ######
  
  test_cutoff_attrition <- ifelse(test_cutoff_attrition=="Yes",1,0)
  test_actual_attrition <- ifelse(test_actual_attrition=="Yes",1,0)
  
  
  #on testing  data
  pred_object_test<- prediction(test_cutoff_attrition, test_actual_attrition)
  
  performance_measures_test<- performance(pred_object_test, "tpr", "fpr")
  
  ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
    (attr(performance_measures_test, "x.values")[[1]])
  
  max(ks_table_test)
  
  ### Lift & Gain Chart 
  
  ### plotting the lift chart
  
  
  lift <- function(labels , predicted_prob,groups=10) {
    
    if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
    if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
    helper = data.frame(cbind(labels , predicted_prob))
    helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
    gaintable = helper %>% group_by(bucket)  %>%
      summarise_at(vars(labels ), funs(total = n(),
                                       totalresp=sum(., na.rm = TRUE))) %>%
      
      mutate(Cumresp = cumsum(totalresp),
             Gain=Cumresp/sum(totalresp)*100,
             Cumlift=Gain/(bucket*(100/groups))) 
    return(gaintable)
  }
  
  attrition_decile = lift(test_actual_attrition, test_pred, groups = 10)
  
  View(attrition_decile)
 #------------------------------------------------------------------------------------------------------------------------- 
 