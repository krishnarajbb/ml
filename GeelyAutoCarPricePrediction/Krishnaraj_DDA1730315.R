#-------------------------------------------------------------------------------------------------------------------------
# Assignment- Linear Regression
#
# By: Krishnaraj Barvathaya
#-------------------------------------------------------------------------------------------------------------------------

##########################################################################################################################

setwd("D:\\Learning\\PGDDS\\03-02-LinearAssigntment")

library(reshape2)
library(MASS)
library(car)
library(dplyr)
library(tidyr)


# Data Load
#-------------------------------------------------------------------------------------------------------------------------
  carprice <- read.csv("CarPrice_Assignment.csv", stringsAsFactors = FALSE, header = TRUE)
  
  View(carprice)
  str(carprice)
  nrow(carprice)
                    

# Data Preparation
#-------------------------------------------------------------------------------------------------------------------------                                                                             
# Check for duplicate values in the data set.
  nrow(unique(carprice))
# Note that the total no. of observations in the dataset is still 205, indicating that there were no duplicates in the data set.
                                                                                      
# Check for missing values
  sum(is.na(carprice))
# No missing values in the dataset.
  
# Remove columns which are not healpful in analysis  
  carprice <- carprice[,-1]  

  carprice <- cbind(carprice[,-2], colsplit(carprice$CarName," ",c("carcompany","carmodel")))
  carprice <- carprice[,-26]
  
# Check for categorical values for there is any mistakes
  write.csv(carprice, "cleancarprice.csv")
  
# Fix categorical values
  carprice$carcompany[which(carprice$carcompany == "maxda")] <- "mazda"
  carprice$carcompany[which(carprice$carcompany == "porcshce")] <- "porsche"
  carprice$carcompany[which(carprice$carcompany == "toyouta")] <- "toyota"
  carprice$carcompany[which(carprice$carcompany == "vokswagen" | carprice$carcompany == "vw")] <- "volkswagen"
  
  carprice$carcompany <- toupper(carprice$carcompany)
  write.csv(carprice, "cleancarprice.csv")
  
# Converting Categorical variables to factors  
  cols_to_factor <- c('symboling','fueltype','aspiration','doornumber','carbody','drivewheel','enginelocation','enginetype','cylindernumber','fuelsystem','carcompany')
  carprice[, cols_to_factor] <- lapply(carprice[, cols_to_factor], factor)
  
  str(carprice)

# Outlier ellimination
#-------------------------------------------------------------------------------------------------------------------------
# Checking for outliers in wheelbase
  quantile(carprice$wheelbase,seq(0,1,0.01))
# Noticed a jump between 99% and 100%. So capping all values above 115.544 (99%) to 115.544
  carprice$wheelbase[which(carprice$wheelbase > 115.544)] <- 115.544

# Checking for outliers in carlength
  quantile(carprice$carlength,seq(0,1,0.01))
  boxplot(carprice$carlength)
# No significant jump noticed

# Checking for outliers in carwidth
  quantile(carprice$carwidth,seq(0,1,0.01))
  boxplot(carprice$carwidth)
# No significant jump noticed

# Checking for outliers in carheight
  quantile(carprice$carheight,seq(0,1,0.01))
  boxplot(carprice$carheight)
# No significant jump noticed

# Checking for outliers in curbweight
  quantile(carprice$curbweight,seq(0,1,0.01))
# Noticed a jump between 0% and 1%. So floorING all the values below 1819.72(1% value) to 1819.72
  carprice$curbweight[which(carprice$curbweight < 1819.72)] <- 1819.72

# Checking for outliers in enginesize
  quantile(carprice$enginesize,seq(0,1,0.01))
# Noticed a jump between 96% and 100%. So capping all values above 209.00 to 209.00
  carprice$enginesize[which(carprice$enginesize < 209.00)] <- 209.00

# Checking for outliers in boreratio
  quantile(carprice$boreratio,seq(0,1,0.01))
  boxplot(carprice$boreratio)
# No significant jump noticed

# Checking for outliers in stroke
  quantile(carprice$stroke,seq(0,1,0.01))
# No significant jump noticed

# Checking for outliers in compressionratio
  quantile(carprice$compressionratio,seq(0,1,0.01))
  boxplot(carprice$compressionratio)
# Noticed a jump between 90% and 91%. So capping all values above 10.9400 to 10.9400
  carprice$compressionratio[which(carprice$compressionratio > 10.9400)] <- 10.9400

# Checking for outliers in horsepower
  quantile(carprice$horsepower,seq(0,1,0.01))
  boxplot(carprice$horsepower)
# Noticed a jump between 97% and 98%. So capping all values above 184.00 to 184.00
  carprice$horsepower[which(carprice$horsepower > 184.00)] <- 184.00

# Checking for outliers in peakrpm
  quantile(carprice$peakrpm,seq(0,1,0.01))
  boxplot(carprice$peakrpm)
# Noticed a jump between 99% and 100%. So capping all values above 6000 to 6000
  carprice$peakrpm[which(carprice$peakrpm > 6000)] <- 6000

# Checking for outliers in citympg
  quantile(carprice$citympg,seq(0,1,0.01))
  boxplot(carprice$citympg)
# Noticed a jump between 98% and 99%. So capping all values above 38.00 to 38.00
  carprice$citympg[which(carprice$citympg > 38.00)] <- 38.00

# Checking for outliers in highwaympg
  quantile(carprice$highwaympg,seq(0,1,0.01))
# No significant jump noticed

# Numeric Factors  
#-------------------------------------------------------------------------------------------------------------------------
# Convert Factors having lelvel 2 into numeric 
  levels(carprice$fueltype)<-c(0,1)
  carprice$fueltype <- as.numeric(levels(carprice$fueltype))[carprice$fueltype]
  
  levels(carprice$aspiration)<-c(0,1)
  carprice$aspiration <- as.numeric(levels(carprice$aspiration))[carprice$aspiration]
  
  levels(carprice$doornumber)<-c(0,1)
  carprice$doornumber <- as.numeric(levels(carprice$doornumber))[carprice$doornumber]
  
  levels(carprice$enginelocation)<-c(0,1)
  carprice$enginelocation <- as.numeric(levels(carprice$enginelocation))[carprice$enginelocation]
  
  View(carprice)
  str(carprice)
  
# Convert factor symboling to numeric
  # levels(carprice$symboling)[1] <- "safe"
  # levels(carprice$symboling)[2:4] <- "neutral"
  # levels(carprice$symboling)[3:5] <- "risky"

  dummy <- data.frame(model.matrix( ~symboling, data = carprice))
  carprice <- cbind(carprice[,-1], dummy[,-1])
  
  View(carprice)
  str(carprice)
  
# Convert factor carbody to numeric
  dummy <- data.frame(model.matrix( ~carbody, data = carprice))
  carprice <- cbind(carprice[,-4], dummy[,-1])

# Convert factor drivewheel to numeric
  dummy <- data.frame(model.matrix( ~drivewheel, data = carprice))
  carprice <- cbind(carprice[,-4], dummy[,-1])
  
# Convert factor enginetype to numeric
  dummy <- data.frame(model.matrix( ~enginetype, data = carprice))
  carprice <- cbind(carprice[,-10], dummy[,-1])
  
# Convert factor cylindernumber to numeric
  dummy <- data.frame(model.matrix( ~cylindernumber, data = carprice))
  carprice <- cbind(carprice[,-10], dummy[,-1])
  
# Convert factor fuelsystem to numeric
  dummy <- data.frame(model.matrix( ~fuelsystem, data = carprice))
  carprice <- cbind(carprice[,-11], dummy[,-1])

# Convert factor carcompany to numeric
  dummy <- data.frame(model.matrix( ~carcompany, data = carprice))
  carprice <- cbind(carprice[,-19], dummy[,-1])
  
  View(carprice)
  str(carprice)
  
# Train and Test Data
#------------------------------------------------------------------------------------------------------------------------- 
  set.seed(100)
  trainindices= sample(1:nrow(carprice), 0.7*nrow(carprice))
  # Generate the train data set
  carprice.train = carprice[trainindices,]
  View(carprice.train)
  
  # Rest of the observations into test data set
  carprice.test = carprice[-trainindices,]
  View(carprice.test)

# Data Modeling 
#------------------------------------------------------------------------------------------------------------------------- 
  model_1 <- lm(price~.,carprice.train)
  summary(model_1)
  # Multiple R-squared:  0.9819,	Adjusted R-squared:  0.9691
  
  step <- stepAIC(model_1, direction = "both")
  
  step
  
  model_2 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                  curbweight + enginesize + peakrpm + carbodyhardtop + carbodyhatchback + 
                  carbodysedan + carbodywagon + drivewheelfwd + enginetypedohcv + 
                  enginetypel + enginetypeohcf + enginetyperotor + cylindernumbersix + 
                  cylindernumberthree + carcompanyBMW + carcompanyBUICK + carcompanyDODGE + 
                  carcompanyHONDA + carcompanyMAZDA + carcompanyMITSUBISHI + 
                  carcompanyNISSAN + carcompanyPLYMOUTH + carcompanyRENAULT + 
                  carcompanyTOYOTA + carcompanyVOLKSWAGEN, data = carprice.train)
  summary(model_2)
  vif(model_2)
  # Multiple R-squared:  0.9798,	Adjusted R-squared:  0.9749
  
  # Though curbweight, carbodyhatchback and few other attributes have high VIF, they cannot be removed since these are highly significant. 
  # With available least significant attribute drivewheelfwd has high VIF, so removing it from model_3
  model_3 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                  curbweight + enginesize + peakrpm + carbodyhardtop + carbodyhatchback + 
                  carbodysedan + carbodywagon + enginetypedohcv + 
                  enginetypel + enginetypeohcf + enginetyperotor + cylindernumbersix + 
                  cylindernumberthree + carcompanyBMW + carcompanyBUICK + carcompanyDODGE + 
                  carcompanyHONDA + carcompanyMAZDA + carcompanyMITSUBISHI + 
                  carcompanyNISSAN + carcompanyPLYMOUTH + carcompanyRENAULT + 
                  carcompanyTOYOTA + carcompanyVOLKSWAGEN, data = carprice.train)
  
  summary(model_3)
  vif(model_3)
  # Multiple R-squared:  0.979,	Adjusted R-squared:  0.974

  
  # Both carbodyhatchback and carbodysedan have high VIF in both model, so need to check their correlation as they might be highly correlated
  cor(carprice.train$carbodyhatchback, carprice.train$carbodysedan)
  # OBSERVATION: They are negatively correlated, so cannot be removed
  
  # Also check for correlation with curbweight
  cor(carprice.train$carbodyhatchback, carprice.train$curbweight)
  cor(carprice.train$carbodysedan, carprice.train$curbweight)
  cor(carprice.train$carwidth, carprice.train$carbodywagon)
  # OBSERVATION: No correlation obsereved 
  
  # Check correlation of curbweight and carwidth
  cor(carprice.train$carwidth, carprice.train$curbweight)
  # Correlation of ~87%, Both the are highly correlated, removing least significant curbweight
  
  model_4 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                   enginesize + peakrpm + carbodyhardtop + carbodyhatchback + 
                   carbodysedan + carbodywagon + enginetypedohcv + 
                   enginetypel + enginetypeohcf + enginetyperotor + cylindernumbersix + 
                   cylindernumberthree + carcompanyBMW + carcompanyBUICK + carcompanyDODGE + 
                   carcompanyHONDA + carcompanyMAZDA + carcompanyMITSUBISHI + 
                   carcompanyNISSAN + carcompanyPLYMOUTH + carcompanyRENAULT + 
                   carcompanyTOYOTA + carcompanyVOLKSWAGEN, data = carprice.train)
  
  summary(model_4)
  vif(model_4)
  # Multiple R-squared:  0.9716,	Adjusted R-squared:  0.9653
  
  # Next least significant variable with high VIF is peakrpm, removing it from model_4
  model_5 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                  enginesize + carbodyhardtop + carbodyhatchback + 
                  carbodysedan + carbodywagon + enginetypedohcv + 
                  enginetypel + enginetypeohcf + enginetyperotor + cylindernumbersix + 
                  cylindernumberthree + carcompanyBMW + carcompanyBUICK + carcompanyDODGE + 
                  carcompanyHONDA + carcompanyMAZDA + carcompanyMITSUBISHI + 
                  carcompanyNISSAN + carcompanyPLYMOUTH + carcompanyRENAULT + 
                  carcompanyTOYOTA + carcompanyVOLKSWAGEN, data = carprice.train)
  
  summary(model_5)
  vif(model_5)
  # Multiple R-squared:  0.9713,	Adjusted R-squared:  0.9651 
  
  # Next least significant variable with high VIF is cylindernumberthree, removing it from model_4
  model_6 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                  enginesize + carbodyhardtop + carbodyhatchback + 
                  carbodysedan + carbodywagon + enginetypedohcv + 
                  enginetypel + enginetypeohcf + enginetyperotor + cylindernumbersix + 
                  carcompanyBMW + carcompanyBUICK + carcompanyDODGE + 
                  carcompanyHONDA + carcompanyMAZDA + carcompanyMITSUBISHI + 
                  carcompanyNISSAN + carcompanyPLYMOUTH + carcompanyRENAULT + 
                  carcompanyTOYOTA + carcompanyVOLKSWAGEN, data = carprice.train)
  
  summary(model_6)
  vif(model_6)
  # Multiple R-squared:  0.9702,	Adjusted R-squared:  0.9641 
  
  # Since all least significant variables are removed, now will try to remove highly significant variable having high VIF, carbodyhatchback
  model_7 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                  enginesize + carbodyhardtop +  
                  carbodysedan + carbodywagon + enginetypedohcv + 
                  enginetypel + enginetypeohcf + enginetyperotor + cylindernumbersix + 
                  carcompanyBMW + carcompanyBUICK + carcompanyDODGE + 
                  carcompanyHONDA + carcompanyMAZDA + carcompanyMITSUBISHI + 
                  carcompanyNISSAN + carcompanyPLYMOUTH + carcompanyRENAULT + 
                  carcompanyTOYOTA + carcompanyVOLKSWAGEN, data = carprice.train)
  
  summary(model_7)
  vif(model_7)
  # Multiple R-squared:  0.9617,	Adjusted R-squared:  0.9543 
  # Since it has not effected the R-squared, we can remove
  
  # Now will try to remove next highly significant variable having high VIF, carwidth
  model_8 <- lm(formula = price ~ aspiration + enginelocation + 
                  enginesize + carbodyhardtop +  
                  carbodysedan + carbodywagon + enginetypedohcv + 
                  enginetypel + enginetypeohcf + enginetyperotor + cylindernumbersix + 
                  carcompanyBMW + carcompanyBUICK + carcompanyDODGE + 
                  carcompanyHONDA + carcompanyMAZDA + carcompanyMITSUBISHI + 
                  carcompanyNISSAN + carcompanyPLYMOUTH + carcompanyRENAULT + 
                  carcompanyTOYOTA + carcompanyVOLKSWAGEN, data = carprice.train)
  
  summary(model_8)
  vif(model_8)
  # Multiple R-squared:  0.9215,	Adjusted R-squared:  0.9071
  # Since it has not effected the R-squared much, we can remove
  
  # Now will try to remove next highly significant variable having high VIF, enginelocation
  model_9 <- lm(formula = price ~ aspiration +  
                  enginesize + carbodyhardtop +  
                  carbodysedan + carbodywagon + enginetypedohcv + 
                  enginetypel + enginetypeohcf + enginetyperotor + cylindernumbersix + 
                  carcompanyBMW + carcompanyBUICK + carcompanyDODGE + 
                  carcompanyHONDA + carcompanyMAZDA + carcompanyMITSUBISHI + 
                  carcompanyNISSAN + carcompanyPLYMOUTH + carcompanyRENAULT + 
                  carcompanyTOYOTA + carcompanyVOLKSWAGEN, data = carprice.train)
  
  summary(model_9)
  vif(model_9)
  # Multiple R-squared:  0.8685,	Adjusted R-squared:  0.8457
  # Since this is effecting, we will not remove
  
  # Now will try to remove next highly significant variable having high VIF, carcompanyTOYOTA
  model_10 <- lm(formula = price ~ aspiration + enginelocation + 
                  enginesize + carbodyhardtop +  
                  carbodysedan + carbodywagon + enginetypedohcv + 
                  enginetypel + enginetypeohcf + enginetyperotor + cylindernumbersix + 
                  carcompanyBMW + carcompanyBUICK + carcompanyDODGE + 
                  carcompanyHONDA + carcompanyMAZDA + carcompanyMITSUBISHI + 
                  carcompanyNISSAN + carcompanyPLYMOUTH + carcompanyRENAULT + 
                  carcompanyVOLKSWAGEN, data = carprice.train)
  summary(model_10)
  vif(model_10)
  # Multiple R-squared:  0.8804,	Adjusted R-squared:  0.8597
  # Since this is effecting, we will not remove  
  
  # Now will try to remove least significant vaiable carbodyhardtop from model_8 
  model_11 <- lm(formula = price ~ aspiration + enginelocation + 
                  enginesize +   
                  carbodysedan + carbodywagon + enginetypedohcv + 
                  enginetypel + enginetypeohcf + enginetyperotor + cylindernumbersix + 
                  carcompanyBMW + carcompanyBUICK + carcompanyDODGE + 
                  carcompanyHONDA + carcompanyMAZDA + carcompanyMITSUBISHI + 
                  carcompanyNISSAN + carcompanyPLYMOUTH + carcompanyRENAULT + 
                  carcompanyTOYOTA + carcompanyVOLKSWAGEN, data = carprice.train)
  
  summary(model_11)
  vif(model_11)
  # Multiple R-squared:  0.9213,	Adjusted R-squared:  0.9076 
  # Since it has not effected the R-squared much, we can remove
  
  # Now will try to remove least significant vaiable carbodysedan
  model_12 <- lm(formula = price ~ aspiration + enginelocation + 
                   enginesize +   
                   carbodywagon + enginetypedohcv + 
                   enginetypel + enginetypeohcf + enginetyperotor + cylindernumbersix + 
                   carcompanyBMW + carcompanyBUICK + carcompanyDODGE + 
                   carcompanyHONDA + carcompanyMAZDA + carcompanyMITSUBISHI + 
                   carcompanyNISSAN + carcompanyPLYMOUTH + carcompanyRENAULT + 
                   carcompanyTOYOTA + carcompanyVOLKSWAGEN, data = carprice.train)
  
  summary(model_12)
  vif(model_12)
  # Multiple R-squared:  0.9209,	Adjusted R-squared:  0.9079 
  # Since it has not effected the R-squared much, we can remove
  
  # Now will try to remove least significant vaiable carbodywagon
  model_13 <- lm(formula = price ~ aspiration + enginelocation + 
                   enginesize +   
                   enginetypedohcv + 
                   enginetypel + enginetypeohcf + enginetyperotor + cylindernumbersix + 
                   carcompanyBMW + carcompanyBUICK + carcompanyDODGE + 
                   carcompanyHONDA + carcompanyMAZDA + carcompanyMITSUBISHI + 
                   carcompanyNISSAN + carcompanyPLYMOUTH + carcompanyRENAULT + 
                   carcompanyTOYOTA + carcompanyVOLKSWAGEN, data = carprice.train)
  
  summary(model_13)
  vif(model_13)
  # Multiple R-squared:  0.9204,	Adjusted R-squared:  0.9081
  # Since it has not effected the R-squared much, we can remove
  
  # Now will try to remove least significant vaiable enginetypel
  model_14 <- lm(formula = price ~ aspiration + enginelocation + 
                   enginesize +   
                   enginetypedohcv + 
                    enginetypeohcf + enginetyperotor + cylindernumbersix + 
                   carcompanyBMW + carcompanyBUICK + carcompanyDODGE + 
                   carcompanyHONDA + carcompanyMAZDA + carcompanyMITSUBISHI + 
                   carcompanyNISSAN + carcompanyPLYMOUTH + carcompanyRENAULT + 
                   carcompanyTOYOTA + carcompanyVOLKSWAGEN, data = carprice.train)
  
  summary(model_14)
  vif(model_14)
  # Multiple R-squared:  0.9165,	Adjusted R-squared:  0.9043
  # Since it has not effected the R-squared much, we can remove
  
  
  # Now will try to remove least significant vaiable carcompanyRENAULT
  model_15 <- lm(formula = price ~ aspiration + enginelocation + 
                   enginesize +   
                   enginetypedohcv + 
                   enginetypeohcf + enginetyperotor + cylindernumbersix + 
                   carcompanyBMW + carcompanyBUICK + carcompanyDODGE + 
                   carcompanyHONDA + carcompanyMAZDA + carcompanyMITSUBISHI + 
                   carcompanyNISSAN + carcompanyPLYMOUTH +  
                   carcompanyTOYOTA + carcompanyVOLKSWAGEN, data = carprice.train)
  
  summary(model_15)
  vif(model_15)
  # Multiple R-squared:  0.9126,	Adjusted R-squared:  0.9007
  # Since it has not effected the R-squared much, we can remove
  
  # Now will try to remove least significant vaiable enginetyperotor
  model_16 <- lm(formula = price ~ aspiration + enginelocation + 
                   enginesize +   
                   enginetypedohcv + 
                   enginetypeohcf + cylindernumbersix + 
                   carcompanyBMW + carcompanyBUICK + carcompanyDODGE + 
                   carcompanyHONDA + carcompanyMAZDA + carcompanyMITSUBISHI + 
                   carcompanyNISSAN + carcompanyPLYMOUTH +  
                   carcompanyTOYOTA + carcompanyVOLKSWAGEN, data = carprice.train)
  
  summary(model_16)
  vif(model_16)
  # Multiple R-squared:  0.9086,	Adjusted R-squared:  0.897
  # Since it has not effected the R-squared much, we can remove
  
  # With model_16 we have all significant variables with VIF<2
  
  
# Predict price for test data
#------------------------------------------------------------------------------------------------------------------------- 

  Predict <- predict(model_2, carprice.test[, -18])
  (cor(carprice.test$price, Predict))^2
  # With model_2 Adjusted R-squared:  0.9749 & test data R-sqared 86.1%
  
  Predict <- predict(model_6, carprice.test[, -18])
  (cor(carprice.test$price, Predict))^2
  # With model_6 Adjusted R-squared:  0.9641 & test data R-sqared 82.87%
  
  Predict <- predict(model_16, carprice.test[, -18])
  (cor(carprice.test$price, Predict))^2
  # With model_16 Adjusted R-squared:  89.70% & test data R-sqared 72.19%

#------------------------------------------------------------------------------------------------------------------------- 
# Conclusion: model_16 can be used to predict the price of car 
#------------------------------------------------------------------------------------------------------------------------- 
  