#setwd("D:\\Learning\\PGDDS\\AssignmentUber")

library(lubridate)
library(ggplot2)
library(stringr)
library(dplyr)
library(tidyr)
library(cowplot)
library(ggthemes)
library(scales)
library(forcats)

# 1. Load Data
  uber_df <- read.csv("Uber Request Data.csv", header = TRUE, stringsAsFactors = FALSE)

#-----------------------------------------------------------------------------------------------------------------

# 2. Data Cleaning and Preparation
#   Identify the data quality issues and clean the data so that you can use it for analysis.
#   Ensure that the dates and time are in the proper format. Derive new variables which will be useful for analysis.

  
  
  getTimeSlot <- function(x) 
  {
    if(x<4) return("Night")
    else if (x>=4 && x<8) return("Early morning")
    else if (x>=8 && x<12) return("Late morning")
    else if (x>=12 && x<15) return("Early afternoon")
    else if (x>=15 && x<18) return("Late afternoon")
    else if (x>=18 && x<21) return("Early evening")
    else if (x>=21 && x<23) return("Late evening")
    else return("Night")
  }

# Checking for duplicate rows
  nrow(distinct(uber_df))
#------------
# CONCLUSION - No duplicate rows found
#------------

# Check NA in Request.timestamp column
  sum(is.na(uber_df$Request.timestamp))

# Convert Request.timestamp column to proper date time format
  uber_df$Request.timestamp <- parse_date_time(uber_df$Request.timestamp, orders = c("dmY" , "dmY HM" , "dmy HMS"))

# Check if any NA generated after conversion
  sum(is.na(uber_df$Request.timestamp))
#------------
# CONCLUSION - There were no NA's in Request.timestamp and all the values have been converted to POSIXct format
#------------

# Check NA in Drop.timestamp column
  sum(is.na(uber_df$Drop.timestamp))

# Convert Drop.timestamp column to proper date time format
  uber_df$Drop.timestamp <- parse_date_time(uber_df$Drop.timestamp, orders = c("dmY" , "dmY HM" , "dmy HMS"))

# Check if any new NA generated after conversion
  sum(is.na(uber_df$Drop.timestamp))
#------------
# CONCLUSION - There were no NA's in Request.timestamp and all the values have been converted to POSIXct format
#------------

# Add a new columns for analysis
  uber_df$request_hour_of_day <- format(uber_df$Request.timestamp, "%H")
  
  #------------
  # CONCLUSION - request_hour_of_day is an Ordered Categorical Variable
  #------------
  
  uber_df$day <- weekdays(uber_df$Request.timestamp)
  
  uber_df$time_slot <- sapply(as.numeric(uber_df$request_hour_of_day), getTimeSlot) 
  
  uber_df$time_slot <- factor(uber_df$time_slot)
  
  uber_df$Status <- factor(uber_df$Status)

  write.csv(uber_df, file = "UberFormated.csv")
  
  slot_order <- c("Early morning", "Late morning", "Early afternoon", "Late afternoon", "Early evening", "Late evening", "Night")
  
  
#-----------------------------------------------------------------------------------------------------------------

# 3. Results Expected
# Visually identify the most pressing problems for Uber. 
# Hint: Create plots to visualize the frequency of requests that get canceled or show 'no cars available'; identify the most problematic types of requests (city to airport / airport to city etc.) and the time slots (early mornings, late evenings etc.) using plots

  # Analysis based on request hour
  airport_request_time <- ggplot(uber_df[uber_df$Pickup.point=="Airport",], aes(x=request_hour_of_day, fill= Status)) + geom_bar() + labs(title = "Airport to City", x = "Hour", y= "Requests")  + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  city_request_time <- ggplot(uber_df[uber_df$Pickup.point=="City",], aes(x=request_hour_of_day, fill= Status)) + geom_bar() + labs(title = "City to Airport", color="Status", x = "Hour", y= "Requests")  + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  plot_grid(airport_request_time, city_request_time, labels = "AUTO")

  # Analysis based on request time slot
  airport_request_slot <- ggplot(uber_df[uber_df$Pickup.point=="Airport",], aes(x=time_slot, fill= Status)) + geom_bar() + scale_x_discrete(limits=slot_order)
  airport_request_slot <- airport_request_slot + labs(title = "Airport to City", x = "Time Slot", y= "Requests")  + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  city_request_slot <- ggplot(uber_df[uber_df$Pickup.point=="City",], aes(x=time_slot, fill= Status)) + geom_bar()  + scale_x_discrete(limits=slot_order)
  city_request_slot <- city_request_slot + labs(title = "City to Airport", x = "Time Slot", y= "Requests") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  plot_grid(airport_request_slot, city_request_slot)

#------------
# CONCLUSION 
#   High demand during early evening between 6:00 PM to 9:00 PM from Airport to City but availability is very low
#   High demand during morning between 4:00 AM to 12:00 noon from City to Airport, but high number of trip cancellations results in revenue loss and dent on Uber's reputation.
#------------

  
# Find out the gap between supply and demand and show the same using plots.

  airport_request <- uber_df %>% filter(Pickup.point == "Airport") %>% group_by(time_slot) %>% summarise(Requested = n(), Completed = length(Request.id[Status == "Trip Completed"]), Cancelled = length(Request.id[Status == "Cancelled"]), No_Car = length(Request.id[Status == "No Cars Available"]))
  city_request <- uber_df %>% filter(Pickup.point == "City") %>% group_by(time_slot) %>% summarise(Requested = n(), Completed = length(Request.id[Status == "Trip Completed"]), Cancelled = length(Request.id[Status == "Cancelled"]), No_Car = length(Request.id[Status == "No Cars Available"]))
  
  airport_request$gap <- round((airport_request$Requested - airport_request$Completed)/airport_request$Requested*100,2)
  city_request$gap <- round((city_request$Requested - city_request$Completed)/city_request$Requested*100,2)
  
  write.csv(airport_request, file = "airport_request.csv")
  write.csv(city_request, file = "city_request.csv")
  
  ap_gap <- ggplot(airport_request, aes(x=time_slot, y= gap, group = 1, label = gap)) + geom_point(color="blue")  + geom_line(size = 1, color="blue") 
  ap_gap <- ap_gap  +  labs(title = "Airport to City", x = "Time Slot", y= "Gap in Percentage") 
  ap_gap <- ap_gap + geom_text()+ theme_solarized() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + scale_x_discrete(limits=slot_order)
  
  cp_gap <- ggplot(city_request, aes(x=time_slot, y= gap, group = 1, label = gap)) + geom_point(color="green")  + geom_line(size = 1, color="green") 
  cp_gap <- cp_gap + labs(title = "City to Airport", x = "Time Slot", y= "Gap in Percentage")  
  cp_gap <- cp_gap + geom_text() + theme_solarized() + theme(axis.text.x = element_text(angle = 90, hjust = 1))+ scale_x_discrete(limits=slot_order)
  
  plot_grid(ap_gap, cp_gap, labels = "AUTO") 

  # Find the time slots when the highest gap exists

#------------
# CONCLUSION 
#     Airport to City - Early evening, between 6:00pm to 9:00pm
#     City to Airport - Early morning, between 4:00am to 8:00am
#------------


# Find the types of requests (city-airport or airport-city) for which the gap is the most severe in the identified time slots

#------------
# CONCLUSION 
#     Airport to City - In early evening, supply demand gap is almost 80%
#------------


# What do you think is the reason for this issue for the supply-demand gap? Write the answer in less than 100 words. You may accompany the write-up with plot(s).

  request_summary <- airport_request[,c("time_slot", "Requested")]
  colnames(request_summary)[2] <- "AirportPickup"
  request_summary <- merge(request_summary, city_request[, c("time_slot", "Requested")])
  colnames(request_summary)[3] <- "CityPickup"
  
  request_diff <- ggplot(request_summary, aes(x=time_slot, group = 1))
  request_diff <- request_diff + geom_line(aes(y=AirportPickup), color="blue")
  request_diff <- request_diff + geom_line(aes(y=CityPickup), color="green") 
  request_diff <- request_diff + labs(title = "Airport Vs City Pickup Request", x = "Time Slot", y= "Request")
  request_diff <- request_diff + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + scale_x_discrete(limits=slot_order)
  request_diff

#------------
# CONCLUSION 
#     We observe very low requests from Airport to City during early afternoon between 12:00pm to 3:00 pm and even during morning slots number 
#     of requests from Airport to city is less. This results in higher waiting time for drivers who reach airport during this time slot. 
#     Since Uber drivers have minimum number of trips to complete per day to get their incentives, they prefer not to wait in Airport during 
#     low request period and hence tend to cancel trips to Airport during morning timeslots. 
#
#     Similarly after early morning, there is very low number of requests for airport trips from city. This is resulting in no car during 
#     high demand time slot, i.e. early evening during 6:00pm to 9:00pm.
#------------


# Recommend some ways to resolve the supply-demand gap.
#   1.	Relax the minimum of trips if Drivers wait in Airport during shallow request time
#   2.	Provide incentives to drivers who wait in Airport so that more cars will be available during high demand.

  
#-----------------------------------------------------------------------------------------------------------------
#Additional Analysis

# Day of the week analysis
  ar_day <- ggplot(uber_df[uber_df$Pickup.point=="Airport",], aes(x=day, fill= factor(Status))) + geom_bar() + labs(title = "Airport to City", color="Status", x = "Hour", y= "Requests") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  cr_day <- ggplot(uber_df[uber_df$Pickup.point=="City",], aes(x=day, fill= factor(Status))) + geom_bar() + labs(title = "Airport to City", color="Status", x = "Hour", y= "Requests") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  plot_grid(ar_day, cr_day, labels = "AUTO")
#------------
# CONCLUSION - No significant observation
#------------

# Driver data analysis
  driverdata <- uber_df %>% group_by(Driver.id, Pickup.point) %>% summarise(tot_request = n(), tot_complete = length(Request.id[Status == "Trip Completed"]), tot_cancel = length(Request.id[Status == "Cancelled"]))
  driverdata$comple_rate <- round((driverdata$tot_cancel/driverdata$tot_request)*100,2)
#------------
# CONCLUSION - More number of drivers canceled their trip from City to Airport when compared with Airport to City
#------------