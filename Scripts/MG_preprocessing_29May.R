#******************************************************************************************#
# Script for analysis of MG data for paper 5 and 6                                         #
# Authors: K Bhargava                                                                      #
# Last updated on: 29th May, 2020                                                          #
#******************************************************************************************#

#******************************************************************************************#
# Import libraries for manipulating time series and plotting
library(tidyverse)
library(lubridate)
library(wesanderson)
library(here)
#******************************************************************************************#

#******************************************************************************************#
# Set path for input and output files
filepath <- "../"
plot_dir <- "../Plots"
#******************************************************************************************#

#******************************************************************************************#
# Read all raw data adn add month, date and time 
cpe <- read_csv(here(filepath,"CPE_all.csv"), col_names = TRUE)
cpe <- cpe %>% mutate(date = date(timestamp), month=as.character(month(timestamp, label=TRUE, abbr=TRUE)), 
                        timeUse = hour(timestamp)) 
sockets <- read_csv(here(filepath,"Sockets_all.csv"), col_names = TRUE) 
sockets <- sockets %>% mutate(date = date(timestamp), month=as.character(month(timestamp, label=TRUE, abbr=TRUE)), 
                      timeUse = hour(timestamp)) 
system <- read_csv(here(filepath,"systemData_jul_dec.csv"), col_names = TRUE)  
system <- system %>% mutate(month=as.character(month(timestamp, label=TRUE, abbr=TRUE)), 
                              timeUse = hour(timestamp)) 
weather <- read_csv(here(filepath,"weather_hourly_jul_dec.csv"), col_names = TRUE) 
weather <- weather %>% mutate(month=as.character(month(timestamp, label=TRUE, abbr=TRUE)), 
                            timeUse = hour(timestamp)) 
#******************************************************************************************#

#******************************************************************************************#
# Convert data into hourly mean, fill in NA for all days missing, add system and Potential PV data 
cpe_hourly <- cpe %>%
  group_by(id, date, timeUse, variable) %>%
  summarise(value=mean(value, na.rm=TRUE))
cpe_hourly <- as.data.frame(cpe_hourly)
cpe_hourly <- cpe_hourly[!is.na(cpe_hourly$id),]
cpe_hourly <- cpe_hourly[cpe_hourly$date>="2019-07-02",]
cpe_hourly <- cpe_hourly %>% mutate(id = paste(id, variable, sep=" "))
cpe_hourly <- cpe_hourly[,-4] # Remove variable

sockets_hourly <- data.frame()
for(i in 1:length(unique(sockets$id))) {
  df <- sockets[sockets$id == unique(sockets$id)[i], ]

  #Extract hourly means for instantaneous power
  df2 <- df[df$variable!="AC_Day_Energy_session",] %>%
    group_by(id, date, timeUse, variable) %>%
    summarise(value = mean(value, na.rm=TRUE))
  df2 <- as.data.frame(df2)
  
  #Correct cumulative total energy if not reset at 0th hour for a day
  df_id <- df[df$variable=="AC_Day_Energy_session", ]
  df3_clean <- data.frame()
  for(e in 1:length(unique(df_id$date))) {
    df_date <- df_id[df_id$date == unique(df_id$date)[e], ]
    if(df_date$value[1]!=0) { #If value has not reset
      #find if and where the value has reset
      flag=FALSE
      if(length(df_date$timestamp)>1) {
        for(l in 2:length(df_date$timestamp)) {
          if(flag==FALSE & df_date$value[l] < df_date$value[l-1]) {
            flag=TRUE
            index <- l
          }
        }
        if(flag==FALSE) { #Means the value never reset to 0
          #Subtract value at 0th index from all values
          df_date$value2 <- df_date$value - df_date$value[1]
        } else {
          df_date$value2 <- df_date$value
          df_date$value2[1:(index-1)] <- (df_date$value[1:(index-1)] - df_date$value[1])
          df_date$value2[index:length(df_date$timestamp)] <- 
            df_date$value[index:length(df_date$timestamp)] + df_date$value2[index-1]
        }
      } else {
        df_date$value2 <- df_date$value
      }
    } else {
      df_date$value2 <- df_date$value
    }
    df3_clean <- rbind(df3_clean, df_date)
  }
  
  #Extract cumulative total energy per hour
  df3 <- df3_clean %>%
    group_by(id, date, timeUse, variable) %>%
    summarise(value = value2[length(na.omit(value2))])
  df3 <- as.data.frame(df3)
  
  #Bind the 2 data frames and add date from the file name
  df2 <- rbind(df2, df3)
  
  #Create a data frame with the above values and bind to a complete df 
  sockets_hourly <- rbind(sockets_hourly, df2)
}
sockets_hourly$value[sockets_hourly$variable=="AC_Day_Energy_session"] <- 
  sockets_hourly$value[sockets_hourly$variable=="AC_Day_Energy_session"] * 0.017 #Wm to Wh
sockets_hourly <- sockets_hourly[sockets_hourly$date>="2019-07-02",]
sockets_hourly <- sockets_hourly %>% mutate(id = paste(id, variable, sep=" "))
sockets_hourly <- sockets_hourly[,-4] # Remove variable
#******************************************************************************************#
