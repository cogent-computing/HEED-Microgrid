#Analysing microgrid data from Aug 2019
library(ggplot2)
library(dplyr)
library(lubridate)
library(readxl)
library(tidyverse)
library(stringr)
library(rlang)
library(readr)

#We start by analysing CPE files to extract instantaneous power - LED1_P, LED2_P, LED3_P
#We have a total of 20 CPE
#Nur1A - 1,2; Nur1B - 3,4; Nur1C - 5,6; Nur2A - 7,8; Nur2B - 9,10; Nur2C - 11,12
#PG- 1-5; #SL - SL1, SL2, SL3 

#For each CPE we first need to concatenate the files for each day - 31 files for each
cpe1 <- data.frame()
setwd("~/OneDrive - Coventry University/HEED_analysis/Micro-grid/Data/08_2019/Nur1A/CPE1/")
file_list <- list.files()
for(i in 1:length(file_list)) {
  df <- read.csv(file_list[i], header = FALSE, quote = "\"", skipNul = TRUE)
  colnames(df) <- c("timestamp", "variable", "value")
  df$value <- as.character(df$value)
  df$variable <- as.character(df$variable)
  
  #Separate the power consumption variables only - mW
  df_sub <- df[df$variable=="LED1_P" | df$variable=="LED2_P" | df$variable=="LED3_P",]
  df_sub$value <- as.numeric(df_sub$value)/1000.0
  df_sub$variable <- as.factor(df_sub$variable)
  
  #Add time - we need to extract hours to get hourly means
  df_sub$time <- as.POSIXct(df_sub$timestamp, tz="GMT", origin="1970-01-01")
  df_sub$timeUse <- format(df_sub$time, format='%H')
  
  #Extract hourly means for each variable - led1_p, led2_p, led3_p
  df2 <- df_sub %>%
    group_by(variable, timeUse) %>%
    summarise(value = mean(value))
  df2$variable <- as.character(df2$variable)
  
  #Add date from the file name
  df2$date <- as.Date((rep(gsub("\\..*","",file_list[i]), length(df2$value))),
                      format = "%d-%m-%Y")
  
  #Create a data frame with the above values and bind to a complete df 
  df <- data.frame(date=df2$date, hour=df2$timeUse, 
                   variable=df2$variable, value=df2$value)
  df$hour <- as.character(df$hour)
  cpe1 <- rbind(cpe1, df)
}
cpe1$id <- rep("Nur1A_CPE1", length(cpe1$value))

cpe2 <- data.frame()
setwd("~/OneDrive - Coventry University/HEED_analysis/Micro-grid/Data/08_2019/Nur1A/CPE2/")
file_list <- list.files()
for(i in 1:length(file_list)) {
  df <- read.csv(file_list[i], header = FALSE, quote = "\"", skipNul = TRUE)
  colnames(df) <- c("timestamp", "variable", "value")
  df$value <- as.character(df$value)
  df$variable <- as.character(df$variable)
  
  #Separate the power consumption variables only - mW
  df_sub <- df[df$variable=="LED1_P" | df$variable=="LED2_P" | df$variable=="LED3_P",]
  df_sub$value <- as.numeric(df_sub$value)/1000.0
  df_sub$variable <- as.factor(df_sub$variable)
  
  #Add time - we need to extract hours to get hourly means
  df_sub$time <- as.POSIXct(df_sub$timestamp, tz="GMT", origin="1970-01-01")
  df_sub$timeUse <- format(df_sub$time, format='%H')
  
  #Extract hourly means for each variable - led1_p, led2_p, led3_p
  df2 <- df_sub %>%
    group_by(variable, timeUse) %>%
    summarise(value = mean(value))
  df2$variable <- as.character(df2$variable)
  
  #Add date from the file name
  df2$date <- as.Date((rep(gsub("\\..*","",file_list[i]), length(df2$value))),
                      format = "%d-%m-%Y")
  
  #Create a data frame with the above values and bind to a complete df 
  df <- data.frame(date=df2$date, hour=df2$timeUse, 
                   variable=df2$variable, value=df2$value)
  df$hour <- as.character(df$hour)
  cpe2 <- rbind(cpe2, df)
}
cpe2$id <- rep("Nur1A_CPE2", length(cpe2$value))

cpe3 <- data.frame()
setwd("~/OneDrive - Coventry University/HEED_analysis/Micro-grid/Data/08_2019/Nur1B/CPE3/")
file_list <- list.files()
for(i in 1:length(file_list)) {
  df <- read.csv(file_list[i], header = FALSE, quote = "\"", skipNul = TRUE)
  colnames(df) <- c("timestamp", "variable", "value")
  df$value <- as.character(df$value)
  df$variable <- as.character(df$variable)
  
  #Separate the power consumption variables only - mW
  df_sub <- df[df$variable=="LED1_P" | df$variable=="LED2_P" | df$variable=="LED3_P",]
  df_sub$value <- as.numeric(df_sub$value)/1000.0
  df_sub$variable <- as.factor(df_sub$variable)
  
  #Add time - we need to extract hours to get hourly means
  df_sub$time <- as.POSIXct(df_sub$timestamp, tz="GMT", origin="1970-01-01")
  df_sub$timeUse <- format(df_sub$time, format='%H')
  
  #Extract hourly means for each variable - led1_p, led2_p, led3_p
  df2 <- df_sub %>%
    group_by(variable, timeUse) %>%
    summarise(value = mean(value))
  df2$variable <- as.character(df2$variable)
  
  #Add date from the file name
  df2$date <- as.Date((rep(gsub("\\..*","",file_list[i]), length(df2$value))),
                      format = "%d-%m-%Y")
  
  #Create a data frame with the above values and bind to a complete df 
  df <- data.frame(date=df2$date, hour=df2$timeUse, 
                   variable=df2$variable, value=df2$value)
  df$hour <- as.character(df$hour)
  cpe3 <- rbind(cpe3, df)
}
cpe3$id <- rep("Nur1B_CPE3", length(cpe3$value))

cpe4 <- data.frame()
setwd("~/OneDrive - Coventry University/HEED_analysis/Micro-grid/Data/08_2019/Nur1B/CPE4/")
file_list <- list.files()
for(i in 1:length(file_list)) {
  df <- read.csv(file_list[i], header = FALSE, quote = "\"", skipNul = TRUE)
  colnames(df) <- c("timestamp", "variable", "value")
  df$value <- as.character(df$value)
  df$variable <- as.character(df$variable)
  
  #Separate the power consumption variables only - mW
  df_sub <- df[df$variable=="LED1_P" | df$variable=="LED2_P" | df$variable=="LED3_P",]
  df_sub$value <- as.numeric(df_sub$value)/1000.0
  df_sub$variable <- as.factor(df_sub$variable)
  
  #Add time - we need to extract hours to get hourly means
  df_sub$time <- as.POSIXct(df_sub$timestamp, tz="GMT", origin="1970-01-01")
  df_sub$timeUse <- format(df_sub$time, format='%H')
  
  #Extract hourly means for each variable - led1_p, led2_p, led3_p
  df2 <- df_sub %>%
    group_by(variable, timeUse) %>%
    summarise(value = mean(value))
  df2$variable <- as.character(df2$variable)
  
  #Add date from the file name
  df2$date <- as.Date((rep(gsub("\\..*","",file_list[i]), length(df2$value))),
                      format = "%d-%m-%Y")
  
  #Create a data frame with the above values and bind to a complete df 
  df <- data.frame(date=df2$date, hour=df2$timeUse, 
                   variable=df2$variable, value=df2$value)
  df$hour <- as.character(df$hour)
  cpe4 <- rbind(cpe4, df)
}
cpe4$id <- rep("Nur1B_CPE4", length(cpe4$value))

cpe5 <- data.frame()
setwd("~/OneDrive - Coventry University/HEED_analysis/Micro-grid/Data/08_2019/Nur1C/CPE5/")
file_list <- list.files()
for(i in 1:length(file_list)) {
  df <- read.csv(file_list[i], header = FALSE, quote = "\"", skipNul = TRUE)
  colnames(df) <- c("timestamp", "variable", "value")
  df$value <- as.character(df$value)
  df$variable <- as.character(df$variable)
  
  #Separate the power consumption variables only - mW
  df_sub <- df[df$variable=="LED1_P" | df$variable=="LED2_P" | df$variable=="LED3_P",]
  df_sub$value <- as.numeric(df_sub$value)/1000.0
  df_sub$variable <- as.factor(df_sub$variable)
  
  #Add time - we need to extract hours to get hourly means
  df_sub$time <- as.POSIXct(df_sub$timestamp, tz="GMT", origin="1970-01-01")
  df_sub$timeUse <- format(df_sub$time, format='%H')
  
  #Extract hourly means for each variable - led1_p, led2_p, led3_p
  df2 <- df_sub %>%
    group_by(variable, timeUse) %>%
    summarise(value = mean(value))
  df2$variable <- as.character(df2$variable)
  
  #Add date from the file name
  df2$date <- as.Date((rep(gsub("\\..*","",file_list[i]), length(df2$value))),
                      format = "%d-%m-%Y")
  
  #Create a data frame with the above values and bind to a complete df 
  df <- data.frame(date=df2$date, hour=df2$timeUse, 
                   variable=df2$variable, value=df2$value)
  df$hour <- as.character(df$hour)
  cpe5 <- rbind(cpe5, df)
}
cpe5$id <- rep("Nur1C_CPE5", length(cpe5$value))

cpe6 <- data.frame()
setwd("~/OneDrive - Coventry University/HEED_analysis/Micro-grid/Data/08_2019/Nur1C/CPE6/")
file_list <- list.files()
for(i in 1:length(file_list)) {
  df <- read.csv(file_list[i], header = FALSE, quote = "\"", skipNul = TRUE)
  colnames(df) <- c("timestamp", "variable", "value")
  df$value <- as.character(df$value)
  df$variable <- as.character(df$variable)
  
  #Separate the power consumption variables only - mW
  df_sub <- df[df$variable=="LED1_P" | df$variable=="LED2_P" | df$variable=="LED3_P",]
  df_sub$value <- as.numeric(df_sub$value)/1000.0
  df_sub$variable <- as.factor(df_sub$variable)
  
  #Add time - we need to extract hours to get hourly means
  df_sub$time <- as.POSIXct(df_sub$timestamp, tz="GMT", origin="1970-01-01")
  df_sub$timeUse <- format(df_sub$time, format='%H')
  
  #Extract hourly means for each variable - led1_p, led2_p, led3_p
  df2 <- df_sub %>%
    group_by(variable, timeUse) %>%
    summarise(value = mean(value))
  df2$variable <- as.character(df2$variable)
  
  #Add date from the file name
  df2$date <- as.Date((rep(gsub("\\..*","",file_list[i]), length(df2$value))),
                      format = "%d-%m-%Y")
  
  #Create a data frame with the above values and bind to a complete df 
  df <- data.frame(date=df2$date, hour=df2$timeUse, 
                   variable=df2$variable, value=df2$value)
  df$hour <- as.character(df$hour)
  cpe6 <- rbind(cpe6, df)
}
cpe6$id <- rep("Nur1C_CPE6", length(cpe6$value))

cpe7 <- data.frame()
setwd("~/OneDrive - Coventry University/HEED_analysis/Micro-grid/Data/08_2019/Nur2A/CPE7/")
file_list <- list.files()
for(i in 1:length(file_list)) {
  df <- read.csv(file_list[i], header = FALSE, quote = "\"", skipNul = TRUE)
  colnames(df) <- c("timestamp", "variable", "value")
  df$value <- as.character(df$value)
  df$variable <- as.character(df$variable)
  
  #Separate the power consumption variables only - mW
  df_sub <- df[df$variable=="LED1_P" | df$variable=="LED2_P" | df$variable=="LED3_P",]
  df_sub$value <- as.numeric(df_sub$value)/1000.0
  df_sub$variable <- as.factor(df_sub$variable)
  
  #Add time - we need to extract hours to get hourly means
  df_sub$time <- as.POSIXct(df_sub$timestamp, tz="GMT", origin="1970-01-01")
  df_sub$timeUse <- format(df_sub$time, format='%H')
  
  #Extract hourly means for each variable - led1_p, led2_p, led3_p
  df2 <- df_sub %>%
    group_by(variable, timeUse) %>%
    summarise(value = mean(value))
  df2$variable <- as.character(df2$variable)
  
  #Add date from the file name
  df2$date <- as.Date((rep(gsub("\\..*","",file_list[i]), length(df2$value))),
                      format = "%d-%m-%Y")
  
  #Create a data frame with the above values and bind to a complete df 
  df <- data.frame(date=df2$date, hour=df2$timeUse, 
                   variable=df2$variable, value=df2$value)
  df$hour <- as.character(df$hour)
  cpe7 <- rbind(cpe7, df)
}
cpe7$id <- rep("Nur2A_CPE7", length(cpe7$value))

cpe8 <- data.frame()
setwd("~/OneDrive - Coventry University/HEED_analysis/Micro-grid/Data/08_2019/Nur2A/CPE8/")
file_list <- list.files()
for(i in 1:length(file_list)) {
  df <- read.csv(file_list[i], header = FALSE, quote = "\"", skipNul = TRUE)
  colnames(df) <- c("timestamp", "variable", "value")
  df$value <- as.character(df$value)
  df$variable <- as.character(df$variable)
  
  #Separate the power consumption variables only - mW
  df_sub <- df[df$variable=="LED1_P" | df$variable=="LED2_P" | df$variable=="LED3_P",]
  df_sub$value <- as.numeric(df_sub$value)/1000.0
  df_sub$variable <- as.factor(df_sub$variable)
  
  #Add time - we need to extract hours to get hourly means
  df_sub$time <- as.POSIXct(df_sub$timestamp, tz="GMT", origin="1970-01-01")
  df_sub$timeUse <- format(df_sub$time, format='%H')
  
  #Extract hourly means for each variable - led1_p, led2_p, led3_p
  df2 <- df_sub %>%
    group_by(variable, timeUse) %>%
    summarise(value = mean(value))
  df2$variable <- as.character(df2$variable)
  
  #Add date from the file name
  df2$date <- as.Date((rep(gsub("\\..*","",file_list[i]), length(df2$value))),
                      format = "%d-%m-%Y")
  
  #Create a data frame with the above values and bind to a complete df 
  df <- data.frame(date=df2$date, hour=df2$timeUse, 
                   variable=df2$variable, value=df2$value)
  df$hour <- as.character(df$hour)
  cpe8 <- rbind(cpe8, df)
}
cpe8$id <- rep("Nur2A_CPE8", length(cpe8$value))

cpe9 <- data.frame()
setwd("~/OneDrive - Coventry University/HEED_analysis/Micro-grid/Data/08_2019/Nur2B/CPE9/")
file_list <- list.files()
for(i in 1:length(file_list)) {
  df <- read.csv(file_list[i], header = FALSE, quote = "\"", skipNul = TRUE)
  colnames(df) <- c("timestamp", "variable", "value")
  df$value <- as.character(df$value)/1000.0
  df$variable <- as.character(df$variable)
  
  #Separate the power consumption variables only - mW
  df_sub <- df[df$variable=="LED1_P" | df$variable=="LED2_P" | df$variable=="LED3_P",]
  df_sub$value <- as.numeric(df_sub$value)/1000.0
  df_sub$variable <- as.factor(df_sub$variable)
  
  #Add time - we need to extract hours to get hourly means
  df_sub$time <- as.POSIXct(df_sub$timestamp, tz="GMT", origin="1970-01-01")
  df_sub$timeUse <- format(df_sub$time, format='%H')
  
  #Extract hourly means for each variable - led1_p, led2_p, led3_p
  df2 <- df_sub %>%
    group_by(variable, timeUse) %>%
    summarise(value = mean(value))
  df2$variable <- as.character(df2$variable)
  
  #Add date from the file name
  df2$date <- as.Date((rep(gsub("\\..*","",file_list[i]), length(df2$value))),
                      format = "%d-%m-%Y")
  
  #Create a data frame with the above values and bind to a complete df 
  df <- data.frame(date=df2$date, hour=df2$timeUse, 
                   variable=df2$variable, value=df2$value)
  df$hour <- as.character(df$hour)
  cpe9 <- rbind(cpe9, df)
}
cpe9$id <- rep("Nur2B_CPE9", length(cpe9$value))

cpe10 <- data.frame()
setwd("~/OneDrive - Coventry University/HEED_analysis/Micro-grid/Data/08_2019/Nur2B/CPE10/")
file_list <- list.files()
for(i in 1:length(file_list)) {
  df <- read.csv(file_list[i], header = FALSE, quote = "\"", skipNul = TRUE)
  colnames(df) <- c("timestamp", "variable", "value")
  df$value <- as.character(df$value)
  df$variable <- as.character(df$variable)
  
  #Separate the power consumption variables only - mW
  df_sub <- df[df$variable=="LED1_P" | df$variable=="LED2_P" | df$variable=="LED3_P",]
  df_sub$value <- as.numeric(df_sub$value)/1000.0
  df_sub$variable <- as.factor(df_sub$variable)
  
  #Add time - we need to extract hours to get hourly means
  df_sub$time <- as.POSIXct(df_sub$timestamp, tz="GMT", origin="1970-01-01")
  df_sub$timeUse <- format(df_sub$time, format='%H')
  
  #Extract hourly means for each variable - led1_p, led2_p, led3_p
  df2 <- df_sub %>%
    group_by(variable, timeUse) %>%
    summarise(value = mean(value))
  df2$variable <- as.character(df2$variable)
  
  #Add date from the file name
  df2$date <- as.Date((rep(gsub("\\..*","",file_list[i]), length(df2$value))),
                      format = "%d-%m-%Y")
  
  #Create a data frame with the above values and bind to a complete df 
  df <- data.frame(date=df2$date, hour=df2$timeUse, 
                   variable=df2$variable, value=df2$value)
  df$hour <- as.character(df$hour)
  cpe10 <- rbind(cpe10, df)
}
cpe10$id <- rep("Nur2B_CPE10", length(cpe10$value))

cpe11 <- data.frame()
setwd("~/OneDrive - Coventry University/HEED_analysis/Micro-grid/Data/08_2019/Nur2C/CPE11/")
file_list <- list.files()
for(i in 1:length(file_list)) {
  df <- read.csv(file_list[i], header = FALSE, quote = "\"", skipNul = TRUE)
  colnames(df) <- c("timestamp", "variable", "value")
  df$value <- as.character(df$value)
  df$variable <- as.character(df$variable)
  
  #Separate the power consumption variables only - mW
  df_sub <- df[df$variable=="LED1_P" | df$variable=="LED2_P" | df$variable=="LED3_P",]
  df_sub$value <- as.numeric(df_sub$value)/1000.0
  df_sub$variable <- as.factor(df_sub$variable)
  
  #Add time - we need to extract hours to get hourly means
  df_sub$time <- as.POSIXct(df_sub$timestamp, tz="GMT", origin="1970-01-01")
  df_sub$timeUse <- format(df_sub$time, format='%H')
  
  #Extract hourly means for each variable - led1_p, led2_p, led3_p
  df2 <- df_sub %>%
    group_by(variable, timeUse) %>%
    summarise(value = mean(value))
  df2$variable <- as.character(df2$variable)
  
  #Add date from the file name
  df2$date <- as.Date((rep(gsub("\\..*","",file_list[i]), length(df2$value))),
                      format = "%d-%m-%Y")
  
  #Create a data frame with the above values and bind to a complete df 
  df <- data.frame(date=df2$date, hour=df2$timeUse, 
                   variable=df2$variable, value=df2$value)
  df$hour <- as.character(df$hour)
  cpe11 <- rbind(cpe11, df)
}
cpe11$id <- rep("Nur2C_CPE11", length(cpe11$value))

cpe12 <- data.frame()
setwd("~/OneDrive - Coventry University/HEED_analysis/Micro-grid/Data/08_2019/Nur2C/CPE12/")
file_list <- list.files()
for(i in 1:length(file_list)) {
  df <- read.csv(file_list[i], header = FALSE, quote = "\"", skipNul = TRUE)
  colnames(df) <- c("timestamp", "variable", "value")
  df$value <- as.character(df$value)
  df$variable <- as.character(df$variable)
  
  #Separate the power consumption variables only - mW
  df_sub <- df[df$variable=="LED1_P" | df$variable=="LED2_P" | df$variable=="LED3_P",]
  df_sub$value <- as.numeric(df_sub$value)/1000.0
  df_sub$variable <- as.factor(df_sub$variable)
  
  #Add time - we need to extract hours to get hourly means
  df_sub$time <- as.POSIXct(df_sub$timestamp, tz="GMT", origin="1970-01-01")
  df_sub$timeUse <- format(df_sub$time, format='%H')
  
  #Extract hourly means for each variable - led1_p, led2_p, led3_p
  df2 <- df_sub %>%
    group_by(variable, timeUse) %>%
    summarise(value = mean(value))
  df2$variable <- as.character(df2$variable)
  
  #Add date from the file name
  df2$date <- as.Date((rep(gsub("\\..*","",file_list[i]), length(df2$value))),
                      format = "%d-%m-%Y")
  
  #Create a data frame with the above values and bind to a complete df 
  df <- data.frame(date=df2$date, hour=df2$timeUse, 
                   variable=df2$variable, value=df2$value)
  df$hour <- as.character(df$hour)
  cpe12 <- rbind(cpe12, df)
}
cpe12$id <- rep("Nur2C_CPE12", length(cpe12$value))

#CPE 1 to 5 from PG 
cpe13 <- data.frame()
setwd("~/OneDrive - Coventry University/HEED_analysis/Micro-grid/Data/08_2019/PG/CPE1/")
file_list <- list.files()
for(i in 1:length(file_list)) {
  df <- read.csv(file_list[i], header = FALSE, quote = "\"", skipNul = TRUE)
  colnames(df) <- c("timestamp", "variable", "value")
  df$value <- as.character(df$value)
  df$variable <- as.character(df$variable)
  
  #Separate the power consumption variables only - mW
  df_sub <- df[df$variable=="LED1_P" | df$variable=="LED2_P" | df$variable=="LED3_P",]
  df_sub$value <- as.numeric(df_sub$value)/1000.0
  df_sub$variable <- as.factor(df_sub$variable)
  
  #Add time - we need to extract hours to get hourly means
  df_sub$time <- as.POSIXct(df_sub$timestamp, tz="GMT", origin="1970-01-01")
  df_sub$timeUse <- format(df_sub$time, format='%H')
  
  #Extract hourly means for each variable - led1_p, led2_p, led3_p
  df2 <- df_sub %>%
    group_by(variable, timeUse) %>%
    summarise(value = mean(value))
  df2$variable <- as.character(df2$variable)
  
  #Add date from the file name
  df2$date <- as.Date((rep(gsub("\\..*","",file_list[i]), length(df2$value))),
                      format = "%d-%m-%Y")
  
  #Create a data frame with the above values and bind to a complete df 
  df <- data.frame(date=df2$date, hour=df2$timeUse, 
                   variable=df2$variable, value=df2$value)
  df$hour <- as.character(df$hour)
  cpe13 <- rbind(cpe13, df)
}
cpe13$id <- rep("PG_CPE1", length(cpe13$value))

cpe14 <- data.frame()
setwd("~/OneDrive - Coventry University/HEED_analysis/Micro-grid/Data/08_2019/PG/CPE2/")
file_list <- list.files()
for(i in 1:length(file_list)) {
  df <- read.csv(file_list[i], header = FALSE, quote = "\"", skipNul = TRUE)
  colnames(df) <- c("timestamp", "variable", "value")
  df$value <- as.character(df$value)
  df$variable <- as.character(df$variable)
  
  #Separate the power consumption variables only - mW
  df_sub <- df[df$variable=="LED1_P" | df$variable=="LED2_P" | df$variable=="LED3_P",]
  df_sub$value <- as.numeric(df_sub$value)/1000.0
  df_sub$variable <- as.factor(df_sub$variable)
  
  #Add time - we need to extract hours to get hourly means
  df_sub$time <- as.POSIXct(df_sub$timestamp, tz="GMT", origin="1970-01-01")
  df_sub$timeUse <- format(df_sub$time, format='%H')
  
  #Extract hourly means for each variable - led1_p, led2_p, led3_p
  df2 <- df_sub %>%
    group_by(variable, timeUse) %>%
    summarise(value = mean(value))
  df2$variable <- as.character(df2$variable)
  
  #Add date from the file name
  df2$date <- as.Date((rep(gsub("\\..*","",file_list[i]), length(df2$value))),
                      format = "%d-%m-%Y")
  
  #Create a data frame with the above values and bind to a complete df 
  df <- data.frame(date=df2$date, hour=df2$timeUse, 
                   variable=df2$variable, value=df2$value)
  df$hour <- as.character(df$hour)
  cpe14 <- rbind(cpe14, df)
}
cpe14$id <- rep("PG_CPE2", length(cpe14$value))

cpe15 <- data.frame()
setwd("~/OneDrive - Coventry University/HEED_analysis/Micro-grid/Data/08_2019/PG/CPE3/")
file_list <- list.files()
for(i in 1:length(file_list)) {
  df <- read.csv(file_list[i], header = FALSE, quote = "\"", skipNul = TRUE)
  colnames(df) <- c("timestamp", "variable", "value")
  df$value <- as.character(df$value)
  df$variable <- as.character(df$variable)
  
  #Separate the power consumption variables only - mW
  df_sub <- df[df$variable=="LED1_P" | df$variable=="LED2_P" | df$variable=="LED3_P",]
  df_sub$value <- as.numeric(df_sub$value)/1000.0
  df_sub$variable <- as.factor(df_sub$variable)
  
  #Add time - we need to extract hours to get hourly means
  df_sub$time <- as.POSIXct(df_sub$timestamp, tz="GMT", origin="1970-01-01")
  df_sub$timeUse <- format(df_sub$time, format='%H')
  
  #Extract hourly means for each variable - led1_p, led2_p, led3_p
  df2 <- df_sub %>%
    group_by(variable, timeUse) %>%
    summarise(value = mean(value))
  df2$variable <- as.character(df2$variable)
  
  #Add date from the file name
  df2$date <- as.Date((rep(gsub("\\..*","",file_list[i]), length(df2$value))),
                      format = "%d-%m-%Y")
  
  #Create a data frame with the above values and bind to a complete df 
  df <- data.frame(date=df2$date, hour=df2$timeUse, 
                   variable=df2$variable, value=df2$value)
  df$hour <- as.character(df$hour)
  cpe15 <- rbind(cpe15, df)
}
cpe15$id <- rep("PG_CPE3", length(cpe15$value))

cpe16 <- data.frame()
setwd("~/OneDrive - Coventry University/HEED_analysis/Micro-grid/Data/08_2019/PG/CPE4/")
file_list <- list.files()
for(i in 1:length(file_list)) {
  df <- read.csv(file_list[i], header = FALSE, quote = "\"", skipNul = TRUE)
  colnames(df) <- c("timestamp", "variable", "value")
  df$value <- as.character(df$value)
  df$variable <- as.character(df$variable)
  
  #Separate the power consumption variables only - mW
  df_sub <- df[df$variable=="LED1_P" | df$variable=="LED2_P" | df$variable=="LED3_P",]
  df_sub$value <- as.numeric(df_sub$value)/1000.0
  df_sub$variable <- as.factor(df_sub$variable)
  
  #Add time - we need to extract hours to get hourly means
  df_sub$time <- as.POSIXct(df_sub$timestamp, tz="GMT", origin="1970-01-01")
  df_sub$timeUse <- format(df_sub$time, format='%H')
  
  #Extract hourly means for each variable - led1_p, led2_p, led3_p
  df2 <- df_sub %>%
    group_by(variable, timeUse) %>%
    summarise(value = mean(value))
  df2$variable <- as.character(df2$variable)
  
  #Add date from the file name
  df2$date <- as.Date((rep(gsub("\\..*","",file_list[i]), length(df2$value))),
                      format = "%d-%m-%Y")
  
  #Create a data frame with the above values and bind to a complete df 
  df <- data.frame(date=df2$date, hour=df2$timeUse, 
                   variable=df2$variable, value=df2$value)
  df$hour <- as.character(df$hour)
  cpe16 <- rbind(cpe16, df)
}
cpe16$id <- rep("PG_CPE4", length(cpe16$value))

cpe17 <- data.frame()
setwd("~/OneDrive - Coventry University/HEED_analysis/Micro-grid/Data/08_2019/PG/CPE5/")
file_list <- list.files()
for(i in 1:length(file_list)) {
  df <- read.csv(file_list[i], header = FALSE, quote = "\"", skipNul = TRUE)
  colnames(df) <- c("timestamp", "variable", "value")
  df$value <- as.character(df$value)
  df$variable <- as.character(df$variable)
  
  #Separate the power consumption variables only - mW
  df_sub <- df[df$variable=="LED1_P" | df$variable=="LED2_P" | df$variable=="LED3_P",]
  df_sub$value <- as.numeric(df_sub$value)/1000.0
  df_sub$variable <- as.factor(df_sub$variable)
  
  #Add time - we need to extract hours to get hourly means
  df_sub$time <- as.POSIXct(df_sub$timestamp, tz="GMT", origin="1970-01-01")
  df_sub$timeUse <- format(df_sub$time, format='%H')
  
  #Extract hourly means for each variable - led1_p, led2_p, led3_p
  df2 <- df_sub %>%
    group_by(variable, timeUse) %>%
    summarise(value = mean(value))
  df2$variable <- as.character(df2$variable)
  
  #Add date from the file name
  df2$date <- as.Date((rep(gsub("\\..*","",file_list[i]), length(df2$value))),
                      format = "%d-%m-%Y")
  
  #Create a data frame with the above values and bind to a complete df 
  df <- data.frame(date=df2$date, hour=df2$timeUse, 
                   variable=df2$variable, value=df2$value)
  df$hour <- as.character(df$hour)
  cpe17 <- rbind(cpe17, df)
}
cpe17$id <- rep("PG_CPE5", length(cpe17$value))

#SL 1,2 and 3 CPE
cpe18 <- data.frame()
setwd("~/OneDrive - Coventry University/HEED_analysis/Micro-grid/Data/08_2019/SL/SL1_CPE/")
file_list <- list.files()
for(i in 1:length(file_list)) {
  df <- read.csv(file_list[i], header = FALSE, quote = "\"", skipNul = TRUE)
  colnames(df) <- c("timestamp", "variable", "value")
  df$value <- as.character(df$value)
  df$variable <- as.character(df$variable)
  
  #Separate the power consumption variables only - mW
  df_sub <- df[df$variable=="LED1_P" | df$variable=="LED2_P" | df$variable=="LED3_P",]
  df_sub$value <- as.numeric(df_sub$value)/1000.0
  df_sub$variable <- as.factor(df_sub$variable)
  
  #Add time - we need to extract hours to get hourly means
  df_sub$time <- as.POSIXct(df_sub$timestamp, tz="GMT", origin="1970-01-01")
  df_sub$timeUse <- format(df_sub$time, format='%H')
  
  #Extract hourly means for each variable - led1_p, led2_p, led3_p
  df2 <- df_sub %>%
    group_by(variable, timeUse) %>%
    summarise(value = mean(value))
  df2$variable <- as.character(df2$variable)
  
  #Add date from the file name
  df2$date <- as.Date((rep(gsub("\\..*","",file_list[i]), length(df2$value))),
                      format = "%d-%m-%Y")
  
  #Create a data frame with the above values and bind to a complete df 
  df <- data.frame(date=df2$date, hour=df2$timeUse, 
                   variable=df2$variable, value=df2$value)
  df$hour <- as.character(df$hour)
  cpe18 <- rbind(cpe18, df)
}
cpe18$id <- rep("SL1_CPE", length(cpe18$value))

cpe19 <- data.frame()
setwd("~/OneDrive - Coventry University/HEED_analysis/Micro-grid/Data/08_2019/SL/SL2_CPE/")
file_list <- list.files()
for(i in 1:length(file_list)) {
  df <- read.csv(file_list[i], header = FALSE, quote = "\"", skipNul = TRUE)
  colnames(df) <- c("timestamp", "variable", "value")
  df$value <- as.character(df$value)
  df$variable <- as.character(df$variable)
  
  #Separate the power consumption variables only - mW
  df_sub <- df[df$variable=="LED1_P" | df$variable=="LED2_P" | df$variable=="LED3_P",]
  df_sub$value <- as.numeric(df_sub$value)/1000.0
  df_sub$variable <- as.factor(df_sub$variable)
  
  #Add time - we need to extract hours to get hourly means
  df_sub$time <- as.POSIXct(df_sub$timestamp, tz="GMT", origin="1970-01-01")
  df_sub$timeUse <- format(df_sub$time, format='%H')
  
  #Extract hourly means for each variable - led1_p, led2_p, led3_p
  df2 <- df_sub %>%
    group_by(variable, timeUse) %>%
    summarise(value = mean(value))
  df2$variable <- as.character(df2$variable)
  
  #Add date from the file name
  df2$date <- as.Date((rep(gsub("\\..*","",file_list[i]), length(df2$value))),
                      format = "%d-%m-%Y")
  
  #Create a data frame with the above values and bind to a complete df 
  df <- data.frame(date=df2$date, hour=df2$timeUse, 
                   variable=df2$variable, value=df2$value)
  df$hour <- as.character(df$hour)
  cpe19 <- rbind(cpe19, df)
}
cpe19$id <- rep("SL2_CPE", length(cpe19$value))

cpe20 <- data.frame()
setwd("~/OneDrive - Coventry University/HEED_analysis/Micro-grid/Data/08_2019/SL/SL3_CPE/")
file_list <- list.files()
for(i in 1:length(file_list)) {
  df <- read.csv(file_list[i], header = FALSE, quote = "\"", skipNul = TRUE)
  colnames(df) <- c("timestamp", "variable", "value")
  df$value <- as.character(df$value)
  df$variable <- as.character(df$variable)
  
  #Separate the power consumption variables only - mW
  df_sub <- df[df$variable=="LED1_P" | df$variable=="LED2_P" | df$variable=="LED3_P",]
  df_sub$value <- as.numeric(df_sub$value)/1000.0
  df_sub$variable <- as.factor(df_sub$variable)
  
  #Add time - we need to extract hours to get hourly means
  df_sub$time <- as.POSIXct(df_sub$timestamp, tz="GMT", origin="1970-01-01")
  df_sub$timeUse <- format(df_sub$time, format='%H')
  
  #Extract hourly means for each variable - led1_p, led2_p, led3_p
  df2 <- df_sub %>%
    group_by(variable, timeUse) %>%
    summarise(value = mean(value))
  df2$variable <- as.character(df2$variable)
  
  #Add date from the file name
  df2$date <- as.Date((rep(gsub("\\..*","",file_list[i]), length(df2$value))),
                      format = "%d-%m-%Y")
  
  #Create a data frame with the above values and bind to a complete df 
  df <- data.frame(date=df2$date, hour=df2$timeUse, 
                   variable=df2$variable, value=df2$value)
  df$hour <- as.character(df$hour)
  cpe20 <- rbind(cpe20, df)
}
cpe20$id <- rep("SL3_CPE", length(cpe20$value))

#Combining all CPEs
cpe_all <- data.frame()
cpe_all <- rbind(cpe_all, cpe1, cpe2, cpe3, cpe4, cpe5, cpe6, cpe7, cpe8, cpe9,
                 cpe10, cpe11, cpe12, cpe13, cpe14, cpe15, cpe16, cpe17, cpe18,
                 cpe19, cpe20)
cpe_all <- cpe_all[order(cpe_all$date),]

#Analysing all sockets to retrieve instantaneous power (vRELAY1_LVL) and 
#daily cumulative energy total (AC_Day_Energy_session)
#We have a total of 10 sockets

#For each socket we first need to concatenate the files for each day - 31 files for each
s1 <- data.frame()
setwd("~/OneDrive - Coventry University/HEED_analysis/Micro-grid/Data/08_2019/Nur1A/S1/")
file_list <- list.files()
for(i in 1:length(file_list)) {
  df <- read.csv(file_list[i], header = FALSE, quote = "\"", skipNul = TRUE)
  colnames(df) <- c("timestamp", "variable", "value")
  df$value <- as.character(df$value)
  df$variable <- as.character(df$variable)
  
  #Separate the power consumption (vRELAY1_LVL in W) and cumulative energy (AC_Day_Energy_session in Wattmin)
  df_sub <- df[df$variable=="vRELAY1_LVL" | df$variable=="AC_Day_Energy_session", ]
  df_sub$value <- as.numeric(df_sub$value)
  df_sub$variable <- as.factor(df_sub$variable)
  
  #Add time - we need to extract hours to get hourly means
  df_sub$time <- as.POSIXct(df_sub$timestamp, tz="GMT", origin="1970-01-01")
  df_sub$timeUse <- format(df_sub$time, format='%H')
  
  #Extract hourly means for instantaneous power
  df2 <- df_sub[df_sub$variable=="vRELAY1_LVL",] %>%
    group_by(timeUse) %>%
    summarise(value = mean(value))
  df2$variable <-rep("vRELAY1_LVL", length(df2$timeUse))
  
  #Correct cumulative total energy if not reset
  df3_sub <- df_sub[df_sub$variable=="AC_Day_Energy_session", ]
  if(df3_sub$value[1]!=0) { #If value has not reset
    #find if and where the value has reset
    flag=FALSE
    for(l in 2:length(df3_sub$timestamp)) {
      if(flag==FALSE & df3_sub$value[l] < df3_sub$value[l-1]) {
        flag=TRUE
        index <- l
      }
    }
    if(flag==FALSE) { #Means the value never reset to 0
      #Subtract value at 0th index from all values
      df3_sub$value2 <- df3_sub$value - df3_sub$value[1]
    } else {
      df3_sub$value2 <- df3_sub$value
      df3_sub$value2[1:(index-1)] <- (df3_sub$value[1:(index-1)] - df3_sub$value[1])
      df3_sub$value2[index:length(df3_sub$timestamp)] <- 
        df3_sub$value[index:length(df3_sub$timestamp)] + df3_sub$value2[index-1]
    }
  } else {
    df3_sub$value2 <- df3_sub$value
  }
  
  #Extract cumulative total energy per hour
  df3 <- df3_sub %>%
    group_by(timeUse) %>%
    summarise(value = value2[length(value2)])
  df3$variable <-rep("AC_Day_Energy_session", length(df3$timeUse))
  
  #Bind the 2 data frames and add date from the file name
  df2 <- rbind(df2, df3)
  df2$date <- as.Date((rep(gsub("\\..*","",file_list[i]), length(df2$value))),
                      format = "%d-%m-%Y")
  
  #Create a data frame with the above values and bind to a complete df 
  df <- data.frame(date=df2$date, hour=df2$timeUse, 
                   variable=df2$variable, value=df2$value)
  df$hour <- as.character(df$hour)
  s1 <- rbind(s1, df)
}
s1$id <- rep("Nur1A_S1", length(s1$value))

s2 <- data.frame()
setwd("~/OneDrive - Coventry University/HEED_analysis/Micro-grid/Data/08_2019/Nur1A/S2/")
file_list <- list.files()
for(i in 1:length(file_list)) {
  df <- read.csv(file_list[i], header = FALSE, quote = "\"", skipNul = TRUE)
  colnames(df) <- c("timestamp", "variable", "value")
  df$value <- as.character(df$value)
  df$variable <- as.character(df$variable)
  
  #Separate the power consumption (vRELAY1_LVL in W) and cumulative energy (AC_Day_Energy_session in Wattmin)
  df_sub <- df[df$variable=="vRELAY1_LVL" | df$variable=="AC_Day_Energy_session", ]
  df_sub$value <- as.numeric(df_sub$value)
  df_sub$variable <- as.factor(df_sub$variable)
  
  #Add time - we need to extract hours to get hourly means
  df_sub$time <- as.POSIXct(df_sub$timestamp, tz="GMT", origin="1970-01-01")
  df_sub$timeUse <- format(df_sub$time, format='%H')
  
  #Extract hourly means for instantaneous power
  df2 <- df_sub[df_sub$variable=="vRELAY1_LVL",] %>%
    group_by(timeUse) %>%
    summarise(value = mean(value))
  df2$variable <-rep("vRELAY1_LVL", length(df2$timeUse))
  
  #Correct cumulative total energy if not reset
  df3_sub <- df_sub[df_sub$variable=="AC_Day_Energy_session", ]
  if(df3_sub$value[1]!=0) { #If value has not reset
    #find if and where the value has reset
    flag=FALSE
    for(l in 2:length(df3_sub$timestamp)) {
      if(flag==FALSE & df3_sub$value[l] < df3_sub$value[l-1]) {
        flag=TRUE
        index <- l
      }
    }
    if(flag==FALSE) { #Means the value never reset to 0
      #Subtract value at 0th index from all values
      df3_sub$value2 <- df3_sub$value - df3_sub$value[1]
    } else {
      df3_sub$value2 <- df3_sub$value
      df3_sub$value2[1:(index-1)] <- (df3_sub$value[1:(index-1)] - df3_sub$value[1])
      df3_sub$value2[index:length(df3_sub$timestamp)] <- 
        df3_sub$value[index:length(df3_sub$timestamp)] + df3_sub$value2[index-1]
    }
  } else {
    df3_sub$value2 <- df3_sub$value
  }
  
  #Extract cumulative total energy per hour
  df3 <- df3_sub %>%
    group_by(timeUse) %>%
    summarise(value = value2[length(value2)])
  df3$variable <-rep("AC_Day_Energy_session", length(df3$timeUse))
  
  #Bind the 2 data frames and add date from the file name
  df2 <- rbind(df2, df3)
  df2$date <- as.Date((rep(gsub("\\..*","",file_list[i]), length(df2$value))),
                      format = "%d-%m-%Y")
  
  #Create a data frame with the above values and bind to a complete df 
  df <- data.frame(date=df2$date, hour=df2$timeUse, 
                   variable=df2$variable, value=df2$value)
  df$hour <- as.character(df$hour)
  s2 <- rbind(s2, df)
}
s2$id <- rep("Nur1A_S2", length(s2$value))

s3 <- data.frame()
setwd("~/OneDrive - Coventry University/HEED_analysis/Micro-grid/Data/08_2019/Nur1B/S1/")
file_list <- list.files()
for(i in 1:length(file_list)) {
  df <- read.csv(file_list[i], header = FALSE, quote = "\"", skipNul = TRUE)
  colnames(df) <- c("timestamp", "variable", "value")
  df$value <- as.character(df$value)
  df$variable <- as.character(df$variable)
  
  #Separate the power consumption (vRELAY1_LVL in W) and cumulative energy (AC_Day_Energy_session in Wattmin)
  df_sub <- df[df$variable=="vRELAY1_LVL" | df$variable=="AC_Day_Energy_session", ]
  df_sub$value <- as.numeric(df_sub$value)
  df_sub$variable <- as.factor(df_sub$variable)
  
  #Add time - we need to extract hours to get hourly means
  df_sub$time <- as.POSIXct(df_sub$timestamp, tz="GMT", origin="1970-01-01")
  df_sub$timeUse <- format(df_sub$time, format='%H')
  
  #Extract hourly means for instantaneous power
  df2 <- df_sub[df_sub$variable=="vRELAY1_LVL",] %>%
    group_by(timeUse) %>%
    summarise(value = mean(value))
  df2$variable <-rep("vRELAY1_LVL", length(df2$timeUse))
  
  #Correct cumulative total energy if not reset
  df3_sub <- df_sub[df_sub$variable=="AC_Day_Energy_session", ]
  if(df3_sub$value[1]!=0) { #If value has not reset
    #find if and where the value has reset
    flag=FALSE
    for(l in 2:length(df3_sub$timestamp)) {
      if(flag==FALSE & df3_sub$value[l] < df3_sub$value[l-1]) {
        flag=TRUE
        index <- l
      }
    }
    if(flag==FALSE) { #Means the value never reset to 0
      #Subtract value at 0th index from all values
      df3_sub$value2 <- df3_sub$value - df3_sub$value[1]
    } else {
      df3_sub$value2 <- df3_sub$value
      df3_sub$value2[1:(index-1)] <- (df3_sub$value[1:(index-1)] - df3_sub$value[1])
      df3_sub$value2[index:length(df3_sub$timestamp)] <- 
        df3_sub$value[index:length(df3_sub$timestamp)] + df3_sub$value2[index-1]
    }
  } else {
    df3_sub$value2 <- df3_sub$value
  }
  
  #Extract cumulative total energy per hour
  df3 <- df3_sub %>%
    group_by(timeUse) %>%
    summarise(value = value2[length(value2)])
  df3$variable <-rep("AC_Day_Energy_session", length(df3$timeUse))
  
  #Bind the 2 data frames and add date from the file name
  df2 <- rbind(df2, df3)
  df2$date <- as.Date((rep(gsub("\\..*","",file_list[i]), length(df2$value))),
                      format = "%d-%m-%Y")
  
  #Create a data frame with the above values and bind to a complete df 
  df <- data.frame(date=df2$date, hour=df2$timeUse, 
                   variable=df2$variable, value=df2$value)
  df$hour <- as.character(df$hour)
  s3 <- rbind(s3, df)
}
s3$id <- rep("Nur1B_S1", length(s3$value))

s4 <- data.frame()
setwd("~/OneDrive - Coventry University/HEED_analysis/Micro-grid/Data/08_2019/Nur1C/S1/")
file_list <- list.files()
for(i in 1:length(file_list)) {
  df <- read.csv(file_list[i], header = FALSE, quote = "\"", skipNul = TRUE)
  colnames(df) <- c("timestamp", "variable", "value")
  df$value <- as.character(df$value)
  df$variable <- as.character(df$variable)
  
  #Separate the power consumption (vRELAY1_LVL in W) and cumulative energy (AC_Day_Energy_session in Wattmin)
  df_sub <- df[df$variable=="vRELAY1_LVL" | df$variable=="AC_Day_Energy_session", ]
  df_sub$value <- as.numeric(df_sub$value)
  df_sub$variable <- as.factor(df_sub$variable)
  
  #Add time - we need to extract hours to get hourly means
  df_sub$time <- as.POSIXct(df_sub$timestamp, tz="GMT", origin="1970-01-01")
  df_sub$timeUse <- format(df_sub$time, format='%H')
  
  #Extract hourly means for instantaneous power
  df2 <- df_sub[df_sub$variable=="vRELAY1_LVL",] %>%
    group_by(timeUse) %>%
    summarise(value = mean(value))
  df2$variable <-rep("vRELAY1_LVL", length(df2$timeUse))
  
  #Correct cumulative total energy if not reset
  df3_sub <- df_sub[df_sub$variable=="AC_Day_Energy_session", ]
  if(df3_sub$value[1]!=0) { #If value has not reset
    #find if and where the value has reset
    flag=FALSE
    for(l in 2:length(df3_sub$timestamp)) {
      if(flag==FALSE & df3_sub$value[l] < df3_sub$value[l-1]) {
        flag=TRUE
        index <- l
      }
    }
    if(flag==FALSE) { #Means the value never reset to 0
      #Subtract value at 0th index from all values
      df3_sub$value2 <- df3_sub$value - df3_sub$value[1]
    } else {
      df3_sub$value2 <- df3_sub$value
      df3_sub$value2[1:(index-1)] <- (df3_sub$value[1:(index-1)] - df3_sub$value[1])
      df3_sub$value2[index:length(df3_sub$timestamp)] <- 
        df3_sub$value[index:length(df3_sub$timestamp)] + df3_sub$value2[index-1]
    }
  } else {
    df3_sub$value2 <- df3_sub$value
  }
  
  #Extract cumulative total energy per hour
  df3 <- df3_sub %>%
    group_by(timeUse) %>%
    summarise(value = value2[length(value2)])
  df3$variable <-rep("AC_Day_Energy_session", length(df3$timeUse))
  
  #Bind the 2 data frames and add date from the file name
  df2 <- rbind(df2, df3)
  df2$date <- as.Date((rep(gsub("\\..*","",file_list[i]), length(df2$value))),
                      format = "%d-%m-%Y")
  
  #Create a data frame with the above values and bind to a complete df 
  df <- data.frame(date=df2$date, hour=df2$timeUse, 
                   variable=df2$variable, value=df2$value)
  df$hour <- as.character(df$hour)
  s4 <- rbind(s4, df)
}
s4$id <- rep("Nur1C_S1", length(s4$value))

s5 <- data.frame()
setwd("~/OneDrive - Coventry University/HEED_analysis/Micro-grid/Data/08_2019/Nur2A/S1/")
file_list <- list.files()
for(i in 1:length(file_list)) {
  df <- read.csv(file_list[i], header = FALSE, quote = "\"", skipNul = TRUE)
  colnames(df) <- c("timestamp", "variable", "value")
  df$value <- as.character(df$value)
  df$variable <- as.character(df$variable)
  
  #Separate the power consumption (vRELAY1_LVL in W) and cumulative energy (AC_Day_Energy_session in Wattmin)
  df_sub <- df[df$variable=="vRELAY1_LVL" | df$variable=="AC_Day_Energy_session", ]
  df_sub$value <- as.numeric(df_sub$value)
  df_sub$variable <- as.factor(df_sub$variable)
  
  #Add time - we need to extract hours to get hourly means
  df_sub$time <- as.POSIXct(df_sub$timestamp, tz="GMT", origin="1970-01-01")
  df_sub$timeUse <- format(df_sub$time, format='%H')
  
  #Extract hourly means for instantaneous power
  df2 <- df_sub[df_sub$variable=="vRELAY1_LVL",] %>%
    group_by(timeUse) %>%
    summarise(value = mean(value))
  df2$variable <-rep("vRELAY1_LVL", length(df2$timeUse))
  
  #Correct cumulative total energy if not reset
  df3_sub <- df_sub[df_sub$variable=="AC_Day_Energy_session", ]
  if(df3_sub$value[1]!=0) { #If value has not reset
    #find if and where the value has reset
    flag=FALSE
    for(l in 2:length(df3_sub$timestamp)) {
      if(flag==FALSE & df3_sub$value[l] < df3_sub$value[l-1]) {
        flag=TRUE
        index <- l
      }
    }
    if(flag==FALSE) { #Means the value never reset to 0
      #Subtract value at 0th index from all values
      df3_sub$value2 <- df3_sub$value - df3_sub$value[1]
    } else {
      df3_sub$value2 <- df3_sub$value
      df3_sub$value2[1:(index-1)] <- (df3_sub$value[1:(index-1)] - df3_sub$value[1])
      df3_sub$value2[index:length(df3_sub$timestamp)] <- 
        df3_sub$value[index:length(df3_sub$timestamp)] + df3_sub$value2[index-1]
    }
  } else {
    df3_sub$value2 <- df3_sub$value
  }
  
  #Extract cumulative total energy per hour
  df3 <- df3_sub %>%
    group_by(timeUse) %>%
    summarise(value = value2[length(value2)])
  df3$variable <-rep("AC_Day_Energy_session", length(df3$timeUse))
  
  #Bind the 2 data frames and add date from the file name
  df2 <- rbind(df2, df3)
  df2$date <- as.Date((rep(gsub("\\..*","",file_list[i]), length(df2$value))),
                      format = "%d-%m-%Y")
  
  #Create a data frame with the above values and bind to a complete df 
  df <- data.frame(date=df2$date, hour=df2$timeUse, 
                   variable=df2$variable, value=df2$value)
  df$hour <- as.character(df$hour)
  s5 <- rbind(s5, df)
}
s5$id <- rep("Nur2A_S1", length(s5$value))

s6 <- data.frame()
setwd("~/OneDrive - Coventry University/HEED_analysis/Micro-grid/Data/08_2019/Nur2A/S2/")
file_list <- list.files()
for(i in 1:length(file_list)) {
  df <- read.csv(file_list[i], header = FALSE, quote = "\"", skipNul = TRUE)
  colnames(df) <- c("timestamp", "variable", "value")
  df$value <- as.character(df$value)
  df$variable <- as.character(df$variable)
  
  #Separate the power consumption (vRELAY1_LVL in W) and cumulative energy (AC_Day_Energy_session in Wattmin)
  df_sub <- df[df$variable=="vRELAY1_LVL" | df$variable=="AC_Day_Energy_session", ]
  df_sub$value <- as.numeric(df_sub$value)
  df_sub$variable <- as.factor(df_sub$variable)
  
  #Add time - we need to extract hours to get hourly means
  df_sub$time <- as.POSIXct(df_sub$timestamp, tz="GMT", origin="1970-01-01")
  df_sub$timeUse <- format(df_sub$time, format='%H')
  
  #Extract hourly means for instantaneous power
  df2 <- df_sub[df_sub$variable=="vRELAY1_LVL",] %>%
    group_by(timeUse) %>%
    summarise(value = mean(value))
  df2$variable <-rep("vRELAY1_LVL", length(df2$timeUse))
  
  #Correct cumulative total energy if not reset
  df3_sub <- df_sub[df_sub$variable=="AC_Day_Energy_session", ]
  if(df3_sub$value[1]!=0) { #If value has not reset
    #find if and where the value has reset
    flag=FALSE
    for(l in 2:length(df3_sub$timestamp)) {
      if(flag==FALSE & df3_sub$value[l] < df3_sub$value[l-1]) {
        flag=TRUE
        index <- l
      }
    }
    if(flag==FALSE) { #Means the value never reset to 0
      #Subtract value at 0th index from all values
      df3_sub$value2 <- df3_sub$value - df3_sub$value[1]
    } else {
      df3_sub$value2 <- df3_sub$value
      df3_sub$value2[1:(index-1)] <- (df3_sub$value[1:(index-1)] - df3_sub$value[1])
      df3_sub$value2[index:length(df3_sub$timestamp)] <- 
        df3_sub$value[index:length(df3_sub$timestamp)] + df3_sub$value2[index-1]
    }
  } else {
    df3_sub$value2 <- df3_sub$value
  }
  
  #Extract cumulative total energy per hour
  df3 <- df3_sub %>%
    group_by(timeUse) %>%
    summarise(value = value2[length(value2)])
  df3$variable <-rep("AC_Day_Energy_session", length(df3$timeUse))
  
  #Bind the 2 data frames and add date from the file name
  df2 <- rbind(df2, df3)
  df2$date <- as.Date((rep(gsub("\\..*","",file_list[i]), length(df2$value))),
                      format = "%d-%m-%Y")
  
  #Create a data frame with the above values and bind to a complete df 
  df <- data.frame(date=df2$date, hour=df2$timeUse, 
                   variable=df2$variable, value=df2$value)
  df$hour <- as.character(df$hour)
  s6 <- rbind(s6, df)
}
s6$id <- rep("Nur2A_S2", length(s6$value))

s7 <- data.frame()
setwd("~/OneDrive - Coventry University/HEED_analysis/Micro-grid/Data/08_2019/Nur2B/S1/")
file_list <- list.files()
for(i in 1:length(file_list)) {
  df <- read.csv(file_list[i], header = FALSE, quote = "\"", skipNul = TRUE)
  colnames(df) <- c("timestamp", "variable", "value")
  df$value <- as.character(df$value)
  df$variable <- as.character(df$variable)
  
  #Separate the power consumption (vRELAY1_LVL in W) and cumulative energy (AC_Day_Energy_session in Wattmin)
  df_sub <- df[df$variable=="vRELAY1_LVL" | df$variable=="AC_Day_Energy_session", ]
  df_sub$value <- as.numeric(df_sub$value)
  df_sub$variable <- as.factor(df_sub$variable)
  
  #Add time - we need to extract hours to get hourly means
  df_sub$time <- as.POSIXct(df_sub$timestamp, tz="GMT", origin="1970-01-01")
  df_sub$timeUse <- format(df_sub$time, format='%H')
  
  #Extract hourly means for instantaneous power
  df2 <- df_sub[df_sub$variable=="vRELAY1_LVL",] %>%
    group_by(timeUse) %>%
    summarise(value = mean(value))
  df2$variable <-rep("vRELAY1_LVL", length(df2$timeUse))
  
  #Correct cumulative total energy if not reset
  df3_sub <- df_sub[df_sub$variable=="AC_Day_Energy_session", ]
  if(df3_sub$value[1]!=0) { #If value has not reset
    #find if and where the value has reset
    flag=FALSE
    for(l in 2:length(df3_sub$timestamp)) {
      if(flag==FALSE & df3_sub$value[l] < df3_sub$value[l-1]) {
        flag=TRUE
        index <- l
      }
    }
    if(flag==FALSE) { #Means the value never reset to 0
      #Subtract value at 0th index from all values
      df3_sub$value2 <- df3_sub$value - df3_sub$value[1]
    } else {
      df3_sub$value2 <- df3_sub$value
      df3_sub$value2[1:(index-1)] <- (df3_sub$value[1:(index-1)] - df3_sub$value[1])
      df3_sub$value2[index:length(df3_sub$timestamp)] <- 
        df3_sub$value[index:length(df3_sub$timestamp)] + df3_sub$value2[index-1]
    }
  } else {
    df3_sub$value2 <- df3_sub$value
  }
  
  #Extract cumulative total energy per hour
  df3 <- df3_sub %>%
    group_by(timeUse) %>%
    summarise(value = value2[length(value2)])
  df3$variable <-rep("AC_Day_Energy_session", length(df3$timeUse))
  
  #Bind the 2 data frames and add date from the file name
  df2 <- rbind(df2, df3)
  df2$date <- as.Date((rep(gsub("\\..*","",file_list[i]), length(df2$value))),
                      format = "%d-%m-%Y")
  
  #Create a data frame with the above values and bind to a complete df 
  df <- data.frame(date=df2$date, hour=df2$timeUse, 
                   variable=df2$variable, value=df2$value)
  df$hour <- as.character(df$hour)
  s7 <- rbind(s7, df)
}
s7$id <- rep("Nur2B_S1", length(s7$value))

s8 <- data.frame()
setwd("~/OneDrive - Coventry University/HEED_analysis/Micro-grid/Data/08_2019/Nur2C/S1/")
file_list <- list.files()
for(i in 1:length(file_list)) {
  df <- read.csv(file_list[i], header = FALSE, quote = "\"", skipNul = TRUE)
  colnames(df) <- c("timestamp", "variable", "value")
  df$value <- as.character(df$value)
  df$variable <- as.character(df$variable)
  
  #Separate the power consumption (vRELAY1_LVL in W) and cumulative energy (AC_Day_Energy_session in Wattmin)
  df_sub <- df[df$variable=="vRELAY1_LVL" | df$variable=="AC_Day_Energy_session", ]
  df_sub$value <- as.numeric(df_sub$value)
  df_sub$variable <- as.factor(df_sub$variable)
  
  #Add time - we need to extract hours to get hourly means
  df_sub$time <- as.POSIXct(df_sub$timestamp, tz="GMT", origin="1970-01-01")
  df_sub$timeUse <- format(df_sub$time, format='%H')
  
  #Extract hourly means for instantaneous power
  df2 <- df_sub[df_sub$variable=="vRELAY1_LVL",] %>%
    group_by(timeUse) %>%
    summarise(value = mean(value))
  df2$variable <-rep("vRELAY1_LVL", length(df2$timeUse))
  
  #Correct cumulative total energy if not reset
  df3_sub <- df_sub[df_sub$variable=="AC_Day_Energy_session", ]
  if(df3_sub$value[1]!=0) { #If value has not reset
    #find if and where the value has reset
    flag=FALSE
    for(l in 2:length(df3_sub$timestamp)) {
      if(flag==FALSE & df3_sub$value[l] < df3_sub$value[l-1]) {
        flag=TRUE
        index <- l
      }
    }
    if(flag==FALSE) { #Means the value never reset to 0
      #Subtract value at 0th index from all values
      df3_sub$value2 <- df3_sub$value - df3_sub$value[1]
    } else {
      df3_sub$value2 <- df3_sub$value
      df3_sub$value2[1:(index-1)] <- (df3_sub$value[1:(index-1)] - df3_sub$value[1])
      df3_sub$value2[index:length(df3_sub$timestamp)] <- 
        df3_sub$value[index:length(df3_sub$timestamp)] + df3_sub$value2[index-1]
    }
  } else {
    df3_sub$value2 <- df3_sub$value
  }
  
  #Extract cumulative total energy per hour
  df3 <- df3_sub %>%
    group_by(timeUse) %>%
    summarise(value = value2[length(value2)])
  df3$variable <-rep("AC_Day_Energy_session", length(df3$timeUse))
  
  #Bind the 2 data frames and add date from the file name
  df2 <- rbind(df2, df3)
  df2$date <- as.Date((rep(gsub("\\..*","",file_list[i]), length(df2$value))),
                      format = "%d-%m-%Y")
  
  #Create a data frame with the above values and bind to a complete df 
  df <- data.frame(date=df2$date, hour=df2$timeUse, 
                   variable=df2$variable, value=df2$value)
  df$hour <- as.character(df$hour)
  s8 <- rbind(s8, df)
}
s8$id <- rep("Nur2C_S1", length(s8$value))

s9 <- data.frame()
setwd("~/OneDrive - Coventry University/HEED_analysis/Micro-grid/Data/08_2019/PG/S1/")
file_list <- list.files()
for(i in 1:length(file_list)) {
  df <- read.csv(file_list[i], header = FALSE, quote = "\"", skipNul = TRUE)
  colnames(df) <- c("timestamp", "variable", "value")
  df$value <- as.character(df$value)
  df$variable <- as.character(df$variable)
  
  #Separate the power consumption (vRELAY1_LVL in W) and cumulative energy (AC_Day_Energy_session in Wattmin)
  df_sub <- df[df$variable=="vRELAY1_LVL" | df$variable=="AC_Day_Energy_session", ]
  df_sub$value <- as.numeric(df_sub$value)
  df_sub$variable <- as.factor(df_sub$variable)
  
  #Add time - we need to extract hours to get hourly means
  df_sub$time <- as.POSIXct(df_sub$timestamp, tz="GMT", origin="1970-01-01")
  df_sub$timeUse <- format(df_sub$time, format='%H')
  
  #Extract hourly means for instantaneous power
  df2 <- df_sub[df_sub$variable=="vRELAY1_LVL",] %>%
    group_by(timeUse) %>%
    summarise(value = mean(value))
  df2$variable <-rep("vRELAY1_LVL", length(df2$timeUse))
  
  #Correct cumulative total energy if not reset
  df3_sub <- df_sub[df_sub$variable=="AC_Day_Energy_session", ]
  if(df3_sub$value[1]!=0) { #If value has not reset
    #find if and where the value has reset
    flag=FALSE
    for(l in 2:length(df3_sub$timestamp)) {
      if(flag==FALSE & df3_sub$value[l] < df3_sub$value[l-1]) {
        flag=TRUE
        index <- l
      }
    }
    if(flag==FALSE) { #Means the value never reset to 0
      #Subtract value at 0th index from all values
      df3_sub$value2 <- df3_sub$value - df3_sub$value[1]
    } else {
      df3_sub$value2 <- df3_sub$value
      df3_sub$value2[1:(index-1)] <- (df3_sub$value[1:(index-1)] - df3_sub$value[1])
      df3_sub$value2[index:length(df3_sub$timestamp)] <- 
        df3_sub$value[index:length(df3_sub$timestamp)] + df3_sub$value2[index-1]
    }
  } else {
    df3_sub$value2 <- df3_sub$value
  }
  
  #Extract cumulative total energy per hour
  df3 <- df3_sub %>%
    group_by(timeUse) %>%
    summarise(value = value2[length(value2)])
  df3$variable <-rep("AC_Day_Energy_session", length(df3$timeUse))
  
  #Bind the 2 data frames and add date from the file name
  df2 <- rbind(df2, df3)
  df2$date <- as.Date((rep(gsub("\\..*","",file_list[i]), length(df2$value))),
                      format = "%d-%m-%Y")
  
  #Create a data frame with the above values and bind to a complete df 
  df <- data.frame(date=df2$date, hour=df2$timeUse, 
                   variable=df2$variable, value=df2$value)
  df$hour <- as.character(df$hour)
  s9 <- rbind(s9, df)
}
s9$id <- rep("PG_S1", length(s9$value))

s10 <- data.frame()
setwd("~/OneDrive - Coventry University/HEED_analysis/Micro-grid/Data/08_2019/PG/S2/")
file_list <- list.files()
for(i in 1:length(file_list)) {
  df <- read.csv(file_list[i], header = FALSE, quote = "\"", skipNul = TRUE)
  colnames(df) <- c("timestamp", "variable", "value")
  df$value <- as.character(df$value)
  df$variable <- as.character(df$variable)
  
  #Separate the power consumption (vRELAY1_LVL in W) and cumulative energy (AC_Day_Energy_session in Wattmin)
  df_sub <- df[df$variable=="vRELAY1_LVL" | df$variable=="AC_Day_Energy_session", ]
  df_sub$value <- as.numeric(df_sub$value)
  df_sub$variable <- as.factor(df_sub$variable)
  
  #Add time - we need to extract hours to get hourly means
  df_sub$time <- as.POSIXct(df_sub$timestamp, tz="GMT", origin="1970-01-01")
  df_sub$timeUse <- format(df_sub$time, format='%H')
  
  #Extract hourly means for instantaneous power
  df2 <- df_sub[df_sub$variable=="vRELAY1_LVL",] %>%
    group_by(timeUse) %>%
    summarise(value = mean(value))
  df2$variable <-rep("vRELAY1_LVL", length(df2$timeUse))
  
  #Correct cumulative total energy if not reset
  df3_sub <- df_sub[df_sub$variable=="AC_Day_Energy_session", ]
  if(df3_sub$value[1]!=0) { #If value has not reset
    #find if and where the value has reset
    flag=FALSE
    for(l in 2:length(df3_sub$timestamp)) {
      if(flag==FALSE & df3_sub$value[l] < df3_sub$value[l-1]) {
        flag=TRUE
        index <- l
      }
    }
    if(flag==FALSE) { #Means the value never reset to 0
      #Subtract value at 0th index from all values
      df3_sub$value2 <- df3_sub$value - df3_sub$value[1]
    } else {
      df3_sub$value2 <- df3_sub$value
      df3_sub$value2[1:(index-1)] <- (df3_sub$value[1:(index-1)] - df3_sub$value[1])
      df3_sub$value2[index:length(df3_sub$timestamp)] <- 
        df3_sub$value[index:length(df3_sub$timestamp)] + df3_sub$value2[index-1]
    }
  } else {
    df3_sub$value2 <- df3_sub$value
  }
  
  #Extract cumulative total energy per hour
  df3 <- df3_sub %>%
    group_by(timeUse) %>%
    summarise(value = value2[length(value2)])
  df3$variable <-rep("AC_Day_Energy_session", length(df3$timeUse))
  
  #Bind the 2 data frames and add date from the file name
  df2 <- rbind(df2, df3)
  df2$date <- as.Date((rep(gsub("\\..*","",file_list[i]), length(df2$value))),
                      format = "%d-%m-%Y")
  
  #Create a data frame with the above values and bind to a complete df 
  df <- data.frame(date=df2$date, hour=df2$timeUse, 
                   variable=df2$variable, value=df2$value)
  df$hour <- as.character(df$hour)
  s10 <- rbind(s10, df)
}
s10$id <- rep("PG_S2", length(s10$value))

#Combining all 10 sockets
s_all <- data.frame()
s_all <- rbind(s_all, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10)
s_all <- s_all[order(s_all$date),]

#Plots for RQ1 - power consumption (W)
#1. Total energy consumption for all lights and sockets a)weekday b)weekend day
#Combine all power consumption data
pc <- data.frame()
pc <- rbind(pc, cpe_all, s_all[s_all$variable=="vRELAY1_LVL",])
#Adding energy consumption per hour per day
pc_sum <- pc %>%
  group_by(date, hour) %>%
  summarise(value=sum(value))

#Add days of week to separate weekends from weekdays
pc_sum$wday <- weekdays(as.Date(pc_sum$date), abbreviate = TRUE)
pc_sum_wday <- pc_sum[pc_sum$wday=="Mon" | pc_sum$wday=="Tue" | pc_sum$wday=="Wed" | 
                pc_sum$wday=="Thu" | pc_sum$wday=="Fri", ]
pc_sum_weday <- pc_sum[pc_sum$wday=="Sat" | pc_sum$wday=="Sun" , ]
pc_sum_wday$time <- as.numeric(pc_sum_wday$hour)
pc_sum_weday$time <- as.numeric(pc_sum_weday$hour)

#Plotting total energy consumption per hour for a weekday
pc_sum_wday$time <- as.factor(pc_sum_wday$time)
pc_sum_wday %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(color=time))  +
  labs(title="Total power consumption of MG on a weekday in Aug 2019" , 
       y="Power consumption (W)",
       x = "Hour of day")

#Plotting total energy consumption per hour for a weekend day
pc_sum_weday$time <- as.factor(pc_sum_weday$time)
pc_sum_weday %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(color=time))  +
  labs(title="Total power consumption of MG on a weekend day in Aug 2019" , 
       y="Power consumption (W)",
       x = "Hour of day")

#2. Hourly energy consumption at Nursery 1 for a weekend/week day in (month)
#cpe1-6 ; s1-4  
pc_nur1 <- data.frame()
pc_nur1 <- rbind(pc_nur1, cpe1, cpe2, cpe3, cpe4, cpe5, cpe6, 
                 s1[s1$variable=="vRELAY1_LVL",],
                 s2[s2$variable=="vRELAY1_LVL",],
                 s3[s3$variable=="vRELAY1_LVL",],
                 s4[s4$variable=="vRELAY1_LVL",])

#Adding energy consumption per hour per day
pc_sum <- pc_nur1 %>%
  group_by(date, hour) %>%
  summarise(value=sum(value))

#Add days of week to separate weekends from weekdays
pc_sum$wday <- weekdays(as.Date(pc_sum$date), abbreviate = TRUE)
pc_sum_wday <- pc_sum[pc_sum$wday=="Mon" | pc_sum$wday=="Tue" | pc_sum$wday=="Wed" | 
                        pc_sum$wday=="Thu" | pc_sum$wday=="Fri", ]
pc_sum_weday <- pc_sum[pc_sum$wday=="Sat" | pc_sum$wday=="Sun" , ]
pc_sum_wday$time <- as.numeric(pc_sum_wday$hour)
pc_sum_weday$time <- as.numeric(pc_sum_weday$hour)

#Plotting total energy consumption per hour for a weekday
pc_sum_wday$time <- as.factor(pc_sum_wday$time)
pc_sum_wday %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(color=time))  +
  labs(title="Power consumption of Nursery 1 on a weekday in Aug 2019" , 
       y="Power consumption (W)",
       x = "Hour of day")

#Plotting total energy consumption per hour for a weekend day
pc_sum_weday$time <- as.factor(pc_sum_weday$time)
pc_sum_weday %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(color=time))  +
  labs(title="Power consumption of Nursery 1 on a weekend day in Aug 2019" , 
       y="Power consumption (W)",
       x = "Hour of day")

#3. Hourly energy consumption at Nursery 2 for a weekend/week day in (month)
#cpe7-12; s5-8
pc_nur2 <- data.frame()
pc_nur2 <- rbind(pc_nur2, cpe7, cpe8, cpe9, cpe10, cpe11, cpe12,
                 s5[s5$variable=="vRELAY1_LVL",],
                 s6[s6$variable=="vRELAY1_LVL",],
                 s7[s7$variable=="vRELAY1_LVL",],
                 s8[s8$variable=="vRELAY1_LVL",])
#Adding energy consumption per hour per day
pc_sum <- pc_nur2 %>%
  group_by(date, hour) %>%
  summarise(value=sum(value))

#Add days of week to separate weekends from weekdays
pc_sum$wday <- weekdays(as.Date(pc_sum$date), abbreviate = TRUE)
pc_sum_wday <- pc_sum[pc_sum$wday=="Mon" | pc_sum$wday=="Tue" | pc_sum$wday=="Wed" | 
                        pc_sum$wday=="Thu" | pc_sum$wday=="Fri", ]
pc_sum_weday <- pc_sum[pc_sum$wday=="Sat" | pc_sum$wday=="Sun" , ]
pc_sum_wday$time <- as.numeric(pc_sum_wday$hour)
pc_sum_weday$time <- as.numeric(pc_sum_weday$hour)

#Plotting total energy consumption per hour for a weekday
pc_sum_wday$time <- as.factor(pc_sum_wday$time)
pc_sum_wday %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(color=time))  +
  labs(title="Power consumption of Nursery 2 on a weekday in Aug 2019" , 
       y="Power consumption (W)",
       x = "Hour of day")

#Plotting total energy consumption per hour for a weekend day
pc_sum_weday$time <- as.factor(pc_sum_weday$time)
pc_sum_weday %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(color=time))  +
  labs(title="Power consumption of Nursery 2 on a weekend day in Aug 2019" , 
       y="Power consumption (W)",
       x = "Hour of day")

#4. Hourly energy consumption at the Playground for a weekend/week day in (month)
#cpe13-17; s9-10
pc_pg <- data.frame()
pc_pg <- rbind(pc_pg, cpe13, cpe14, cpe15, cpe16, cpe17, 
               s9[s9$variable=="vRELAY1_LVL",],
               s10[s10$variable=="vRELAY1_LVL",])
#Adding energy consumption per hour per day
pc_sum <- pc_pg %>%
  group_by(date, hour) %>%
  summarise(value=sum(value))

#Add days of week to separate weekends from weekdays
pc_sum$wday <- weekdays(as.Date(pc_sum$date), abbreviate = TRUE)
pc_sum_wday <- pc_sum[pc_sum$wday=="Mon" | pc_sum$wday=="Tue" | pc_sum$wday=="Wed" | 
                        pc_sum$wday=="Thu" | pc_sum$wday=="Fri", ]
pc_sum_weday <- pc_sum[pc_sum$wday=="Sat" | pc_sum$wday=="Sun" , ]
pc_sum_wday$time <- as.numeric(pc_sum_wday$hour)
pc_sum_weday$time <- as.numeric(pc_sum_weday$hour)

#Plotting total energy consumption per hour for a weekday
pc_sum_wday$time <- as.factor(pc_sum_wday$time)
pc_sum_wday %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(color=time))  +
  labs(title="Power consumption of Playground on a weekday in Aug 2019" , 
       y="Power consumption (W)",
       x = "Hour of day")

#Plotting total energy consumption per hour for a weekend day
pc_sum_weday$time <- as.factor(pc_sum_weday$time)
pc_sum_weday %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(color=time))  +
  labs(title="Power consumption of Playground on a weekend day in Aug 2019" , 
       y="Power consumption (W)",
       x = "Hour of day")

#5. Hourly socket consumption at Nursery 1 for a weekend/week day in (month)
#s1, s2, s3, s4 
pc_nur1_s <- data.frame()
pc_nur1_s <- rbind(pc_nur1_s, s1[s1$variable=="vRELAY1_LVL",],
                   s2[s2$variable=="vRELAY1_LVL",],
                   s3[s3$variable=="vRELAY1_LVL",],
                   s4[s4$variable=="vRELAY1_LVL",])

#Adding energy consumption per hour per day
pc_sum <- pc_nur1_s %>%
  group_by(date, hour) %>%
  summarise(value=sum(value))

#Add days of week to separate weekends from weekdays
pc_sum$wday <- weekdays(as.Date(pc_sum$date), abbreviate = TRUE)
pc_sum_wday <- pc_sum[pc_sum$wday=="Mon" | pc_sum$wday=="Tue" | pc_sum$wday=="Wed" | 
                        pc_sum$wday=="Thu" | pc_sum$wday=="Fri", ]
pc_sum_weday <- pc_sum[pc_sum$wday=="Sat" | pc_sum$wday=="Sun" , ]
pc_sum_wday$time <- as.numeric(pc_sum_wday$hour)
pc_sum_weday$time <- as.numeric(pc_sum_weday$hour)

#Plotting total energy consumption per hour for a weekday
pc_sum_wday$time <- as.factor(pc_sum_wday$time)
pc_sum_wday %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(color=time))  +
  labs(title="Power consumption of Nursery 1 sockets on a weekday in Aug 2019" , 
       y="Power consumption (W)",
       x = "Hour of day")

#Plotting total energy consumption per hour for a weekend day
pc_sum_weday$time <- as.factor(pc_sum_weday$time)
pc_sum_weday %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(color=time))  +
  labs(title="Power consumption of Nursery 1 sockets on a weekend day in Aug 2019" , 
       y="Power consumption (W)",
       x = "Hour of day")

#6. Hourly socket consumption at Nursery 2 for a weekend/week day in (month)
#s5, s6, s7, s8
pc_nur2_s <- data.frame()
pc_nur2_s <- rbind(pc_nur2_s, s5[s5$variable=="vRELAY1_LVL",],
                 s6[s6$variable=="vRELAY1_LVL",],
                 s7[s7$variable=="vRELAY1_LVL",],
                 s8[s8$variable=="vRELAY1_LVL",])

#Adding energy consumption per hour per day
pc_sum <- pc_nur2_s %>%
  group_by(date, hour) %>%
  summarise(value=sum(value))

#Add days of week to separate weekends from weekdays
pc_sum$wday <- weekdays(as.Date(pc_sum$date), abbreviate = TRUE)
pc_sum_wday <- pc_sum[pc_sum$wday=="Mon" | pc_sum$wday=="Tue" | pc_sum$wday=="Wed" | 
                        pc_sum$wday=="Thu" | pc_sum$wday=="Fri", ]
pc_sum_weday <- pc_sum[pc_sum$wday=="Sat" | pc_sum$wday=="Sun" , ]
pc_sum_wday$time <- as.numeric(pc_sum_wday$hour)
pc_sum_weday$time <- as.numeric(pc_sum_weday$hour)

#Plotting total energy consumption per hour for a weekday
pc_sum_wday$time <- as.factor(pc_sum_wday$time)
pc_sum_wday %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(color=time))  +
  labs(title="Power consumption of Nursery 2 sockets on a weekday in Aug 2019" , 
       y="Power consumption (W)",
       x = "Hour of day")

#Plotting total energy consumption per hour for a weekend day
pc_sum_weday$time <- as.factor(pc_sum_weday$time)
pc_sum_weday %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(color=time))  +
  labs(title="Power consumption of Nursery 2 sockets on a weekend day in Aug 2019" , 
       y="Power consumption (W)",
       x = "Hour of day")

#7. Hourly socket consumption at the Playground for a weekend/week day in (month)
#s9, s10
pc_pg_s <- data.frame()
pc_pg_s <- rbind(pc_pg_s, s9[s9$variable=="vRELAY1_LVL",],
                 s10[s10$variable=="vRELAY1_LVL",])

#Adding energy consumption per hour per day
pc_sum <- pc_pg_s %>%
  group_by(date, hour) %>%
  summarise(value=sum(value))

#Add days of week to separate weekends from weekdays
pc_sum$wday <- weekdays(as.Date(pc_sum$date), abbreviate = TRUE)
pc_sum_wday <- pc_sum[pc_sum$wday=="Mon" | pc_sum$wday=="Tue" | pc_sum$wday=="Wed" | 
                        pc_sum$wday=="Thu" | pc_sum$wday=="Fri", ]
pc_sum_weday <- pc_sum[pc_sum$wday=="Sat" | pc_sum$wday=="Sun" , ]
pc_sum_wday$time <- as.numeric(pc_sum_wday$hour)
pc_sum_weday$time <- as.numeric(pc_sum_weday$hour)

#Plotting total energy consumption per hour for a weekday
pc_sum_wday$time <- as.factor(pc_sum_wday$time)
pc_sum_wday %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(color=time))  +
  labs(title="Power consumption of Playground sockets on a weekday in Aug 2019" , 
       y="Power consumption (W)",
       x = "Hour of day")

#Plotting total energy consumption per hour for a weekend day
pc_sum_weday$time <- as.factor(pc_sum_weday$time)
pc_sum_weday %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(color=time))  +
  labs(title="Power consumption of Playground sockets on a weekend day in Aug 2019" , 
       y="Power consumption (W)",
       x = "Hour of day")

#8. Hourly indoor light consumption at Nursery 1 for a weekend/week day in (month)
#cpe1-led1,led2; cpe2; cpe3-led1,led2; cpe4; cpe5-led1,led2; cpe6
pc_nur1_indoor_cpe <- data.frame()
pc_nur1_indoor_cpe <- rbind(pc_nur1_indoor_cpe, 
                      cpe1[cpe1$variable=="LED1_P" | cpe1$variable=="LED2_P",],
                      cpe2, cpe3[cpe3$variable=="LED1_P" | cpe3$variable=="LED2_P",],
                      cpe4, cpe5[cpe5$variable=="LED1_P" | cpe5$variable=="LED2_P",],
                      cpe6)

#Adding energy consumption per hour per day
pc_sum <- pc_nur1_indoor_cpe %>%
  group_by(date, hour) %>%
  summarise(value=sum(value))

#Add days of week to separate weekends from weekdays
pc_sum$wday <- weekdays(as.Date(pc_sum$date), abbreviate = TRUE)
pc_sum_wday <- pc_sum[pc_sum$wday=="Mon" | pc_sum$wday=="Tue" | pc_sum$wday=="Wed" | 
                        pc_sum$wday=="Thu" | pc_sum$wday=="Fri", ]
pc_sum_weday <- pc_sum[pc_sum$wday=="Sat" | pc_sum$wday=="Sun" , ]
pc_sum_wday$time <- as.numeric(pc_sum_wday$hour)
pc_sum_weday$time <- as.numeric(pc_sum_weday$hour)

#Plotting total energy consumption per hour for a weekday
pc_sum_wday$time <- as.factor(pc_sum_wday$time)
pc_sum_wday %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(color=time))  +
  labs(title="Power consumption of Nursery 1 indoor lights on a weekday in Aug 2019" , 
       y="Power consumption (W)",
       x = "Hour of day")

#Plotting total energy consumption per hour for a weekend day
pc_sum_weday$time <- as.factor(pc_sum_weday$time)
pc_sum_weday %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(color=time))  +
  labs(title="Power consumption of Nursery 1 indoor lights on a weekend day in Aug 2019" , 
       y="Power consumption (W)",
       x = "Hour of day")

#9. Hourly indoor light consumption at Nursery 2 for a weekend/week day in (month)
#cpe7; cpe8-led1,led2; cpe9; cpe10-led1,led2; cpe11, cpe12-led1,led2
pc_nur2_indoor_cpe <- data.frame()
pc_nur2_indoor_cpe <- rbind(pc_nur2_indoor_cpe, cpe7, 
                      cpe8[cpe8$variable=="LED1_P" | cpe8$variable=="LED2_P",],
                      cpe9, cpe10[cpe10$variable=="LED1_P" | cpe10$variable=="LED2_P",],
                      cpe11, cpe12[cpe12$variable=="LED1_P" | cpe12$variable=="LED2_P",])

#Adding energy consumption per hour per day
pc_sum <- pc_nur2_indoor_cpe %>%
  group_by(date, hour) %>%
  summarise(value=sum(value))

#Add days of week to separate weekends from weekdays
pc_sum$wday <- weekdays(as.Date(pc_sum$date), abbreviate = TRUE)
pc_sum_wday <- pc_sum[pc_sum$wday=="Mon" | pc_sum$wday=="Tue" | pc_sum$wday=="Wed" | 
                        pc_sum$wday=="Thu" | pc_sum$wday=="Fri", ]
pc_sum_weday <- pc_sum[pc_sum$wday=="Sat" | pc_sum$wday=="Sun" , ]
pc_sum_wday$time <- as.numeric(pc_sum_wday$hour)
pc_sum_weday$time <- as.numeric(pc_sum_weday$hour)

#Plotting total energy consumption per hour for a weekday
pc_sum_wday$time <- as.factor(pc_sum_wday$time)
pc_sum_wday %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(color=time))  +
  labs(title="Power consumption of Nursery 2 indoor lights on a weekday in Aug 2019" , 
       y="Power consumption (W)",
       x = "Hour of day")

#Plotting total energy consumption per hour for a weekend day
pc_sum_weday$time <- as.factor(pc_sum_weday$time)
pc_sum_weday %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(color=time))  +
  labs(title="Power consumption of Nursery 2 indoor lights on a weekend day in Aug 2019" , 
       y="Power consumption (W)",
       x = "Hour of day")

#10. Hourly outdoor light consumption at Nursery 1 for a weekend/week day in (month)
#cpe1-led3; cpe3-led3; cpe5-led3
pc_nur1_outdoor_cpe <- data.frame()
pc_nur1_outdoor_cpe <- rbind(pc_nur1_outdoor_cpe, cpe1[cpe1$variable=="LED3_P",],
                             cpe3[cpe3$variable=="LED3_P",],
                             cpe5[cpe5$variable=="LED3_P",])

#Adding energy consumption per hour per day
pc_sum <- pc_nur1_outdoor_cpe %>%
  group_by(date, hour) %>%
  summarise(value=sum(value))

#Add days of week to separate weekends from weekdays
pc_sum$wday <- weekdays(as.Date(pc_sum$date), abbreviate = TRUE)
pc_sum_wday <- pc_sum[pc_sum$wday=="Mon" | pc_sum$wday=="Tue" | pc_sum$wday=="Wed" | 
                        pc_sum$wday=="Thu" | pc_sum$wday=="Fri", ]
pc_sum_weday <- pc_sum[pc_sum$wday=="Sat" | pc_sum$wday=="Sun" , ]
pc_sum_wday$time <- as.numeric(pc_sum_wday$hour)
pc_sum_weday$time <- as.numeric(pc_sum_weday$hour)

#Plotting total energy consumption per hour for a weekday
pc_sum_wday$time <- as.factor(pc_sum_wday$time)
pc_sum_wday %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(color=time))  +
  labs(title="Power consumption of Nursery 1 outdoor lights on a weekday in Aug 2019" , 
       y="Power consumption (W)",
       x = "Hour of day")

#Plotting total energy consumption per hour for a weekend day
pc_sum_weday$time <- as.factor(pc_sum_weday$time)
pc_sum_weday %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(color=time))  +
  labs(title="Power consumption of Nursery 1 outdoor lights on a weekend day in Aug 2019" , 
       y="Power consumption (W)",
       x = "Hour of day")

#11. Hourly outdoor light consumption at Nursery 2 for a weekend/week day in (month)
#cpe8-led3; cpe10-led3; cpe12-led3
pc_nur2_outdoor_cpe <- data.frame()
pc_nur2_outdoor_cpe <- rbind(pc_nur2_outdoor_cpe, cpe8[cpe8$variable=="LED3_P", ],
                             cpe10[cpe10$variable=="LED3_P", ] ,
                             cpe12[cpe12$variable=="LED3_P", ])

#Adding energy consumption per hour per day
pc_sum <- pc_nur2_outdoor_cpe %>%
  group_by(date, hour) %>%
  summarise(value=sum(value))

#Add days of week to separate weekends from weekdays
pc_sum$wday <- weekdays(as.Date(pc_sum$date), abbreviate = TRUE)
pc_sum_wday <- pc_sum[pc_sum$wday=="Mon" | pc_sum$wday=="Tue" | pc_sum$wday=="Wed" | 
                        pc_sum$wday=="Thu" | pc_sum$wday=="Fri", ]
pc_sum_weday <- pc_sum[pc_sum$wday=="Sat" | pc_sum$wday=="Sun" , ]
pc_sum_wday$time <- as.numeric(pc_sum_wday$hour)
pc_sum_weday$time <- as.numeric(pc_sum_weday$hour)

#Plotting total energy consumption per hour for a weekday
pc_sum_wday$time <- as.factor(pc_sum_wday$time)
pc_sum_wday %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(color=time))  +
  labs(title="Power consumption of Nursery 2 outdoor lights on a weekday in Aug 2019" , 
       y="Power consumption (W)",
       x = "Hour of day")

#Plotting total energy consumption per hour for a weekend day
pc_sum_weday$time <- as.factor(pc_sum_weday$time)
pc_sum_weday %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(color=time))  +
  labs(title="Power consumption of Nursery 2 outdoor lights on a weekend day in Aug 2019" , 
       y="Power consumption (W)",
       x = "Hour of day")

#12. Hourly light consumption at playground for a weekend/week day in (month)
#cpe13, cpe14, cpe15, cpe16, cpe17
pc_pg_cpe <- data.frame()
pc_pg_cpe <- rbind(pc_pg_cpe, cpe13, cpe14, cpe15, cpe16, cpe17)
#Adding energy consumption per hour per day
pc_sum <- pc_pg_cpe %>%
  group_by(date, hour) %>%
  summarise(value=sum(value))
#Add days of week to separate weekends from weekdays
pc_sum$wday <- weekdays(as.Date(pc_sum$date), abbreviate = TRUE)
pc_sum_wday <- pc_sum[pc_sum$wday=="Mon" | pc_sum$wday=="Tue" | pc_sum$wday=="Wed" | 
                        pc_sum$wday=="Thu" | pc_sum$wday=="Fri", ]
pc_sum_weday <- pc_sum[pc_sum$wday=="Sat" | pc_sum$wday=="Sun" , ]

pc_sum_wday$time <- as.numeric(pc_sum_wday$hour)
pc_sum_weday$time <- as.numeric(pc_sum_weday$hour)

#Plotting total energy consumption per hour for a weekday
pc_sum_wday$time <- as.factor(pc_sum_wday$time)
pc_sum_wday %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(color=time))  +
  labs(title="Power consumption of lights at playground on a weekday in Aug 2019" , 
       y="Power consumption (W)",
       x = "Hour of day")

#Plotting total energy consumption per hour for a weekend day
pc_sum_weday$time <- as.factor(pc_sum_weday$time)
pc_sum_weday %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(color=time))  +
  labs(title="Power consumption of lights at playground on a weekend day in Aug 2019" , 
       y="Power consumption (W)",
       x = "Hour of day")

#13. Hourly light consumption at streetlights for a weekend/week day in (month)
#cpe18, cpe19, cpe20
pc_sl_cpe <- data.frame()
pc_sl_cpe <- rbind(pc_sl_cpe, cpe18, cpe19, cpe20)
#Adding energy consumption per hour per day
pc_sum <- pc_sl_cpe %>%
  group_by(date, hour) %>%
  summarise(value=sum(value))
#Add days of week to separate weekends from weekdays
pc_sum$wday <- weekdays(as.Date(pc_sum$date), abbreviate = TRUE)
pc_sum_wday <- pc_sum[pc_sum$wday=="Mon" | pc_sum$wday=="Tue" | pc_sum$wday=="Wed" | 
                        pc_sum$wday=="Thu" | pc_sum$wday=="Fri", ]
pc_sum_weday <- pc_sum[pc_sum$wday=="Sat" | pc_sum$wday=="Sun" , ]

pc_sum_wday$time <- as.numeric(pc_sum_wday$hour)
pc_sum_weday$time <- as.numeric(pc_sum_weday$hour)

#Plotting total energy consumption per hour for a weekday
pc_sum_wday$time <- as.factor(pc_sum_wday$time)
pc_sum_wday %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(color=time))  +
  labs(title="Power consumption of streetlights on a weekday in Aug 2019" , 
       y="Power consumption (W)",
       x = "Hour of day")

#Plotting total energy consumption per hour for a weekend day
pc_sum_weday$time <- as.factor(pc_sum_weday$time)
pc_sum_weday %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(color=time))  +
  labs(title="Power consumption of streetlights on a weekend day in Aug 2019" , 
       y="Power consumption (W)",
       x = "Hour of day")

#############################################################################
#Reading in System data - with 3 lines of column headers
filename <- "~/OneDrive - Coventry University/HEED_analysis/Micro-grid/Data/08_2019/System_data/2019-08-01 to 2019-08-31.xls"
headers <- read_excel(filename, col_names = FALSE, na="..", n_max = 3)
#Replace NA in header with "" for missing row 3 values
headers[is.na(headers)] <- ""
column_labels <- headers %>% summarize_all(str_c, collapse = " ")
headers = unname(unlist(column_labels[1,]))
system_data <- read_excel(filename, col_names = headers, na="..", skip = 3)
#Replace NA in data frame with "" for missing values as in raw file
system_data[is.na(system_data)] <- ""

#Extract system overview data for AC Consumption L1 - W
#Find all column names with System overview
columns <- headers[which(grepl("System overview", headers, fixed=TRUE))]
#Find all column names with AC Consumption L1
colNames <- columns[which(grepl("AC Consumption L1", columns, fixed=TRUE))]
colNames <- c(headers[1], colNames) 
sysOverview <- system_data[,colNames]

#Extract Solar Charger data to get PV power - W
#Find all column names with Solar Charger
columns <- headers[which(grepl("Solar Charger", headers, fixed=TRUE))]
#Find all column names with PV power
colNames <- columns[which(grepl("PV power", columns, fixed=TRUE))]
colNames <- c(headers[1], colNames) 
solarCharger <- system_data[,colNames]

#Extract Battery Monitor data to get State of charge (%), Discharged Energy(kWh), Charged Energy(kWh)
#Find all column names with Solar Charger
columns <- headers[which(grepl("Battery Monitor", headers, fixed=TRUE))]
#Find all column names with State of charge
colNames <- columns[which(grepl("State of charge", columns, fixed=TRUE))]
#Find all column names with Discharged Energy
colNames <- c(colNames, columns[which(grepl("Discharged Energy", columns, fixed=TRUE))])
#Find all column names with Charged Energy
colNames <- c(colNames, columns[which(grepl("Charged Energy", columns, fixed=TRUE))])
colNames <- c(headers[1], colNames) 
batteryMonitor <- system_data[,colNames]

systemData <- data.frame()
systemData <- cbind(sysOverview, solarCharger[,-1], batteryMonitor[,-1])

#############################################################################
#Reading in the weather data file
filename <- "~/OneDrive - Coventry University/HEED_analysis/Micro-grid/Data/08_2019/Weather_data/Micro-grid weather data.csv"
headers <- read_csv(filename, col_names = FALSE, na="..", n_max = 2)
#Replace NA in header with "" for missing row 3 values
headers[is.na(headers)] <- ""
column_labels <- headers %>% summarize_all(str_c, collapse = " ")
headers = unname(unlist(column_labels[1,]))
weather_data <- read_csv(filename, col_names = headers, na="..", skip = 2)
#Replace NA in data frame with "" for missing values as in raw file
weather_data[is.na(weather_data)] <- ""

############################################################################
############################# RQ 2 #########################################
#1. Daily and total socket consumption by the micro–grid users - Wmin
s_consumption <- s_all[s_all$variable=="AC_Day_Energy_session", ]
s_consumption$value <- s_consumption$value * 0.017 #Converting Wm to Wh

#Daily consumption - get daily consumption for each socket
s_hours <- s_consumption %>%
  group_by(date,id) %>%
  summarise(value = value[length(value)])

#Combine locations as nursery 1, nursery 2 and playground and add the total socket consumption
s_hours$location <- s_hours$id
index <- which(grepl("Nur1", s_hours$id, fixed=TRUE))
s_hours$location[index] <- "Nursery1_S"
index <- which(grepl("Nur2", s_hours$id, fixed=TRUE))
s_hours$location[index] <- "Nursery2_S"
index <- which(grepl("PG", s_hours$id, fixed=TRUE))
s_hours$location[index] <- "Playground_S"
#Sum consumption per day for each location
s_total <- s_hours %>%
  group_by(date, location) %>%
  summarise(value = sum(value))
  
s_total$location <- as.factor(s_total$location)
s_total$date <- as.factor(s_total$date)
s_total %>%
  ggplot(aes(x = date, y= value, fill = location)) +
  geom_bar(stat="identity", width=.5, position = "dodge") + 
  labs(title="Daily socket consumption in Aug 2019" , 
       y="Daily socket consumption (Wh)",
       x = "Day of month") +
  theme(axis.text.x = element_text(size=6, angle=45))

#Total socket consumption since commissioning for all locations
startDate <- as.Date("2019-07-01")
#Sum consumption per day over all locations
s_day <- s_total %>%
  group_by(date) %>%
  summarise(value = sum(value))
s_day$date <- as.Date(s_day$date)
s_day$dayElapsed <- as.numeric(s_day$date - startDate)
s_day %>%
  ggplot(aes(x=dayElapsed, y=value)) +
  geom_point(shape=8, color="orange") + 
  labs(title="Total consumption at all sockets since commissioning " , 
       y="Total socket consumption (Wh)",
       x = "Days since commissioning")

#2. Daily total user consumption loads for the micro–grid in (month).
#daily aux, streetlights, pg_cpe, n2_cpe, n1_cpe, pg_s, n2_s, n1_s
#we have daily socket usage for nur1, nur2 and pg in s_total but in Wh. So calculate using vRELAY1_LVL

#Calculating sockets per day - total socket load can be found from current power values
sc <- s_all[s_all$variable=="vRELAY1_LVL", ]
#Add locations: nursery1, nursery2, playground
#Combine locations as nursery 1, nursery 2 and playground and add the total socket consumption
sc$location <- sc$id
index <- which(grepl("Nur1", sc$id, fixed=TRUE))
sc$location[index] <- "Nursery1_S"
index <- which(grepl("Nur2", sc$id, fixed=TRUE))
sc$location[index] <- "Nursery2_S"
index <- which(grepl("PG", sc$id, fixed=TRUE))
sc$location[index] <- "Playground_S"

#Daily consumption for each location - summing hourly values for all lights in a location on a day
sc_hours <- sc %>%
  group_by(date,location) %>%
  summarise(value = sum(value))

#Calculating cpe per day - total light load can be found from the current power values
cpe_consumption <- cpe_all
#Add locations: nursery1, nursery2, playground
#Combine locations as nursery 1, nursery 2 and playground and add the total socket consumption
cpe_consumption$location <- cpe_consumption$id
index <- which(grepl("Nur1", cpe_consumption$id, fixed=TRUE))
cpe_consumption$location[index] <- "Nursery1_CPE"
index <- which(grepl("Nur2", cpe_consumption$id, fixed=TRUE))
cpe_consumption$location[index] <- "Nursery2_CPE"
index <- which(grepl("PG", cpe_consumption$id, fixed=TRUE))
cpe_consumption$location[index] <- "Playground_CPE"
index <- which(grepl("SL", cpe_consumption$id, fixed=TRUE))
cpe_consumption$location[index] <- "Streetlights"

#Daily consumption for each location - summing hourly values for all lights in a location on a day
cpe_hours <- cpe_consumption %>%
  group_by(date,location) %>%
  summarise(value = sum(value))

#The AC auxiliary loads and excluding system losses, is given by the 
#[AC Consumption L1 - all socket and CPE consumption values].
ac_consumption <- systemData[,c(1,2)]
colnames(ac_consumption) <- c("timestamp", "AC_consumption_L1_W")
#Calculate hourly mean
ac_consumption$timestamp <- strptime(ac_consumption$timestamp, "%Y-%m-%d %H:%M:%S")
ac_consumption$date <- format(ac_consumption$timestamp, format='%Y-%m-%d')
ac_consumption$time <- format(ac_consumption$timestamp, format='%H')
#Extract hourly means 
ac_hours <- ac_consumption %>%
  group_by(date, time) %>%
  summarise(value = mean(AC_consumption_L1_W))
#Daily ac consumption - sum of values for all 24 hours 
ac_days <- ac_hours %>%
  group_by(date) %>%
  summarise(value=sum(value))
colnames(ac_days) <- c("Date", "AC_consumption")

#Calculating auxiliary load as ac consumption - sc - cpe
#Use spread to separate keys into columns 
library(tidyr)
cpe_spread <- spread(cpe_hours, location, value)
sc_spread <- spread(sc_hours, location, value)

#Merge all the datasets
ac_days <- cbind(ac_days, cpe_spread[,-1], sc_spread[,-1])
ac_days$Aux <- ac_days$AC_consumption - ac_days$Nursery1_CPE - ac_days$Nursery2_CPE - ac_days$Playground_CPE - 
  ac_days$Streetlights - ac_days$Nursery1_S - ac_days$Nursery2_S - ac_days$Playground_S
daily_profile <- ac_days[,-2]
colnames(daily_profile) <- c("Date", "N1-CPE", "N2-CPE", "PG-CPE", "SL", "N1-Sockets", "N2-Sockets",
                             "PG-Sockets", "Aux")
daily_profile$Date <- as.factor(daily_profile$Date)
daily_profile_gather <- gather(daily_profile, "key", "value", 2:9)
daily_profile_gather %>%
  ggplot(aes(x = Date, y= value, fill = key)) +
  geom_bar(stat="identity", width=.5, position = "stack") + 
  labs(title="Daily user and system power consumption in Aug 2019" , 
       y="Daily consumption (W)",
       x = "Day of month") +
  theme(axis.text.x = element_text(size=6, angle=45))

#3. Monthly energy consumption profile for the micro-grid users.
library(lubridate)
daily_profile_gather$month <- as.character(month(daily_profile_gather$Date, label=TRUE, abbr=TRUE))
monthly_profile <- daily_profile_gather %>%
  group_by(month, key) %>%
  summarise(value=sum(value))

monthly_profile$month <- as.factor(monthly_profile$month)  
monthly_profile %>%
  ggplot(aes(x = month, y= value/1000, fill = key)) +
  geom_bar(stat="identity", width=.5, position = "stack") + 
  labs(title="Total monthly power consumption" , 
       y="Total power consumption (kW)",
       x = "Month of study") 

##############################################################################################
############################### RQ 3 #################################################################
#1. Average daily state of charge values each month
battery_state <- systemData[, c(1,4)]
colnames(battery_state) <- c("timestamp", "Battery_state")
battery_state$Battery_state <- as.numeric(battery_state$Battery_state)
battery_state <- na.omit(battery_state)
#Calculate hourly mean
battery_state$timestamp <- strptime(battery_state$timestamp, "%Y-%m-%d %H:%M:%S")
battery_state$date <- format(battery_state$timestamp, format='%Y-%m-%d')
battery_state$time <- format(battery_state$timestamp, format='%H')
#Extract hourly means for each variable - led1_p, led2_p, led3_p
bs_hours <- battery_state %>%
  group_by(date, time) %>%
  summarise(value = mean(Battery_state))
bs_hours$month <- as.character(month(bs_hours$date, label=TRUE, abbr=TRUE))
#Average daily state of charge values each month 
bs_daily <- bs_hours %>%
  group_by(month, date) %>%
  summarise(value=mean(value))
bs_daily %>%
  ggplot(aes(x = month, y= value, fill=month)) +
  geom_boxplot() + 
  labs(title="Battery state of charge values in each month" , 
       y="Average daily state of charge of battery (%)",
       x = "Month of study") 

#2. Typical day micro-grid energy generation and consumption in (month)
#Hourly Pot. PV power output values will be provided separately from live data obtained from SolCast in kW/m2. 
#This value needs to be multiplied by the area of the array (16.37 m2) and panel efficiency (15.5%).

#Act. PV Power Output: [PV Power]

#AC load: [AC consumption L1]

#Capture losses = Pot. PV power output – Act. PV power output

#Battery Charge Power: The [Charged Energy] gives value in kWh. 
#Convert to kW and then take a mean for the hour.

#Battery Discharge Power: The [Discharge Energy] gives value in kWh. 
#Convert to kW and then take a mean for the hour.

#Battery state of charge is stated in the system data file







#3. Monthly utilised PV power yield, PV power capture losses and system losses


#4. Micro-grid performance ratio, production factor and overall system efficiency for each month