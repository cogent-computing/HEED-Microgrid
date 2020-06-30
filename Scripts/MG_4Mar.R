# Script for analysis of MG data for paper 5                           

library(tidyverse)
library(lubridate)
library(readxl)
library(rlang)

#Read in all CPE files
cpe <- data.frame()
for(k in 1:20) {
  #Read files for each CPE
  setwd("~/OneDrive - Coventry University/HEED_analysis/Micro-grid/Data/")
  if(k==1) {
    str = "Nur 1A CPE1"
    setwd("./Nur 1A CPE1/")
  } else if(k==2) {
    str = "Nur 1A CPE2"
    setwd("./Nur 1A CPE2/")
  } else if(k==3) {
    str = "Nur 1B CPE3"
    setwd("./Nur 1B CPE3/")
  } else if(k==4) {
    str = "Nur 1B CPE4"
    setwd("./Nur 1B CPE4/")
  } else if(k==5) {
    str = "Nur 1C CPE5"
    setwd("./Nur 1C CPE5/")
  } else if(k==6) {
    str = "Nur 1C CPE6"
    setwd("./Nur 1C CPE6/")
  } else if(k==7) {
    str = "Nur 2A CPE7"
    setwd("./Nur 2A CPE7/")
  } else if(k==8) {
    str = "Nur 2A CPE8"
    setwd("./Nur 2A CPE8/")
  } else if(k==9) {
    str = "Nur 2B CPE9"
    setwd("./Nur 2B CPE9/")
  } else if(k==10) {
    str = "Nur 2B CPE10"
    setwd("./Nur 2B CPE10/")
  } else if(k==11) {
    str = "Nur 2C CPE11"
    setwd("./Nur 2C CPE11/")
  } else if(k==12) {
    str = "Nur 2C CPE12"
    setwd("./Nur 2C CPE12/")
  } else if(k==13) {
    str = "Playground CPE1"
    setwd("./Playground CPE1/")
  } else if(k==14) {
    str = "Playground CPE2"
    setwd("./Playground CPE2/")
  } else if(k==15) {
    str = "Playground CPE3"
    setwd("./Playground CPE3/")
  } else if(k==16) {
    str = "Playground CPE4"
    setwd("./Playground CPE4/")
  } else if(k==17) {
    str = "Playground CPE5"
    setwd("./Playground CPE5/")
  } else if(k==18) {
    str = "Streetlight 1 CPE"
    setwd("./Streetlight 1 CPE/")
  } else if(k==19) {
    str = "Streetlight 2 CPE"
    setwd("./Streetlight 2 CPE/")
  } else if(k==20) {
    str = "Streetlight 3 CPE"
    setwd("./Streetlight 3 CPE/")
  }
  
  #For each CPE - read files for each month - July, Aug, Sep, Oct, Nov and Dec
  monthsList <- c("Jul","Aug","Sep","Oct","Nov","Dec")
  #Files available from 19th July 2019
  for(j in 1:length(monthsList)) {
    if(j==1) {
      setwd("./07 2019/")
    } else if(j==2) {
      setwd("../08 2019/")
    } else if(j==3) {
      setwd("../09 2019/")
    } else if(j==4) {
      setwd("../10 2019/")
    } else if(j==5) {
      setwd("../11 2019/")
    } else if(j==6) {
      setwd("../12 2019/")
    }
    
    #For each month read all files
    file_list <- list.files()
    file_list <- file_list[file.size(file_list)>0]
    #Read each file in the list and append to cpe with the correct label
    for(i in 1:length(file_list)) {
      df <- read.csv(file_list[i], header = FALSE, quote = "\"", skipNul = TRUE)
      colnames(df) <- c("timestamp", "variable", "value")
      df$value <- as.character(df$value)
      df$variable <- as.character(df$variable)
      
      #Separate the power consumption variables only
      df_sub <- df[df$variable=="LED1_P" | df$variable=="LED2_P" | df$variable=="LED3_P",]
      df_sub$value <- as.numeric(df_sub$value)/1000.0 #Converting to W
      
      #Add ID to each file
      df_sub$id <- rep(str, length(df_sub$timestamp))
      
      #Add to cpe 
      cpe <- rbind(cpe, df_sub)
    }
  }
}
setwd("~/OneDrive - Coventry University/HEED_analysis/Micro-grid/")
write.csv(cpe, file="CPE_all.csv", row.names=FALSE)

#############################################################################
#Reading all socket files
sockets <- data.frame()
for(k in 1:10) {
  #Read files for each CPE
  setwd("~/OneDrive - Coventry University/HEED_analysis/Micro-grid/Data/")
  if(k==1) {
    str = "Nur 1A S1"
    setwd("./Nur 1A S1/")
  } else if(k==2) {
    str = "Nur 1A S2"
    setwd("./Nur 1A S2/")
  } else if(k==3) {
    str = "Nur 1B S1"
    setwd("./Nur 1B S1/")
  } else if(k==4) {
    str = "Nur 1C S1"
    setwd("./Nur 1C S1/")
  } else if(k==5) {
    str = "Nur 2A S1"
    setwd("./Nur 2A S1/")
  } else if(k==6) {
    str = "Nur 2A S2"
    setwd("./Nur 2A S2/")
  } else if(k==7) {
    str = "Nur 2B S1"
    setwd("./Nur 2B S1/")
  } else if(k==8) {
    str = "Nur 2C S1"
    setwd("./Nur 2C S1/")
  } else if(k==9) {
    str = "Playground S1"
    setwd("./Playground S1/")
  } else if(k==10) {
    str = "Playground S2"
    setwd("./Playground S2/")
  } 
  
  #For each Socket - read files for each month - July, Aug, Sep, Oct,Nov and Dec
  monthsList <- c("Jul","Aug","Sep","Oct","Nov","Dec")
  #Files available from 19th July 2019
  for(j in 1:length(monthsList)) {
    if(j==1) {
      setwd("./07 2019/")
    } else if(j==2) {
      setwd("../08 2019/")
    } else if(j==3) {
      setwd("../09 2019/")
    } else if(j==4) {
      setwd("../10 2019/")
    } else if(j==5) {
      setwd("../11 2019/")
    } else if(j==6) {
      setwd("../12 2019/")
    }
    
    #For each month list all files
    file_list <- list.files()
    file_list <- file_list[file.size(file_list)>0]
    #Read each daily file in the list and append to sockets with the correct label
    if(length(file_list)>0) {
      for(i in 1:length(file_list)) {
        df <- read.csv(file_list[i], header = FALSE, quote = "\"", skipNul = TRUE)
        colnames(df) <- c("timestamp", "variable", "value")
        df$value <- as.character(df$value)
        df$variable <- as.character(df$variable)
        
        #Separate the power consumption (vRELAY1_LVL in W) and cumulative energy (AC_Day_Energy_session in Wattmin)
        df_sub <- df[df$variable=="vRELAY1_LVL" | 
                       df$variable=="AC_Day_Energy_session", ]
        df_sub$value <- as.numeric(df_sub$value)
        
        #Add ID to each file
        df_sub$id <- rep(str, length(df_sub$timestamp))
        
        #Add to cpe 
        sockets <- rbind(sockets, df_sub)
      } 
    }
  }
}
setwd("~/OneDrive - Coventry University/HEED_analysis/Micro-grid/")
write.csv(sockets, file="Sockets_all.csv", row.names=FALSE)

###############################################################################
#Once the values are all read - analyse the data to get missing days and hours
#Create a vector of all dates in each month
july_2019 <- seq(as.Date("2019-07-02"), as.Date("2019-07-31"), by="days")
aug_2019 <- seq(as.Date("2019-08-01"), as.Date("2019-08-31"), by="days")
sep_2019 <- seq(as.Date("2019-09-01"), as.Date("2019-09-30"), by="days")
oct_2019 <- seq(as.Date("2019-10-01"), as.Date("2019-10-31"), by="days")
nov_2019 <- seq(as.Date("2019-11-01"), as.Date("2019-11-30"), by="days")
dec_2019 <- seq(as.Date("2019-12-01"), as.Date("2019-12-31"), by="days")
all_days <- c(july_2019, aug_2019, sep_2019, oct_2019, nov_2019, dec_2019)
#Create a vector for all 24 hours in a day
all_hours <- format( seq.POSIXt(as.POSIXct(Sys.Date()), as.POSIXct(Sys.Date()+1), 
                                by = "1 hour"), "%H", tz="GMT")
all_hours <- all_hours[-25]

############################################################################
#Analyse the cpe data to extract hourly means for instantaenous power

#Reading in the CPE data
cpe <- read.csv("~/OneDrive - Coventry University/HEED_analysis/Micro-grid/CPE_all.csv",
                stringsAsFactors = FALSE)

#Reading in the socket data
sockets <- read.csv("~/OneDrive - Coventry University/HEED_analysis/Micro-grid/Sockets_all.csv",
                    stringsAsFactors = FALSE)

#Total days of study = 183 (02 July to 31 Dec 2019)
cpe$time <- as.POSIXct(as.numeric(cpe$timestamp), tz="GMT", origin="1970-01-01")
#Add 2 hours to change time to african time zone
cpe$time <- cpe$time %m+% hours(2)
cpe$date <- date(cpe$time)
cpe$timeUse <- format(cpe$time, format='%H')
cpe_sub <- cpe %>%
  group_by(id, date, timeUse, variable) %>%
  summarise(value=mean(value, na.rm=TRUE))
cpe_sub <- as.data.frame(cpe_sub)
cpe_sub <- cpe_sub[cpe_sub$date>="2019-07-02",]
cpe_sub$month <- as.character(month(cpe_sub$date, label=TRUE, abbr=TRUE))
cpe_write <- cpe_sub
cpe_write$id <- paste(cpe_write$id, cpe_write$variable) #Combine id and variable
cpe_write <- cpe_write[,-4] #Remove variable
cpe_write <- spread(cpe_write, id, value)
cpe_write$time <- as.POSIXct(paste(paste(cpe_write$date, 
                                         cpe_write$timeUse), ":00:00",sep=""),
                             format="%Y-%m-%d %H:%M:%S", tz="GMT")
cpe_write <- cpe_write[,-c(1,2,3)] #Remove date, time and month
cpe_write <- cpe_write[,c(61,1:60)] #Rearrange columns
#Select data from 02nd July
cpe_write <- cpe_write[cpe_write$time>="2019-07-02 00:00:00 GMT", ]
setwd("~/OneDrive - Coventry University/HEED_analysis/Micro-grid/")
write.csv(cpe_write, file="CPE_all_hourly_mean.csv", row.names=F)

################################################################################
#Analyse the sockets data to extract hourly means for instantaenous power and 
#hourly totals for AC_day_energy_session. Total days of study = 183 till Dec
sockets_sub <- data.frame()
for(i in 1:length(unique(sockets$id))) {
  df <- sockets[sockets$id == unique(sockets$id)[i], ]
  
  #Add time - we need to extract hours to get hourly means
  df$time <- as.POSIXct(df$timestamp, tz="GMT", origin="1970-01-01")
  #Add 2 hours to change time to african time zone
  df$time <- df$time %m+% hours(2)
  #Get date per file
  df$date <- date(df$time)
  df$timeUse <- format(df$time, format='%H')
  
  #Extract hourly means for instantaneous power
  df2 <- df[df$variable!="AC_Day_Energy_session",] %>%
    group_by(id, date, timeUse, variable) %>%
    summarise(value = mean(value, na.rm=TRUE))
  df2 <- as.data.frame(df2)
  
  #Correct cumulative total energy if not reset at 0th hour for a day
  df3_sub <- df[df$variable=="AC_Day_Energy_session", ]
  df3_clean <- data.frame()
  for(d in 1:length(unique(df3_sub$id))) {
    df_id <- df3_sub[df3_sub$id==unique(df3_sub$id)[d],]
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
  }
  
  #Extract cumulative total energy per hour
  df3 <- df3_clean %>%
    group_by(id, date, timeUse, variable) %>%
    summarise(value = value2[length(na.omit(value2))])
  df3 <- as.data.frame(df3)
  
  #Bind the 2 data frames and add date from the file name
  df2 <- rbind(df2, df3)
  
  #Create a data frame with the above values and bind to a complete df 
  sockets_sub <- rbind(sockets_sub, df2)
}
sockets_sub$month <- as.character(month(sockets_sub$date, label=TRUE, abbr=TRUE))
sockets_sub$value[sockets_sub$variable=="AC_Day_Energy_session"] <- 
  sockets_sub$value[sockets_sub$variable=="AC_Day_Energy_session"] * 0.017 #Wm to Wh
sockets_sub$id <- paste(sockets_sub$id, sockets_sub$variable) #Combine id and variable
sockets_sub <- sockets_sub[,-4] #Remove variable
sockets_sub <- sockets_sub[sockets_sub$date>="2019-07-02",]
sockets_all <- spread(sockets_sub, id, value)
sockets_write <- data.frame()
#Calculating diff of energy session to get consumption per hour
for(i in 1:length(unique(sockets_all$date))) {
  df <- sockets_all[sockets_all$date == unique(sockets_all$date)[i], ]
  s1 <- df$`Nur 1A S1 AC_Day_Energy_session`
  s2 <- df$`Nur 1A S2 AC_Day_Energy_session`
  s3 <- df$`Nur 1B S1 AC_Day_Energy_session`
  s4 <- df$`Nur 1C S1 AC_Day_Energy_session`
  s5 <- df$`Nur 2A S1 AC_Day_Energy_session`
  s6 <- df$`Nur 2A S2 AC_Day_Energy_session`
  s7 <- df$`Nur 2B S1 AC_Day_Energy_session`
  s8 <- df$`Nur 2C S1 AC_Day_Energy_session`
  s9 <- df$`Playground S1 AC_Day_Energy_session`
  s10 <- df$`Playground S2 AC_Day_Energy_session`
  for(j in 2:length(df$date)) {
    df$`Nur 1A S1 AC_Day_Energy_session`[j] <- s1[j] - s1[j-1]
    df$`Nur 1A S2 AC_Day_Energy_session`[j] <- s2[j] - s2[j-1]
    df$`Nur 1B S1 AC_Day_Energy_session`[j] <- s3[j] - s3[j-1]
    df$`Nur 1C S1 AC_Day_Energy_session`[j] <- s4[j] - s4[j-1]
    df$`Nur 2A S1 AC_Day_Energy_session`[j] <- s5[j] - s5[j-1]
    df$`Nur 2A S2 AC_Day_Energy_session`[j] <- s6[j] - s6[j-1]
    df$`Nur 2B S1 AC_Day_Energy_session`[j] <- s7[j] - s7[j-1]
    df$`Nur 2C S1 AC_Day_Energy_session`[j] <- s8[j] - s8[j-1]
    df$`Playground S1 AC_Day_Energy_session`[j] <- s9[j] - s9[j-1]
    df$`Playground S1 AC_Day_Energy_session`[j] <- s10[j] - s10[j-1]
  }
  sockets_write <- rbind(sockets_write, df)
}
sockets_write$time <- as.POSIXct(paste(paste(sockets_write$date, 
                                             sockets_write$timeUse), ":00:00",sep=""),
                                 format="%Y-%m-%d %H:%M:%S", tz="GMT")
sockets_write <- sockets_write[,-c(1,2,3)] #Remove date and time
sockets_write <- sockets_write[,c(21,1:20)] #Rearrange columns
#Select data from 19th July
sockets_write <- sockets_write[sockets_write$time>="2019-07-02 00:00:00 GMT", ]
#Store data in a file
setwd("~/OneDrive - Coventry University/HEED_analysis/Micro-grid/")
write.csv(sockets_write, file="Sockets_all_hourly_mean.csv", row.names=FALSE)

##################################################################################
#Number of on hours per day for each id and each variable
cpe_on_hours <- cpe_sub %>%
  group_by(id, date, variable) %>%
  summarise(onHours = length(na.omit(value)))
cpe_on_hours <- as.data.frame(cpe_on_hours)
#Once we have hours on for each available day, look for missing dates
cpe_on_hours2 <- data.frame()
for(i in 1:length(unique(cpe_on_hours$id))) {
  df <- cpe_on_hours[cpe_on_hours$id == unique(cpe_on_hours$id)[i], ]
  for(j in 1:length(unique(df$variable))) {
    df_sub <- df[df$variable == unique(df$variable)[j], ]
    for(k in 1:length(all_days)) {
      if(!(all_days[k] %in% df_sub$date)) {
        df_sub <- rbind(df_sub, data.frame(id=df_sub$id[1], date=all_days[k],
                                           variable=df_sub$variable[1], onHours=0))
      }
    }
    cpe_on_hours2 <- rbind(cpe_on_hours2, df_sub)
  }
}
cpe_on_hours2 <- cpe_on_hours2[order(cpe_on_hours2$date),]
cpe_on_hours2$month <- as.character(month(cpe_on_hours2$date,label=TRUE, abbr=TRUE))

cpe_on_hours2$date <- as.Date(cpe_on_hours2$date)
cpe_on_hours2$id <- as.factor(cpe_on_hours2$id)
cpe_on_hours2$variable <- as.factor(cpe_on_hours2$variable)
cpe_on_hours2 %>%
  ggplot(aes(date, onHours, color=id, shape=variable)) + 
  geom_point() +
  labs(title="Hours of data collection per day for each LED at Microgrid since Jul 2019" ,
       y="Hours of data collection in a day",
       x = "Day of study" ,
       color="CPE", 
       shape="LED") +
  scale_shape_manual(values=c(0,4,1,5,2)) +
  theme(axis.text.x = element_text(size=8), plot.title = element_text(size=10)) +
  scale_y_continuous(breaks=c(0,2,4,6,8,10,12,14,16,18,20,22,24), 
                     labels=c("0","2","4","6","8","10","12","14","16","18","20",
                              "22","24"),
                     limits=c(0,24)) +
  scale_x_date(date_minor_breaks = "1 day") +
  theme(legend.box = "vertical", legend.position = "bottom",
        legend.key.size = unit(0.1, "cm"), 
        legend.margin = margin(t=0,r=0,b=0,l=0))

###########################################################################
##Number of on hours for socket data
sockets_sub$variable <- sockets_sub$id
for(i in 1:length(sockets_sub$id)) {
  if(grepl("vRELAY1_LVL", sockets_sub$id[i], fixed=TRUE)) {
    sockets_sub$variable[i] <- "vRELAY1_LVL"
  } else {
    sockets_sub$variable[i] <- "AC_Day_Energy_session"
  }
  sockets_sub$id[i] <- str_remove(sockets_sub$id[i], sockets_sub$variable[i])
}
sockets_on_hours <- sockets_sub %>%
  group_by(id, date, variable) %>%
  summarise(onHours = length(na.omit(value)))
sockets_on_hours <- as.data.frame(sockets_on_hours)

sockets_on_hours2 <- data.frame()
for(i in 1:length(unique(sockets_on_hours$id))) {
  df <- sockets_on_hours[sockets_on_hours$id == unique(sockets_on_hours$id)[i], ]
  for(j in 1:length(unique(df$variable))) {
    df_sub <- df[df$variable == unique(df$variable)[j], ]
    for(k in 1:length(all_days)) {
      if(!(all_days[k] %in% df_sub$date)) {
        df_sub <- rbind(df_sub, data.frame(id=df_sub$id[1], date=all_days[k],
                                           variable=df_sub$variable[1], onHours=0))
      }
    }
    sockets_on_hours2 <- rbind(sockets_on_hours2, df_sub)
  }
}
sockets_on_hours2 <- sockets_on_hours2[order(sockets_on_hours2$date), ]

sockets_on_hours2$month <- as.character(month(sockets_on_hours2$date,label=TRUE, 
                                              abbr=TRUE))
sockets_on_hours2$date <- as.Date(sockets_on_hours2$date)
sockets_on_hours2$id <- as.factor(sockets_on_hours2$id)
sockets_on_hours2$variable <- as.factor(sockets_on_hours2$variable)
sockets_on_hours2 %>%
  ggplot(aes(date, onHours, color=id, shape=variable)) + 
  geom_point() +
  labs(title="Hours of data collection per day for each socket at Microgrid since Jul 2019" ,
       y="Hours of data collection in a day",
       x = "Day of study" ,
       color="Sockets", 
       shape="Parameter") + 
  theme(axis.text.x = element_text(size=8), plot.title = element_text(size=10)) +
  scale_y_continuous(breaks=c(0,2,4,6,8,10,12,14,16,18,20,22,24), 
                     labels=c("0","2","4","6","8","10","12","14","16","18","20",
                              "22","24"),
                     limits=c(0,24)) +
  scale_x_date(date_minor_breaks = "1 day") +
  theme(legend.box = "vertical", legend.position = "bottom",
        legend.key.size = unit(0.1, "cm"), 
        legend.margin = margin(t=0,r=0,b=0,l=0))

#Plot yield map for yield - yield calculated as % hours of total per day i.e. 24
cpe_on_hours2$yield <- cpe_on_hours2$onHours * 100.0 / 24.0
cpe_on_hours2$id2 <- paste(as.character(cpe_on_hours2$id), 
                           as.character(cpe_on_hours2$variable))
sockets_on_hours2$yield <- sockets_on_hours2$onHours * 100.0 / 24.0
sockets_on_hours2$id2 <- paste(as.character(sockets_on_hours2$id),
                               as.character(sockets_on_hours2$variable))
library(wesanderson)
#Plotting a heat map for CPE
pal <- wes_palette("Zissou1", 100, type = "continuous")
ggplot(cpe_on_hours2, aes(date, id2)) + geom_tile(aes(fill = yield)) +
  scale_fill_gradientn(colours = pal) + 
  xlab("X axis") +
  ylab("Y axis") +
  labs(title="Yield for CPE data at Microgrid: 02nd Jul'19 - 31st Dec'19" , 
       y="ID",
       x = "Day of study",
       fill="Yield") + 
  theme(axis.text.x = element_text(size = 8),
        plot.title = element_text(size=10),
        axis.text = element_text(size=4))

ggplot(sockets_on_hours2, aes(date, id2)) + geom_tile(aes(fill = yield)) +
  scale_fill_gradientn(colours = pal) + 
  xlab("X axis") +
  ylab("Y axis") +
  labs(title="Yield for sockets data at Microgrid: 02nd Jul'19 - 31st Dec'19" , 
       y="ID",
       x = "Day of study",
       fill="Yield") + 
  theme(axis.text.x = element_text(size = 8),
        plot.title = element_text(size=10),
        axis.text = element_text(size=7))

##############################################################################
#Last hour on per day for each id and each variable
cpe_sub$time <- as.numeric(cpe_sub$timeUse)
cpe_max_time <- cpe_sub %>%
  group_by(id, date, variable) %>%
  summarise(lastHour = max(time))
cpe_max_time <- as.data.frame(cpe_max_time)
#Once we have hours active for each available day, look for missing dates
cpe_max_time2 <- data.frame()
for(i in 1:length(unique(cpe_max_time$id))) {
  df <- cpe_max_time[cpe_max_time$id == unique(cpe_max_time$id)[i], ]
  for(j in 1:length(unique(df$variable))) {
    df_sub <- df[df$variable == unique(df$variable)[j], ]
    for(k in 1:length(all_days)) {
      if(!(all_days[k] %in% df_sub$date)) {
        df_sub <- rbind(df_sub, data.frame(id=df_sub$id[1], date=all_days[k],
                                           variable=df_sub$variable[1], lastHour=-1))
      }
    }
    cpe_max_time2 <- rbind(cpe_max_time2, df_sub)
  }
}
cpe_max_time2 <- cpe_max_time2[order(cpe_max_time2$date),]
cpe_max_time2$month <- as.character(month(cpe_max_time2$date,label=TRUE, abbr=TRUE))

cpe_max_time2$date <- as.Date(cpe_max_time2$date)
cpe_max_time2$id <- as.factor(cpe_max_time2$id)
cpe_max_time2$variable <- as.factor(cpe_max_time2$variable)
cpe_max_time2 %>%
  ggplot(aes(date, lastHour, color=id, shape=variable)) + 
  geom_point() +
  labs(title="Last hour of data collection per day for each LED at Microgrid since Jul 2019" ,
       y="Last hour of data collection",
       x = "Day of study" ,
       color="CPE", 
       shape="LED") +
  scale_shape_manual(values=c(0,4,1,5,2)) +
  theme(axis.text.x = element_text(size=8), plot.title = element_text(size=10)) +
  scale_y_continuous(breaks=c(0,1,3,5,7,9,11,13,15,17,19,21,23), 
                     labels=c("0","1","3","5","7","9","11","13","15","17",
                              "19","21","23"),
                     limits=c(-1,23)) +
  scale_x_date(date_minor_breaks = "1 day") + 
  theme(legend.box = "vertical", legend.position = "bottom",
        legend.key.size = unit(0.1, "cm"), 
        legend.margin = margin(t=0,r=0,b=0,l=0))

##############################################################################
#Last hour on per day for each id and each variable
sockets_sub$time <- as.numeric(sockets_sub$timeUse)
sockets_max_time <- sockets_sub %>%
  group_by(id, date, variable) %>%
  summarise(onHours = max(time))
sockets_max_time <- as.data.frame(sockets_max_time)
#Once we have hours active for each available day, look for missing dates
sockets_max_time2 <- data.frame()
for(i in 1:length(unique(sockets_max_time$id))) {
  df <- sockets_max_time[sockets_max_time$id == unique(sockets_max_time$id)[i], ]
  for(j in 1:length(unique(df$variable))) {
    df_sub <- df[df$variable == unique(df$variable)[j], ]
    for(k in 1:length(all_days)) {
      if(!(all_days[k] %in% df_sub$date)) {
        df_sub <- rbind(df_sub, data.frame(id=df_sub$id[1], date=all_days[k],
                                           variable=df_sub$variable[1], onHours=-1))
      }
    }
    sockets_max_time2 <- rbind(sockets_max_time2, df_sub)
  }
}
sockets_max_time2 <- sockets_max_time2[order(sockets_max_time2$date), ]
sockets_max_time2$month <- as.character(month(sockets_max_time2$date,label=TRUE, 
                                              abbr=TRUE))
sockets_max_time2$date <- as.Date(sockets_max_time2$date)
sockets_max_time2$id <- as.factor(sockets_max_time2$id)
sockets_max_time2$variable <- as.factor(sockets_max_time2$variable)
sockets_max_time2 %>%
  ggplot(aes(date, onHours, color=id, shape=variable)) + 
  geom_point() +
  scale_shape_manual(values=c(0,4,1,5)) +
  labs(title="Last hour of data collection per day for each socket at microgrid since Jul 2019" ,
       y="Last hour of data collection",
       x = "Day of study" ,
       color="Sockets", 
       shape="Parameter") + 
  theme(axis.text.x = element_text(size=8), plot.title = element_text(size=10)) +
  scale_y_continuous(breaks=c(0,1,3,5,7,9,11,13,15,17,19,21,23), 
                     labels=c("0","1","3","5","7","9","11","13","15","17",
                              "19","21","23"),
                     limits=c(-1,23)) +
  scale_x_date(date_minor_breaks = "1 day") + 
  theme(legend.box = "vertical", legend.position = "bottom",
        legend.key.size = unit(0.1, "cm"), 
        legend.margin = margin(t=0,r=0,b=0,l=0))

#################################################################################################
setwd("~/OneDrive - Coventry University/HEED_analysis/Micro-grid/Data/")
systemData <- data.frame()
for(k in 1:3) {
  #Read files for each CPE
  setwd("~/OneDrive - Coventry University/HEED_analysis/Micro-grid/Data/System Data/")
  if(k==1) {
    setwd("./Full data/") ##Time in Africa time
  } else if(k==2) {
    setwd("./11 2019/") ##Time in Africa time
  } else if(k==3) {
    setwd("./12 2019/") ##Time in Africa time
  } 
  
  #For each month list all files
  file_list <- list.files()
  
  headers <- read_csv(file_list[1], col_names = FALSE, na="..", n_max = 3)
  headers[is.na(headers)] <- ""
  column_labels <- headers %>% summarize_all(str_c, collapse = " ")
  headers = unname(unlist(column_labels[1,]))
  system_data <- read_csv(file_list[1], col_names = headers, na="..", skip = 3)
  system_data[is.na(system_data)] <- "" 
  
  #Extract values AC Consumption L1 (W), PV Power (W), State of charge (%), 
  #Discharged Energy (kWh), Charged Energy (kWh) 
  
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
  
  df <- data.frame()
  df <- cbind(sysOverview, solarCharger[,-1], batteryMonitor[,-1])
  df$`System overview [0] AC Consumption L1 W` <- 
    as.numeric(df$`System overview [0] AC Consumption L1 W`)
  df$`Solar Charger [260] PV power ` <- 
    as.numeric(df$`Solar Charger [260] PV power `)
  df$`Battery Monitor [258] Discharged Energy kWh` <- 
    as.numeric(df$`Battery Monitor [258] Discharged Energy kWh`)
  df$`Battery Monitor [258] Charged Energy kWh` <- 
    as.numeric(df$`Battery Monitor [258] Charged Energy kWh`) 
  #Time is in Berlin time zone for data till Nov otherwise Africa zone
  colnames(df) <- c("timestamp","AC_consumption_W","PV_power_W",
                    "State_of_charge","Discharged_energy_kWh",
                    "Charged_energy_kWh")
  
  systemData <- rbind(systemData, df)
}
#Trim time from 02 July
systemData$State_of_charge <- as.numeric(systemData$State_of_charge)
systemData$date <- date(systemData$timestamp)
systemData <- systemData[systemData$date>="2019-07-02",]
systemData <- systemData[-which(duplicated(systemData$timestamp) == TRUE), ]
setwd("~/OneDrive - Coventry University/HEED_analysis/Micro-grid/")
write.csv(systemData, file="systemData_jul_dec.csv", row.names=FALSE)

############################################################################################
######Analyse data to get hourly values #######################################
system_gather <- gather(systemData, "id", "value", 2:6)
system_gather$timeUse <- format(system_gather$timestamp, format='%H')  

#Hourly means can be calculated for AC consumption and PV power
system_hourly <- system_gather[system_gather$id=="AC_consumption_W" | 
                                 system_gather$id=="PV_power_W",] %>%
  group_by(date,timeUse,id) %>%
  summarise(value=mean(value,na.rm = TRUE))
system_hourly <- as.data.frame(system_hourly)

#Calculate the last value in an hour for state of charge
system_soc <- system_gather[system_gather$id=="State_of_charge",] %>%
  group_by(date,timeUse,id) %>%
  summarise(value=value[length(na.omit(value))])
system_soc <- as.data.frame(system_soc)
system_hourly <- rbind(system_hourly, system_soc)

#Calculate hourly values for discharged and charged energy by taking hourly differences
battery_charge <- system_gather[system_gather$id=="Charged_energy_kWh",]
battery_charge <- battery_charge[complete.cases(battery_charge), ]
#Extract hourly values by taking the last value for each hour 
battery_charge_hours <- battery_charge %>%
  group_by(date, timeUse, id) %>%
  summarise(value = value[length(na.omit(value))])
battery_charge_hours <- as.data.frame(battery_charge_hours)
a <- diff(battery_charge_hours$value)
battery_charge_hours <- battery_charge_hours[-1,]
battery_charge_hours$value <- a
battery_charge_hours$value <- battery_charge_hours$value * 1000.0 #W
system_hourly <- rbind(system_hourly, battery_charge_hours)

battery_discharge <- system_gather[system_gather$id=="Discharged_energy_kWh",]
battery_discharge <- battery_discharge[complete.cases(battery_discharge), ]
#Extract hourly values by taking the max value each hour 
battery_discharge_hours <- battery_discharge %>%
  group_by(date, timeUse, id) %>%
  summarise(value = value[length(na.omit(value))])
battery_discharge_hours <- as.data.frame(battery_discharge_hours)
a <- diff(battery_discharge_hours$value)
battery_discharge_hours <- battery_discharge_hours[-1,]
battery_discharge_hours$value <- a
battery_discharge_hours$value <- battery_discharge_hours$value * 1000.0 #W
system_hourly <- rbind(system_hourly, battery_discharge_hours)

system_hourly$month <- as.character(month(system_hourly$date, abbr=TRUE,
                                          label=TRUE))
system_hourly$time <- as.POSIXct(paste(paste(system_hourly$date, 
                                             system_hourly$timeUse),
                                       ":00:00",sep=""),
                                 format="%Y-%m-%d %H:%M:%S", tz="GMT")
system_hourly <- system_hourly[,-c(1,2,5)] #Remove date, time and month
system_hourly <- system_hourly[,c(3,1:2)] #Rearrange columns

system_write <- spread(system_hourly, id, value)
#Replace NA for charged and discharged energy with 0
system_write$Charged_energy_kWh[is.na(system_write$Charged_energy_kWh)] <- 0
system_write$Discharged_energy_kWh[is.na(system_write$Discharged_energy_kWh)] <- 0

colnames(system_write) <- c("time","AC_consumption_W","Charged_energy_W",
                            "Discharged_energy_W", "PV_power_W","State_of_charge")
setwd("~/OneDrive - Coventry University/HEED_analysis/Micro-grid/")
write.csv(system_write, file="system_all_hourly_jul_dec.csv", row.names=FALSE)

###########################################################################################
#Reading in weather data - hourly data is stored
filename <- "~/OneDrive - Coventry University/HEED_analysis/Micro-grid/Data/Weather data/Micro-grid weather data.csv"
headers <- read_csv(filename, col_names = FALSE, na="..", n_max = 2)
#Replace NA in header with "" for missing row 3 values
headers[is.na(headers)] <- ""
column_labels <- headers %>% summarize_all(str_c, collapse = " ")
headers = unname(unlist(column_labels[1,]))
weather_data <- read_csv(filename, col_names = headers, na="..", skip = 2)
#Replace NA in data frame with "" for missing values as in raw file
weather_data[is.na(weather_data)] <- ""
weather_data <- weather_data[,1:3]
weather_data$timestamp <- as.POSIXct(weather_data$`Time (2019!) `, tz="GMT", origin="1970-01-01",
                                     format = "%d/%m/%Y %H:%M")
##Data is 12 years behind - change year to 2019
weather_data$timestamp <- weather_data$timestamp %m+% years(12)
weather_data <- weather_data[,-c(1,2)]
weather_data <- weather_data[,c(2,1)]
weather_data <- weather_data[weather_data$timestamp>="2019-07-02 00:00:00 GMT", ]
weather_data <- weather_data[complete.cases(weather_data), ]
weather_data <- weather_data[-1,]
weather_data$`In-plane array Incident Solar kW/m2` <- 
  weather_data$`In-plane array Incident Solar kW/m2` * 16.37 * 0.155 * 1000.0 #W
colnames(weather_data) <- c("time","Pot_PV_power_W")
setwd("~/OneDrive - Coventry University/HEED_analysis/Micro-grid/")
write.csv(weather_data, file="weather_hourly_jul_dec.csv", row.names=FALSE)

##################################################################################
##CPE, socket, system and weather data has been analysed to get hourly values#####
##Use the hourly values to analyse the data for plots#############################
weather_data <- read.csv("~/OneDrive - Coventry University/HEED_analysis/Micro-grid/weather_hourly_jul_dec.csv",
                         header = TRUE,stringsAsFactors = FALSE)
weather_data$time <- as.POSIXct(weather_data$time, 
                                format = "%Y-%m-%d %H:%M")
weather_data$date <- date(weather_data$time)
weather_data$timestamp <- format(weather_data$time, format='%H')
weather_data <- weather_data[,-1]
weather_gather <- gather(weather_data, "id", "value", 1)

cpe_data <- read.csv("~/OneDrive - Coventry University/HEED_analysis/Micro-grid/CPE_all_hourly_mean.csv",
                     header=TRUE, stringsAsFactors = FALSE)
cpe_data$time <- as.POSIXct(cpe_data$time, format="%Y-%m-%d %H:%M")
cpe_data$date <- date(cpe_data$time)
cpe_data$timestamp <- format(cpe_data$time, format='%H')
cpe_data <- cpe_data[,-1]
cpe_gather <- gather(cpe_data, "id", "value", 1:60)

sockets_data <- read.csv("~/OneDrive - Coventry University/HEED_analysis/Micro-grid/Sockets_all_hourly_mean.csv",
                         header = TRUE, stringsAsFactors = FALSE)
sockets_data$time <- as.POSIXct(sockets_data$time, format="%Y-%m-%d %H:%M")
sockets_data$date <- date(sockets_data$time)
sockets_data$timestamp <- format(sockets_data$time, format='%H')
sockets_data <- sockets_data[,-1]
sockets_gather <- gather(sockets_data, "id", "value", 1:20)

system_data <- read.csv("~/OneDrive - Coventry University/HEED_analysis/Micro-grid/system_all_hourly_jul_dec.csv",
                        header=TRUE, stringsAsFactors = FALSE)
system_data$time <- as.POSIXct(system_data$time, format="%Y-%m-%d %H:%M")
system_data$date <- date(system_data$time)
system_data$timestamp <- format(system_data$time, format='%H')
system_data <- system_data[,-1]
system_gather <- gather(system_data, "id", "value", 1:5)

###########################################################################################
##Combining weather, system, cpe and socket hourly data for Micro-grid
mg_gather <- data.frame()
mg_gather <- rbind(mg_gather, weather_gather, cpe_gather, sockets_gather,
                     system_gather)
mg_data <- spread(mg_gather, id, value)
mg_data <- mg_data[mg_data$date<="2019-12-31",]
setwd("~/OneDrive - Coventry University/HEED_analysis/Micro-grid/")
write.csv(mg_data, file="MG_data_hourly_jul_dec.csv", row.names=FALSE)

####################################################################################################
#Plotting MG data for paper 5
library(ggplot2)
library(dplyr)
library(lubridate)
library(readxl)
library(tidyverse)
library(stringr)
library(rlang)
library(readr)
setwd("~/OneDrive - Coventry University/HEED_analysis/Micro-grid/")
mg_data <- read.csv("MG_data_hourly_jul_dec.csv", header=TRUE)
mg_data$date <- as.Date(mg_data$date)

#Predicted load 
pg_predicted <- data.frame(month=rep("Predicted",24),timestamp = c(0:23), 
                             avgLoad=c(0,0,0,0,0,0,0.32,0.32,0.11,0.05,0.02,0.02,
                                       0.02,0.02,0.02,0.02,0.02,0.06,0.12,0.32,
                                       0.32,0.32,0.32,0), stringsAsFactors = FALSE)
pg_predicted$avgLoad <- pg_predicted$avgLoad * 1000.0 #Wh

nur_predicted <- data.frame(month=rep("Predicted",24),timestamp = c(0:23), 
                            avgLoad=c(0,0,0,0,0,0,0.24,0.24,0.24,0.24,0,0,
                                      0.355,0.355,0,0.24,0.24,0.24,0.24,0.24,
                                      0.24,0.24,0,0), stringsAsFactors = FALSE)
nur_predicted$avgLoad <- nur_predicted$avgLoad * 1000.0 #Wh

#For plot 1 - hourly avg to show typical data since commissioning (all sockets and lights)
#Load 1 - sum CPE_P values with vRELAY_LVL values for sockets
mg_users1 <- mg_data[,c("date","timestamp", "AC_consumption_W",
                    colnames(mg_data)[grepl("CPE",colnames(mg_data),fixed=TRUE) | 
                      grepl("LVL",colnames(mg_data),fixed=TRUE)])]

#Finding quality of data - missing data for all variables
var <- vector(mode="character",length=0L)
len <- vector(mode="integer",length=0L)
for(i in 1:length(mg_users1)) {
  var <- append(var,colnames(mg_users1)[i])
  len <- append(len, length(na.omit(mg_users1[,i])))
}
mg_data_qual <- data.frame(var=var, len=len, yield=len*100/4392)
write.csv(mg_data_qual, file="~/OneDrive - Coventry University/HEED_analysis/Micro-grid/MG_missing_data.csv", row.names = FALSE)

#Remove data between 4th to 18th Sep (end dates inclusive)
mg_users1 <- mg_users1[mg_users1$date<"2019-09-04" | mg_users1$date>"2019-09-18",]

#Get monthly totals
#################################################################################
###Total monthly load 
mg_users1_monthly <- mg_users1
#Total load at nurseries - select all CPE and sockets at Nur except Nur1AS1
test <- mg_users1_monthly[,c(4:9,11:47)]
mg_users1_monthly$nurLoad <- rowSums(test)
#Total load at playground
test <- mg_users1_monthly[,c(48:64)]
mg_users1_monthly$pgLoad <- rowSums(test)

mg_users1_monthly$month <- as.character(month(mg_users1_monthly$date, label=TRUE, abbr=TRUE))
monthly_load <- mg_users1_monthly %>%
  dplyr::group_by(month) %>%
  dplyr::summarise(nurLoad = sum(nurLoad, na.rm=TRUE))
monthly_load_days <- mg_users1_monthly %>%
  dplyr::group_by(month) %>%
  dplyr::summarise(nurDays = length(unique(date)))
monthly_load_pg <- mg_users1_monthly %>%
  dplyr::group_by(month) %>%
  dplyr::summarise(pgLoad = sum(pgLoad, na.rm=TRUE))
monthly_load_pg_days <- mg_users1_monthly %>%
  dplyr::group_by(month) %>%
  dplyr::summarise(pgDays = length(unique(date)))

monthly_load$pgLoad <- monthly_load_pg$pgLoad
colnames(monthly_load) <- c("Month","Nursery_load_W","Playground_load_W")
write.csv(monthly_load, file="~/OneDrive - Coventry University/HEED_analysis/Micro-grid/MG_total_monthly.csv", 
          row.names=FALSE)

################################################################################
#Get typical load for each device
mg_users1 <- gather(mg_users1, "id","value", 3:73)
mg_users1$month <- as.character(month(mg_users1$date, abbr=TRUE, label=TRUE))
typical_user <- mg_users1 %>%
  dplyr::group_by(month, timestamp, id) %>%
  dplyr::summarise(value=mean(value, na.rm=TRUE))
typical_user <- as.data.frame(typical_user)
user_load1 <- spread(typical_user, id, value) 
#Replace NaN with NA
user_load1[user_load1=="NaN"] <- NA

#Total load at nurseries - select all CPE and sockets at Nur except Nur1AS1
test <- user_load1[,c(4:9,11:47)]
user_load1$nurLoad <- rowSums(test)

#Total load at playground
test <- user_load1[,48:64]
user_load1$pgLoad <- rowSums(test)

#Adding predicted columns for nursery and playground load
user_load1 <- cbind(user_load1, "nur_predicted"=rep(nur_predicted$avgLoad,6))
user_load1 <- cbind(user_load1, "pg_predicted"=rep(pg_predicted$avgLoad,6))

#For plot 1 - hourly avg to show typical load at nurseries
user_load1 %>%
  ggplot(aes(timestamp)) +
  geom_line(aes(y=nurLoad, color="Actual", linetype=month)) + 
  geom_line(aes(y = nur_predicted, color="Predicted")) +
  labs(title="Typical day socket and light load at the Nurseries (Micro-grid): 02 Jul'19 - 31st Dec'19",
       y="Energy consumption (Wh)",
       x = "Time of day (hours)",
       color="Group",
       linetype="Month") +
  scale_x_continuous(labels=c("0","3","6","9","12","15","18","21","24"),
                     breaks = c(0,3,6,9,12,15,18,21,24), minor_breaks = 0:24) +
  theme(plot.title = element_text(size=10), legend.position = "bottom",
        legend.box = "horizontal",  legend.key.size = unit(0.5, "cm"), 
        legend.margin = margin(t=0,r=0,b=0,l=0)) 

#Hourly avg to show typical load at playground
user_load1 %>%
  ggplot(aes(timestamp)) +
  geom_line(aes(y=pgLoad, color=month, linetype=month)) + 
  geom_line(aes(y = pg_predicted)) +
  labs(title="Typical day socket and light load at the Playground (Micro-grid): 02 Jul'19 - 31st Dec'19",
       y="Energy consumption (Wh)",
       x = "Time of day (hours)",
       color="Month",
       linetype="Month") +
  scale_x_continuous(labels=c("0","3","6","9","12","15","18","21","24"),
                     breaks = c(0,3,6,9,12,15,18,21,24), minor_breaks = 0:24) +
  theme(plot.title = element_text(size=10), legend.position = "bottom",
        legend.box = "horizontal",  legend.key.size = unit(0.5, "cm"), 
        legend.margin = margin(t=0,r=0,b=0,l=0)) 

##Plotting typical AC consumption values
user_load1 %>%
  ggplot(aes(timestamp)) +
  geom_line(aes(y=AC_consumption_W, color=month, linetype=month)) + 
  geom_line(aes(y = nur_predicted+pg_predicted)) +
  labs(title="Typical day AC consumption at the Micro-grid: 02 Jul'19 - 31st Dec'19",
       y="Energy consumption (Wh)",
       x = "Time of day (hours)",
       color="Month",
       linetype="Month") +
  scale_x_continuous(labels=c("0","3","6","9","12","15","18","21","24"),
                     breaks = c(0,3,6,9,12,15,18,21,24), minor_breaks = 0:24) +
  theme(plot.title = element_text(size=10), legend.position = "bottom",
        legend.box = "horizontal",  legend.key.size = unit(0.5, "cm"), 
        legend.margin = margin(t=0,r=0,b=0,l=0)) 

####Separating out nursery load 
nurUserLoad <- user_load1[,c(1:2,74)]
nurUserLoad <- spread(nurUserLoad, month, nurLoad)
nurUserLoad$Predicted <- nur_predicted$avgLoad
nurUserLoad <- gather(nurUserLoad, "id", "value", 2:8)
nurUserLoad$id2 <- factor(nurUserLoad$id, 
                            levels = c("Jul","Aug","Sep","Oct","Nov","Dec","Predicted"),
                            labels = c("Jul","Aug","Sep","Oct","Nov","Dec","Predicted"))
#For plot 1 - hourly avg to show typical load at nurseries
nurUserLoad %>%
  ggplot(aes(timestamp)) +
  geom_line(aes(y=value, color=id2, linetype=id2)) + 
  labs(title="Typical day socket and light load at the Nurseries (Micro-grid): 02 Jul'19 - 31st Dec'19",
       y="Energy consumption (Wh)",
       x = "Time of day (hours)",
       color="Month",
       linetype="Month") +
  scale_linetype_manual(values=c(2,3,4,5,6,7,1)) +
  scale_color_manual(values=c("darkgreen", "darkmagenta", "deepskyblue",
                              "darkorange","firebrick1","darkorchid","gray48"))+
  scale_x_continuous(labels=c("0","3","6","9","12","15","18","21","24"),
                     breaks = c(0,3,6,9,12,15,18,21,24), minor_breaks = 0:24) +
  theme(plot.title = element_text(size=10), legend.position = "bottom",
        legend.box = "horizontal",  legend.key.size = unit(0.5, "cm"), 
        legend.margin = margin(t=0,r=0,b=0,l=0)) 

###Separating out playground load 
pgUserLoad <- user_load1[,c(1:2,75)]
pgUserLoad <- spread(pgUserLoad, month, pgLoad)
pgUserLoad$Predicted <- pg_predicted$avgLoad
pgUserLoad <- gather(pgUserLoad, "id", "value", 2:8)
pgUserLoad$id2 <- factor(pgUserLoad$id, 
                          levels = c("Jul","Aug","Sep","Oct","Nov","Dec","Predicted"),
                          labels = c("Jul","Aug","Sep","Oct","Nov","Dec","Predicted"))
#For plot 1 - hourly avg to show typical load at playground
pgUserLoad %>%
  ggplot(aes(timestamp)) +
  geom_line(aes(y=value, color=id2, linetype=id2)) + 
  labs(title="Typical day socket and light load at the Playground (Micro-grid): 02 Jul'19 - 31st Dec'19",
       y="Energy consumption (Wh)",
       x = "Time of day (hours)",
       color="Month",
       linetype="Month") +
  scale_linetype_manual(values=c(2,3,4,5,6,7,1)) +
  scale_color_manual(values=c("darkgreen", "darkmagenta", "deepskyblue",
                              "darkorange","firebrick1","darkorchid","gray48"))+
  scale_x_continuous(labels=c("0","3","6","9","12","15","18","21","24"),
                     breaks = c(0,3,6,9,12,15,18,21,24), minor_breaks = 0:24) +
  theme(plot.title = element_text(size=10), legend.position = "bottom",
        legend.box = "horizontal",  legend.key.size = unit(0.5, "cm"), 
        legend.margin = margin(t=0,r=0,b=0,l=0)) 

###Separating out playground load 
acLoad <- user_load1[,c(1:3)]
acLoad <- spread(acLoad, month, AC_consumption_W)
acLoad$Predicted <- pg_predicted$avgLoad + nur_predicted$avgLoad
acLoad <- gather(acLoad, "id", "value", 2:8)
acLoad$id2 <- factor(acLoad$id, 
                         levels = c("Jul","Aug","Sep","Oct","Nov","Dec","Predicted"),
                         labels = c("Jul","Aug","Sep","Oct","Nov","Dec","Predicted"))
#For plot 1 - hourly avg to show typical AC consumption at microgrid
acLoad %>%
  ggplot(aes(timestamp)) +
  geom_line(aes(y=value, color=id2, linetype=id2)) + 
  labs(title="Typical day AC consumption at the Micro-grid: 02 Jul'19 - 31st Dec'19",
       y="Energy consumption (Wh)",
       x = "Time of day (hours)",
       color="Month",
       linetype="Month") +
  scale_linetype_manual(values=c(2,3,4,5,6,7,1)) +
  scale_color_manual(values=c("darkgreen", "darkmagenta", "deepskyblue",
                              "darkorange","firebrick1","darkorchid","gray48"))+
  scale_x_continuous(labels=c("0","3","6","9","12","15","18","21","24"),
                     breaks = c(0,3,6,9,12,15,18,21,24), minor_breaks = 0:24) +
  theme(plot.title = element_text(size=10), legend.position = "bottom",
        legend.box = "horizontal",  legend.key.size = unit(0.5, "cm"), 
        legend.margin = margin(t=0,r=0,b=0,l=0)) 

################################################################################
#For plot 2 - hourly avg to show typical data since commissioning (all sockets and lights)
mg_system_weather <- mg_data[,c("date","timestamp","AC_consumption_W", 
                                "Charged_energy_W","Discharged_energy_W",
                                "Pot_PV_power_W","PV_power_W","State_of_charge")]

#Remove data between 4th to 18th Sep (end dates inclusive)
mg_system_weather <- mg_system_weather[mg_system_weather$date<"2019-09-04" | 
                                         mg_system_weather$date>"2019-09-18",]

#Consider only hours where all values are available
mg_system_weather <- mg_system_weather[complete.cases(mg_system_weather),]
#Calculate capture loss = Pot PV - Actual PV
mg_system_weather$losses <- mg_system_weather$Pot_PV_power_W - 
  mg_system_weather$PV_power_W
mg_system_weather$month <- as.character(month(mg_system_weather$date, abbr=TRUE,
                                                label=TRUE))
mg_system <- gather(mg_system_weather, "id", "value", 3:9)
typical_mg_data <- mg_system %>%
  group_by(month, timestamp, id) %>%
  summarise(value = mean(value, na.rm=TRUE))
typical_mg_data <- as.data.frame(typical_mg_data)
typical_mg_data <- typical_mg_data[order(typical_mg_data$month),]
typical_mg_data <- spread(typical_mg_data, id, value)

###Plotting data for a typical day in each month
plotTypical <- function(df,p) {
  df %>%
    ggplot(aes(x=timestamp)) +
    geom_line(aes(y = AC_consumption_W/1000.0, color = "AC Load", group="AC Load"), linetype=1) + 
    geom_point(aes(y = AC_consumption_W/1000.0, color = "AC Load"), shape=1) + 
    geom_line(aes(y = PV_power_W/1000.0, color = "Actual PV Power", group="Actual PV Power"), linetype=2) + 
    geom_point(aes(y = PV_power_W/1000.0, color = "Actual PV Power"), shape=2) + 
    geom_line(aes(y = losses/1000.0, color = "Capture losses", group="Capture losses"), linetype=3) + 
    geom_point(aes(y = losses/1000.0, color = "Capture losses"), shape=3) + 
    geom_line(aes(y = Charged_energy_W/1000.0, color = "Charged Energy", group="Charged Energy"), linetype=4) + 
    geom_point(aes(y = Charged_energy_W/1000.0, color = "Charged Energy"), shape=4) + 
    geom_line(aes(y = Discharged_energy_W/1000.0, color = "Discharged Energy", group="Discharged Energy"), linetype=5) + 
    geom_point(aes(y = Discharged_energy_W/1000.0, color = "Discharged Energy"), shape=5) + 
    geom_line(aes(y = Pot_PV_power_W/1000.0, color = "Pot. PV Power", group="Pot. PV Power"), linetype=6) + 
    geom_point(aes(y = Pot_PV_power_W/1000.0, color = "Pot. PV Power"), shape=6) + 
    geom_line(aes(y = State_of_charge/p, color = "State of charge", group="State of charge"), linetype=7) + 
    geom_point(aes(y = State_of_charge/p, color = "State of charge"), shape=7) +
    scale_y_continuous(sec.axis = sec_axis(~.*p, name = "State of charge (%)")) +
    labs(y="Energy (kWh)",
         x = "Time of day (00-23 hours)", 
         colour="Parameter") +
    scale_x_continuous(labels=c("0","2","4","6","8","10","12","14","16","18",
                                "20","22","24"),
                       breaks=c(0,2,4,6,8,10,12,14,16,18,20,22,24)) +
    theme(plot.title = element_text(size=10), legend.position = "bottom",
          legend.box = "horizontal",  legend.key.size = unit(0.5, "cm"), 
          legend.margin = margin(t=0,r=0,b=0,l=0)) 
}

p <- plotTypical(typical_mg_data[typical_mg_data$month=="Jul", ], 55.0)
p + labs(title = "Actual Micro-grid power profile for a typical day in Jul 2019")

p <- plotTypical(typical_mg_data[typical_mg_data$month=="Aug", ], 50.0)
p + labs(title = "Actual Micro-grid power profile for a typical day in Aug 2019")

p <- plotTypical(typical_mg_data[typical_mg_data$month=="Sep", ], 50.0)
p + labs(title = "Actual Micro-grid power profile for a typical day in Sep 2019")

p <- plotTypical(typical_mg_data[typical_mg_data$month=="Oct", ], 60.0)
p + labs(title = "Actual Micro-grid power profile for a typical day in Oct 2019")

p <- plotTypical(typical_mg_data[typical_mg_data$month=="Nov", ], 60.0)
p + labs(title = "Actual Micro-grid power profile for a typical day in Nov 2019")

p <- plotTypical(typical_mg_data[typical_mg_data$month=="Dec", ], 60.0)
p + labs(title = "Actual Micro-grid power profile for a typical day in Dec 2019")

#########Combining data for first and next 3 months ####################
mg_system$month2 <- month(mg_system$date)
mg_system$phase <- ifelse(mg_system$month2<10, 1, 2)
typical_phase_mg <- mg_system %>%
  group_by(phase, timestamp, id) %>%
  summarise(value = mean(value, na.rm=TRUE))
typical_phase_mg <- as.data.frame(typical_phase_mg)
typical_phase_mg <- spread(typical_phase_mg, id, value)

p <- plotTypical(typical_phase_mg[typical_phase_mg$phase==1, ], 50.0)
p + labs(title = "Actual Micro-grid power profile for a typical day in Jul-Sep 2019")

p <- plotTypical(typical_phase_mg[typical_phase_mg$phase==2, ], 60.0)
p + labs(title = "Actual Micro-grid power profile for a typical day in Oct-Dec 2019")

####Combining all months #############################################
typical_mg <- mg_system %>%
  group_by(timestamp, id) %>%
  summarise(value = mean(value, na.rm=TRUE))
typical_mg <- as.data.frame(typical_mg)
typical_mg <- spread(typical_mg, id, value)

p <- plotTypical(typical_mg, 55.0)
p + labs(title = "Actual Micro-grid power profile for a typical day in Jul-Dec 2019")

###Plot 3 - total energy consumption per day since commissioning
##Calculating aux for hours that are complete
mg_users2 <- mg_data[,c("date","timestamp", "AC_consumption_W")]
#Remove data between 4th to 18th Sep (end dates inclusive)
mg_users2 <- mg_users2[mg_users2$date<"2019-09-04" | mg_users2$date>"2019-09-18",]
mg_users2 <- mg_users2[complete.cases(mg_users2), ]

complete_days <- mg_users2 %>%
  group_by(date) %>%
  summarise(len = length(na.omit(AC_consumption_W)))
complete_days <- as.data.frame(complete_days)
mg_users2 <- mg_users2[mg_users2$date %in% 
                             complete_days$date[which(complete_days$len==24)],]
#Daily consumption
mg_daily <- mg_users2 %>%
  group_by(date) %>%
  summarise(consumption = sum(AC_consumption_W))
startDate <- as.Date("2019-07-01")
mg_daily$days <- as.numeric(mg_daily$date - startDate)

mg_daily %>%
  ggplot(aes(x=days, y=consumption/1000.0, color="Actual")) +
  geom_point(shape=8) + 
  labs(title="Daily AC consumption at the Micro-grid since commissioning on 2nd Jul'19 to 31st Dec'19 " , 
       y="Total energy consumption (kWh)",
       x = "Days since commissioning",
       colour="") + 
  theme(plot.title = element_text(size=10),legend.position = "none") +
  scale_x_continuous(breaks = c(0,20,40,60,80,100,120,140,160,180),
                     labels=c("0","20","40","60","80","100","120","140","160","180")) +
  scale_y_continuous(limits = c(0.5,5.5))

#####Plot 4 - final yield, capture losses, system losses
#Yield = AC_consumption
#System loss = PV_power - Yield 
#Capture loss = Pot_PV_power - PV_power
mg_users3 <- mg_data[,c("date","timestamp","AC_consumption_W", 
                            "Pot_PV_power_W","PV_power_W")]
mg_users3 <- mg_users3[complete.cases(mg_users3) | 
             (mg_users3$date>="2019-09-04" & mg_users3$date<="2019-09-18"),]

complete_days <- mg_users3 %>%
  group_by(date) %>%
  summarise(len = length(timestamp))
complete_days <- as.data.frame(complete_days)
mg_users3 <- mg_users3[mg_users3$date %in% 
                             complete_days$date[which(complete_days$len==24)],]
#Daily consumption
mg_users3 <- gather(mg_users3, "id", "value", 3:5)
mg_perf <- mg_users3 %>%
  group_by(date, id) %>%
  summarise(value = sum(value,na.rm=TRUE))
mg_perf <- as.data.frame(mg_perf)
mg_perf <- spread(mg_perf, id, value)
startDate <- as.Date("2019-07-01")
mg_perf$days <- as.numeric(mg_perf$date - startDate)

#Capture losses
mg_perf$captureLoss <- mg_perf$Pot_PV_power_W - mg_perf$PV_power_W
mg_perf$systemLoss <- mg_perf$PV_power_W - mg_perf$AC_consumption_W

#Daily average per month
mg_perf <- gather(mg_perf, "id","value",c(2,3,4,6,7))
mg_perf$month <- as.character(month(mg_perf$date, abbr=TRUE, label=TRUE))
mg_monthly <- mg_perf %>%
  group_by(month, id) %>%
  summarise(value=mean(value))
mg_monthly <- as.data.frame(mg_monthly)
mg_monthly$month2 <- factor(mg_monthly$month, 
                              levels = c("Jul","Aug","Sep","Oct","Nov","Dec"),
                              labels = c("Jul","Aug","Sep","Oct","Nov","Dec"))
mg_monthly[mg_monthly$id!="Pot_PV_power_W" & mg_monthly$id!="PV_power_W", ] %>%
  ggplot(aes(x = month2, y= value/1000.0, fill = id)) +
  geom_bar(stat="identity", width=.5, position = "stack") + 
  labs(title="Daily average electrical energy values at the Micro-grid in 2019" ,
       y="Consumed and potential electrical energy (kWh)",
       x = "Month",
       fill="Parameter")  +
  scale_fill_manual(labels = c("Final yield", "Capture losses","System losses"), 
                    values = wes_palette("GrandBudapest1", n = 3)) +
  theme(plot.title = element_text(size=10), legend.position = "bottom",
        legend.box = "horizontal",  legend.key.size = unit(0.5, "cm"), 
        legend.margin = margin(t=0,r=0,b=0,l=0)) 

#####Plot 6 - to assess performance in comparison to the predicted behaviour
#Over and under prediction
mg_pred <- mg_data[,c("date","timestamp",
                        colnames(mg_data)[grepl("CPE",colnames(mg_data),fixed=TRUE) | 
                                            grepl("LVL",colnames(mg_data),fixed=TRUE)])]
#Remove data between 4th to 18th Sep (end dates inclusive)
mg_pred <- mg_pred[mg_pred$date<"2019-09-04" | mg_pred$date>"2019-09-18",]

#Total load at nurseries - select all CPE and sockets at Nur except Nur1AS1
test <- mg_pred[,c(3:8,10:46)]
mg_pred$nurLoad <- rowSums(test)

#Total load at playground
test <- mg_pred[,47:63]
mg_pred$pgLoad <- rowSums(test)

#Adding predicted columns for nursery and playground load - removed 4-18 Sep
mg_pred <- cbind(mg_pred, "nur_predicted"=rep(nur_predicted$avgLoad,168))
mg_pred <- cbind(mg_pred, "pg_predicted"=rep(pg_predicted$avgLoad,168))

#Separate nursery data
nur_pred <- mg_pred[,c(1,2,73,75)]
#Separate playground data
pg_pred <- mg_pred[,c(1,2,74,76)]
#Remove NA values
nur_pred <- nur_pred[complete.cases(nur_pred),]
pg_pred <- pg_pred[complete.cases(pg_pred),]

#Difference between Pred and actual for nursery data
nur_pred$diff <- nur_pred$nur_predicted - nur_pred$nurLoad
nur_pred$diff_range <- nur_pred$diff
summary(nur_pred$diff)
for(i in 1:length(nur_pred$date)) {
  if(abs(nur_pred$diff[i])<=50) {
    nur_pred$diff_range[i] = 1
  } else if(abs(nur_pred$diff[i])>=50 & abs(nur_pred$diff[i])<100) {
    nur_pred$diff_range[i] = 2
  } else if(abs(nur_pred$diff[i])>=100 & abs(nur_pred$diff[i])<150) {
    nur_pred$diff_range[i] = 3
  } else if(abs(nur_pred$diff[i])>=150 & abs(nur_pred$diff[i])<250) {
    nur_pred$diff_range[i] = 4
  } else if(abs(nur_pred$diff[i])>=250) {
    nur_pred$diff_range[i] = 5
  } 
}
#Rounding to the nearest 100th value
nur_pred$consumed_roundUp <- round(nur_pred$nurLoad, digits=-2)
nur_pred$diff_roundUp <- nur_pred$nur_predicted - nur_pred$consumed_roundUp

#Difference between Pred and actual for playground data
pg_pred$diff <- pg_pred$pg_predicted - pg_pred$pgLoad
pg_pred$diff_range <- pg_pred$diff
summary(pg_pred$diff)
for(i in 1:length(pg_pred$date)) {
  if(abs(pg_pred$diff[i])<=50) {
    pg_pred$diff_range[i] = 1
  } else if(abs(pg_pred$diff[i])>=50 & abs(pg_pred$diff[i])<100) {
    pg_pred$diff_range[i] = 2
  } else if(abs(pg_pred$diff[i])>=100 & abs(pg_pred$diff[i])<150) {
    pg_pred$diff_range[i] = 3
  } else if(abs(pg_pred$diff[i])>=150 & abs(pg_pred$diff[i])<250) {
    pg_pred$diff_range[i] = 4
  } else if(abs(pg_pred$diff[i])>=250) {
    pg_pred$diff_range[i] = 5
  } 
}
#Rounding to the nearest 100th value
pg_pred$consumed_roundUp <- round(pg_pred$pgLoad, digits=-2)
pg_pred$diff_roundUp <- pg_pred$pg_predicted - pg_pred$consumed_roundUp

##Plotting energy use over and under predictions for nursery data
nur_pred %>%
  ggplot(aes(diff_roundUp)) +
  geom_histogram(aes(y=(..count..)*100/sum(..count..)), binwidth=100,colour="black", fill="white") +
  labs(title="Distribution of energy-use over and under predictions at the Nurseries (Micro-grid): 02nd Jul'19 to 31st Dec'19" , 
       x = "Error (Wh)",
       y="Occurrences (%)") +
  theme(plot.title = element_text(size=8)) +
  geom_vline(aes(xintercept=median(diff_roundUp)),
             color="red") +
  geom_vline(aes(xintercept=quantile(diff_roundUp)[[2]]),
             color="blue", linetype="dashed") +
  geom_vline(aes(xintercept=quantile(diff_roundUp)[[4]]),
             color="blue", linetype="dashed") +
  geom_vline(aes(xintercept=quantile(diff_roundUp)[[1]]),
             color="darkgreen", linetype="dashed") +
  geom_vline(aes(xintercept=quantile(diff_roundUp)[[5]]),
             color="darkgreen", linetype="dashed")

##Plotting over and use across hours
nur_pred$diff_range <- as.factor(nur_pred$diff_range)
nur_pred %>%
  ggplot(aes(timestamp, diff, color=diff_range)) +
  geom_point() +
  geom_hline(aes(yintercept=0),
             color="black", linetype="dashed") +
  labs(title="Energy-use over and under predictions at the Nurseries (Micro-grid): 02nd Jul'19 to 31st Dec'19" , 
       x = "Time of day (00-23 hours)",
       y="Error (Wh)",
       color="Absolute error (Wh)") +
  scale_color_manual(labels = c("<50", ">=50 & <100",">=100 & <150",
                                ">=150 & <250",">=250"),
                     values = c("lightpink",wes_palette("GrandBudapest1", n = 4))) +
  theme(plot.title = element_text(size=9), legend.position = "bottom",
        legend.key.size = unit(0.38, "cm"), 
        legend.margin = margin(t=0,r=0,b=0,l=0)) 

##Plotting over and use across hours for each month
library(wesanderson)
nur_pred$month <- as.character(month(nur_pred$date, abbr=TRUE, label=TRUE))
nur_pred$month2 <- factor(nur_pred$month, 
                           levels = c("Jul","Aug","Sep","Oct","Nov","Dec"),
                           labels = c("Jul","Aug","Sep","Oct","Nov","Dec"))
nur_pred %>%
  ggplot(aes(timestamp, diff, color=month2, shape=month2)) +
  geom_point() +
  geom_hline(aes(yintercept=0),
             color="black", linetype="dashed") +
  labs(title="Energy-use over and under predictions at the Nurseries (Micro-grid): 02nd Jul'19 to 31st Dec'19" , 
       x = "Time of day (00-23 hours)",
       y="Error (Wh)",
       color="Month",
       shape="Month") +
  scale_color_manual(values = c("darkgreen", "darkmagenta", "deepskyblue",
                                "darkorange","firebrick1","darkorchid")) +
  scale_shape_manual(values=c(1,4,1,4,1,4)) + 
  theme(plot.title = element_text(size=9), legend.position = "bottom",
        legend.key.size = unit(0.38, "cm"), 
        legend.margin = margin(t=0,r=0,b=0,l=0)) 

############Plotting energy use over and under predictions for playground data
pg_pred %>%
  ggplot(aes(diff_roundUp)) +
  geom_histogram(aes(y=(..count..)*100/sum(..count..)), binwidth=100,colour="black", fill="white") +
  labs(title="Distribution of energy-use over and under predictions at the Playground (Micro-grid): 02nd Jul'19 to 31st Dec'19" , 
       x = "Error (Wh)",
       y="Occurrences (%)") +
  theme(plot.title = element_text(size=8)) +
  geom_vline(aes(xintercept=median(diff_roundUp)),
             color="red") +
  geom_vline(aes(xintercept=quantile(diff_roundUp)[[2]]),
             color="blue", linetype="dashed") +
  geom_vline(aes(xintercept=quantile(diff_roundUp)[[4]]),
             color="blue", linetype="dashed") +
  geom_vline(aes(xintercept=quantile(diff_roundUp)[[1]]),
             color="darkgreen", linetype="dashed") +
  geom_vline(aes(xintercept=quantile(diff_roundUp)[[5]]),
             color="darkgreen", linetype="dashed")

##Plotting over and use across hours
pg_pred$diff_range <- as.factor(pg_pred$diff_range)
pg_pred %>%
  ggplot(aes(timestamp, diff, color=diff_range)) +
  geom_point() +
  geom_hline(aes(yintercept=0),
             color="black", linetype="dashed") +
  labs(title="Energy-use over and under predictions at the Playground (Micro-grid): 02nd Jul'19 to 31st Dec'19" , 
       x = "Time of day (00-23 hours)",
       y="Error (Wh)",
       color="Absolute error (Wh)") +
  scale_color_manual(labels = c("<50", ">=50 & <100",">=100 & <150",
                                ">=150 & <250",">=250"),
                     values = c("lightpink",wes_palette("GrandBudapest1", n = 4))) +
  theme(plot.title = element_text(size=9), legend.position = "bottom",
        legend.key.size = unit(0.38, "cm"), 
        legend.margin = margin(t=0,r=0,b=0,l=0)) 

##Plotting over and use across hours for each month
library(wesanderson)
pg_pred$month <- as.character(month(pg_pred$date, abbr=TRUE, label=TRUE))
pg_pred$month2 <- factor(pg_pred$month, 
                         levels = c("Jul","Aug","Sep","Oct","Nov","Dec"),
                         labels = c("Jul","Aug","Sep","Oct","Nov","Dec"))
pg_pred %>%
  ggplot(aes(timestamp, diff, color=month2, shape=month2)) +
  geom_point() +
  geom_hline(aes(yintercept=0),
             color="black", linetype="dashed") +
  labs(title="Energy-use over and under predictions at the Playground (Micro-grid): 02nd Jul'19 to 31st Dec'19" , 
       x = "Time of day (00-23 hours)",
       y="Error (Wh)",
       color="Month",
       shape="Month") +
  scale_color_manual(values = c("darkgreen", "darkmagenta", "deepskyblue",
                                "darkorange","firebrick1","darkorchid")) +
  scale_shape_manual(values=c(1,4,1,4,1,4)) + 
  theme(plot.title = element_text(size=9), legend.position = "bottom",
        legend.key.size = unit(0.38, "cm"), 
        legend.margin = margin(t=0,r=0,b=0,l=0)) 

#######################################################################
#####Typical system load using predicted values#################################
setwd("~/OneDrive - Coventry University/HEED_analysis/Micro-grid/Data/System Predicted/")
systemData <- data.frame()
file_list <- list.files()
for(k in 1:length(file_list)) {
  #Read headers spread across 2 rows
  headers <- read_csv(file_list[k], col_names = FALSE, na="..", n_max = 2)
  headers[is.na(headers)] <- ""
  column_labels <- headers %>% summarize_all(str_c, collapse = " ")
  headers = unname(unlist(column_labels[1,]))
  
  system_data <- read_csv(file_list[k], col_names = headers, na="..", skip = 2)
  system_data[is.na(system_data)] <- "" 
  
  #Extract values Time, AC Primary Load kW, Actual PV power, 
  #Potential PV power output kW, Capture losses kW, Charge Power kW, 
  #Discharge Power kW, State of Charge
  
  #Extract AC Primary Load
  colNames <- headers[which(grepl("AC Primary Load", headers, fixed=TRUE))]
  #Extract Actual PV power
  colNames <- c(colNames, 
                headers[which(grepl("Actual PV power", headers, fixed=TRUE))])
  #Extract Potential PV Power Output
  colNames <- c(colNames, 
                headers[which(grepl("Potential PV Power Output", headers, fixed=TRUE))])
  #Extract Capture losses 
  colNames <- c(colNames, 
                headers[which(grepl("Capture losses", headers, fixed=TRUE))])
  #Extract Charge Power
  colNames <- c(colNames, 
                headers[which(grepl("Lead Acid Charge Power", headers, fixed=TRUE))])
  #Extract Discharge Power
  colNames <- c(colNames, 
                headers[which(grepl("Lead Acid Discharge Power", headers, fixed=TRUE))])
  #Extract State of Charge
  colNames <- c(colNames, 
                headers[which(grepl("State of Charge", headers, fixed=TRUE))])
  #Extract time
  colNames <- c(headers[1], colNames) 
  
  #Subset the system data
  df <- system_data[,colNames]
  
  #Time is in Africa zone but 12 hours behind
  colnames(df) <- c("timestamp","AC_consumption_kW","PV_power_kW",
                    "Potential_PV_power_kW", "Capture_losses_kW",
                    "Charged_energy_kW", "Discharged_energy_kW",
                    "State_of_charge")
  df$timestamp <- as.POSIXct(df$timestamp, tz="GMT", origin="1970-01-01", 
                             format="%d/%m/%Y %H:%M")
  df$timestamp <- df$timestamp %m+% years(12)
  systemData <- rbind(systemData, df)
}
#Trim time from 02 July
systemData$date <- date(systemData$timestamp)
systemData <- systemData[systemData$date>="2019-07-02",]
setwd("~/OneDrive - Coventry University/HEED_analysis/Micro-grid/")
write.csv(systemData, file="systemData_predicted_june_dec.csv", row.names=FALSE)

#####Read the predicted data
system_data <- read.csv("~/OneDrive - Coventry University/HEED_analysis/Micro-grid/systemData_predicted_june_dec.csv",
                        header=TRUE, stringsAsFactors = FALSE)
system_data$timestamp <- as.POSIXct(system_data$timestamp, format="%Y-%m-%d %H:%M")
system_data$date <- as.Date(system_data$date)
system_data$time <- format(system_data$timestamp, format='%H')
#Consider only hours where all values are available
system_data <- system_data[complete.cases(system_data),]
system_data$month <- as.character(month(system_data$date, abbr=TRUE,label=TRUE))
system_gather <- gather(system_data, "id", "value", 2:8)

typical_mg_pred <- system_gather %>%
  group_by(month, time, id) %>%
  summarise(value = mean(value, na.rm=TRUE))
typical_mg_pred <- as.data.frame(typical_mg_pred)
typical_mg_pred <- spread(typical_mg_pred, id, value)

###Plotting data for a typical day in each month
plotTypical <- function(df,p) {
  df %>%
    ggplot(aes(x=as.numeric(time))) +
    geom_line(aes(y = AC_consumption_kW, color = "AC Load", group="AC Load"), linetype=1) + 
    geom_point(aes(y = AC_consumption_kW, color = "AC Load"), shape=1) + 
    geom_line(aes(y = PV_power_kW, color = "Actual PV Power", group="Actual PV Power"), linetype=2) + 
    geom_point(aes(y = PV_power_kW, color = "Actual PV Power"), shape=2) + 
    geom_line(aes(y = Capture_losses_kW, color = "Capture losses", group="Capture losses"), linetype=3) + 
    geom_point(aes(y = Capture_losses_kW, color = "Capture losses"), shape=3) + 
    geom_line(aes(y = Charged_energy_kW, color = "Charged Energy", group="Charged Energy"), linetype=4) + 
    geom_point(aes(y = Charged_energy_kW, color = "Charged Energy"), shape=4) + 
    geom_line(aes(y = Discharged_energy_kW, color = "Discharged Energy", group="Discharged Energy"), linetype=5) + 
    geom_point(aes(y = Discharged_energy_kW, color = "Discharged Energy"), shape=5) + 
    geom_line(aes(y = Potential_PV_power_kW, color = "Pot. PV Power", group="Pot. PV Power"), linetype=6) + 
    geom_point(aes(y = Potential_PV_power_kW, color = "Pot. PV Power"), shape=6) + 
    geom_line(aes(y = State_of_charge/p, color = "State of charge", group="State of charge"), linetype=7) + 
    geom_point(aes(y = State_of_charge/p, color = "State of charge"), shape=7) +
    scale_y_continuous(sec.axis = sec_axis(~.*p, name = "State of charge (%)")) +
    labs(y="Energy (kWh)",
         x = "Time of day (00-23 hours)", 
         colour="Parameter") +
    scale_x_continuous(labels=c("0","2","4","6","8","10","12","14","16","18",
                                "20","22","24"),
                       breaks=c(0,2,4,6,8,10,12,14,16,18,20,22,24)) +
    theme(plot.title = element_text(size=10), legend.position = "bottom",
          legend.box = "horizontal",  legend.key.size = unit(0.5, "cm"), 
          legend.margin = margin(t=0,r=0,b=0,l=0)) 
}

p <- plotTypical(typical_mg_pred[typical_mg_pred$month=="Jul", ], 55.0)
p + labs(title = "Predicted Micro-grid power profile for a typical day in Jul 2019")

p <- plotTypical(typical_mg_pred[typical_mg_pred$month=="Aug", ], 50.0)
p + labs(title = "Predicted Micro-grid power profile for a typical day in Aug 2019")

p <- plotTypical(typical_mg_pred[typical_mg_pred$month=="Sep", ], 50.0)
p + labs(title = "Predicted Micro-grid power profile for a typical day in Sep 2019")

p <- plotTypical(typical_mg_pred[typical_mg_pred$month=="Oct", ], 60.0)
p + labs(title = "Predicted Micro-grid power profile for a typical day in Oct 2019")

p <- plotTypical(typical_mg_pred[typical_mg_pred$month=="Nov", ], 60.0)
p + labs(title = "Predicted Micro-grid power profile for a typical day in Nov 2019")

p <- plotTypical(typical_mg_pred[typical_mg_pred$month=="Dec", ], 60.0)
p + labs(title = "Predicted Micro-grid power profile for a typical day in Dec 2019")

#########Combining data for first and next 3 months ####################
system_gather$month2 <- month(system_gather$date)
system_gather$phase <- ifelse(system_gather$month2<10, 1, 2)
typical_phase_pred <- system_gather %>%
  group_by(phase, time, id) %>%
  summarise(value = mean(value, na.rm=TRUE))
typical_phase_pred <- as.data.frame(typical_phase_pred)
typical_phase_pred <- spread(typical_phase_pred, id, value)

p <- plotTypical(typical_phase_pred[typical_phase_pred$phase==1, ], 50.0)
p + labs(title = "Predicted Micro-grid power profile for a typical day in Jul-Sep 2019")

p <- plotTypical(typical_phase_pred[typical_phase_pred$phase==2, ], 50.0)
p + labs(title = "Predicted Micro-grid power profile for a typical day in Oct-Dec 2019")

####Combining all months #############################################
typical_mg <- system_gather %>%
  group_by(time, id) %>%
  summarise(value = mean(value, na.rm=TRUE))
typical_mg <- as.data.frame(typical_mg)
typical_mg <- spread(typical_mg, id, value)

p <- plotTypical(typical_mg, 55.0)
p + labs(title = "Predicted Micro-grid power profile for a typical day in Jul-Dec 2019")

########################################################################
# Calculating AC load and user load values for Mesh Power
library(tidyverse)
library(lubridate)
library(readxl)
library(rlang)
setwd("~/OneDrive - Coventry University/HEED_analysis/Micro-grid/")
mg_data <- read.csv("MG_data_hourly_jul_dec.csv", header=TRUE)
mg_data$date <- as.Date(mg_data$date)
mg_data <- mg_data[,c(1:3,6:11,13,15:21,23:29,31:37,39,41:47,49:55,57:72,74,76,80:88)]
# Sum all sockets, CPE and streetlights except Nur1AS1
# Remove Nur1AS1
mg_data <- mg_data[,-10]
mg_data$User_Load_SL_W <- rowSums(mg_data[,c(4:72)])
mg_data$User_Load_W <- rowSums(mg_data[,c(4:63)])
# Only keep complete cases
mg_data <- mg_data[complete.cases(mg_data),]
mg_data$timestamp <- ifelse(mg_data$timestamp<10, paste(mg_data$date, paste("0",mg_data$timestamp,":00:00",sep="")), 
                       paste(mg_data$date,paste(mg_data$timestamp,":00:00", sep=""), sep=" "))
mg_data <- mg_data[,-1] #remove date
write.csv(mg_data,"./MG_hourly_load_jul_dec.csv", row.names=FALSE)
