#******************************************************************************************#
# This is the script for imputing missing data for all Community Hall                      #
# Author: K Bhargava                                                                       #
# Last updated on: 17th July 2020                                                          #
#******************************************************************************************#

#******************************************************************************************#
# Importing libraries
library(tidyverse)
library(lubridate)
library(wesanderson)
library("imputeTS") # for na_seadec imputation
library(xts) # for converting data into time series
library(timeDate)
library(here)
#******************************************************************************************#

#******************************************************************************************#
# Set working directory 
filepath <- "Data"
plot_dir <- "Plots/Paper 5"
MONTHS <- c("Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar")
#******************************************************************************************#

#******************************************************************************************#
# Read hourly data 
mg_hourly <- read.csv(here(filepath,"hourly_data_jul19_to_mar20.csv"), header = TRUE, 
                      stringsAsFactors=FALSE)
mg_hourly <- mg_hourly %>% mutate(date = as.Date(date))
#******************************************************************************************#

#******************************************************************************************#
# Imputation using na_seadec owing to seasonality - works on univariate time series
methodImpute <- c("ma")
# Impute missing values for all variables
variables <- c("Battery.Monitor.Charged.Energy", "Battery.Monitor.Discharged.Energy",  
               "Battery.Monitor.State.of.charge..", "Battery.Monitor.Voltage.V", 
               "Solar.Charger.Battery.watts.W", "Solar.Charger.PV.power", 
               "System.overview.AC.Consumption.L1.W", "System.overview.Battery.Power.W",
               "Nur.1A.CPE1.LED1_P", "Nur.1A.CPE1.LED2_P", "Nur.1A.CPE1.LED3_P", "Nur.1A.CPE2.LED1_P", 
               "Nur.1A.CPE2.LED2_P", "Nur.1A.CPE2.LED3_P", "Nur.1A.S1.AC_Day_Energy_session", 
               "Nur.1A.S1.vRELAY1_LVL", "Nur.1A.S2.AC_Day_Energy_session", 
               "Nur.1A.S2.vRELAY1_LVL", "Nur.1B.CPE3.LED1_P", "Nur.1B.CPE3.LED2_P", 
               "Nur.1B.CPE3.LED3_P", "Nur.1B.CPE4.LED1_P", "Nur.1B.CPE4.LED2_P", 
               "Nur.1B.CPE4.LED3_P", "Nur.1B.S1.AC_Day_Energy_session", 
               "Nur.1B.S1.vRELAY1_LVL", "Nur.1C.CPE5.LED1_P", "Nur.1C.CPE5.LED2_P", 
               "Nur.1C.CPE5.LED3_P", "Nur.1C.CPE6.LED1_P", "Nur.1C.CPE6.LED2_P", 
               "Nur.1C.CPE6.LED3_P", "Nur.1C.S1.AC_Day_Energy_session", 
               "Nur.1C.S1.vRELAY1_LVL", "Nur.2A.CPE7.LED1_P", "Nur.2A.CPE7.LED2_P", 
               "Nur.2A.CPE7.LED3_P", "Nur.2A.CPE8.LED1_P", "Nur.2A.CPE8.LED2_P", 
               "Nur.2A.CPE8.LED3_P", "Nur.2A.S1.AC_Day_Energy_session", 
               "Nur.2A.S1.vRELAY1_LVL", "Nur.2A.S2.AC_Day_Energy_session", 
               "Nur.2A.S2.vRELAY1_LVL", "Nur.2B.CPE10.LED1_P", "Nur.2B.CPE10.LED2_P", 
               "Nur.2B.CPE10.LED3_P", "Nur.2B.CPE9.LED1_P", "Nur.2B.CPE9.LED2_P", 
               "Nur.2B.CPE9.LED3_P", "Nur.2B.S1.AC_Day_Energy_session", 
               "Nur.2B.S1.vRELAY1_LVL", "Nur.2C.CPE11.LED1_P", "Nur.2C.CPE11.LED2_P", 
               "Nur.2C.CPE11.LED3_P", "Nur.2C.CPE12.LED1_P" , "Nur.2C.CPE12.LED2_P", 
               "Nur.2C.CPE12.LED3_P", "Nur.2C.S1.AC_Day_Energy_session", 
               "Nur.2C.S1.vRELAY1_LVL", "Playground.CPE1.LED1_P", "Playground.CPE1.LED2_P", 
               "Playground.CPE1.LED3_P", "Playground.CPE2.LED1_P", "Playground.CPE2.LED2_P", 
               "Playground.CPE2.LED3_P", "Playground.CPE3.LED1_P", "Playground.CPE3.LED2_P", 
               "Playground.CPE3.LED3_P", "Playground.CPE4.LED1_P", "Playground.CPE4.LED2_P", 
               "Playground.CPE4.LED3_P", "Playground.CPE5.LED1_P", "Playground.CPE5.LED2_P", 
               "Playground.CPE5.LED3_P", "Playground.S1.AC_Day_Energy_session", 
               "Playground.S1.vRELAY1_LVL", "Playground.S2.AC_Day_Energy_session", 
               "Playground.S2.vRELAY1_LVL", "Streetlight.1.CPE.LED1_P", 
               "Streetlight.1.CPE.LED2_P", "Streetlight.1.CPE.LED3_P", 
               "Streetlight.2.CPE.LED1_P", "Streetlight.2.CPE.LED2_P", 
               "Streetlight.2.CPE.LED3_P", "Streetlight.3.CPE.LED1_P", 
               "Streetlight.3.CPE.LED2_P", "Streetlight.3.CPE.LED3_P")
na_seadec_imputedData <- data.frame()
for(k in seq_along(variables)) {
  df <- mg_hourly[c("date","timeUse",variables[k])]
  # Convert data frame into a time series using xts to serve as input to na_seadec function
  # For this data, seasonality is 1 day with a reading every hour
  df.ts <- spread(df, timeUse, variables[k])
  df.ts <- xts(df.ts[,-1], order.by=as.Date(df.ts[,1], "%Y-%m-%d"))
  
  # Impute data using different functions of na_seadec and bind to df
  for(j in seq_along(methodImpute)) {
    df1 <- as.data.frame(na_seadec(df.ts, algorithm=methodImpute[j], find_frequency=TRUE))
    df1 <- df1 %>% mutate(date=row.names(df1))
    df1 <- gather(df1, "timeUse", "value", 1:24)
    df1[is.na(df1)] <- 0
    df1 <- df1[order(df1$date),]
    df <- cbind(df, df1$value)
  }
  colnames(df) <- c(colnames(df)[1:2],paste(variables[k],"original",sep="_"),
                    paste(variables[k],methodImpute,sep="_"))
  df <- gather(df, "variable","value",3:4)
  
  # Bind data for all SL
  na_seadec_imputedData <- rbind(na_seadec_imputedData, df)
}
na_seadec_imputedData <- spread(na_seadec_imputedData, variable, value)
na_seadec_imputedData <- na_seadec_imputedData %>% 
  mutate(Potential.PV.power.W = mg_hourly$Potential_PV_power_W,
         month = as.character(lubridate::month(date, label=TRUE, abbr=TRUE)),
         timestamp = as.POSIXct(paste(date, ifelse(timeUse<10, paste("0",timeUse,":00:00",sep=""), 
                                                   paste(timeUse,":00:00",sep="")), sep=" "), origin="1970-01-01",tz="GMT"))
write.csv(na_seadec_imputedData, file=here(filepath,"na_seadec_ma_data.csv"), row.names=FALSE)
#******************************************************************************************#

#******************************************************************************************#
#Read in data files and plot data
na_seadec_imputedData <- read.csv(here(filepath,"na_seadec_ma_data.csv"), header=TRUE, 
                                  stringsAsFactors = FALSE)
na_seadec_imputedData <- na_seadec_imputedData %>% 
  mutate(date=as.Date(date),timestamp=as.POSIXct(timestamp,tz="GMT",origin="1970-01-01"),
         month = factor(month, levels = MONTHS, labels = MONTHS))
na_seadec_sub <- gather(na_seadec_imputedData, "id", "value", c(3:179))

# Plot data for all variables to visualise original and imputed data
plotHourly <- function(df) {
  ggplot(df, aes(timestamp, value)) + facet_wrap(~id, scales = "free", ncol=2) + 
    geom_line(linetype=3) + theme(legend.position = "bottom") + 
    labs(x="Timestamp", y="Energy (Wh)", color="Variable", linetype="Variable") +
    theme(title=element_text(size=10))
}
# Plot system data
plotHourly(na_seadec_sub[na_seadec_sub$id %in% unique(na_seadec_sub$id)[c(1:6)],]) +
  labs(title="Original and imputed Charged and discharged energy and SoC")
ggsave(here(plot_dir,"imputed_soc.png"))
plotHourly(na_seadec_sub[na_seadec_sub$id %in% unique(na_seadec_sub$id)[c(153:154,173:174)],]) +
  labs(title="Original and imputed PV power and AC load data")
ggsave(here(plot_dir,"imputed_pv_ac.png"))
plotHourly(na_seadec_sub[na_seadec_sub$id %in% unique(na_seadec_sub$id)[c(151:152,175:176)],]) +
  labs(title="Original and imputed solar battery power and system battery power")
ggsave(here(plot_dir,"imputed_bp.png"))

# Plot CPE data
plotHourly(na_seadec_sub[na_seadec_sub$id %in% unique(na_seadec_sub$id)[c(9:14)],]) +
  labs(title="Original and imputed CPE 1 data")
ggsave(here(plot_dir,"imputed_cpe1.png"))
plotHourly(na_seadec_sub[na_seadec_sub$id %in% unique(na_seadec_sub$id)[c(15:20)],]) +
  labs(title="Original and imputed CPE 2 data")
ggsave(here(plot_dir,"imputed_cpe2.png"))
plotHourly(na_seadec_sub[na_seadec_sub$id %in% unique(na_seadec_sub$id)[c(29:34)],]) +
  labs(title="Original and imputed CPE 3 data")
ggsave(here(plot_dir,"imputed_cpe3.png"))
plotHourly(na_seadec_sub[na_seadec_sub$id %in% unique(na_seadec_sub$id)[c(35:40)],]) +
  labs(title="Original and imputed CPE 4 data")
ggsave(here(plot_dir,"imputed_cpe4.png"))
plotHourly(na_seadec_sub[na_seadec_sub$id %in% unique(na_seadec_sub$id)[c(45:50)],]) +
  labs(title="Original and imputed CPE 5 data")
ggsave(here(plot_dir,"imputed_cpe5.png"))
plotHourly(na_seadec_sub[na_seadec_sub$id %in% unique(na_seadec_sub$id)[c(51:56)],]) +
  labs(title="Original and imputed CPE 6 data")
ggsave(here(plot_dir,"imputed_cpe6.png"))
plotHourly(na_seadec_sub[na_seadec_sub$id %in% unique(na_seadec_sub$id)[c(61:66)],]) +
  labs(title="Original and imputed CPE 7 data")
ggsave(here(plot_dir,"imputed_cpe7.png"))
plotHourly(na_seadec_sub[na_seadec_sub$id %in% unique(na_seadec_sub$id)[c(67:72)],]) +
  labs(title="Original and imputed CPE 8 data")
ggsave(here(plot_dir,"imputed_cpe8.png"))
plotHourly(na_seadec_sub[na_seadec_sub$id %in% unique(na_seadec_sub$id)[c(87:92)],]) +
  labs(title="Original and imputed CPE 9 data")
ggsave(here(plot_dir,"imputed_cpe9.png"))
plotHourly(na_seadec_sub[na_seadec_sub$id %in% unique(na_seadec_sub$id)[c(81:86)],]) +
  labs(title="Original and imputed CPE 10 data")
ggsave(here(plot_dir,"imputed_cpe10.png"))
plotHourly(na_seadec_sub[na_seadec_sub$id %in% unique(na_seadec_sub$id)[c(97:102)],]) +
  labs(title="Original and imputed CPE 11 data")
ggsave(here(plot_dir,"imputed_cpe11.png"))
plotHourly(na_seadec_sub[na_seadec_sub$id %in% unique(na_seadec_sub$id)[c(103:108)],]) +
  labs(title="Original and imputed CPE 12 data")
ggsave(here(plot_dir,"imputed_cpe12.png"))
plotHourly(na_seadec_sub[na_seadec_sub$id %in% unique(na_seadec_sub$id)[c(113:118)],]) +
  labs(title="Original and imputed Playground CPE 1 data")
ggsave(here(plot_dir,"imputed_cpe13.png"))
plotHourly(na_seadec_sub[na_seadec_sub$id %in% unique(na_seadec_sub$id)[c(119:124)],]) +
  labs(title="Original and imputed Playground CPE 2 data")
ggsave(here(plot_dir,"imputed_cpe14.png"))
plotHourly(na_seadec_sub[na_seadec_sub$id %in% unique(na_seadec_sub$id)[c(125:130)],]) +
  labs(title="Original and imputed Playground CPE 3 data")
ggsave(here(plot_dir,"imputed_cpe15.png"))
plotHourly(na_seadec_sub[na_seadec_sub$id %in% unique(na_seadec_sub$id)[c(131:136)],]) +
  labs(title="Original and imputed Playground CPE 4 data")
ggsave(here(plot_dir,"imputed_cpe16.png"))
plotHourly(na_seadec_sub[na_seadec_sub$id %in% unique(na_seadec_sub$id)[c(137:142)],]) +
  labs(title="Original and imputed Playground CPE 5 data")
ggsave(here(plot_dir,"imputed_cpe17.png"))
plotHourly(na_seadec_sub[na_seadec_sub$id %in% unique(na_seadec_sub$id)[c(155:160)],]) +
  labs(title="Original and imputed SL CPE 1 data")
ggsave(here(plot_dir,"imputed_cpe18.png"))
plotHourly(na_seadec_sub[na_seadec_sub$id %in% unique(na_seadec_sub$id)[c(161:166)],]) +
  labs(title="Original and imputed SL CPE 2 data")
ggsave(here(plot_dir,"imputed_cpe19.png"))
plotHourly(na_seadec_sub[na_seadec_sub$id %in% unique(na_seadec_sub$id)[c(167:172)],]) +
  labs(title="Original and imputed SL CPE 3 data")
ggsave(here(plot_dir,"imputed_cpe20.png"))

# Plot socket data
plotHourly(na_seadec_sub[na_seadec_sub$id %in% unique(na_seadec_sub$id)[c(21:24)],]) +
  labs(title="Original and imputed nur 1A S1 data")
ggsave(here(plot_dir,"imputed_s1.png"))
plotHourly(na_seadec_sub[na_seadec_sub$id %in% unique(na_seadec_sub$id)[c(25:28)],]) +
  labs(title="Original and imputed nur 1A S2 data")
ggsave(here(plot_dir,"imputed_s2.png"))
plotHourly(na_seadec_sub[na_seadec_sub$id %in% unique(na_seadec_sub$id)[c(41:44)],]) +
  labs(title="Original and imputed nur 1B S1 data")
ggsave(here(plot_dir,"imputed_s3.png"))
plotHourly(na_seadec_sub[na_seadec_sub$id %in% unique(na_seadec_sub$id)[c(57:60)],]) +
  labs(title="Original and imputed nur 1C S1 data")
ggsave(here(plot_dir,"imputed_s4.png"))
plotHourly(na_seadec_sub[na_seadec_sub$id %in% unique(na_seadec_sub$id)[c(73:76)],]) +
  labs(title="Original and imputed nur 2A S1 data")
ggsave(here(plot_dir,"imputed_s5.png"))
plotHourly(na_seadec_sub[na_seadec_sub$id %in% unique(na_seadec_sub$id)[c(77:80)],]) +
  labs(title="Original and imputed nur 2A S2 data")
ggsave(here(plot_dir,"imputed_s6.png"))
plotHourly(na_seadec_sub[na_seadec_sub$id %in% unique(na_seadec_sub$id)[c(93:96)],]) +
  labs(title="Original and imputed nur 2B S1 data")
ggsave(here(plot_dir,"imputed_s7.png"))
plotHourly(na_seadec_sub[na_seadec_sub$id %in% unique(na_seadec_sub$id)[c(109:112)],]) +
  labs(title="Original and imputed nur 2C S1 data")
ggsave(here(plot_dir,"imputed_s8.png"))
plotHourly(na_seadec_sub[na_seadec_sub$id %in% unique(na_seadec_sub$id)[c(143:146)],]) +
  labs(title="Original and imputed Playground S1 data")
ggsave(here(plot_dir,"imputed_s9.png"))
plotHourly(na_seadec_sub[na_seadec_sub$id %in% unique(na_seadec_sub$id)[c(147:150)],]) +
  labs(title="Original and imputed Playground S2 data")
ggsave(here(plot_dir,"imputed_s10.png"))
#******************************************************************************************#

#******************************************************************************************#
# Compute statistics for original and imputed data
stats_na_seadec_sub <- na_seadec_sub %>% group_by(id) %>%
  summarise(mean = mean(value, na.rm=TRUE), median = median(value, na.rm=TRUE), 
            sd = sd(value, na.rm=TRUE), skew = skewness(value, na.rm=TRUE), 
            kurt = kurtosis(value, na.rm=TRUE))
stats_na_seadec_sub <- as.data.frame(stats_na_seadec_sub)  
stats_na_seadec_sub <- stats_na_seadec_sub[complete.cases(stats_na_seadec_sub),]
# Save the statistics file
write.csv(stats_na_seadec_sub, file=here(filepath,"stats_na_seadec.csv"), row.names=FALSE)
#******************************************************************************************#

#******************************************************************************************#
#* Correct data: replace negative values with zero for all variables except System Battery Power
# Compare value ranges of original and imputed data 
value_ranges <- na_seadec_sub %>% group_by(id) %>% 
  summarise(minValue=min(value,na.rm=TRUE), maxValue=max(value,na.rm=TRUE))

na_seadec_sub[!(na_seadec_sub$id=="System.overview.Battery.Power.W_ma" | 
                na_seadec_sub$id=="System.overview.Battery.Power.W_original"),] <- 
  na_seadec_sub[!(na_seadec_sub$id=="System.overview.Battery.Power.W_ma" | 
                    na_seadec_sub$id=="System.overview.Battery.Power.W_original"),] %>%
  mutate(value=ifelse(value<0,0,value))
na_seadec_correctedData <- spread(na_seadec_sub, id, value)

# Correct SoC, calculate +ve/-ve battery power, +ve/-ve solar battery power
na_seadec_correctedData <- na_seadec_correctedData %>% 
  mutate(Battery.Monitor.State.of.charge.._ma=ifelse(Battery.Monitor.State.of.charge.._ma>100,100,
                                                     Battery.Monitor.State.of.charge.._ma),
         Positive.Solar.Battery.Power_ma=ifelse(Solar.Charger.Battery.watts.W_ma<0,0,
                                                Solar.Charger.Battery.watts.W_ma),
         Negative.Solar.Battery.Power_ma=ifelse(Solar.Charger.Battery.watts.W_ma>0,0,
                                                Solar.Charger.Battery.watts.W_ma),
         Positive.Solar.Battery.Power_original=ifelse(Solar.Charger.Battery.watts.W_original<0,0,
                                                      Solar.Charger.Battery.watts.W_original),
         Negative.Solar.Battery.Power_original=ifelse(Solar.Charger.Battery.watts.W_original>0,0,
                                                      Solar.Charger.Battery.watts.W_original),
         Positive.System.Battery.Power_ma=ifelse(System.overview.Battery.Power.W_ma<0,0,
                                                 System.overview.Battery.Power.W_ma),
         Negative.System.Battery.Power_ma=ifelse(System.overview.Battery.Power.W_ma>0,0,
                                                 System.overview.Battery.Power.W_ma),
         Positive.System.Battery.Power_original=ifelse(System.overview.Battery.Power.W_original<0,0,
                                                       System.overview.Battery.Power.W_original),
         Negative.System.Battery.Power_original=ifelse(System.overview.Battery.Power.W_original>0,0,
                                                       System.overview.Battery.Power.W_original))
#******************************************************************************************#

#******************************************************************************************#
# Based on SoC, calculate actual PV power, Solar Battery power, AC consumption,
# System Battery power, +ve/-ve actual solar battery power, +ve/-ve actual battery power
# Cut off voltage for SoC: 
na_seadec_correctedData <- na_seadec_correctedData %>%
  mutate(Actual.PV.power.W_ma=ifelse((Battery.Monitor.State.of.charge.._ma<=40 &
                                        is.na(Battery.Monitor.State.of.charge.._original)), 0, 
                                     Solar.Charger.PV.power_ma),
         Actual.Solar.Charger.Battery.Power_ma=ifelse((Battery.Monitor.State.of.charge.._ma<=40 & 
                                                         is.na(Solar.Charger.Battery.watts.W_original)),0,
                                                      Solar.Charger.Battery.watts.W_ma),
         Actual.AC.consumption_ma=ifelse((Battery.Monitor.State.of.charge.._ma<=40 & 
                                            is.na(System.overview.AC.Consumption.L1.W_original)),0,
                                         System.overview.AC.Consumption.L1.W_ma),
         Actual.System.Battery.Power_ma=ifelse((Battery.Monitor.State.of.charge.._ma<=40 &
                                                  is.na(System.overview.Battery.Power.W_original)), 0,
                                               System.overview.Battery.Power.W_ma),
         Positive.Actual.Solar.Charger.Battery.Power_ma=ifelse(Actual.Solar.Charger.Battery.Power_ma<0,0,
                                                               Actual.Solar.Charger.Battery.Power_ma),
         Negative.Actual.Solar.Charger.Battery.Power_ma=ifelse(Actual.Solar.Charger.Battery.Power_ma>0,0,
                                                               Actual.Solar.Charger.Battery.Power_ma),
         Positive.Actual.System.Battery.Power_ma=ifelse(Actual.System.Battery.Power_ma<0,0,
                                                        Actual.System.Battery.Power_ma),
         Negative.Actual.System.Battery.Power_ma=ifelse(Actual.System.Battery.Power_ma>0,0,
                                                        Actual.System.Battery.Power_ma))


# Change the actual load values to zero between 4th and 18th of Sep - check if missing

na_seadec_correctedData <- na_seadec_correctedData %>% 
  mutate()
#******************************************************************************************#

# Save corrected data
na_seadec_correctedData <- na_seadec_correctedData %>% mutate(month=as.character(month))
write.csv(na_seadec_correctedData, file=here(filepath,"na_seadec_correctedData.csv"), row.names=FALSE)
#******************************************************************************************#