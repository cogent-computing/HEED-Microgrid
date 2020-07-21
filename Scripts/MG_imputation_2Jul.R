#******************************************************************************************#
# This is the script for imputing missing data for all Community Hall                      #
# Author: K Bhargava                                                                       #
# Last updated on: 21st July 2020                                                          #
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
variables <- colnames(mg_hourly)[3:90]
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

# Plot data for all variables to visualize original and imputed data
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

# Correct SoC, and calculate +ve/-ve battery power and +ve/-ve solar battery power
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
# Estimate power outages based on SoC - cut off at 60% SOC for CPE and 40% for others taken from Hall data
# Based on SoC, calculate actual PV power, Solar Battery power, AC consumption,
# System Battery power, +ve/-ve actual solar battery power, +ve/-ve actual battery power
socThresh <- 40
cpeThresh <- 60
na_seadec_correctedData <- na_seadec_correctedData %>%
  mutate(Actual.Nur.1A.CPE1.LED1_P_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=cpeThresh & 
                                               is.na(Nur.1A.CPE1.LED1_P_original),0,Nur.1A.CPE1.LED1_P_ma),
         Actual.Nur.1A.CPE1.LED2_P_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=cpeThresh & 
                                               is.na(Nur.1A.CPE1.LED2_P_original),0,Nur.1A.CPE1.LED2_P_ma),
         Actual.Nur.1A.CPE1.LED3_P_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=cpeThresh & 
                                               is.na(Nur.1A.CPE1.LED3_P_original),0,Nur.1A.CPE1.LED3_P_ma),
         Actual.Nur.1A.CPE2.LED1_P_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=cpeThresh & 
                                               is.na(Nur.1A.CPE2.LED1_P_original),0,Nur.1A.CPE2.LED1_P_ma),
         Actual.Nur.1A.CPE2.LED2_P_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=cpeThresh & 
                                               is.na(Nur.1A.CPE2.LED2_P_original),0,Nur.1A.CPE2.LED2_P_ma),
         Actual.Nur.1A.CPE2.LED3_P_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=cpeThresh & 
                                               is.na(Nur.1A.CPE2.LED3_P_original),0,Nur.1A.CPE2.LED3_P_ma),
         Actual.Nur.1B.CPE3.LED1_P_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=cpeThresh & 
                                               is.na(Nur.1B.CPE3.LED1_P_original),0,Nur.1B.CPE3.LED1_P_ma),
         Actual.Nur.1B.CPE3.LED2_P_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=cpeThresh & 
                                               is.na(Nur.1B.CPE3.LED2_P_original),0,Nur.1B.CPE3.LED2_P_ma),
         Actual.Nur.1B.CPE3.LED3_P_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=cpeThresh & 
                                               is.na(Nur.1B.CPE3.LED3_P_original),0,Nur.1B.CPE3.LED3_P_ma),
         Actual.Nur.1B.CPE4.LED1_P_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=cpeThresh & 
                                               is.na(Nur.1B.CPE4.LED1_P_original),0,Nur.1B.CPE4.LED1_P_ma),
         Actual.Nur.1B.CPE4.LED2_P_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=cpeThresh & 
                                               is.na(Nur.1B.CPE4.LED2_P_original),0,Nur.1B.CPE4.LED2_P_ma),
         Actual.Nur.1B.CPE4.LED3_P_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=cpeThresh & 
                                               is.na(Nur.1B.CPE4.LED3_P_original),0,Nur.1B.CPE4.LED3_P_ma),
         Actual.Nur.1C.CPE5.LED1_P_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=cpeThresh & 
                                               is.na(Nur.1C.CPE5.LED1_P_original),0,Nur.1C.CPE5.LED1_P_ma),
         Actual.Nur.1C.CPE5.LED2_P_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=cpeThresh & 
                                               is.na(Nur.1C.CPE5.LED2_P_original),0,Nur.1C.CPE5.LED2_P_ma),
         Actual.Nur.1C.CPE5.LED3_P_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=cpeThresh & 
                                               is.na(Nur.1C.CPE5.LED3_P_original),0,Nur.1C.CPE5.LED3_P_ma),
         Actual.Nur.1C.CPE6.LED1_P_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=cpeThresh & 
                                               is.na(Nur.1C.CPE6.LED1_P_original),0,Nur.1C.CPE6.LED1_P_ma),
         Actual.Nur.1C.CPE6.LED2_P_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=cpeThresh & 
                                               is.na(Nur.1C.CPE6.LED2_P_original),0,Nur.1C.CPE6.LED2_P_ma),
         Actual.Nur.1C.CPE6.LED3_P_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=cpeThresh & 
                                               is.na(Nur.1C.CPE6.LED3_P_original),0,Nur.1C.CPE6.LED3_P_ma),
         Actual.Nur.2A.CPE7.LED1_P_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=cpeThresh & 
                                               is.na(Nur.2A.CPE7.LED1_P_original),0,Nur.2A.CPE7.LED1_P_ma),
         Actual.Nur.2A.CPE7.LED2_P_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=cpeThresh & 
                                               is.na(Nur.2A.CPE7.LED2_P_original),0,Nur.2A.CPE7.LED2_P_ma),
         Actual.Nur.2A.CPE7.LED3_P_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=cpeThresh & 
                                               is.na(Nur.2A.CPE7.LED3_P_original),0,Nur.2A.CPE7.LED3_P_ma),
         Actual.Nur.2A.CPE8.LED1_P_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=cpeThresh & 
                                               is.na(Nur.2A.CPE8.LED1_P_original),0,Nur.2A.CPE8.LED1_P_ma),
         Actual.Nur.2A.CPE8.LED2_P_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=cpeThresh & 
                                               is.na(Nur.2A.CPE8.LED2_P_original),0,Nur.2A.CPE8.LED2_P_ma),
         Actual.Nur.2A.CPE8.LED3_P_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=cpeThresh & 
                                               is.na(Nur.2A.CPE8.LED3_P_original),0,Nur.2A.CPE8.LED3_P_ma),
         Actual.Nur.2B.CPE9.LED1_P_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=cpeThresh & 
                                               is.na(Nur.2B.CPE9.LED1_P_original),0,Nur.2B.CPE9.LED1_P_ma),
         Actual.Nur.2B.CPE9.LED2_P_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=cpeThresh & 
                                               is.na(Nur.2B.CPE9.LED2_P_original),0,Nur.2B.CPE9.LED2_P_ma),
         Actual.Nur.2B.CPE9.LED3_P_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=cpeThresh & 
                                               is.na(Nur.2B.CPE9.LED3_P_original),0,Nur.2B.CPE9.LED3_P_ma),
         Actual.Nur.2B.CPE10.LED1_P_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=cpeThresh & 
                                               is.na(Nur.2B.CPE10.LED1_P_original),0,Nur.2B.CPE10.LED1_P_ma),
         Actual.Nur.2B.CPE10.LED2_P_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=cpeThresh & 
                                               is.na(Nur.2B.CPE10.LED2_P_original),0,Nur.2B.CPE10.LED2_P_ma),
         Actual.Nur.2B.CPE10.LED3_P_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=cpeThresh & 
                                               is.na(Nur.2B.CPE10.LED3_P_original),0,Nur.2B.CPE10.LED3_P_ma),
         Actual.Nur.2C.CPE11.LED1_P_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=cpeThresh & 
                                               is.na(Nur.2C.CPE11.LED1_P_original),0,Nur.2C.CPE11.LED1_P_ma),
         Actual.Nur.2C.CPE11.LED2_P_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=cpeThresh & 
                                               is.na(Nur.2C.CPE11.LED2_P_original),0,Nur.2C.CPE11.LED2_P_ma),
         Actual.Nur.2C.CPE11.LED3_P_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=cpeThresh & 
                                               is.na(Nur.2C.CPE11.LED3_P_original),0,Nur.2C.CPE11.LED3_P_ma),
         Actual.Nur.2C.CPE12.LED1_P_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=cpeThresh & 
                                                is.na(Nur.2C.CPE12.LED1_P_original),0,Nur.2C.CPE12.LED1_P_ma),
         Actual.Nur.2C.CPE12.LED2_P_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=cpeThresh & 
                                                is.na(Nur.2C.CPE12.LED2_P_original),0,Nur.2C.CPE12.LED2_P_ma),
         Actual.Nur.2C.CPE12.LED3_P_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=cpeThresh & 
                                                is.na(Nur.2C.CPE12.LED3_P_original),0,Nur.2C.CPE12.LED3_P_ma),
         Actual.Playground.CPE1.LED1_P_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=cpeThresh & 
                                      is.na(Playground.CPE1.LED1_P_original), 0, Playground.CPE1.LED1_P_ma),
         Actual.Playground.CPE1.LED2_P_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=cpeThresh & 
                                      is.na(Playground.CPE1.LED2_P_original), 0, Playground.CPE1.LED2_P_ma),
         Actual.Playground.CPE1.LED3_P_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=cpeThresh & 
                                      is.na(Playground.CPE1.LED3_P_original), 0, Playground.CPE1.LED3_P_ma),
         Actual.Playground.CPE2.LED1_P_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=cpeThresh & 
                                      is.na(Playground.CPE2.LED1_P_original), 0, Playground.CPE2.LED1_P_ma),
         Actual.Playground.CPE2.LED2_P_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=cpeThresh & 
                                      is.na(Playground.CPE2.LED2_P_original), 0, Playground.CPE2.LED2_P_ma),
         Actual.Playground.CPE2.LED3_P_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=cpeThresh & 
                                      is.na(Playground.CPE2.LED3_P_original), 0, Playground.CPE2.LED3_P_ma),
         Actual.Playground.CPE3.LED1_P_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=cpeThresh & 
                                     is.na(Playground.CPE3.LED1_P_original), 0, Playground.CPE3.LED1_P_ma),
         Actual.Playground.CPE3.LED2_P_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=cpeThresh & 
                                     is.na(Playground.CPE3.LED2_P_original), 0, Playground.CPE3.LED2_P_ma),
         Actual.Playground.CPE3.LED3_P_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=cpeThresh & 
                                     is.na(Playground.CPE3.LED3_P_original), 0, Playground.CPE3.LED3_P_ma),
         Actual.Playground.CPE4.LED1_P_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=cpeThresh & 
                                     is.na(Playground.CPE4.LED1_P_original), 0, Playground.CPE4.LED1_P_ma),
         Actual.Playground.CPE4.LED2_P_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=cpeThresh & 
                                     is.na(Playground.CPE4.LED2_P_original), 0, Playground.CPE4.LED2_P_ma),
         Actual.Playground.CPE4.LED3_P_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=cpeThresh & 
                                     is.na(Playground.CPE4.LED3_P_original), 0, Playground.CPE4.LED3_P_ma),
         Actual.Playground.CPE5.LED1_P_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=cpeThresh & 
                                     is.na(Playground.CPE5.LED1_P_original), 0, Playground.CPE5.LED1_P_ma),
         Actual.Playground.CPE5.LED2_P_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=cpeThresh & 
                                     is.na(Playground.CPE5.LED2_P_original), 0, Playground.CPE5.LED2_P_ma),
         Actual.Playground.CPE5.LED3_P_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=cpeThresh & 
                                     is.na(Playground.CPE5.LED3_P_original), 0, Playground.CPE5.LED3_P_ma),
         Actual.Streetlight.1.CPE.LED1_P_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=cpeThresh & 
                                    is.na(Streetlight.1.CPE.LED1_P_original),0,Streetlight.1.CPE.LED1_P_ma),
         Actual.Streetlight.1.CPE.LED2_P_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=cpeThresh & 
                                    is.na(Streetlight.1.CPE.LED2_P_original),0,Streetlight.1.CPE.LED2_P_ma),
         Actual.Streetlight.1.CPE.LED3_P_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=cpeThresh & 
                                    is.na(Streetlight.1.CPE.LED3_P_original),0,Streetlight.1.CPE.LED3_P_ma),
         Actual.Streetlight.2.CPE.LED1_P_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=cpeThresh & 
                                    is.na(Streetlight.2.CPE.LED1_P_original),0,Streetlight.2.CPE.LED1_P_ma),
         Actual.Streetlight.2.CPE.LED2_P_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=cpeThresh & 
                                    is.na(Streetlight.2.CPE.LED2_P_original),0,Streetlight.2.CPE.LED2_P_ma),
         Actual.Streetlight.2.CPE.LED3_P_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=cpeThresh & 
                                    is.na(Streetlight.2.CPE.LED3_P_original),0,Streetlight.2.CPE.LED3_P_ma),
         Actual.Streetlight.3.CPE.LED1_P_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=cpeThresh & 
                                    is.na(Streetlight.3.CPE.LED1_P_original),0,Streetlight.3.CPE.LED1_P_ma),
         Actual.Streetlight.3.CPE.LED2_P_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=cpeThresh & 
                                    is.na(Streetlight.3.CPE.LED2_P_original),0,Streetlight.3.CPE.LED2_P_ma),
         Actual.Streetlight.3.CPE.LED3_P_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=cpeThresh & 
                                    is.na(Streetlight.3.CPE.LED3_P_original),0,Streetlight.3.CPE.LED3_P_ma),
         Actual.Nur.1A.S1.AC_Day_Energy_session_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=socThresh &
                      is.na(Nur.1A.S1.AC_Day_Energy_session_original),0,Nur.1A.S1.AC_Day_Energy_session_ma),
         Actual.Nur.1A.S2.AC_Day_Energy_session_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=socThresh &
                      is.na(Nur.1A.S2.AC_Day_Energy_session_original),0,Nur.1A.S2.AC_Day_Energy_session_ma),
         Actual.Nur.1B.S1.AC_Day_Energy_session_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=socThresh &
                      is.na(Nur.1B.S1.AC_Day_Energy_session_original),0,Nur.1B.S1.AC_Day_Energy_session_ma),
         Actual.Nur.1C.S1.AC_Day_Energy_session_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=socThresh &
                      is.na(Nur.1C.S1.AC_Day_Energy_session_original),0,Nur.1C.S1.AC_Day_Energy_session_ma),
         Actual.Nur.2A.S1.AC_Day_Energy_session_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=socThresh &
                      is.na(Nur.2A.S1.AC_Day_Energy_session_original),0,Nur.2A.S1.AC_Day_Energy_session_ma),
         Actual.Nur.2A.S2.AC_Day_Energy_session_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=socThresh &
                      is.na(Nur.2A.S2.AC_Day_Energy_session_original),0,Nur.2A.S2.AC_Day_Energy_session_ma),
         Actual.Nur.2B.S1.AC_Day_Energy_session_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=socThresh &
                      is.na(Nur.2B.S1.AC_Day_Energy_session_original),0,Nur.2B.S1.AC_Day_Energy_session_ma),
         Actual.Nur.2C.S1.AC_Day_Energy_session_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=socThresh &
                      is.na(Nur.2C.S1.AC_Day_Energy_session_original),0,Nur.2C.S1.AC_Day_Energy_session_ma),
         Actual.Playground.S1.AC_Day_Energy_session_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=socThresh &
              is.na(Playground.S1.AC_Day_Energy_session_original),0,Playground.S1.AC_Day_Energy_session_ma),
         Actual.Playground.S2.AC_Day_Energy_session_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=socThresh &
              is.na(Playground.S2.AC_Day_Energy_session_original),0,Playground.S2.AC_Day_Energy_session_ma),
         Actual.Nur.1A.S1.vRELAY1_LVL_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=socThresh &
                                          is.na(Nur.1A.S1.vRELAY1_LVL_original),0,Nur.1A.S1.vRELAY1_LVL_ma),
         Actual.Nur.1A.S2.vRELAY1_LVL_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=socThresh &
                                          is.na(Nur.1A.S2.vRELAY1_LVL_original),0,Nur.1A.S2.vRELAY1_LVL_ma),
         Actual.Nur.1B.S1.vRELAY1_LVL_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=socThresh &
                                          is.na(Nur.1B.S1.vRELAY1_LVL_original),0,Nur.1B.S1.vRELAY1_LVL_ma),
         Actual.Nur.1C.S1.vRELAY1_LVL_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=socThresh &
                                          is.na(Nur.1C.S1.vRELAY1_LVL_original),0,Nur.1C.S1.vRELAY1_LVL_ma),
         Actual.Nur.2A.S1.vRELAY1_LVL_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=socThresh &
                                          is.na(Nur.2A.S1.vRELAY1_LVL_original),0,Nur.2A.S1.vRELAY1_LVL_ma),
         Actual.Nur.2A.S2.vRELAY1_LVL_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=socThresh &
                                          is.na(Nur.2A.S2.vRELAY1_LVL_original),0,Nur.2A.S2.vRELAY1_LVL_ma),
         Actual.Nur.2B.S1.vRELAY1_LVL_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=socThresh &
                                          is.na(Nur.2B.S1.vRELAY1_LVL_original),0,Nur.2B.S1.vRELAY1_LVL_ma),
         Actual.Nur.2C.S1.vRELAY1_LVL_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=socThresh &
                                          is.na(Nur.2C.S1.vRELAY1_LVL_original),0,Nur.2C.S1.vRELAY1_LVL_ma),
         Actual.Playground.S1.vRELAY1_LVL_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=socThresh & 
                                 is.na(Playground.S1.vRELAY1_LVL_original), 0,Playground.S1.vRELAY1_LVL_ma),
         Actual.Playground.S2.vRELAY1_LVL_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=socThresh & 
                                 is.na(Playground.S2.vRELAY1_LVL_original), 0,Playground.S2.vRELAY1_LVL_ma),
         Actual.Solar.Charger.Battery.Power_ma=ifelse(Battery.Monitor.State.of.charge.._ma<=socThresh & 
                          is.na(Solar.Charger.Battery.watts.W_original),0,Solar.Charger.Battery.watts.W_ma),
         Actual.PV.power.W_ma=ifelse((Battery.Monitor.State.of.charge.._ma<=socThresh &
                                     is.na(Solar.Charger.PV.power_original)), 0, Solar.Charger.PV.power_ma),
         Actual.AC.consumption_ma=ifelse((Battery.Monitor.State.of.charge.._ma<=socThresh & 
              is.na(System.overview.AC.Consumption.L1.W_original)),0,System.overview.AC.Consumption.L1.W_ma),
         Actual.System.Battery.Power_ma=ifelse((Battery.Monitor.State.of.charge.._ma<=socThresh &
                     is.na(System.overview.Battery.Power.W_original)), 0,System.overview.Battery.Power.W_ma),
         Positive.Actual.Solar.Charger.Battery.Power_ma=ifelse(Actual.Solar.Charger.Battery.Power_ma<0,0,
                                                               Actual.Solar.Charger.Battery.Power_ma),
         Negative.Actual.Solar.Charger.Battery.Power_ma=ifelse(Actual.Solar.Charger.Battery.Power_ma>0,0,
                                                               Actual.Solar.Charger.Battery.Power_ma),
         Positive.Actual.System.Battery.Power_ma=ifelse(Actual.System.Battery.Power_ma<0,0,
                                                        Actual.System.Battery.Power_ma),
         Negative.Actual.System.Battery.Power_ma=ifelse(Actual.System.Battery.Power_ma>0,0,
                                                        Actual.System.Battery.Power_ma))
#******************************************************************************************#

#******************************************************************************************#
# Apply corrections based on component failures based on check sheets
# Get patterns of missing data (system, CPE and sockets) and map with check sheets
missingData <- na_seadec_correctedData[,c(4, which(grepl("original", 
                                                         colnames(na_seadec_correctedData), fixed=TRUE)))]
missingData <- missingData[,c(1,4,78,88, which(grepl("LED1_P", colnames(missingData), fixed=TRUE)|
                                                 grepl("LED2_P", colnames(missingData), fixed=TRUE)|
                                                 grepl("LED3_P", colnames(missingData), fixed=TRUE)|
                                                 grepl("vRELAY1_LVL", colnames(missingData), fixed=TRUE) ))]
missingData$load <- rowSums(missingData[5:74], na.rm=TRUE) 
missingData <- missingData[,c(1:4,75,which(grepl("LED2_P", colnames(missingData), fixed=TRUE) | 
                                             grepl("vRELAY1_LVL", colnames(missingData), fixed=TRUE) ))]
ggplot(missingData, aes(x=timestamp)) + 
  geom_line(aes(y=System.overview.AC.Consumption.L1.W_original), color="blue") + geom_line(aes(y=load)) 

missingData <- gather(missingData, id, value, 2:35)
missingData <- missingData %>% mutate(month=month(timestamp, label=TRUE, abbr=TRUE))
missingLevel <- missingData %>% group_by(timestamp, id) %>% summarise(count=ifelse(is.na(value),0,1))
missingLevel <- as.data.frame(missingLevel)
missingLevel <- missingLevel %>% mutate(id=substr(id, 1, str_length(id)-9))
missingLevel <- missingLevel %>% mutate(month=month(timestamp, label=TRUE, abbr=TRUE))
ggplot(missingLevel[missingLevel$timestamp>="2020-03-23" & missingLevel$timestamp<="2020-03-29",], aes(timestamp, id)) + geom_tile(aes(fill = count)) + xlab("X axis") + ylab("Y axis") + 
  labs(x = "Day of study", fill="Yield (%)") + THEME + 
  guides(fill = guide_colorbar(barwidth = 8, barheight = 0.5))

# Apply corrections based on the below reported component failures:
# 1. All CPE, socket and system load is 0 between 4-18 Sep where original is missing due to black out
sub_1 <- na_seadec_correctedData[na_seadec_correctedData$date>="2019-09-04" & 
                                   na_seadec_correctedData$date<="2019-09-18",]
sub_1 <- sub_1 %>% 
  mutate(Actual.Nur.1A.CPE1.LED1_P_ma=ifelse(is.na(Nur.1A.CPE1.LED1_P_original),0,Actual.Nur.1A.CPE1.LED1_P_ma),
         Actual.Nur.1A.CPE1.LED2_P_ma=ifelse(is.na(Nur.1A.CPE1.LED2_P_original),0,Actual.Nur.1A.CPE1.LED2_P_ma),
         Actual.Nur.1A.CPE1.LED3_P_ma=ifelse(is.na(Nur.1A.CPE1.LED3_P_original),0,Actual.Nur.1A.CPE1.LED3_P_ma),
         Actual.Nur.1A.CPE2.LED1_P_ma=ifelse(is.na(Nur.1A.CPE2.LED1_P_original),0,Actual.Nur.1A.CPE2.LED1_P_ma),
         Actual.Nur.1A.CPE2.LED2_P_ma=ifelse(is.na(Nur.1A.CPE2.LED2_P_original),0,Actual.Nur.1A.CPE2.LED2_P_ma),
         Actual.Nur.1A.CPE2.LED3_P_ma=ifelse(is.na(Nur.1A.CPE2.LED3_P_original),0,Actual.Nur.1A.CPE2.LED3_P_ma),
         Actual.Nur.1B.CPE3.LED1_P_ma=ifelse(is.na(Nur.1B.CPE3.LED1_P_original),0,Actual.Nur.1B.CPE3.LED1_P_ma),
         Actual.Nur.1B.CPE3.LED2_P_ma=ifelse(is.na(Nur.1B.CPE3.LED2_P_original),0,Actual.Nur.1B.CPE3.LED2_P_ma),
         Actual.Nur.1B.CPE3.LED3_P_ma=ifelse(is.na(Nur.1B.CPE3.LED3_P_original),0,Actual.Nur.1B.CPE3.LED3_P_ma),
         Actual.Nur.1B.CPE4.LED1_P_ma=ifelse(is.na(Nur.1B.CPE4.LED1_P_original),0,Actual.Nur.1B.CPE4.LED1_P_ma),
         Actual.Nur.1B.CPE4.LED2_P_ma=ifelse(is.na(Nur.1B.CPE4.LED2_P_original),0,Actual.Nur.1B.CPE4.LED2_P_ma),
         Actual.Nur.1B.CPE4.LED3_P_ma=ifelse(is.na(Nur.1B.CPE4.LED3_P_original),0,Actual.Nur.1B.CPE4.LED3_P_ma),
         Actual.Nur.1C.CPE5.LED1_P_ma=ifelse(is.na(Nur.1C.CPE5.LED1_P_original),0,Actual.Nur.1C.CPE5.LED1_P_ma),
         Actual.Nur.1C.CPE5.LED2_P_ma=ifelse(is.na(Nur.1C.CPE5.LED2_P_original),0,Actual.Nur.1C.CPE5.LED2_P_ma),
         Actual.Nur.1C.CPE5.LED3_P_ma=ifelse(is.na(Nur.1C.CPE5.LED3_P_original),0,Actual.Nur.1C.CPE5.LED3_P_ma),
         Actual.Nur.1C.CPE6.LED1_P_ma=ifelse(is.na(Nur.1C.CPE6.LED1_P_original),0,Actual.Nur.1C.CPE6.LED1_P_ma),
         Actual.Nur.1C.CPE6.LED2_P_ma=ifelse(is.na(Nur.1C.CPE6.LED2_P_original),0,Actual.Nur.1C.CPE6.LED2_P_ma),
         Actual.Nur.1C.CPE6.LED3_P_ma=ifelse(is.na(Nur.1C.CPE6.LED3_P_original),0,Actual.Nur.1C.CPE6.LED3_P_ma),
         Actual.Nur.2A.CPE7.LED1_P_ma=ifelse(is.na(Nur.2A.CPE7.LED1_P_original),0,Actual.Nur.2A.CPE7.LED1_P_ma),
         Actual.Nur.2A.CPE7.LED2_P_ma=ifelse(is.na(Nur.2A.CPE7.LED2_P_original),0,Actual.Nur.2A.CPE7.LED2_P_ma),
         Actual.Nur.2A.CPE7.LED3_P_ma=ifelse(is.na(Nur.2A.CPE7.LED3_P_original),0,Actual.Nur.2A.CPE7.LED3_P_ma),
         Actual.Nur.2A.CPE8.LED1_P_ma=ifelse(is.na(Nur.2A.CPE8.LED1_P_original),0,Actual.Nur.2A.CPE8.LED1_P_ma),
         Actual.Nur.2A.CPE8.LED2_P_ma=ifelse(is.na(Nur.2A.CPE8.LED2_P_original),0,Actual.Nur.2A.CPE8.LED2_P_ma),
         Actual.Nur.2A.CPE8.LED3_P_ma=ifelse(is.na(Nur.2A.CPE8.LED3_P_original),0,Actual.Nur.2A.CPE8.LED3_P_ma),
         Actual.Nur.2B.CPE9.LED1_P_ma=ifelse(is.na(Nur.2B.CPE9.LED1_P_original),0,Actual.Nur.2B.CPE9.LED1_P_ma),
         Actual.Nur.2B.CPE9.LED2_P_ma=ifelse(is.na(Nur.2B.CPE9.LED2_P_original),0,Actual.Nur.2B.CPE9.LED2_P_ma),
         Actual.Nur.2B.CPE9.LED3_P_ma=ifelse(is.na(Nur.2B.CPE9.LED3_P_original),0,Actual.Nur.2B.CPE9.LED3_P_ma),
         Actual.Nur.2B.CPE10.LED1_P_ma=ifelse(is.na(Nur.2B.CPE10.LED1_P_original),0,Actual.Nur.2B.CPE10.LED1_P_ma),
         Actual.Nur.2B.CPE10.LED2_P_ma=ifelse(is.na(Nur.2B.CPE10.LED2_P_original),0,Actual.Nur.2B.CPE10.LED2_P_ma),
         Actual.Nur.2B.CPE10.LED3_P_ma=ifelse(is.na(Nur.2B.CPE10.LED3_P_original),0,Actual.Nur.2B.CPE10.LED3_P_ma),
         Actual.Nur.2C.CPE11.LED1_P_ma=ifelse(is.na(Nur.2C.CPE11.LED1_P_original),0,Actual.Nur.2C.CPE11.LED1_P_ma),
         Actual.Nur.2C.CPE11.LED2_P_ma=ifelse(is.na(Nur.2C.CPE11.LED2_P_original),0,Actual.Nur.2C.CPE11.LED2_P_ma),
         Actual.Nur.2C.CPE11.LED3_P_ma=ifelse(is.na(Nur.2C.CPE11.LED3_P_original),0,Actual.Nur.2C.CPE11.LED3_P_ma),
         Actual.Nur.2C.CPE12.LED1_P_ma=ifelse(is.na(Nur.2C.CPE12.LED1_P_original),0,Actual.Nur.2C.CPE12.LED1_P_ma),
         Actual.Nur.2C.CPE12.LED2_P_ma=ifelse(is.na(Nur.2C.CPE12.LED2_P_original),0,Actual.Nur.2C.CPE12.LED2_P_ma),
         Actual.Nur.2C.CPE12.LED3_P_ma=ifelse(is.na(Nur.2C.CPE12.LED3_P_original),0,Actual.Nur.2C.CPE12.LED3_P_ma),
         Actual.Playground.CPE1.LED1_P_ma=ifelse(is.na(Playground.CPE1.LED1_P_original), 0, Actual.Playground.CPE1.LED1_P_ma),
         Actual.Playground.CPE1.LED2_P_ma=ifelse(is.na(Playground.CPE1.LED2_P_original), 0, Actual.Playground.CPE1.LED2_P_ma),
         Actual.Playground.CPE1.LED3_P_ma=ifelse(is.na(Playground.CPE1.LED3_P_original), 0, Actual.Playground.CPE1.LED3_P_ma),
         Actual.Playground.CPE2.LED1_P_ma=ifelse(is.na(Playground.CPE2.LED1_P_original), 0, Actual.Playground.CPE2.LED1_P_ma),
         Actual.Playground.CPE2.LED2_P_ma=ifelse(is.na(Playground.CPE2.LED2_P_original), 0, Actual.Playground.CPE2.LED2_P_ma),
         Actual.Playground.CPE2.LED3_P_ma=ifelse(is.na(Playground.CPE2.LED3_P_original), 0, Actual.Playground.CPE2.LED3_P_ma),
         Actual.Playground.CPE3.LED1_P_ma=ifelse(is.na(Playground.CPE3.LED1_P_original), 0, Actual.Playground.CPE3.LED1_P_ma),
         Actual.Playground.CPE3.LED2_P_ma=ifelse(is.na(Playground.CPE3.LED2_P_original), 0, Actual.Playground.CPE3.LED2_P_ma),
         Actual.Playground.CPE3.LED3_P_ma=ifelse(is.na(Playground.CPE3.LED3_P_original), 0, Actual.Playground.CPE3.LED3_P_ma),
         Actual.Playground.CPE4.LED1_P_ma=ifelse(is.na(Playground.CPE4.LED1_P_original), 0, Actual.Playground.CPE4.LED1_P_ma),
         Actual.Playground.CPE4.LED2_P_ma=ifelse(is.na(Playground.CPE4.LED2_P_original), 0, Actual.Playground.CPE4.LED2_P_ma),
         Actual.Playground.CPE4.LED3_P_ma=ifelse(is.na(Playground.CPE4.LED3_P_original), 0, Actual.Playground.CPE4.LED3_P_ma),
         Actual.Playground.CPE5.LED1_P_ma=ifelse(is.na(Playground.CPE5.LED1_P_original), 0, Actual.Playground.CPE5.LED1_P_ma),
         Actual.Playground.CPE5.LED2_P_ma=ifelse(is.na(Playground.CPE5.LED2_P_original), 0, Actual.Playground.CPE5.LED2_P_ma),
         Actual.Playground.CPE5.LED3_P_ma=ifelse(is.na(Playground.CPE5.LED3_P_original), 0, Actual.Playground.CPE5.LED3_P_ma),
         Actual.Streetlight.1.CPE.LED1_P_ma=ifelse(is.na(Streetlight.1.CPE.LED1_P_original),0,Actual.Streetlight.1.CPE.LED1_P_ma),
         Actual.Streetlight.1.CPE.LED2_P_ma=ifelse(is.na(Streetlight.1.CPE.LED2_P_original),0,Actual.Streetlight.1.CPE.LED2_P_ma),
         Actual.Streetlight.1.CPE.LED3_P_ma=ifelse(is.na(Streetlight.1.CPE.LED3_P_original),0,Actual.Streetlight.1.CPE.LED3_P_ma),
         Actual.Streetlight.2.CPE.LED1_P_ma=ifelse(is.na(Streetlight.2.CPE.LED1_P_original),0,Actual.Streetlight.2.CPE.LED1_P_ma),
         Actual.Streetlight.2.CPE.LED2_P_ma=ifelse(is.na(Streetlight.2.CPE.LED2_P_original),0,Actual.Streetlight.2.CPE.LED2_P_ma),
         Actual.Streetlight.2.CPE.LED3_P_ma=ifelse(is.na(Streetlight.2.CPE.LED3_P_original),0,Actual.Streetlight.2.CPE.LED3_P_ma),
         Actual.Streetlight.3.CPE.LED1_P_ma=ifelse(is.na(Streetlight.3.CPE.LED1_P_original),0,Actual.Streetlight.3.CPE.LED1_P_ma),
         Actual.Streetlight.3.CPE.LED2_P_ma=ifelse(is.na(Streetlight.3.CPE.LED2_P_original),0,Actual.Streetlight.3.CPE.LED2_P_ma),
         Actual.Streetlight.3.CPE.LED3_P_ma=ifelse(is.na(Streetlight.3.CPE.LED3_P_original),0,Actual.Streetlight.3.CPE.LED3_P_ma),
         Actual.Nur.1A.S1.AC_Day_Energy_session_ma=ifelse(is.na(Nur.1A.S1.AC_Day_Energy_session_original),0,
                                                          Actual.Nur.1A.S1.AC_Day_Energy_session_ma),
         Actual.Nur.1A.S2.AC_Day_Energy_session_ma=ifelse(is.na(Nur.1A.S2.AC_Day_Energy_session_original),0,
                                                          Actual.Nur.1A.S2.AC_Day_Energy_session_ma),
         Actual.Nur.1B.S1.AC_Day_Energy_session_ma=ifelse(is.na(Nur.1B.S1.AC_Day_Energy_session_original),0,
                                                          Actual.Nur.1B.S1.AC_Day_Energy_session_ma),
         Actual.Nur.1C.S1.AC_Day_Energy_session_ma=ifelse(is.na(Nur.1C.S1.AC_Day_Energy_session_original),0,
                                                          Actual.Nur.1C.S1.AC_Day_Energy_session_ma),
         Actual.Nur.2A.S1.AC_Day_Energy_session_ma=ifelse(is.na(Nur.2A.S1.AC_Day_Energy_session_original),0,
                                                          Actual.Nur.2A.S1.AC_Day_Energy_session_ma),
         Actual.Nur.2A.S2.AC_Day_Energy_session_ma=ifelse(is.na(Nur.2A.S2.AC_Day_Energy_session_original),0,
                                                          Actual.Nur.2A.S2.AC_Day_Energy_session_ma),
         Actual.Nur.2B.S1.AC_Day_Energy_session_ma=ifelse(is.na(Nur.2B.S1.AC_Day_Energy_session_original),0,
                                                          Actual.Nur.2B.S1.AC_Day_Energy_session_ma),
         Actual.Nur.2C.S1.AC_Day_Energy_session_ma=ifelse(is.na(Nur.2C.S1.AC_Day_Energy_session_original),0,
                                                          Actual.Nur.2C.S1.AC_Day_Energy_session_ma),
         Actual.Playground.S1.AC_Day_Energy_session_ma=ifelse(is.na(Playground.S1.AC_Day_Energy_session_original),
                                                              0,Actual.Playground.S1.AC_Day_Energy_session_ma),
         Actual.Playground.S2.AC_Day_Energy_session_ma=ifelse(is.na(Playground.S2.AC_Day_Energy_session_original),
                                                              0,Actual.Playground.S2.AC_Day_Energy_session_ma),
         Actual.Nur.1A.S1.vRELAY1_LVL_ma=ifelse(is.na(Nur.1A.S1.vRELAY1_LVL_original),0,Actual.Nur.1A.S1.vRELAY1_LVL_ma),
         Actual.Nur.1A.S2.vRELAY1_LVL_ma=ifelse(is.na(Nur.1A.S2.vRELAY1_LVL_original),0,Actual.Nur.1A.S2.vRELAY1_LVL_ma),
         Actual.Nur.1B.S1.vRELAY1_LVL_ma=ifelse(is.na(Nur.1B.S1.vRELAY1_LVL_original),0,Actual.Nur.1B.S1.vRELAY1_LVL_ma),
         Actual.Nur.1C.S1.vRELAY1_LVL_ma=ifelse(is.na(Nur.1C.S1.vRELAY1_LVL_original),0,Actual.Nur.1C.S1.vRELAY1_LVL_ma),
         Actual.Nur.2A.S1.vRELAY1_LVL_ma=ifelse(is.na(Nur.2A.S1.vRELAY1_LVL_original),0,Actual.Nur.2A.S1.vRELAY1_LVL_ma),
         Actual.Nur.2A.S2.vRELAY1_LVL_ma=ifelse(is.na(Nur.2A.S2.vRELAY1_LVL_original),0,Actual.Nur.2A.S2.vRELAY1_LVL_ma),
         Actual.Nur.2B.S1.vRELAY1_LVL_ma=ifelse(is.na(Nur.2B.S1.vRELAY1_LVL_original),0,Actual.Nur.2B.S1.vRELAY1_LVL_ma),
         Actual.Nur.2C.S1.vRELAY1_LVL_ma=ifelse(is.na(Nur.2C.S1.vRELAY1_LVL_original),0,Actual.Nur.2C.S1.vRELAY1_LVL_ma),
         Actual.Playground.S1.vRELAY1_LVL_ma=ifelse(is.na(Playground.S1.vRELAY1_LVL_original),0,Actual.Playground.S1.vRELAY1_LVL_ma),
         Actual.Playground.S2.vRELAY1_LVL_ma=ifelse(is.na(Playground.S2.vRELAY1_LVL_original),0,Actual.Playground.S2.vRELAY1_LVL_ma),
         Actual.Solar.Charger.Battery.Power_ma=ifelse(is.na(Solar.Charger.Battery.watts.W_original),0,
                                                      Actual.Solar.Charger.Battery.Power_ma),
         Actual.PV.power.W_ma=ifelse(is.na(Solar.Charger.PV.power_original),0,Actual.PV.power.W_ma),
         Actual.AC.consumption_ma=ifelse(is.na(System.overview.AC.Consumption.L1.W_original),0,
                                         Actual.AC.consumption_ma),
         Actual.System.Battery.Power_ma=ifelse(is.na(System.overview.Battery.Power.W_original),0,
                                               Actual.System.Battery.Power_ma),
         Positive.Actual.Solar.Charger.Battery.Power_ma=ifelse(Actual.Solar.Charger.Battery.Power_ma<0,0,
                                                               Actual.Solar.Charger.Battery.Power_ma),
         Negative.Actual.Solar.Charger.Battery.Power_ma=ifelse(Actual.Solar.Charger.Battery.Power_ma>0,0,
                                                               Actual.Solar.Charger.Battery.Power_ma),
         Positive.Actual.System.Battery.Power_ma=ifelse(Actual.System.Battery.Power_ma<0,0,
                                                        Actual.System.Battery.Power_ma),
         Negative.Actual.System.Battery.Power_ma=ifelse(Actual.System.Battery.Power_ma>0,0,
                                                        Actual.System.Battery.Power_ma))

# Remove these rows from na_seadec_corrected data and bind rows from sub_1
na_seadec_correctedData <- na_seadec_correctedData[!(na_seadec_correctedData$date>="2019-09-04" & 
                                                       na_seadec_correctedData$date<="2019-09-18"),] 
na_seadec_correctedData <- rbind(na_seadec_correctedData, sub_1)
na_seadec_correctedData <- na_seadec_correctedData[order(na_seadec_correctedData$timestamp),]

# 2. Socket Nur1AS1 load is 0 from 20 Sep to 17 Feb and 20 Mar to 28 Mar
na_seadec_correctedData <- na_seadec_correctedData %>% 
  mutate(Actual.Nur.1A.S1.AC_Day_Energy_session_ma=ifelse((date>="2019-09-20" & date<="2020-02-17" & 
                                                            is.na(Nur.1A.S1.AC_Day_Energy_session_original))|
                                                           (date>="2020-03-20" & date<="2020-03-28" & 
                                                              is.na(Nur.1A.S1.AC_Day_Energy_session_original)),
                                                          0,Actual.Nur.1A.S1.AC_Day_Energy_session_ma),
         Actual.Nur.1A.S1.vRELAY1_LVL_ma=ifelse((date>="2019-09-20" & date<="2020-02-17" & 
                                                  is.na(Nur.1A.S1.vRELAY1_LVL_original)) |
                                                (date>="2020-03-20" & date<="2020-03-28" & 
                                                   is.na(Nur.1A.S1.vRELAY1_LVL_original)),
                                                0,Actual.Nur.1A.S1.vRELAY1_LVL_ma))

# 3. Streetlight3 (LED1,2,3) load is 0 between 1-8 Jan
na_seadec_correctedData <- na_seadec_correctedData %>% 
  mutate(Actual.Streetlight.3.CPE.LED1_P_ma=ifelse(date>="2020-01-01" & date<="2020-01-08" & 
                                                     is.na(Streetlight.3.CPE.LED1_P_original),0,
                                                   Actual.Streetlight.3.CPE.LED1_P_ma),
         Actual.Streetlight.3.CPE.LED2_P_ma=ifelse(date>="2020-01-01" & date<="2020-01-08" & 
                                                     is.na(Streetlight.3.CPE.LED2_P_original),0,
                                                   Actual.Streetlight.3.CPE.LED2_P_ma),            
         Actual.Streetlight.3.CPE.LED3_P_ma=ifelse(date>="2020-01-01" & date<="2020-01-08" & 
                                                     is.na(Streetlight.3.CPE.LED3_P_original),0,
                                                   Actual.Streetlight.3.CPE.LED3_P_ma))

# 4. Nur1ACPE2 (LED1,2,3) load is 0 between 1 Feb - 8 Mar
na_seadec_correctedData <- na_seadec_correctedData %>% 
  mutate(Actual.Nur.1A.CPE2.LED1_P_ma=ifelse(date>="2020-02-01" & date<="2020-03-08" & 
                                               is.na(Nur.1A.CPE2.LED1_P_original),0,
                                             Actual.Nur.1A.CPE2.LED1_P_ma),
         Actual.Nur.1A.CPE2.LED2_P_ma=ifelse(date>="2020-02-01" & date<="2020-03-08" & 
                                               is.na(Nur.1A.CPE2.LED2_P_original),0,
                                             Actual.Nur.1A.CPE2.LED2_P_ma),                  
         Actual.Nur.1A.CPE2.LED3_P_ma=ifelse(date>="2020-02-01" & date<="2020-03-08" & 
                                               is.na(Nur.1A.CPE2.LED3_P_original),0,
                                             Actual.Nur.1A.CPE2.LED3_P_ma))

# 5. Nur2BCPE10 (LED1,2,3) load is 0 between 6 Feb - 8 Mar
na_seadec_correctedData <- na_seadec_correctedData %>% 
  mutate(Actual.Nur.2B.CPE10.LED1_P_ma=ifelse(date>="2020-02-06" & date<="2020-03-08" & 
                                               is.na(Nur.2B.CPE10.LED1_P_original),0,
                                             Actual.Nur.2B.CPE10.LED1_P_ma),
         Actual.Nur.2B.CPE10.LED2_P_ma=ifelse(date>="2020-02-06" & date<="2020-03-08" & 
                                               is.na(Nur.2B.CPE10.LED2_P_original),0,
                                             Actual.Nur.2B.CPE10.LED2_P_ma),                  
         Actual.Nur.2B.CPE10.LED3_P_ma=ifelse(date>="2020-02-06" & date<="2020-03-08" & 
                                               is.na(Nur.2B.CPE10.LED3_P_original),0,
                                             Actual.Nur.2B.CPE10.LED3_P_ma))

# 6. Sockets at Nur2 (2AS1, 2BS1, 2CS1) load is 0 between 12 Feb - 8 Mar
na_seadec_correctedData <- na_seadec_correctedData %>% 
  mutate(Actual.Nur.2A.S1.AC_Day_Energy_session_ma=ifelse(date>="2020-02-12" & date<="2020-03-08" &
                                                            is.na(Nur.2A.S1.AC_Day_Energy_session_original),
                                                          0,Actual.Nur.2A.S1.AC_Day_Energy_session_ma), 
         Actual.Nur.2B.S1.AC_Day_Energy_session_ma=ifelse(date>="2020-02-12" & date<="2020-03-08" &
                                                            is.na(Nur.2B.S1.AC_Day_Energy_session_original),
                                                          0,Actual.Nur.2B.S1.AC_Day_Energy_session_ma),     
         Actual.Nur.2C.S1.AC_Day_Energy_session_ma=ifelse(date>="2020-02-12" & date<="2020-03-08" &
                                                            is.na(Nur.2C.S1.AC_Day_Energy_session_original),
                                                          0,Actual.Nur.2C.S1.AC_Day_Energy_session_ma),
         Actual.Nur.2A.S1.vRELAY1_LVL_ma=ifelse(date>="2020-02-12" & date<="2020-03-08" &
                                                  is.na(Nur.2A.S1.vRELAY1_LVL_original),
                                                0,Actual.Nur.2A.S1.vRELAY1_LVL_ma), 
         Actual.Nur.2B.S1.vRELAY1_LVL_ma=ifelse(date>="2020-02-12" & date<="2020-03-08" &
                                                  is.na(Nur.2B.S1.vRELAY1_LVL_original),
                                                0,Actual.Nur.2B.S1.vRELAY1_LVL_ma),
         Actual.Nur.2C.S1.vRELAY1_LVL_ma=ifelse(date>="2020-02-12" & date<="2020-03-08" &
                                                  is.na(Nur.2C.S1.vRELAY1_LVL_original),
                                                0,Actual.Nur.2C.S1.vRELAY1_LVL_ma))

# 7. Socket Nur2AS2 load is 0 between 12 Feb - 31 Mar
na_seadec_correctedData <- na_seadec_correctedData %>% 
  mutate(Actual.Nur.2A.S2.AC_Day_Energy_session_ma=ifelse(date>="2020-02-12" & date<="2020-03-31" & 
                                                            is.na(Nur.2A.S2.AC_Day_Energy_session_original),
                                                          0, Actual.Nur.2A.S2.AC_Day_Energy_session_ma),
         Actual.Nur.2A.S2.vRELAY1_LVL_ma=ifelse(date>="2020-02-12" & date<="2020-03-31" & 
                                                  is.na(Nur.2A.S2.vRELAY1_LVL_original),
                                                0,Actual.Nur.2A.S2.vRELAY1_LVL_ma))

# Save corrected data
na_seadec_correctedData <- na_seadec_correctedData %>% mutate(month=as.character(month))
write.csv(na_seadec_correctedData, file=here(filepath,"na_seadec_correctedData.csv"), row.names=FALSE)
#******************************************************************************************#