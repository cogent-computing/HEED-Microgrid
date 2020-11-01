#******************************************************************************************#
# This is the script for analysing data for Microgrid                                      #
# Author: K Bhargava                                                                       #
# Last updated on: 24th Oct 2020                                                           #
#******************************************************************************************#

#******************************************************************************************#
# Importing libraries
library(tidyverse)
library(lubridate)
library(wesanderson)
library(extrafont)
library(here)
#******************************************************************************************#

#******************************************************************************************#
# Define macros - theme for all plots
MONTHS <- c("Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar")
THEME <- theme(legend.position = "bottom", legend.text=element_text(size=10, family="Times New Roman"),
               legend.key.size = unit(0.5, "cm"),legend.margin = margin(t=0,r=0,b=0,l=0), 
               panel.grid.major.y = element_line(colour="grey"), 
               panel.grid.minor = element_blank(), panel.background = element_blank(), 
               axis.line = element_line(colour = "black"), 
               axis.text = element_text(size=9, family="Times New Roman"),
               axis.title = element_text(size=10, family="Times New Roman")) 
#******************************************************************************************#

#******************************************************************************************#
# Set working directory 
filepath <- "Data"
plot_dir <- "Plots/Paper 6"
#******************************************************************************************#

#******************************************************************************************#
#Read in hourly data file 
imputed_data <- read.csv(here(filepath,"na_seadec_correctedData.csv"), header=TRUE, stringsAsFactors = FALSE)
imputed_data <- imputed_data %>% mutate(date=as.Date(date),
                                        timestamp=as.POSIXct(timestamp,tz="GMT",origin="1970-01-01"),
                                        month = factor(month, levels = MONTHS,labels = MONTHS))
na_seadec_sub <- imputed_data[,c(1:3, which(grepl("Actual", colnames(imputed_data), fixed=TRUE)))]
na_seadec_sub <- na_seadec_sub[, c(1:3, which(grepl("CPE", colnames(na_seadec_sub), fixed=TRUE) |
                                                grepl("vRELAY", colnames(na_seadec_sub), fixed=TRUE)))]
na_seadec_sub <- na_seadec_sub %>% mutate(weekday = as.character(wday(date, label=TRUE, abbr=TRUE)),
                                          Actual.User.Load.W=rowSums(na_seadec_sub[,c(4:73)]),
                                          Actual.User.Load.NP.W = rowSums(na_seadec_sub[,c(4:54,64:73)]))

na_seadec_nur <- na_seadec_sub[,c(1:3,74,which(grepl("Nur", colnames(na_seadec_sub), fixed=TRUE)))]
na_seadec_nur <- na_seadec_nur %>% mutate(Actual.User.Load.W=rowSums(na_seadec_nur[,c(5:48)]),
                                          Predicted.User.Load.W=rep(nur_predicted$User.Load.W,274))

na_seadec_nur1 <- na_seadec_nur[, c(1:4, which(grepl("Nur.1", colnames(na_seadec_nur), fixed=TRUE)))]
na_seadec_nur1 <- na_seadec_nur1 %>% mutate(Actual.User.Load.W=rowSums(na_seadec_nur1[,c(5:26)]))
# Outdoor lights: LED 3 in CPE1, CPE3 and CPE5
na_seadec_nur1 <- na_seadec_nur1 %>% mutate(Socket.load.W=rowSums(na_seadec_nur1[,c(23:26)]),
                                            Indoor.light.load.W=rowSums(na_seadec_nur1[,c(5:6,8:12,14:18,20:22)]),
                                            Outdoor.light.load.W=rowSums(na_seadec_nur1[,c(7,13,19)]))

na_seadec_nur2 <- na_seadec_nur[, c(1:4, which(grepl("Nur.2", colnames(na_seadec_nur), fixed=TRUE)))]
na_seadec_nur2 <- na_seadec_nur2 %>% mutate(Actual.User.Load.W=rowSums(na_seadec_nur2[,c(5:26)]))
# Outdoor lights: LED 3 in CPE7, CPE9, CPE12
na_seadec_nur2 <- na_seadec_nur2 %>% mutate(Socket.load.W=rowSums(na_seadec_nur2[,c(23:26)]),
                                            Indoor.light.load.W=rowSums(na_seadec_nur2[,c(5:6,8:12,14:21)]),
                                            Outdoor.light.load.W=rowSums(na_seadec_nur2[,c(7,13,22)]))

na_seadec_pg <- na_seadec_sub[,c(1:3,74,which(grepl("Playground", colnames(na_seadec_sub), fixed=TRUE)))]
na_seadec_pg <- na_seadec_pg %>% mutate(Actual.User.Load.W=rowSums(na_seadec_pg[,c(5:21)]),
                                        Predicted.User.Load.W=rep(pg_predicted$User.Load.W,274))
na_seadec_pg <- na_seadec_pg %>% mutate(Socket.load.W=rowSums(na_seadec_pg[,c(20:21)]))
na_seadec_pg <- na_seadec_pg %>% mutate(Light.load.W=rowSums(na_seadec_pg[,c(5:19)]))

na_seadec_sl <- na_seadec_sub[,c(1:3,74,which(grepl("Streetlight", colnames(na_seadec_sub), fixed=TRUE)))]
na_seadec_sl <- na_seadec_sl %>% mutate(Light.load.W=rowSums(na_seadec_sl[,c(5:13)]))

#Read in daily data - 2nd July 2019 to 31st Mar 2020
system_daily <- read.csv(here(filepath,"system_daily_correctedData.csv"), header=TRUE, stringsAsFactors=FALSE)
system_daily <- system_daily %>% mutate(date=as.Date(date), month=factor(month, levels = MONTHS,labels = MONTHS))

#Read in monthly data 
system_monthly <- read.csv(here(filepath,"monthly_avg_correctedData.csv"), header=TRUE, stringsAsFactors = FALSE)
system_monthly <- system_monthly %>% mutate(month=factor(month, levels = MONTHS,labels = MONTHS))

# Predicted load 
pg_predicted <- data.frame(month=rep("Predicted",24),timeUse = c(0:23), 
                           User.Load.W=c(0,0,0,0,0,0,0.32,0.32,0.11,0.05,0.02,0.02,
                                         0.02,0.02,0.02,0.02,0.02,0.06,0.12,0.32,
                                         0.32,0.32,0.32,0)*1000.0,stringsAsFactors = FALSE)

nur_predicted <- data.frame(month=rep("Predicted",24),timeUse = c(0:23), 
                            User.Load.W=c(0,0,0,0,0,0,0.24,0.24,0.24,0.24,0,0,
                                          0.355,0.355,0,0.24,0.24,0.24,0.24,0.24,
                                          0.24,0.24,0,0)*1000.0, stringsAsFactors = FALSE)
#******************************************************************************************#

#******************************************************************************************#
#1. How do refugees use energy and what are their energy needs, patterns of use and aspirations?
#1.a Hourly energy consumption (all lights + sockets from nursery, playground and SL) for week/weekend in (month)
plotLoad1 <- function(df) {
  ggplot(df, aes(as.factor(timeUse), Actual.User.Load.W/1000)) + geom_boxplot() + THEME + 
    labs(y="Actual User Load (kW)", x = "Time of day") + scale_x_discrete(breaks=seq(0,24,by=2))
}
# energy consumption in Jul
plotLoad1(na_seadec_sub[na_seadec_sub$month=="Jul" & na_seadec_sub$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.18,0.03))
ggsave(here(plot_dir,"userLoad_jul_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad1(na_seadec_sub[na_seadec_sub$month=="Jul" & na_seadec_sub$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.18,0.03))
ggsave(here(plot_dir,"userLoad_jul_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Aug
plotLoad1(na_seadec_sub[na_seadec_sub$month=="Aug" & na_seadec_sub$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.18,0.03))
ggsave(here(plot_dir,"userLoad_aug_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad1(na_seadec_sub[na_seadec_sub$month=="Aug" & na_seadec_sub$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.18,0.03))
ggsave(here(plot_dir,"userLoad_aug_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Sep
plotLoad1(na_seadec_sub[na_seadec_sub$month=="Sep" & na_seadec_sub$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.18,0.03))
ggsave(here(plot_dir,"userLoad_sep_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad1(na_seadec_sub[na_seadec_sub$month=="Sep" & na_seadec_sub$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.18,0.03))
ggsave(here(plot_dir,"userLoad_sep_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Oct
plotLoad1(na_seadec_sub[na_seadec_sub$month=="Oct" & na_seadec_sub$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.18,0.03))
ggsave(here(plot_dir,"userLoad_oct_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad1(na_seadec_sub[na_seadec_sub$month=="Oct" & na_seadec_sub$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.18,0.03))
ggsave(here(plot_dir,"userLoad_oct_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Nov
plotLoad1(na_seadec_sub[na_seadec_sub$month=="Nov" & na_seadec_sub$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.18,0.03))
ggsave(here(plot_dir,"userLoad_nov_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad1(na_seadec_sub[na_seadec_sub$month=="Nov" & na_seadec_sub$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.18,0.03))
ggsave(here(plot_dir,"userLoad_nov_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Dec
plotLoad1(na_seadec_sub[na_seadec_sub$month=="Dec" & na_seadec_sub$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.18,0.03))
ggsave(here(plot_dir,"userLoad_dec_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad1(na_seadec_sub[na_seadec_sub$month=="Dec" & na_seadec_sub$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.18,0.03))
ggsave(here(plot_dir,"userLoad_dec_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Jan
plotLoad1(na_seadec_sub[na_seadec_sub$month=="Jan" & na_seadec_sub$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.18,0.03))
ggsave(here(plot_dir,"userLoad_jan_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad1(na_seadec_sub[na_seadec_sub$month=="Jan" & na_seadec_sub$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.18,0.03))
ggsave(here(plot_dir,"userLoad_jan_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Feb
plotLoad1(na_seadec_sub[na_seadec_sub$month=="Feb" & na_seadec_sub$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.7,0.1))
ggsave(here(plot_dir,"userLoad_feb_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad1(na_seadec_sub[na_seadec_sub$month=="Feb" & na_seadec_sub$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.18,0.03))
ggsave(here(plot_dir,"userLoad_feb_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Mar
plotLoad1(na_seadec_sub[na_seadec_sub$month=="Mar" & na_seadec_sub$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.4,0.1))
ggsave(here(plot_dir,"userLoad_mar_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad1(na_seadec_sub[na_seadec_sub$month=="Mar" & na_seadec_sub$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.18,0.03))
ggsave(here(plot_dir,"userLoad_mar_wend_tod.pdf"), width = 8, height = 6, units = "cm")

#1.b Hourly energy consumption (all lights + sockets from nursery and playground) for week/weekend in (month)
plotLoad2 <- function(df) {
  ggplot(df, aes(as.factor(timeUse), Actual.User.Load.NP.W/1000)) + geom_boxplot() + THEME + 
    labs(y="Actual User Load (kW)", x = "Time of day") + scale_x_discrete(breaks=seq(0,24,by=2))
}
# energy consumption in Jul
plotLoad2(na_seadec_sub[na_seadec_sub$month=="Jul" & na_seadec_sub$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.14,0.02))
ggsave(here(plot_dir,"actualLoad_jul_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad2(na_seadec_sub[na_seadec_sub$month=="Jul" & na_seadec_sub$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.14,0.02))
ggsave(here(plot_dir,"actualLoad_jul_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Aug
plotLoad2(na_seadec_sub[na_seadec_sub$month=="Aug" & na_seadec_sub$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.14,0.02))
ggsave(here(plot_dir,"actualLoad_aug_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad2(na_seadec_sub[na_seadec_sub$month=="Aug" & na_seadec_sub$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.14,0.02))
ggsave(here(plot_dir,"actualLoad_aug_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Sep
plotLoad2(na_seadec_sub[na_seadec_sub$month=="Sep" & na_seadec_sub$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.14,0.02))
ggsave(here(plot_dir,"actualLoad_sep_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad2(na_seadec_sub[na_seadec_sub$month=="Sep" & na_seadec_sub$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.14,0.02))
ggsave(here(plot_dir,"actualLoad_sep_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Oct
plotLoad2(na_seadec_sub[na_seadec_sub$month=="Oct" & na_seadec_sub$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.14,0.02))
ggsave(here(plot_dir,"actualLoad_oct_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad2(na_seadec_sub[na_seadec_sub$month=="Oct" & na_seadec_sub$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.14,0.02))
ggsave(here(plot_dir,"actualLoad_oct_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Nov
plotLoad2(na_seadec_sub[na_seadec_sub$month=="Nov" & na_seadec_sub$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.14,0.02))
ggsave(here(plot_dir,"actualLoad_nov_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad2(na_seadec_sub[na_seadec_sub$month=="Nov" & na_seadec_sub$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.14,0.02))
ggsave(here(plot_dir,"actualLoad_nov_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Dec
plotLoad2(na_seadec_sub[na_seadec_sub$month=="Dec" & na_seadec_sub$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.14,0.02))
ggsave(here(plot_dir,"actualLoad_dec_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad2(na_seadec_sub[na_seadec_sub$month=="Dec" & na_seadec_sub$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.14,0.02))
ggsave(here(plot_dir,"actualLoad_dec_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Jan
plotLoad2(na_seadec_sub[na_seadec_sub$month=="Jan" & na_seadec_sub$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.14,0.02))
ggsave(here(plot_dir,"actualLoad_jan_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad2(na_seadec_sub[na_seadec_sub$month=="Jan" & na_seadec_sub$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.14,0.02))
ggsave(here(plot_dir,"actualLoad_jan_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Feb
plotLoad2(na_seadec_sub[na_seadec_sub$month=="Feb" & na_seadec_sub$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.7,0.1))
ggsave(here(plot_dir,"actualLoad_feb_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad2(na_seadec_sub[na_seadec_sub$month=="Feb" & na_seadec_sub$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.12,0.02))
ggsave(here(plot_dir,"actualLoad_feb_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in March
plotLoad2(na_seadec_sub[na_seadec_sub$month=="Mar" & na_seadec_sub$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.7,0.1))
ggsave(here(plot_dir,"actualLoad_mar_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad2(na_seadec_sub[na_seadec_sub$month=="Mar" & na_seadec_sub$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.12,0.02))
ggsave(here(plot_dir,"actualLoad_mar_wend_tod.pdf"), width = 8, height = 6, units = "cm")

#1.c Hourly energy consumption at Nursery 1 for a weekend/weekday in (month)
# energy consumption in Jul
plotLoad1(na_seadec_nur1[na_seadec_nur1$month=="Jul" & na_seadec_nur1$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.04,0.005))
ggsave(here(plot_dir,"nur1_userLoad_jul_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad1(na_seadec_nur1[na_seadec_nur1$month=="Jul" & na_seadec_nur1$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.02,0.005))
ggsave(here(plot_dir,"nur1_userLoad_jul_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Aug
plotLoad1(na_seadec_nur1[na_seadec_nur1$month=="Aug" & na_seadec_nur1$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.04,0.005))
ggsave(here(plot_dir,"nur1_userLoad_aug_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad1(na_seadec_nur1[na_seadec_nur1$month=="Aug" & na_seadec_nur1$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.02,0.005))
ggsave(here(plot_dir,"nur1_userLoad_aug_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Sep
plotLoad1(na_seadec_nur1[na_seadec_nur1$month=="Sep" & na_seadec_nur1$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.02,0.005))
ggsave(here(plot_dir,"nur1_userLoad_sep_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad1(na_seadec_nur1[na_seadec_nur1$month=="Sep" & na_seadec_nur1$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.02,0.005))
ggsave(here(plot_dir,"nur1_userLoad_sep_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Oct
plotLoad1(na_seadec_nur1[na_seadec_nur1$month=="Oct" & na_seadec_nur1$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.025,0.005))
ggsave(here(plot_dir,"nur1_userLoad_oct_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad1(na_seadec_nur1[na_seadec_nur1$month=="Oct" & na_seadec_nur1$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.02,0.005))
ggsave(here(plot_dir,"nur1_userLoad_oct_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Nov
plotLoad1(na_seadec_nur1[na_seadec_nur1$month=="Nov" & na_seadec_nur1$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.025,0.005))
ggsave(here(plot_dir,"nur1_userLoad_nov_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad1(na_seadec_nur1[na_seadec_nur1$month=="Nov" & na_seadec_nur1$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.02,0.005))
ggsave(here(plot_dir,"nur1_userLoad_nov_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Dec
plotLoad1(na_seadec_nur1[na_seadec_nur1$month=="Dec" & na_seadec_nur1$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.025,0.005))
ggsave(here(plot_dir,"nur1_userLoad_dec_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad1(na_seadec_nur1[na_seadec_nur1$month=="Dec" & na_seadec_nur1$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.02,0.005))
ggsave(here(plot_dir,"nur1_userLoad_dec_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Jan
plotLoad1(na_seadec_nur1[na_seadec_nur1$month=="Jan" & na_seadec_nur1$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.04,0.01))
ggsave(here(plot_dir,"nur1_userLoad_jan_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad1(na_seadec_nur1[na_seadec_nur1$month=="Jan" & na_seadec_nur1$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.02,0.005))
ggsave(here(plot_dir,"nur1_userLoad_jan_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Feb
plotLoad1(na_seadec_nur1[na_seadec_nur1$month=="Feb" & na_seadec_nur1$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.5,0.1))
ggsave(here(plot_dir,"nur1_userLoad_feb_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad1(na_seadec_nur1[na_seadec_nur1$month=="Feb" & na_seadec_nur1$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.02,0.005))
ggsave(here(plot_dir,"nur1_userLoad_feb_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in March
plotLoad1(na_seadec_nur1[na_seadec_nur1$month=="Mar" & na_seadec_nur1$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.4,0.1))
ggsave(here(plot_dir,"nur1_userLoad_mar_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad1(na_seadec_nur1[na_seadec_nur1$month=="Mar" & na_seadec_nur1$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.06,0.01))
ggsave(here(plot_dir,"nur1_userLoad_mar_wend_tod.pdf"), width = 8, height = 6, units = "cm")

#1.d Hourly energy consumption at Nursery 2 for a weekend/weekday in (month)
# energy consumption in Jul
plotLoad1(na_seadec_nur2[na_seadec_nur2$month=="Jul" & na_seadec_nur2$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.05,0.01))
ggsave(here(plot_dir,"nur2_userLoad_jul_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad1(na_seadec_nur2[na_seadec_nur2$month=="Jul" & na_seadec_nur2$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.05,0.01))
ggsave(here(plot_dir,"nur2_userLoad_jul_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Aug
plotLoad1(na_seadec_nur2[na_seadec_nur2$month=="Aug" & na_seadec_nur2$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.06,0.01))
ggsave(here(plot_dir,"nur2_userLoad_aug_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad1(na_seadec_nur2[na_seadec_nur2$month=="Aug" & na_seadec_nur2$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.05,0.01))
ggsave(here(plot_dir,"nur2_userLoad_aug_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Sep
plotLoad1(na_seadec_nur2[na_seadec_nur2$month=="Sep" & na_seadec_nur2$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.06,0.01))
ggsave(here(plot_dir,"nur2_userLoad_sep_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad1(na_seadec_nur2[na_seadec_nur2$month=="Sep" & na_seadec_nur2$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.05,0.01))
ggsave(here(plot_dir,"nur2_userLoad_sep_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Oct
plotLoad1(na_seadec_nur2[na_seadec_nur2$month=="Oct" & na_seadec_nur2$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.06,0.01))
ggsave(here(plot_dir,"nur2_userLoad_oct_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad1(na_seadec_nur2[na_seadec_nur2$month=="Oct" & na_seadec_nur2$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.05,0.01))
ggsave(here(plot_dir,"nur2_userLoad_oct_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Nov
plotLoad1(na_seadec_nur2[na_seadec_nur2$month=="Nov" & na_seadec_nur2$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.02,0.005))
ggsave(here(plot_dir,"nur2_userLoad_nov_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad1(na_seadec_nur2[na_seadec_nur2$month=="Nov" & na_seadec_nur2$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.03,0.005))
ggsave(here(plot_dir,"nur2_userLoad_nov_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Dec
plotLoad1(na_seadec_nur2[na_seadec_nur2$month=="Dec" & na_seadec_nur2$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.02,0.005))
ggsave(here(plot_dir,"nur2_userLoad_dec_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad1(na_seadec_nur2[na_seadec_nur2$month=="Dec" & na_seadec_nur2$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.03,0.005))
ggsave(here(plot_dir,"nur2_userLoad_dec_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Jan
plotLoad1(na_seadec_nur2[na_seadec_nur2$month=="Jan" & na_seadec_nur2$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.02,0.005))
ggsave(here(plot_dir,"nur2_userLoad_jan_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad1(na_seadec_nur2[na_seadec_nur2$month=="Jan" & na_seadec_nur2$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.06,0.01))
ggsave(here(plot_dir,"nur2_userLoad_jan_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Feb
plotLoad1(na_seadec_nur2[na_seadec_nur2$month=="Feb" & na_seadec_nur2$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.03,0.005), limits=c(0,0.03))
ggsave(here(plot_dir,"nur2_userLoad_feb_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad1(na_seadec_nur2[na_seadec_nur2$month=="Feb" & na_seadec_nur2$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.03,0.005))
ggsave(here(plot_dir,"nur2_userLoad_feb_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Mar
plotLoad1(na_seadec_nur2[na_seadec_nur2$month=="Mar" & na_seadec_nur2$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.025,0.005))
ggsave(here(plot_dir,"nur2_userLoad_mar_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad1(na_seadec_nur2[na_seadec_nur2$month=="Mar" & na_seadec_nur2$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.08,0.01))
ggsave(here(plot_dir,"nur2_userLoad_mar_wend_tod.pdf"), width = 8, height = 6, units = "cm")

#1.e Hourly energy consumption at Playground for a weekend/weekday in (month)
# energy consumption in Jul
plotLoad1(na_seadec_pg[na_seadec_pg$month=="Jul" & na_seadec_pg$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.08,0.02))
ggsave(here(plot_dir,"pg_userLoad_jul_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad1(na_seadec_pg[na_seadec_pg$month=="Jul" & na_seadec_pg$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.08,0.02))
ggsave(here(plot_dir,"pg_userLoad_jul_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Aug
plotLoad1(na_seadec_pg[na_seadec_pg$month=="Aug" & na_seadec_pg$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.09,0.01))
ggsave(here(plot_dir,"pg_userLoad_aug_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad1(na_seadec_pg[na_seadec_pg$month=="Aug" & na_seadec_pg$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.09,0.01))
ggsave(here(plot_dir,"pg_userLoad_aug_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Sep
plotLoad1(na_seadec_pg[na_seadec_pg$month=="Sep" & na_seadec_pg$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.1,0.02))
ggsave(here(plot_dir,"pg_userLoad_sep_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad1(na_seadec_pg[na_seadec_pg$month=="Sep" & na_seadec_pg$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.1,0.02))
ggsave(here(plot_dir,"pg_userLoad_sep_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Oct
plotLoad1(na_seadec_pg[na_seadec_pg$month=="Oct" & na_seadec_pg$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.10,0.02))
ggsave(here(plot_dir,"pg_userLoad_oct_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad1(na_seadec_pg[na_seadec_pg$month=="Oct" & na_seadec_pg$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.10,0.02))
ggsave(here(plot_dir,"pg_userLoad_oct_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Nov
plotLoad1(na_seadec_pg[na_seadec_pg$month=="Nov" & na_seadec_pg$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.10,0.02))
ggsave(here(plot_dir,"pg_userLoad_nov_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad1(na_seadec_pg[na_seadec_pg$month=="Nov" & na_seadec_pg$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.10,0.02))
ggsave(here(plot_dir,"pg_userLoad_nov_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Dec
plotLoad1(na_seadec_pg[na_seadec_pg$month=="Dec" & na_seadec_pg$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.12,0.02))
ggsave(here(plot_dir,"pg_userLoad_dec_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad1(na_seadec_pg[na_seadec_pg$month=="Dec" & na_seadec_pg$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.12,0.02))
ggsave(here(plot_dir,"pg_userLoad_dec_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Jan
plotLoad1(na_seadec_pg[na_seadec_pg$month=="Jan" & na_seadec_pg$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.12,0.02))
ggsave(here(plot_dir,"pg_userLoad_jan_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad1(na_seadec_pg[na_seadec_pg$month=="Jan" & na_seadec_pg$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.12,0.02))
ggsave(here(plot_dir,"pg_userLoad_jan_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Feb
plotLoad1(na_seadec_pg[na_seadec_pg$month=="Feb" & na_seadec_pg$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.12,0.02))
ggsave(here(plot_dir,"pg_userLoad_feb_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad1(na_seadec_pg[na_seadec_pg$month=="Feb" & na_seadec_pg$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.08,0.01))
ggsave(here(plot_dir,"pg_userLoad_feb_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Mar
plotLoad1(na_seadec_pg[na_seadec_pg$month=="Mar" & na_seadec_pg$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.12,0.02))
ggsave(here(plot_dir,"pg_userLoad_mar_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad1(na_seadec_pg[na_seadec_pg$month=="Mar" & na_seadec_pg$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.09,0.01))
ggsave(here(plot_dir,"pg_userLoad_mar_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# Plot socket consumption
plotLoad3 <- function(df) {
  ggplot(df, aes(as.factor(timeUse), Socket.load.W/1000)) + geom_boxplot() + THEME + 
    labs(y="Socket Load (kW)", x = "Time of day") + scale_x_discrete(breaks=seq(0,24,by=2))
}

#1.f Hourly socket consumption at Nursery 1 for a weekend/weekday in (month)
# energy consumption in Jul
plotLoad3(na_seadec_nur1[na_seadec_nur1$month=="Jul" & na_seadec_nur1$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.018,0.003))
ggsave(here(plot_dir,"nur1_socketLoad_jul_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad3(na_seadec_nur1[na_seadec_nur1$month=="Jul" & na_seadec_nur1$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.018,0.003))
ggsave(here(plot_dir,"nur1_socketLoad_jul_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Aug
plotLoad3(na_seadec_nur1[na_seadec_nur1$month=="Aug" & na_seadec_nur1$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.018,0.003))
ggsave(here(plot_dir,"nur1_socketLoad_aug_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad3(na_seadec_nur1[na_seadec_nur1$month=="Aug" & na_seadec_nur1$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.018,0.003))
ggsave(here(plot_dir,"nur1_socketLoad_aug_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Sep
plotLoad3(na_seadec_nur1[na_seadec_nur1$month=="Sep" & na_seadec_nur1$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.018,0.002))
ggsave(here(plot_dir,"nur1_socketLoad_sep_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad3(na_seadec_nur1[na_seadec_nur1$month=="Sep" & na_seadec_nur1$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.018,0.001))
ggsave(here(plot_dir,"nur1_socketLoad_sep_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Oct
plotLoad3(na_seadec_nur1[na_seadec_nur1$month=="Oct" & na_seadec_nur1$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.010,0.002))
ggsave(here(plot_dir,"nur1_socketLoad_oct_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad3(na_seadec_nur1[na_seadec_nur1$month=="Oct" & na_seadec_nur1$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.010,0.002))
ggsave(here(plot_dir,"nur1_socketLoad_oct_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Nov
plotLoad3(na_seadec_nur1[na_seadec_nur1$month=="Nov" & na_seadec_nur1$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.018,0.002))
ggsave(here(plot_dir,"nur1_socketLoad_nov_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad3(na_seadec_nur1[na_seadec_nur1$month=="Nov" & na_seadec_nur1$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.018,0.002))
ggsave(here(plot_dir,"nur1_socketLoad_nov_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Dec
plotLoad3(na_seadec_nur1[na_seadec_nur1$month=="Dec" & na_seadec_nur1$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.018,0.002))
ggsave(here(plot_dir,"nur1_socketLoad_dec_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad3(na_seadec_nur1[na_seadec_nur1$month=="Dec" & na_seadec_nur1$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.018,0.002))
ggsave(here(plot_dir,"nur1_socketLoad_dec_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Jan
plotLoad3(na_seadec_nur1[na_seadec_nur1$month=="Jan" & na_seadec_nur1$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.018,0.002))
ggsave(here(plot_dir,"nur1_socketLoad_jan_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad3(na_seadec_nur1[na_seadec_nur1$month=="Jan" & na_seadec_nur1$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.018,0.002))
ggsave(here(plot_dir,"nur1_socketLoad_jan_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Feb
plotLoad3(na_seadec_nur1[na_seadec_nur1$month=="Feb" & na_seadec_nur1$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.5,0.1))
ggsave(here(plot_dir,"nur1_socketLoad_feb_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad3(na_seadec_nur1[na_seadec_nur1$month=="Feb" & na_seadec_nur1$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.018,0.002))
ggsave(here(plot_dir,"nur1_socketLoad_feb_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Mar
plotLoad3(na_seadec_nur1[na_seadec_nur1$month=="Mar" & na_seadec_nur1$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.05,0.01), limits=c(0,0.05))
ggsave(here(plot_dir,"nur1_socketLoad_mar_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad3(na_seadec_nur1[na_seadec_nur1$month=="Mar" & na_seadec_nur1$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.05,0.01))
ggsave(here(plot_dir,"nur1_socketLoad_mar_wend_tod.pdf"), width = 8, height = 6, units = "cm")

#1.g Hourly socket consumption at Nursery 2 for a weekend/weekday in (month)
# energy consumption in Jul
plotLoad3(na_seadec_nur2[na_seadec_nur2$month=="Jul" & na_seadec_nur2$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.021,0.002))
ggsave(here(plot_dir,"nur2_socketLoad_jul_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad3(na_seadec_nur2[na_seadec_nur2$month=="Jul" & na_seadec_nur2$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.01,0.002))
ggsave(here(plot_dir,"nur2_socketLoad_jul_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Aug
plotLoad3(na_seadec_nur2[na_seadec_nur2$month=="Aug" & na_seadec_nur2$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.04,0.005))
ggsave(here(plot_dir,"nur2_socketLoad_aug_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad3(na_seadec_nur2[na_seadec_nur2$month=="Aug" & na_seadec_nur2$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.02,0.005))
ggsave(here(plot_dir,"nur2_socketLoad_aug_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Sep
plotLoad3(na_seadec_nur2[na_seadec_nur2$month=="Sep" & na_seadec_nur2$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.04,0.005))
ggsave(here(plot_dir,"nur2_socketLoad_sep_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad3(na_seadec_nur2[na_seadec_nur2$month=="Sep" & na_seadec_nur2$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.02,0.005))
ggsave(here(plot_dir,"nur2_socketLoad_sep_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Oct
plotLoad3(na_seadec_nur2[na_seadec_nur2$month=="Oct" & na_seadec_nur2$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.04,0.005))
ggsave(here(plot_dir,"nur2_socketLoad_oct_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad3(na_seadec_nur2[na_seadec_nur2$month=="Oct" & na_seadec_nur2$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.02,0.005))
ggsave(here(plot_dir,"nur2_socketLoad_oct_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Nov
plotLoad3(na_seadec_nur2[na_seadec_nur2$month=="Nov" & na_seadec_nur2$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.04,0.005))
ggsave(here(plot_dir,"nur2_socketLoad_nov_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad3(na_seadec_nur2[na_seadec_nur2$month=="Nov" & na_seadec_nur2$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.02,0.005))
ggsave(here(plot_dir,"nur2_socketLoad_nov_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Dec
plotLoad3(na_seadec_nur2[na_seadec_nur2$month=="Dec" & na_seadec_nur2$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.04,0.001))
ggsave(here(plot_dir,"nur2_socketLoad_dec_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad3(na_seadec_nur2[na_seadec_nur2$month=="Dec" & na_seadec_nur2$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.02,0.001))
ggsave(here(plot_dir,"nur2_socketLoad_dec_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Jan
plotLoad3(na_seadec_nur2[na_seadec_nur2$month=="Jan" & na_seadec_nur2$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.04,0.005))
ggsave(here(plot_dir,"nur2_socketLoad_jan_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad3(na_seadec_nur2[na_seadec_nur2$month=="Jan" & na_seadec_nur2$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.05,0.005))
ggsave(here(plot_dir,"nur2_socketLoad_jan_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Feb
plotLoad3(na_seadec_nur2[na_seadec_nur2$month=="Feb" & na_seadec_nur2$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.04,0.005), limits=c(0,0.02))
ggsave(here(plot_dir,"nur2_socketLoad_feb_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad3(na_seadec_nur2[na_seadec_nur2$month=="Feb" & na_seadec_nur2$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.08,0.001))
ggsave(here(plot_dir,"nur2_socketLoad_feb_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Mar
plotLoad3(na_seadec_nur2[na_seadec_nur2$month=="Mar" & na_seadec_nur2$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.04,0.005))
ggsave(here(plot_dir,"nur2_socketLoad_mar_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad3(na_seadec_nur2[na_seadec_nur2$month=="Mar" & na_seadec_nur2$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.1,0.01))
ggsave(here(plot_dir,"nur2_socketLoad_mar_wend_tod.pdf"), width = 8, height = 6, units = "cm")

#1.h Hourly socket consumption at the Playground for a weekend/weekday in (month)
# energy consumption in Jul
plotLoad3(na_seadec_pg[na_seadec_pg$month=="Jul" & na_seadec_pg$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.03,0.005))
ggsave(here(plot_dir,"pg_socketLoad_jul_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad3(na_seadec_pg[na_seadec_pg$month=="Jul" & na_seadec_pg$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.02,0.005))
ggsave(here(plot_dir,"pg_socketLoad_jul_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Aug
plotLoad3(na_seadec_pg[na_seadec_pg$month=="Aug" & na_seadec_pg$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.09,0.01))
ggsave(here(plot_dir,"pg_socketLoad_aug_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad3(na_seadec_pg[na_seadec_pg$month=="Aug" & na_seadec_pg$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.03,0.005))
ggsave(here(plot_dir,"pg_socketLoad_aug_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Sep
plotLoad3(na_seadec_pg[na_seadec_pg$month=="Sep" & na_seadec_pg$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.03,0.005))
ggsave(here(plot_dir,"pg_socketLoad_sep_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad3(na_seadec_pg[na_seadec_pg$month=="Sep" & na_seadec_pg$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.03,0.005))
ggsave(here(plot_dir,"pg_socketLoad_sep_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Oct
plotLoad3(na_seadec_pg[na_seadec_pg$month=="Oct" & na_seadec_pg$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.05,0.005))
ggsave(here(plot_dir,"pg_socketLoad_oct_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad3(na_seadec_pg[na_seadec_pg$month=="Oct" & na_seadec_pg$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.05,0.005))
ggsave(here(plot_dir,"pg_socketLoad_oct_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Nov
plotLoad3(na_seadec_pg[na_seadec_pg$month=="Nov" & na_seadec_pg$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.05,0.005))
ggsave(here(plot_dir,"pg_socketLoad_nov_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad3(na_seadec_pg[na_seadec_pg$month=="Nov" & na_seadec_pg$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.05,0.005))
ggsave(here(plot_dir,"pg_socketLoad_nov_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Dec
plotLoad3(na_seadec_pg[na_seadec_pg$month=="Dec" & na_seadec_pg$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.06,0.01))
ggsave(here(plot_dir,"pg_socketLoad_dec_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad3(na_seadec_pg[na_seadec_pg$month=="Dec" & na_seadec_pg$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.06,0.01))
ggsave(here(plot_dir,"pg_socketLoad_dec_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Jan
plotLoad3(na_seadec_pg[na_seadec_pg$month=="Jan" & na_seadec_pg$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.06,0.01))
ggsave(here(plot_dir,"pg_socketLoad_jan_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad3(na_seadec_pg[na_seadec_pg$month=="Jan" & na_seadec_pg$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.06,0.01))
ggsave(here(plot_dir,"pg_socketLoad_jan_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Feb
plotLoad3(na_seadec_pg[na_seadec_pg$month=="Feb" & na_seadec_pg$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.06,0.005))
ggsave(here(plot_dir,"pg_socketLoad_feb_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad3(na_seadec_pg[na_seadec_pg$month=="Feb" & na_seadec_pg$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.06,0.005))
ggsave(here(plot_dir,"pg_socketLoad_feb_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Mar
plotLoad3(na_seadec_pg[na_seadec_pg$month=="Mar" & na_seadec_pg$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.12,0.02))
ggsave(here(plot_dir,"pg_socketLoad_mar_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad3(na_seadec_pg[na_seadec_pg$month=="Mar" & na_seadec_pg$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.12,0.02))
ggsave(here(plot_dir,"pg_socketLoad_mar_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# Plot indoor light consumption
plotLoad4 <- function(df) {
  ggplot(df, aes(as.factor(timeUse), Indoor.light.load.W/1000)) + geom_boxplot() + THEME + 
    labs(y="Indoor Light Load (kW)", x = "Time of day") + scale_x_discrete(breaks=seq(0,24,by=2))
}

#1.i Hourly indoor light consumption at Nursery 1 for a weekend/weekday in (month)
# energy consumption in Jul
plotLoad4(na_seadec_nur1[na_seadec_nur1$month=="Jul" & na_seadec_nur1$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.03,0.005))
ggsave(here(plot_dir,"nur1_indoorLoad_jul_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad4(na_seadec_nur1[na_seadec_nur1$month=="Jul" & na_seadec_nur1$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.02,0.005))
ggsave(here(plot_dir,"nur1_indoorLoad_jul_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Aug
plotLoad4(na_seadec_nur1[na_seadec_nur1$month=="Aug" & na_seadec_nur1$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.03,0.005))
ggsave(here(plot_dir,"nur1_indoorLoad_aug_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad4(na_seadec_nur1[na_seadec_nur1$month=="Aug" & na_seadec_nur1$weekday%in%c("Sat","Sun"),]) 
ggsave(here(plot_dir,"nur1_indoorLoad_aug_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Sep
plotLoad4(na_seadec_nur1[na_seadec_nur1$month=="Sep" & na_seadec_nur1$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.03,0.005))
ggsave(here(plot_dir,"nur1_indoorLoad_sep_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad4(na_seadec_nur1[na_seadec_nur1$month=="Sep" & na_seadec_nur1$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.02,0.002))
ggsave(here(plot_dir,"nur1_indoorLoad_sep_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Oct
plotLoad4(na_seadec_nur1[na_seadec_nur1$month=="Oct" & na_seadec_nur1$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.03,0.005))
ggsave(here(plot_dir,"nur1_indoorLoad_oct_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad4(na_seadec_nur1[na_seadec_nur1$month=="Oct" & na_seadec_nur1$weekday%in%c("Sat","Sun"),]) 
ggsave(here(plot_dir,"nur1_indoorLoad_oct_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Nov
plotLoad4(na_seadec_nur1[na_seadec_nur1$month=="Nov" & na_seadec_nur1$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.03,0.001))
ggsave(here(plot_dir,"nur1_indoorLoad_nov_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad4(na_seadec_nur1[na_seadec_nur1$month=="Nov" & na_seadec_nur1$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.02,0.005))
ggsave(here(plot_dir,"nur1_indoorLoad_nov_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Dec
plotLoad4(na_seadec_nur1[na_seadec_nur1$month=="Dec" & na_seadec_nur1$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.03,0.001))
ggsave(here(plot_dir,"nur1_indoorLoad_dec_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad4(na_seadec_nur1[na_seadec_nur1$month=="Dec" & na_seadec_nur1$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.02,0.001))
ggsave(here(plot_dir,"nur1_indoorLoad_dec_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Jan
plotLoad4(na_seadec_nur1[na_seadec_nur1$month=="Jan" & na_seadec_nur1$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.05,0.005))
ggsave(here(plot_dir,"nur1_indoorLoad_jan_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad4(na_seadec_nur1[na_seadec_nur1$month=="Jan" & na_seadec_nur1$weekday%in%c("Sat","Sun"),]) 
ggsave(here(plot_dir,"nur1_indoorLoad_jan_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Feb
plotLoad4(na_seadec_nur1[na_seadec_nur1$month=="Feb" & na_seadec_nur1$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.03,0.005))
ggsave(here(plot_dir,"nur1_indoorLoad_feb_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad4(na_seadec_nur1[na_seadec_nur1$month=="Feb" & na_seadec_nur1$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.02,0.001))
ggsave(here(plot_dir,"nur1_indoorLoad_feb_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Mar
plotLoad4(na_seadec_nur1[na_seadec_nur1$month=="Mar" & na_seadec_nur1$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.03,0.001))
ggsave(here(plot_dir,"nur1_indoorLoad_mar_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad4(na_seadec_nur1[na_seadec_nur1$month=="Mar" & na_seadec_nur1$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.02,0.001))
ggsave(here(plot_dir,"nur1_indoorLoad_mar_wend_tod.pdf"), width = 8, height = 6, units = "cm")

#1.j Hourly indoor light consumption at Nursery 2 for a weekend/weekday in (month)
# energy consumption in Jul
plotLoad4(na_seadec_nur2[na_seadec_nur2$month=="Jul" & na_seadec_nur2$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.03,0.005))
ggsave(here(plot_dir,"nur2_indoorLoad_jul_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad4(na_seadec_nur2[na_seadec_nur2$month=="Jul" & na_seadec_nur2$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.04,0.005))
ggsave(here(plot_dir,"nur2_indoorLoad_jul_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Aug
plotLoad4(na_seadec_nur2[na_seadec_nur2$month=="Aug" & na_seadec_nur2$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.03,0.005))
ggsave(here(plot_dir,"nur2_indoorLoad_aug_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad4(na_seadec_nur2[na_seadec_nur2$month=="Aug" & na_seadec_nur2$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.03,0.005))
ggsave(here(plot_dir,"nur2_indoorLoad_aug_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Sep
plotLoad4(na_seadec_nur2[na_seadec_nur2$month=="Sep" & na_seadec_nur2$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.03,0.005))
ggsave(here(plot_dir,"nur2_indoorLoad_sep_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad4(na_seadec_nur2[na_seadec_nur2$month=="Sep" & na_seadec_nur2$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.03,0.005))
ggsave(here(plot_dir,"nur2_indoorLoad_sep_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Oct
plotLoad4(na_seadec_nur2[na_seadec_nur2$month=="Oct" & na_seadec_nur2$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.03,0.005))
ggsave(here(plot_dir,"nur2_indoorLoad_oct_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad4(na_seadec_nur2[na_seadec_nur2$month=="Oct" & na_seadec_nur2$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.03,0.005))
ggsave(here(plot_dir,"nur2_indoorLoad_oct_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Nov
plotLoad4(na_seadec_nur2[na_seadec_nur2$month=="Nov" & na_seadec_nur2$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.03,0.001))
ggsave(here(plot_dir,"nur2_indoorLoad_nov_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad4(na_seadec_nur2[na_seadec_nur2$month=="Nov" & na_seadec_nur2$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.03,0.001))
ggsave(here(plot_dir,"nur2_indoorLoad_nov_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Dec
plotLoad4(na_seadec_nur2[na_seadec_nur2$month=="Dec" & na_seadec_nur2$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.03,0.001))
ggsave(here(plot_dir,"nur2_indoorLoad_dec_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad4(na_seadec_nur2[na_seadec_nur2$month=="Dec" & na_seadec_nur2$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.03,0.001))
ggsave(here(plot_dir,"nur2_indoorLoad_dec_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Jan
plotLoad4(na_seadec_nur2[na_seadec_nur2$month=="Jan" & na_seadec_nur2$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.03,0.001))
ggsave(here(plot_dir,"nur2_indoorLoad_jan_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad4(na_seadec_nur2[na_seadec_nur2$month=="Jan" & na_seadec_nur2$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.03,0.001))
ggsave(here(plot_dir,"nur2_indoorLoad_jan_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Feb
plotLoad4(na_seadec_nur2[na_seadec_nur2$month=="Feb" & na_seadec_nur2$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.03,0.002))
ggsave(here(plot_dir,"nur2_indoorLoad_feb_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad4(na_seadec_nur2[na_seadec_nur2$month=="Feb" & na_seadec_nur2$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.03,0.001))
ggsave(here(plot_dir,"nur2_indoorLoad_feb_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Mar
plotLoad4(na_seadec_nur2[na_seadec_nur2$month=="Mar" & na_seadec_nur2$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.03,0.002))
ggsave(here(plot_dir,"nur2_indoorLoad_mar_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad4(na_seadec_nur2[na_seadec_nur2$month=="Mar" & na_seadec_nur2$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.02,0.001))
ggsave(here(plot_dir,"nur2_indoorLoad_mar_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# Plot outdoor light consumption
plotLoad5 <- function(df) {
  ggplot(df, aes(as.factor(timeUse), Outdoor.light.load.W/1000)) + geom_boxplot() + THEME + 
    labs(y="Outdoor Light Load (kW)", x = "Time of day") + scale_x_discrete(breaks=seq(0,24,by=2))
}

#1.k Hourly outdoor light consumption at Nursery 1 for a weekend/weekday in (month)
# energy consumption in Jul
plotLoad5(na_seadec_nur1[na_seadec_nur1$month=="Jul" & na_seadec_nur1$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.03,0.005))
ggsave(here(plot_dir,"nur1_outdoorLoad_jul_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad5(na_seadec_nur1[na_seadec_nur1$month=="Jul" & na_seadec_nur1$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.02,0.005))
ggsave(here(plot_dir,"nur1_outdoorLoad_jul_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Aug
plotLoad5(na_seadec_nur1[na_seadec_nur1$month=="Aug" & na_seadec_nur1$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.03,0.005))
ggsave(here(plot_dir,"nur1_outdoorLoad_aug_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad5(na_seadec_nur1[na_seadec_nur1$month=="Aug" & na_seadec_nur1$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.02,0.005))
ggsave(here(plot_dir,"nur1_outdoorLoad_aug_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Sep
plotLoad5(na_seadec_nur1[na_seadec_nur1$month=="Sep" & na_seadec_nur1$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.03,0.005))
ggsave(here(plot_dir,"nur1_outdoorLoad_sep_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad5(na_seadec_nur1[na_seadec_nur1$month=="Sep" & na_seadec_nur1$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.02,0.005))
ggsave(here(plot_dir,"nur1_outdoorLoad_sep_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Oct
plotLoad5(na_seadec_nur1[na_seadec_nur1$month=="Oct" & na_seadec_nur1$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.03,0.005))
ggsave(here(plot_dir,"nur1_outdoorLoad_oct_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad5(na_seadec_nur1[na_seadec_nur1$month=="Oct" & na_seadec_nur1$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.02,0.005))
ggsave(here(plot_dir,"nur1_outdoorLoad_oct_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Nov
plotLoad5(na_seadec_nur1[na_seadec_nur1$month=="Nov" & na_seadec_nur1$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.03,0.005))
ggsave(here(plot_dir,"nur1_outdoorLoad_nov_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad5(na_seadec_nur1[na_seadec_nur1$month=="Nov" & na_seadec_nur1$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.02,0.005))
ggsave(here(plot_dir,"nur1_outdoorLoad_nov_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Dec
plotLoad5(na_seadec_nur1[na_seadec_nur1$month=="Dec" & na_seadec_nur1$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.03,0.005))
ggsave(here(plot_dir,"nur1_outdoorLoad_dec_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad5(na_seadec_nur1[na_seadec_nur1$month=="Dec" & na_seadec_nur1$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.02,0.005))
ggsave(here(plot_dir,"nur1_outdoorLoad_dec_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Jan
plotLoad5(na_seadec_nur1[na_seadec_nur1$month=="Jan" & na_seadec_nur1$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.03,0.005))
ggsave(here(plot_dir,"nur1_outdoorLoad_jan_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad5(na_seadec_nur1[na_seadec_nur1$month=="Jan" & na_seadec_nur1$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.02,0.005))
ggsave(here(plot_dir,"nur1_outdoorLoad_jan_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Feb
plotLoad5(na_seadec_nur1[na_seadec_nur1$month=="Feb" & na_seadec_nur1$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.03,0.005))
ggsave(here(plot_dir,"nur1_outdoorLoad_feb_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad5(na_seadec_nur1[na_seadec_nur1$month=="Feb" & na_seadec_nur1$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.02,0.005))
ggsave(here(plot_dir,"nur1_outdoorLoad_feb_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Mar
plotLoad5(na_seadec_nur1[na_seadec_nur1$month=="Mar" & na_seadec_nur1$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.03,0.005))
ggsave(here(plot_dir,"nur1_outdoorLoad_mar_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad5(na_seadec_nur1[na_seadec_nur1$month=="Mar" & na_seadec_nur1$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.02,0.005))
ggsave(here(plot_dir,"nur1_outdoorLoad_mar_wend_tod.pdf"), width = 8, height = 6, units = "cm")

#1.l Hourly outdoor light consumption at Nursery 2 for a weekend/weekday in (month)
# energy consumption in Jul
plotLoad5(na_seadec_nur2[na_seadec_nur2$month=="Jul" & na_seadec_nur2$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.03,0.005))
ggsave(here(plot_dir,"nur2_outdoorLoad_jul_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad5(na_seadec_nur2[na_seadec_nur2$month=="Jul" & na_seadec_nur2$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.02,0.005))
ggsave(here(plot_dir,"nur2_outdoorLoad_jul_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Aug
plotLoad5(na_seadec_nur2[na_seadec_nur2$month=="Aug" & na_seadec_nur2$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.03,0.005))
ggsave(here(plot_dir,"nur2_outdoorLoad_aug_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad5(na_seadec_nur2[na_seadec_nur2$month=="Aug" & na_seadec_nur2$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.02,0.005))
ggsave(here(plot_dir,"nur2_outdoorLoad_aug_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Sep
plotLoad5(na_seadec_nur2[na_seadec_nur2$month=="Sep" & na_seadec_nur2$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.03,0.005))
ggsave(here(plot_dir,"nur2_outdoorLoad_sep_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad5(na_seadec_nur2[na_seadec_nur2$month=="Sep" & na_seadec_nur2$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.02,0.005))
ggsave(here(plot_dir,"nur2_outdoorLoad_sep_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Oct
plotLoad5(na_seadec_nur2[na_seadec_nur2$month=="Oct" & na_seadec_nur2$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.03,0.005))
ggsave(here(plot_dir,"nur2_outdoorLoad_oct_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad5(na_seadec_nur2[na_seadec_nur2$month=="Oct" & na_seadec_nur2$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.02,0.005))
ggsave(here(plot_dir,"nur2_outdoorLoad_oct_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Nov
plotLoad5(na_seadec_nur2[na_seadec_nur2$month=="Nov" & na_seadec_nur2$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.03,0.005))
ggsave(here(plot_dir,"nur2_outdoorLoad_nov_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad5(na_seadec_nur2[na_seadec_nur2$month=="Nov" & na_seadec_nur2$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.02,0.005))
ggsave(here(plot_dir,"nur2_outdoorLoad_nov_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Dec
plotLoad5(na_seadec_nur2[na_seadec_nur2$month=="Dec" & na_seadec_nur2$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.03,0.003))
ggsave(here(plot_dir,"nur2_outdoorLoad_dec_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad5(na_seadec_nur2[na_seadec_nur2$month=="Dec" & na_seadec_nur2$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.02,0.003))
ggsave(here(plot_dir,"nur2_outdoorLoad_dec_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Jan
plotLoad5(na_seadec_nur2[na_seadec_nur2$month=="Jan" & na_seadec_nur2$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.03,0.003))
ggsave(here(plot_dir,"nur2_outdoorLoad_jan_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad5(na_seadec_nur2[na_seadec_nur2$month=="Jan" & na_seadec_nur2$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.02,0.003))
ggsave(here(plot_dir,"nur2_outdoorLoad_jan_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Feb
plotLoad5(na_seadec_nur2[na_seadec_nur2$month=="Feb" & na_seadec_nur2$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.03,0.003))
ggsave(here(plot_dir,"nur2_outdoorLoad_feb_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad5(na_seadec_nur2[na_seadec_nur2$month=="Feb" & na_seadec_nur2$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.03,0.003))
ggsave(here(plot_dir,"nur2_outdoorLoad_feb_wend_tod.pdf"), width = 8, height = 6, units = "cm")

# energy consumption in Mar
plotLoad5(na_seadec_nur2[na_seadec_nur2$month=="Mar" & na_seadec_nur2$weekday%in%c("Mon","Tue","Wed","Thu","Fri"),]) + 
  scale_y_continuous(breaks=seq(0,0.03,0.003))
ggsave(here(plot_dir,"nur2_outdoorLoad_mar_wday_tod.pdf"), width = 8, height = 6, units = "cm")
plotLoad5(na_seadec_nur2[na_seadec_nur2$month=="Mar" & na_seadec_nur2$weekday%in%c("Sat","Sun"),]) + 
  scale_y_continuous(breaks=seq(0,0.03,0.003))
ggsave(here(plot_dir,"nur2_outdoorLoad_mar_wend_tod.pdf"), width = 8, height = 6, units = "cm")
#******************************************************************************************#

#******************************************************************************************#
# 2.	Does energy availability through a PV system increase energy demand over time?
nur1_daily <- na_seadec_nur1 %>% group_by(month,date) %>% 
  summarise(Socket.load.W=sum(Socket.load.W),Indoor.light.load.W=sum(Indoor.light.load.W),
            Outdoor.light.load.W=sum(Outdoor.light.load.W))
nur1_daily <- nur1_daily %>% mutate(Light.load.W=Indoor.light.load.W+Outdoor.light.load.W)

nur2_daily <- na_seadec_nur2 %>% group_by(month, date) %>%
  summarise(Socket.load.W=sum(Socket.load.W), Indoor.light.load.W=sum(Indoor.light.load.W),
            Outdoor.light.load.W=sum(Outdoor.light.load.W))
nur2_daily <- nur2_daily %>% mutate(Light.load.W=Indoor.light.load.W+Outdoor.light.load.W)

pg_daily <- na_seadec_pg %>% group_by(month, date) %>%
  summarise(Socket.load.W=sum(Socket.load.W), Light.load.W=sum(Light.load.W))

sl_daily <- na_seadec_sl %>% group_by(month, date) %>% summarise(Light.load.W=sum(Light.load.W))

#2.a Daily socket consumption by the micro–grid users.
daily_socketLoad <- data.frame()
daily_socketLoad <- as.data.frame(nur1_daily[,c(1:3)])
daily_socketLoad <- cbind(daily_socketLoad, nur2_daily[,3], pg_daily[,3])
colnames(daily_socketLoad) <- c("month", "date", "Nur 1", "Nur 2", "Playground")
daily_socketLoad <- gather(daily_socketLoad, id, value, 3:5)

#daily socket load for nursery 1 and 2, and playground
dailySocket <- function(df) {
  ggplot(df, aes(date, value/1000, fill=id)) + geom_bar(stat="identity", position=position_dodge()) + THEME + 
    labs(x="Date", y="Socket consumption (kWh)", fill="") 
}
dailySocket(daily_socketLoad[daily_socketLoad$month=="Jul",]) + scale_y_continuous(breaks=seq(0,0.2,0.03)) 
ggsave(here(plot_dir,"daily_socketLoad_jul.pdf"), width = 8, height = 6, units = "cm")

dailySocket(daily_socketLoad[daily_socketLoad$month=="Aug",]) + scale_y_continuous(breaks=seq(0,0.4,0.05)) 
ggsave(here(plot_dir,"daily_socketLoad_aug.pdf"), width = 8, height = 6, units = "cm")

dailySocket(daily_socketLoad[daily_socketLoad$month=="Sep",]) + scale_y_continuous(breaks=seq(0,0.4,0.05)) 
ggsave(here(plot_dir,"daily_socketLoad_sep.pdf"), width = 8, height = 6, units = "cm")

dailySocket(daily_socketLoad[daily_socketLoad$month=="Oct",]) + scale_y_continuous(breaks=seq(0,0.4,0.05)) 
ggsave(here(plot_dir,"daily_socketLoad_oct.pdf"), width = 8, height = 6, units = "cm")

dailySocket(daily_socketLoad[daily_socketLoad$month=="Nov",]) + scale_y_continuous(breaks=seq(0,0.4,0.05)) 
ggsave(here(plot_dir,"daily_socketLoad_nov.pdf"), width = 8, height = 6, units = "cm")

dailySocket(daily_socketLoad[daily_socketLoad$month=="Dec",]) + scale_y_continuous(breaks=seq(0,0.4,0.05)) 
ggsave(here(plot_dir,"daily_socketLoad_dec.pdf"), width = 8, height = 6, units = "cm")

dailySocket(daily_socketLoad[daily_socketLoad$month=="Jan",]) + scale_y_continuous(breaks=seq(0,0.4,0.05)) 
ggsave(here(plot_dir,"daily_socketLoad_jan.pdf"), width = 8, height = 6, units = "cm")

dailySocket(daily_socketLoad[daily_socketLoad$month=="Feb",]) + scale_y_continuous(breaks=seq(0,1.0,0.25)) 
ggsave(here(plot_dir,"daily_socketLoad_feb.pdf"), width = 8, height = 6, units = "cm")

dailySocket(daily_socketLoad[daily_socketLoad$month=="Mar",]) + scale_y_continuous(breaks=seq(0,0.6,0.1)) 
ggsave(here(plot_dir,"daily_socketLoad_mar.pdf"), width = 8, height = 6, units = "cm")

#2.b Daily total socket consumption at the microgrid
daily_socketLoad <- spread(daily_socketLoad, id, value)
daily_socketLoad <- daily_socketLoad %>% mutate(Socket.load.W=`Nur 1` + `Nur 2` + Playground)
daily_socketLoad <- daily_socketLoad %>% mutate(days=as.numeric(date - as.Date("2019-07-01")))
ggplot(daily_socketLoad, aes(days, Socket.load.W/1000)) + geom_point(shape=1, color="coral1") + 
  labs(y="Socket load (kWh)", x = "Days since commissioning") + THEME + 
  scale_x_continuous(breaks = seq(1,260,28)) + scale_y_continuous(breaks=seq(0,2,0.3))
ggsave(here(plot_dir,"daily_socketLoad_jul19_mar20.pdf"), width = 8, height = 6, units = "cm")

#2.c Daily total consumption by the microgrid users
daily_users <- as.data.frame(nur1_daily[,c(1:3,6)])
daily_users <- cbind(daily_users, nur2_daily[,c(3,6)], pg_daily[,c(3:4)], sl_daily[,3])
colnames(daily_users) <- c("month","date","N1 sockets","N1 lights","N2 sockets", "N2 lights", "P sockets",
                           "P lights", "SL")
daily_users <- daily_users %>% mutate(AC.consumption=system_daily[,3])
daily_users <- daily_users %>% mutate(User.load=rowSums(daily_users[,c(3:9)]))
daily_users <- daily_users %>% mutate(Aux=AC.consumption-User.load)
daily_users <- daily_users[,-c(10,11)]
daily_users <- gather(daily_users, id, value, 3:10)

#daily load by microgrid users
dailyLoad <- function(df) {
  ggplot(df, aes(date, value/1000, fill=id)) + geom_bar(stat="identity", position=position_stack()) + THEME + 
    labs(x="Date", y="Energy consumption (kWh)", fill="") + 
    theme(axis.text = element_text(size=7), axis.title = element_text(size=9),
          legend.text=element_text(size=7), legend.key.size = unit(0.3, "cm")) 
}
theme(legend.position = "bottom", legend.text=element_text(size=10, family="Times New Roman"),
      legend.key.size = unit(0.5, "cm"),legend.margin = margin(t=0,r=0,b=0,l=0), 
      panel.grid.major.y = element_line(colour="grey"), 
      panel.grid.minor = element_blank(), panel.background = element_blank(), 
      axis.line = element_line(colour = "black"), 
      axis.text = element_text(size=9, family="Times New Roman"),
      axis.title = element_text(size=10, family="Times New Roman")) 
dailyLoad(daily_users[daily_users$month=="Jul",]) 
ggsave(here(plot_dir,"daily_userLoad_jul.pdf"), width = 8, height = 6, units = "cm")
dailyLoad(daily_users[daily_users$month=="Aug",]) 
ggsave(here(plot_dir,"daily_userLoad_aug.pdf"), width = 8, height = 6, units = "cm")
dailyLoad(daily_users[daily_users$month=="Sep",]) 
ggsave(here(plot_dir,"daily_userLoad_sep.pdf"), width = 8, height = 6, units = "cm")
dailyLoad(daily_users[daily_users$month=="Oct",]) 
ggsave(here(plot_dir,"daily_userLoad_oct.pdf"), width = 8, height = 6, units = "cm")
dailyLoad(daily_users[daily_users$month=="Nov",]) 
ggsave(here(plot_dir,"daily_userLoad_nov.pdf"), width = 8, height = 6, units = "cm")
dailyLoad(daily_users[daily_users$month=="Dec",]) 
ggsave(here(plot_dir,"daily_userLoad_dec.pdf"), width = 8, height = 6, units = "cm")
dailyLoad(daily_users[daily_users$month=="Jan",]) 
ggsave(here(plot_dir,"daily_userLoad_jan.pdf"), width = 8, height = 6, units = "cm")
dailyLoad(daily_users[daily_users$month=="Feb",]) + scale_y_continuous(breaks=seq(0,6,1))
ggsave(here(plot_dir,"daily_userLoad_feb.pdf"), width = 8, height = 6, units = "cm")
dailyLoad(daily_users[daily_users$month=="Mar",]) 
ggsave(here(plot_dir,"daily_userLoad_mar.pdf"), width = 8, height = 6, units = "cm")
#******************************************************************************************#
