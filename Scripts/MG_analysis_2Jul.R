#******************************************************************************************#
# This is the script for analysing data for Microgrid                                      #
# Author: K Bhargava                                                                       #
# Last updated on: 30th Nov 2020                                                           #
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
plot_dir <- "Plots/Paper 5"
#******************************************************************************************#

#******************************************************************************************#
#Read in data files and plot data
imputed_data <- read.csv(here(filepath,"na_seadec_correctedData.csv"), header=TRUE, stringsAsFactors = FALSE)
imputed_data <- imputed_data %>% mutate(date=as.Date(date),
                                        timestamp=as.POSIXct(timestamp,tz="GMT",origin="1970-01-01"),
                                        month = factor(month, levels = MONTHS,labels = MONTHS))

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
# Calculate daily data - Remove SoC, voltage, system and solar battery power (+actual)
na_seadec_sub <- imputed_data[,-c(9:10,11:12,156:157,180:181,270,273)] 
# Calculate daily loads
na_seadec_sub <- gather(na_seadec_sub, id, value, c(5:267))
system_daily <- na_seadec_sub %>% group_by(id, month, date) %>% summarise(value=sum(value, na.rm=TRUE))
system_daily <- as.data.frame(system_daily)

# Calculate daily value of SoC -  take mean for the day
na_seadec_sub <- imputed_data[,c(1:4,9:10)]
# Calculate daily loads
na_seadec_sub <- gather(na_seadec_sub, id, value, c(5:6))
system_daily_soc <- na_seadec_sub %>% group_by(id, month, date) %>%summarise(value=mean(value, na.rm=TRUE))
system_daily_soc <- as.data.frame(system_daily_soc)

# Bind data sets
system_daily <- rbind(system_daily, system_daily_soc)

system_daily <- spread(system_daily, id, value)
write.csv(system_daily, file=here(filepath,"system_daily_correctedData.csv"), row.names=FALSE)
#******************************************************************************************#

#*****************************************************************************************#
# Monthly daily avg - 2nd July 2019 to 31st Mar 2020
system_daily <- read.csv(here(filepath,"system_daily_correctedData.csv"), header=TRUE, 
                         stringsAsFactors=FALSE)
system_daily <- system_daily %>% mutate(date=as.Date(date), month=factor(month, levels = MONTHS,labels = MONTHS))
system_daily <- gather(system_daily, id, value, 3:267)
system_monthly <- system_daily %>% group_by(id, month) %>% summarise(value=mean(value, na.rm=TRUE))
system_monthly <- as.data.frame(system_monthly)
# Converting power from W to kW
system_monthly <- system_monthly %>% mutate(value=ifelse(id=="Battery.Monitor.State.of.charge.._ma" | 
                                      id=="Battery.Monitor.State.of.charge.._original", value, value/1000.0))
# Consider absolute values for all variables
system_monthly <- spread(system_monthly, id, value)
write.csv(system_monthly, file=here(filepath,"monthly_avg_correctedData.csv"), row.names=FALSE)
#******************************************************************************************#

#******************************************************************************************#
# Plots for identifying categories of data - Nursery 2 sockets
socket_yield <- imputed_data[, c(1:3, which(grepl("vRELAY", colnames(imputed_data), fixed=TRUE)))]
socket_yield <- socket_yield[, c(1:3, which(grepl("Nur.2", colnames(socket_yield), fixed=TRUE)))]
socket_yield <- socket_yield[, c(1:3, which(grepl("original", colnames(socket_yield), fixed=TRUE)))]
colnames(socket_yield) <- c("date","timeUse","month","Nur 2A S1","Nur 2A S2",
                            "Nur 2B S1", "Nur 2C S1")
socket_yield <- gather(socket_yield, variable, value, 4:7)
socket_yield <- socket_yield %>% mutate(type=ifelse(is.na(value), "Communication failure", "Available"))

# Categorize the missing data - power outage, component failures and communication failures
# Power failure between 4-18 Sep
socket_yield <- socket_yield %>% mutate(type=ifelse(date>="2019-09-04" & date<="2019-09-18",
                                                    "Power outage", type))

# Component failures 12Feb-8Mar and 9Mar to 31Mar for Nur2AS2
socket_yield <- socket_yield %>% mutate(type=ifelse(date>="2020-02-12" & date<="2020-03-08",
                                                    "Component fault",type))
socket_yield <- socket_yield %>% mutate(type=ifelse(variable=="Nur.2A.S2.vRELAY1_LVL_original" &
                                                      date>="2020-03-09" & date<="2020-03-31" ,
                                                    "Component fault", type))

pal <- wes_palette("Zissou1", 100, type = "continuous")
plotYield <- function(df) {
  ggplot(df, aes(date, timeUse)) + geom_tile(aes(fill=type)) + 
    scale_y_continuous(breaks=seq(0,24,by=6)) + 
    labs(x = "Day of study", y="Time of day", fill="") + THEME +
          guides(fill=guide_legend(nrow=2,byrow=TRUE)) + 
    theme(legend.text=element_text(size=8, family="Times New Roman"),
          legend.key.size = unit(0.1, "cm"))
}
plotYield(socket_yield) + facet_wrap(~variable, nrow=2)
ggsave(here(plot_dir,"socket_categorical_jul19_mar20.pdf"), width = 8, height = 8, units = "cm")
#******************************************************************************************#

#******************************************************************************************#
# Plots 7a-b - Typical user load and predicted load at the micro-grid’s nursery (a) 
# and playground (b) between 2nd July and 31st Mar
# Subset imputed data and calculate load for nurseries and playground
na_seadec_sub <- imputed_data[,c(1:3, which(grepl("Actual", colnames(imputed_data), fixed=TRUE)))]
na_seadec_sub <- na_seadec_sub[, c(1:3, which(grepl("CPE", colnames(na_seadec_sub), fixed=TRUE) |
                                                grepl("vRELAY", colnames(na_seadec_sub), fixed=TRUE)))]
na_seadec_nur <- na_seadec_sub[,c(1:3,which(grepl("Nur", colnames(na_seadec_sub), fixed=TRUE)))]
na_seadec_nur <- na_seadec_nur %>% mutate(Actual.User.Load.W=rowSums(na_seadec_nur[,c(4:47)]),
                                Predicted.User.Load.W=rep(nur_predicted$User.Load.W,274))

na_seadec_pg <- na_seadec_sub[,c(1:3,which(grepl("Playground", colnames(na_seadec_sub), fixed=TRUE)))]
na_seadec_pg <- na_seadec_pg %>% mutate(Actual.User.Load.W=rowSums(na_seadec_pg[,c(4:20)]),
                                    Predicted.User.Load.W=rep(pg_predicted$User.Load.W,274))

na_seadec_sl <- na_seadec_sub[,c(1:3,which(grepl("Streetlight", colnames(na_seadec_sub), fixed=TRUE)))]
na_seadec_sl <- na_seadec_sl %>% mutate(Actual.User.Load.W=rowSums(na_seadec_sl[,c(4:12)]))

# Calculate typical load (actual and predicted) at Nurseries and playground
typical_load_nur <- na_seadec_nur %>% group_by(month, timeUse) %>% 
  summarise(User.Load.W = mean(Actual.User.Load.W))
typical_load_nur <- as.data.frame(typical_load_nur)
typical_load_nur <- rbind(typical_load_nur, nur_predicted)

typical_load_pg <- na_seadec_pg %>% group_by(month, timeUse) %>% 
  summarise(User.Load.W = mean(Actual.User.Load.W))
typical_load_pg <- as.data.frame(typical_load_pg)
typical_load_pg <- rbind(typical_load_pg, pg_predicted)

# Plot typical loads
plotTypical <- function(df) {
  ggplot(df, aes(timeUse, User.Load.W/1000.0, color=month)) + 
    geom_line(aes(linetype=month)) + scale_x_continuous(breaks=seq(0,24,2)) + THEME + 
    scale_y_continuous(breaks=seq(0,0.7,0.1)) + theme(legend.text=element_text(size=7, family="Times New Roman"))+
    labs(x="Time of day", y="User load (kW)",color="", linetype="")
}
# title="Typical day load profile at the Nurseries between Jul'19 and Mar'20"
plotTypical(typical_load_nur) 
ggsave(here(plot_dir,"typical_load_nur_jul19_mar20.pdf"), width = 8, height = 6, units = "cm")
# title="Typical day load profile at the Playground between Jul'19 and Mar'20"
plotTypical(typical_load_pg) 
ggsave(here(plot_dir,"typical_load_pg_jul19_mar20.pdf"), width = 8, height = 6, units = "cm")

# title="Typical day load profile at the Nurseries between Jul'19 and Mar'20"
plotTypical(typical_load_nur[typical_load_nur$month!="Predicted",]) + 
  scale_y_continuous(breaks=seq(0,0.1,0.01))
ggsave(here(plot_dir,"typical_load_nur_jul19_mar20.pdf"), width = 8, height = 6, units = "cm")
# title="Typical day load profile at the Playground between Jul'19 and Mar'20"
plotTypical(typical_load_pg[typical_load_pg$month!="Predicted",]) + 
  scale_y_continuous(breaks=seq(0,0.1,0.01))
ggsave(here(plot_dir,"typical_load_pg_jul19_mar20.pdf"), width = 8, height = 6, units = "cm")

#"Typical day load profile at the nurseries between Jul'19 and Mar'20"
overall_load_nur <- typical_load_nur %>% group_by(timeUse) %>% 
  summarise(User.Load.W=mean(User.Load.W))
overall_load_nur <- data.frame(month=rep("Actual",24), timeUse = c(0:23), 
                           User.Load.W=overall_load_nur$User.Load.W,
                           stringsAsFactors = FALSE)
overall_load_nur <- rbind(overall_load_nur, nur_predicted)
plotTypical(overall_load_nur) 
ggsave(here(plot_dir,"overall_load_nur_jul19_mar20.pdf"), width = 8, height = 6, units = "cm")

#"Typical day load profile at the playground between Jul'19 and Mar'20"
overall_load_pg <- typical_load_pg %>% group_by(timeUse) %>% 
  summarise(User.Load.W=mean(User.Load.W))
overall_load_pg <- data.frame(month=rep("Actual",24), timeUse = c(0:23), 
                               User.Load.W=overall_load_pg$User.Load.W,
                               stringsAsFactors = FALSE)
overall_load_pg <- rbind(overall_load_pg, pg_predicted)
plotTypical(overall_load_pg) 
ggsave(here(plot_dir,"overall_load_pg_jul19_mar20.pdf"), width = 8, height = 6, units = "cm")
#******************************************************************************************#

#******************************************************************************************#
# Plots 8b-c - User demand over and under predictions at the nurseries (b) and playground (c)
# Calculate over or under prediction for each instance
na_seadec_nur <- na_seadec_nur %>% 
  mutate(Diff=Predicted.User.Load.W-Actual.User.Load.W, 
         Diff_range=ifelse(Diff<(-500),1,ifelse(Diff>=(-500) & Diff<(-400), 2, 
         ifelse(Diff>=(-400) & Diff<(-300), 3, ifelse(Diff>=(-300) & Diff<(-200), 4,
         ifelse(Diff>=(-200) & Diff<(-100), 5, ifelse(Diff>=(-100) & Diff<0, 6, 
         ifelse(Diff>=0 & Diff<100, 7, ifelse(Diff>=100 & Diff<200, 8,
         ifelse(Diff>=200 & Diff<300, 9, 10))))))))))

na_seadec_pg <- na_seadec_pg %>% 
  mutate(Diff=Predicted.User.Load.W-Actual.User.Load.W, 
         Diff_range=ifelse(Diff<0,1,ifelse(Diff>=0 & Diff<100, 2, 
         ifelse(Diff>=100 & Diff<200, 3, ifelse(Diff>=200 & Diff<300, 4, 5)))))

# Prediction error - kWh/day - nurseries
na_seadec_nur_daily <- na_seadec_nur[,c(1:3, 48,50)]
na_seadec_nur_daily <- na_seadec_nur %>% group_by(month, date) %>% summarise(error=sum(Diff),
                                                                             load=sum(Actual.User.Load.W))

# Prediction error - kWh/day - playground
na_seadec_pg_daily <- na_seadec_pg[,c(1:3, 21, 23)]
na_seadec_pg_daily <- na_seadec_pg %>% group_by(month, date) %>% summarise(error=sum(Diff),
                                                                             load=sum(Actual.User.Load.W))

# Daily data - kWh/day - streetlight
na_seadec_sl_daily <- na_seadec_sl[,c(1:3, 13)]
na_seadec_sl_daily <- na_seadec_sl_daily %>% group_by(month, date) %>% summarise(load=sum(Actual.User.Load.W))

plotError_scatter <- function(df) {
  ggplot(df, aes(timeUse, Diff/1000.0, color=month, shape=month)) +
    geom_point() + scale_shape_manual(values=c(1,4,1,4,1,4,1,4,1)) + 
    labs(x="Time of day", y="Prediction error (kW)", color="", shape="") +
    scale_x_continuous(breaks=seq(0,24,2)) + scale_y_continuous(breaks=seq(-1,0.75,0.25)) + 
    THEME + geom_hline(aes(yintercept=0), color="black", linetype="dashed") +
    theme(legend.text=element_text(size=7, family="Times New Roman"))
}
# title="Over and under predictions of user load at Nurseries between Jul'19 and Mar'20"
plotError_scatter(na_seadec_nur) 
ggsave(here(plot_dir,"diffPred_nur_jul19_mar20.pdf"), width = 8, height = 6, units = "cm")
# title="Over and under predictions of user load at Playground between Jul'19 and Mar'20"
plotError_scatter(na_seadec_pg) + scale_y_continuous(breaks=seq(-1,0.40,0.10))
ggsave(here(plot_dir,"diffPred_pg_jul19_mar20.pdf"), width = 8, height = 6, units = "cm")

plotError_tile <- function(df) {
  ggplot(df, aes(timeUse, date)) + geom_tile(aes(fill = Diff_range/1000.0)) + 
    scale_x_continuous(breaks=seq(0,24,by=3)) + xlab("X axis") + ylab("Y axis") + 
    labs(x="Time of day", y = "DATE", fill="Error (kW)") + THEME + 
    guides(fill = guide_colorbar(barheight = 8, barwidth = 0.5)) + scale_y_date(date_breaks = "1 month") +
    theme(legend.text=element_text(size=7, family="Times New Roman"), legend.position = "right",
          legend.title = element_text(size=7, family="Times New Roman"))
}
pal <- wes_palette("Zissou1", 10, type = "continuous")
# title="Over and under predictions of user load at Nurseries between Jul'19 and Mar'20"
plotError_tile(na_seadec_nur) + scale_fill_gradientn(colours = pal) 
ggsave(here(plot_dir,"diffPred_tile__nur_jul19_mar20.pdf"), width = 8, height = 6, units = "cm")
# title="Over and under predictions of user load at Playground between Jul'19 and Mar'20"
plotError_tile(na_seadec_pg) + scale_fill_gradientn(colours = pal) 
ggsave(here(plot_dir,"diffPred_tile_pg_jul19_mar20.pdf"), width = 8, height = 6, units = "cm")

# prediction error at nursery as a function of time of day per reading
ggplot(na_seadec_nur, aes(x=as.factor(timeUse), y=Diff/1000)) + 
  geom_boxplot(lwd=0.2,outlier.shape=1) + THEME + 
  labs(y="Prediction error (kW)", x = "Time of day") + scale_y_continuous(breaks=seq(-1,0.8,0.2)) +
  scale_x_discrete(breaks=seq(0,24,by=2))
ggsave(here(plot_dir,"diffPred_nur_tod_jul19_mar20.pdf"), width = 8, height = 6, units = "cm")

# daily prediction error at nursery as a function of month per reading
ggplot(na_seadec_nur_daily, aes(x=as.factor(month), y=error/1000)) + geom_boxplot() + THEME + 
  labs(y="Prediction error (kWh/day)", x = "Month") + scale_y_continuous(breaks=seq(0,4,0.5), limits=c(0,4))
ggsave(here(plot_dir,"dailyError_nur_month_jul19_mar20.pdf"), width = 8, height = 6, units = "cm")

# prediction error at playground as a function of time of day per reading
ggplot(na_seadec_pg, aes(x=as.factor(timeUse), y=Diff/1000)) + geom_boxplot() + THEME + 
  labs(y="Prediction error (kW)", x = "Time of day") + scale_y_continuous(breaks=seq(-1,0.8,0.1)) +
  scale_x_discrete(breaks=seq(0,24,by=2))
ggsave(here(plot_dir,"diffPred_pg_tod_jul19_mar20.pdf"), width = 8, height = 6, units = "cm")

# daily prediction error at nursery as a function of month per reading
ggplot(na_seadec_pg_daily, aes(x=as.factor(month), y=error/1000)) + geom_boxplot() + THEME + 
  labs(y="Prediction error (kWh/day)", x = "Month") + scale_y_continuous(breaks=seq(0,2.5,0.5), limits=c(0,2.5))
ggsave(here(plot_dir,"dailyError_pg_month_jul19_mar20.pdf"), width = 8, height = 6, units = "cm")

# actual user load at nursery as a function of time of day per reading
ggplot(na_seadec_nur, aes(x=as.factor(timeUse), y=Actual.User.Load.W/1000)) + geom_boxplot() + THEME + 
  labs(y="Actual User Load (kW)", x = "Time of day") + scale_y_continuous(breaks=seq(0,1.2,0.1)) +
  scale_x_discrete(breaks=seq(0,24,by=2))
ggsave(here(plot_dir,"userLoad_nur_tod_jul19_mar20.pdf"), width = 8, height = 6, units = "cm")

# daily user load at nursery as a function of month per reading
ggplot(na_seadec_nur_daily, aes(x=as.factor(month), y=load/1000)) + geom_boxplot() + THEME + 
  labs(y="Actual User Load (kWh/day)", x = "Month") + scale_y_continuous(breaks=seq(0,2.0,0.4), limits=c(0,2.0))
ggsave(here(plot_dir,"dailyLoad_nur_month_jul19_mar20.pdf"), width = 8, height = 6, units = "cm")

# actual user load at playground as a function of time of day per reading
ggplot(na_seadec_pg, aes(x=as.factor(timeUse), y=Actual.User.Load.W/1000)) + geom_boxplot() + THEME + 
  labs(y="Actual User Load (kW)", x = "Time of day") + scale_y_continuous(breaks=seq(0,1.2,0.02)) +
  scale_x_discrete(breaks=seq(0,24,by=2))
ggsave(here(plot_dir,"userLoad_pg_tod_jul19_mar20.pdf"), width = 8, height = 6, units = "cm")

# daily user load at playground as a function of month per reading
ggplot(na_seadec_pg_daily, aes(x=as.factor(month), y=load/1000)) + geom_boxplot() + THEME + 
  labs(y="Actual User Load (kWh/day)", x = "Month") + scale_y_continuous(breaks=seq(0,1.0,0.2), limits=c(0,1.0))
ggsave(here(plot_dir,"dailyLoad_pg_month_jul19_mar20.pdf"), width = 8, height = 6, units = "cm")

# Plotting separately for sockets and CPEs
na_devs_nur <- na_seadec_nur
na_devs_nur <- na_devs_nur %>% mutate(Light.load.W =rowSums(na_devs_nur[,c(4:39)]),
                              Socket.load.W = rowSums(na_devs_nur[,c(40:47)]))
na_devs_pg <- na_seadec_pg
na_devs_pg <- na_devs_pg %>% mutate(Light.load.W =rowSums(na_devs_pg[,c(4:18)]),
                                      Socket.load.W = rowSums(na_devs_pg[,c(19:20)]))

# Calculating daily values
na_devs_nur_daily <- na_devs_nur[,c(1:3,52:53)]
na_devs_nur_daily <- na_devs_nur_daily %>% group_by(month, date) %>% summarise(Light.load.W=sum(Light.load.W),
                                                                       Socket.load.W=sum(Socket.load.W))
na_devs_pg_daily <- na_devs_pg[,c(1:3,25:26)]
na_devs_pg_daily <- na_devs_pg_daily %>% group_by(month, date) %>% summarise(Light.load.W=sum(Light.load.W),
                                                                               Socket.load.W=sum(Socket.load.W))

# light load at nursery as a function of time of day per reading
ggplot(na_devs_nur, aes(x=as.factor(timeUse), y=Light.load.W/1000)) + 
  geom_boxplot(lwd=0.2,outlier.size = 0, outlier.shape=1) + THEME + 
  labs(y="Light load (kW)", x = "Time of day") + scale_y_continuous(breaks=seq(0,1.0,0.01))  +
  scale_x_discrete(breaks=seq(0,24,by=2)) +
  stat_boxplot(geom='errorbar', linetype=1, width=0.5)
ggsave(here(plot_dir,"lightLoad_nur_tod_jul19_mar20.pdf"), width = 8, height = 6, units = "cm")

# daily light load at nursery as a function of month per reading
ggplot(na_devs_nur_daily, aes(x=as.factor(month), y=Light.load.W/1000)) + 
  geom_boxplot(lwd=0.2,outlier.size = 0, outlier.shape=1) + THEME + 
  labs(y="Light load (kWh/day)", x = "Month") + scale_y_continuous(breaks=seq(0,1.0,0.1)) +
  stat_boxplot(geom='errorbar', linetype=1, width=0.5)
ggsave(here(plot_dir,"dailyLight_nur_month_jul19_mar20.pdf"), width = 8, height = 6, units = "cm")

# socket load at nursery as a function of time of day per reading
ggplot(na_devs_nur, aes(x=as.factor(timeUse), y=Socket.load.W/1000)) + 
  geom_boxplot(lwd=0.2, outlier.size = 0, outlier.shape=1) + THEME + 
  labs(y="Socket load (kW)", x = "Time of day") + scale_y_continuous(breaks=seq(0,0.2,0.01),
                                                                     limits=c(0,0.04))  +
  scale_x_discrete(breaks=seq(0,24,by=2)) +
  stat_boxplot(geom='errorbar', linetype=1, width=0.5)
ggsave(here(plot_dir,"socketLoad_nur_tod_jul19_mar20.pdf"), width = 8, height = 6, units = "cm")

# daily socket load at nursery as a function of month per reading
ggplot(na_devs_nur_daily, aes(x=as.factor(month), y=Socket.load.W/1000)) + 
  geom_boxplot(lwd=0.2, outlier.size = 0, outlier.shape=1) + THEME + 
  labs(y="Socket load (kWh/day)", x = "Month") + scale_y_continuous(breaks=seq(0,0.4,0.1),
                                                                    limits=c(0,0.4)) +
  stat_boxplot(geom='errorbar', linetype=1, width=0.5)
ggsave(here(plot_dir,"dailySocket_nur_month_jul19_mar20.pdf"), width = 8, height = 6, units = "cm")

# light load at playground as a function of time of day per reading
ggplot(na_devs_pg, aes(x=as.factor(timeUse), y=Light.load.W/1000)) + geom_boxplot() + THEME + 
  labs(y="Light load (kW)", x = "Time of day") + scale_y_continuous(breaks=seq(0,1.0,0.01))  +
  scale_x_discrete(breaks=seq(0,24,by=2))
ggsave(here(plot_dir,"lightLoad_pg_tod_jul19_mar20.pdf"), width = 8, height = 6, units = "cm")

# daily light load at playground as a function of month per reading
ggplot(na_devs_pg_daily, aes(x=as.factor(month), y=Light.load.W/1000)) + geom_boxplot() + THEME + 
  labs(y="Light load (kWh/day)", x = "Month") + scale_y_continuous(breaks=seq(0,0.6,0.1))
ggsave(here(plot_dir,"dailyLight_pg_month_jul19_mar20.pdf"), width = 8, height = 6, units = "cm")

# socket load at playground as a function of time of day per reading
ggplot(na_devs_pg, aes(x=as.factor(timeUse), y=Socket.load.W/1000)) + 
  geom_boxplot(lwd=0.2, outlier.size = 0, outlier.shape=1) + THEME + 
  labs(y="Socket load (kW)", x = "Time of day") + scale_y_continuous(breaks=seq(0,0.03,0.01),
                                                                     limits=c(0,0.03))  +
  scale_x_discrete(breaks=seq(0,24,by=2)) + stat_boxplot(geom='errorbar', linetype=1, width=0.5)
ggsave(here(plot_dir,"socketLoad_pg_tod_jul19_mar20.pdf"), width = 8, height = 6, units = "cm")

# daily socket load at playground as a function of month per reading
ggplot(na_devs_pg_daily, aes(x=as.factor(month), y=Socket.load.W/1000)) + 
  geom_boxplot(lwd=0.2, outlier.size = 0, outlier.shape=1) + THEME + 
  labs(y="Socket load (kWh/day)", x = "Month") + scale_y_continuous(breaks=seq(0,0.5,0.1)) +
  stat_boxplot(geom='errorbar', linetype=1, width=0.5)
ggsave(here(plot_dir,"dailySocket_pg_month_jul19_mar20.pdf"), width = 8, height = 6, units = "cm")

# actual light load at streetlight as a function of time of day per reading
ggplot(na_seadec_sl, aes(x=as.factor(timeUse), y=Actual.User.Load.W/1000)) + geom_boxplot() + THEME + 
  labs(y="Light load (kW)", x = "Time of day") + scale_y_continuous(breaks=seq(0,0.05,0.01)) +
  scale_x_discrete(breaks=seq(0,24,by=2))
ggsave(here(plot_dir,"lightLoad_sl_tod_jul19_mar20.pdf"), width = 8, height = 6, units = "cm")

# daily light load at streetlight as a function of month per reading
ggplot(na_seadec_sl_daily, aes(x=as.factor(month), y=load/1000)) + geom_boxplot() + THEME + 
  labs(y="Light load (kWh/day)", x = "Month") + scale_y_continuous(breaks=seq(0,0.5,0.1))
ggsave(here(plot_dir,"lightLoad_sl_month_jul19_mar20.pdf"), width = 8, height = 6, units = "cm")

# Plotting behaviour of CPE and sockets for weekdays and weekends
na_devs_nur <- na_devs_nur %>% mutate(weekday = as.character(wday(date, label=TRUE, abbr=TRUE)))
na_devs_pg <- na_devs_pg %>% mutate(weekday = as.character(wday(date, label=TRUE, abbr=TRUE)))
na_devs_nur_daily <- na_devs_nur_daily %>% mutate(weekday = as.character(wday(date, label=TRUE, abbr=TRUE)))
na_devs_pg_daily <- na_devs_pg_daily %>% mutate(weekday = as.character(wday(date, label=TRUE, abbr=TRUE)))

# light load at nursery as a function of time of day per reading over weekdays and weekends
ggplot(na_devs_nur[na_devs_nur$weekday %in% c("Mon","Tue","Wed","Thu","Fri"),], 
       aes(x=as.factor(timeUse), y=Light.load.W/1000)) + geom_boxplot() + THEME + 
  labs(y="Light load (kW)", x = "Time of day") + scale_y_continuous(breaks=seq(0,1.0,0.01))  +
  scale_x_discrete(breaks=seq(0,24,by=2))
ggsave(here(plot_dir,"lightLoad_nur_wday_tod_jul19_mar20.pdf"), width = 8, height = 6, units = "cm")
ggplot(na_devs_nur[na_devs_nur$weekday %in% c("Sat","Sun"),], aes(x=as.factor(timeUse), y=Light.load.W/1000)) + 
  geom_boxplot() + THEME + labs(y="Light load (kW)", x = "Time of day")  +
  scale_x_discrete(breaks=seq(0,24,by=2)) + scale_y_continuous(breaks=seq(0,1.0,0.01))
ggsave(here(plot_dir,"lightLoad_nur_wend_tod_jul19_mar20.pdf"), width = 8, height = 6, units = "cm")

# light load at nursery as a function of month per reading over weekdays and weekends
ggplot(na_devs_nur[na_devs_nur$weekday %in% c("Mon","Tue","Wed","Thu","Fri"),], 
       aes(x=as.factor(month), y=Light.load.W/1000)) + geom_boxplot() + THEME + 
  labs(y="Light load (kW)", x = "Month") + scale_y_continuous(breaks=seq(0,1.0,0.01))
ggsave(here(plot_dir,"lightLoad_nur_wday_month_jul19_mar20.pdf"), width = 8, height = 6, units = "cm")
ggplot(na_devs_nur[na_devs_nur$weekday %in% c("Sat","Sun"),], aes(x=as.factor(month), y=Light.load.W/1000)) + 
  geom_boxplot() + THEME + labs(y="Light load (kW)", x = "Month") + scale_y_continuous(breaks=seq(0,1.0,0.01))
ggsave(here(plot_dir,"lightLoad_nur_wend_month_jul19_mar20.pdf"), width = 8, height = 6, units = "cm")

# daily light load at nursery as a function of month per reading over weekdays and weekends
ggplot(na_devs_nur_daily[na_devs_nur_daily$weekday %in% c("Mon","Tue","Wed","Thu","Fri"),], 
       aes(x=as.factor(month), y=Light.load.W/1000)) + geom_boxplot() + THEME + 
  labs(y="Light load (kWh/day)", x = "Month") + scale_y_continuous(breaks=seq(0,1.0,0.1))
ggsave(here(plot_dir,"dailyLight_nur_wday_month_jul19_mar20.pdf"), width = 8, height = 6, units = "cm")
ggplot(na_devs_nur_daily[na_devs_nur_daily$weekday %in% c("Sat","Sun"),], aes(x=as.factor(month), y=Light.load.W/1000)) + 
  geom_boxplot() + THEME + labs(y="Light load (kWh/day)", x = "Month") + scale_y_continuous(breaks=seq(0,1.0,0.1))
ggsave(here(plot_dir,"dailyLight_nur_wend_month_jul19_mar20.pdf"), width = 8, height = 6, units = "cm")

# socket load at nursery as a function of time of day per reading over weekdays and weekends
ggplot(na_devs_nur[na_devs_nur$weekday %in% c("Mon","Tue","Wed","Thu","Fri"),], 
       aes(x=as.factor(timeUse), y=Socket.load.W/1000)) + geom_boxplot() + THEME + 
  labs(y="Socket load (kW)", x = "Time of day") + scale_y_continuous(breaks=seq(0,1.2,0.1))  +
  scale_x_discrete(breaks=seq(0,24,by=2))
ggsave(here(plot_dir,"socketLoad_nur_wday_tod_jul19_mar20.pdf"), width = 8, height = 6, units = "cm")
ggplot(na_devs_nur[na_devs_nur$weekday %in% c("Sat","Sun"),], aes(x=as.factor(timeUse), y=Socket.load.W/1000)) + 
  geom_boxplot() + THEME + labs(y="Socket load (kW)", x = "Time of day") + 
  scale_y_continuous(breaks=seq(0,1.2,0.01))  + scale_x_discrete(breaks=seq(0,24,by=2))
ggsave(here(plot_dir,"socketLoad_nur_wend_tod_jul19_mar20.pdf"), width = 8, height = 6, units = "cm")

# socket load at nursery as a function of month per reading over weekdays and weekends
ggplot(na_devs_nur[na_devs_nur$weekday %in% c("Mon","Tue","Wed","Thu","Fri"),], 
       aes(x=as.factor(month), y=Socket.load.W/1000)) + geom_boxplot() + THEME + 
  labs(y="Socket load (kW)", x = "Month") + scale_y_continuous(breaks=seq(0,1.2,0.1))
ggsave(here(plot_dir,"socketLoad_nur_wday_month_jul19_mar20.pdf"), width = 8, height = 6, units = "cm")
ggplot(na_devs_nur[na_devs_nur$weekday %in% c("Sat","Sun"),], aes(x=as.factor(month), y=Socket.load.W/1000)) + 
  geom_boxplot() + THEME + labs(y="Socket load (kW)", x = "Month") + scale_y_continuous(breaks=seq(0,1.2,0.02))
ggsave(here(plot_dir,"socketLoad_nur_wend_month_jul19_mar20.pdf"), width = 8, height = 6, units = "cm")

# daily socket load at nursery as a function of month per reading over weekdays and weekends
ggplot(na_devs_nur_daily[na_devs_nur_daily$weekday %in% c("Mon","Tue","Wed","Thu","Fri"),], 
       aes(x=as.factor(month), y=Socket.load.W/1000)) + geom_boxplot() + THEME + 
  labs(y="Socket load (kWh/day)", x = "Month") + scale_y_continuous(breaks=seq(0,1.6,0.4))
ggsave(here(plot_dir,"dailySocket_nur_wday_month_jul19_mar20.pdf"), width = 8, height = 6, units = "cm")
ggplot(na_devs_nur_daily[na_devs_nur_daily$weekday %in% c("Sat","Sun"),], aes(x=as.factor(month), y=Socket.load.W/1000)) + 
  geom_boxplot() + THEME + labs(y="Socket load (kWh/day)", x = "Month") + scale_y_continuous(breaks=seq(0,0.5,0.1))
ggsave(here(plot_dir,"dailySocket_nur_wend_month_jul19_mar20.pdf"), width = 8, height = 6, units = "cm")

# light load at playground as a function of time of day per reading over weekdays and weekends
ggplot(na_devs_pg[na_devs_pg$weekday %in% c("Mon","Tue","Wed","Thu","Fri"),], 
       aes(x=as.factor(timeUse), y=Light.load.W/1000)) + geom_boxplot() + THEME + 
  labs(y="Light load (kW)", x = "Time of day") + scale_y_continuous(breaks=seq(0,1.0,0.01))  +
  scale_x_discrete(breaks=seq(0,24,by=2))
ggsave(here(plot_dir,"lightLoad_pg_wday_tod_jul19_mar20.pdf"), width = 8, height = 6, units = "cm")
ggplot(na_devs_pg[na_devs_pg$weekday %in% c("Sat","Sun"),], aes(x=as.factor(timeUse), y=Light.load.W/1000)) + 
  geom_boxplot() + THEME + labs(y="Light load (kW)", x = "Time of day")  +
  scale_x_discrete(breaks=seq(0,24,by=2)) + scale_y_continuous(breaks=seq(0,1.0,0.01))
ggsave(here(plot_dir,"lightLoad_pg_wend_tod_jul19_mar20.pdf"), width = 8, height = 6, units = "cm")

# light load at playground as a function of month per reading over weekdays and weekends
ggplot(na_devs_pg[na_devs_pg$weekday %in% c("Mon","Tue","Wed","Thu","Fri"),], 
       aes(x=as.factor(month), y=Light.load.W/1000)) + geom_boxplot() + THEME + 
  labs(y="Light load (kW)", x = "Month") + scale_y_continuous(breaks=seq(0,1.0,0.01))
ggsave(here(plot_dir,"lightLoad_pg_wday_month_jul19_mar20.pdf"), width = 8, height = 6, units = "cm")
ggplot(na_devs_pg[na_devs_pg$weekday %in% c("Sat","Sun"),], aes(x=as.factor(month), y=Light.load.W/1000)) + 
  geom_boxplot() + THEME + labs(y="Light load (kW)", x = "Month") + scale_y_continuous(breaks=seq(0,1.0,0.01))
ggsave(here(plot_dir,"lightLoad_pg_wend_month_jul19_mar20.pdf"), width = 8, height = 6, units = "cm")

# daily light load at playground as a function of month per reading over weekdays and weekends
ggplot(na_devs_pg_daily[na_devs_pg_daily$weekday %in% c("Mon","Tue","Wed","Thu","Fri"),], 
       aes(x=as.factor(month), y=Light.load.W/1000)) + geom_boxplot() + THEME + 
  labs(y="Light load (kWh/day)", x = "Month") + scale_y_continuous(breaks=seq(0,1.0,0.1))
ggsave(here(plot_dir,"dailyLight_pg_wday_month_jul19_mar20.pdf"), width = 8, height = 6, units = "cm")
ggplot(na_devs_pg_daily[na_devs_pg_daily$weekday %in% c("Sat","Sun"),], aes(x=as.factor(month), y=Light.load.W/1000)) + 
  geom_boxplot() + THEME + labs(y="Light load (kWh/day)", x = "Month") + scale_y_continuous(breaks=seq(0,1.0,0.1))
ggsave(here(plot_dir,"dailyLight_pg_wend_month_jul19_mar20.pdf"), width = 8, height = 6, units = "cm")

# socket load at playground as a function of time of day per reading over weekdays and weekends
ggplot(na_devs_pg[na_devs_pg$weekday %in% c("Mon","Tue","Wed","Thu","Fri"),], 
       aes(x=as.factor(timeUse), y=Socket.load.W/1000)) + geom_boxplot() + THEME + 
  labs(y="Socket load (kW)", x = "Time of day") + scale_y_continuous(breaks=seq(0,1.2,0.02))  +
  scale_x_discrete(breaks=seq(0,24,by=2))
ggsave(here(plot_dir,"socketLoad_pg_wday_tod_jul19_mar20.pdf"), width = 8, height = 6, units = "cm")
ggplot(na_devs_pg[na_devs_pg$weekday %in% c("Sat","Sun"),], aes(x=as.factor(timeUse), y=Socket.load.W/1000)) + 
  geom_boxplot() + THEME + labs(y="Socket load (kW)", x = "Time of day") + 
  scale_y_continuous(breaks=seq(0,1.2,0.01))  + scale_x_discrete(breaks=seq(0,24,by=2))
ggsave(here(plot_dir,"socketLoad_pg_wend_tod_jul19_mar20.pdf"), width = 8, height = 6, units = "cm")

# socket load at playground as a function of month per reading over weekdays and weekends
ggplot(na_devs_pg[na_devs_pg$weekday %in% c("Mon","Tue","Wed","Thu","Fri"),], 
       aes(x=as.factor(month), y=Socket.load.W/1000)) + geom_boxplot() + THEME + 
  labs(y="Socket load (kW)", x = "Month") + scale_y_continuous(breaks=seq(0,1.2,0.02))
ggsave(here(plot_dir,"socketLoad_pg_wday_month_jul19_mar20.pdf"), width = 8, height = 6, units = "cm")
ggplot(na_devs_pg[na_devs_pg$weekday %in% c("Sat","Sun"),], aes(x=as.factor(month), y=Socket.load.W/1000)) + 
  geom_boxplot() + THEME + labs(y="Socket load (kW)", x = "Month") + scale_y_continuous(breaks=seq(0,1.2,0.01))
ggsave(here(plot_dir,"socketLoad_pg_wend_month_jul19_mar20.pdf"), width = 8, height = 6, units = "cm")

# daily socket load at playground as a function of month per reading over weekdays and weekends
ggplot(na_devs_pg_daily[na_devs_pg_daily$weekday %in% c("Mon","Tue","Wed","Thu","Fri"),], 
       aes(x=as.factor(month), y=Socket.load.W/1000)) + geom_boxplot() + THEME + 
  labs(y="Socket load (kWh/day)", x = "Month") + scale_y_continuous(breaks=seq(0,1.0,0.1))
ggsave(here(plot_dir,"dailySocket_pg_wday_month_jul19_mar20.pdf"), width = 8, height = 6, units = "cm")
ggplot(na_devs_pg_daily[na_devs_pg_daily$weekday %in% c("Sat","Sun"),], aes(x=as.factor(month), y=Socket.load.W/1000)) + 
  geom_boxplot() + THEME + labs(y="Socket load (kWh/day)", x = "Month") + scale_y_continuous(breaks=seq(0,1.0,0.1))
ggsave(here(plot_dir,"dailySocket_pg_wend_month_jul19_mar20.pdf"), width = 8, height = 6, units = "cm")
#******************************************************************************************#

#******************************************************************************************#
# Plot 9-b - Daily AC consumption values since commissioning micro-grid (b)
system_daily <- spread(system_daily, id, value)
system_daily <- system_daily %>% mutate(days=as.numeric(date - as.Date("2019-07-01")))
system_daily <- system_daily %>% 
  mutate(Actual.User.Load.W = rowSums(system_daily[,c(4:9,11,13,14:19,21:27,29:35,37,39:45,47:53,55:70,72,74)]))
system_daily_sub <- system_daily[, c(268,269,3)]
colnames(system_daily_sub) <- c("days", "User load", "AC consumption")
system_daily_sub <- gather(system_daily_sub, id, value, 2:3)
# "Daily AC consumption at the Microgrid between Jul'19 and Mar'20"
ggplot(system_daily_sub, aes(days, value/1000, color=id, shape=id)) + geom_point() + 
  scale_shape_manual(values=c(1,4)) + labs(y="Energy consumption (kWh)", x = "Days since commissioning", 
                                           colour="", shape="") + THEME + 
  scale_x_continuous(breaks = seq(1,260,28)) + scale_y_continuous(breaks=seq(0,6,1))
ggsave(here(plot_dir,"daily_acLoad_jul19_mar20.pdf"), width = 8, height = 6, units = "cm")
#******************************************************************************************#

#******************************************************************************************#
# Plots 11a-d: Typical day showing key power generation and consumption parameters at the 
# micro-grid during (a) Jul-Sep, (b) Oct-Dec and (c?) Jan-Mar

# +ve/-ve actual battery power (B_cp, B_dp), +ve actual solar battery power (E_a);
# actual AC load (E_load); potential pv (E_p); SoC
na_seadec_sub <- imputed_data[,c(1:3,276:277,274,272,155,9)]
# Calculate capture loss: Potential PV - Actual PV; add phases for above months
na_seadec_sub <- na_seadec_sub %>% 
  mutate(Capture.loss.W=Potential.PV.power.W-Positive.Actual.Solar.Charger.Battery.Power_ma,
         phase=ifelse(month=="Jul" | month=="Aug" | month=="Sep", 1, 
                      ifelse(month=="Oct" | month=="Nov" | month=="Dec",2,3)))

na_seadec_sub <- gather(na_seadec_sub, id, value, 4:10)
typical_day <- na_seadec_sub %>% group_by(phase, timeUse, id) %>% summarise(value=mean(value))
typical_day <- as.data.frame(typical_day)
typical_day <- spread(typical_day, id, value)
colnames(typical_day) <- c("phase","timeUse","E_load","SoC","L_c","B_dp","E_a","B_cp","E_p")

# Plot typical values for each SL
plotTypical <- function(df, st, lim, br, p) {
  ggplot(df, aes(x=timeUse)) + geom_line(aes(y=B_cp/1000.0, color="B_cp", linetype="B_cp")) +
    geom_line(aes(y=abs(B_dp)/1000.0, color="B_dp",linetype="B_dp")) + 
    geom_line(aes(y=E_a/1000.0, color="E_a",linetype="E_a")) +
    geom_line(aes(y=E_load/1000.0, color="E_load",linetype="E_load")) + 
    geom_line(aes(y=E_p/1000.0, color="E_p",linetype="E_p")) +
    geom_line(aes(y=L_c/1000.0, color="L_c",linetype="L_c")) + 
    geom_line(aes(y = SoC/p, color = "SoC",linetype="SoC")) + 
    scale_y_continuous(breaks= seq(st,lim,br), sec.axis = sec_axis(~.*p, 
                                                                  name = "State of Charge (%)")) +
    labs(y="Energy (kW)", x = "Time of day", colour="", linetype="") +
    scale_x_continuous(breaks=seq(0,24,by=2)) + THEME
}
# "Actual typical day profile for the Microgrid between Jul'19 and Sep'19"
plotTypical(typical_day[typical_day$phase==1,], 0, 2.00, 0.25, 50) 
ggsave(here(plot_dir,"typical_day_jul19_sep19.pdf"), width = 8, height = 6, units = "cm")
"Actual typical day profile for the Microgrid between Oct'19 and Dec'19"
plotTypical(typical_day[typical_day$phase==2,], -0.5, 1.75, 0.25, 55) 
ggsave(here(plot_dir,"typical_day_oct19_dec19.pdf"), width = 8, height = 6, units = "cm")
# title="Actual typical day profile for the Microgrid between Jan'20 and Mar'20"
plotTypical(typical_day[typical_day$phase==3,], 0, 1.75, 0.25, 55) 
ggsave(here(plot_dir,"typical_day_jan20_mar20.pdf"), width = 8, height = 6, units = "cm")
#******************************************************************************************#

#******************************************************************************************#
# Plot 12b - Daily average yield, capture losses and system losses at micro-grid 
# Get daily data for PV power, Potential PV and AC load 
# actual AC load (Yield), Ea (System loss=Ea-Yield), potential PV (Capture loss=Pot. PV- Ea)
na_seadec_sub <- system_daily[,c(1,2,3,239,245)] 
colnames(na_seadec_sub) <- c("month", "date", "E_load", "E_a", "E_p")
na_seadec_sub <- na_seadec_sub %>% mutate(L_c=E_p-E_a, L_bos=E_a-E_load)
na_seadec_sub <- na_seadec_sub[,-c(4:5)]
# Daily average per month
mg_perf <- gather(na_seadec_sub, id, value, c(3:5))
mg_perf <- mg_perf %>% group_by(month, id) %>% summarise(value=mean(value))
mg_perf <- as.data.frame(mg_perf)  
# "Daily average electrical energy values at the Microgrid between Jul'19 and Mar'20"
ggplot(mg_perf, aes(month, value/1000.0, fill=id)) + geom_bar(stat="identity", width=.5, position = "stack") + 
  scale_y_continuous(breaks=seq(0,14,2)) + 
  labs(y="Consumed & potential electric energy(kWh)", x = "Month", fill="") +
  scale_fill_manual(values = wes_palette("GrandBudapest1", n = 3)) + THEME + 
  theme(axis.title = element_text(size=7), legend.text = element_text(size=7), axis.text = element_text(size=7))
ggsave(here(plot_dir,"mg_perf_jul19_mar20.pdf"), width = 8, height = 6, units = "cm")
#******************************************************************************************#