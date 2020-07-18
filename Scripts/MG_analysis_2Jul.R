#******************************************************************************************#
# This is the script for analysing data for Microgrid                                      #
# Author: K Bhargava                                                                       #
# Last updated on: 6th July 2020                                                           #
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
imputed_data <- read.csv(here(filepath,"na_seadec_correctedData.csv"), 
                         header=TRUE, stringsAsFactors = FALSE)
imputed_data <- imputed_data %>% 
  mutate(date=as.Date(date),timestamp=as.POSIXct(timestamp,tz="GMT",origin="1970-01-01"),
         month = factor(month, levels = c("Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar"),
                        labels = c("Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar")))

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
# Get number of hours per day for full data from original data - all variables needed
system_sub_original <- imputed_data[,c(1:2,4,seq(7,175,by=2))]
system_sub_original <- system_sub_original[complete.cases(system_sub_original),]
onHours <- system_sub_original %>% group_by(month, date) %>% 
  summarise(hours=length(Battery.Monitor.Charged.Energy_original))
write.csv(onHours, file=here(filepath,"onHours_all_data_nepal.csv"), row.names=FALSE)

# Get number of hours per day for full data from original data - all variables except Nur1AS1
system_sub_original <- imputed_data[,c(1:2,4,seq(7,175,by=2))]
system_sub_original <- system_sub_original[,-c(13:14)]
system_sub_original <- system_sub_original[complete.cases(system_sub_original),]
onHours <- system_sub_original %>% group_by(month, date) %>% 
  summarise(hours=length(Battery.Monitor.Charged.Energy_original))
write.csv(onHours, file=here(filepath,"onHours_data_exceptNur1AS1_nepal.csv"), row.names=FALSE)
#******************************************************************************************#

#******************************************************************************************#
# Calculate daily data - imputed data except SoC
na_seadec_sub <- imputed_data[,c(1:2,4,3,seq(6,174,by=2))]
na_seadec_sub <- na_seadec_sub[,-7] # Remove SoC
# Calculate daily loads
na_seadec_sub <- gather(na_seadec_sub, id, value, c(4:88))
system_daily <- na_seadec_sub %>% group_by(id, month, date) %>% summarise(value=sum(value))
system_daily <- as.data.frame(system_daily)

# Calculate daily value of SoC -  take mean for the day
na_seadec_sub <- imputed_data[,c(1:2,4,10)]
# Calculate daily loads
na_seadec_sub <- gather(na_seadec_sub, id, value, c(4))
system_daily_soc <- na_seadec_sub %>% group_by(id, month, date) %>%summarise(value=mean(value))
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
system_daily <- system_daily %>% mutate(date=as.Date(date), 
                                        month=factor(month, levels = c("Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar"),
                                                     labels = c("Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar")))
system_daily <- gather(system_daily, id, value, 3:88)
system_monthly <- system_daily %>% group_by(id, month) %>% summarise(value=mean(value))
system_monthly <- as.data.frame(system_monthly)
# Converting power from W to kW
system_monthly <- system_monthly %>% 
  mutate(value=ifelse(id=="Battery.Monitor.State.of.charge.._ma", value, value/1000.0))
# Consider absolute values for all variables
system_monthly <- spread(system_monthly, id, value)
write.csv(system_monthly, file=here(filepath,"monthly_avg_correctedData.csv"), row.names=FALSE)
#******************************************************************************************#

#******************************************************************************************#
# Plots 7a-b - Typical user load and predicted load at the micro-grid’s nursery (a) 
# and playground (b) between 2nd July and 31st Mar
# Subset imputed data and calculate load for nurseries and playground
na_seadec_nur <- imputed_data[,c(1:2,4,seq(12,114,by=2))]
na_seadec_nur <- na_seadec_nur[,-c(10,12,20,28,36,38,46,54)] # Remove energy session values
na_seadec_nur <- na_seadec_nur %>% mutate(Actual.User.Load.W=rowSums(na_seadec_nur[,c(4:47)]),
                                Predicted.User.Load.W=rep(nur_predicted$User.Load.W,274))

na_seadec_pg <- imputed_data[,c(1:2,4,seq(116,152,by=2))]
na_seadec_pg <- na_seadec_pg[,-c(19,21)] # Remove energy session values
na_seadec_pg <- na_seadec_pg %>% mutate(Actual.User.Load.W=rowSums(na_seadec_pg[,c(4:20)]),
                                    Predicted.User.Load.W=rep(pg_predicted$User.Load.W,274))

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
    geom_line(aes(linetype=month)) + scale_x_continuous(breaks=seq(0,24,2)) +
    THEME + labs(x="Time of day", y="User load (kW)", color="Month", linetype="Month")
}
plotTypical(typical_load_nur) + 
  labs(title="Typical day load profile at the Nurseries between Jul'19 and Mar'20")
ggsave(here(plot_dir,"typical_load_nur_jul19_mar20.png"))
plotTypical(typical_load_pg) + 
  labs(title="Typical day load profile at the Playground between Jul'19 and Mar'20")
ggsave(here(plot_dir,"typical_load_pg_jul19_mar20.png"))
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

plotError_scatter <- function(df) {
  ggplot(df, aes(timeUse, Diff/1000.0, color=month, shape=month)) +
    geom_point() + scale_shape_manual(values=c(1,4,1,4,1,4,1,4,1)) + 
    labs(x="Time of day", y="Prediction error (kW)", color="Month", shape="Month") +
    scale_x_continuous(breaks=seq(0,24,2)) +  THEME +
    geom_hline(aes(yintercept=0), color="black", linetype="dashed")
}
plotError_scatter(na_seadec_nur) + 
  labs(title="Over and under predictions of user load at Nurseries between Jul'19 and Mar'20")
ggsave(here(plot_dir,"diffPred_nur_jul19_mar20.png"))
plotError_scatter(na_seadec_pg) + 
  labs(title="Over and under predictions of user load at Playground between Jul'19 and Mar'20")
ggsave(here(plot_dir,"diffPred_pg_jul19_mar20.png"))

plotError_tile <- function(df) {
  ggplot(df, aes(date, timeUse)) + geom_tile(aes(fill = Diff_range)) + 
    scale_y_continuous(breaks=seq(0,24,by=3)) + xlab("X axis") + ylab("Y axis") + 
    labs(y="Time of day", x = "DATE", fill="Error (kW)") + THEME + 
    guides(fill = guide_colorbar(barwidth = 20))
}
pal <- wes_palette("Zissou1", 10, type = "continuous")
plotError_tile(na_seadec_nur) + 
  scale_fill_gradientn(colours = pal, breaks=c(2,4,6,8,10), 
        labels = c(">-0.5 & <-0.4", ">-0.3 & <-0.2",">-0.1 & <0", ">0.1 & <0.2", ">0.3")) + 
  labs(title="Over and under predictions of user load at Nurseries between Jul'19 and Mar'20")
ggsave(here(plot_dir,"diffPred_tile__nur_jul19_mar20.png"))
plotError_tile(na_seadec_pg) + 
  scale_fill_gradientn(colours = pal, breaks=c(1,2,3,4,5), 
                       labels = c("<0", ">0 & <0.1",">0.1 & <0.2", ">0.2 & <0.3", ">0.3")) + 
  labs(title="Over and under predictions of user load at Playground between Jul'19 and Mar'20")
ggsave(here(plot_dir,"diffPred_tile_pg_jul19_mar20.png"))
#******************************************************************************************#

#******************************************************************************************#
# Plot 9-b - Daily AC consumption values since commissioning micro-grid (b)
system_daily <- spread(system_daily, id, value)
system_daily <- system_daily %>% mutate(days=as.numeric(date - as.Date("2019-07-01")))
ggplot(system_daily, aes(days, System.overview.AC.Consumption.L1.W_ma/1000.0, color="Actual")) +
  geom_point(shape=8) + theme(legend.position="none") + 
  labs(title="Daily AC consumption at the Microgrid between Jul'19 and Mar'20" , 
       y="AC consumption (kW)", x = "Days since commissioning", colour="") + THEME +
  scale_x_continuous(breaks = seq(1,274,28)) + scale_y_continuous(breaks=seq(0,6,1)) + theme(legend.position = "none")
ggsave(here(plot_dir,"daily_acLoad_jul19_mar20.png"))
#******************************************************************************************#

#******************************************************************************************#
# Plots 11a-d: Typical day showing key power generation and consumption parameters at the 
# micro-grid during (a) Jul-Sep, (b) Oct-Dec and (c?) Jan-Mar
na_seadec_sub <- imputed_data[,c(1:2,4,3,6,8,10,154,174)]
# Calculate capture loss: Potential PV - Actual PV; add phases for above months
na_seadec_sub <- na_seadec_sub %>% 
  mutate(Capture.loss.W=Potential.PV.power.W- Solar.Charger.PV.power_ma,
         phase=ifelse(month=="Jul" | month=="Aug" | month=="Sep", 1, 
                      ifelse(month=="Oct" | month=="Nov" | month=="Dec",2,3)))

na_seadec_sub <- gather(na_seadec_sub, id, value, 4:10)
typical_day <- na_seadec_sub %>% group_by(phase, timeUse, id) %>% summarise(value=mean(value))
typical_day <- as.data.frame(typical_day)
typical_day <- spread(typical_day, id, value)
colnames(typical_day) <- c("phase","timeUse","B_cp","B_dp","SoC","L_c","E_p","E_a","E_load")

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
    labs(y="Energy (kW)", x = "Time of day", colour="Parameter", linetype="Parameter") +
    scale_x_continuous(breaks=seq(0,24,by=2)) + THEME
}
plotTypical(typical_day[typical_day$phase==1,], 0, 2.00, 0.25, 50) + 
  labs(title="Actual typical day profile for the Microgrid between Jul'19 and Sep'19")
ggsave(here(plot_dir,"typical_day_jul19_sep19.png"))
plotTypical(typical_day[typical_day$phase==2,], -0.5, 1.75, 0.25, 55) + 
  labs(title="Actual typical day profile for the Microgrid between Oct'19 and Dec'19")
ggsave(here(plot_dir,"typical_day_oct19_dec19.png"))
plotTypical(typical_day[typical_day$phase==3,], 0, 1.75, 0.25, 55) + 
  labs(title="Actual typical day profile for the Microgrid between Jan'20 and Mar'20")
ggsave(here(plot_dir,"typical_day_jan20_mar20.png"))
#******************************************************************************************#

#******************************************************************************************#
# Plot 12b - Daily average yield, capture losses and system losses at micro-grid 
# Get daily data for PV power, Potential PV and AC load 
# Yield = AC consumption, System loss=PV-Yield, Capture loss=Pot. PV- PV
na_seadec_sub <- system_daily[,c(1,2,77,78,88)] 
colnames(na_seadec_sub) <- c("month", "date", "E_p", "E_a", "E_load")
na_seadec_sub <- na_seadec_sub %>% mutate(L_c=E_p-E_a, L_s=E_a-E_load)
na_seadec_sub <- na_seadec_sub[,-c(3,4)]
# Daily average per month
mg_perf <- gather(na_seadec_sub, id, value, c(3:5))
mg_perf <- mg_perf %>% group_by(month, id) %>% summarise(value=mean(value))
mg_perf <- as.data.frame(mg_perf)  
ggplot(mg_perf, aes(month, value/1000.0, fill=id)) + 
  geom_bar(stat="identity", width=.5, position = "stack") + 
  scale_y_continuous(breaks=seq(0,14,2)) +
  labs(title="Daily average electrical energy values at the Microgrid between Jul'19 and Mar'20" ,
       y="Consumed and potential electrical energy (kWh)", x = "Month", fill="Variable") +
  scale_fill_manual(labels = c("Yield", "Capture losses","System losses"), 
                    values = wes_palette("GrandBudapest1", n = 3)) + THEME
ggsave(here(plot_dir,"mg_perf_jul19_mar20.png"))
#******************************************************************************************#