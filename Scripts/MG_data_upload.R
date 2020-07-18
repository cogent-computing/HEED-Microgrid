#******************************************************************************************#
# This is the script to prepare data for upload on Zenodo                                  #
# Author: K Bhargava                                                                       #
# Last updated on: 17th July 2020                                                          #
#******************************************************************************************#

#******************************************************************************************#
# Importing libraries
library(tidyverse)
library(lubridate)
library(here)
#******************************************************************************************#

#******************************************************************************************#
# Set working directory 
filepath <- "Data/Zenodo"
#******************************************************************************************#

#******************************************************************************************#
# Read all raw data and add month, date and time 
cpe <- read_csv(here(filepath,"CPE_all_jun19_to_mar20.csv"), col_names = TRUE)
cpe <- as.data.frame(cpe[,1:4])
cpe <- distinct(cpe)
cpe <- spread(cpe, variable, value)
cpe <- cpe %>% mutate(date = date(timestamp)) 
cpe <- cpe[order(cpe$id, cpe$timestamp),]
cpe <- cpe[cpe$date>="2019-07-02" & cpe$date<="2020-03-31",]
cpe <- cpe[,-6] #remove date
colnames(cpe) <- c("Timestamp (Africa/Kigali)", "ID", "LED1_P (W)", "LED2_P (W)", "LED3_P (W)")

sockets <- read_csv(here(filepath,"sockets_all_jun19_to_mar20.csv"), col_names = TRUE) 
sockets <- as.data.frame(sockets[,1:4])
sockets <- distinct(sockets)
sockets <- spread(sockets, variable, value)
sockets <- sockets %>% mutate(date = date(timestamp)) 
sockets <- sockets[order(sockets$id, sockets$timestamp),]
sockets <- sockets[sockets$date>="2019-07-02" & sockets$date<="2020-03-31",]
sockets <- sockets[,-c(3,5)] #Remove AC day energy session values and date
colnames(sockets) <- c("Timestamp (Africa/Kigali)", "ID", "vRELAY1_LVL (W)")

system <- read_csv(here(filepath,"systemData_jun19_to_mar20.csv"), col_names = TRUE) 
system <- as.data.frame(system[,1:9])
system <- distinct(system)
system <- system %>% mutate(date = date(timestamp)) 
system <- system[order(system$timestamp),]
system <- system[system$date>="2019-07-02" & system$date<="2020-03-31",]
system <- system[,-c(2,10)] #Remove date
colnames(system) <- c("Timestamp (Africa/Kigali)", "Battery Monitor State of Charge (%)",
                      "Battery Monitor Discharged Energy (kWh)", "Battery Monitor Charged Energy (kWh)",
                      "Solar Charger Battery Power (W)", "Solar Charger PV Power (W)", 
                      "System Overview AC Consumption (W)", "System Overview Battery Power (W)")
#******************************************************************************************#

#******************************************************************************************#
# Save data for Microgrid
filepath <- "Data/Zenodo/Microgrid"

# Save system data
write.csv(system, file=here(filepath,"System_data.csv"), row.names=FALSE)

# Save CPE data - split into different nurseries (1A, 1B, 1C; 2A, 2B, 2C), PG and SL
cpe_sub <- cpe[grepl("Nur 1A", cpe$ID, fixed=TRUE), ]
write.csv(cpe_sub, file=here(filepath,"Nursery1A_CPE_data.csv"), row.names=FALSE)

cpe_sub <- cpe[grepl("Nur 1B", cpe$ID, fixed=TRUE), ]
write.csv(cpe_sub, file=here(filepath,"Nursery1B_CPE_data.csv"), row.names=FALSE)

cpe_sub <- cpe[grepl("Nur 1C", cpe$ID, fixed=TRUE), ]
write.csv(cpe_sub, file=here(filepath,"Nursery1C_CPE_data.csv"), row.names=FALSE)

cpe_sub <- cpe[grepl("Nur 2A", cpe$ID, fixed=TRUE), ]
write.csv(cpe_sub, file=here(filepath,"Nursery2A_CPE_data.csv"), row.names=FALSE)

cpe_sub <- cpe[grepl("Nur 2B", cpe$ID, fixed=TRUE), ]
write.csv(cpe_sub, file=here(filepath,"Nursery2B_CPE_data.csv"), row.names=FALSE)

cpe_sub <- cpe[grepl("Nur 2C", cpe$ID, fixed=TRUE), ]
write.csv(cpe_sub, file=here(filepath,"Nursery2C_CPE_data.csv"), row.names=FALSE)

cpe_sub <- cpe[grepl("Playground", cpe$ID, fixed=TRUE), ]
write.csv(cpe_sub, file=here(filepath,"Playground_CPE_data.csv"), row.names=FALSE)

cpe_sub <- cpe[grepl("Streetlight", cpe$ID, fixed=TRUE), ]
write.csv(cpe_sub, file=here(filepath,"Streetlight_CPE_data.csv"), row.names=FALSE)

# Save sockets data - split into different nurseries (1A, 1B, 1C; 2A, 2B, 2C) and PG
sockets_sub <- sockets[grepl("Nur 1A", sockets$ID, fixed=TRUE), ]
write.csv(sockets_sub, file=here(filepath,"Nursery1A_sockets_data.csv"), row.names=FALSE)

sockets_sub <- sockets[grepl("Nur 1B", sockets$ID, fixed=TRUE), ]
write.csv(sockets_sub, file=here(filepath,"Nursery1B_sockets_data.csv"), row.names=FALSE)

sockets_sub <- sockets[grepl("Nur 1C", sockets$ID, fixed=TRUE), ]
write.csv(sockets_sub, file=here(filepath,"Nursery1C_sockets_data.csv"), row.names=FALSE)

sockets_sub <- sockets[grepl("Nur 2A", sockets$ID, fixed=TRUE), ]
write.csv(sockets_sub, file=here(filepath,"Nursery2A_sockets_data.csv"), row.names=FALSE)

sockets_sub <- sockets[grepl("Nur 2B", sockets$ID, fixed=TRUE), ]
write.csv(sockets_sub, file=here(filepath,"Nursery2B_sockets_data.csv"), row.names=FALSE)

sockets_sub <- sockets[grepl("Nur 2C", sockets$ID, fixed=TRUE), ]
write.csv(sockets_sub, file=here(filepath,"Nursery2C_sockets_data.csv"), row.names=FALSE)

sockets_sub <- sockets[grepl("Playground", sockets$ID, fixed=TRUE), ]
write.csv(sockets_sub, file=here(filepath,"Playground_sockets_data.csv"), row.names=FALSE)
#******************************************************************************************#