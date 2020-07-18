#**********************************************************************#
# This is the script to prepare data for upload on HEED data portal    #
# Authors: K Bhargava                                                  #
# Last updated on: 17th July, 2020                                     #
#**********************************************************************#

#**********************************************************************#
# Import libraries for manipulating time series and plotting
library(tidyverse)
library(lubridate)
library(here)
#**********************************************************************#

#**********************************************************************#
# Define MACROS
NUM_CPE = 20
NUM_SOCKETS = 10
#**********************************************************************#

#**********************************************************************#
# Set path for output files
output_directory <- "Data/Data portal"
#**********************************************************************#

#**********************************************************************#
# Read in all CPE files, crop data between July and March, cluster into separate locations and save
cpe <- data.frame()
for(k in 1:NUM_CPE) {
  if(k==1) {
    filepath <- "Data/Nur 1A CPE1"
  } else if(k==2) {
    filepath <- "Data/Nur 1A CPE2"
  } else if(k==3) {
    filepath <- "Data/Nur 1B CPE3"
  } else if(k==4) {
    filepath <- "Data/Nur 1B CPE4"
  } else if(k==5) {
    filepath <- "Data/Nur 1C CPE5"
  } else if(k==6) {
    filepath <- "Data/Nur 1C CPE6"
  } else if(k==7) {
    filepath <- "Data/Nur 2A CPE7"
  } else if(k==8) {
    filepath <- "Data/Nur 2A CPE8"
  } else if(k==9) {
    filepath <- "Data/Nur 2B CPE9"
  } else if(k==10) {
    filepath <- "Data/Nur 2B CPE10"
  } else if(k==11) {
    filepath <- "Data/Nur 2C CPE11"
  } else if(k==12) {
    filepath <- "Data/Nur 2C CPE12"
  } else if(k==13) {
    filepath <- "Data/Playground CPE1"
  } else if(k==14) {
    filepath <- "Data/Playground CPE2"
  } else if(k==15) {
    filepath <- "Data/Playground CPE3"
  } else if(k==16) {
    filepath <- "Data/Playground CPE4"
  } else if(k==17) {
    filepath <- "Data/Playground CPE5"
  } else if(k==18) {
    filepath <- "Data/Streetlight 1 CPE"
  } else if(k==19) {
    filepath <- "Data/Streetlight 2 CPE"
  } else if(k==20) {
    filepath <- "Data/Streetlight 3 CPE"
  }
  file_list <- list.files(here(filepath))
  file_list <- file_list[file.size(here(filepath,file_list))>0]
  
  # Read all files - add ID and bind all data
  df <- map_dfr(file_list, ~ read_csv(here(filepath,.x), col_names = FALSE) %>%
                  mutate(id = substr(filepath,6,nchar(filepath)), 
                         X1 = as.double(X1), X2=as.character(X2), X3=as.character(X3)))
  cpe <- rbind(cpe,df)
}
cpe <- as.data.frame(cpe)
cpe <- cpe[,1:4]
colnames(cpe) <- c("timestamp", "variable", "value","id")
# Convert timestamp to Africa/Kigali time zone (GMT+2)
cpe <- cpe %>% mutate(timestamp = as.POSIXct(timestamp, tz="GMT", origin="1970-01-01") %m+% hours(2), 
                      date = date(timestamp))
cpe <- cpe[cpe$date>="2019-07-02" & cpe$date<="2020-03-31",]
cpe <- cpe[,-5] #remove date
cpe <- distinct(cpe)
cpe <- cpe[order(cpe$id, cpe$timestamp),]
cpe <- cpe[!is.na(cpe$id),]
colnames(cpe) <- c("Timestamp (Africa/Kigali)", "Variable", "Value", "ID")
cpe <- cpe[,c(1,4,2,3)] #re-arrange columns

# Save CPE data - split for each nursery[1A,1B,1C,2A,2B,2C], playground, streetlight
cpe_sub <- cpe[grepl("Nur 1A", cpe$ID, fixed=TRUE), ]
write.csv(cpe_sub, file=here(output_directory,"Nursery1A_CPE_data.csv"), row.names=FALSE)

cpe_sub <- cpe[grepl("Nur 1B", cpe$ID, fixed=TRUE), ]
write.csv(cpe_sub, file=here(output_directory,"Nursery1B_CPE_data.csv"), row.names=FALSE)

cpe_sub <- cpe[grepl("Nur 1C", cpe$ID, fixed=TRUE), ]
write.csv(cpe_sub, file=here(output_directory,"Nursery1C_CPE_data.csv"), row.names=FALSE)

cpe_sub <- cpe[grepl("Nur 2A", cpe$ID, fixed=TRUE), ]
write.csv(cpe_sub, file=here(output_directory,"Nursery2A_CPE_data.csv"), row.names=FALSE)

cpe_sub <- cpe[grepl("Nur 2B", cpe$ID, fixed=TRUE), ]
write.csv(cpe_sub, file=here(output_directory,"Nursery2B_CPE_data.csv"), row.names=FALSE)

cpe_sub <- cpe[grepl("Nur 2C", cpe$ID, fixed=TRUE), ]
write.csv(cpe_sub, file=here(output_directory,"Nursery2C_CPE_data.csv"), row.names=FALSE)

cpe_sub <- cpe[grepl("Playground", cpe$ID, fixed=TRUE), ]
write.csv(cpe_sub, file=here(output_directory,"Playground_CPE_data.csv"), row.names=FALSE)

cpe_sub <- cpe[grepl("Streetlight", cpe$ID, fixed=TRUE), ]
write.csv(cpe_sub, file=here(output_directory,"Streetlight_CPE_data.csv"), row.names=FALSE)
#**********************************************************************#

#**********************************************************************#
# Read all socket files
sockets <- data.frame()
for(k in 1:NUM_SOCKETS) {
  if(k==1) {
    filepath <- "Data/Nur 1A S1"
  } else if(k==2) {
    filepath <- "Data/Nur 1A S2"
  } else if(k==3) {
    filepath <- "Data/Nur 1B S1"
  } else if(k==4) {
    filepath <- "Data/Nur 1C S1"
  } else if(k==5) {
    filepath <- "Data/Nur 2A S1"
  } else if(k==6) {
    filepath <- "Data/Nur 2A S2"
  } else if(k==7) {
    filepath <- "Data/Nur 2B S1"
  } else if(k==8) {
    filepath <- "Data/Nur 2C S1"
  } else if(k==9) {
    filepath <- "Data/Playground S1"
  } else if(k==10) {
    filepath <- "Data/Playground S2"
  } 
  file_list <- list.files(here(filepath))
  file_list <- file_list[file.size(here(filepath,file_list))>0]
  
  # Read all files - add ID and bind all data
  df <- map_dfr(file_list, ~ read_csv(here(filepath,.x), col_names = FALSE) %>%
                  mutate(id = substr(filepath,6,nchar(filepath)), 
                         X1 = as.double(X1), X2=as.character(X2), X3=as.character(X3)))
  sockets <- rbind(sockets,df)
}
sockets <- as.data.frame(sockets)
sockets <- sockets[,1:4]
colnames(sockets) <- c("timestamp", "variable", "value","id")
# Convert timestamp to Africa/Kigali time zone (GMT+2)
sockets <- sockets %>% mutate(timestamp = as.POSIXct(timestamp, tz="GMT", origin="1970-01-01") %m+% hours(2), 
                      date = date(timestamp))
sockets <- sockets[sockets$date>="2019-07-02" & sockets$date<="2020-03-31",]
sockets <- sockets[,-5] #remove date
sockets <- distinct(sockets)
sockets <- sockets[order(sockets$id, sockets$timestamp),]
sockets <- sockets[!is.na(sockets$id),]
colnames(sockets) <- c("Timestamp (Africa/Kigali)", "Variable", "Value", "ID")
sockets <- sockets[,c(1,4,2,3)] #re-arrange columns

# Save sockets data - split into different nurseries (1A, 1B, 1C; 2A, 2B, 2C) and PG
sockets_sub <- sockets[grepl("Nur 1A", sockets$ID, fixed=TRUE), ]
write.csv(sockets_sub, file=here(output_directory,"Nursery1A_sockets_data.csv"), row.names=FALSE)

sockets_sub <- sockets[grepl("Nur 1B", sockets$ID, fixed=TRUE), ]
write.csv(sockets_sub, file=here(output_directory,"Nursery1B_sockets_data.csv"), row.names=FALSE)

sockets_sub <- sockets[grepl("Nur 1C", sockets$ID, fixed=TRUE), ]
write.csv(sockets_sub, file=here(output_directory,"Nursery1C_sockets_data.csv"), row.names=FALSE)

sockets_sub <- sockets[grepl("Nur 2A", sockets$ID, fixed=TRUE), ]
write.csv(sockets_sub, file=here(output_directory,"Nursery2A_sockets_data.csv"), row.names=FALSE)

sockets_sub <- sockets[grepl("Nur 2B", sockets$ID, fixed=TRUE), ]
write.csv(sockets_sub, file=here(output_directory,"Nursery2B_sockets_data.csv"), row.names=FALSE)

sockets_sub <- sockets[grepl("Nur 2C", sockets$ID, fixed=TRUE), ]
write.csv(sockets_sub, file=here(output_directory,"Nursery2C_sockets_data.csv"), row.names=FALSE)

sockets_sub <- sockets[grepl("Playground", sockets$ID, fixed=TRUE), ]
write.csv(sockets_sub, file=here(output_directory,"Playground_sockets_data.csv"), row.names=FALSE)
#**********************************************************************#
#*
#*Data for system is uploaded from the repository Data/System Data/Jun19_to_Mar20