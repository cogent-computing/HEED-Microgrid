#******************************************************************************************#
# This is the script for analysing data for Community Hall                                 #
# Author: K Bhargava                                                                       #
# Last updated on: 3rd July 2020                                                           #
#******************************************************************************************#

#******************************************************************************************#
# Importing libraries
library(tidyverse)
library(lubridate)
library(wesanderson)
library(here)
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

