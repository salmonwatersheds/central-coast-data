#'******************************************************************************
#' The goal of the script is to pull relevant WVI data from the database
#' and update with new estimated spawner abundance data
#' 
#' Files produced: 
#' 
#' Resources/Notes:
#'
#'******************************************************************************
#'
require(tidyverse)
library(arsenal)

source("/Users/erichertz/Salmon Watersheds Dropbox/Eric Hertz/X Drive/1_PROJECTS/1_Active/Population Methods and Analysis/population-indicators/code/functions_general.R")

#source("../../population-indicators/code/functions_general.R")

######

esc_data <- retrieve_data_from_PSF_databse_fun(name_dataset = "appdata.vwdl_dataset1cu_output")

esc_data_cc <- filter(esc_data, region =='Central Coast')

setwd('/Users/erichertz/Salmon Watersheds Dropbox/Eric Hertz/X Drive/1_PROJECTS/1_Active/Population Methods and Analysis/population-data/central-coast-data')
write.csv(esc_data_cc, "output/dataset1_cc_20250604.csv", row.names=FALSE)

### rs data

rs_data <- retrieve_data_from_PSF_databse_fun(name_dataset = "appdata.vwdl_dataset5_output")

rs_data_cc <- filter(rs_data, region =='Central Coast')%>%
              filter(., species_name !="Steelhead")

setwd('/Users/erichertz/Salmon Watersheds Dropbox/Eric Hertz/X Drive/1_PROJECTS/1_Active/Population Methods and Analysis/population-data/central-coast-data')
write.csv(rs_data_cc, "output/dataset5_cc_2025-06-11.csv", row.names=FALSE)


