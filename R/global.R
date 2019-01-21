
library(tidyverse)
library(data.table)
library(Cairo)   # For nicer ggplot2 output when deployed on Linux


source("R/select_data_map.R")
source("R/make_pdfs.R")

# Read data that might be needed ------
# First, individual GCMS:
# Make a list of GCMs for drop-down. 
gcms <- list.files("data/annual")

# Read interannual data for individual GCMs. 
gcm_files <- list.files("data/interannual_calcs", 
                        recursive = T, pattern = ".csv", full = T)
gcm_dat <- purrr::map(gcm_files, fread) # 2.6 sec locally.
names(gcm_dat) <- gcm_files # Name gcm data. 

# Model means: 
mean_dat <- purrr::map(list.files("data/model_means", pattern = ".csv", full = T),
                       fread)
names(mean_dat) <- list.files("data/model_means")

# Load a template .csv for annual data that's used to figure out which rows to read based on click. 
annual_template <- fread("data/annual/bcc-csm1-1-m/1970_bcc-csm1-1-m_swe.csv") %>% 
  mutate(lines = 1:nrow(.))

# Load monte carlo data --------
mc_files <- list.files("data/mc_stats", full = T)
mc_dat <- purrr::map(mc_files, fread)
mc_files <- basename(mc_files)
mc_files <- str_replace(mc_files, ".csv", "")
names(mc_dat) <- mc_files

# Get color limits data. 
if(file.exists("data/color_limits/color_limits.csv")){
  color_lims <- read_csv("data/color_limits/color_limits.csv")
}


