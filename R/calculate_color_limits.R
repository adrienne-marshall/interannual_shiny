# Make limits info for GCM dat - so all color scales are the same. ---------
# Need to separate by historic + future vs delta, swe variable, variability metric. 

source("R/global.R")

library(tidyverse)

# Historic/future data: 
hist_fut <- gcm_dat[!str_detect(gcm_files, "delta")] %>% 
  bind_rows() %>% 
  rename("hist_means" = "value") %>% 
  bind_rows(mean_dat[!str_detect(names(mean_dat), "delta")]) %>% 
  group_by(variable) %>% 
  summarise(max_val = max(abs(hist_means)),
            min_val = min(abs(hist_means))) %>% 
  mutate(color_case = "hist_future")

delta <- gcm_dat[str_detect(gcm_files, "delta")] %>% 
  bind_rows() %>% 
  rename("hist_means" = "value") %>% 
  bind_rows(mean_dat[str_detect(names(mean_dat), "delta")]) %>% 
  group_by(variable) %>% 
  summarise(max_val = max(abs(hist_means)),
            min_val = -1*max(abs(hist_means))) %>% 
  mutate(color_case = "delta")

out_dat <- bind_rows(hist_fut, delta)

out_dat$min_val[out_dat$color_case == "delta" & out_dat$variable == "doms_iq_range"] <- -100
out_dat$max_val[out_dat$color_case == "delta" & out_dat$variable == "doms_iq_range"] <- 100

write_csv(out_dat, "data/color_limits/color_limits.csv")


