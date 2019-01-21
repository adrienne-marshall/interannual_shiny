# This function takes the information from a plot click event.
# It should load the data for that pixel and make PDFs of historic and future values of SWEmax and DOMS. 

make_pdfs <- function(click_info, 
                      annual_template = annual_template,
                      gcm){
  library(tidyverse)
  library(ggthemes)
  library(ggridges)
  library(patchwork)
  
  # Make a placeholder for when there's no data yet.
  if(is.null(click_info$x)){
    p <- ggplot() + 
      labs(title = "Please click in a location with data to see probability distributions.") + 
      theme(plot.title = element_text(face = "plain", hjust = 0))
  } # end no-data case.
  
  if(!is.null(click_info$x)){
    # Figure out row number in annual template.
    closest_x <- which.min(abs(annual_template$x - click_info$x))
    closest_y <- which.min(abs(annual_template$y - click_info$y))
    
    line <- annual_template %>%
      filter(x == annual_template$x[closest_x],
             y == annual_template$y[closest_y])
    
    # Handle error if there's no data here. 
    if(nrow(line) > 0){
      
    # Read in annual data.
    if(gcm != "10-model mean"){
      annual_files <- list.files(paste0("data/annual/", gcm), full = T)
      annual_dat <- purrr::map(annual_files, function(x){
        df <- fread(x, skip = line$lines[1]-1, nrows = 1)
        names(df) <- c("x", "y", "swe_max", "doms")
        
        year <- as.numeric(str_sub(basename(x), 1, 4))
        df <- df %>% mutate(year = year)
        
        return(df)
      }) %>% bind_rows() %>% 
        gather(variable, value, -year, -x, -y) %>% 
        mutate(gcm = gcm) %>% 
        mutate(case = ifelse(year < 2000, "Historic", "RCP 8.5")) 
    } # end check to make sure we have one GCM.
    
    # If we have 10-model mean, might want to read in data for all GCMs and plot as facets.
    if(gcm == "10-model mean"){
      annual_files <- list.files("data/annual", full = T, recursive = T)
      annual_dat <- purrr::map(annual_files, function(x){
        df <- fread(x, skip = line$lines[1]-1, nrows = 1)
        names(df) <- c("x", "y", "swe_max", "doms")
        
        year <- as.numeric(str_sub(basename(x), 1, 4))
        df <- df %>% mutate(year = year)
        
        gcm <- str_extract(x, "data/annual/.+/")
        gcm <- str_replace(gcm, "data/annual/", "")
        gcm <- str_sub(gcm, 1, -2)
        
        df <- df %>% mutate(gcm = gcm)
        return(df)
        
      }) %>% bind_rows() %>% 
        gather(variable, value, -year, -x, -y, -gcm) %>% 
        mutate(case = ifelse(year < 2000, "Historic", "RCP 8.5")) 
      
    } # end reading data for 10-model mean. 
    
    make_plot <- function(var1, label, guide1, show_gcm){
      # Get quantile values. 
      vals <- annual_dat %>% 
        filter(variable == var1) %>% 
        group_by(case) %>% 
        summarise(p25 = quantile(value, 0.25),
                  p75 = quantile(value, 0.75))
      
      # Make plot.
      ans <- ggplot(annual_dat %>% filter(variable == var1), 
                    aes(x = value, color = case, fill = case, 
                        y = fct_rev(as_factor(gcm)))) + 
        geom_density_ridges(alpha = 0.5, color = "white", scale = 900, 
                            quantile_lines = T, quantiles = c(0.25, 0.75)) + 
        scale_fill_manual(breaks = c("Historic", "RCP 8.5"),
                          values = c("dodgerblue", "red"), 
                          guide = guide1) +
        scale_x_continuous(expand = c(0, 0)) + 
        scale_y_discrete(expand = c(0, 0)) + 
        labs(x = label) + 
        theme_few() + 
        theme(legend.title = element_blank(),
              axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank()) 
      
      if(length(unique(annual_dat$gcm)) > 1){
        ans <- ggplot(annual_dat %>% filter(variable == var1), 
                      aes(x = value, color = case, fill = case, 
                          y = fct_rev(as_factor(gcm)))) + 
          geom_density_ridges(alpha = 0.5, color = "white", scale = 1, 
                              quantile_lines = T, quantiles = c(0.25, 0.75)) + 
          scale_fill_manual(breaks = c("Historic", "RCP 8.5"),
                            values = c("dodgerblue", "red"), 
                            guide = guide1) +
          scale_x_continuous(expand = c(0, 0)) + 
          labs(x = label) + 
          theme_few() + 
          theme(legend.title = element_blank(),
                axis.title.y = element_blank()) 
        }
      
      return(ans)
    }
    
    swe_plot <- make_plot(var1 = "swe_max", label = "SWEmax (mm)", guide1 = F)
    
    doms_plot <- make_plot(var1 = "doms", 
                           label = "DOMS (day of water year)", 
                           guide = "legend")
    
    p <- swe_plot + doms_plot
    } # end if statement to ensure click is in data area.. 
    
    if(nrow(line) == 0){
      p <- ggplot() + 
        labs(title = "Please click in a location with data to see probability distributions.") + 
        theme(plot.title = element_text(face = "plain", hjust = 0))
    }
  
  } # end check to see that we have click data. 
  
  return(p)
}