# Function to make maps. 

make_map <- function(dat, case, swe_variable, variability_metric, gcm, significance_values,
                     color_label = "value",
                     plot_label = ""){
  
  require(ggthemes)
  require(gridExtra)
  require(cowplot)
  require(pals)
  require(patchwork)
  
  # First, if showing values: 
  if(significance_values == "values"){
  
  # Get model means and/or individual GCMs. 
    # First, means: 
  if(gcm == "10-model mean"){
    dat <- mean_dat[str_detect(names(mean_dat), case)][[1]] # choose correct case (hist/future/delta) 
    names(dat)[names(dat) == "hist_means"] <- "value"
  } else{
    # If single GCM, choose that one. 
    dat <- gcm_dat[str_detect(names(gcm_dat), gcm)]
    dat <- dat[str_detect(names(mean_dat), case)][[1]] # Choose case. 
  } # end selecting based on GCM. 
  
  # Get data for variability metric and SWE metric.
  var <- paste0(swe_variable, "_", variability_metric)

  small_dat <- dat %>% filter(variable == var) 
  
  # Get color limits. 
  color_lims2 <- color_lims %>% filter(variable == var)
  if(case == "delta"){color_lims2 <- color_lims2 %>% 
    filter(color_case == "delta")
  } else {color_lims2 <- color_lims2 %>% 
    filter(color_case == "hist_future")}
  
  # Choose legend label based on variability metric and swe variable. 
  if(swe_variable == "swe"){color_label <- "mm"}
  if(swe_variable == "doms"){
    if(variability_metric %in% c("25", "75", "means") & case != "delta"){
      color_label <- "day\nof\nwater\nyear"} else {color_label <- "days"}
  }
  if(variability_metric == "cv"){color_label <- ""} # overrides previous setting. 

  # Make a plot. ----------
  p <- ggplot(small_dat, aes(x = x, y = y, fill = value)) +
      geom_raster() +
      theme_map() +
      labs(title = plot_label, fill = color_label) +# str_replace_all(color_label, "\n", "")) + #fill = color_label
     borders("state", size = 0.3) + 
     theme(text = element_text(size = 14),
            #title = element_text(size = 10, face = "plain"),
            axis.title = element_blank(),
            axis.ticks = element_blank(), 
            axis.text = element_blank(),
            axis.line = element_blank(),
            panel.background = element_blank(), 
            panel.grid = element_blank(),
            plot.background = element_blank(),
            legend.key.size = unit(0.4, 'in')) +
      coord_quickmap(ylim = c(32, 52), xlim = c(-124,-103.1562)) 

  if(case == "delta"){
    p <- p + 
      scale_fill_distiller(type = "div", palette = "RdBu",
                           limits = c(color_lims2$min_val[1], color_lims2$max_val[1])
                           # limits = c(-1, 1)*max(abs(small_dat$value))
                           )
  } else {
    p <- p + 
      scale_fill_distiller(type = "seq", palette = "YlGnBu", direction = 1,
                           limits = c(color_lims2$min_val[1], color_lims2$max_val[1]))
  }
  
  } # end if showing values. 
  
  # If showing significance: 
  if(significance_values == "significance"){
    if(gcm == "10-model mean"){
      dat <- mc_dat$model_means
      color_label <- "At least \n50% of \nGCMs \nagree"
    } else {
      dat <- mc_dat[names(mc_dat) == gcm][[1]] %>% 
        rename(summary = direction)
      color_label <- "Significant \nchange"}
    
    # Get correct SWE variable and variability metric. 
    dat <- dat %>% filter(variable == variability_metric,
                          swe_var == swe_variable)
    
    p <- ggplot(dat, aes(x = x, y = y, fill = summary)) + 
      geom_raster(alpha = 0.5) +
      scale_fill_manual(values = c("decrease" = "dodgerblue",
                                   "neither" = "grey75", 
                                   "increase" = "red"),
                        breaks = c("decrease", "neither", "increase")) + 
      theme_map() +
      labs(fill = color_label) +
      borders("state", size = 0.3) + 
      theme(text = element_text(size = 14),
            plot.title = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank(), 
            axis.text = element_blank(),
            axis.line = element_blank(),
            panel.background = element_blank(), 
            panel.grid = element_blank(),
            plot.background = element_blank(),
            legend.key.size = unit(0.4, 'in')) +
      coord_quickmap(ylim = c(32, 52), xlim = c(-124,-103.1562)) 
    
    
  } # end if showing significance. 

  return(p)
} # end function


  
