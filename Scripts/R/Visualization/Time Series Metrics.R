#' load packages:
#' -------------------------------------------------------------------------------------------

Packages <- c("readxl", "ggplot2", "ggpubr", "dplyr", "tidyr", "gridExtra", "egg")
lapply(Packages, library, character.only = TRUE)

region <- "Soebatsfontein"



#' load datasets:
#' -------------------------------------------------------------------------------------------


#' read in the sensor timeseries data:
sensor_timeseries_data <- read_xlsx(path = paste0("C:/Users/Rieser/OneDrive/EAGLE M.Sc/Term 5 (Winter 2021 - 2022)/Master Thesis/Results/", 
                                                  region, "/Time Series/Time_series_daily_statistics_", region, ".xlsx"),
                           sheet = 1, col_names = TRUE)

#' read in precipitation data:
precipitation_df <- read_xlsx(path = paste0("C:/GEODATA/Master Thesis/Meteorological data/Precipitation time series/", region, ".xlsx"),
                              sheet = 1, col_names = TRUE)


head(sensor_timeseries_data)



#' Data processing and plotting:
#' -------------------------------------------------------------------------------------------



plot_timeseries <- function(Index_name, Class_name, Date_min, Date_max, Axis_unit) {

  #' dataset processing:
  #' -------------------
  
  #' first the indices time series:

  #' the data recoding function, dataframe is the original table, recode_lookup contains the conversion matrix
  metrics_new <- c("VV+VH", "VH/VV", "VV/VH", "VH*VV", "VV-VH")                                  #' new metrics
  names(metrics_new) <- c("add_VVVH", "div_VHVV", "div_VVVH", "mult_VHVV", "sub_VVVH")           #' named with old Types

  sensor_timeseries_data_filter <- sensor_timeseries_data %>%
      mutate(Index = recode(Index, !!!metrics_new,),
             Date = as.Date(Date)) %>%
    filter(Index == Index_name, Class == Class_name) %>%
    as.data.frame()                                      

  #' the precipitation data:
  precipitation_filter <- precipitation_df %>%
    mutate(Date = as.Date(Date)) %>%
    filter(Date %in% c(as.Date(Date_min) : as.Date(Date_max))) 
  
  #' set a max of 20:
  precipitation_filter$Precipitation[precipitation_filter$Precipitation >= 20] <- 20 
  
  #' join the two datasets:
  timeseries_select <- full_join(precipitation_filter, sensor_timeseries_data_filter, by = "Date", keep = F)
  
  
  #' Plotting the data:
  #' ------------------
  
  plot <- ggplot(timeseries_select, aes(x = Date, y = Precipitation)) + 
    geom_bar(stat = "identity", fill="#215495", color = NA, width = 1, position = "dodge") + 
    geom_point(data = ~ transform(., median_transformed = scales::rescale(median, range(Precipitation, na.rm = T), range(median, na.rm = T))),
               aes(x = Date, y = median_transformed),
               shape = 16, size = 0.5, color = "#E39922") + 
    scale_x_date(expand = c(0, 0), 
                 date_breaks = "1 year",
                 date_minor_breaks = "1 month",
                 date_labels = "%Y") +
    scale_y_continuous(expand = c(0,0), 
                       name = "Precipitation (mm)",
                       sec.axis = sec_axis(~ scales::rescale(., range(timeseries_select$median, na.rm = T), range(timeseries_select$Precipitation, na.rm = T)),
                                           name = paste(Axis_unit))
    ) +
    #coord_cartesian(ylim=c(0, 20)) +
    ggtitle(paste0(Index_name)) +
    theme_bw() + 
    theme(axis.text.y = element_text(size=7, color = "black"), 
          axis.text.x = element_text(size=7, color = "black", hjust = -2), 
          axis.title =  element_text(size=9, color = "black"), 
          axis.title.x =  element_blank(),
          axis.title.y.right = element_text(size=9, color = "black", angle = 90), 
          panel.grid.major.x = element_line(colour="black", size = 0.2, linetype = "solid"),
          panel.grid.major.y = element_line(colour="lightgray", size = 0.1, linetype = "solid"),
          panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 10, colour = "black")
    )
  
  return (plot)
  
}



#' Plot the graphics:
#' -------------------------------------------------------------------------------------------


plot_add_VVVH <- plot_timeseries("VV+VH", "Biocrusts", "2017-01-01", "2021-12-31", "BI (dB)")
plot_div_VHVV <- plot_timeseries("VH/VV", "Biocrusts", "2017-01-01", "2021-12-31", "BI (dB)")
plot_div_VVVH <- plot_timeseries("VV/VH", "Biocrusts", "2017-01-01", "2021-12-31", "BI (dB)")
plot_mult_VHVV <- plot_timeseries("VH*VV", "Biocrusts", "2017-01-01", "2021-12-31", "BI (dB)")
plot_sub_VVVH <- plot_timeseries("VV-VH", "Biocrusts", "2017-01-01", "2021-12-31", "BI (dB)")
plot_VV <- plot_timeseries("VV", "Biocrusts", "2017-01-01", "2021-12-31", "BI (dB)")
plot_VH <- plot_timeseries("VH", "Biocrusts", "2017-01-01", "2021-12-31", "BI (dB)")
plot_NDVI <- plot_timeseries("NDVI", "Biocrusts", "2017-01-01", "2021-12-31", "")
plot_BSCI <- plot_timeseries("BSCI", "Biocrusts", "2017-01-01", "2021-12-31", "")
plot_CI <- plot_timeseries("CI", "Biocrusts", "2017-01-01", "2021-12-31", "")
plot_mCI <- plot_timeseries("mCI", "Biocrusts", "2017-01-01", "2021-12-31", "")

plot_grid_sar <- egg::ggarrange(plot_VV, plot_VH,
                              plot_div_VHVV, plot_div_VVVH,
                              plot_add_VVVH, plot_sub_VVVH, 
                              plot_mult_VHVV, ncol = 1)

plot_grid_optical <- egg::ggarrange(plot_NDVI, plot_BSCI, 
                              plot_CI, plot_mCI, nrow = 4)



#' Export the plots:
#' -------------------------------------------------------------------------------------------


#' export plots:
ggsave(filename = paste0("Time_series_SAR_", region, ".png"),
         plot = plot_grid_sar, device = "png",
         path = "C:/Users/Rieser/OneDrive/EAGLE M.Sc/Term 5 (Winter 2021 - 2022)/Master Thesis/Graphics/Time Series/",
         width = 160, height = 210, units = "mm", dpi = 300)

ggsave(filename = paste0("Time_series_optical_", region, ".png"),
         plot = plot_grid_optical, device = "png",
         path = "C:/Users/Rieser/OneDrive/EAGLE M.Sc/Term 5 (Winter 2021 - 2022)/Master Thesis/Graphics/Time Series/",
         width = 160, height = 120, units = "mm", dpi = 300)

