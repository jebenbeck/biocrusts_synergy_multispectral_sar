#' load packages:
#' -------------------------------------------------------------------------------------------

Packages <- c("readxl", "ggplot2", "dplyr", "tidyr", "gridExtra", "lubridate", "ggsignif")
lapply(Packages, library, character.only = TRUE)



#### Data preprocessing: ####
#' -------------------------------------------------------------------------------------------


data_preprocessing <- function(region) {
  
  #' load datasets:
  #' --------------
  
  
  #' read in the sensor timeseries data:
  sensor_timeseries_data <- read_xlsx(path = paste0("C:/Users/Rieser/OneDrive/EAGLE M.Sc/Term 5 (Winter 2021 - 2022)/Master Thesis/Results/", 
                                                    region, "/Time Series/Time_series_daily_statistics_", region, ".xlsx"),
                                      sheet = 1, col_names = TRUE)
  
  #' read in precipitation data:
  precipitation_df <- read_xlsx(path = paste0("C:/Users/Rieser/OneDrive/EAGLE M.Sc/Term 5 (Winter 2021 - 2022)/Master Thesis/Data_2/Precipitation data/", region, ".xlsx"),
                                sheet = 1, col_names = TRUE) 
  
  
  #' Preprocess the metrics time series data:
  #' -----------------------------
  
  #' the data recoding function, dataframe is the original table, recode_lookup contains the conversion matrix
  metrics_new <- c("VV+VH", "VH/VV", "VV/VH", "VH*VV", "VV-VH")                                  #' new metrics
  names(metrics_new) <- c("add_VVVH", "div_VHVV", "div_VVVH", "mult_VHVV", "sub_VVVH")           #' named with old Types
  
  sensor_timeseries_data_rec <- sensor_timeseries_data %>%
    mutate(Index = recode(Index, !!!metrics_new,),
           Date = as.Date(Date)) %>%
    as.data.frame()
  
  
  #' Preprocess the precipitation data:
  #' -----------------------------
  
  #' convert and sort by date:
  precipitation_df_rec <- precipitation_df %>% 
    mutate(Date = as.Date(Date)) %>%
    arrange(Date) 
  
  #' create an index of precipitation events: 
  precipitation_df_rec$event <- 0
  precipitation_df_rec$event[precipitation_df_rec$Precipitation >= 0.1] <- 1 
  
  last_event_index <- cumsum(precipitation_df_rec$event) + 1
  
  # get the dates of the events and index the vector with the last_event_index, 
  # added an NA as the first date because there was no event
  last_event_date <- c(as.Date(NA), precipitation_df_rec[which(precipitation_df_rec$event==1), "Date"])[last_event_index]
  
  # substract the event's date with the date of the last event and add this information to the dataframe:
  precipitation_df_rec$dae <- as.numeric(precipitation_df_rec$Date - last_event_date)
  precipitation_df_rec$dae[precipitation_df_rec$dae >= 5] <- "5+" 
  
  #' filter to the selected timeframe
  precipitation_filter <- precipitation_df_rec %>%
    filter(Date %in% c(as.Date("2017-01-01") : as.Date("2021-12-31"))) 

  
  #' Join both datasets:
  #' -------------------
  
  timeseries_select <- full_join(precipitation_filter, sensor_timeseries_data_rec, by = "Date", keep = F) %>%
    #' reformat the date to get info on DoY, Year and Season:
    mutate(Year = format.Date(Date, format = "%Y"),
           DoY = yday(Date),   
           Season = cut(DoY, breaks = c(-Inf, 60, 152, 244, 335, Inf),
                        labels = c("Winter", "Spring", "Summer", "Autumn", "Winter"))) %>%
    as.data.frame()
  
  
  return(timeseries_select)
  
}

timeseries_Lie <- data_preprocessing("Lieberose")
timeseries_Amo <- data_preprocessing("Amoladeras")
timeseries_Cau <- data_preprocessing("Cautivo")
timeseries_Neg <- data_preprocessing("Negev")
timeseries_Soe <- data_preprocessing("Soebatsfontein")



#### Calculate ANOVAS ####
#' -------------------------------------------------------------------------------------------


calculate_ANOVAs <- function(dataframe, region) {
  
  if(region == "Soebatsfontein") {
    date_remove <- c(c(as.Date("2018-10-11"): as.Date("2018-11-28")), 
                     c(as.Date("2020-05-20"): as.Date("2020-11-13")),
                     c(as.Date("2020-12-06"): as.Date("2021-03-24")))
    
    dataframe <- dataframe %>%
      filter(!Date %in% date_remove)
  }
  
  #' arrange timeseries:
  timeseries_data_by_index <- dataframe %>%
    #na.omit() %>%
    filter(Class == "Biocrusts") %>%
    arrange(Index) %>%
    group_by(Index) %>%
    group_split(.keep = T) 
  
  #' calculate ANOVA for each metric:
  p <- list()
  
  for (i in 1:length(timeseries_data_by_index)){
    df <- timeseries_data_by_index[[i]]
    Index_name <- df$Index[1]
    ANOVA <- summary(aov(df$median ~ df$dae))
    ANOVA_df <- data.frame(ANOVA[[1]])
    p_value <- round(ANOVA_df$Pr..F.[1], 5)
    Df_value <- paste0(ANOVA_df$Df[1], ", ", ANOVA_df$Df[2])
    F_value <- round(ANOVA_df$F.value[1], 2)
    p[[i]] <- data.frame(cbind(Df_value, F_value, p_value, Index_name, region))
  }
  
  df_p <- do.call(rbind, p)
  
  return(df_p)

}

ANOVA_df_Indices_Lie <- calculate_ANOVAs(timeseries_Lie, "Lieberose")
ANOVA_df_Indices_Amo <- calculate_ANOVAs(timeseries_Amo, "Amoladeras")
ANOVA_df_Indices_Cau <- calculate_ANOVAs(timeseries_Cau, "Cautivo")
ANOVA_df_Indices_Neg <- calculate_ANOVAs(timeseries_Neg, "Negev")
ANOVA_df_Indices_Soe <- calculate_ANOVAs(timeseries_Soe, "Soebatsfontein")

#' Combine all ANOVA values:
ANOVA_df_full <- rbind(ANOVA_df_Indices_Lie, ANOVA_df_Indices_Amo, ANOVA_df_Indices_Cau, 
                       ANOVA_df_Indices_Neg, ANOVA_df_Indices_Soe) %>%
  #' add in significant stars:
  mutate(
    p_value_2 = cut(as.numeric(p_value), breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
                    labels = c("***", "**", "*", "ns"))
  )

ANOVA_df_full



#### Plot boxplots for metric values for each day after precipitation event ####
#' -------------------------------------------------------------------------------------------


#' Plot:

plot_boxplots_dae <- function(dataframe, region, height, export) {
  
  if(region == "Soebatsfontein") {
    date_remove <- c(c(as.Date("2018-10-11"): as.Date("2018-11-28")), 
                     c(as.Date("2020-05-20"): as.Date("2020-11-13")),
                     c(as.Date("2020-12-06"): as.Date("2021-03-24")))
    
    dataframe <- dataframe %>%
      filter(!Date %in% date_remove)
  }
  
  timeseries_data_plot <- dataframe %>%
    filter(Class == "Biocrusts") %>%
    drop_na(dae)
  
  #' make boxplot:
  DAPE_plot <- ggplot(data = timeseries_data_plot, aes(x = dae, group=dae, y=median)) + 
    facet_wrap(.~Index, ncol = 4, scales = "free") +
    geom_boxplot(linetype = "dashed", outlier.shape = NA, size = 0.3, width = .25) +
    stat_boxplot(aes(ymin = ..lower.., ymax = ..upper..), size = 0.3, 
                 fill="#1BB325", 
                 outlier.shape = 21, outlier.size = 0.5, width = .5 ) +
    stat_boxplot(geom = "errorbar", aes(ymin = ..ymax..), size= 0.3, width = 0.25) +
    stat_boxplot(geom = "errorbar", aes(ymax = ..ymin..), size = 0.3, width = 0.25) +
    scale_x_discrete(name = "Number of dry days after precipitation event") + 
    scale_y_continuous(name = "Metric values") + 
    theme_bw()+
    theme(axis.text.y = element_text(size=7, color = "black"), 
          axis.text.x = element_text(size=7, color = "black"), 
          axis.title = element_text(size=9, color = "black"), 
          panel.grid = element_blank(),        
          panel.grid.major.y = element_line(colour="lightgray", size = 0.1, linetype = "solid"),
          strip.text = element_text(size = 10, colour = "black"),
          strip.background = element_rect(fill = "white")
    )
  
  #plot(DAPE_plot)
  
  #' export plot:
  if(export == TRUE){
    ggsave(filename = paste0("DAPE_Metrics_", region, ".png"),
           plot = DAPE_plot, device = "png",
           path = "C:/Users/Rieser/OneDrive/EAGLE M.Sc/Term 5 (Winter 2021 - 2022)/Master Thesis/Graphics/DAPE Metrics/",
           width = 160, height = 110/height, units = "mm", dpi = 300)
  }

}

plot_boxplots_dae(timeseries_Lie, "Lieberose", 1, T)
plot_boxplots_dae(timeseries_Amo, "Amoladeras", 1, T)
plot_boxplots_dae(timeseries_Cau, "Cautivo", 1, T)
plot_boxplots_dae(timeseries_Neg, "Negev", 3, T)
plot_boxplots_dae(timeseries_Soe, "Soebatsfontein", 1, T)



#### Plot TLCC for metric values for 10 days ####
#' -------------------------------------------------------------------------------------------


plot_barplots_cor <- function(dataframe, region, height, export) {
  
  if(region == "Soebatsfontein") {
    date_remove <- c(c(as.Date("2018-10-11"): as.Date("2018-11-28")), 
                     c(as.Date("2020-05-20"): as.Date("2020-11-13")),
                     c(as.Date("2020-12-06"): as.Date("2021-03-24")))
    
    dataframe <- dataframe %>%
      filter(!Date %in% date_remove)
  }
  
  timeseries_data_plot <- dataframe %>%
    filter(Class == "Biocrusts")
  
  #' calculate corellations for each metric:
  list_TLCC_df <- list()
  
  for(i in 1:length(unique(timeseries_data_plot$Index))){
    #' filter to single metric:
    index <- unique(timeseries_data_plot$Index)[i]
    df <- timeseries_data_plot %>%
      filter(Index == index)
    #' calculate time lagged cross correlations to get the best lag: 
    TLCC <- ccf(df$Precipitation, df$median, lag.max = 10, na.action = na.pass, type = "correlation", plot = F)
    #' convert to data frame:
    list_TLCC_df[[i]] <- data.frame(Lag = TLCC$lag, Correlation = TLCC$acf, Index = index)
  }
  
  #' combine all corellation dataframes:
  TLCC_metrics_df <- do.call(rbind, list_TLCC_df)
  
  #' Plot barplot:
  Cor_plot <- ggplot(data = TLCC_metrics_df, aes(x = Lag, y = Correlation)) + 
    facet_wrap(.~Index, ncol = 6, scales = "free") +
    geom_bar(stat = "identity", fill = "#215495") +
    scale_x_continuous(limits = c(-0.5, 5.5),
                       breaks = seq(0, 5, 1),
                       expand = c(0,0),
                       name = "Lag (days)") +
    scale_y_continuous(limits = c(-0.5,0.5),
                       breaks = seq(-1, 1, 0.2),
                       expand = c(0,0)) +
    theme_bw() +
    theme(axis.text.y = element_text(size=7, color = "black"), 
          axis.text.x = element_text(size=7, color = "black"), 
          axis.title = element_text(size=9, color = "black"), 
          panel.grid = element_blank(),        
          panel.grid.major.y = element_line(colour="lightgray", size = 0.1, linetype = "solid"),
          strip.text = element_text(size = 10, colour = "black"),
          strip.background = element_rect(fill = "white"))
  
  #plot(Cor_plot)
  
  #' export plot:
  if(export == TRUE){
    ggsave(filename = paste0("TLCC_Metrics_", region, ".png"),
           plot = Cor_plot, device = "png",
           path = "C:/Users/Rieser/OneDrive/EAGLE M.Sc/Term 5 (Winter 2021 - 2022)/Master Thesis/Graphics/TLCC Metrics/",
           width = 160, height = 70/height, units = "mm", dpi = 300)
  }
  
}

plot_barplots_cor(timeseries_Lie, "Lieberose", 1, T)
plot_barplots_cor(timeseries_Amo, "Amoladeras", 1, T)
plot_barplots_cor(timeseries_Cau, "Cautivo", 1, T)
plot_barplots_cor(timeseries_Neg, "Negev", 2, T)
plot_barplots_cor(timeseries_Soe, "Soebatsfontein", 1, T)



#### Seasonal differences ####
#' -------------------------------------------------------------------------------------------


plot_boxplots_season <- function(dataframe, region, legend_position, height, export) {
  
  timeseries_data_plot <- dataframe %>%
    filter(Class == "Biocrusts"
    )
  
  #' make boxplot:
  Seasonal_plot <- ggplot(data = timeseries_data_plot, aes(x = Season, fill = Season, y=median)) + 
    facet_wrap(.~Index, scales = "free", ncol = 6) +
    geom_boxplot(linetype = "dashed", outlier.shape = NA, size = 0.3, width = .25) +
    stat_boxplot(aes(ymin = ..lower.., ymax = ..upper..), size = 0.3, 
                 outlier.shape = 21, outlier.size = 0.5, width = .5 ) +
    stat_boxplot(geom = "errorbar", aes(ymin = ..ymax..), size= 0.3, width = 0.25) +
    stat_boxplot(geom = "errorbar", aes(ymax = ..ymin..), size = 0.3, width = 0.25) +
    scale_x_discrete(name = "Seasons") + 
    scale_y_continuous(name = "Metric values") + 
    scale_fill_manual(values = c("#215495", "#1BB325", "#E32722", "#E3C922")) +
    geom_signif(comparisons = list(c("Winter", "Spring"),
                                   c("Spring", "Summer"),
                                   c("Summer", "Autumn"),
                                   c("Winter", "Summer"),
                                   c("Spring", "Autumn"),
                                   c("Winter", "Autumn")
                                   ),
                step_increase = 0.05, vjust = 1.5,
                map_signif_level = TRUE, textsize = 4/.pt, tip_length = 0.05/.pt, size = 0.1
                ) +
    theme_bw()+
    theme(axis.text.y = element_text(size=7, color = "black"), 
          axis.text.x = element_blank(), 
          axis.title.y = element_text(size=9, color = "black"), 
          axis.title.x = element_blank(), 
          axis.ticks.x = element_blank(),
          panel.grid = element_blank(),        
          panel.grid.major.y = element_line(colour="lightgray", size = 0.1, linetype = "solid"),
          strip.text = element_text(size = 10, colour = "black"),
          strip.background = element_rect(fill = "white"),
          legend.position = legend_position,
          legend.title = element_text(size=9, color = "black"),
          legend.text = element_text(size=7, color = "black"),
          legend.key.height = unit(20, "pt"),
          legend.key.width = unit(8, "pt")
    )
  
  
  #plot(Seasonal_plot)
  
  #' export plot:
  if(export == TRUE){
    ggsave(filename = paste0("Metrics_Seasonal_", region, ".png"),
           plot = Seasonal_plot, device = "png",
           path = "C:/Users/Rieser/OneDrive/EAGLE M.Sc/Term 5 (Winter 2021 - 2022)/Master Thesis/Graphics/Seasonal Differences/",
           width = 160, height = 120/height, units = "mm", dpi = 300)
  }
  
}

plot_boxplots_season(timeseries_Lie, "Lieberose", c(0.925, 0.25), 1, T)
plot_boxplots_season(timeseries_Amo, "Amoladeras", c(0.925, 0.25), 1, T)
plot_boxplots_season(timeseries_Cau, "Cautivo", c(0.925, 0.25), 1, T)
plot_boxplots_season(timeseries_Neg, "Negev", "right", 2, T)
plot_boxplots_season(timeseries_Soe, "Soebatsfontein", c(0.925, 0.25), 1, T)



#### Differences in Classes ####
#' -------------------------------------------------------------------------------------------


plot_boxplots_classes <- function(dataframe, region, legend_position, height, export) {
  
  timeseries_data_plot <- dataframe %>%
    drop_na(Class)
  
  #' make boxplot:
  Class_plot <- ggplot(data = timeseries_data_plot, aes(x = Class, fill = Class, y= median)) + 
    facet_wrap(.~Index, scales = "free", ncol = 6) +
    geom_boxplot(linetype = "dashed", outlier.shape = NA, size = 0.3, width = .25) +
    stat_boxplot(aes(ymin = ..lower.., ymax = ..upper..), size = 0.3, 
                 outlier.shape = 21, outlier.size = 0.5, width = .5 ) +
    stat_boxplot(geom = "errorbar", aes(ymin = ..ymax..), size= 0.3, width = 0.25) +
    stat_boxplot(geom = "errorbar", aes(ymax = ..ymin..), size = 0.3, width = 0.25) +
    scale_x_discrete(name = "Seasons") + 
    scale_y_continuous(name = "Metric values") + 
    scale_fill_manual(values = c("#E3C922", "#215495", "#1BB325"),
                      labels = c("Bare", "Biocrusts", "Vegetation")) +
    geom_signif(comparisons = list(c("Bare Soil", "Biocrusts"),
                                   c("Biocrusts", "Vegetation"),
                                   c("Bare Soil", "Vegetation")
                                   ),
                step_increase = 0.05, vjust = 1.5,
                map_signif_level = TRUE, textsize = 4/.pt, tip_length = 0.05/.pt, size = 0.1
                ) +
    theme_bw()+
    theme(axis.text.y = element_text(size=7, color = "black"), 
          axis.text.x = element_blank(), 
          axis.title.y = element_text(size=9, color = "black"), 
          axis.title.x = element_blank(), 
          axis.ticks.x = element_blank(),
          panel.grid = element_blank(),        
          panel.grid.major.y = element_line(colour="lightgray", size = 0.1, linetype = "solid"),
          strip.text = element_text(size = 10, colour = "black"),
          strip.background = element_rect(fill = "white"),
          legend.position = legend_position,
          legend.title = element_text(size=9, color = "black"),
          legend.text = element_text(size=7, color = "black"),
          legend.key.height = unit(20, "pt"),
          legend.key.width = unit(8, "pt")
    )
  
  
  #plot(Seasonal_plot)
  
  #' export plot:
  if(export == TRUE){
    ggsave(filename = paste0("Classes_difference_", region, ".png"),
           plot = Class_plot, device = "png",
           path = "C:/Users/Rieser/OneDrive/EAGLE M.Sc/Term 5 (Winter 2021 - 2022)/Master Thesis/Graphics/Class Differences/",
           width = 160, height = 90/height, units = "mm", dpi = 300)
  }
  
}

plot_boxplots_classes(timeseries_Lie, "Lieberose", c(0.925, 0.25), 1, T)
plot_boxplots_classes(timeseries_Amo, "Amoladeras", c(0.925, 0.25), 1, T)
plot_boxplots_classes(timeseries_Cau, "Cautivo", c(0.925, 0.25), 1, T)
plot_boxplots_classes(timeseries_Neg, "Negev", "right", 2, T)
plot_boxplots_classes(timeseries_Soe, "Soebatsfontein", c(0.925, 0.25), 1, T)

