#' load packages:
#' -------------------------------------------------------------------------------------------

Packages <- c("readxl", "ggplot2", "dplyr", "tidyr", "gridExtra", "lubridate", "ggsignif")
lapply(Packages, library, character.only = TRUE)

apply_ANOVA <- function(region) {
  
#' load datasets:
#' -------------------------------------------------------------------------------------------


#' read in the sensor timeseries data:
sensor_timeseries_data <- read_xlsx(path = paste0("C:/Users/Rieser/OneDrive/EAGLE M.Sc/Term 5 (Winter 2021 - 2022)/Master Thesis/Results/", 
                                                  region, "/Time Series/Time_series_daily_statistics_", region, ".xlsx"),
                                    sheet = 1, col_names = TRUE)

#' read in precipitation data:
precipitation_df <- read_xlsx(path = paste0("C:/Users/Rieser/OneDrive/EAGLE M.Sc/Term 5 (Winter 2021 - 2022)/Master Thesis/Data_2/Precipitation data/", region, ".xlsx"),
                              sheet = 1, col_names = TRUE) 



#' Data processing:
#' -------------------------------------------------------------------------------------------


#' The metrics time series data:
#' -----------------------------

#' the data recoding function, dataframe is the original table, recode_lookup contains the conversion matrix
metrics_new <- c("VV+VH", "VH/VV", "VV/VH", "VH*VV", "VV-VH")                                  #' new metrics
names(metrics_new) <- c("add_VVVH", "div_VHVV", "div_VVVH", "mult_VHVV", "sub_VVVH")           #' named with old Types

sensor_timeseries_data_rec <- sensor_timeseries_data %>%
  mutate(Index = recode(Index, !!!metrics_new,),
         Date = as.Date(Date)) %>%
  #filter(Index == "NDVI", Class == "Biocrusts") %>%
  as.data.frame()


#' The precipitation data:
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


if(region == "Soebatsfontein") {
  
  date_remove <- c(
    c(as.Date("2018-10-11"): as.Date("2018-11-28")), 
    c(as.Date("2020-05-20"): as.Date("2020-11-13")),
    c(as.Date("2020-12-06"): as.Date("2021-03-24")))
  
  precipitation_filter <- precipitation_filter %>%
    filter(!Date %in% date_remove)

}


#' Join both datasets:
#' -----------------------------

timeseries_select <- left_join(precipitation_filter, sensor_timeseries_data_rec, by = "Date", keep = F) %>%
  #' reformat the date to get info on DoY, Year and Season:
  mutate(Year = format.Date(Date, format = "%Y"),
         DoY = yday(Date),   
         Season = cut(DoY, breaks = c(-Inf, 60, 152, 244, 335, Inf),
                      labels = c("Winter", "Spring", "Summer", "Autumn", "Winter")))

#' arrange timeseries:
timeseries_select_1 <- timeseries_select %>%
  arrange(Index) %>%
  filter(Class == "Biocrusts") %>%
  group_by(Index) %>%
  group_split(.keep = T)

#' Calculate ANOVA:
#' -------------------------------------------------------------------------------------------


#' calculate ANOVA:
p <- list()
  
  for (i in 1:length(timeseries_select_1)){
    df <- timeseries_select_1[[i]]
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

ANOVA_df_BSC_Indices_Lie <- apply_ANOVA("Lieberose")
ANOVA_df_BSC_Indices_Amo <- apply_ANOVA("Amoladeras")
ANOVA_df_BSC_Indices_Cau <- apply_ANOVA("Cautivo")
ANOVA_df_BSC_Indices_Neg <- apply_ANOVA("Negev")
ANOVA_df_BSC_Indices_Soe <- apply_ANOVA("Soebatsfontein")

#' Combine all ANOVA values:
ANOVA_df_full <- rbind(ANOVA_df_BSC_Indices_Lie, ANOVA_df_BSC_Indices_Amo, ANOVA_df_BSC_Indices_Cau, 
      ANOVA_df_BSC_Indices_Neg, ANOVA_df_BSC_Indices_Soe)

#' add in significant stars:
ANOVA_df_full_2 <- ANOVA_df_full %>%
  mutate(
    p_value_2 = cut(as.numeric(p_value), breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
                 labels = c("***", "**", "*", "ns"))
  ) %>%
  arrange()

ANOVA_df_full_2


TukeyHSD(aov(timeseries_select_1$median ~ timeseries_select_1$dae))

