#############################################################################################
#######################  Time Series Statistics  ####################### 
#############################################################################################




#' load required packages
#' -------------------------------------------------------------------------------------------


#' the required packages:
Packages <- c("raster", "lubridate", "parallel", "doParallel", "foreach", "dplyr",
              "tidyr", "forcats", "xlsx")
lapply(Packages, library, character.only = TRUE)


#' change parameters:
region = "Negev"



#' setup parallel processing:
#' -------------------------------------------------------------------------------------------


#' parallel processing:
no_cores <- 5 #detectCores() - 2  
registerDoParallel(cores=no_cores)  
cl <- makeCluster(no_cores, type="PSOCK")  



#' Function to calculate daily statistics:
#' -------------------------------------------------------------------------------------------

calculate_daily_statistics <- function(sensor) {
  
  
  ####' Satellite data:
  #' ------------------
  
  
  #' path to the Satellite images:
  images_path <- paste0("C:/GEODATA/Master Thesis/Satellite data/Images/", sensor, "/", region)
  
  #' list of all images:
  list_images <- list.files(images_path, recursive = F, full.names = T, 
                                  pattern = "*.tif$")
  
  #' import all images to list:
  list_images <- setNames(lapply(list_images, stack), 
                                substring(tools::file_path_sans_ext(basename(list_images)), 
                                          first = 0, last = 10))
  
  #' select the indices:
  if (sensor == "Sentinel-1"){
    list_images <- list_images
  } else {
    list_images <- sapply(X = names(list_images),
                                 FUN = function(x) raster::subset(list_images[[x]], 5:8),
                                 simplify = F, USE.NAMES = T)
  }
  
  
  ####' Classification data:
  #' -----------------------
  
  
  #' path to the classification rasters:
  classification_path <- paste0("C:/Users/Rieser/OneDrive/EAGLE M.Sc/Term 5 (Winter 2021 - 2022)/Master Thesis/Results/", region, "/Classification/")
  
  #' list of all classification files:
  list_classification <- list.files(classification_path, recursive = F, full.names = T, 
                                  pattern = "*.tif$")
  
  #' import all images to list:
  stack_classification <- stack(list_classification)
  names(stack_classification) <- c("Class", "Probability_Bare", "Probability_Biocrusts", "Probability_Vegetation")
  
  
  ####' Combine the datasets:
  #' -----------------------
  
  
  #' Combine images with classification:
  list_images_class <- sapply(X = names(list_images),
                                     FUN = function(x) stack(list_images[[x]], stack_classification),
                                     simplify = F, USE.NAMES = T)
  
  
    ####' Calculate metrics:
  #' -----------------------
  
  
  #' calculate median and stDev of each index, acquisition and class:
  #' list to store results:
  list_df_stats <- list()
  
  #' calculation for each image:
  list_df_stats <- foreach (i = 1:length(list_images_class), 
                            .packages = c("raster", "dplyr", "tidyr", "lubridate", "forcats")) %dopar% {
    
    date <- as.Date(names(list_images_class)[[i]])                                 #' store acquisition date
    
    list_df_stats[[i]] <- raster::as.data.frame(list_images_class[[i]], na.rm = T) %>%
      rename(Class = Class_category) %>%                                           #' rename column
      pivot_longer(cols = c(Probability_Bare, Probability_Biocrusts, Probability_Vegetation), 
                   names_to = "Class_Probability", values_to = "Probability") %>%  #' combine different probabilities in single column
      filter(Probability >= 0.9) %>%                                               #' only high-probability pixels
      select(-c(Class_Probability, Probability)) %>%                               #' select columns
      pivot_longer(cols = !Class, 
                   names_to = "Index", values_to = "Value") %>%                    #' convert indices columns to factors
      group_by(Class, Index) %>%                                                   #' calculations for each class and index
      dplyr::summarize(median = median(Value, na.rm = T),                          #' calculate median
                       stDev = sd(Value, na.rm = T),                               #' calculate stdev
                       .groups = "keep") %>%
      mutate(Date = date, .before = Class,                                         #' insert acquisition date as column
             Year = format.Date(Date, format = "%Y"),                              #' insert year
             DoY = yday(Date),                                                     #' insert DoY 
             Sensor = sensor,                                                      #' insert Sensor
             Class =  fct_recode(Class, "Bare Soil" = "1",                         #' change class names
                                        "Biocrusts" = "2", 
                                        "Vegetation" = "3")
      ) 
  }

  #' combine all results in single dataframe:
  df_stats <- do.call("rbind", list_df_stats)
  
  return(df_stats)
  
}

#' run calculations for the three different sensors:

t1 <- Sys.time()
S2_statistics <- calculate_daily_statistics("Sentinel-2")

LS_statistics <- calculate_daily_statistics("Landsat")

S1_statistics <- calculate_daily_statistics("Sentinel-1")
t2 <- Sys.time()
t2-t1

#' stop parallel cluster:
stopCluster(cl)



#' Join and export:
#' -------------------------------------------------------------------------------------------


#' join the tables:
statistics_full <-  rbind(S2_statistics, LS_statistics, S1_statistics) %>%
  arrange(Date, Class, Index) %>%
  as.data.frame()

head(statistics_full)
unique(statistics_full$Sensor)

#' Export the table to excel file:
write.xlsx(x = statistics_full, 
           file = paste0("C:/Users/Rieser/OneDrive/EAGLE M.Sc/Term 5 (Winter 2021 - 2022)/Master Thesis/Results/", region, 
                         "/Time Series/Time_series_daily_statistics_", region, ".xlsx"), 
           col.names = TRUE, row.names = FALSE)
