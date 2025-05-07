library(raster)
library(tidyr)
library(xlsx)


study_area <- "Amoladeras"


# Get filesnames of high resolution images
landsat_image <- stack(paste0("G:/My Drive/Regression_Images/Regression_Landsat_", study_area, ".tif"))
sentinel_image <- stack(paste0("G:/My Drive/Regression_Images/Regression_Sentinel-2_", study_area, ".tif")) 
plot(sentinel_image$NDVI)
plot(landsat_image$NDVI)


coefficients_list <- list()

for (i in 1:length(names(sentinel_image))){
    band <- names(sentinel_image)[[i]]
    stack_bands <- stack(sentinel_image[[i]], landsat_image[[i]])
    df <- data.frame(na.omit(values(stack_bands)))
    names(df) = c("Sentinel", "Landsat")
    coefficients_band <- lm(Sentinel ~ Landsat, data=df)
    df_coefficients <- as.data.frame(coefficients_band$coefficients)
    df_coefficients$Band <- band
    df_coefficients$Type <- c("Intercept", "Slope")
    names(df_coefficients) <- c("Coefficients", "Band", "Type")
    coefficients_band_wide <- pivot_wider(df_coefficients, names_from = Type, values_from = Coefficients)
    coefficients_band_wide_df <- as.data.frame(coefficients_band_wide)
    
    coefficients_list[[i]] <- coefficients_band_wide_df
}


df <- do.call("rbind", coefficients_list)
df

write.xlsx(x = df, file = paste0("G:/My Drive/Regression_Images/Coefficients_Landsat_", study_area ,"_casted.xlsx"), col.names = T, row.names = F)
