Packages <- c("raster", "rgdal","spatialEco", "exactextractr")


lapply(Packages, library, character.only = TRUE)





#' reference data processing for classification data
#' -------------------------------------------------------------------------------------------


region <- "Lieberose"

#' Read in the reference polygon shapefiles
reference_data <- readOGR(paste0("C:/GEODATA/Master Thesis/Reference Data/", region, "/Classification/Reference_data_", region, ".shp"))
plot(reference_data)
  
#' only keep the column "Name" containing the land cover information
names(reference_data) <- "Class"
head(reference_data)

#' define raster, the resampling will be based on:
file_reference_raster <- list.files(path = paste0("C:/GEODATA/Master Thesis/Satellite data/Metrics/", region, "/Sentinel-2/"),
                                    pattern = "*.tif" , full.names = T)[[1]]
  
sentinel_raster <- stack(file_reference_raster)
plot(sentinel_raster[[1]])
plot(reference_data, add = T)
  
#' get number of pixels within the AOI (not NAs)
pixelcount <- cellStats(sentinel_raster, function(i, ...) sum(!is.na(i)))
pixelcount[[1]]
  
#' convert classes to numeric values to make a reference raster dataset:
reference_data@data$Class <- as.factor(reference_data@data$Class)
levels(reference_data@data$Class) <- 1:length(levels(reference_data@data$Class))
reference_data@data$Class <- as.numeric(as.character(reference_data@data$Class))
unique(reference_data@data$Class)
  
#' rasterize the points:
reference_raster <- rasterize(reference_data, sentinel_raster, 'Class')
plot(reference_raster)
  
#' export raster:
writeRaster(reference_raster, filename = paste0("C:/GEODATA/Master Thesis/Reference data/", region, "/Classification/Reference_data_raster_classes_", region) , format="GTiff", 
            overwrite=TRUE)




#' reference data processing for regression data based on biocrust pixels:
#' -------------------------------------------------------------------------------------------


region <- "Soebatsfontein"
rectification <- "False"
clipping <- "True"


# Get the reference image:
if (region == "Soebatsfontein") {
  reference_raster <- raster(paste0("C:/GEODATA/Master Thesis/Reference data/", region, "/Products Emilio/CRCIA.tif"))
} else {
  reference_raster <- raster(paste0("C:/GEODATA/Master Thesis/Reference data/", region, "/Products Emilio/CDI_95.tif"))
}
reference_raster
plot(reference_raster)

#' define raster, the resampling will be based on:
file_sentinel_raster <- list.files(path = paste0("C:/GEODATA/Master Thesis/Satellite data/Metrics/", region, "/Sentinel-2/"),
                                    pattern = "*.tif" , full.names = T)[[1]]

sentinel_raster <- stack(file_sentinel_raster)[[1]]
plot(sentinel_raster[[1]])

#' select the biocrust pixels:
if (region == "Soebatsfontein"){
  biocrusts_raster <- reference_raster == 1
} else {
  biocrusts_raster <- reference_raster == 2
}

plot(biocrusts_raster)

if (rectification == "True") {
  #' rectify the reference image, as it is rotated:
  biocrusts_raster <- raster::rectify(biocrusts_raster, method = "ngb")
  plot(biocrusts_raster)
  
  #' export the rectified reference image:
  writeRaster(biocrusts_raster, filename = paste0("C:/GEODATA/Master Thesis/Reference data/", region, "/Products Emilio/CDI_95_rect"), format="GTiff", overwrite=TRUE)
}

#' resample the raster to 10-m Sentinel-2 cells:
reference_raster_10m <- exactextractr::exact_resample(biocrusts_raster, sentinel_raster, 'mean')
reference_raster_10m
plot(reference_raster_10m)

#' mask raster to subset area:
if(clipping == "True") {
  area <- readOGR(paste0("C:/GEODATA/Master Thesis/Reference data/", region, "/Regression/Reference_area.shp"))
  reference_raster_10m <- mask(reference_raster_10m, area)
  plot(reference_raster_10m)
}

#' export raster:
writeRaster(reference_raster_10m, filename = paste0("C:/GEODATA/Master Thesis/Reference data/", region, "/Regression/Reference_biocrust_probability_", region), format="GTiff", overwrite=TRUE)



#' reference data processing for regression data based on fractional coverage:
#' -------------------------------------------------------------------------------------------


region <- "Cautivo"

# Get the reference image:
reference_raster_lichen <- raster(paste0("C:/GEODATA/Master Thesis/Reference data/", region, "/Products Emilio/Cover probability maps/lichen_crusts.tif"))
plot(reference_raster_lichen)

reference_raster_cyano <- raster(paste0("C:/GEODATA/Master Thesis/Reference data/", region, "/Products Emilio/Cover probability maps/cyanobacteria_crusts.tif"))
plot(reference_raster_cyano)

#' add up cyanobacteria and lichen crust coverage:
biocrusts_raster <- reference_raster_cyano + reference_raster_lichen
plot(biocrusts_raster) 

#' define raster, the resampling will be based on:
file_sentinel_raster <- list.files(path = paste0("C:/GEODATA/Master Thesis/Satellite data/Metrics/", region, "/Sentinel-2/"),
                                    pattern = "*.tif" , full.names = T)[[1]]

sentinel_raster <- stack(file_sentinel_raster)[[1]]
plot(sentinel_raster)

#' resample the raster to 10-m Sentinel-2 cells:
reference_raster_10m <- exactextractr::exact_resample(biocrusts_raster, sentinel_raster, 'mean')
reference_raster_10m
plot(reference_raster_10m)

#' export raster:
writeRaster(reference_raster_10m, filename = paste0("C:/GEODATA/Master Thesis/Reference data/", region, "/Regression/Reference_biocrust_probability", region, "_fc"), format="GTiff", overwrite=TRUE)

