library(raster)

# Define the Proj.4 spatial reference 
EPSG_Lieberose <- "+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs" 
EPSG_Negev <- "+proj=utm +zone=36 +datum=WGS84 +units=m +no_defs" 
EPSG_Spain <- "+proj=utm +zone=30 +datum=WGS84 +units=m +no_defs" 
EPSG_Soebatsfontein <- "+proj=utm +zone=33 +south +datum=WGS84 +units=m +no_defs" 

Reproject_DEM <- function(region, EPSG){
  #' load dem:
  DEM <- raster(paste0("C:/GEODATA/Master Thesis/Terrain/", region, "/DEM_", region, ".tif"))
  
  #' load reference raster:
  reference_raster <- stack(paste0("C:/GEODATA/Master Thesis/Satellite data/Metrics/", region, "/Sentinel-2/median_Sentinel-2_", region, "_2017-01-01_2021-12-31.tif"))
  
  # Project Raster
  projected_raster <- projectRaster(DEM, crs = EPSG, res = 30)

  #' resample Raster:
  DEM_new <- raster::resample(projected_raster, reference_raster, method = "bilinear")

  #' export the new DEM to disk as raster:
  writeRaster(DEM_new, filename=paste0("C:/GEODATA/Master Thesis/Terrain/", region, "/DEM_", region, "_res"),
              format="GTiff", overwrite=TRUE, options=c("INTERLEAVE=BAND","COMPRESS=LZW"))
  
  return(DEM_new) 
}

DEM_res <- Reproject_DEM("Soebatsfontein", EPSG_Soebatsfontein)
plot(DEM_res)

