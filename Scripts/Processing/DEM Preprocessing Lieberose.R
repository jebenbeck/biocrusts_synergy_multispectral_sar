#' ----
#' Title: DEM preprocessing
#' Description: Generates a .tif raster from multiple ASCII DEM tiles
#' Author: Jakob Rieser
#' ----



# Setup ----


#' Packages needed:
Packages <- c("plyr", "raster")

lapply(Packages, library, character.only = TRUE)

#' definition of input path. This is the folder holding the ASCII files
indir <- "C:/GEODATA/Master Thesis/Terrain/Lieberose/ASCII"

#' definition of the outpot path. This is the location the DEM .tif raster will be exported to  
outdir <- "C:/GEODATA/Master Thesis/Terrain/Lieberose/TIF/"



# Data import ----


#' import all ascii to a list (may take up to a few minutes depending on file size):

list_asc <- list.files(indir, full.names = T, pattern = "*.xyz")

file_list <- lapply(list_asc, read.table, sep =" ")

names(file_list) <- gsub(".xyz","", list.files(indir, full.names = FALSE, pattern = "*.xyz"), fixed = TRUE)



# Restructure the data ----


#' change the column names, as R sets the default names to "V1", "V2" and "V3" to the corresponding coordinate:
file_list <- lapply(file_list, setNames, c("longitude", "latitude", "height"))

#' combine all tables in list to one huge table:
table_full <- ldply(file_list, rbind)

#' remove the first column:
table_full_2 <- subset(table_full, select = c(longitude, latitude, height))

#' separate the x and y (long, lat) from the z (height)
xy <- table_full_2[,c(1,2)]



# Spatial data ----


#' make a Spatial Points dataframe from each observation (row) featuring lon and lat as the coordinates and height as the data:
spdf <- SpatialPointsDataFrame(coords = xy, data = table_full_2[3],
                               proj4string = CRS("+init=epsg:25833"))
spdf

# coerce this to a SpatialPixelsDataFrame
gridded(spdf) <- TRUE

# coerce to raster
DEM_raster <- raster(spdf)
DEM_raster
plot(DEM_raster)



# Data export ----
writeRaster(DEM_raster, paste0(outdir, "DEM.tif"), format = "GTiff", overwrite = T)