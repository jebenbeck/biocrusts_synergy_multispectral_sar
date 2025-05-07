library(remotes)
install_github("ropenspain/climaemet", force = T)
library(climaemet)
library(sf)
library(xlsx)

path <- "C:/GEODATA/Master Thesis/Meteorological data/"

#' Use this function to register your API Key temporarly or permanently
aemet_api_key("eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJqYWtvYi5yaWVzZXJAc3R1ZC1tYWlsLnVuaS13dWVyemJ1cmcuZGUiLCJqdGkiOiJkYzE2NGQ1Yy02MGFmLTQxMGEtYjEyYy1lZDllM2RjNGQxODciLCJpc3MiOiJBRU1FVCIsImlhdCI6MTY0NzQ1NTI4MywidXNlcklkIjoiZGMxNjRkNWMtNjBhZi00MTBhLWIxMmMtZWQ5ZTNkYzRkMTg3Iiwicm9sZSI6IiJ9.8lt3tXDjJr2LEGVlC75VH2cHeFbzeJO2PE_XjyTCg8I", install = T)

#' Get AEMET stations
stations <- aemet_stations(return_sf = T) # Need to have the API Key registered

plot(stations)

#' export stations to shapefile to see which are nearest to the AOI
#st_write(stations, "C:/GEODATA/Master Thesis/Meteorological data/AEMET_stations.shp")

#' select closest station 
Almeria_station <- "6325O" #' ALMERÍA AEROPUERTO station identifier
Abla_station <- "6302A" #' ABLA station identifier

#' get daily data for selected timeframe
data_daily_almeria <- aemet_daily_clim(Almeria_station, start = "1980-01-01", end = "2021-12-31")
data_daily_abla <- aemet_daily_clim(Abla_station, start = "1980-01-01", end = "2021-12-31")

data_daily_almeria_df <- as.data.frame(data_daily_almeria)
data_daily_abla_df <- as.data.frame(data_daily_abla)

head(data_daily_almeria_df)
head(data_daily_abla_df)

#' export excel table:
write.xlsx(x = data_daily_almeria_df, file = paste0(path, "Almeria_daily_19800101_20211231.xlsx"), col.names=TRUE, row.names=FALSE)
write.xlsx(x = data_daily_abla_df, file = paste0(path, "Abla_daily_20010101_20211231.xlsx"), col.names=TRUE, row.names=FALSE)
