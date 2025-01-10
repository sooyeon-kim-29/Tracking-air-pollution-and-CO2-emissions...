
library(sp) 
library(ncdf4) 
library(terra)
library(raster) 
library(sf)
library(dplyr) 
library(exactextractr)
options(scipen = 999, digits = 6)
year <- c(2000:2020)


#reading shp file (13k city boundary) (* should have .shp, .dbf, .shx)
city.shp <- st_read("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/NO2/Inputs/hdc_naming_warp.shp")
city <- as_Spatial(city.shp)

city.shp1 <- st_read("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/NO2/Inputs/C40.shp")
city1 <- as_Spatial(city.shp1)



for (l in 2000:2003) { #TNR_Aviation_SPS exists only till 2003, so separate periods into ~2003 and 2003~
  
  #13k 
  j <- "ENE" #seed
  co2 <- raster(paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/CO2/Inputs/v8/", j, "_emi_nc/v8.0_FT2022_GHG_CO2_", l, "_", j, "_emi.nc"))
  names(co2) <- "co2"
  co2[is.na(co2)] <- 0
  
  #for loop
  sctr <- c("IND", "REF_TRF", "TNR_Aviation_CDS", "TNR_Aviation_LTO", "TNR_Aviation_SPS", "TNR_Aviation_CRS", "TRO", "TNR_Other", "TNR_Ship", "PRO_FFF", "RCO", "NMM", "CHE", "IRO", "NEU", "PRU_SOL", "AGS", "SWD_INC", "NFE")
  for (i in 1:19) {
    j <- sctr[i]
    co2_ <- raster(paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/CO2/Inputs/v8/", j, "_emi_nc/v8.0_FT2022_GHG_CO2_", l, "_", j, "_emi.nc"))
    names(co2_) <- "co2"
    co2_[is.na(co2_)] <- 0
    
    co2 <- co2 + co2_
  }
  
  #population
  pop <- raster(paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/NO2/Inputs/ppp_", l, "_1km_Aggregated.tif"), varname="pop")
  pop_<-raster::extract(pop, city, fun=sum, na.rm=TRUE, weights=FALSE, df=TRUE)
  names(pop_)[2] <- "pop"
  write.csv(pop_, paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/CO2/Outputs/v8/pop_13k_", l, ".csv"), row.names = TRUE)
  gc()
  
  #CO2
  co2_ <- raster::extract(co2, city, fun=sum, na.rm=TRUE, weights=FALSE, df=TRUE)
  gc()
  
  #CO2 per capita
  co2_per_cap <- merge(co2_, pop_, by="ID")
  co2_per_cap$layer <- ifelse(co2_per_cap$layer==0, NA, co2_per_cap$layer)
  co2_per_cap$co2_per_cap <- co2_per_cap$layer/co2_per_cap$pop
  co2_per_cap$year <- l
  
  write.csv(co2_per_cap, paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/CO2/Outputs/v8/co2_per_capita_13k_", l, ".csv"), row.names = TRUE)
  
  
  
  #C40
  #population
  pop_<-raster::extract(pop, city1, fun=sum, na.rm=TRUE, weights=FALSE, df=TRUE)
  names(pop_)[2] <- "pop"
  write.csv(pop_, paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/CO2/Outputs/v8/pop_c40_", l, ".csv"), row.names = TRUE)
  gc()
  
  #CO2
  co2_ <- raster::extract(co2, city1, fun=sum, na.rm=TRUE, weights=FALSE, df=TRUE)
  gc()
  
  #CO2 per capita
  co2_per_cap <- merge(co2_, pop_, by="ID")
  co2_per_cap$layer <- ifelse(co2_per_cap$layer==0, NA, co2_per_cap$layer)
  co2_per_cap$co2_per_cap <- co2_per_cap$layer/co2_per_cap$pop
  co2_per_cap$year <- l
  
  write.csv(co2_per_cap, paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/CO2/Outputs/v8/co2_per_capita_c40_", l, ".csv"), row.names = TRUE)
  
}




for (l in 2004:2020) { #TNR_Aviation_SPS exists only till 2003, so separate periods into ~2003 and 2003~
  
  #13k 
  j <- "ENE" #seed
  co2 <- raster(paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/CO2/Inputs/v8/", j, "_emi_nc/v8.0_FT2022_GHG_CO2_", l, "_", j, "_emi.nc"))
  names(co2) <- "co2"
  co2[is.na(co2)] <- 0
  
  #for loop
  sctr <- c("IND", "REF_TRF", "TNR_Aviation_CDS", "TNR_Aviation_LTO", "TNR_Aviation_CRS", "TRO", "TNR_Other", "TNR_Ship", "PRO_FFF", "RCO", "NMM", "CHE", "IRO", "NEU", "PRU_SOL", "AGS", "SWD_INC", "NFE")
  for (i in 1:18) {
    j <- sctr[i]
    co2_ <- raster(paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/CO2/Inputs/v8/", j, "_emi_nc/v8.0_FT2022_GHG_CO2_", l, "_", j, "_emi.nc"))
    names(co2_) <- "co2"
    co2_[is.na(co2_)] <- 0
    
    co2 <- co2 + co2_
  }
  
  #population
  pop <- raster(paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/NO2/Inputs/ppp_", l, "_1km_Aggregated.tif"), varname="pop")
  pop_<-raster::extract(pop, city, fun=sum, na.rm=TRUE, weights=FALSE, df=TRUE)
  names(pop_)[2] <- "pop"
  write.csv(pop_, paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/CO2/Outputs/v8/pop_13k_", l, ".csv"), row.names = TRUE)
  gc()
  
  #CO2
  co2_ <- raster::extract(co2, city, fun=sum, na.rm=TRUE, weights=FALSE, df=TRUE)
  gc()
  
  #CO2 per capita
  co2_per_cap <- merge(co2_, pop_, by="ID")
  co2_per_cap$layer <- ifelse(co2_per_cap$layer==0, NA, co2_per_cap$layer)
  co2_per_cap$co2_per_cap <- co2_per_cap$layer/co2_per_cap$pop
  co2_per_cap$year <- l
  
  write.csv(co2_per_cap, paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/CO2/Outputs/v8/co2_per_capita_13k_", l, ".csv"), row.names = TRUE)
  
  
  
  #C40
  #population
  pop_<-raster::extract(pop, city1, fun=sum, na.rm=TRUE, weights=FALSE, df=TRUE)
  names(pop_)[2] <- "pop"
  write.csv(pop_, paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/CO2/Outputs/v8/pop_c40_", l, ".csv"), row.names = TRUE)
  gc()
  
  #CO2
  co2_ <- raster::extract(co2, city1, fun=sum, na.rm=TRUE, weights=FALSE, df=TRUE)
  gc()
  
  #CO2 per capita
  co2_per_cap <- merge(co2_, pop_, by="ID")
  co2_per_cap$layer <- ifelse(co2_per_cap$layer==0, NA, co2_per_cap$layer)
  co2_per_cap$co2_per_cap <- co2_per_cap$layer/co2_per_cap$pop
  co2_per_cap$year <- l
  
  write.csv(co2_per_cap, paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/CO2/Outputs/v8/co2_per_capita_c40_", l, ".csv"), row.names = TRUE)
  
}



