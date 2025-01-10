

# ** on GW HPC (Pegasus) ** ##
library(sp) 
library(ncdf4) 
library(terra)
library(raster) 
library(sf)
library(dplyr)   



#reading shp file (13k city boundary) (* should have .shp, .dbf, .shx)
city.shp <- st_read("/GWSPH/groups/anenberggrp/sykim/data/hdc_naming_warp.shp")
city <- vect(city.shp)

#PWNO2 estimation (2000-2020)
years <- c(2000:2020)
for(i in years)  {
  #read NO2
  pm25 <- terra::rast(paste0("/GWSPH/groups/anenberggrp/soga/data/vandonkelaar_pm25/processed/clip_vandonkelaarpm25_", i, ".tif"))
  pm25 <- pm25[[1]]
  names(pm25) <- 'pm25'
  pm25[pm25 == 0] <- NA
  #read population
  pop <- terra::rast(paste0("/GWSPH/groups/anenberggrp/sykim/data/ppp_", i, "_1km_Aggregated.tif"))
  #matching resolution
  pm251 <- resample(pm25, pop, method="bilinear")
  #removing every missing value from both pollution/population datasets 
  mask_pm25 <- !is.na(pm251)
  mask_pop <- !is.na(pop)
  combined_mask <- mask_pm25 & mask_pop
  pm251 <- mask(pm251, combined_mask, maskvalues = FALSE)
  pop <- mask(pop, combined_mask, maskvalues = FALSE)
  
  #estimate numerator of PWO3 (pop*o3)
  popxpm25 <- pop*pm251
  num<-terra::extract(popxpm25, city, fun=sum, na.rm=TRUE)
  names(num)[2] <- "pop_times_pol_sum"
  #estimate denominator of PWO3 (sum of pop)
  den<-terra::extract(pop, city, fun=sum, na.rm=TRUE)
  names(den)[2] <- "pop_sum"
  
  #merge numerator and denominator datasets
  pw <- merge(num, den, by='ID')
  pw$pw_pm=pw$pop_times_pol_sum/pw$pop_sum
  
  #export
  write.csv(pw, paste0("/GWSPH/groups/anenberggrp/sykim/data/new/pwpm_", i, "_13k_24jul1.csv"), row.names = TRUE)
}






#reading shp file (c40 city boundary) (* should have .shp, .dbf, .shx)
city.shp1 <- st_read("/GWSPH/groups/anenberggrp/sykim/data/C40.shp")
city1 <- vect(city.shp1)

#PWNO2 estimation (2000-2020)
years <- c(2000:2020)
for(i in years)  {
  #read NO2
  pm25 <- terra::rast(paste0("/GWSPH/groups/anenberggrp/soga/data/vandonkelaar_pm25/processed/clip_vandonkelaarpm25_", i, ".tif"))
  pm25 <- pm25[[1]]
  names(pm25) <- 'pm25'
  pm25[pm25 == 0] <- NA
  #read population
  pop <- terra::rast(paste0("/GWSPH/groups/anenberggrp/sykim/data/ppp_", i, "_1km_Aggregated.tif"))
  #matching resolution
  pm251 <- resample(pm25, pop, method="bilinear")
  #removing every missing value from both pollution/population datasets 
  mask_pm25 <- !is.na(pm251)
  mask_pop <- !is.na(pop)
  combined_mask <- mask_pm25 & mask_pop
  pm251 <- mask(pm251, combined_mask, maskvalues = FALSE)
  pop <- mask(pop, combined_mask, maskvalues = FALSE)
  
  #estimate numerator of PWO3 (pop*o3)
  popxpm25 <- pop*pm251
  num<-terra::extract(popxpm25, city1, fun=sum, na.rm=TRUE)
  names(num)[2] <- "pop_times_pol_sum"
  #estimate denominator of PWO3 (sum of pop)
  den<-terra::extract(pop, city1, fun=sum, na.rm=TRUE)
  names(den)[2] <- "pop_sum"
  
  #merge numerator and denominator datasets
  pw <- merge(num, den, by='ID')
  pw$pw_pm=pw$pop_times_pol_sum/pw$pop_sum
  
  #export
  write.csv(pw, paste0("/GWSPH/groups/anenberggrp/sykim/data/new/pwpm_", i, "_c40_24jul1.csv"), row.names = TRUE)
}


