
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

#PWNO2 estimation (2005-2020)
years <- c(2005:2020)
for(i in years)  {
  #read NO2
  no2 <- terra::rast(paste0("/GWSPH/groups/anenberggrp/soga/data/larkin_no2/processed/clip_NO2_", i, ".tif"))
  no2 <- no2[[1]]
  names(no2) <- 'no2'
  # no2[no2 == 0] <- NA
  #read population
  pop <- terra::rast(paste0("/GWSPH/groups/anenberggrp/sykim/data/ppp_", i, "_1km_Aggregated.tif"))
  #matching resolution
  no21 <- resample(no2, pop, method="bilinear")
  #removing every missing value from both pollution/population datasets 
  mask_o3 <- !is.na(no21)
  mask_pop <- !is.na(pop)
  combined_mask <- mask_o3 & mask_pop
  no21 <- mask(no21, combined_mask, maskvalues = FALSE)
  pop <- mask(pop, combined_mask, maskvalues = FALSE)
  
  #estimate numerator of PWO3 (pop*o3)
  popxno2 <- pop*no21
  num<-terra::extract(popxno2, city, fun=sum, na.rm=TRUE)
  names(num)[2] <- "pop_times_pol_sum"
  #estimate denominator of PWO3 (sum of pop)
  den<-terra::extract(pop, city, fun=sum, na.rm=TRUE)
  names(den)[2] <- "pop_sum"
  
  #merge numerator and denominator datasets
  pw <- merge(num, den, by='ID')
  pw$pw_no2=pw$pop_times_pol_sum/pw$pop_sum
  
  #export
  write.csv(pw, paste0("/GWSPH/groups/anenberggrp/sykim/data/new/pwno2_", i, "_13k_24jul12.csv"), row.names = TRUE)
}






#reading shp file (c40 city boundary) (* should have .shp, .dbf, .shx)
city.shp1 <- st_read("/GWSPH/groups/anenberggrp/sykim/data/C40.shp")
city1 <- vect(city.shp1)

#PWNO2 estimation (2000-2020)
for(i in years)  {
  #read NO2
  no2 <- terra::rast(paste0("/GWSPH/groups/anenberggrp/soga/data/larkin_no2/processed/clip_NO2_", i, ".tif"))
  no2 <- no2[[1]]
  names(no2) <- 'no2'
  # no2[no2 == 0] <- NA
  #read population
  pop <- terra::rast(paste0("/GWSPH/groups/anenberggrp/sykim/data/ppp_", i, "_1km_Aggregated.tif"))
  #matching resolution
  no21 <- resample(no2, pop, method="bilinear")
  #removing every missing value from both pollution/population datasets 
  mask_o3 <- !is.na(no21)
  mask_pop <- !is.na(pop)
  combined_mask <- mask_o3 & mask_pop
  no21 <- mask(no21, combined_mask, maskvalues = FALSE)
  pop <- mask(pop, combined_mask, maskvalues = FALSE)
  
  #estimate numerator of PWO3 (pop*o3)
  popxno2 <- pop*no21
  num<-terra::extract(popxno2, city1, fun=sum, na.rm=TRUE)
  names(num)[2] <- "pop_times_pol_sum"
  #estimate denominator of PWO3 (sum of pop)
  den<-terra::extract(pop, city1, fun=sum, na.rm=TRUE)
  names(den)[2] <- "pop_sum"
  
  #merge numerator and denominator datasets
  pw <- merge(num, den, by='ID')
  pw$pw_no2=pw$pop_times_pol_sum/pw$pop_sum
  
  #export
  write.csv(pw, paste0("/GWSPH/groups/anenberggrp/sykim/data/new/pwno2_", i, "_c40_24jul12.csv"), row.names = TRUE)
}


