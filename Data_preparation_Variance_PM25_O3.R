

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
years <- c(2019, 2005, 2012)
#years <- c(2006:2011,2013:2018, 2020)
for(i in years)  {
  #read NO2 (1): pm2.5
  pm25 <- terra::rast(paste0("/GWSPH/groups/anenberggrp/soga/data/vandonkelaar_pm25/processed/clip_vandonkelaarpm25_", i, ".tif"))
  #read NO2 (2): uncertainty
  uncertainty <- terra::rast(paste0("/GWSPH/groups/anenberggrp/sykim/data/new/revision/PM25_Annual_uncertainty/V5GL03.HybridPM25E.Global.", i, "01-", i, "12.nc"))
  uncertainty <- uncertainty[[1]]
  print(i)
  #resample
  pm25 <- resample(pm25, uncertainty, method="bilinear")
  #variance (ug/m3)
  var <- (pm25*uncertainty/100)^2
  var_ <- terra::extract(var, city, fun=mean, na.rm=TRUE)
  
  #export
  write.csv(var_, paste0("/GWSPH/groups/anenberggrp/sykim/data/new/uncertainty/pwpm_", i, "_uncertainty_var_absolute_squared.csv"), row.names = TRUE)
  
}


#reading shp file (13k city boundary) (* should have .shp, .dbf, .shx)
city.shp <- st_read("/GWSPH/groups/anenberggrp/sykim/data/hdc_naming_warp.shp")
city <- vect(city.shp)

#PWNO2 estimation (2000-2020)
years <- c(2019, 2005, 2012, 2006:2011, 2013:2018, 2020)
for(i in years)  {
  #read NO2 (2): uncertainty
  uncertainty <- terra::rast(paste0("/GWSPH/groups/anenberggrp/sykim/data/O3/Inputs/all_grids_", i, ".tif"))
  uncertainty <- uncertainty[[2]]
  names(uncertainty) <- 'var'
  print(i)
  
  var <- terra::extract(uncertainty, city, fun=mean, na.rm=TRUE)
  
  #export
  write.csv(var, paste0("/GWSPH/groups/anenberggrp/sykim/data/new/uncertainty/pwo3_", i, "_uncertainty_var.csv"), row.names = TRUE)
  
  
}

