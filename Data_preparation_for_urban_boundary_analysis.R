
library(dplyr)    
library(sp) 
library(ncdf4) 
library(terra)
library(raster) 
library(sf)
library(dplyr)   
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(tmap)
library(RColorBrewer)
library(scales)
library("gridExtra") 
library(rgeos)
library(patchwork)     
library(ggpubr)





##################################################################################################
##################################################################################################
##################################################################################################
#PM2.5: corx
##################################################################################################
##################################################################################################
##################################################################################################
city.shp <- st_read("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/NO2/Inputs/All_C40_blundaries_shp/Abidjan.shp")
cities <- c('Accra','Addis Ababa','Amman','Amsterdam','Athens','Auckland','Austin',
            'Bangkok','Barcelona','Beijing','Bengaluru','Berlin','Bogota','Boston','Buenos Aires',
            'Cape Town','Chengdu','Chennai','Chicago','Copenhagen','Curitiba','Dakar','Dalian','Dar es Salaam',  
            'Delhi NCT','Dhaka','Dubai','Durban (eThekwini)','Ekurhuleni','Freetown','Fuzhou','Guadalajara','Guangzhou', 
            'Hangzhou','Hanoi','Heidelberg','Ho Chi Minh City','Hong Kong','Houston','Istanbul','Jakarta','Johannesburg', 
            'Karachi','Kolkata','Kuala Lumpur','Lagos','Lima','Lisbon','London','Los Angeles','Madrid','Medellin',
            'Melbourne','Mexico City','Miami','Milan','Montreal','Moscow','Mumbai','Nairobi','Nanjing',
            'New Orleans','New York City','Oslo','Paris','Philadelphia','Phoenix','Portland','Qingdao','Quezon City','Quito',
            'Rio de Janeiro','Rome','Rotterdam','Salvador','San Francisco' ,  'Santiago' ,  'São Paulo',
            'Seattle','Seoul','Shanghai','Shenzhen','Singapore','Stockholm','Sydney','Tel Aviv-Yafo',
            'Tokyo','Toronto','Tshwane','Vancouver','Venice','Warsaw','Washington, DC','Wuhan','Yokohama','Zhenjiang')
for (i in cities) {
  a <- st_read(paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/NO2/Inputs/All_C40_blundaries_shp/", i, ".shp"))
  city.shp <- rbind(city.shp, a)
}
city1 = raster::as.data.frame(city.shp, xy=TRUE)
city1$ID <- seq.int(nrow(city1)) 
#seed for merging all period data
c40_pm25 <-read.csv(paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Final_estimates/PM25/pwpm_2000_c40_24jul1.csv"), header=TRUE)
c40 <- merge(city1, c40_pm25, by='ID')
names(c40)[7] <- "pwpm_c40"
names(c40)[3] <- "geo_c40"
keeps <- c("city","pwpm_c40", "geo_c40", "ID")
c40 = c40[keeps]
c40 <-rename(c40, id_c40="ID")
c40$year <- 2000
# i loop
years <- c(2001:2020)
for (i in years)
{
  c40_pm25 <-read.csv(paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Final_estimates/PM25/pwpm_", i, "_c40_24jul1.csv"), header=TRUE)
  c401 <- merge(city1, c40_pm25, by='ID')
  names(c401)[7] <- "pwpm_c40"
  names(c401)[3] <- "geo_c40"
  keeps <- c("city","pwpm_c40", "geo_c40", "ID")
  c401 = c401[keeps]
  c401 <-rename(c401, id_c40="ID")
  c401$year <- i
  c40 <- rbind(c40, c401)
}
#unifying city names
c40$city[c40$city == 'Bogotá'] <- 'Bogota'
c40$city[c40$city == 'Delhi NCT'] <- 'Delhi [New Delhi]'
c40$city[c40$city == 'Durban (eThekwini)'] <- 'Durban'
c40$city[c40$city == 'Montréal'] <- 'Montreal'
c40$city[c40$city == 'New York City'] <- 'New York'
c40$city[c40$city == 'Quezon City'] <- 'Quezon City [Manila]'
c40$city[c40$city == 'Rotterdam'] <- 'Rotterdam [The Hague]'
c40$city[c40$city == 'Tel Aviv-Yafo'] <- 'Tel Aviv'
c40$city[c40$city == 'Washington, DC'] <- 'Washington D.C.'

#seed for merging 13k data for all period
k13_pm25 <-read.csv(paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Final_estimates/PM25/pwpm_2000_13k_24jul1.csv"), header=TRUE)
city.shp <- st_read("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/NO2/Inputs/hdc_naming_warp.shp")
city = raster::as.data.frame(city.shp, xy=TRUE)
city$ID <- city$ID_HDC_G0
k13 <- merge(city, k13_pm25, by='ID')
names(k13)[3] <- "city"
names(k13)[7] <- "geo_13k"
names(k13)[11] <- "pwpm_13k"
keeps <- c("city","geo_13k", "pwpm_13k", "ID")
k13 = k13[keeps]
k13$year <- 2000
#i loop
for (i in years) {
  k13_pm25 <-read.csv(paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Final_estimates/PM25/pwpm_", i, "_13k_24jul1.csv"), header=TRUE)
  k131 <- merge(city, k13_pm25, by='ID')
  names(k131)[3] <- "city"
  names(k131)[7] <- "geo_13k"
  names(k131)[11] <- "pwpm_13k"
  keeps <- c("city","geo_13k", "pwpm_13k", "ID")
  k131 = k131[keeps]
  k131$year <- i
  k13 <- rbind(k13, k131)
}
k13 <-subset(k13, (ID != 909 & ID!=1734 & ID!=839 & ID!=12885 & ID !=8893 & ID !=1050))
k13 <-rename(k13, id_13k="ID")

cor <- merge(k13, c40, by=c('city', 'year'))
cor1 <-subset(cor, (city != "Nanjing" & city != "Fuzhou"))

#Nanjing & Fuzhou (cities having two points in 13k data)
#13k boundary (joined)
x <-subset(cor, (city == "Nanjing" | city == "Fuzhou"))
xx <- x %>% group_by(city, year) %>%
  summarise(pwpm_13k=mean(pwpm_13k), pwpm_c40=mean(pwpm_c40), id_13k=sum(id_13k), id_c40=mean(id_c40))

y <- x[c(1),3] #Fuzhou
yy <- x[c(2),3]
yyy <- st_union(y,yy)
yyyy <- st_as_sf(yyy)
yyyy$city <- "Fuzhou"
z <- x[c(43),3] #Nanjing
zz <- x[c(44),3]
zzz <- st_union(z, zz)
zzzz <- st_as_sf(zzz)
zzzz$city <- "Nanjing"
yz<-rbind(yyyy,zzzz)

cor2 <- merge(xx, yz, by="city")
names(cor2)[7] <- "geo_13k"
#C40 boundary
a <- subset(cor, ((city=="Nanjing" | city=='Fuzhou') & year==2000))
a<-a[c(2,3), c(1,7)]
cor2 <- merge(cor2, a, by='city')


#final dataset
cor3 <- rbind(cor2, cor1)
cor3 <- subset(cor3, city != "San Francisco")
st_crs(cor3$geo_c40) <- st_crs("+proj=longlat +datum=WGS84")


# stratification
#population
#13k
pop_13k <- read.csv("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Final_estimates/PM25/pwpm_2019_13k_24jul1.csv", header=TRUE)
pop_13k <- pop_13k[c("ID", "pop_sum")]
pop_13k <- rename(pop_13k, pop_13k="pop_sum")
pop_13k <- subset(pop_13k, (ID != 909 & ID!=1734 & ID!=839 & ID!=12885 & ID !=8893 & ID !=1050))
#remove wrong cities
pop_13k <- merge(pop_13k, city, by="ID")
pop_13k <- pop_13k[c("pop_13k", "NAME_MAIN")]
pop_13k <- rename(pop_13k, city="NAME_MAIN")
pop1<-subset(pop_13k, city%in%c("Fuzhou", "Nanjing"))
pop11<-pop1 %>% group_by(city) %>%
  summarise(pop_13k=sum(pop_13k))

pop111<-subset(pop_13k, !(city %in% c("Fuzhou", "Nanjing")))
pop2 <- rbind(pop11, pop111)

corx <- merge(cor3, pop2, by='city')
#C40
pop_c40 <- read.csv("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Final_estimates/PM25/pwpm_2019_c40_24jul1.csv", header=TRUE)
pop_c40 <- pop_c40[c("ID", "pop_sum")]
pop_c40 <- rename(pop_c40, pop_c40="pop_sum")
pop_c40 <- merge(pop_c40, city1, by="ID")
pop_c40 <- pop_c40[c("ID", "pop_c40")]
pop_c40 <- rename(pop_c40, id_c40="ID")

corx <- merge(corx, pop_c40, by='id_c40')
corx$pop_dif <- abs(corx$pop_13k-corx$pop_c40)

#total area
sf_use_s2(FALSE)
corx$area_c40 <- st_area(corx$geo_c40)
corx$area_13k <- st_area(corx$geo_13k)
corx$area_dif <- abs(corx$area_c40-corx$area_13k)
corx$area_dif_num <- as.numeric(corx$area_dif)
corx$area_c40_num <- as.numeric(corx$area_c40)
corx$area_13k_num <- as.numeric(corx$area_13k)

corx$pop_den <- corx$pop_13k/corx$area_13k_num

#region
pre <-read.csv("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/NO2/Data_pre.csv", header=TRUE)
keeps <- c("ID","GBDSuperRegion", "Country")
pre_ = pre[keeps]
pre_ <- unique(pre_)
pre_ <- rename(pre_, id_13k="ID")
pre_$id_13k[pre_$id_13k==12221 | pre_$id_13k==12620] <- 24841 #Fuzhou
pre_$id_13k[pre_$id_13k==12547 | pre_$id_13k==12131] <- 24678 #Nanjing
pre_ <- unique(pre_)
corx <- merge(corx, pre_, by='id_13k')

#country GDP per capita
gdp <-read.csv("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/PM2.5/Inputs/gdp.csv", header=TRUE)
gdp<-gdp[c("Country.Name", "X2021")]
gdp<-rename(gdp, Country='Country.Name')
gdp<-rename(gdp, gdp='X2021')
gdp$gdp_cat <- ifelse(gdp$gdp<=1045, "Low income", 0) #world bank category
gdp$gdp_cat <- ifelse(gdp$gdp<=4095 & gdp$gdp>1045, "Lower-middle income", gdp$gdp_cat)
gdp$gdp_cat <- ifelse(gdp$gdp<=12695 & gdp$gdp>4095, "Upper-middle income", gdp$gdp_cat)
gdp$gdp_cat <- ifelse(gdp$gdp>12695, "High income", gdp$gdp_cat)
corx <- merge(corx, gdp, by='Country', all.x=TRUE)

#Moran's I
test <- subset(corx, corx$year==2019)
test <- test[c("id_13k", "geo_13k")]
pm <- raster(paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/PM2.5/Inputs/Annual/V5GL03.HybridPM25c_0p10.Global.201901-201912.nc"))

id_13k <- test$id_13k
moran <- rep(1,92)
for (i in 1:92) {
  a <- st_as_sf(test$geo_13k[i])
  raster_extent <- extent(a)
  pm_cropped <- crop(pm, raster_extent)
  moran[i] <- Moran(pm_cropped)
}
moran <- cbind(id_13k, moran)
corx <- merge(corx, moran, by="id_13k")

#bivariate regression 
corx$dif <- corx$pwpm_13k-corx$pwpm_c40
corx$abs_dif <- abs(corx$dif)
corx$pct_chg <- corx$abs_dif/corx$pwpm_c40
corx$ratio <- corx$pwpm_13k/corx$pwpm_c40
corx$year_cat <- factor(corx$year)
corx$region_cat <- factor(corx$GBDSuperRegion)

corx1 <-subset(corx, (year == 2019))

hist(corx1$abs_dif)
hist(log(corx1$abs_dif))
#hist(corx$area_c40_num)
hist(corx1$area_13k_num) #area
hist(log(corx1$area_13k_num))
hist(corx1$area_dif)
hist(log(corx1$area_dif))
hist(corx1$pop_13k) #pop
hist(log(corx1$pop_13k))
hist(corx1$pop_dif)
hist(log(corx1$pop_dif))
hist(corx1$pop_den)
hist(log(corx1$pop_den))
#hist(corx$mnt)
#hist(log(corx$mnt_der))
#hist(corx$gdp)
hist(corx1$moran)

corx1$log_abs_dif <- log(corx1$abs_dif)
corx1$log_area <- log(corx1$area_13k_num)
corx1$log_area_dif <- log(corx1$area_dif_num)
corx1$log_pop <- log(corx1$pop_13k)
corx1$log_pop_dif <- log(corx1$pop_dif)
corx1$log_pop_den <- log(corx1$pop_den)
#corx1$log_mnt <- log(corx1$mnt_der)


saveRDS(corx, file="/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/etc/corx.rds")
saveRDS(corx1, file="/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/etc/corx1.rds")














##################################################################################################
##################################################################################################
##################################################################################################
#NO2: cory
##################################################################################################
##################################################################################################
##################################################################################################
city.shp <- st_read("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/NO2/Inputs/All_C40_blundaries_shp/Abidjan.shp")
cities <- c('Accra','Addis Ababa','Amman','Amsterdam','Athens','Auckland','Austin',
            'Bangkok','Barcelona','Beijing','Bengaluru','Berlin','Bogota','Boston','Buenos Aires',
            'Cape Town','Chengdu','Chennai','Chicago','Copenhagen','Curitiba','Dakar','Dalian','Dar es Salaam',  
            'Delhi NCT','Dhaka','Dubai','Durban (eThekwini)','Ekurhuleni','Freetown','Fuzhou','Guadalajara','Guangzhou', 
            'Hangzhou','Hanoi','Heidelberg','Ho Chi Minh City','Hong Kong','Houston','Istanbul','Jakarta','Johannesburg', 
            'Karachi','Kolkata','Kuala Lumpur','Lagos','Lima','Lisbon','London','Los Angeles','Madrid','Medellin',
            'Melbourne','Mexico City','Miami','Milan','Montreal','Moscow','Mumbai','Nairobi','Nanjing',
            'New Orleans','New York City','Oslo','Paris','Philadelphia','Phoenix','Portland','Qingdao','Quezon City','Quito',
            'Rio de Janeiro','Rome','Rotterdam','Salvador','San Francisco' ,  'Santiago' ,  'São Paulo',
            'Seattle','Seoul','Shanghai','Shenzhen','Singapore','Stockholm','Sydney','Tel Aviv-Yafo',
            'Tokyo','Toronto','Tshwane','Vancouver','Venice','Warsaw','Washington, DC','Wuhan','Yokohama','Zhenjiang')
for (i in cities) {
  a <- st_read(paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/NO2/Inputs/All_C40_blundaries_shp/", i, ".shp"))
  city.shp <- rbind(city.shp, a)
}
city1 = raster::as.data.frame(city.shp, xy=TRUE)
city1$ID <- seq.int(nrow(city1)) 
#seed for merging all period data
c40_no2 <-read.csv(paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Final_estimates/NO2/pwno2_2005_c40_24jul12.csv"), header=TRUE)
c40 <- merge(city1, c40_no2, by='ID')
names(c40)[7] <- "pwno2_c40"
names(c40)[3] <- "geo_c40"
keeps <- c("city","pwno2_c40", "geo_c40", "ID")
c40 = c40[keeps]
c40 <-rename(c40, id_c40="ID")
c40$year <- 2005
# i loop
years <- c(2006:2020)
for (i in years)
{
  c40_no2 <-read.csv(paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Final_estimates/NO2/pwno2_", i, "_c40_24jul12.csv"), header=TRUE)
  c401 <- merge(city1, c40_no2, by='ID')
  names(c401)[7] <- "pwno2_c40"
  names(c401)[3] <- "geo_c40"
  keeps <- c("city","pwno2_c40", "geo_c40", "ID")
  c401 = c401[keeps]
  c401 <-rename(c401, id_c40="ID")
  c401$year <- i
  c40 <- rbind(c40, c401)
}
#unifying city names
c40$city[c40$city == 'Bogotá'] <- 'Bogota'
c40$city[c40$city == 'Delhi NCT'] <- 'Delhi [New Delhi]'
c40$city[c40$city == 'Durban (eThekwini)'] <- 'Durban'
c40$city[c40$city == 'Montréal'] <- 'Montreal'
c40$city[c40$city == 'New York City'] <- 'New York'
c40$city[c40$city == 'Quezon City'] <- 'Quezon City [Manila]'
c40$city[c40$city == 'Rotterdam'] <- 'Rotterdam [The Hague]'
c40$city[c40$city == 'Tel Aviv-Yafo'] <- 'Tel Aviv'
c40$city[c40$city == 'Washington, DC'] <- 'Washington D.C.'

#seed for merging 13k data for all period
k13_no2 <-read.csv(paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Final_estimates/NO2/pwno2_2005_13k_24jul12.csv"), header=TRUE)
city.shp <- st_read("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/NO2/Inputs/hdc_naming_warp.shp")
city = raster::as.data.frame(city.shp, xy=TRUE)
city$ID <- city$ID_HDC_G0
k13 <- merge(city, k13_no2, by='ID')
names(k13)[3] <- "city"
names(k13)[7] <- "geo_13k"
names(k13)[11] <- "pwno2_13k"
keeps <- c("city","geo_13k", "pwno2_13k", "ID")
k13 = k13[keeps]
k13$year <- 2005
#i loop
for (i in years) {
  k13_no2 <-read.csv(paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Final_estimates/NO2/pwno2_", i, "_13k_24jul12.csv"), header=TRUE)
  k131 <- merge(city, k13_no2, by='ID')
  names(k131)[3] <- "city"
  names(k131)[7] <- "geo_13k"
  names(k131)[11] <- "pwno2_13k"
  keeps <- c("city","geo_13k", "pwno2_13k", "ID")
  k131 = k131[keeps]
  k131$year <- i
  k13 <- rbind(k13, k131)
}
k13 <-subset(k13, (ID != 909 & ID!=1734 & ID!=839 & ID!=12885 & ID !=8893 & ID !=1050))
k13 <-rename(k13, id_13k="ID")

cor <- merge(k13, c40, by=c('city', 'year'))
cor1 <-subset(cor, (city != "Nanjing" & city != "Fuzhou"))

#Nanjing & Fuzhou (cities having two points in 13k data)
#13k boundary (joined)
x <-subset(cor, (city == "Nanjing" | city == "Fuzhou"))
xx <- x %>% group_by(city, year) %>%
  summarise(pwno2_13k=mean(pwno2_13k), pwno2_c40=mean(pwno2_c40), id_13k=sum(id_13k), id_c40=mean(id_c40))

y <- x[c(1),3] #Fuzhou
yy <- x[c(2),3]
yyy <- st_union(y,yy)
yyyy <- st_as_sf(yyy)
yyyy$city <- "Fuzhou"
z <- x[c(33),3] #Nanjing
zz <- x[c(34),3]
zzz <- st_union(z, zz)
zzzz <- st_as_sf(zzz)
zzzz$city <- "Nanjing"
yz<-rbind(yyyy,zzzz)

cor2 <- merge(xx, yz, by="city")
names(cor2)[7] <- "geo_13k"
#C40 boundary
a <- subset(cor, ((city=="Nanjing" | city=='Fuzhou') & year==2005))
a<-a[c(2,3), c(1,7)]
cor2 <- merge(cor2, a, by='city')

#final dataset
cor3 <- rbind(cor2, cor1)
cor3 <- subset(cor3, city != "San Francisco")
st_crs(cor3$geo_c40) <- st_crs("+proj=longlat +datum=WGS84")



# stratification
#population
#13k
pop_13k <- read.csv("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Final_estimates/NO2/pwno2_2019_13k_24jul12.csv", header=TRUE)
pop_13k <- pop_13k[c("ID", "pop_sum")]
pop_13k <- rename(pop_13k, pop_13k="pop_sum")
pop_13k <- subset(pop_13k, (ID != 909 & ID!=1734 & ID!=839 & ID!=12885 & ID !=8893 & ID !=1050))
#remove wrong cities
pop_13k <- merge(pop_13k, city, by="ID")
pop_13k <- pop_13k[c("pop_13k", "NAME_MAIN")]
pop_13k <- rename(pop_13k, city="NAME_MAIN")
pop1<-subset(pop_13k, city%in%c("Fuzhou", "Nanjing"))
pop11<-pop1 %>% group_by(city) %>%
  summarise(pop_13k=sum(pop_13k))

pop111<-subset(pop_13k, !(city %in% c("Fuzhou", "Nanjing")))
pop2 <- rbind(pop11, pop111)

cory <- merge(cor3, pop2, by='city')

#C40
pop_c40 <- read.csv("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Final_estimates/NO2/pwno2_2019_c40_24jul12.csv", header=TRUE)
pop_c40 <- pop_c40[c("ID", "pop_sum")]
pop_c40 <- rename(pop_c40, pop_c40="pop_sum")
pop_c40 <- merge(pop_c40, city1, by="ID")
pop_c40 <- pop_c40[c("ID", "pop_c40")]
pop_c40 <- rename(pop_c40, id_c40="ID")
cory <- merge(cory, pop_c40, by='id_c40')

cory$pop_dif <- abs(cory$pop_13k-cory$pop_c40)

#total area
sf_use_s2(FALSE)
cory$area_c40 <- st_area(cory$geo_c40)
cory$area_13k <- st_area(cory$geo_13k)
cory$area_dif <- abs(cory$area_c40-cory$area_13k)
cory$area_dif_num <- as.numeric(cory$area_dif)
cory$area_c40_num <- as.numeric(cory$area_c40)
cory$area_13k_num <- as.numeric(cory$area_13k)

cory$pop_den <- cory$pop_13k/cory$area_13k_num


#region
pre <-read.csv("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/NO2/Data_pre.csv", header=TRUE)
keeps <- c("ID","GBDSuperRegion", "Country")
pre_ = pre[keeps]
pre_ <- unique(pre_)
pre_ <- rename(pre_, id_13k="ID")
pre_$id_13k[pre_$id_13k==12221 | pre_$id_13k==12620] <- 24841 #Fuzhou
pre_$id_13k[pre_$id_13k==12547 | pre_$id_13k==12131] <- 24678 #Nanjing
pre_ <- unique(pre_)
cory <- merge(cory, pre_, by='id_13k')

#country GDP per capita
gdp <-read.csv("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/PM2.5/Inputs/gdp.csv", header=TRUE)
gdp<-gdp[c("Country.Name", "X2021")]
gdp<-rename(gdp, Country='Country.Name')
gdp<-rename(gdp, gdp='X2021')
gdp$gdp_cat <- ifelse(gdp$gdp<=1045, "Low income", 0) #world bank category
gdp$gdp_cat <- ifelse(gdp$gdp<=4095 & gdp$gdp>1045, "Lower-middle income", gdp$gdp_cat)
gdp$gdp_cat <- ifelse(gdp$gdp<=12695 & gdp$gdp>4095, "Upper-middle income", gdp$gdp_cat)
gdp$gdp_cat <- ifelse(gdp$gdp>12695, "High income", gdp$gdp_cat)
cory <- merge(cory, gdp, by='Country', all.x=TRUE)

#Moran's I
test <- subset(cory, cory$year==2019)
test <- test[c("id_13k", "geo_13k")]
pm <- raster(paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/NO2/Inputs/final/GlobalNO2LUR_2019.tif"))

id_13k <- test$id_13k
moran <- rep(1,92)
for (i in 1:92) {
  a <- st_as_sf(test$geo_13k[i])
  raster_extent <- extent(a)
  pm_cropped <- crop(pm, raster_extent)
  moran[i] <- Moran(pm_cropped)
}
moran <- cbind(id_13k, moran)
cory <- merge(cory, moran, by="id_13k")

#bivariate regression 
cory$dif <- cory$pwno2_13k-cory$pwno2_c40
cory$abs_dif <- abs(cory$dif)
cory$pct_chg <- cory$abs_dif/cory$pwno2_c40
cory$ratio <- cory$pwno2_13k/cory$pwno2_c40
cory$year_cat <- factor(cory$year)
cory$region_cat <- factor(cory$GBDSuperRegion)

cory1 <-subset(cory, (year == 2019))

hist(cory1$abs_dif)
hist(log(cory1$abs_dif))
#hist(cory$area_c40_num)
hist(cory1$area_13k_num) #area
hist(log(cory1$area_13k_num))
hist(cory1$area_dif)
hist(log(cory1$area_dif))
hist(cory1$pop_13k) #pop
hist(log(cory1$pop_13k))
hist(cory1$pop_dif)
hist(log(cory1$pop_dif))
hist(cory1$pop_den)
hist(log(cory1$pop_den))
#hist(cory$mnt)
#hist(log(cory$mnt_der))
#hist(cory$gdp)
hist(cory1$moran)

cory1$log_abs_dif <- log(cory1$abs_dif)
cory1$log_area <- log(cory1$area_13k_num)
cory1$log_area_dif <- log(cory1$area_dif_num)
cory1$log_pop <- log(cory1$pop_13k)
cory1$log_pop_dif <- log(cory1$pop_dif)
cory1$log_pop_den <- log(cory1$pop_den)
#cory1$log_mnt <- log(cory1$mnt_der)




saveRDS(cory, file="/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/etc/cory.rds")
saveRDS(cory1, file="/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/etc/cory1.rds")















##################################################################################################
##################################################################################################
##################################################################################################
#O3: corz
##################################################################################################
##################################################################################################
##################################################################################################
city.shp <- st_read("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/NO2/Inputs/All_C40_blundaries_shp/Abidjan.shp")
cities <- c('Accra','Addis Ababa','Amman','Amsterdam','Athens','Auckland','Austin',
            'Bangkok','Barcelona','Beijing','Bengaluru','Berlin','Bogota','Boston','Buenos Aires',
            'Cape Town','Chengdu','Chennai','Chicago','Copenhagen','Curitiba','Dakar','Dalian','Dar es Salaam',  
            'Delhi NCT','Dhaka','Dubai','Durban (eThekwini)','Ekurhuleni','Freetown','Fuzhou','Guadalajara','Guangzhou', 
            'Hangzhou','Hanoi','Heidelberg','Ho Chi Minh City','Hong Kong','Houston','Istanbul','Jakarta','Johannesburg', 
            'Karachi','Kolkata','Kuala Lumpur','Lagos','Lima','Lisbon','London','Los Angeles','Madrid','Medellin',
            'Melbourne','Mexico City','Miami','Milan','Montreal','Moscow','Mumbai','Nairobi','Nanjing',
            'New Orleans','New York City','Oslo','Paris','Philadelphia','Phoenix','Portland','Qingdao','Quezon City','Quito',
            'Rio de Janeiro','Rome','Rotterdam','Salvador','San Francisco' ,  'Santiago' ,  'São Paulo',
            'Seattle','Seoul','Shanghai','Shenzhen','Singapore','Stockholm','Sydney','Tel Aviv-Yafo',
            'Tokyo','Toronto','Tshwane','Vancouver','Venice','Warsaw','Washington, DC','Wuhan','Yokohama','Zhenjiang')
for (i in cities) {
  a <- st_read(paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/NO2/Inputs/All_C40_blundaries_shp/", i, ".shp"))
  city.shp <- rbind(city.shp, a)
}
city1 = raster::as.data.frame(city.shp, xy=TRUE)
city1$ID <- seq.int(nrow(city1)) 
#seed for merging all period data
c40_o3 <-read.csv(paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Final_estimates/O3/pwo3_2000_c40_24jul1.csv"), header=TRUE)
c40 <- merge(city1, c40_o3, by='ID')
names(c40)[7] <- "pwo3_c40"
names(c40)[3] <- "geo_c40"
keeps <- c("city","pwo3_c40", "geo_c40", "ID")
c40 = c40[keeps]
c40 <-rename(c40, id_c40="ID")
c40$year <- 2000
# i loop
years <- c(2001:2020)
for (i in years)
{
  c40_o3 <-read.csv(paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Final_estimates/O3/pwo3_", i, "_c40_24jul1.csv"), header=TRUE)
  c401 <- merge(city1, c40_o3, by='ID')
  names(c401)[7] <- "pwo3_c40"
  names(c401)[3] <- "geo_c40"
  keeps <- c("city","pwo3_c40", "geo_c40", "ID")
  c401 = c401[keeps]
  c401 <-rename(c401, id_c40="ID")
  c401$year <- i
  c40 <- rbind(c40, c401)
}
#unifying city names
c40$city[c40$city == 'Bogotá'] <- 'Bogota'
c40$city[c40$city == 'Delhi NCT'] <- 'Delhi [New Delhi]'
c40$city[c40$city == 'Durban (eThekwini)'] <- 'Durban'
c40$city[c40$city == 'Montréal'] <- 'Montreal'
c40$city[c40$city == 'New York City'] <- 'New York'
c40$city[c40$city == 'Quezon City'] <- 'Quezon City [Manila]'
c40$city[c40$city == 'Rotterdam'] <- 'Rotterdam [The Hague]'
c40$city[c40$city == 'Tel Aviv-Yafo'] <- 'Tel Aviv'
c40$city[c40$city == 'Washington, DC'] <- 'Washington D.C.'

#seed for merging 13k data for all period
k13_o3 <-read.csv(paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Final_estimates/O3/pwo3_2000_13k_24jul1.csv"), header=TRUE)
city.shp <- st_read("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/NO2/Inputs/hdc_naming_warp.shp")
city = raster::as.data.frame(city.shp, xy=TRUE)
city$ID <- city$ID_HDC_G0
k13 <- merge(city, k13_o3, by='ID')
names(k13)[3] <- "city"
names(k13)[7] <- "geo_13k"
names(k13)[11] <- "pwo3_13k"
keeps <- c("city","geo_13k", "pwo3_13k", "ID")
k13 = k13[keeps]
k13$year <- 2000
#i loop
for (i in years) {
  k13_o3 <-read.csv(paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Final_estimates/O3/pwo3_", i, "_13k_24jul1.csv"), header=TRUE)
  k131 <- merge(city, k13_o3, by='ID')
  names(k131)[3] <- "city"
  names(k131)[7] <- "geo_13k"
  names(k131)[11] <- "pwo3_13k"
  keeps <- c("city","geo_13k", "pwo3_13k", "ID")
  k131 = k131[keeps]
  k131$year <- i
  k13 <- rbind(k13, k131)
}
k13 <-subset(k13, (ID != 909 & ID!=1734 & ID!=839 & ID!=12885 & ID !=8893 & ID !=1050))
k13 <-rename(k13, id_13k="ID")

cor <- merge(k13, c40, by=c('city', 'year'))
cor1 <-subset(cor, (city != "Nanjing" & city != "Fuzhou"))

#Nanjing & Fuzhou (cities having two points in 13k data)
#13k boundary (joined)
x <-subset(cor, (city == "Nanjing" | city == "Fuzhou"))
xx <- x %>% group_by(city, year) %>%
  summarise(pwo3_13k=mean(pwo3_13k), pwo3_c40=mean(pwo3_c40), id_13k=sum(id_13k), id_c40=mean(id_c40))

y <- x[c(1),3] #Fuzhou
yy <- x[c(2),3]
yyy <- st_union(y,yy)
yyyy <- st_as_sf(yyy)
yyyy$city <- "Fuzhou"
z <- x[c(43),3] #Nanjing
zz <- x[c(44),3]
zzz <- st_union(z, zz)
zzzz <- st_as_sf(zzz)
zzzz$city <- "Nanjing"
yz<-rbind(yyyy,zzzz)

cor2 <- merge(xx, yz, by="city")
names(cor2)[7] <- "geo_13k"
#C40 boundary
a <- subset(cor, ((city=="Nanjing" | city=='Fuzhou') & year==2000))
a<-a[c(2,3), c(1,7)]
cor2 <- merge(cor2, a, by='city')

#final dataset
cor3 <- rbind(cor2, cor1)
cor3 <- subset(cor3, city != "San Francisco")
st_crs(cor3$geo_c40) <- st_crs("+proj=longlat +datum=WGS84")



# stratification
#population
#13k
pop_13k <- read.csv("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Final_estimates/O3/pwo3_2019_13k_24jul1.csv", header=TRUE)
pop_13k <- pop_13k[c("ID", "pop_sum")]
pop_13k <- rename(pop_13k, pop_13k="pop_sum")
pop_13k <- subset(pop_13k, (ID != 909 & ID!=1734 & ID!=839 & ID!=12885 & ID !=8893 & ID !=1050))
#remove wrong cities
pop_13k <- merge(pop_13k, city, by="ID")
pop_13k <- pop_13k[c("pop_13k", "NAME_MAIN")]
pop_13k <- rename(pop_13k, city="NAME_MAIN")
pop1<-subset(pop_13k, city%in%c("Fuzhou", "Nanjing"))
pop11<-pop1 %>% group_by(city) %>%
  summarise(pop_13k=sum(pop_13k))

pop111<-subset(pop_13k, !(city %in% c("Fuzhou", "Nanjing")))
pop2 <- rbind(pop11, pop111)

corz <- merge(cor3, pop2, by='city')

#C40
pop_c40 <- read.csv("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Final_estimates/O3/pwo3_2019_c40_24jul1.csv", header=TRUE)
pop_c40 <- pop_c40[c("ID", "pop_sum")]
pop_c40 <- rename(pop_c40, pop_c40="pop_sum")
pop_c40 <- merge(pop_c40, city1, by="ID")
pop_c40 <- pop_c40[c("ID", "pop_c40")]
pop_c40 <- rename(pop_c40, id_c40="ID")
corz <- merge(corz, pop_c40, by='id_c40')
corz$pop_dif <- abs(corz$pop_13k-corz$pop_c40)


#total area
sf_use_s2(FALSE)
corz$area_c40 <- st_area(corz$geo_c40)
corz$area_13k <- st_area(corz$geo_13k)
corz$area_dif <- abs(corz$area_c40-corz$area_13k)
corz$area_dif_num <- as.numeric(corz$area_dif)
corz$area_c40_num <- as.numeric(corz$area_c40)
corz$area_13k_num <- as.numeric(corz$area_13k)

corz$pop_den <- corz$pop_13k/corz$area_13k_num

#region
pre <-read.csv("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/NO2/Data_pre.csv", header=TRUE)
keeps <- c("ID","GBDSuperRegion", "Country")
pre_ = pre[keeps]
pre_ <- unique(pre_)
pre_ <- rename(pre_, id_13k="ID")
pre_$id_13k[pre_$id_13k==12221 | pre_$id_13k==12620] <- 24841 #Fuzhou
pre_$id_13k[pre_$id_13k==12547 | pre_$id_13k==12131] <- 24678 #Nanjing
pre_ <- unique(pre_)
corz <- merge(corz, pre_, by='id_13k')

#country GDP per capita
gdp <-read.csv("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/PM2.5/Inputs/gdp.csv", header=TRUE)
gdp<-gdp[c("Country.Name", "X2021")]
gdp<-rename(gdp, Country='Country.Name')
gdp<-rename(gdp, gdp='X2021')
gdp$gdp_cat <- ifelse(gdp$gdp<=1045, "Low income", 0) #world bank category
gdp$gdp_cat <- ifelse(gdp$gdp<=4095 & gdp$gdp>1045, "Lower-middle income", gdp$gdp_cat)
gdp$gdp_cat <- ifelse(gdp$gdp<=12695 & gdp$gdp>4095, "Upper-middle income", gdp$gdp_cat)
gdp$gdp_cat <- ifelse(gdp$gdp>12695, "High income", gdp$gdp_cat)
corz <- merge(corz, gdp, by='Country', all.x=TRUE)


#Moran's I
test <- subset(corz, corz$year==2019)
test <- test[c("id_13k", "geo_13k")]
pm <- raster(paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/O3/Inputs/all_grids_2019.tif"))

id_13k <- test$id_13k
moran <- rep(1,92)
for (i in 1:92) {
  a <- st_as_sf(test$geo_13k[i])
  raster_extent <- extent(a)
  pm_cropped <- crop(pm, raster_extent)
  moran[i] <- Moran(pm_cropped)
}
moran <- cbind(id_13k, moran)
corz <- merge(corz, moran, by="id_13k")

#bivariate regression 
corz$dif <- corz$pwo3_13k-corz$pwo3_c40
corz$abs_dif <- abs(corz$dif)
corz$pct_chg <- corz$abs_dif/corz$pwo3_c40
corz$ratio <- corz$pwo3_13k/corz$pwo3_c40
corz$year_cat <- factor(corz$year)
corz$region_cat <- factor(corz$GBDSuperRegion)

corz1 <-subset(corz, (year == 2019))

hist(corz1$abs_dif)
hist(log(corz1$abs_dif))
#hist(corz$area_c40_num)
hist(corz1$area_13k_num) #area
hist(log(corz1$area_13k_num))
hist(corz1$area_dif)
hist(log(corz1$area_dif))
hist(corz1$pop_13k) #pop
hist(log(corz1$pop_13k))
hist(corz1$pop_dif)
hist(log(corz1$pop_dif))
hist(corz1$pop_den)
hist(log(corz1$pop_den))
#hist(corz$mnt)
#hist(log(corz$mnt_der))
#hist(corz$gdp)
hist(corz1$moran)

corz1$log_abs_dif <- log(corz1$abs_dif)
corz1$log_area <- log(corz1$area_13k_num)
corz1$log_area_dif <- log(corz1$area_dif_num)
corz1$log_pop <- log(corz1$pop_13k)
corz1$log_pop_dif <- log(corz1$pop_dif)
corz1$log_pop_den <- log(corz1$pop_den)
#corz1$log_mnt <- log(corz1$mnt_der)




saveRDS(corz, file="/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/etc/corz.rds")
saveRDS(corz1, file="/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/etc/corz1.rds")










##################################################################################################
##################################################################################################
##################################################################################################
#CO2: corw
##################################################################################################
##################################################################################################
##################################################################################################
city.shp <- st_read("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/NO2/Inputs/All_C40_blundaries_shp/Abidjan.shp")
cities <- c('Accra','Addis Ababa','Amman','Amsterdam','Athens','Auckland','Austin',
            'Bangkok','Barcelona','Beijing','Bengaluru','Berlin','Bogota','Boston','Buenos Aires',
            'Cape Town','Chengdu','Chennai','Chicago','Copenhagen','Curitiba','Dakar','Dalian','Dar es Salaam',  
            'Delhi NCT','Dhaka','Dubai','Durban (eThekwini)','Ekurhuleni','Freetown','Fuzhou','Guadalajara','Guangzhou', 
            'Hangzhou','Hanoi','Heidelberg','Ho Chi Minh City','Hong Kong','Houston','Istanbul','Jakarta','Johannesburg', 
            'Karachi','Kolkata','Kuala Lumpur','Lagos','Lima','Lisbon','London','Los Angeles','Madrid','Medellin',
            'Melbourne','Mexico City','Miami','Milan','Montreal','Moscow','Mumbai','Nairobi','Nanjing',
            'New Orleans','New York City','Oslo','Paris','Philadelphia','Phoenix','Portland','Qingdao','Quezon City','Quito',
            'Rio de Janeiro','Rome','Rotterdam','Salvador','San Francisco' ,  'Santiago' ,  'São Paulo',
            'Seattle','Seoul','Shanghai','Shenzhen','Singapore','Stockholm','Sydney','Tel Aviv-Yafo',
            'Tokyo','Toronto','Tshwane','Vancouver','Venice','Warsaw','Washington, DC','Wuhan','Yokohama','Zhenjiang')
for (i in cities) {
  a <- st_read(paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/NO2/Inputs/All_C40_blundaries_shp/", i, ".shp"))
  city.shp <- rbind(city.shp, a)
}
city1 = raster::as.data.frame(city.shp, xy=TRUE)
city1$ID <- seq.int(nrow(city1)) 
#seed for merging all period data
c40_co2 <-read.csv(paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Final_estimates/CO2/co2_per_capita_c40_2000.csv"), header=TRUE)
c40 <- merge(city1, c40_co2, by='ID')
names(c40)[3] <- "geo_c40"
c40 <- rename(c40, co2_per_cap_c40="co2_per_cap")
keeps <- c("city","co2_per_cap_c40", "geo_c40", "ID")
c40 = c40[keeps]
c40 <-rename(c40, id_c40="ID")
c40$year <- 2000
# i loop
years <- c(2001:2020)
for (i in years)
{
  c40_co2 <-read.csv(paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Final_estimates/CO2/co2_per_capita_c40_", i, ".csv"), header=TRUE)
  c401 <- merge(city1, c40_co2, by='ID')
  names(c401)[3] <- "geo_c40"
  c401 <- rename(c401, co2_per_cap_c40="co2_per_cap")
  keeps <- c("city","co2_per_cap_c40", "geo_c40", "ID")
  c401 = c401[keeps]
  c401 <-rename(c401, id_c40="ID")
  c401$year <- i
  c40 <- rbind(c40, c401)
}
#unifying city names
c40$city[c40$city == 'Bogotá'] <- 'Bogota'
c40$city[c40$city == 'Delhi NCT'] <- 'Delhi [New Delhi]'
c40$city[c40$city == 'Durban (eThekwini)'] <- 'Durban'
c40$city[c40$city == 'Montréal'] <- 'Montreal'
c40$city[c40$city == 'New York City'] <- 'New York'
c40$city[c40$city == 'Quezon City'] <- 'Quezon City [Manila]'
c40$city[c40$city == 'Rotterdam'] <- 'Rotterdam [The Hague]'
c40$city[c40$city == 'Tel Aviv-Yafo'] <- 'Tel Aviv'
c40$city[c40$city == 'Washington, DC'] <- 'Washington D.C.'

#seed for merging 13k data for all period
city.shp <- st_read("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/NO2/Inputs/hdc_naming_warp.shp")
city = raster::as.data.frame(city.shp, xy=TRUE)
city$ID <- city$ID_HDC_G0

k13_co2 <-read.csv(paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Final_estimates/CO2/co2_per_capita_13k_2000.csv"), header=TRUE)
k13 <- merge(city, k13_co2, by='ID')
k13 <- rename(k13, co2_per_cap_13k="co2_per_cap")
names(k13)[3] <- "city"
names(k13)[7] <- "geo_13k"
keeps <- c("city","geo_13k", "co2_per_cap_13k", "ID")
k13 = k13[keeps]
k13$year <- 2000
#i loop
for (i in years) {
  k13_co2 <-read.csv(paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Final_estimates/CO2/co2_per_capita_13k_", i, ".csv"), header=TRUE)
  k131 <- merge(city, k13_co2, by='ID')
  k131 <- rename(k131, co2_per_cap_13k="co2_per_cap")
  names(k131)[3] <- "city"
  names(k131)[7] <- "geo_13k"
  keeps <- c("city","geo_13k", "co2_per_cap_13k", "ID")
  k131 = k131[keeps]
  k131$year <- i
  k13 <- rbind(k13, k131)
}
k13 <-subset(k13, (ID != 909 & ID!=1734 & ID!=839 & ID!=12885 & ID !=8893 & ID !=1050))
k13 <-rename(k13, id_13k="ID")

cor <- merge(k13, c40, by=c('city', 'year'))
cor1 <-subset(cor, (city != "Nanjing" & city != "Fuzhou"))

#Nanjing & Fuzhou (cities having two points in 13k data)
#13k boundary (joined)
x <-subset(cor, (city == "Nanjing" | city == "Fuzhou"))
xx <- x %>% group_by(city, year) %>%
  summarise(co2_per_cap_13k=mean(co2_per_cap_13k), co2_per_cap_c40=mean(co2_per_cap_c40), id_13k=sum(id_13k), id_c40=mean(id_c40))

y <- x[c(1),3] #Fuzhou
yy <- x[c(2),3]
yyy <- st_union(y,yy)
yyyy <- st_as_sf(yyy)
yyyy$city <- "Fuzhou"
z <- x[c(43),3] #Nanjing
zz <- x[c(44),3]
zzz <- st_union(z, zz)
zzzz <- st_as_sf(zzz)
zzzz$city <- "Nanjing"
yz<-rbind(yyyy,zzzz)

cor2 <- merge(xx, yz, by="city")
names(cor2)[7] <- "geo_13k"
#C40 boundary
a <- subset(cor, ((city=="Nanjing" | city=='Fuzhou') & year==2000))
a<-a[c(2,3), c(1,7)]
cor2 <- merge(cor2, a, by='city')

#final dataset
cor3 <- rbind(cor2, cor1)
cor3 <- subset(cor3, city != "San Francisco")
st_crs(cor3$geo_c40) <- st_crs("+proj=longlat +datum=WGS84")


# stratification
#population
#13k
pop_13k <- read.csv("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Final_estimates/CO2/co2_per_capita_13k_2019.csv", header=TRUE)
pop_13k <- pop_13k[c("ID", "pop")]
pop_13k <- rename(pop_13k, pop_13k="pop")
pop_13k <- subset(pop_13k, (ID != 909 & ID!=1734 & ID!=839 & ID!=12885 & ID !=8893 & ID !=1050))
#remove wrong cities
pop_13k <- merge(pop_13k, city, by="ID")
pop_13k <- pop_13k[c("pop_13k", "NAME_MAIN")]
pop_13k <- rename(pop_13k, city="NAME_MAIN")
pop1<-subset(pop_13k, city%in%c("Fuzhou", "Nanjing"))
pop11<-pop1 %>% group_by(city) %>%
  summarise(pop_13k=sum(pop_13k))

pop111<-subset(pop_13k, !(city %in% c("Fuzhou", "Nanjing")))
pop2 <- rbind(pop11, pop111)

corw <- merge(cor3, pop2, by='city')

#C40
pop_c40 <- read.csv("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Final_estimates/CO2/co2_per_capita_c40_2019.csv", header=TRUE)
pop_c40 <- pop_c40[c("ID", "pop")]
pop_c40 <- rename(pop_c40, pop_c40="pop")
pop_c40 <- merge(pop_c40, city1, by="ID")
pop_c40 <- pop_c40[c("ID", "pop_c40")]
pop_c40 <- rename(pop_c40, id_c40="ID")
corw <- merge(corw, pop_c40, by='id_c40')
corw$pop_dif <- abs(corw$pop_13k-corw$pop_c40)


#total area
sf_use_s2(FALSE)
corw$area_c40 <- st_area(corw$geo_c40)
corw$area_13k <- st_area(corw$geo_13k)
corw$area_dif <- abs(corw$area_c40-corw$area_13k)
corw$area_dif_num <- as.numeric(corw$area_dif)
corw$area_c40_num <- as.numeric(corw$area_c40)
corw$area_13k_num <- as.numeric(corw$area_13k)

corw$pop_den <- corw$pop_13k/corw$area_13k_num

#region
pre <-read.csv("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/NO2/Data_pre.csv", header=TRUE)
keeps <- c("ID","GBDSuperRegion", "Country")
pre_ = pre[keeps]
pre_ <- unique(pre_)
pre_ <- rename(pre_, id_13k="ID")
pre_$id_13k[pre_$id_13k==12221 | pre_$id_13k==12620] <- 24841 #Fuzhou
pre_$id_13k[pre_$id_13k==12547 | pre_$id_13k==12131] <- 24678 #Nanjing
pre_ <- unique(pre_)
corw <- merge(corw, pre_, by='id_13k')

#country GDP per capita
gdp <-read.csv("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/PM2.5/Inputs/gdp.csv", header=TRUE)
gdp<-gdp[c("Country.Name", "X2021")]
gdp<-rename(gdp, Country='Country.Name')
gdp<-rename(gdp, gdp='X2021')
gdp$gdp_cat <- ifelse(gdp$gdp<=1045, "Low income", 0) #world bank category
gdp$gdp_cat <- ifelse(gdp$gdp<=4095 & gdp$gdp>1045, "Lower-middle income", gdp$gdp_cat)
gdp$gdp_cat <- ifelse(gdp$gdp<=12695 & gdp$gdp>4095, "Upper-middle income", gdp$gdp_cat)
gdp$gdp_cat <- ifelse(gdp$gdp>12695, "High income", gdp$gdp_cat)
corw <- merge(corw, gdp, by='Country', all.x=TRUE)

#Moran's I
test <- subset(corw, corw$year==2019)
test <- test[c("id_13k", "geo_13k")]

l <- 2019
j <- "ENE"
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
  
  co2 <- co2 + co2_ }

id_13k <- test$id_13k
moran <- rep(1,92)
for (i in 1:92) {
  a <- st_as_sf(test$geo_13k[i])
  raster_extent <- extent(a)
  pm_cropped <- crop(co2, raster_extent)
  moran[i] <- Moran(pm_cropped)
}
moran <- cbind(id_13k, moran)
corw <- merge(corw, moran, by="id_13k")


#bivariate regression 
corw$dif <- corw$co2_per_cap_13k-corw$co2_per_cap_c40
corw$abs_dif <- abs(corw$dif)
corw$pct_chg <- corw$abs_dif/corw$co2_per_cap_c40
corw$ratio <- corw$co2_per_cap_13k/corw$co2_per_cap_c40
corw$year_cat <- factor(corw$year)
corw$region_cat <- factor(corw$GBDSuperRegion)

corw1 <-subset(corw, (year == 2019))

hist(corw1$abs_dif)
hist(log(corw1$abs_dif))
#hist(corw$area_c40_num)
hist(corw1$area_13k_num) #area
hist(log(corw1$area_13k_num))
hist(corw1$area_dif)
hist(log(corw1$area_dif))
hist(corw1$pop_13k) #pop
hist(log(corw1$pop_13k))
hist(corw1$pop_dif)
hist(log(corw1$pop_dif))
hist(corw1$pop_den)
hist(log(corw1$pop_den))
#hist(corw$mnt)
#hist(log(corw$mnt_der))
#hist(corw$gdp)
hist(corw1$moran)

corw1$log_abs_dif <- log(corw1$abs_dif)
corw1$log_area <- log(corw1$area_13k_num)
corw1$log_area_dif <- log(corw1$area_dif_num)
corw1$log_pop <- log(corw1$pop_13k)
corw1$log_pop_dif <- log(corw1$pop_dif)
corw1$log_pop_den <- log(corw1$pop_den)
#corw1$log_mnt <- log(corw1$mnt_der)



saveRDS(corw, file="/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/etc/corw.rds")
saveRDS(corw1, file="/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/etc/corw1.rds")


