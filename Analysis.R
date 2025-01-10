

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
library(mblm)
library(Kendall)
library(tidyverse)
library(fmsb)
library(extrafont)
library(scales)
library(grid)
library(png) 
library(corrplot)
library(patchwork)     
library(ggpubr)
library(ggrepel)
library(stringr)
library(showtext)
library(GGally)
library(ggnewscale)
font_import(pattern = "Lato")
loadfonts()
options(scipen=999)




#Data & Descriptive Analysis
#POPULATION-WEIGHTED AVERAGE
#PM25 
#extract final sample: cities with whole time-series
pwpm<-read.csv("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Final_estimates/PM25/pwpm_2000_13k_24jul1.csv", header=TRUE)
pwpm<-rename(pwpm, pwpm=pw_pm)
mss <- which(is.na(pwpm$pwpm))
inf <- which(is.infinite(pwpm$pwpm))
zero <- which(pwpm$pwpm == 0)
mss <- c(mss, inf, zero)
mss_list <- pwpm[mss, 2]
years<-c(2001:2020)
for (i in years) {
  pwpm<-read.csv(paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Final_estimates/PM25/pwpm_", i, "_13k_24jul1.csv"), header=TRUE)
  pwpm<-rename(pwpm, pwpm=pw_pm)
  mss <- which(is.na(pwpm$pwpm))
  inf <- which(is.infinite(pwpm$pwpm))
  zero <- which(pwpm$pwpm == 0)
  mss <- c(mss, inf, zero)  
  mss_list_ <- pwpm[mss, 2]
  mss_list <- c(mss_list, mss_list_)
}
mss_list <- unique(mss_list)
#data
pwpm<-read.csv("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Final_estimates/PM25/pwpm_2000_13k_24jul1.csv", header=TRUE)
pwpm<-rename(pwpm, pwpm=pw_pm)
pwpm <- pwpm[!(pwpm$ID %in% mss_list), ]
summary_pm_13k<-pwpm %>%
  group_by() %>%
  summarize(n=n(), mean=mean(pwpm), sd=sd(pwpm), median=median(pwpm), min=min(pwpm), max=max(pwpm))
summary_pm_13k$year<-2000

years<-c(2001:2020)
for (i in years) {
  pwpm<-read.csv(paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Final_estimates/PM25/pwpm_", i, "_13k_24jul1.csv"), header=TRUE)
  pwpm<-rename(pwpm, pwpm=pw_pm)
  pwpm <- pwpm[!(pwpm$ID %in% mss_list), ]
  a<-pwpm %>%
    group_by() %>%
    summarize(n=n(), mean=mean(pwpm), sd=sd(pwpm), median=median(pwpm), min=min(pwpm), max=max(pwpm))
  a$year<-i
  summary_pm_13k<-rbind(summary_pm_13k, a)
}

#NO2
#extract final sample: cities with whole time-series
pwno2<-read.csv("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Final_estimates/NO2/pwno2_2005_13k_24jul12.csv", header=TRUE)
pwno2 <- pwno2[, -1]
pwno2<-rename(pwno2, pwno2=pw_no2)
mss <- which(is.na(pwno2$pwno2))
inf <- which(is.infinite(pwno2$pwno2))
mss <- c(mss, inf)
mss_list <- pwno2[mss, 1]

years<-c(2006:2020)
for (i in years) {
  pwno2<-read.csv(paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Final_estimates/NO2/pwno2_", i, "_13k_24jul12.csv"), header=TRUE)
  pwno2 <- pwno2[, -1]
  pwno2<-rename(pwno2, pwno2=pw_no2)
  mss <- which(is.na(pwno2$pwno2))
  inf <- which(is.infinite(pwno2$pwno2))
  mss <- c(mss, inf)
  mss_list_ <- pwno2[mss, 1]
  mss_list <- c(mss_list, mss_list_)
}
mss_list <- unique(mss_list)
#data
pwno2<-read.csv("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Final_estimates/NO2/pwno2_2005_13k_24jul12.csv", header=TRUE)
pwno2 <- pwno2[, -1]
pwno2<-rename(pwno2, pwno2=pw_no2)
pwno2 <- pwno2[!(pwno2$ID %in% mss_list), ]
summary_no2_13k<-pwno2 %>%
  group_by() %>%
  summarize(n=n(), mean=mean(pwno2), sd=sd(pwno2), median=median(pwno2), min=min(pwno2), max=max(pwno2))
summary_no2_13k$year<-2005

years<-c(2006:2020)
for (i in years) {
  pwno2<-read.csv(paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Final_estimates/NO2/pwno2_", i, "_13k_24jul12.csv"), header=TRUE)
  pwno2 <- pwno2[, -1]
  pwno2<-rename(pwno2, pwno2=pw_no2)
  pwno2 <- pwno2[!(pwno2$ID %in% mss_list), ]
  a<-pwno2 %>%
    group_by() %>%
    summarize(n=n(), mean=mean(pwno2), sd=sd(pwno2), median=median(pwno2), min=min(pwno2), max=max(pwno2))
  a$year<-i
  summary_no2_13k<-rbind(summary_no2_13k, a)
}

#O3
#extract final sample: cities with whole time-series
pwo3<-read.csv("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Final_estimates/O3/pwo3_2000_13k_24jul1.csv", header=TRUE)
pwo3<-rename(pwo3, pwo3=pw_o3)
mss <- which(is.na(pwo3$pwo3))
inf <- which(is.infinite(pwo3$pwo3))
zero <- which(pwo3$pwo3 == 0)
mss <- c(mss, inf, zero)
mss_list <- pwo3[mss, 2]
years<-c(2001:2020)
for (i in years) {
  pwo3<-read.csv(paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Final_estimates/O3/pwo3_", i, "_13k_24jul1.csv"), header=TRUE)  
  pwo3<-rename(pwo3, pwo3=pw_o3)
  mss <- which(is.na(pwo3$pwo3))
  inf <- which(is.infinite(pwo3$pwo3))
  zero <- which(pwo3$pwo3 == 0)
  mss <- c(mss, inf, zero)
  mss_list_ <- pwo3[mss, 2]
  mss_list <- c(mss_list, mss_list_)
}
mss_list <- unique(mss_list)
#data
pwo3<-read.csv("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Final_estimates/O3/pwo3_2000_13k_24jul1.csv", header=TRUE)
pwo3<-rename(pwo3, pwo3=pw_o3)
pwo3 <- pwo3[!(pwo3$ID %in% mss_list), ]
summary_o3_13k<-pwo3 %>%
  group_by() %>%
  summarize(n=n(), mean=mean(pwo3), sd=sd(pwo3), median=median(pwo3), min=min(pwo3), max=max(pwo3))
summary_o3_13k$year <- 2000

years <- c(2001:2020)
for (i in years) {
  pwo3<-read.csv(paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Final_estimates/O3/pwo3_", i, "_13k_24jul1.csv"), header=TRUE)  
  pwo3<-rename(pwo3, pwo3=pw_o3)
  pwo3 <- pwo3[!(pwo3$ID %in% mss_list), ]
  a<-pwo3 %>%
    group_by() %>%
    summarize(n=n(), mean=mean(pwo3), sd=sd(pwo3), median=median(pwo3), min=min(pwo3), max=max(pwo3))
  a$year<-i
  summary_o3_13k<-rbind(summary_o3_13k, a)
}

#CO2
co2_per_cap <- read.csv("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Final_estimates/CO2/co2_per_capita_13k_2000.csv", header=TRUE)
mss <- which(is.na(co2_per_cap$co2_per_cap))
inf <- which(is.infinite(co2_per_cap$co2_per_cap))
mss <- c(mss, inf)
mss_list <- co2_per_cap[mss, 2]
years <- c(2001:2020)
for (i in years) {
  co2_per_cap<-read.csv(paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Final_estimates/CO2/co2_per_capita_13k_", i, ".csv"), header=TRUE)
  mss <- which(is.na(co2_per_cap$co2_per_cap))
  inf <- which(is.infinite(co2_per_cap$co2_per_cap))
  mss <- c(mss, inf)
  mss_list_ <- co2_per_cap[mss, 2]
  mss_list <- c(mss_list, mss_list_)
}
mss_list <- unique(mss_list)

co2_per_cap <- read.csv("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Final_estimates/CO2/co2_per_capita_13k_2000.csv", header=TRUE)
co2_per_cap$co2_per_cap <- ifelse(co2_per_cap$co2_per_cap=="Inf", NA, co2_per_cap$co2_per_cap)
co2_per_cap <- co2_per_cap[!(co2_per_cap$ID %in% mss_list), ]
summary_co2_per_cap <- co2_per_cap %>%
  group_by() %>%
  summarize(n=n(), mean=mean(co2_per_cap), sd=sd(co2_per_cap), median=median(co2_per_cap), min=min(co2_per_cap), max=max(co2_per_cap))
summary_co2_per_cap$year <- 2000

years <- c(2001:2020)
for (i in years) {
  co2_per_cap<-read.csv(paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Final_estimates/CO2/co2_per_capita_13k_", i, ".csv"), header=TRUE)
  co2_per_cap$co2_per_cap <- ifelse(co2_per_cap$co2_per_cap=="Inf", NA, co2_per_cap$co2_per_cap)
  co2_per_cap <- co2_per_cap[!(co2_per_cap$ID %in% mss_list), ]
  summary_co2_per_cap_ <- co2_per_cap %>%
    group_by() %>%
    summarize(n=n(), mean=mean(co2_per_cap), sd=sd(co2_per_cap), median=median(co2_per_cap), min=min(co2_per_cap), max=max(co2_per_cap))
  summary_co2_per_cap_$year <- i
  
  summary_co2_per_cap<-rbind(summary_co2_per_cap, summary_co2_per_cap_)
}

##########################
#### MAP: FIGURE 1 #######
##########################
discrete_gradient_pal <- function(colours, bins = 5) {
  ramp <- scales::colour_ramp(colours)
  
  function(x) {
    if (length(x) == 0) return(character())
    
    i <- floor(x * bins)
    i <- ifelse(i > bins-1, bins-1, i)
    ramp(i/(bins-1))
  }
}

scale_colour_discrete_gradient <- function(..., colours, bins = 10, na.value = NA, guide = "colourbar", aesthetics = "colour", colors)  {
  colours <- if (missing(colours)) 
    colors
  else colours
  continuous_scale(
    aesthetics,
    "discrete_gradient",
    discrete_gradient_pal(colours, bins),
    na.value = na.value,
    guide = guide,
    ...
  )
}

#PM25
pwpm<-read.csv("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Final_estimates/PM25/pwpm_2000_13k_24jul1.csv", header=TRUE)
pwpm<-rename(pwpm, pwpm=pw_pm)
pwpm <- na.omit(pwpm)
pwpm <- pwpm[is.finite(pwpm$pwpm), ]
pwpm <- subset(pwpm, pwpm != 0)
pwpm$year <- 2000

years<-c(2001:2020)
for (i in years) {
  pwpm_<-read.csv(paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Final_estimates/PM25/pwpm_", i, "_13k_24jul1.csv"), header=TRUE)
  pwpm_<-rename(pwpm_, pwpm=pw_pm)
  pwpm_ <- na.omit(pwpm_)
  pwpm_ <- pwpm_[is.finite(pwpm_$pwpm), ]
  pwpm_ <- subset(pwpm_, pwpm != 0)
  pwpm_$year <- i
  pwpm <- rbind(pwpm, pwpm_)
}
#ggplot(pwpm, aes(x = as.factor(year), y = pwpm)) +
#  geom_boxplot() 

aa<-read.csv("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/NO2/Inputs/sogacities_forpawan.csv", header=TRUE)
aa$id <- seq.int(nrow(aa))
aa <- rename(aa, lat='LAT_CENTROID', lon='LNG_CENTROID', country='GEOPYCOUNTRY')
keeps <- c("id", "lat", "lon", "country")
aaa = aa[keeps]
city.shp <- st_read("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/NO2/Inputs/hdc_naming_warp.shp")
city.shp <- rename(city.shp, id='ID_HDC_G0')
bb <- merge(city.shp, aaa, by='id')
bb <- rename(bb, ID='id')
a <- merge(bb, pwpm, by='ID')
#a$pwpm <- ifelse(a$pwpm>=70, 70, a$pwpm) 
i <- 2019
x <-subset(a, a$year==i)
world <- ne_countries(scale='medium', returnclass="sf")
class(world)
median_value <- round(median(x$pwpm), digits=1)
q99 <- round(as.numeric(quantile(x$pwpm, 0.99)), digits=1)
q1 <- round(as.numeric(quantile(x$pwpm, 0.01)), digits=1)
x$pwpm <- ifelse(x$pwpm<=q1, q1, x$pwpm) 
x$pwpm <- ifelse(x$pwpm>=q99, q99, x$pwpm) 
legend_title <- bquote(paste("Population-weighted ", PM[2.5] * " (\u03BCg/m³, 2019)"))
unit_title <- bquote(paste(PM[2.5] * " (", mu, "g/m³)"))

pdf(paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Study/Adv_Sci/Co-authors/Figures/Figure1_PM25.pdf"),
    width     = 11,
    height    = 8)
par(family = "Lato")
par(mar = c(1, 1, 1, 1))
ggplot() +
  geom_sf(data = world, fill = 'grey80', color = 'white') +
  geom_point(data=x, mapping = aes(x=lon, y=lat, color=pwpm), fill=x$pwpm, size=0.5) +
  scale_colour_discrete_gradient(
    colours = rev(RColorBrewer::brewer.pal(9, "Spectral")),
    breaks = seq(q1, q99, length.out=6),
    limits = c(q1, q99),
    guide = guide_colourbar(nbin = 10, raster = FALSE, frame.colour = "grey80", ticks.colour = NA),  name=unit_title) +  
  labs(title="", x='', y="") +
  theme_void() +
  theme(plot.title=element_text(hjust=0.5, size = 17),
        panel.background = element_rect(fill = "grey96", color="white"), 
        legend.text = element_text(vjust = 0.3, hjust=0.5, size=12),
        legend.title = element_text(size = 14),
        plot.margin = margin(0, 10, 0, 10, "pt"),
        legend.key.height = unit(2, "cm"),  
        legend.key.width = unit(.5, "cm"),
        aspect.ratio=1.8/3) +
  ylim(-50,80) + xlim(-150, 170)
dev.off()

#NO2 
pwno2<-read.csv("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Final_estimates/NO2/pwno2_2005_13k_24jul12.csv", header=TRUE)
pwno2 <- pwno2[, -1]
pwno2<-rename(pwno2, pwno2=pw_no2)
pwno2 <- na.omit(pwno2)
pwno2 <- pwno2[is.finite(pwno2$pwno2), ]
pwno2 <- subset(pwno2, pwno2 != 0)
pwno2$year <- 2005

years<-c(2006:2020)
for (i in years) {
  pwno2_<-read.csv(paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Final_estimates/NO2/pwno2_", i, "_13k_24jul12.csv"), header=TRUE)
  pwno2_ <- pwno2_[, -1]
  pwno2_<-rename(pwno2_, pwno2=pw_no2)
  pwno2_ <- na.omit(pwno2_)
  pwno2_ <- pwno2_[is.finite(pwno2_$pwno2), ]
  pwno2_ <- subset(pwno2_, pwno2 != 0)
  pwno2_$year <- i
  pwno2 <- rbind(pwno2, pwno2_)
}
#ggplot(pwno2, aes(x = as.factor(year), y = pwno2)) +
# geom_boxplot() 

aa<-read.csv("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/NO2/Inputs/sogacities_forpawan.csv", header=TRUE)
aa$id <- seq.int(nrow(aa))
aa <- rename(aa, lat='LAT_CENTROID', lon='LNG_CENTROID', country='GEOPYCOUNTRY')
keeps <- c("id", "lat", "lon", "country")
aaa = aa[keeps]
city.shp <- st_read("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/NO2/Inputs/hdc_naming_warp.shp")
city.shp <- rename(city.shp, id='ID_HDC_G0')
bb <- merge(city.shp, aaa, by='id')
bb <- rename(bb, ID='id')
a <- merge(bb, pwno2, by='ID')
#a$pwno2 <- ifelse(a$pwno2>=20, 20, a$pwno2) 
#a$pwno2 <- ifelse(a$pwno2==0, 0.0000001, a$pwno2)

i <- 2019
x <-subset(a, a$year==i)
world <- ne_countries(scale='medium', returnclass="sf")
class(world)
median_value <- round(median(x$pwno2), digits=1)
q99 <- round(as.numeric(quantile(x$pwno2, 0.99)), digits=1)
q1 <- round(as.numeric(quantile(x$pwno2, 0.01)), digits=1)
x$pwno2 <- ifelse(x$pwno2<=q1, q1, x$pwno2) 
x$pwno2 <- ifelse(x$pwno2>=q99, q99, x$pwno2) 
legend_title <- bquote(paste("Population-weighted ", NO[2] * " (ppb, 2019)"))
unit_title <- bquote(paste(NO[2] *" (ppb)"))


pdf(paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Study/Adv_Sci/Co-authors/Figures/Figure1_NO2.pdf"),
    width     = 11,
    height    = 8,
    pointsize = 5)
par(family = "Lato")
par(mar = c(10, 10, 10, 10))
ggplot() +
  geom_sf(data = world, fill = 'grey80', color = 'white') +
  geom_point(data=x, mapping = aes(x=lon, y=lat, color=pwno2), fill=x$pwno2, size=0.5) +
  scale_colour_discrete_gradient(
    colours = rev(RColorBrewer::brewer.pal(9, "Spectral")),
    breaks = seq(q1, q99, length.out=6),
    limits = c(q1, q99),
    guide = guide_colourbar(nbin = 10, raster = FALSE, frame.colour = "grey80", ticks.colour = NA),  name=unit_title) +  
  labs(title="", x='', y="") +
  theme_void() +
  theme(plot.title=element_text(hjust=0.5, size = 17),
        panel.background = element_rect(fill = "grey96", color="white"), 
        legend.text = element_text(vjust = 0.3, hjust=0.5, size=12),
        legend.title = element_text(size = 14),
        plot.margin = margin(0, 10, 0, 10, "pt"),
        legend.key.height = unit(2, "cm"),  
        legend.key.width = unit(.5, "cm"),
        aspect.ratio=1.8/3) +
  ylim(-50,80) + xlim(-150, 170)
dev.off()

#O3
pwo3<-read.csv("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Final_estimates/O3/pwo3_2000_13k_24jul1.csv", header=TRUE)
pwo3<-rename(pwo3, pwo3=pw_o3)
pwo3 <- na.omit(pwo3)
pwo3 <- pwo3[is.finite(pwo3$pwo3), ]
pwo3 <- subset(pwo3, pwo3 != 0)
pwo3$year <- 2000

years <- c(2001:2020)
for (i in years) {
  pwo3_<-read.csv(paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Final_estimates/O3/pwo3_", i, "_13k_24jul1.csv"), header=TRUE)  
  pwo3_<-rename(pwo3_, pwo3=pw_o3)
  pwo3_ <- na.omit(pwo3_)
  pwo3_ <- pwo3_[is.finite(pwo3_$pwo3), ]
  pwo3_ <- subset(pwo3_, pwo3 != 0)
  pwo3_$year <- i
  pwo3 <- rbind(pwo3, pwo3_)
}
#ggplot(pwo3, aes(x = as.factor(year), y = pwo3)) +
# geom_boxplot() 

aa<-read.csv("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/NO2/Inputs/sogacities_forpawan.csv", header=TRUE)
aa$id <- seq.int(nrow(aa))
aa <- rename(aa, lat='LAT_CENTROID', lon='LNG_CENTROID', country='GEOPYCOUNTRY')
keeps <- c("id", "lat", "lon", "country")
aaa = aa[keeps]
city.shp <- st_read("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/NO2/Inputs/hdc_naming_warp.shp")
city.shp <- rename(city.shp, id='ID_HDC_G0')
bb <- merge(city.shp, aaa, by='id')
bb <- rename(bb, ID='id')
a <- merge(bb, pwo3, by='ID')
#a$pwo3 <- ifelse(a$pwo3>=60, 60, a$pwo3) 

i <- 2019
x <-subset(a, a$year==i)
world <- ne_countries(scale='medium', returnclass="sf")
class(world)
q99 <- round(as.numeric(quantile(x$pwo3, 0.99)), digits=1)
q1 <- round(as.numeric(quantile(x$pwo3, 0.01)), digits=1)
x$pwo3 <- ifelse(x$pwo3<=q1, q1, x$pwo3) 
x$pwo3 <- ifelse(x$pwo3>=q99, q99, x$pwo3) 
legend_title <- bquote(paste("Population-weighted ", O[3] * " (ppb, 2019)"))
unit_title <- bquote(paste(O[3] * " (ppb)"))


pdf(paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Study/Adv_Sci/Co-authors/Figures/Figure1_O3.pdf"),
    width     = 11,
    height    = 8,
    pointsize = 5)
par(family = "Lato")
par(mar = c(1, 1, 1, 1))
ggplot() +
  geom_sf(data = world, fill = 'grey80', color = 'white') +
  geom_point(data=x, mapping = aes(x=lon, y=lat, color=pwo3), fill=x$pwo3, size=0.5) +
  scale_colour_discrete_gradient(
    colours = rev(RColorBrewer::brewer.pal(9, "Spectral")),
    breaks = seq(q1, q99, length.out=6),
    limits = c(q1, q99),
    guide = guide_colourbar(nbin = 10, raster = FALSE, frame.colour = "grey80", ticks.colour = NA),  name=unit_title) +  
  labs(title="", x='', y="") +
  theme_void() +
  theme(plot.title=element_text(hjust=0.5, size = 17),
        panel.background = element_rect(fill = "grey96", color="white"), 
        legend.text = element_text(vjust = 0.3, hjust=0.5, size=12),
        legend.title = element_text(size = 14),
        plot.margin = margin(0, 10, 0, 10, "pt"),
        legend.key.height = unit(2, "cm"),  
        legend.key.width = unit(.5, "cm"),
        aspect.ratio=1.8/3) +
  ylim(-50,80) + xlim(-150, 170)
dev.off()

#CO2
co2_per_cap <- read.csv("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Final_estimates/CO2/co2_per_capita_13k_2000.csv", header=TRUE)
co2_per_cap <- na.omit(co2_per_cap)
co2_per_cap <- co2_per_cap[is.finite(co2_per_cap$co2_per_cap), ]
co2_per_cap <- subset(co2_per_cap, co2_per_cap != 0)
co2_per_cap$year <- 2000

years <- c(2001:2020)
for (i in years) {
  co2_per_cap_<-read.csv(paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Final_estimates/CO2/co2_per_capita_13k_", i, ".csv"), header=TRUE)
  co2_per_cap_ <- na.omit(co2_per_cap_)
  co2_per_cap_ <- co2_per_cap_[is.finite(co2_per_cap_$co2_per_cap), ]
  co2_per_cap_ <- subset(co2_per_cap_, co2_per_cap != 0)
  co2_per_cap_$year <- i
  co2_per_cap <- rbind(co2_per_cap, co2_per_cap_)
}
#ggplot(co2_per_cap, aes(x = as.factor(year), y = log(co2_per_cap))) +
# geom_boxplot() 

aa<-read.csv("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/NO2/Inputs/sogacities_forpawan.csv", header=TRUE)
aa$id <- seq.int(nrow(aa))
aa <- rename(aa, lat='LAT_CENTROID', lon='LNG_CENTROID', country='GEOPYCOUNTRY')
keeps <- c("id", "lat", "lon", "country")
aaa = aa[keeps]
city.shp <- st_read("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/NO2/Inputs/hdc_naming_warp.shp")
city.shp <- rename(city.shp, id='ID_HDC_G0')
bb <- merge(city.shp, aaa, by='id')
bb <- rename(bb, ID='id')
a <- merge(bb, co2_per_cap, by='ID')
a <- a[, !names(a) %in% c("X", "X.1")]

i <- 2019
x <-subset(a, a$year==i)
world <- ne_countries(scale='medium', returnclass="sf")
class(world)
median_value <- round(median(x$co2_per_cap), digits=1)
q90 <- round(as.numeric(quantile(x$co2_per_cap, 0.9)), digits=1)
q1 <- round(as.numeric(quantile(x$co2_per_cap, 0.01)), digits=1)
x$co2_per_cap <- ifelse(x$co2_per_cap<=q1, q1, x$co2_per_cap) 
x$co2_per_cap <- ifelse(x$co2_per_cap>=q90, q90, x$co2_per_cap) 
legend_title <- bquote(paste(CO[2], " per capita (ton, 2019)"))
unit_title <- bquote(atop(CO[2]~"per capita", "(metric tons)"))

pdf(paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Study/Adv_Sci/Co-authors/Figures/Figure1_CO2.pdf"),
    width     = 11,
    height    = 8,
    pointsize = 5)
par(family = "Lato")
par(mar = c(1, 1, 1, 1))
ggplot() +
  geom_sf(data = world, fill = 'grey80', color = 'white') +
  geom_point(data=x, mapping = aes(x=lon, y=lat, color=co2_per_cap), fill=x$co2_per_cap, size=0.5) +
  scale_colour_discrete_gradient(
    colours = rev(RColorBrewer::brewer.pal(9, "Spectral")),
    breaks = seq(q1, q90, length.out=6),
    limits = c(q1, q90),
    guide = guide_colourbar(nbin = 10, raster = FALSE, frame.colour = "grey80", ticks.colour = NA),  name=unit_title) +  
  labs(title="", x='', y="") +
  theme_void() +
  theme(plot.title=element_text(hjust=0.5, size = 17),
        panel.background = element_rect(fill = "grey96", color="white"), 
        legend.text = element_text(vjust = 0.3, hjust=0.5, size=12),
        legend.title = element_text(size = 14),
        plot.margin = margin(0, 10, 0, 10, "pt"),
        legend.key.height = unit(2, "cm"),  
        legend.key.width = unit(.5, "cm"),
        aspect.ratio=1.8/3) +
  ylim(-50,80) + xlim(-150, 170)
dev.off()




###########################################
#### Spatial correlation: Figure S2 #######
###########################################
aa<-read.csv("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/NO2/Inputs/sogacities_forpawan.csv", header=TRUE)
aa$id <- seq.int(nrow(aa))
aa <- rename(aa, lat='LAT_CENTROID', lon='LNG_CENTROID', country='GEOPYCOUNTRY')
keeps <- c("id", "lat", "lon", "country")
aaa = aa[keeps]
city.shp <- st_read("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/NO2/Inputs/hdc_naming_warp.shp")
city.shp <- rename(city.shp, id='ID_HDC_G0')
bb <- merge(city.shp, aaa, by='id')
bb <- rename(bb, ID='id')
a <- merge(bb, pwpm, by='ID')
#a$pwpm <- ifelse(a$pwpm>=70, 70, a$pwpm) 
i <- 2019
pm19 <-subset(a, a$year==i)
pm19 <- as.data.frame(pm19)

aa<-read.csv("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/NO2/Inputs/sogacities_forpawan.csv", header=TRUE)
aa$id <- seq.int(nrow(aa))
aa <- rename(aa, lat='LAT_CENTROID', lon='LNG_CENTROID', country='GEOPYCOUNTRY')
keeps <- c("id", "lat", "lon", "country")
aaa = aa[keeps]
city.shp <- st_read("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/NO2/Inputs/hdc_naming_warp.shp")
city.shp <- rename(city.shp, id='ID_HDC_G0')
bb <- merge(city.shp, aaa, by='id')
bb <- rename(bb, ID='id')
a <- merge(bb, pwno2, by='ID')
#a$pwno2 <- ifelse(a$pwno2>=20, 20, a$pwno2) 
#a$pwno2 <- ifelse(a$pwno2==0, 0.0000001, a$pwno2)
i <- 2019
no219 <-subset(a, a$year==i)
no219 <- as.data.frame(no219)

aa<-read.csv("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/NO2/Inputs/sogacities_forpawan.csv", header=TRUE)
aa$id <- seq.int(nrow(aa))
aa <- rename(aa, lat='LAT_CENTROID', lon='LNG_CENTROID', country='GEOPYCOUNTRY')
keeps <- c("id", "lat", "lon", "country")
aaa = aa[keeps]
city.shp <- st_read("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/NO2/Inputs/hdc_naming_warp.shp")
city.shp <- rename(city.shp, id='ID_HDC_G0')
bb <- merge(city.shp, aaa, by='id')
bb <- rename(bb, ID='id')
a <- merge(bb, pwo3, by='ID')
#a$pwo3 <- ifelse(a$pwo3>=60, 60, a$pwo3) 
i <- 2019
o319 <-subset(a, a$year==i)
o319 <- as.data.frame(o319)

aa<-read.csv("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/NO2/Inputs/sogacities_forpawan.csv", header=TRUE)
aa$id <- seq.int(nrow(aa))
aa <- rename(aa, lat='LAT_CENTROID', lon='LNG_CENTROID', country='GEOPYCOUNTRY')
keeps <- c("id", "lat", "lon", "country")
aaa = aa[keeps]
city.shp <- st_read("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/NO2/Inputs/hdc_naming_warp.shp")
city.shp <- rename(city.shp, id='ID_HDC_G0')
bb <- merge(city.shp, aaa, by='id')
bb <- rename(bb, ID='id')
a <- merge(bb, co2_per_cap, by='ID')
a <- a[, !names(a) %in% c("X", "X.1")]
i <- 2019
co219 <-subset(a, a$year==i)
co219 <- as.data.frame(co219)

hist(pm19$pwpm)
hist(no219$pwno2)
hist(o319$pwo3)
subset_data <- subset(co219, co2_per_cap < 40)
hist(subset_data$co2_per_cap)
hist(co219$co2_per_cap)
hist(log(co219$co2_per_cap))

b <- read.csv("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/etc/GBDsuperregion_final_final.csv")
pmno <- merge(pm19, no219, by="ID", all=FALSE)
pmno <- merge(pmno, b, by="ID", all=FALSE)
pmo <- merge(pm19, o319, by="ID", all=FALSE)
pmo <- merge(pmo, b, by="ID", all=FALSE)
pmco <- merge(pm19, co219, by="ID", all=FALSE)
pmco <- merge(pmco, b, by="ID", all=FALSE)
noo <- merge(no219, o319, by="ID", all=FALSE)
noo <- merge(noo, b, by="ID", all=FALSE)
noco <- merge(no219, co219, by="ID", all=FALSE)
noco <- merge(noco, b, by="ID", all=FALSE)
oco <- merge(o319, co219, by="ID", all=FALSE)
oco <- merge(oco, b, by="ID", all=FALSE)

options(scipen = 999, digits = 6)
cor.test(pmo$pwpm, pmo$pwo3, method = "pearson")
cor.test(pmco$pwpm, log(pmco$co2_per_cap), method = "pearson")
cor.test(noo$pwno2, noo$pwo3, method = "pearson")
cor.test(noco$pwno2, log(noco$co2_per_cap), method = "pearson")
cor.test(oco$pwo3, log(oco$co2_per_cap), method = "pearson")

#plots
showtext_auto()
font_add_google("Lato")
pal <- c("grey30", "olivedrab2", "mediumseagreen", "mediumpurple", "#0072B2", "darkorange", "#CC6677")


#PMNO2
pmno <- subset(pmno, !is.na(pmno$GBDSuperRegion))
a <- cor.test(pmno$pwpm, pmno$pwno2, method = "pearson")
est <- round(a$estimate, 4)
p <- round(a$p.value, 4)
p <- ifelse(p==0, "<0.0001", paste0("=", p))
regression_line <- lm(pwno2 ~ pwpm, data = pmno)

pdf(file = paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Study/Nature_cities/Figures/Extended_Data_Fig.2/FigureS2_PM25_NO2.pdf"),
    width     = 10,
    height    = 10
)
par(mar = c(2, 2, 2, 2))
ggplot(pmno, aes(x = pwpm, y = pwno2, color = GBDSuperRegion, shape = GBDSuperRegion)) +
  geom_point(size = 1.7, alpha = 0.35) +
  scale_color_manual(values = pal, name = "GBD super-region") +
  scale_shape_manual(values = c(16, 16, 17, 15, 16, 17, 16)) +
  theme(legend.position = "none", text = element_text(size = 25)) +
  geom_smooth(method = "lm", se = FALSE, linetype = "solid", size = 1.5) +
  geom_abline(intercept = coef(regression_line)[1], slope = coef(regression_line)[2], color = "red", size = 1.5) +
  annotate("text", 
           x = mean(range(pmno$pwpm)), 
           y = quantile(range(pmno$pwno2), 0.9), 
           label = paste("r =", est, ", p", p, "\ny =", round(regression_line$coefficients[1], 2), "+", round(regression_line$coefficients[2], 3), "x"), 
           hjust = 0.5, size = 8, color = "red") +
  labs(
    x = bquote(PM[2.5] * " (" * mu * "g/m"^3 * ")"),
    y = bquote(NO[2] * " (ppb)")
  )
dev.off()


#PMO3
pmo <- subset(pmo, !is.na(pmo$GBDSuperRegion))
a <- cor.test(pmo$pwpm, pmo$pwo3, method = "pearson")
est <- round(a$estimate, 4)
p <- round(a$p.value, 4)
p <- ifelse(p==0, "<0.0001", paste0("=", p))
regression_line <- lm(pwo3 ~ pwpm, data = pmo)

pdf(file = paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Study/Nature_cities/Figures/Extended_Data_Fig.2/FigureS2_PM25_O3.pdf"),
    width     = 10,
    height    = 10
)
par(mar = c(2, 2, 2, 2))
ggplot(pmo, aes(x = pwpm, y = pwo3, color=GBDSuperRegion, shape=GBDSuperRegion)) +
  geom_point(size = 1.7, alpha = 0.35) +
  scale_color_manual(values = pal, name = "GBD super-region") +
  scale_shape_manual(values = c(16, 16, 17, 15, 16, 17, 16)) +
  theme(legend.position = "none", text = element_text(size = 25)) +
  geom_smooth(method = "lm", se = FALSE, linetype = "solid", size = 1.5) +
  geom_abline(intercept = coef(regression_line)[1], slope = coef(regression_line)[2], color = "red", size = 1.5) +
  annotate("text", x = mean(range(pmo$pwpm)), y = quantile(range(pmo$pwo3), 0.1), label = paste("r =", est, ", p", p, "\ny=", round(regression_line$coefficients[1],2), "+", round(regression_line$coefficients[2],3), "x" ), hjust = .5, size=8, color="red") +
  labs(x = bquote(PM[2.5] * " (" * mu * "g/m"^3 * ")"), 
       y = bquote(O[3] * " (ppb)"))
dev.off()

#PMCO2
pmco <- subset(pmco, !is.na(pmco$GBDSuperRegion))
a <- cor.test(pmco$pwpm, log(pmco$co2_per_cap), method = "pearson")
est <- round(a$estimate, 4)
p <- round(a$p.value, 4)
p <- ifelse(p==0, "<0.0001", paste0("=", p))
regression_line <- lm(log(co2_per_cap) ~ pwpm, data = pmco)

pdf(file = paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Study/Nature_cities/Figures/Extended_Data_Fig.2/FigureS2_PM25_CO2.pdf"),
    width     = 10,
    height    = 10
)
par(mar = c(2, 2, 2, 2))
ggplot(pmco, aes(x = pwpm, y = log(co2_per_cap), color=GBDSuperRegion, shape=GBDSuperRegion)) +
  geom_point(size = 1.7, alpha = 0.35) +
  scale_color_manual(values = pal, name = "GBD super-region") +
  scale_shape_manual(values = c(16, 16, 17, 15, 16, 17, 16)) +
  theme(legend.position = "none", text = element_text(size = 25)) +
  geom_smooth(method = "lm", se = FALSE, linetype = "solid", size = 1.5) +
  geom_abline(intercept = coef(regression_line)[1], slope = coef(regression_line)[2], color = "red", size = 1.5) +
  annotate("text", x = mean(range(pmco$pwpm)), y = quantile(range(log(pmco$co2_per_cap)), 0.95), label = paste("r =", est, ", p", p, "\ny=", round(regression_line$coefficients[1],2), "+", round(regression_line$coefficients[2],4), "x"), hjust = .5, size=8, color="red") +
  labs(x = bquote(PM[2.5] * " (" * mu * "g/m"^3 * ")"),
       y = bquote(paste("log(", FFCO[2] * " per capita) (metric tons)")))
dev.off()

#NO2O3
noo <- subset(noo, !is.na(noo$GBDSuperRegion))
a <- cor.test(noo$pwno2, noo$pwo3, method = "pearson")
est <- round(a$estimate, 4)
p <- round(a$p.value, 4)
p <- ifelse(p==0, "<0.0001", paste0("=", p))
regression_line <- lm(pwo3 ~ pwno2, data = noo)

pdf(file = paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Study/Nature_cities/Figures/Extended_Data_Fig.2/FigureS2_NO2_O3.pdf"),
    width     = 10,
    height    = 10
)
par(mar = c(2, 2, 2, 2))
ggplot(noo, aes(x = pwno2, y = pwo3, color=GBDSuperRegion, shape=GBDSuperRegion)) +
  geom_point(size = 1.7, alpha = 0.35) +
  scale_color_manual(values = pal, name = "GBD super-region") +
  scale_shape_manual(values = c(16, 16, 17, 15, 16, 17, 16)) +
  theme(legend.position = "none", text = element_text(size = 25)) +
  geom_smooth(method = "lm", se = FALSE, linetype = "solid", size = 1.5) +
  geom_abline(intercept = coef(regression_line)[1], slope = coef(regression_line)[2], color = "red", size = 1.5) +
  annotate("text", x = mean(range(noo$pwno2)), y = quantile(range(noo$pwo3), 0.05), label = paste("r =", est, ", p", p, "\ny=", round(regression_line$coefficients[1],2), "+", round(regression_line$coefficients[2],3), "x"), hjust = .5, size=8, color="red") +
  labs(x = bquote(NO[2] * " (ppb)"), y = bquote(O[3] * " (ppb)"))
dev.off()

#NO2CO2
noco <- subset(noco, !is.na(noco$GBDSuperRegion))
a <- cor.test(noco$pwno2, log(noco$co2_per_cap), method = "pearson")
est <- round(a$estimate, 4)
p <- round(a$p.value, 4)
p <- ifelse(p==0, "<0.0001", paste0("=", p))
regression_line <- lm(log(co2_per_cap) ~ pwno2, data = noco)

pdf(file = paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Study/Nature_cities/Figures/Extended_Data_Fig.2/FigureS2_NO2_CO2.pdf"),
    width     = 10,
    height    = 10
)
par(mar = c(2, 2, 2, 2))
ggplot(noco, aes(x = pwno2, y = log(co2_per_cap), color=GBDSuperRegion, shape=GBDSuperRegion)) +
  geom_point(size = 1.7, alpha = 0.35) +
  scale_color_manual(values = pal, name = "GBD super-region") +
  scale_shape_manual(values = c(16, 16, 17, 15, 16, 17, 16)) +
  theme(legend.position = "none", text = element_text(size = 25)) +
  geom_smooth(method = "lm", se = FALSE, linetype = "solid", size = 1.5) +
  geom_abline(intercept = coef(regression_line)[1], slope = coef(regression_line)[2], color = "red", size = 1.5) +
  annotate("text", x = mean(range(noco$pwno2)), y = quantile(range(log(noco$co2_per_cap)), 0.1), label = paste("r =", est, ", p", p, "\ny=", round(regression_line$coefficients[1],2), "+", round(regression_line$coefficients[2],3), "x"), hjust = .5, size=8, color="red") +
  labs(x = bquote(NO[2] * " (ppb)"), y = bquote(paste("log(", FFCO[2] * " per capita) (metric tons)")))
dev.off()

#O3CO2
oco <- subset(oco, !is.na(oco$GBDSuperRegion))
a <- cor.test(oco$pwo3, log(oco$co2_per_cap), method = "pearson")
est <- round(a$estimate, 4)
p <- round(a$p.value, 4)
p <- ifelse(p==0, "<0.0001", paste0("=", p))
regression_line <- lm(log(co2_per_cap) ~ pwo3, data = oco)

pdf(file = paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Study/Nature_cities/Figures/Extended_Data_Fig.2/FigureS2_O3_CO2.pdf"),
    width     = 10,
    height    = 10
)
par(mar = c(2, 2, 2, 2))
ggplot(oco, aes(x = pwo3, y = log(co2_per_cap), color=GBDSuperRegion, shape=GBDSuperRegion)) +
  geom_point(size = 1.7, alpha = 0.35) +
  scale_color_manual(values = pal, name = "GBD super-region") +
  scale_shape_manual(values = c(16, 16, 17, 15, 16, 17, 16)) +
  theme(legend.position = "none", text = element_text(size = 25)) +
  geom_smooth(method = "lm", se = FALSE, linetype = "solid", size = 1.3) +
  geom_abline(intercept = coef(regression_line)[1], slope = coef(regression_line)[2], color = "red", size = 1.5) +
  annotate("text", x = mean(range(oco$pwo3)), y = quantile(range(log(oco$co2_per_cap)), 0.95), label = paste("r =", est, ", p", p, "\ny=", round(regression_line$coefficients[1],2), "+", round(regression_line$coefficients[2],3), "x"), hjust = .5, size=8, color="red") +
  labs(x = bquote(O[3] * " (ppb)"), y = bquote(paste("log(", FFCO[2] * " per capita) (metric tons)")))
dev.off()

#Legend plot
pdf(file = paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Study/Nature_cities/Figures/Extended_Data_Fig.2/FigureS2_legend.pdf"),
    width     = 20,
    height    = 10
)
ggplot(oco, aes(x = pwo3, y = log(co2_per_cap), color=GBDSuperRegion, shape=GBDSuperRegion)) +
  geom_point(size=5, alpha=0.6) +
  scale_color_manual(values=pal, name="GBD super-region") +
  scale_shape_manual(values = c(16, 16, 17, 15, 16, 17, 16)) +
  guides(
    shape = guide_legend(override.aes = list(color = pal), ncol=2)) + 
  theme(text = element_text(size = 25)) + 
  annotate("text", x = mean(range(oco$pwo3)), y = quantile(range(log(oco$co2_per_cap)), 0.9), label = paste("R =", est, ", p", p), hjust = .5, size=10, color="brown2") +
  geom_abline(intercept = coef(regression_line)[1], slope = coef(regression_line)[2], color = "brown2", size=.8) +
  labs(x = expression(O[3]), y = bquote(paste("log(", FFCO[2] * " per capita)")))
dev.off()






##########################################
####### TIME SERIES PLOT: Figure 2 #######
##########################################
#data
x<-read.csv("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/etc/GBDsuperregion_final_final.csv") 
x<-unique(x)

#PM25
#extract final sample: cities with whole time-series
pwpm<-read.csv("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Final_estimates/PM25/pwpm_2000_13k_24jul1.csv", header=TRUE)
pwpm<-rename(pwpm, pwpm=pw_pm)
mss <- which(is.na(pwpm$pwpm))
inf <- which(is.infinite(pwpm$pwpm))
zero <- which(pwpm$pwpm == 0)
mss <- c(mss, inf, zero)
mss_list <- pwpm[mss, 2]
years<-c(2001:2020)
for (i in years) {
  pwpm<-read.csv(paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Final_estimates/PM25/pwpm_", i, "_13k_24jul1.csv"), header=TRUE)
  pwpm<-rename(pwpm, pwpm=pw_pm)
  mss <- which(is.na(pwpm$pwpm))
  inf <- which(is.infinite(pwpm$pwpm))
  zero <- which(pwpm$pwpm == 0)
  mss <- c(mss, inf, zero)  
  mss_list_ <- pwpm[mss, 2]
  mss_list <- c(mss_list, mss_list_)
}
mss_list <- unique(mss_list)
#data
pwpm<-read.csv("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Final_estimates/PM25/pwpm_2000_13k_24jul1.csv", header=TRUE)
pwpm<-rename(pwpm, pwpm=pw_pm)
pwpm <- pwpm[!(pwpm$ID %in% mss_list), ]
pwpm$year<-2000
years<-c(2001:2020)
for (i in years) {
  pwpm_<-read.csv(paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Final_estimates/PM25/pwpm_", i, "_13k_24jul1.csv"), header=TRUE)
  pwpm_<-rename(pwpm_, pwpm=pw_pm)
  pwpm_ <- pwpm_[!(pwpm_$ID %in% mss_list), ]
  pwpm_$year<-i
  pwpm <- rbind(pwpm, pwpm_)
}
pm<-merge(pwpm, x, by="ID")
a <- pm %>% group_by(year) %>% summarise(n=n(), pm25_13k_mean=mean(pwpm), pm25_13k_median=median(pwpm))
a$GBDSuperRegion<-'Global'
aa <- pm %>% 
  group_by(year, GBDSuperRegion) %>% 
  summarise(n=n(), pm25_13k_mean=mean(pwpm), pm25_13k_median=median(pwpm))
aaa<-rbind(aa, a)

showtext_auto()
pal <- c("darkgrey", "red", "olivedrab2", "mediumseagreen", "mediumpurple", "skyblue1", "darkorange", "lightpink1")
font_add_google("Lato")

pdf(file = paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Study/Adv_Sci/Co-authors/Figures/Figure2_PM25.pdf"),
    width     = 14,
    height    = 10
)
par(mar = c(1, 2, 2, 2))
ggplot(aaa, aes(x=year, y=pm25_13k_mean, group=GBDSuperRegion, color=GBDSuperRegion, linetype=GBDSuperRegion)) +
  geom_rect(data = data.frame(xmin = 2000, xmax = 2020), #setting background color for 2000-2020
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            fill = "grey98",
            alpha = 1, inherit.aes = FALSE) +
  geom_segment(
    data = tibble(y = seq(0, 70, by = 10), x1 = 2000, x2 = 2020),
    aes(x = x1, xend = x2, y = y, yend = y),
    inherit.aes = FALSE,
    color = "grey91",
    size = .6
  ) +
  scale_x_continuous(
    expand = c(0, 0),
    limits = c(2000, 2021), 
    breaks = seq(2000, 2020, by = 5)) +
  scale_y_continuous(
    expand = c(0, 0),
    breaks = seq(0, 70, by = 10),  
    labels = seq(0, 70, by = 10)) +
  geom_vline(
    xintercept = seq(2000, 2020, by = 5),
    color = "grey91", 
    size = .6)  +
  
  geom_line(linewidth = 1.7, 
            alpha = 1.5) +
  labs(x="Year", y=bquote(paste(PM[2.5] * " (\u03BCg/m³)")), title="") +
  theme_minimal() +
  theme(text=element_text(size=15)) +
  scale_linetype_manual(values = c("solid", "solid", "solid", "solid", "solid", "solid", "solid", "solid"), name="GBD super region") +
  scale_color_manual(values=pal, name="GBD super region") +
  theme(legend.position = "none", panel.grid = element_blank(),
        axis.text = element_text(color = "black"), 
        plot.margin = margin(0, 0, 0, 0),
        axis.title = element_text(color = "black"),
        axis.title.x = element_text(size=25, margin = margin(20,0,15,0), hjust=0.5),
        axis.title.y = element_text(size=25, margin = margin(0,15,0,15)),
        axis.text.x = element_text(size = 22, margin = margin(t = 20)),
        axis.text.y = element_text(size = 22, margin = margin(r = 20)))
dev.off()

#NO2
#extract final sample: cities with whole time-series
pwno2<-read.csv("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Final_estimates/NO2/pwno2_2005_13k_24jul12.csv", header=TRUE)
pwno2 <- pwno2[, -1]
pwno2<-rename(pwno2, pwno2=pw_no2)
mss <- which(is.na(pwno2$pwno2))
inf <- which(is.infinite(pwno2$pwno2))
zero <- which(pwno2$pwno2 == 0)
mss <- c(mss, inf, zero)
mss_list <- pwno2[mss, 1]
years<-c(2006:2020)
for (i in years) {
  pwno2<-read.csv(paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Final_estimates/NO2/pwno2_", i, "_13k_24jul12.csv"), header=TRUE)
  pwno2 <- pwno2[, -1]
  pwno2<-rename(pwno2, pwno2=pw_no2)
  mss <- which(is.na(pwno2$pwno2))
  inf <- which(is.infinite(pwno2$pwno2))
  zero <- which(pwno2$pwno2 == 0)
  mss <- c(mss, inf, zero)
  mss_list_ <- pwno2[mss, 1]
  mss_list <- c(mss_list, mss_list_)
}
mss_list <- unique(mss_list)
#data
pwno2<-read.csv("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Final_estimates/NO2/pwno2_2005_13k_24jul12.csv", header=TRUE)
pwno2 <- pwno2[, -1]
pwno2<-rename(pwno2, pwno2=pw_no2)
pwno2 <- pwno2[!(pwno2$ID %in% mss_list), ]
pwno2$year<-2005
years<-c(2006:2020)
for (i in years) {
  pwno2_<-read.csv(paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Final_estimates/NO2/pwno2_", i, "_13k_24jul12.csv"), header=TRUE)
  pwno2_ <- pwno2_[, -1]
  pwno2_<-rename(pwno2_, pwno2=pw_no2)
  pwno2_ <- pwno2_[!(pwno2_$ID %in% mss_list), ]
  pwno2_$year<-i
  pwno2<-rbind(pwno2, pwno2_)
}
no2<-merge(pwno2, x, by="ID")
a <- no2 %>% group_by(year) %>% summarise(n=n(), no2_13k_mean=mean(pwno2), no2_13k_median=median(pwno2))
a$GBDSuperRegion<-'Global'
aa <- no2 %>% 
  group_by(year, GBDSuperRegion) %>% 
  summarise(n=n(), no2_13k_mean=mean(pwno2), no2_13k_median=median(pwno2))
aaa<-rbind(aa, a)

showtext_auto()
pal <- c("darkgrey", "red", "olivedrab2", "mediumseagreen", "mediumpurple", "skyblue1", "darkorange", "lightpink1")
font_add_google("Lato")

pdf(file = paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Study/Adv_Sci/Co-authors/Figures/Figure2_NO2.pdf"),
    width     = 14,
    height    = 10
)
par(mar = c(1, 2, 2, 2))
ggplot(aaa, aes(x=year, y=no2_13k_mean, group=GBDSuperRegion, color=GBDSuperRegion, linetype=GBDSuperRegion)) +
  geom_rect(data = data.frame(xmin = 2000, xmax = 2020), #setting background color for 2000-2020
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            fill = "grey98",
            alpha = 1, inherit.aes = FALSE) +
  geom_segment(
    data = tibble(y = seq(0, 12, by = 2), x1 = 2000, x2 = 2020),
    aes(x = x1, xend = x2, y = y, yend = y),
    inherit.aes = FALSE,
    color = "grey91",
    size = .6
  ) +
  scale_x_continuous(
    expand = c(0, 0),
    limits = c(2000, 2021), 
    breaks = seq(2000, 2020, by = 5)) +
  scale_y_continuous(
    expand = c(0, 0),
    breaks = seq(0, 12, by = 2),  
    labels = seq(0, 12, by = 2)) +
  geom_vline(
    xintercept = seq(2000, 2020, by = 5),
    color = "grey91", 
    size = .6)  +
  
  geom_line(linewidth = 1.7, 
            alpha = 1.5) +
  labs(x="Year", y=bquote(paste(NO[2] * " (ppb)")), title="") +
  theme_minimal() +
  theme(text=element_text(size=15)) +
  scale_linetype_manual(values = c("solid", "solid", "solid", "solid", "solid", "solid", "solid", "solid"), name="GBD super region") +
  scale_color_manual(values=pal, name="GBD super region") +
  theme(legend.position = "none", panel.grid = element_blank(),
        axis.text = element_text(color = "black"), 
        plot.margin = margin(0, 0, 0, 0),
        axis.title = element_text(color = "black"),
        axis.title.x = element_text(size=25, margin = margin(20,0,15,0), hjust=0.5),
        axis.title.y = element_text(size=25, margin = margin(0,15,0,15)),
        axis.text.x = element_text(size = 22, margin = margin(t = 20)),
        axis.text.y = element_text(size = 22, margin = margin(r = 20))) 
dev.off()

#O3
#extract final sample: cities with whole time-series
pwo3<-read.csv("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Final_estimates/O3/pwo3_2000_13k_24jul1.csv", header=TRUE)
pwo3<-rename(pwo3, pwo3=pw_o3)
mss <- which(is.na(pwo3$pwo3))
inf <- which(is.infinite(pwo3$pwo3))
zero <- which(pwo3$pwo3 == 0)
mss <- c(mss, inf, zero)
mss_list <- pwo3[mss, 2]
years<-c(2001:2020)
for (i in years) {
  pwo3<-read.csv(paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Final_estimates/O3/pwo3_", i, "_13k_24jul1.csv"), header=TRUE)  
  pwo3<-rename(pwo3, pwo3=pw_o3)
  mss <- which(is.na(pwo3$pwo3))
  inf <- which(is.infinite(pwo3$pwo3))
  zero <- which(pwo3$pwo3 == 0)
  mss <- c(mss, inf, zero)
  mss_list_ <- pwo3[mss, 2]
  mss_list <- c(mss_list, mss_list_)
}
mss_list <- unique(mss_list)
#data
pwo3<-read.csv("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Final_estimates/O3/pwo3_2000_13k_24jul1.csv", header=TRUE)
pwo3<-rename(pwo3, pwo3=pw_o3)
pwo3 <- pwo3[!(pwo3$ID %in% mss_list), ]
pwo3$year <- 2000
years <- c(2001:2020)
for (i in years) {
  pwo3_<-read.csv(paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Final_estimates/O3/pwo3_", i, "_13k_24jul1.csv"), header=TRUE)
  pwo3_<-rename(pwo3_, pwo3=pw_o3)
  pwo3_ <- pwo3_[!(pwo3_$ID %in% mss_list), ]
  pwo3_$year<-i
  pwo3<-rbind(pwo3, pwo3_)
}
o3<-merge(pwo3, x, by="ID")
a <- o3 %>% group_by(year) %>% summarise(n=n(), o3_13k_mean=mean(pwo3), o3_13k_median=median(pwo3))
a$GBDSuperRegion<-'Global'
aa <- o3 %>% 
  group_by(year, GBDSuperRegion) %>% 
  summarise(n=n(), o3_13k_mean=mean(pwo3), o3_13k_median=median(pwo3))
aaa<-rbind(aa, a)

showtext_auto()
pal <- c("darkgrey", "red", "olivedrab2", "mediumseagreen", "mediumpurple", "skyblue1", "darkorange", "lightpink1")
font_add_google("Lato")

pdf(file = paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Study/Adv_Sci/Co-authors/Figures/Figure2_O3.pdf"),
    width     = 14,
    height    = 10
)
par(mar = c(1, 2, 2, 2))
ggplot(aaa, aes(x=year, y=o3_13k_mean, group=GBDSuperRegion, color=GBDSuperRegion, linetype=GBDSuperRegion)) +
  geom_rect(data = data.frame(xmin = 2000, xmax = 2020), #setting background color for 2000-2020
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            fill = "grey98",
            alpha = 1, inherit.aes = FALSE) +
  geom_segment(
    data = tibble(y = seq(20, 80, by = 10), x1 = 2000, x2 = 2020),
    aes(x = x1, xend = x2, y = y, yend = y),
    inherit.aes = FALSE,
    color = "grey91",
    size = .6
  ) +
  scale_x_continuous(
    expand = c(0, 0),
    limits = c(2000, 2021), 
    breaks = seq(2000, 2020, by = 5)) +
  scale_y_continuous(
    expand = c(0, 0),
    breaks = seq(20, 80, by = 10),  
    labels = seq(20, 80, by = 10)) +
  geom_vline(
    xintercept = seq(2000, 2020, by = 5),
    color = "grey91", 
    size = .6)  +
  
  geom_line(linewidth = 1.7, 
            alpha = 1.5) +
  labs(x="Year", y=bquote(paste(O[3] * " (ppb)")), title="") +
  theme_minimal() +
  theme(text=element_text(size=15)) +
  scale_linetype_manual(values = c("solid", "solid", "solid", "solid", "solid", "solid", "solid", "solid"), name="GBD super region") +
  scale_color_manual(values=pal, name="GBD super region") +
  theme(legend.position = "none", panel.grid = element_blank(),
        axis.text = element_text(color = "black"), 
        plot.margin = margin(0, 0, 0, 0),
        axis.title = element_text(color = "black"),
        axis.title.x = element_text(size=25, margin = margin(20,0,15,0), hjust=0.5),
        axis.title.y = element_text(size=25, margin = margin(0,15,0,15)),
        axis.text.x = element_text(size = 22, margin = margin(t = 20)),
        axis.text.y = element_text(size = 22, margin = margin(r = 20)))
dev.off()

#CO2
#extract final sample: cities with whole time-series
co2_per_cap <- read.csv("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Final_estimates/CO2/co2_per_capita_13k_2000.csv", header=TRUE)
mss <- which(is.na(co2_per_cap$co2_per_cap))
inf <- which(is.infinite(co2_per_cap$co2_per_cap))
zero <- which(co2_per_cap$co2_per_cap == 0)
mss <- c(mss, inf, zero)
mss_list <- co2_per_cap[mss, 2]
years <- c(2001:2020)
for (i in years) {
  co2_per_cap<-read.csv(paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Final_estimates/CO2/co2_per_capita_13k_", i, ".csv"), header=TRUE)
  mss <- which(is.na(co2_per_cap$co2_per_cap))
  inf <- which(is.infinite(co2_per_cap$co2_per_cap))
  zero <- which(co2_per_cap$co2_per_cap == 0)
  mss <- c(mss, inf, zero)
  mss_list_ <- co2_per_cap[mss, 2]
  mss_list <- c(mss_list, mss_list_)
}
mss_list <- unique(mss_list)
#data
co2_per_cap <- read.csv("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Final_estimates/CO2/co2_per_capita_13k_2000.csv", header=TRUE)
co2_per_cap <- co2_per_cap[!(co2_per_cap$ID %in% mss_list), ]
co2_per_cap$year <- 2000
years <- c(2001:2020)
for (i in years) {
  co2_per_cap_<-read.csv(paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Final_estimates/CO2/co2_per_capita_13k_", i, ".csv"), header=TRUE)
  co2_per_cap_$co2_per_cap <- ifelse(co2_per_cap_$co2_per_cap=="Inf", NA, co2_per_cap_$co2_per_cap)
  co2_per_cap_ <- co2_per_cap_[!(co2_per_cap_$ID %in% mss_list), ]
  co2_per_cap_$year <- i
  co2_per_cap<-rbind(co2_per_cap, co2_per_cap_)
}
co2<-merge(co2_per_cap, x, by="ID")
a <- co2 %>% group_by(year) %>% summarise(n=n(), co2_13k_mean=mean(co2_per_cap), co2_13k_median=median(co2_per_cap))
a$GBDSuperRegion<-'Global'
aa <- co2 %>% 
  group_by(year, GBDSuperRegion) %>% 
  summarise(n=n(), co2_13k_mean=mean(co2_per_cap), co2_13k_median=median(co2_per_cap))
aaa<-rbind(aa, a)
aaa <- na.omit(aaa) #remove Kosovo, wihch is categorized as NA (not included in any GBD super-regions)

showtext_auto()
pal <- c("darkgrey", "red", "olivedrab2", "mediumseagreen", "mediumpurple", "skyblue1", "darkorange", "lightpink1")
font_add_google("Lato")

pdf(file = paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Study/Adv_Sci/Co-authors/Figures/Figure2_CO2.pdf"),
    width     = 14,
    height    = 10
)
par(mar = c(1, 2, 2, 2))
ggplot(aaa, aes(x=year, y=co2_13k_median, group=GBDSuperRegion, color=GBDSuperRegion, linetype=GBDSuperRegion)) +
  geom_rect(data = data.frame(xmin = 2000, xmax = 2020), #setting background color for 2000-2020
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            fill = "grey98",
            alpha = 1, inherit.aes = FALSE) +
  geom_segment(
    data = tibble(y = seq(0, 8, by = 2), x1 = 2000, x2 = 2020),
    aes(x = x1, xend = x2, y = y, yend = y),
    inherit.aes = FALSE,
    color = "grey91",
    size = .6
  ) +
  scale_x_continuous(
    expand = c(0, 0),
    limits = c(2000, 2021), 
    breaks = seq(2000, 2020, by = 5)) +
  scale_y_continuous(
    expand = c(0, 0),
    breaks = seq(0, 8, by = 2),  
    labels = seq(0, 8, by = 2)) +
  geom_vline(
    xintercept = seq(2000, 2020, by = 5),
    color = "grey91", 
    size = .6)  +
  
  geom_line(linewidth = 1.7, 
            alpha = 1.5) +
  labs(x="Year", y=bquote(paste(FFCO[2] * " per capita (metric tons)")), title="") +
  theme_minimal() +
  theme(text=element_text(size=15)) +
  scale_linetype_manual(values = c("solid", "solid", "solid", "solid", "solid", "solid", "solid", "solid"), name="GBD super region") +
  scale_color_manual(values=pal, name="GBD super region") +
  theme(legend.position = "none", panel.grid = element_blank(),
        axis.text = element_text(color = "black"), 
        plot.margin = margin(0, 0, 0, 0),
        axis.title = element_text(color = "black"),
        axis.title.x = element_text(size = 25, margin = margin(20,0,15,0), hjust=0.5),
        axis.title.y = element_text(size = 25, margin = margin(0,15,0,15)),
        axis.text.x = element_text(size = 22, margin = margin(t = 20)),
        axis.text.y = element_text(size = 22, margin = margin(r = 20)))
dev.off()



############################################
##### Spiderweb plot: Figure 3 #############
############################################
#data
x<-read.csv("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/etc/GBDsuperregion_final_final.csv") 
x<-unique(x)
city.shp <- st_read("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/NO2/Inputs/hdc_naming_warp.shp")
city.shp <- rename(city.shp, id='ID_HDC_G0')

#PM25
#extract final sample: cities with whole time-series
pwpm<-read.csv("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Final_estimates/PM25/pwpm_2000_13k_24jul1.csv", header=TRUE)
pwpm<-rename(pwpm, pwpm=pw_pm)
mss <- which(is.na(pwpm$pwpm))
inf <- which(is.infinite(pwpm$pwpm))
zero <- which(pwpm$pwpm == 0)
mss <- c(mss, inf, zero)
mss_list <- pwpm[mss, 2]
years<-c(2001:2020)
for (i in years) {
  pwpm<-read.csv(paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Final_estimates/PM25/pwpm_", i, "_13k_24jul1.csv"), header=TRUE)
  pwpm<-rename(pwpm, pwpm=pw_pm)
  mss <- which(is.na(pwpm$pwpm))
  inf <- which(is.infinite(pwpm$pwpm))
  zero <- which(pwpm$pwpm == 0)
  mss <- c(mss, inf, zero)  
  mss_list_ <- pwpm[mss, 2]
  mss_list <- c(mss_list, mss_list_)
}
mss_list <- unique(mss_list)
mss_list_pm <- mss_list
#data
pwpm<-read.csv("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Final_estimates/PM25/pwpm_2000_13k_24jul1.csv", header=TRUE)
pwpm<-rename(pwpm, pwpm=pw_pm)
pwpm <- pwpm[!(pwpm$ID %in% mss_list), ]
pwpm$year<-2000
years<-c(2001:2020)
for (i in years) {
  pwpm_<-read.csv(paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Final_estimates/PM25/pwpm_", i, "_13k_24jul1.csv"), header=TRUE)
  pwpm_<-rename(pwpm_, pwpm=pw_pm)
  pwpm_ <- pwpm_[!(pwpm_$ID %in% mss_list), ]
  pwpm_$year<-i
  pwpm <- rbind(pwpm, pwpm_)
}
pm<-merge(pwpm, x, by="ID")

#NO2
#extract final sample: cities with whole time-series
pwno2<-read.csv("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Final_estimates/NO2/pwno2_2005_13k_24jul12.csv", header=TRUE)
pwno2 <- pwno2[, -1]
pwno2<-rename(pwno2, pwno2=pw_no2)
mss <- which(is.na(pwno2$pwno2))
inf <- which(is.infinite(pwno2$pwno2))
zero <- which(pwno2$pwno2 == 0)
mss <- c(mss, inf, zero)
mss_list <- pwno2[mss, 1]
years<-c(2006:2020)
for (i in years) {
  pwno2<-read.csv(paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Final_estimates/NO2/pwno2_", i, "_13k_24jul12.csv"), header=TRUE)
  pwno2 <- pwno2[, -1]
  pwno2<-rename(pwno2, pwno2=pw_no2)
  mss <- which(is.na(pwno2$pwno2))
  inf <- which(is.infinite(pwno2$pwno2))
  zero <- which(pwno2$pwno2 == 0)
  mss <- c(mss, inf, zero)
  mss_list_ <- pwno2[mss, 1]
  mss_list <- c(mss_list, mss_list_)
}
mss_list <- unique(mss_list)
mss_list_no <- mss_list

#data
pwno2<-read.csv("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Final_estimates/NO2/pwno2_2005_13k_24jul12.csv", header=TRUE)
pwno2 <- pwno2[, -1]
pwno2<-rename(pwno2, pwno2=pw_no2)
pwno2 <- pwno2[!(pwno2$ID %in% mss_list), ]
pwno2$year<-2005
years<-c(2006:2020)
for (i in years) {
  pwno2_<-read.csv(paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Final_estimates/NO2/pwno2_", i, "_13k_24jul12.csv"), header=TRUE)
  pwno2_ <- pwno2_[, -1]
  pwno2_<-rename(pwno2_, pwno2=pw_no2)
  pwno2_ <- pwno2_[!(pwno2_$ID %in% mss_list), ]
  pwno2_$year<-i
  pwno2<-rbind(pwno2, pwno2_)
}
no2<-merge(pwno2, x, by="ID")

#O3
#extract final sample: cities with whole time-series
pwo3<-read.csv("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Final_estimates/O3/pwo3_2000_13k_24jul1.csv", header=TRUE)
pwo3<-rename(pwo3, pwo3=pw_o3)
mss <- which(is.na(pwo3$pwo3))
inf <- which(is.infinite(pwo3$pwo3))
zero <- which(pwo3$pwo3 == 0)
mss <- c(mss, inf, zero)
mss_list <- pwo3[mss, 2]
years<-c(2001:2020)
for (i in years) {
  pwo3<-read.csv(paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Final_estimates/O3/pwo3_", i, "_13k_24jul1.csv"), header=TRUE)  
  pwo3<-rename(pwo3, pwo3=pw_o3)
  mss <- which(is.na(pwo3$pwo3))
  inf <- which(is.infinite(pwo3$pwo3))
  zero <- which(pwo3$pwo3 == 0)
  mss <- c(mss, inf, zero)
  mss_list_ <- pwo3[mss, 2]
  mss_list <- c(mss_list, mss_list_)
}
mss_list <- unique(mss_list)
mss_list_o <- mss_list

#data
pwo3<-read.csv("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Final_estimates/O3/pwo3_2000_13k_24jul1.csv", header=TRUE)
pwo3<-rename(pwo3, pwo3=pw_o3)
pwo3 <- pwo3[!(pwo3$ID %in% mss_list), ]
pwo3$year <- 2000
years <- c(2001:2020)
for (i in years) {
  pwo3_<-read.csv(paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Final_estimates/O3/pwo3_", i, "_13k_24jul1.csv"), header=TRUE)
  pwo3_<-rename(pwo3_, pwo3=pw_o3)
  pwo3_ <- pwo3_[!(pwo3_$ID %in% mss_list), ]
  pwo3_$year<-i
  pwo3<-rbind(pwo3, pwo3_)
}
o3<-merge(pwo3, x, by="ID")

#CO2
#extract final sample: cities with whole time-series
co2_per_cap <- read.csv("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Final_estimates/CO2/co2_per_capita_13k_2000.csv", header=TRUE)
mss <- which(is.na(co2_per_cap$co2_per_cap))
inf <- which(is.infinite(co2_per_cap$co2_per_cap))
zero <- which(co2_per_cap$co2_per_cap == 0)
mss <- c(mss, inf, zero)
mss_list <- co2_per_cap[mss, 2]
years <- c(2001:2020)
for (i in years) {
  co2_per_cap<-read.csv(paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Final_estimates/CO2/co2_per_capita_13k_", i, ".csv"), header=TRUE)
  mss <- which(is.na(co2_per_cap$co2_per_cap))
  inf <- which(is.infinite(co2_per_cap$co2_per_cap))
  zero <- which(co2_per_cap$co2_per_cap == 0)
  mss <- c(mss, inf, zero)
  mss_list_ <- co2_per_cap[mss, 2]
  mss_list <- c(mss_list, mss_list_)
}
mss_list <- unique(mss_list)
mss_list_co <- mss_list

#data
co2_per_cap <- read.csv("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Final_estimates/CO2/co2_per_capita_13k_2000.csv", header=TRUE)
co2_per_cap <- co2_per_cap[!(co2_per_cap$ID %in% mss_list), ]
co2_per_cap$year <- 2000
years <- c(2001:2020)
for (i in years) {
  co2_per_cap_<-read.csv(paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Final_estimates/CO2/co2_per_capita_13k_", i, ".csv"), header=TRUE)
  co2_per_cap_$co2_per_cap <- ifelse(co2_per_cap_$co2_per_cap=="Inf", NA, co2_per_cap_$co2_per_cap)
  co2_per_cap_ <- co2_per_cap_[!(co2_per_cap_$ID %in% mss_list), ]
  co2_per_cap_$year <- i
  co2_per_cap<-rbind(co2_per_cap, co2_per_cap_)
}
co2<-merge(co2_per_cap, x, by="ID")

#final samples
mss_list <- c(mss_list_pm, mss_list_no, mss_list_o, mss_list_co)
mss_list <- unique(mss_list)
list <- setdiff(city.shp$id, mss_list)
kosovo <- c(3297, 3309, 3322, 3323, 3334, 3338, 3344) #not included in any GBD superregion
list <- setdiff(list, kosovo)

#Customize radarchart2 function to have the dot size option
radarchart2 <- function(df, axistype = 0, seg = 4, pty = 16, pcol = 1:8, plty = 1:6, 
                        plwd = 1, pdensity = NULL, pangle = 45, pfcol = NA, cglty = 3, 
                        cglwd = 1, cglcol = "navy", axislabcol = "blue", vlabcol = "black", title = "", 
                        maxmin = TRUE, na.itp = TRUE, centerzero = FALSE, vlabels = NULL, 
                        vlcex = NULL, caxislabels = NULL, calcex = NULL, paxislabels = NULL, 
                        palcex = NULL, psize = 1, ...) {
  
  if (!is.data.frame(df)) {
    cat("The data must be given as a dataframe.\n")
    return()
  }
  
  if ((n <- length(df)) < 3) {
    cat("The number of variables must be 3 or more.\n")
    return()
  }
  
  if (maxmin == FALSE) {
    dfmax <- apply(df, 2, max)
    dfmin <- apply(df, 2, min)
    df <- rbind(dfmax, dfmin, df)
  }
  
  plot(c(-1.2, 1.2), c(-1.2, 1.2), type = "n", frame.plot = FALSE, 
       axes = FALSE, xlab = "", ylab = "", main = title, asp = 1, ...)
  
  theta <- seq(90, 450, length = n + 1) * pi/180
  theta <- theta[1:n]
  xx <- cos(theta)
  yy <- sin(theta)
  CGap <- ifelse(centerzero, 0, 1)
  
  for (i in 0:seg) {
    polygon(xx * (i + CGap)/(seg + CGap), yy * (i + CGap)/(seg + 
                                                             CGap), lty = cglty, lwd = cglwd, border = cglcol)
    
    if (axistype == 1 | axistype == 3) 
      CAXISLABELS <- paste(i/seg * 100, "(%)")
    
    if (axistype == 4 | axistype == 5) 
      CAXISLABELS <- sprintf("%3.2f", i/seg)
    
    if (!is.null(caxislabels) & (i < length(caxislabels))) 
      CAXISLABELS <- caxislabels[i + 1]
    
    if (axistype == 1 | axistype == 3 | axistype == 4 | 
        axistype == 5) {
      if (is.null(calcex)) 
        text(-0.05, (i + CGap)/(seg + CGap), CAXISLABELS, 
             col = axislabcol)
      else text(-0.05, (i + CGap)/(seg + CGap), CAXISLABELS, 
                col = axislabcol, cex = calcex)
    }
  }
  
  if (centerzero) {
    arrows(0, 0, xx * 1, yy * 1, lwd = cglwd, lty = cglty, 
           length = 0, col = cglcol)
  }
  else {
    arrows(xx/(seg + CGap), yy/(seg + CGap), xx * 1, yy * 
             1, lwd = cglwd, lty = cglty, length = 0, col = cglcol)
  }
  
  PAXISLABELS <- df[1, 1:n]
  
  if (!is.null(paxislabels)) 
    PAXISLABELS <- paxislabels
  
  if (axistype == 2 | axistype == 3 | axistype == 5) {
    if (is.null(palcex)) 
      text(xx[1:n], yy[1:n], PAXISLABELS, col = axislabcol)
    else text(xx[1:n], yy[1:n], PAXISLABELS, col = axislabcol, 
              cex = palcex)
  }
  
  VLABELS <- colnames(df)
  
  if (!is.null(vlabels)) 
    VLABELS <- vlabels
  
  if (is.null(vlcex)) 
    text(xx * 1.2, yy * 1.2, VLABELS, col = vlabcol)
  else text(xx * 1.2, yy * 1.2, VLABELS, cex = vlcex, col = vlabcol)
  
  series <- length(df[[1]])
  SX <- series - 2
  
  if (length(pty) < SX) {
    ptys <- rep(pty, SX)
  }
  else {
    ptys <- pty
  }
  
  if (length(pcol) < SX) {
    pcols <- rep(pcol, SX)
  }
  else {
    pcols <- pcol
  }
  
  if (length(plty) < SX) {
    pltys <- rep(plty, SX)
  }
  else {
    pltys <- plty
  }
  
  if (length(plwd) < SX) {
    plwds <- rep(plwd, SX)
  }
  else {
    plwds <- plwd
  }
  
  if (length(pdensity) < SX) {
    pdensities <- rep(pdensity, SX)
  }
  else {
    pdensities <- pdensity
  }
  
  if (length(pangle) < SX) {
    pangles <- rep(pangle, SX)
  }
  else {
    pangles <- pangle
  }
  
  if (length(pfcol) < SX) {
    pfcols <- rep(pfcol, SX)
  }
  else {
    pfcols <- pfcol
  }
  
  for (i in 3:series) {
    xxs <- xx
    yys <- yy
    scale <- CGap/(seg + CGap) + (df[i, ] - df[2, ])/(df[1, 
    ] - df[2, ]) * seg/(seg + CGap)
    
    if (sum(!is.na(df[i, ])) < 3) {
      cat(sprintf("[DATA NOT ENOUGH] at %d\n%g\n", i, 
                  df[i, ]))
    }
    else {
      for (j in 1:n) {
        if (is.na(df[i, j])) {
          if (na.itp) {
            left <- ifelse(j > 1, j - 1, n)
            while (is.na(df[i, left])) {
              left <- ifelse(left > 1, left - 1, n)
            }
            right <- ifelse(j < n, j + 1, 1)
            while (is.na(df[i, right])) {
              right <- ifelse(right < n, right + 1, 
                              1)
            }
            xxleft <- xx[left] * CGap/(seg + CGap) + 
              xx[left] * (df[i, left] - df[2, left])/(df[1, 
                                                         left] - df[2, left]) * seg/(seg + CGap)
            yyleft <- yy[left] * CGap/(seg + CGap) + 
              yy[left] * (df[i, left] - df[2, left])/(df[1, 
                                                         left] - df[2, left]) * seg/(seg + CGap)
            xxright <- xx[right] * CGap/(seg + CGap) + 
              xx[right] * (df[i, right] - df[2, right])/(df[1, 
                                                            right] - df[2, right]) * seg/(seg + 
                                                                                            CGap)
            yyright <- yy[right] * CGap/(seg + CGap) + 
              yy[right] * (df[i, right] - df[2, right])/(df[1, 
                                                            right] - df[2, right]) * seg/(seg + 
                                                                                            CGap)
            if (xxleft > xxright) {
              xxtmp <- xxleft
              yytmp <- yyleft
              xxleft <- xxright
              yyleft <- yyright
              xxright <- xxtmp
              yyright <- yytmp
            }
            xxs[j] <- xx[j] * (yyleft * xxright - yyright * 
                                 xxleft)/(yy[j] * (xxright - xxleft) - 
                                            xx[j] * (yyright - yyleft))
            yys[j] <- (yy[j]/xx[j]) * xxs[j]
          }
          else {
            xxs[j] <- 0
            yys[j] <- 0
          }
        }
        else {
          xxs[j] <- xx[j] * CGap/(seg + CGap) + xx[j] * 
            (df[i, j] - df[2, j])/(df[1, j] - df[2, 
                                                 j]) * seg/(seg + CGap)
          yys[j] <- yy[j] * CGap/(seg + CGap) + yy[j] * 
            (df[i, j] - df[2, j])/(df[1, j] - df[2, 
                                                 j]) * seg/(seg + CGap)
        }
      }
      
      if (is.null(pdensities)) {
        polygon(xxs, yys, lty = pltys[i - 2], lwd = plwds[i - 
                                                            2], border = pcols[i - 2], col = pfcols[i - 
                                                                                                      2])
      }
      else {
        polygon(xxs, yys, lty = pltys[i - 2], lwd = plwds[i - 
                                                            2], border = pcols[i - 2], density = pdensities[i - 
                                                                                                              2], angle = pangles[i - 2], col = pfcols[i - 
                                                                                                                                                         2])
      }
      
      points(xx * scale, yy * scale, pch = ptys[i - 2], 
             col = pcols[i - 2], cex = psize)  # Customize dot size
    }
  }
}




###################################
######## BY GBD: Figure 3 #########
###################################
#seed (empty frame)
column_names <- c("PM2.5", "NO2", "O3", "CO2")
column_types <- c("numeric", "numeric", "numeric", "numeric")
spider2 <- data.frame(matrix(vector(), nrow = 0, ncol = length(column_names)))
names(spider2) <- column_names

for (i in list) {
  #.  i<-950   #860: DC, 950: NYC, 12445: Seoul
  pp <- subset(city.shp, city.shp$ID_HDC_G0==i)
  city_name <- pp$NAME_MAIN
  
  #percent change from 2005 to 2019
  #pm
  x1 <- subset(pm, ID==i)
  x1 <- arrange(x1, year) #sort
  x1 <- subset(x1, (year>=2005)&(year<=2019))
  a <- MannKendall(x1$pwpm) 
  aa <- a$sl
  xx1 <- -(mean(x1[c(1:3),5])- mean(x1[c(13:15),5]))/mean(x1[c(1:3),5])*100
  #no2
  x2 <- subset(no2, ID==i)
  x2 <- arrange(x2, year) #sort
  x2 <- subset(x2, (year>=2005)&(year<=2019))
  xx2 <- -(mean(x2[c(1:3),4])- mean(x2[c(13:15),4]))/mean(x2[c(1:3),4])*100
  #o3
  x3 <- subset(o3, ID==i)
  x3 <- arrange(x3, year) #sort
  x3 <- subset(x3, (year>=2005)&(year<=2019))
  xx3 <- -(mean(x3[c(1:3),5])- mean(x3[c(13:15),5]))/mean(x3[c(1:3),5])*100
  #co2
  x4 <- subset(co2_per_cap, ID==i)
  x4 <- arrange(x4, year) #sort
  x4 <- subset(x4, (year>=2005)&(year<=2019))
  xx4 <- -(mean(x4[c(1:3),5])- mean(x4[c(13:15),5]))/mean(x4[c(1:3),5])*100
  
  i_combined <- as.data.frame(rbind(xx1, xx2, xx3, xx4))
  rownames(i_combined) <- NULL
  i_combined$ID <- i
  colnames(i_combined)[1] <- "final"
  
  
  #spiderweb plot
  spider2_ <- as.data.frame(t(i_combined))
  spider2_$V1 <- as.numeric(spider2_$V1)
  spider2_$V2 <- as.numeric(spider2_$V2)
  spider2_$V3 <- as.numeric(spider2_$V3)
  spider2_$V4 <- as.numeric(spider2_$V4)
  colnames(spider2_)[1] <- "PM2.5
"
  colnames(spider2_)[2] <- "NO2"
  colnames(spider2_)[3] <- "O3"
  colnames(spider2_)[4] <- "CO2"
  spider2_ <- spider2_[1, , drop=FALSE]
  rownames(spider2_) <- i
  
  spider2 <- rbind(spider2, spider2_)
}

#cleaning
spider2_ <- spider2
spider2_ <- apply(spider2_, 2, function(x) ifelse(is.infinite(x), NA, x)) #removing "Inf"
spider2_ <- na.omit(spider2_)
spider2_ <- as.data.frame(spider2_) #final: 12,550 cities

#GBD super-region
x<-read.csv("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/etc/GBDsuperregion_final_final.csv")
x <- na.omit(x)
#x<-read.csv("/Users/k_sy_n_macbook/Desktop/Dropbox/GWU/Lab/13000cities/etc/GBDsuperregion_final.csv")
list_ <- unique(x$GBDSuperRegion)


#TOTAL
#global mean
#y1 <- mean(spider2_$PM2.5)
#y2 <- mean(spider2_$NO2)
#y3 <- mean(spider2_$O3)
#y4 <- mean(spider2_$CO2)
yy1 <- subset(pm, ((year>=2005)&(year<=2019)) & (ID %in% list))
yy1 <- yy1 %>% group_by(year) %>% summarise(mean=mean(pwpm)) 
yy1 <- as.data.frame(yy1)
y1 <- -(mean(yy1[c(1:3),2])-mean(yy1[c(13:15),2]))/mean(yy1[c(1:3),2])*100
zz1 <- subset(pm, (year>=2005&year<=2019) & (ID %in% list))
zz1 <- zz1 %>% group_by(year) %>% summarise(mean=mean(pwpm)) 
zz1 <- arrange(zz1, year) #sort
a1 <- MannKendall(zz1$mean) 

yy2 <- subset(no2,((year>=2005)&(year<=2019)) & (ID %in% list))
yy2 <- yy2 %>% group_by(year) %>% summarise(mean=mean(pwno2)) 
yy2 <- as.data.frame(yy2)
y2 <- -(mean(yy2[c(1:3),2])-mean(yy2[c(13:15),2]))/mean(yy2[c(1:3),2])*100
zz2 <- subset(no2, (year>=2005&year<=2019) & (ID %in% list))
zz2 <- zz2 %>% group_by(year) %>% summarise(mean=mean(pwno2)) 
zz2 <- arrange(zz2, year) #sort
a2 <- MannKendall(zz2$mean) 

yy3 <- subset(o3, ((year>=2005)&(year<=2019)) & (ID %in% list))
yy3 <- yy3 %>% group_by(year) %>% summarise(mean=mean(pwo3)) 
yy3 <- as.data.frame(yy3)
y3 <- -(mean(yy3[c(1:3),2])-mean(yy3[c(13:15),2]))/mean(yy3[c(1:3),2])*100
zz3 <- subset(o3, (year>=2005&year<=2019) & (ID %in% list))
zz3 <- zz3 %>% group_by(year) %>% summarise(mean=mean(pwo3)) 
zz3 <- arrange(zz3, year) #sort
a3 <- MannKendall(zz3$mean) 

yy4 <- subset(co2_per_cap, ((year>=2005)&(year<=2019)) & (ID %in% list))
yy4 <- yy4 %>% group_by(year) %>% summarise(mean=median(co2_per_cap)) 
yy4 <- as.data.frame(yy4)
y4 <- -(mean(yy4[c(1:3),2])-mean(yy4[c(13:15),2]))/mean(yy4[c(1:3),2])*100
zz4 <- subset(co2_per_cap, (year>=2005&year<=2019) & (ID %in% list))
zz4 <- zz4 %>% group_by(year) %>% summarise(mean=median(co2_per_cap)) 
zz4 <- arrange(zz4, year) #sort
a4 <- MannKendall(zz4$mean) 

total <- data.frame(y1, y2, y3, y4)
names(total) <- names(spider2_)
spider_total <- rbind(spider2_, total)


#text (total)
pm_total <- paste0(format(round(y1, 0), big.mark=","), "%")
no2_total <- paste0(format(round(y2, 0), big.mark=","), "%")
o3_total <- paste0(format(round(y3, 0), big.mark=","), "%")
co2_total <- paste0(format(round(y4, 0), big.mark=","), "%")

#text (total) position
pm_total_position_x <- 0
pm_total_position_y <- (y1/200 + 0.5)+0.1
pm_total_position_y <- as.numeric(ifelse(pm_total_position_y>=1.1, 1, pm_total_position_y))

no2_total_position_x <- -(y2/200 + 0.5)-0.2
no2_total_position_x <- as.numeric(ifelse(no2_total_position_x>=1.1, 1, no2_total_position_x))
no2_total_position_y <- 0.05

o3_total_position_x <- 0
o3_total_position_y <- -(y3/200 + 0.5)-0.2
o3_total_position_y <- as.numeric(ifelse(o3_total_position_y>=1.1, 1, o3_total_position_y))

co2_total_position_x <- (y4/200 + 0.5)+0.1
co2_total_position_x <- as.numeric(ifelse(co2_total_position_x>=1.1, 1, co2_total_position_x))
co2_total_position_y <- 0.05

#marking inf, ~-100, 100~
#values
spider_total[,1] <- ifelse((spider_total[,1]>100)|(spider_total[,1]==Inf), 100, spider_total[,1])
spider_total[,1] <- ifelse(spider_total[,1]< -100, -100, spider_total[,1])

spider_total[,2] <- ifelse((spider_total[,2]>100)|(spider_total[,2]==Inf), 100, spider_total[,2])
spider_total[,2] <- ifelse(spider_total[,2]< -100, -100, spider_total[,2])

spider_total[,3] <- ifelse((spider_total[,3]>100)|(spider_total[,3]==Inf), 100, spider_total[,3])
spider_total[,3] <- ifelse(spider_total[,3]< -100, -100, spider_total[,3])

spider_total[,4] <- ifelse((spider_total[,4]>100)|(spider_total[,4]==Inf), 100, spider_total[,4])
spider_total[,4] <- ifelse(spider_total[,4]< -100, -100, spider_total[,4])

max <- c(100,100,100,100)
min <- c(-100,-100,-100,-100)
z <- rbind(max, min, spider_total, total)

#significance
z[nrow(z),1] <- ifelse(a1$sl>0.05, Inf, z[nrow(z),1])
z[nrow(z),2] <- ifelse(a2$sl>0.05, Inf, z[nrow(z),2])
z[nrow(z),3] <- ifelse(a3$sl>0.05, Inf, z[nrow(z),3])
z[nrow(z),4] <- ifelse(a4$sl>0.05, Inf, z[nrow(z),4])

#population change
#create total pop data using pollutants data
pop<-read.csv("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Final_estimates/CO2/pop_13k_2000.csv", header=TRUE)
pop$year <- 2000
for (n in c(2001:2020)) {
  pop_<-read.csv(paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Final_estimates/CO2/pop_13k_", n, ".csv"), header=TRUE)
  pop_$year <- n
  pop <- rbind(pop, pop_)
}
pop <- subset(pop, ID %in% row_number(spider2))
pop <- pop[, c("ID", "year", "pop")]
#annual total pop
popx <- pop %>% group_by(year) %>% summarize(pop = sum(pop))
pop_05 <- subset(popx, popx$year %in% c(2005:2007))
pop_19 <- subset(popx, popx$year %in% c(2017:2019))
#pop changes from (2005-2007) to (2017-2019)
pop_chg <- ((mean(pop_19$pop)-mean(pop_05$pop))/mean(pop_05$pop)) * 100
pop_chg <- round(pop_chg, digits=0)
sign <- ifelse(pop_chg>=0, "+", "")
pop_chg_text <- paste0(sign, pop_chg, "%")

icon <- readPNG("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/etc/Picture1.png")  
image_grob <- rasterGrob(icon, interpolate = TRUE, x = 0.16, y = 0.179, width = 0.1, height = 0.1)

#plot
n_rows <- nrow(spider_total)+1
line_colors <- rep("azure3", n_rows) #line color
line_colors[n_rows-1] <- "#003366"
line_colors[n_rows] <- "brown2"
line_alphas <- rep(0.1, n_rows) #line alpha
line_alphas[n_rows-1] <- 1
line_alphas[n_rows] <- 1
#line_alphas[n_rows] <- 0
line_width <- rep(0.5, n_rows) #line width
line_width[n_rows-1] <- 7
dot_type <- rep(32, n_rows) 
dot_type[n_rows] <- 19

pm_axis_title <- expression(bold("PM"["2.5"]))
no2_axis_title <- expression(bold("NO"["2"]))
o3_axis_title <- expression(bold("O"["3"]))
co2_axis_title <- expression(bold("FFCO"["2"]))
axisnames <- c(pm_axis_title, no2_axis_title, o3_axis_title, co2_axis_title)

showtext_auto()
font_add_google("Lato")
pdf(file = paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Study/Adv_Sci/Co-authors/Figures/Figure3_Global.pdf"),
    width     = 10,
    height    = 10
)
par(family = "Lato")
par(family = "Lato")
par(mar = c(0.5, 0, 6.5, 0))
par(xpd = NA)
radar <- radarchart2(z, axistype=1, pty=dot_type, cglwd=2.5,
                     pcol = alpha(line_colors, line_alphas), 
                     plty = 1, axislabcol = "black", psize=2.5,
                     plwd=line_width,  pfcol = alpha("black", 0),
                     caxislabels = c("-100%", "-50%", "0%", "50%", "100%"),
                     cglcol = "black", vlcex = 2.3, 
                     title = paste0("Global"), cex.main = 3.6,
                     vlabels=axisnames, calcex = 1.5)
text(x = pm_total_position_x+0.15, y = pm_total_position_y, labels = pm_total, cex = 2.5, col = "#003366", pos = 3, font=2.4) #PM
text(x = no2_total_position_x-0.05, y = no2_total_position_y-0.1, labels = no2_total, cex = 2.5, col = "#003366", pos = 3, font=2.4) #NO2
text(x = o3_total_position_x+0.17, y = o3_total_position_y-0.05, labels = o3_total, cex = 2.5, col = "#003366", pos = 3, font=2.4) #O3
text(x = co2_total_position_x+0.2, y = co2_total_position_y-0.1, labels = co2_total, cex = 2.5, col = "#003366", pos = 3, font=2.4) #CO2

text(x = -0.7, y = -0.87,labels = pop_chg_text, cex = 2.5, col = "gray45", pos = 3) #population change
grid.draw(image_grob)
dev.off()


#GBD super-regions
num <- c(1:7)
n <- c(1:7)
for (j in num) {
  #   j=2
  gbd_name <- list_[j]
  x_gbd <- subset(x, x$GBDSuperRegion == list_[j])
  id_gbd <- x_gbd$ID
  
  spider2_$row <- rownames(spider2_)
  spider_gbd <- spider2_[spider2_$row %in% id_gbd, ]
  spider_gbd <- spider_gbd[, -which(names(spider_gbd) == "row")]
  n[j] <- nrow(spider_gbd)
  
  #mean
  #  y1 <- mean(spider_gbd$PM2.5)
  # y2 <- mean(spider_gbd$NO2)
  #y3 <- mean(spider_gbd$O3)
  #y4 <- mean(spider_gbd$CO2)
  yy1 <- subset(pm, ((year>=2005)&(year<=2019)) & (ID %in% list) & (GBDSuperRegion==gbd_name))
  yy1 <- yy1 %>% group_by(year) %>% summarise(mean=mean(pwpm)) 
  yy1 <- as.data.frame(yy1)
  y1 <- -(mean(yy1[c(1:3),2])-mean(yy1[c(13:15),2]))/mean(yy1[c(1:3),2])*100
  zz1 <- subset(pm, (year>=2005&year<=2019) & (ID %in% list) & (GBDSuperRegion==gbd_name))
  zz1 <- zz1 %>% group_by(year) %>% summarise(mean=mean(pwpm)) 
  zz1 <- arrange(zz1, year) #sort
  a1 <- MannKendall(zz1$mean) 
  
  yy2 <- subset(no2,((year>=2005)&(year<=2019)) & (ID %in% list) & (GBDSuperRegion==gbd_name))
  yy2 <- yy2 %>% group_by(year) %>% summarise(mean=mean(pwno2)) 
  yy2 <- as.data.frame(yy2)
  y2 <- -(mean(yy2[c(1:3),2])-mean(yy2[c(13:15),2]))/mean(yy2[c(1:3),2])*100
  zz2 <- subset(no2, (year>=2005&year<=2019) & (ID %in% list) & (GBDSuperRegion==gbd_name))
  zz2 <- zz2 %>% group_by(year) %>% summarise(mean=mean(pwno2)) 
  zz2 <- arrange(zz2, year) #sort
  a2 <- MannKendall(zz2$mean) 
  
  yy3 <- subset(o3, ((year>=2005)&(year<=2019)) & (ID %in% list) & (GBDSuperRegion==gbd_name))
  yy3 <- yy3 %>% group_by(year) %>% summarise(mean=mean(pwo3)) 
  yy3 <- as.data.frame(yy3)
  y3 <- -(mean(yy3[c(1:3),2])-mean(yy3[c(13:15),2]))/mean(yy3[c(1:3),2])*100
  zz3 <- subset(o3, (year>=2005&year<=2019) & (ID %in% list) & (GBDSuperRegion==gbd_name))
  zz3 <- zz3 %>% group_by(year) %>% summarise(mean=mean(pwo3)) 
  zz3 <- arrange(zz3, year) #sort
  a3 <- MannKendall(zz3$mean) 
  
  yy4 <- subset(co2, ((year>=2005)&(year<=2019)) & (ID %in% list) & (GBDSuperRegion==gbd_name))
  yy4 <- yy4 %>% group_by(year) %>% summarise(mean=median(co2_per_cap)) 
  yy4 <- as.data.frame(yy4)
  y4 <- -(mean(yy4[c(1:3),2])-mean(yy4[c(13:15),2]))/mean(yy4[c(1:3),2])*100
  zz4 <- subset(co2, (year>=2005&year<=2019) & (ID %in% list) & (GBDSuperRegion==gbd_name))
  zz4 <- zz4 %>% group_by(year) %>% summarise(mean=mean(co2_per_cap)) 
  zz4 <- arrange(zz4, year) #sort
  a4 <- MannKendall(zz4$mean) 
  
  total <- data.frame(y1, y2, y3, y4)
  names(total) <- names(spider_gbd)
  spider_gbd <- rbind(spider_gbd, total)
  
  #text (total)
  pm_total <- paste0(format(round(y1, 0), big.mark=","), "%")
  no2_total <- paste0(format(round(y2, 0), big.mark=","), "%")
  o3_total <- paste0(format(round(y3, 0), big.mark=","), "%")
  co2_total <- paste0(format(round(y4, 0), big.mark=","), "%")
  
  #text (total) position
  pm_total_position_x <- 0
  pm_total_position_y <- (y1/200 + 0.5)+0.15
  pm_total_position_y <- as.numeric(ifelse(pm_total_position_y>=1.1, 1, pm_total_position_y))
  
  no2_total_position_x <- -(y2/200 + 0.5)-0.2
  no2_total_position_x <- as.numeric(ifelse(no2_total_position_x>=1.1, 1, no2_total_position_x))
  no2_total_position_y <- 0.05
  
  o3_total_position_x <- 0
  o3_total_position_y <- -(y3/200 + 0.5)-0.3
  o3_total_position_y <- as.numeric(ifelse(o3_total_position_y>=1.1, 1, o3_total_position_y))
  
  #co2_total_position_x <- ifelse(y4>=0, (y4/200 + 0.7), (y4/200 + 0.5)+0.3)
  co2_total_position_x <- if(y4>=60) {y4/200 + 0.6
  } else if(y4<60 & y4>=0) {y4/200 + 0.7
  } else {y4/200 + 0.8}
  co2_total_position_x <- as.numeric(ifelse(co2_total_position_x>=1.1, 1, co2_total_position_x))
  co2_total_position_y <- 0.05
  
  #marking inf, ~-100, 100~
  #values
  spider_gbd[,1] <- ifelse((spider_gbd[,1]>100)|(spider_gbd[,1]==Inf), 100, spider_gbd[,1])
  spider_gbd[,1] <- ifelse(spider_gbd[,1]< -100, -100, spider_gbd[,1])
  
  spider_gbd[,2] <- ifelse((spider_gbd[,2]>100)|(spider_gbd[,2]==Inf), 100, spider_gbd[,2])
  spider_gbd[,2] <- ifelse(spider_gbd[,2]< -100, -100, spider_gbd[,2])
  
  spider_gbd[,3] <- ifelse((spider_gbd[,3]>100)|(spider_gbd[,3]==Inf), 100, spider_gbd[,3])
  spider_gbd[,3] <- ifelse(spider_gbd[,3]< -100, -100, spider_gbd[,3])
  
  spider_gbd[,4] <- ifelse((spider_gbd[,4]>100)|(spider_gbd[,4]==Inf), 100, spider_gbd[,4])
  spider_gbd[,4] <- ifelse(spider_gbd[,4]< -100, -100, spider_gbd[,4])
  
  max <- c(100,100,100,100)
  min <- c(-100,-100,-100,-100)
  z <- rbind(max, min, spider_gbd, total)
  
  #significance
  z[nrow(z),1] <- ifelse(a1$sl>0.05, Inf, z[nrow(z),1])
  z[nrow(z),2] <- ifelse(a2$sl>0.05, Inf, z[nrow(z),2])
  z[nrow(z),3] <- ifelse(a3$sl>0.05, Inf, z[nrow(z),3])
  z[nrow(z),4] <- ifelse(a4$sl>0.05, Inf, z[nrow(z),4])
  
  #population change
  #create total pop data using pollutants data
  pop<-read.csv("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Final_estimates/CO2/pop_13k_2000.csv", header=TRUE)
  pop$year <- 2000
  for (n in c(2001:2020)) {
    pop_<-read.csv(paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Final_estimates/CO2/pop_13k_", n, ".csv"), header=TRUE)
    pop_$year <- n
    pop <- rbind(pop, pop_)
  }
  pop <- subset(pop, ID %in% id_gbd)
  pop <- pop[, c("ID", "year", "pop")]
  #annual total pop
  popx <- pop %>% group_by(year) %>% summarize(pop = sum(pop))
  pop_05 <- subset(popx, popx$year %in% c(2005:2007))
  pop_19 <- subset(popx, popx$year %in% c(2017:2019))
  #pop changes from (2005-2007) to (2017-2019)
  pop_chg <- ((mean(pop_19$pop)-mean(pop_05$pop))/mean(pop_05$pop)) * 100
  pop_chg <- round(pop_chg, digits=0)
  sign <- ifelse(pop_chg>=0, "+", "")
  pop_chg_text <- paste0(sign, pop_chg, "%")
  
  icon <- readPNG("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/etc/Picture1.png")  
  image_grob <- rasterGrob(icon, interpolate = TRUE, x = 0.185, y = 0.179, width = 0.1, height = 0.1)
  
  #plot
  n_rows <- nrow(z)-2
  line_colors <- rep("azure3", n_rows) #line color
  line_colors[n_rows-1] <- "#003366"
  line_colors[n_rows] <- "brown1"
  line_alphas <- rep(0.7, n_rows) #line alpha
  line_alphas[n_rows-1] <- 1
  line_alphas[n_rows] <- 1
  line_width <- rep(0.5, n_rows) #line width
  line_width[n_rows-1] <- 7
  line_type <- rep(1, n_rows)
  line_type[n_rows] <- 0
  dot_type <- rep(32, n_rows) 
  dot_type[n_rows] <- 19
  
  title_lines <- rev(strwrap(paste0(list_[j]), width = 30))
  title_font_sizes <- ifelse(length(title_lines) == 2, c(2.8, 2.8), 3)
  pm_axis_title <- expression(bold("PM"["2.5"]))
  no2_axis_title <- expression(bold("NO"["2"]))
  o3_axis_title <- expression(bold("O"["3"]))
  co2_axis_title <- expression(bold("FFCO"["2"]))
  axisnames <- c(pm_axis_title, no2_axis_title, o3_axis_title, co2_axis_title)
  
  showtext_auto()
  font_add_google("Lato")
  pdf(file = paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Study/Adv_Sci/Co-authors/Figures/Figure3_", gbd_name, ".pdf"),
      width     = 10,
      height    = 10
  )
  par(family = "Lato")
  par(mar = c(0.5, 0, 6.5, 0))
  par(xpd = NA)
  radar <- radarchart2(z, axistype=1, pty=dot_type, cglwd=2.5,
                       pcol = alpha(line_colors, line_alphas), 
                       plty = line_type, axislabcol = "black", psize=2.5,
                       plwd=line_width,  pfcol = alpha("black", 0),
                       caxislabels = c("-100%", "-50%", "0%", "50%", "100%"),
                       cglcol = "black", vlcex = 2.3, cex.main = 3.6,
                       vlabels=axisnames, calcex = 1.5)
  text(x = pm_total_position_x+0.25, y = pm_total_position_y, labels = pm_total, cex = 2.5, col = "#003366", pos = 3, font=2.4) #PM
  text(x = no2_total_position_x, y = no2_total_position_y+0.05, labels = no2_total, cex = 2.5, col = "#003366", pos = 3, font=2.4) #NO2
  text(x = o3_total_position_x+0.25, y = o3_total_position_y, labels = o3_total, cex = 2.5, col = "#003366", pos = 3, font=2.4) #O3
  text(x = min(co2_total_position_x[[1]],2.4), y = co2_total_position_y+0.05, labels = co2_total, cex = 2.5, col = "#003366", pos = 3, font=2.4) #CO2
  for (i in rev(1:length(title_lines))) {
    mtext(text = title_lines[i], side = 3, line = i * 2.4, adj = 0.5, padj=0.5, cex = title_font_sizes, font = 2.4, col = "black", at = 0)}
  
  text(x = -0.6, y = -0.87, labels = pop_chg_text, cex = 2.5, col = "gray45", pos = 3) #population change
  grid.draw(image_grob)
  dev.off()
  
}



#####################################
######## BY city: Figure S3 #########
#####################################
#city.shp <- st_read("/Users/k_sy_n_macbook/Desktop/Dropbox/GWU/Lab/13000cities/NO2/Inputs/hdc_naming_warp.shp")
city.shp <- st_read("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/NO2/Inputs/hdc_naming_warp.shp")
list <- c(12141, 11920, 13017, 12456, 6998, 9933, 12882, 12445, 9752, 10745, 6887, 3915, 155, 1308, 10774, 950, 12911, 14, 3574, 3686)

for (i in list) {
  #.  i<-12445   #860: DC, 950: NYC, 12445: Seoul
  pp <- subset(city.shp, city.shp$ID_HDC_G0==i)
  city_name <- pp$NAME_MAIN
  
  #percent change from 2005 to 2019
  #pm
  x1 <- subset(pm, ID==i)
  x1 <- arrange(x1, year) #sort
  x1 <- subset(x1, (year>=2005)&(year<=2019))
  xx1 <- -(mean(x1[c(1:3),5])- mean(x1[c(13:15),5]))/mean(x1[c(1:3),5])*100
  #no2
  x2 <- subset(no2, ID==i)
  x2 <- arrange(x2, year) #sort
  x2 <- subset(x2, (year>=2005)&(year<=2019))
  xx2 <- -(mean(x2[c(1:3),4])- mean(x2[c(13:15),4]))/mean(x2[c(1:3),4])*100
  #o3
  x3 <- subset(o3, ID==i)
  x3 <- arrange(x3, year) #sort
  x3 <- subset(x3, (year>=2005)&(year<=2019))
  xx3 <- -(mean(x3[c(1:3),5])- mean(x3[c(13:15),5]))/mean(x3[c(1:3),5])*100
  #co2
  x4 <- subset(co2_per_cap, ID==i)
  x4 <- arrange(x4, year) #sort
  x4 <- subset(x4, (year>=2005)&(year<=2019))
  xx4 <- -(mean(x4[c(1:3),5])- mean(x4[c(13:15),5]))/mean(x4[c(1:3),5])*100
  
  i_combined <- as.data.frame(rbind(xx1, xx2, xx3, xx4))
  rownames(i_combined) <- NULL
  i_combined$ID <- i
  colnames(i_combined)[1] <- "final"
  
  #significance
  #pm
  a <- MannKendall(x1$pwpm) 
  pm_sig <- ifelse(a$sl<=0.05, "O", "X")
  #no2
  a <- MannKendall(x2$pwno2)
  no2_sig <- ifelse(a$sl<=0.05, "O", "X")
  #o3
  a <- MannKendall(x3$pwo3)
  o3_sig <- ifelse(a$sl<=0.05, "O", "X")
  #co2
  a <- MannKendall(x4$co2_per_cap)
  co2_sig <- ifelse(a$sl<=0.05, "O", "X")
  
  #marking inf, ~-100, 100~
  #texts
  pm_extrm_values <- ifelse((i_combined[1,1]>100)|(i_combined[1,1]< -100)|(i_combined[1,1]==Inf), paste0(format(i_combined[1,1], big.mark=","), "%"), "")
  no2_extrm_values <- ifelse((i_combined[2,1]>100)|(i_combined[2,1]< -100)|(i_combined[2,1]==Inf), paste0(format(i_combined[2,1], big.mark=","), "%"), "")
  o3_extrm_values <- ifelse((i_combined[3,1]>100)|(i_combined[3,1]< -100)|(i_combined[3,1]==Inf), paste0(format(i_combined[3,1], big.mark=","), "%"), "")
  co2_extrm_values <- ifelse((i_combined[4,1]>100)|(i_combined[4,1]< -100)|(i_combined[4,1]==Inf), paste0(format(i_combined[4,1], big.mark=","), "%"), "")
  #values
  i_combined[1,1] <- ifelse((i_combined[1,1]>100)|(i_combined[1,1]==Inf), 100, i_combined[1,1])
  i_combined[1,1] <- ifelse(i_combined[1,1]< -100, -100, i_combined[1,1])
  
  i_combined[2,1] <- ifelse((i_combined[2,1]>100)|(i_combined[2,1]==Inf), 100, i_combined[2,1])
  i_combined[2,1] <- ifelse(i_combined[2,1]< -100, -100, i_combined[2,1])
  
  i_combined[3,1] <- ifelse((i_combined[3,1]>100)|(i_combined[3,1]==Inf), 100, i_combined[3,1])
  i_combined[3,1] <- ifelse(i_combined[3,1]< -100, -100, i_combined[3,1])   
  
  i_combined[4,1] <- ifelse((i_combined[4,1]>100)|(i_combined[4,1]==Inf), 100, i_combined[4,1])
  i_combined[4,1] <- ifelse(i_combined[4,1]< -100, -100, i_combined[4,1])
  #texts position in plots
  pm_extrm_position_x <- 0.2
  pm_extrm_position_y <- ifelse(i_combined[1,1]==100, 1, 0.2)
  
  no2_extrm_position_x <- ifelse(i_combined[2,1]==100, -1, -0.2)
  no2_extrm_position_y <- 0
  
  o3_extrm_position_x <- 0
  o3_extrm_position_y <- ifelse(i_combined[3,1]==100, -1, -0.2)
  
  co2_extrm_position_x <- ifelse(i_combined[4,1]==100, 1, 0.2)
  co2_extrm_position_y <- 0
  
  
  #attainment & 2019 levels
  y1 <- subset(x1, year==2019)
  att_color_pm <- ifelse(y1$pwpm<=5, "#55C667FF", "darkmagenta") #pm25 WHo: 5ug/m3
  pm_title <- ifelse(y1$pwpm<=5, "(<=WHO AQG)", "(>WHO AQG)") 
  
  y2 <- subset(x2, year==2019)
  att_color_no2 <- ifelse(y2$pwno2<=5.32, "#55C667FF", "darkmagenta") #no2 WHO: 10ug/m3 = 5.32ppb
  no2_title <- ifelse(y2$pwno2<=5.32, "(<=WHO AQG)", "(>WHO AQG)") 
  
  y3 <- subset(x3, year==2019) 
  att_color_o3 <- ifelse(y3$pwo3<=30.6, "#55C667FF", "darkmagenta") #o3 WHO: 100ug/m3 = 50.96ppb
  o3_title <- ifelse(y3$pwo3<=30.6, "(<=WHO AQG)", "(>WHO AQG)") 
  
  #population change (%)
  pop<-read.csv("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Final_estimates/CO2/pop_13k_2000.csv", header=TRUE)
  pop$year <- 2000
  for (n in c(2001:2020)) {
    pop_<-read.csv(paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Final_estimates/CO2/pop_13k_", n, ".csv"), header=TRUE)
    pop_$year <- n
    pop <- rbind(pop, pop_)
  }
  pop <- pop[, c("ID", "year", "pop")]
  pop_05 <- subset(pop, (year%in%c(2005:2007)&ID==i))
  pop_19 <- subset(pop, (year%in%c(2017:2019)&ID==i))
  pop_chg <- ((mean(pop_19$pop)-mean(pop_05$pop))/mean(pop_05$pop)) * 100
  pop_chg <- round(pop_chg, digits=0)
  sign <- ifelse(pop_chg>=0, "+", "")
  pop_chg_text <- paste0(sign, pop_chg, "%\n")
  
  #spiderweb plot
  spider <- as.data.frame(t(i_combined))
  spider$V1 <- as.numeric(spider$V1)
  spider$V2 <- as.numeric(spider$V2)
  spider$V3 <- as.numeric(spider$V3)
  spider$V4 <- as.numeric(spider$V4)
  colnames(spider)[1] <- "PM2.5
"
  colnames(spider)[2] <- "NO2"
  colnames(spider)[3] <- "O3"
  colnames(spider)[4] <- "CO2"
  spider <- spider[1, , drop=FALSE]
  
  max <- c(100,100,100,100)
  min <- c(-100,-100,-100,-100)
  default <- c(0,0,0,0)
  z <- rbind(max, min, default, spider, spider)
  
  #significance
  z[5,1] <- ifelse(pm_sig=="X", Inf, z[5,1])
  z[5,2] <- ifelse(no2_sig=="X", Inf, z[5,2])
  z[5,3] <- ifelse(o3_sig=="X", Inf, z[5,3])
  z[5,4] <- ifelse(co2_sig=="X", Inf, z[5,4])
  
  
  #plot
  category_colors <- c("grey30", "#003366", "red")
  category_line_alpha <- c(1, 1, 1)
  category_line_width <- c(3, 8, 1)
  category_line_type <- c(1, 1, 0)
  category_fill_alpha <- c(0, 0.3, 0)
  category_dot_type <- c(32, 16, 19)
  category_label_color <- c(att_color_pm, att_color_no2, att_color_o3, "black")
  
  pm_axis_title <- expression(bold("PM"["2.5"]))
  no2_axis_title <- expression(bold("NO"["2"]))
  o3_axis_title <- expression(bold("O"["3"]))
  co2_axis_title <- expression(bold("FFCO"["2"]))
  axisnames <- c(pm_axis_title, no2_axis_title, o3_axis_title, co2_axis_title)
  
  icon <- readPNG("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/etc/Picture1.png")  
  image_grob <- rasterGrob(icon, interpolate = TRUE, x = 0.165, y = 0.19, width = 0.1, height = 0.1)
  
  showtext_auto()
  font_add_google("Lato")
  pdf(file = paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Study/Adv_Sci/Co-authors/Figures/FigureS3/FigureS3_", city_name, ".pdf"),
      width     = 10,
      height    = 10
  )
  par(family = "Lato")
  par(mar = c(1, 1, 8, 1))
  par(oma = c(0, 0, 0, 0))
  radar <-radarchart2(z, axistype=1, pcol = alpha(category_colors, category_line_alpha), 
                      pfcol = alpha("#003366", category_fill_alpha),
                      pty=category_dot_type, psize=3, cglwd=3,
                      plty = category_line_type, axislabcol = "black", plwd=category_line_width,
                      caxislabels = c("-100%", "", "0%", "", "100%"), 
                      cglcol = "black", vlcex = 2.5, 
                      title = paste0(city_name), cex.main=4,
                      calcex = 2.5, vlabels=axisnames, vlabcol=category_label_color)
  #text(x = 0.45, y = 1.11, labels = pm_title, cex = 1.5, col = att_color_pm, pos = 3) #PM
  #text(x = -1.18, y = -0.2, labels = no2_title, cex = 1.5, col = att_color_no2, pos = 3) #NO2
  #text(x = 0.38, y = -1.28, labels = o3_title, cex = 1.5, col = att_color_o3, pos = 3) #O3
  text(x = pm_extrm_position_x, y = pm_extrm_position_y, labels = pm_extrm_values, cex = 1.7, col = "darkblue", pos = 3) #PM extreme
  text(x = no2_extrm_position_x, y = no2_extrm_position_y, labels = no2_extrm_values, cex = 1.7, col = "darkblue", pos = 3) #NO2 extreme
  text(x = o3_extrm_position_x, y = o3_extrm_position_y, labels = o3_extrm_values, cex = 1.7, col = "darkblue", pos = 3) #O3 extreme
  text(x = co2_extrm_position_x, y = co2_extrm_position_y, labels = co2_extrm_values, cex = 1.7, col = "darkblue", pos = 3) #CO2 extreme
  text(x = -0.75, y = -1.0, labels = pop_chg_text, cex = 2.5, col = "gray45", pos = 3) #population change
  grid.draw(image_grob)
  
  dev.off()
  
}





########################################
###### Correlation: Figure 4 ###########
########################################
#DATA: use files after running lines 1179-1342 for Fig3
#AT CITY-LEVEL
combined <- list(pm, no2, o3, co2) %>% reduce(full_join, by=c('ID', "year"))
combined <- combined[c("ID", "year", "pwpm", "pwno2", "pwo3", "co2_per_cap", "country.x", "GBDSuperRegion.x")]
combined <- rename(combined, country="country.x")
combined <- rename(combined, GBDSuperRegion="GBDSuperRegion.x")
combined <- na.omit(combined)

#indexing each GBD super-region with numbers
gbd_list <- sort(unique(combined$GBDSuperRegion))
index <- c(1:7)
gbd_list <- as.data.frame(cbind(gbd_list, index))
gbd_list <- rename(gbd_list, GBDSuperRegion="gbd_list")
combined <- merge(combined, gbd_list, by="GBDSuperRegion")

#seed
a <- subset(combined, ID==list[[1]]) #list: from Fig.3 (n=12,573)
title <- a[1,2]
gbd <- a[1,1]
a <- a[,c(4:7)]
num_vars <- ncol(a) 
cor_matrix <- matrix(NA, nrow = num_vars, ncol = num_vars) #empty frame
p_value_matrix <- matrix(NA, nrow = num_vars, ncol = num_vars) #empty frame
#cor matrix
for (i in 1:num_vars) { 
  for (j in 1:num_vars) {
    if ((sum(!is.na(a[, i])) >= 5) & (sum(!is.na(a[, j])) >= 5)) {   #skip with columns with many NAs
      cor_test_result <- cor.test(a[, i], a[, j], use = "pairwise.complete.obs")
      cor_matrix[i, j] <- cor_test_result$estimate
      p_value_matrix[i, j] <- cor_test_result$p.value
    }
  }
}

x <- data.frame(matrix(nrow = 6, ncol = 5)) #creating empty df for final table
colnames(x)[1] <- "coef"
colnames(x)[2] <- "p"
colnames(x)[3] <- "assc"
colnames(x)[4] <- "ID"
colnames(x)[5] <- "gbd"

x[1,1] <- cor_matrix[1,2]
x[2,1] <- cor_matrix[1,3]
x[3,1] <- cor_matrix[1,4]
x[4,1] <- cor_matrix[2,3]
x[5,1] <- cor_matrix[2,4]
x[6,1] <- cor_matrix[3,4]

x[1,2] <- p_value_matrix[1,2]
x[2,2] <- p_value_matrix[1,3]
x[3,2] <- p_value_matrix[1,4]
x[4,2] <- p_value_matrix[2,3]
x[5,2] <- p_value_matrix[2,4]
x[6,2] <- p_value_matrix[3,4]

x[1,3] <- "pm_no2"
x[2,3] <- "pm_o3"
x[3,3] <- "pm_co2"
x[4,3] <- "no2_o3"
x[5,3] <- "no2_co2"
x[6,3] <- "o3_co2"

x[,4] <- title
x[,5] <- gbd

#for loop
for (k in c(list[2:length(list)])) {
  
  a <- subset(combined, ID==k)
  title <- a[1,2]
  gbd <- a[1,1]
  a <- a[,c(4:7)]
  num_vars <- ncol(a) 
  cor_matrix <- matrix(NA, nrow = num_vars, ncol = num_vars) #empty frame
  p_value_matrix <- matrix(NA, nrow = num_vars, ncol = num_vars) #empty frame
  #cor matrix
  for (i in 1:num_vars) { 
    for (j in 1:num_vars) {
      if ((sum(!is.na(a[, i])) >= 5) & (sum(!is.na(a[, j])) >= 5)) {  #skip with columns with many NAs
        cor_test_result <- cor.test(a[, i], a[, j], use = "pairwise.complete.obs")
        cor_matrix[i, j] <- cor_test_result$estimate
        p_value_matrix[i, j] <- cor_test_result$p.value
      }
    }
  }
  
  x_ <- data.frame(matrix(nrow = 6, ncol = 5)) #creating empty df for final table
  colnames(x_)[1] <- "coef"
  colnames(x_)[2] <- "p"
  colnames(x_)[3] <- "assc"
  colnames(x_)[4] <- "ID"
  colnames(x_)[5] <- "gbd"
  
  x_[1,1] <- cor_matrix[1,2]
  x_[2,1] <- cor_matrix[1,3]
  x_[3,1] <- cor_matrix[1,4]
  x_[4,1] <- cor_matrix[2,3]
  x_[5,1] <- cor_matrix[2,4]
  x_[6,1] <- cor_matrix[3,4]
  
  x_[1,2] <- p_value_matrix[1,2]
  x_[2,2] <- p_value_matrix[1,3]
  x_[3,2] <- p_value_matrix[1,4]
  x_[4,2] <- p_value_matrix[2,3]
  x_[5,2] <- p_value_matrix[2,4]
  x_[6,2] <- p_value_matrix[3,4]
  
  x_[1,3] <- "pm_no2"
  x_[2,3] <- "pm_o3"
  x_[3,3] <- "pm_co2"
  x_[4,3] <- "no2_o3"
  x_[5,3] <- "no2_co2"
  x_[6,3] <- "o3_co2"
  
  x_[,4] <- title
  x_[,5] <- gbd
  
  x <- rbind(x, x_)
  
}
x$assc_i <- 0
x$assc_i <- ifelse(x$assc=="pm_no2", 1, x$assc_i)
x$assc_i <- ifelse(x$assc=="pm_o3", 2, x$assc_i)
x$assc_i <- ifelse(x$assc=="pm_co2", 3, x$assc_i)
x$assc_i <- ifelse(x$assc=="no2_o3", 4, x$assc_i)
x$assc_i <- ifelse(x$assc=="no2_co2", 5, x$assc_i)
x$assc_i <- ifelse(x$assc=="o3_co2", 6, x$assc_i)

x$sig <- ifelse(x$p <= 0.05 & x$coef >= 0, "o+",
                ifelse(x$p <= 0.05 & x$coef < 0, "o-",
                       ifelse(x$p > 0.05 & x$coef >= 0, "x+",
                              ifelse(x$p > 0.05 & x$coef < 0, "x-", NA))))

x$gbd_i <- 0
x$gbd_i <- ifelse(x$gbd=="Central Europe, Eastern Europe, and Central Asia", 1, x$gbd_i)
x$gbd_i <- ifelse(x$gbd=="High income", 2, x$gbd_i)
x$gbd_i <- ifelse(x$gbd=="Latin America and Caribbean", 3, x$gbd_i)
x$gbd_i <- ifelse(x$gbd=="North Africa and Middle East", 4, x$gbd_i)
x$gbd_i <- ifelse(x$gbd=="South Asia", 5, x$gbd_i)
x$gbd_i <- ifelse(x$gbd=="Southeast Asia, East Asia, and Oceania", 6, x$gbd_i)
x$gbd_i <- ifelse(x$gbd=="Sub-Saharan Africa", 7, x$gbd_i)

#FREQ TABLE
#GLOBAL
#corr
#seed
y<-x
i=1
test <- subset(y, y$assc_i==i)
freq_table <- table(test$sig)
freq_df <- as.data.frame(freq_table)

pct <- data.frame(matrix(nrow = 1, ncol = 5)) #empty frame
colnames(pct) <- c("assc", "sig_pos", "sig_neg", "insig_pos", "insig_neg")
pct$sig_pos <- freq_df[2,2]/(freq_df[1,2]+freq_df[2,2]+freq_df[3,2]+freq_df[4,2])*100
pct$sig_neg <- freq_df[1,2]/(freq_df[1,2]+freq_df[2,2]+freq_df[3,2]+freq_df[4,2])*100
pct$insig_pos <- freq_df[4,2]/(freq_df[1,2]+freq_df[2,2]+freq_df[3,2]+freq_df[4,2])*100
pct$insig_neg <- freq_df[3,2]/(freq_df[1,2]+freq_df[2,2]+freq_df[3,2]+freq_df[4,2])*100
pct$total <- sum(freq_df$Freq)
pct$assc <- i
#for loop
for (i in c(2:6)) {
  test <- subset(y, y$assc_i==i)
  freq_table <- table(test$sig)
  freq_df <- as.data.frame(freq_table)
  
  pct_ <- data.frame(matrix(nrow = 1, ncol = 5)) #empty frame
  colnames(pct_) <- c("assc", "sig_pos", "sig_neg", "insig_pos", "insig_neg")
  pct_$sig_pos <- freq_df[2,2]/(freq_df[1,2]+freq_df[2,2]+freq_df[3,2]+freq_df[4,2])*100
  pct_$sig_neg <- freq_df[1,2]/(freq_df[1,2]+freq_df[2,2]+freq_df[3,2]+freq_df[4,2])*100
  pct_$insig_pos <- freq_df[4,2]/(freq_df[1,2]+freq_df[2,2]+freq_df[3,2]+freq_df[4,2])*100
  pct_$insig_neg <- freq_df[3,2]/(freq_df[1,2]+freq_df[2,2]+freq_df[3,2]+freq_df[4,2])*100
  pct_$total <- sum(freq_df$Freq)
  pct_$assc <- i
  
  pct <- rbind(pct, pct_)
}
pct$gbd_i <- 0
df_name <- paste("cor_0", sep = "")
assign(df_name, pct)

#EACH GBD
for (k in 1:7) {
  #subset
  y <- subset(x, x$gbd_i==k)
  #corr
  #seed
  i=1
  test <- subset(y, y$assc_i==i)
  freq_table <- table(test$sig)
  freq_df <- as.data.frame(freq_table)
  
  pct <- data.frame(matrix(nrow = 1, ncol = 5)) #empty frame
  colnames(pct) <- c("assc", "sig_pos", "sig_neg", "insig_pos", "insig_neg")
  pct$sig_pos <- freq_df[2,2]/(freq_df[1,2]+freq_df[2,2]+freq_df[3,2]+freq_df[4,2])*100
  pct$sig_neg <- freq_df[1,2]/(freq_df[1,2]+freq_df[2,2]+freq_df[3,2]+freq_df[4,2])*100
  pct$insig_pos <- freq_df[4,2]/(freq_df[1,2]+freq_df[2,2]+freq_df[3,2]+freq_df[4,2])*100
  pct$insig_neg <- freq_df[3,2]/(freq_df[1,2]+freq_df[2,2]+freq_df[3,2]+freq_df[4,2])*100
  pct$total <- sum(freq_df$Freq)
  pct$assc <- i
  #for loop
  for (i in c(2:6)) {
    test <- subset(y, y$assc_i==i)
    freq_table <- table(test$sig)
    freq_df <- as.data.frame(freq_table)
    
    pct_ <- data.frame(matrix(nrow = 1, ncol = 5)) #empty frame
    colnames(pct_) <- c("assc", "sig_pos", "sig_neg", "insig_pos", "insig_neg")
    pct_$sig_pos <- freq_df[2,2]/(freq_df[1,2]+freq_df[2,2]+freq_df[3,2]+freq_df[4,2])*100
    pct_$sig_neg <- freq_df[1,2]/(freq_df[1,2]+freq_df[2,2]+freq_df[3,2]+freq_df[4,2])*100
    pct_$insig_pos <- freq_df[4,2]/(freq_df[1,2]+freq_df[2,2]+freq_df[3,2]+freq_df[4,2])*100
    pct_$insig_neg <- freq_df[3,2]/(freq_df[1,2]+freq_df[2,2]+freq_df[3,2]+freq_df[4,2])*100
    pct_$total <- sum(freq_df$Freq)
    pct_$assc <- i
    
    pct <- rbind(pct, pct_)
  }
  
  pct$gbd_i <- k
  
  df_name <- paste("cor_", k, sep = "")
  assign(df_name, pct)
}

cor_fin <- cbind(cor_0, cor_1, cor_2, cor_3, cor_4, cor_5, cor_6, cor_7)
cor_table <- rbind(cor_0, cor_1, cor_2, cor_3, cor_4, cor_5, cor_6, cor_7)
cor_table$pos <- cor_table$sig_pos + cor_table$insig_pos
cor_table$neg <- cor_table$sig_neg + cor_table$insig_neg

#total number of samples in the begining
length(unique(x$ID))
for (i in 1:7) {
  test <- subset(x, x$gbd_i==i)
  print(length(unique(test$ID)))
}

#Pie charts
for (i in c(0:7)) { #i: gbd
  data <- get(paste0("cor_", i))
  data <- data %>%
    dplyr::select(assc, sig_pos, insig_pos, sig_neg, insig_neg, total, gbd_i)
  
  for (j in c(1:6)) { #j: association
    pct <- as.numeric(data[j, 2:5])
    category <- names(data[, 2:5])
    df <- data.frame(pct = pct, category = category)
    df[5,] <- df[3,]
    df <- df[-3, ]
    
    df$category <- factor(df$category, levels = df$category)
    custom_colors <- c("dodgerblue3", alpha("dodgerblue3", alpha = 0.4), alpha("firebrick3", alpha = 0.4), "firebrick3")
    
    # Create a pie chart with custom colors
    par(mar = c(1, 1, 1, 1))
    p <- ggplot(data = df, aes(x = "", y = pct, fill = category)) +
      geom_bar(stat = "identity", color = "white", show.legend = FALSE) +
      scale_fill_manual(values = custom_colors) +
      coord_polar(theta = "y", start = 0) + 
      theme_void() +
      labs(title = NULL) +
      scale_y_reverse()
    ggsave(paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Study/Nature_cities/Figures/Figure4/cor_", i, "_", j, ".pdf"), plot = p, width = 49, height = 49, units = "in")
    dev.off() 
    
  }}





################################################################################
###### Correlation_subgroup analysis: Extended Data Figure 4 ###################
################################################################################
#categorization by population size
pop_cat <- combined[c("ID", "year", "pop")]
pop_cat <- subset(pop_cat, pop_cat$year==2019)
pop_cat$pop_cat <- ifelse(pop_cat$pop<50000, "small", "large")
pop_cat <- pop_cat[c("ID", "pop_cat")]
table(pop_cat$pop_cat)

#merge
combined <- merge(combined, pop_cat, by="ID")
combined_small <- subset(combined, combined$pop_cat=="small")
combined_large <- subset(combined, combined$pop_cat=="large")

#Small
#indexing each GBD super-region with numbers
gbd_list <- sort(unique(combined_small$GBDSuperRegion))
index <- c(1:7)
gbd_list <- as.data.frame(cbind(gbd_list, index))
gbd_list <- rename(gbd_list, GBDSuperRegion="gbd_list")
combined_small <- merge(combined_small, gbd_list, by="GBDSuperRegion")
list_small <- unique(combined_small$ID)

#seed
a <- subset(combined_small, ID==list_small[[1]])
title <- a[1,2]
gbd <- a[1,1]
a <- a[,c(4:7)]
num_vars <- ncol(a) 
cor_matrix <- matrix(NA, nrow = num_vars, ncol = num_vars) #empty frame
p_value_matrix <- matrix(NA, nrow = num_vars, ncol = num_vars) #empty frame
#cor matrix
for (i in 1:num_vars) { 
  for (j in 1:num_vars) {
    if ((sum(!is.na(a[, i])) >= 5) & (sum(!is.na(a[, j])) >= 5)) {   #skip with columns with many NAs
      cor_test_result <- cor.test(a[, i], a[, j], use = "pairwise.complete.obs")
      cor_matrix[i, j] <- cor_test_result$estimate
      p_value_matrix[i, j] <- cor_test_result$p.value
    }
  }
}

x <- data.frame(matrix(nrow = 6, ncol = 5)) #creating empty df for final table
colnames(x)[1] <- "coef"
colnames(x)[2] <- "p"
colnames(x)[3] <- "assc"
colnames(x)[4] <- "ID"
colnames(x)[5] <- "gbd"

x[1,1] <- cor_matrix[1,2]
x[2,1] <- cor_matrix[1,3]
x[3,1] <- cor_matrix[1,4]
x[4,1] <- cor_matrix[2,3]
x[5,1] <- cor_matrix[2,4]
x[6,1] <- cor_matrix[3,4]

x[1,2] <- p_value_matrix[1,2]
x[2,2] <- p_value_matrix[1,3]
x[3,2] <- p_value_matrix[1,4]
x[4,2] <- p_value_matrix[2,3]
x[5,2] <- p_value_matrix[2,4]
x[6,2] <- p_value_matrix[3,4]

x[1,3] <- "pm_no2"
x[2,3] <- "pm_o3"
x[3,3] <- "pm_co2"
x[4,3] <- "no2_o3"
x[5,3] <- "no2_co2"
x[6,3] <- "o3_co2"

x[,4] <- title
x[,5] <- gbd

#for loop
for (k in c(list_small[2:length(list_small)])) {
  
  a <- subset(combined_small, ID==k)
  title <- a[1,2]
  gbd <- a[1,1]
  a <- a[,c(4:7)]
  num_vars <- ncol(a) 
  cor_matrix <- matrix(NA, nrow = num_vars, ncol = num_vars) #empty frame
  p_value_matrix <- matrix(NA, nrow = num_vars, ncol = num_vars) #empty frame
  #cor matrix
  for (i in 1:num_vars) { 
    for (j in 1:num_vars) {
      if ((sum(!is.na(a[, i])) >= 5) & (sum(!is.na(a[, j])) >= 5)) {  #skip with columns with many NAs
        cor_test_result <- cor.test(a[, i], a[, j], use = "pairwise.complete.obs")
        cor_matrix[i, j] <- cor_test_result$estimate
        p_value_matrix[i, j] <- cor_test_result$p.value
      }
    }
  }
  
  x_ <- data.frame(matrix(nrow = 6, ncol = 5)) #creating empty df for final table
  colnames(x_)[1] <- "coef"
  colnames(x_)[2] <- "p"
  colnames(x_)[3] <- "assc"
  colnames(x_)[4] <- "ID"
  colnames(x_)[5] <- "gbd"
  
  x_[1,1] <- cor_matrix[1,2]
  x_[2,1] <- cor_matrix[1,3]
  x_[3,1] <- cor_matrix[1,4]
  x_[4,1] <- cor_matrix[2,3]
  x_[5,1] <- cor_matrix[2,4]
  x_[6,1] <- cor_matrix[3,4]
  
  x_[1,2] <- p_value_matrix[1,2]
  x_[2,2] <- p_value_matrix[1,3]
  x_[3,2] <- p_value_matrix[1,4]
  x_[4,2] <- p_value_matrix[2,3]
  x_[5,2] <- p_value_matrix[2,4]
  x_[6,2] <- p_value_matrix[3,4]
  
  x_[1,3] <- "pm_no2"
  x_[2,3] <- "pm_o3"
  x_[3,3] <- "pm_co2"
  x_[4,3] <- "no2_o3"
  x_[5,3] <- "no2_co2"
  x_[6,3] <- "o3_co2"
  
  x_[,4] <- title
  x_[,5] <- gbd
  
  x <- rbind(x, x_)
  
}
x$assc_i <- 0
x$assc_i <- ifelse(x$assc=="pm_no2", 1, x$assc_i)
x$assc_i <- ifelse(x$assc=="pm_o3", 2, x$assc_i)
x$assc_i <- ifelse(x$assc=="pm_co2", 3, x$assc_i)
x$assc_i <- ifelse(x$assc=="no2_o3", 4, x$assc_i)
x$assc_i <- ifelse(x$assc=="no2_co2", 5, x$assc_i)
x$assc_i <- ifelse(x$assc=="o3_co2", 6, x$assc_i)

x$sig <- ifelse(x$p <= 0.05 & x$coef >= 0, "o+",
                ifelse(x$p <= 0.05 & x$coef < 0, "o-",
                       ifelse(x$p > 0.05 & x$coef >= 0, "x+",
                              ifelse(x$p > 0.05 & x$coef < 0, "x-", NA))))

x$gbd_i <- 0
x$gbd_i <- ifelse(x$gbd=="Central Europe, Eastern Europe, and Central Asia", 1, x$gbd_i)
x$gbd_i <- ifelse(x$gbd=="High income", 2, x$gbd_i)
x$gbd_i <- ifelse(x$gbd=="Latin America and Caribbean", 3, x$gbd_i)
x$gbd_i <- ifelse(x$gbd=="North Africa and Middle East", 4, x$gbd_i)
x$gbd_i <- ifelse(x$gbd=="South Asia", 5, x$gbd_i)
x$gbd_i <- ifelse(x$gbd=="Southeast Asia, East Asia, and Oceania", 6, x$gbd_i)
x$gbd_i <- ifelse(x$gbd=="Sub-Saharan Africa", 7, x$gbd_i)

#FREQ TABLE
#GLOBAL
#corr
#seed
y<-x
i=1
test <- subset(y, y$assc_i==i)
freq_table <- table(test$sig)
freq_table <- as.data.frame(freq_table)
freq_df <- data.frame(
  Var1 = c("o-", "o+", "x-", "x+")
)
freq_df <- merge(freq_df, freq_table, by = "Var1", all = TRUE)
freq_df[is.na(freq_df)] <- 0

pct <- data.frame(matrix(nrow = 1, ncol = 5)) #empty frame
colnames(pct) <- c("assc", "sig_pos", "sig_neg", "insig_pos", "insig_neg")
pct$sig_pos <- freq_df[2,2] / sum(replace(freq_df[1:4,2], is.na(freq_df[1:4,2]), 0)) * 100
pct$sig_neg <- freq_df[1,2] / sum(replace(freq_df[1:4,2], is.na(freq_df[1:4,2]), 0)) * 100
pct$insig_pos <- freq_df[4,2] / sum(replace(freq_df[1:4,2], is.na(freq_df[1:4,2]), 0)) * 100
pct$insig_neg <- freq_df[3,2] / sum(replace(freq_df[1:4,2], is.na(freq_df[1:4,2]), 0)) * 100
pct$total <- sum(freq_df$Freq)
pct$assc <- i

#for loop
for (i in c(2:6)) {
  test <- subset(y, y$assc_i==i)
  freq_table <- table(test$sig)
  freq_table <- as.data.frame(freq_table)
  freq_df <- data.frame(
    Var1 = c("o-", "o+", "x-", "x+")
  )
  freq_df <- merge(freq_df, freq_table, by = "Var1", all = TRUE)
  freq_df[is.na(freq_df)] <- 0
  
  pct_ <- data.frame(matrix(nrow = 1, ncol = 5)) #empty frame
  colnames(pct_) <- c("assc", "sig_pos", "sig_neg", "insig_pos", "insig_neg")
  pct_$sig_pos <- freq_df[2,2] / sum(replace(freq_df[1:4,2], is.na(freq_df[1:4,2]), 0)) * 100
  pct_$sig_neg <- freq_df[1,2] / sum(replace(freq_df[1:4,2], is.na(freq_df[1:4,2]), 0)) * 100
  pct_$insig_pos <- freq_df[4,2] / sum(replace(freq_df[1:4,2], is.na(freq_df[1:4,2]), 0)) * 100
  pct_$insig_neg <- freq_df[3,2] / sum(replace(freq_df[1:4,2], is.na(freq_df[1:4,2]), 0)) * 100
  pct_$total <- sum(freq_df$Freq)
  pct_$assc <- i
  
  pct <- rbind(pct, pct_)
}
pct$gbd_i <- 0
df_name <- paste("cor_0", sep = "")
assign(df_name, pct)

#EACH GBD
for (k in 1:7) {
  #subset
  y <- subset(x, x$gbd_i==k)
  #corr
  #seed
  i=1
  test <- subset(y, y$assc_i==i)
  freq_table <- table(test$sig)
  freq_table <- as.data.frame(freq_table)
  freq_df <- data.frame(
    Var1 = c("o-", "o+", "x-", "x+")
  )
  freq_df <- merge(freq_df, freq_table, by = "Var1", all = TRUE)
  freq_df[is.na(freq_df)] <- 0
  
  pct <- data.frame(matrix(nrow = 1, ncol = 5)) #empty frame
  colnames(pct) <- c("assc", "sig_pos", "sig_neg", "insig_pos", "insig_neg")
  pct$sig_pos <- freq_df[2,2] / sum(replace(freq_df[1:4,2], is.na(freq_df[1:4,2]), 0)) * 100
  pct$sig_neg <- freq_df[1,2] / sum(replace(freq_df[1:4,2], is.na(freq_df[1:4,2]), 0)) * 100
  pct$insig_pos <- freq_df[4,2] / sum(replace(freq_df[1:4,2], is.na(freq_df[1:4,2]), 0)) * 100
  pct$insig_neg <- freq_df[3,2] / sum(replace(freq_df[1:4,2], is.na(freq_df[1:4,2]), 0)) * 100
  pct$total <- sum(freq_df$Freq)
  pct$assc <- i
  
  #for loop
  for (i in c(2:6)) {
    test <- subset(y, y$assc_i==i)
    freq_table <- table(test$sig)
    freq_table <- as.data.frame(freq_table)
    freq_df <- data.frame(
      Var1 = c("o-", "o+", "x-", "x+")
    )
    freq_df <- merge(freq_df, freq_table, by = "Var1", all = TRUE)
    freq_df[is.na(freq_df)] <- 0
    
    pct_ <- data.frame(matrix(nrow = 1, ncol = 5)) #empty frame
    colnames(pct_) <- c("assc", "sig_pos", "sig_neg", "insig_pos", "insig_neg")
    pct_$sig_pos <- freq_df[2,2] / sum(replace(freq_df[1:4,2], is.na(freq_df[1:4,2]), 0)) * 100
    pct_$sig_neg <- freq_df[1,2] / sum(replace(freq_df[1:4,2], is.na(freq_df[1:4,2]), 0)) * 100
    pct_$insig_pos <- freq_df[4,2] / sum(replace(freq_df[1:4,2], is.na(freq_df[1:4,2]), 0)) * 100
    pct_$insig_neg <- freq_df[3,2] / sum(replace(freq_df[1:4,2], is.na(freq_df[1:4,2]), 0)) * 100
    pct_$total <- sum(freq_df$Freq)
    pct_$assc <- i
    
    pct <- rbind(pct, pct_)
  }
  
  pct$gbd_i <- k
  
  df_name <- paste("cor_", k, sep = "")
  assign(df_name, pct)
}

cor_fin <- cbind(cor_0, cor_1, cor_2, cor_3, cor_4, cor_5, cor_6, cor_7)
cor_table <- rbind(cor_0, cor_1, cor_2, cor_3, cor_4, cor_5, cor_6, cor_7)
cor_table$pos <- cor_table$sig_pos + cor_table$insig_pos
cor_table$neg <- cor_table$sig_neg + cor_table$insig_neg

#total number of samples in the begining
length(unique(x$ID))
for (i in 1:7) {
  test <- subset(x, x$gbd_i==i)
  print(length(unique(test$ID)))
}

#Pie charts
for (i in c(0:7)) { #i: gbd
  data <- get(paste0("cor_", i))
  data <- data %>%
    dplyr::select(assc, sig_pos, insig_pos, sig_neg, insig_neg, total, gbd_i)
  
  for (j in c(1:6)) { #j: association
    pct <- as.numeric(data[j, 2:5])
    category <- names(data[, 2:5])
    df <- data.frame(pct = pct, category = category)
    df[5,] <- df[3,]
    df <- df[-3, ]
    
    df$category <- factor(df$category, levels = df$category)
    custom_colors <- c("dodgerblue3", alpha("dodgerblue3", alpha = 0.4), alpha("firebrick3", alpha = 0.4), "firebrick3")
    
    # Create a pie chart with custom colors
    par(mar = c(1, 1, 1, 1))
    p <- ggplot(data = df, aes(x = "", y = pct, fill = category)) +
      geom_bar(stat = "identity", color = "white", show.legend = FALSE) +
      scale_fill_manual(values = custom_colors) +
      coord_polar(theta = "y", start = 0) + 
      theme_void() +
      labs(title = NULL) +
      scale_y_reverse()
    ggsave(paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Study/Nature_cities/Revision_communications_E&E/Figures/Figure4/small_cor_", i, "_", j, ".pdf"), plot = p, width = 49, height = 49, units = "in")
    dev.off() 
    
  }}


#Large
#indexing each GBD super-region with numbers
gbd_list <- sort(unique(combined_large$GBDSuperRegion))
index <- c(1:7)
gbd_list <- as.data.frame(cbind(gbd_list, index))
gbd_list <- rename(gbd_list, GBDSuperRegion="gbd_list")
combined_large <- merge(combined_large, gbd_list, by="GBDSuperRegion")
list_small <- unique(combined_large$ID)

#seed
a <- subset(combined_large, ID==list_small[[1]])
title <- a[1,2]
gbd <- a[1,1]
a <- a[,c(4:7)]
num_vars <- ncol(a) 
cor_matrix <- matrix(NA, nrow = num_vars, ncol = num_vars) #empty frame
p_value_matrix <- matrix(NA, nrow = num_vars, ncol = num_vars) #empty frame
#cor matrix
for (i in 1:num_vars) { 
  for (j in 1:num_vars) {
    if ((sum(!is.na(a[, i])) >= 5) & (sum(!is.na(a[, j])) >= 5)) {   #skip with columns with many NAs
      cor_test_result <- cor.test(a[, i], a[, j], use = "pairwise.complete.obs")
      cor_matrix[i, j] <- cor_test_result$estimate
      p_value_matrix[i, j] <- cor_test_result$p.value
    }
  }
}

x <- data.frame(matrix(nrow = 6, ncol = 5)) #creating empty df for final table
colnames(x)[1] <- "coef"
colnames(x)[2] <- "p"
colnames(x)[3] <- "assc"
colnames(x)[4] <- "ID"
colnames(x)[5] <- "gbd"

x[1,1] <- cor_matrix[1,2]
x[2,1] <- cor_matrix[1,3]
x[3,1] <- cor_matrix[1,4]
x[4,1] <- cor_matrix[2,3]
x[5,1] <- cor_matrix[2,4]
x[6,1] <- cor_matrix[3,4]

x[1,2] <- p_value_matrix[1,2]
x[2,2] <- p_value_matrix[1,3]
x[3,2] <- p_value_matrix[1,4]
x[4,2] <- p_value_matrix[2,3]
x[5,2] <- p_value_matrix[2,4]
x[6,2] <- p_value_matrix[3,4]

x[1,3] <- "pm_no2"
x[2,3] <- "pm_o3"
x[3,3] <- "pm_co2"
x[4,3] <- "no2_o3"
x[5,3] <- "no2_co2"
x[6,3] <- "o3_co2"

x[,4] <- title
x[,5] <- gbd

#for loop
for (k in c(list_small[2:length(list_small)])) {
  
  a <- subset(combined_large, ID==k)
  title <- a[1,2]
  gbd <- a[1,1]
  a <- a[,c(4:7)]
  num_vars <- ncol(a) 
  cor_matrix <- matrix(NA, nrow = num_vars, ncol = num_vars) #empty frame
  p_value_matrix <- matrix(NA, nrow = num_vars, ncol = num_vars) #empty frame
  #cor matrix
  for (i in 1:num_vars) { 
    for (j in 1:num_vars) {
      if ((sum(!is.na(a[, i])) >= 5) & (sum(!is.na(a[, j])) >= 5)) {  #skip with columns with many NAs
        cor_test_result <- cor.test(a[, i], a[, j], use = "pairwise.complete.obs")
        cor_matrix[i, j] <- cor_test_result$estimate
        p_value_matrix[i, j] <- cor_test_result$p.value
      }
    }
  }
  
  x_ <- data.frame(matrix(nrow = 6, ncol = 5)) #creating empty df for final table
  colnames(x_)[1] <- "coef"
  colnames(x_)[2] <- "p"
  colnames(x_)[3] <- "assc"
  colnames(x_)[4] <- "ID"
  colnames(x_)[5] <- "gbd"
  
  x_[1,1] <- cor_matrix[1,2]
  x_[2,1] <- cor_matrix[1,3]
  x_[3,1] <- cor_matrix[1,4]
  x_[4,1] <- cor_matrix[2,3]
  x_[5,1] <- cor_matrix[2,4]
  x_[6,1] <- cor_matrix[3,4]
  
  x_[1,2] <- p_value_matrix[1,2]
  x_[2,2] <- p_value_matrix[1,3]
  x_[3,2] <- p_value_matrix[1,4]
  x_[4,2] <- p_value_matrix[2,3]
  x_[5,2] <- p_value_matrix[2,4]
  x_[6,2] <- p_value_matrix[3,4]
  
  x_[1,3] <- "pm_no2"
  x_[2,3] <- "pm_o3"
  x_[3,3] <- "pm_co2"
  x_[4,3] <- "no2_o3"
  x_[5,3] <- "no2_co2"
  x_[6,3] <- "o3_co2"
  
  x_[,4] <- title
  x_[,5] <- gbd
  
  x <- rbind(x, x_)
  
}
x$assc_i <- 0
x$assc_i <- ifelse(x$assc=="pm_no2", 1, x$assc_i)
x$assc_i <- ifelse(x$assc=="pm_o3", 2, x$assc_i)
x$assc_i <- ifelse(x$assc=="pm_co2", 3, x$assc_i)
x$assc_i <- ifelse(x$assc=="no2_o3", 4, x$assc_i)
x$assc_i <- ifelse(x$assc=="no2_co2", 5, x$assc_i)
x$assc_i <- ifelse(x$assc=="o3_co2", 6, x$assc_i)

x$sig <- ifelse(x$p <= 0.05 & x$coef >= 0, "o+",
                ifelse(x$p <= 0.05 & x$coef < 0, "o-",
                       ifelse(x$p > 0.05 & x$coef >= 0, "x+",
                              ifelse(x$p > 0.05 & x$coef < 0, "x-", NA))))

x$gbd_i <- 0
x$gbd_i <- ifelse(x$gbd=="Central Europe, Eastern Europe, and Central Asia", 1, x$gbd_i)
x$gbd_i <- ifelse(x$gbd=="High income", 2, x$gbd_i)
x$gbd_i <- ifelse(x$gbd=="Latin America and Caribbean", 3, x$gbd_i)
x$gbd_i <- ifelse(x$gbd=="North Africa and Middle East", 4, x$gbd_i)
x$gbd_i <- ifelse(x$gbd=="South Asia", 5, x$gbd_i)
x$gbd_i <- ifelse(x$gbd=="Southeast Asia, East Asia, and Oceania", 6, x$gbd_i)
x$gbd_i <- ifelse(x$gbd=="Sub-Saharan Africa", 7, x$gbd_i)

#FREQ TABLE
#GLOBAL
#corr
#seed
y<-x
i=1
test <- subset(y, y$assc_i==i)
freq_table <- table(test$sig)
freq_table <- as.data.frame(freq_table)
freq_df <- data.frame(
  Var1 = c("o-", "o+", "x-", "x+")
)
freq_df <- merge(freq_df, freq_table, by = "Var1", all = TRUE)
freq_df[is.na(freq_df)] <- 0

pct <- data.frame(matrix(nrow = 1, ncol = 5)) #empty frame
colnames(pct) <- c("assc", "sig_pos", "sig_neg", "insig_pos", "insig_neg")
pct$sig_pos <- freq_df[2,2] / sum(replace(freq_df[1:4,2], is.na(freq_df[1:4,2]), 0)) * 100
pct$sig_neg <- freq_df[1,2] / sum(replace(freq_df[1:4,2], is.na(freq_df[1:4,2]), 0)) * 100
pct$insig_pos <- freq_df[4,2] / sum(replace(freq_df[1:4,2], is.na(freq_df[1:4,2]), 0)) * 100
pct$insig_neg <- freq_df[3,2] / sum(replace(freq_df[1:4,2], is.na(freq_df[1:4,2]), 0)) * 100
pct$total <- sum(freq_df$Freq)
pct$assc <- i

#for loop
for (i in c(2:6)) {
  test <- subset(y, y$assc_i==i)
  freq_table <- table(test$sig)
  freq_table <- as.data.frame(freq_table)
  freq_df <- data.frame(
    Var1 = c("o-", "o+", "x-", "x+")
  )
  freq_df <- merge(freq_df, freq_table, by = "Var1", all = TRUE)
  freq_df[is.na(freq_df)] <- 0
  
  pct_ <- data.frame(matrix(nrow = 1, ncol = 5)) #empty frame
  colnames(pct_) <- c("assc", "sig_pos", "sig_neg", "insig_pos", "insig_neg")
  pct_$sig_pos <- freq_df[2,2] / sum(replace(freq_df[1:4,2], is.na(freq_df[1:4,2]), 0)) * 100
  pct_$sig_neg <- freq_df[1,2] / sum(replace(freq_df[1:4,2], is.na(freq_df[1:4,2]), 0)) * 100
  pct_$insig_pos <- freq_df[4,2] / sum(replace(freq_df[1:4,2], is.na(freq_df[1:4,2]), 0)) * 100
  pct_$insig_neg <- freq_df[3,2] / sum(replace(freq_df[1:4,2], is.na(freq_df[1:4,2]), 0)) * 100
  pct_$total <- sum(freq_df$Freq)
  pct_$assc <- i
  
  pct <- rbind(pct, pct_)
}
pct$gbd_i <- 0
df_name <- paste("cor_0", sep = "")
assign(df_name, pct)

#EACH GBD
for (k in 1:7) {
  #subset
  y <- subset(x, x$gbd_i==k)
  #corr
  #seed
  i=1
  test <- subset(y, y$assc_i==i)
  freq_table <- table(test$sig)
  freq_table <- as.data.frame(freq_table)
  freq_df <- data.frame(
    Var1 = c("o-", "o+", "x-", "x+")
  )
  freq_df <- merge(freq_df, freq_table, by = "Var1", all = TRUE)
  freq_df[is.na(freq_df)] <- 0
  
  pct <- data.frame(matrix(nrow = 1, ncol = 5)) #empty frame
  colnames(pct) <- c("assc", "sig_pos", "sig_neg", "insig_pos", "insig_neg")
  pct$sig_pos <- freq_df[2,2] / sum(replace(freq_df[1:4,2], is.na(freq_df[1:4,2]), 0)) * 100
  pct$sig_neg <- freq_df[1,2] / sum(replace(freq_df[1:4,2], is.na(freq_df[1:4,2]), 0)) * 100
  pct$insig_pos <- freq_df[4,2] / sum(replace(freq_df[1:4,2], is.na(freq_df[1:4,2]), 0)) * 100
  pct$insig_neg <- freq_df[3,2] / sum(replace(freq_df[1:4,2], is.na(freq_df[1:4,2]), 0)) * 100
  pct$total <- sum(freq_df$Freq)
  pct$assc <- i
  
  #for loop
  for (i in c(2:6)) {
    test <- subset(y, y$assc_i==i)
    freq_table <- table(test$sig)
    freq_table <- as.data.frame(freq_table)
    freq_df <- data.frame(
      Var1 = c("o-", "o+", "x-", "x+")
    )
    freq_df <- merge(freq_df, freq_table, by = "Var1", all = TRUE)
    freq_df[is.na(freq_df)] <- 0
    
    pct_ <- data.frame(matrix(nrow = 1, ncol = 5)) #empty frame
    colnames(pct_) <- c("assc", "sig_pos", "sig_neg", "insig_pos", "insig_neg")
    pct_$sig_pos <- freq_df[2,2] / sum(replace(freq_df[1:4,2], is.na(freq_df[1:4,2]), 0)) * 100
    pct_$sig_neg <- freq_df[1,2] / sum(replace(freq_df[1:4,2], is.na(freq_df[1:4,2]), 0)) * 100
    pct_$insig_pos <- freq_df[4,2] / sum(replace(freq_df[1:4,2], is.na(freq_df[1:4,2]), 0)) * 100
    pct_$insig_neg <- freq_df[3,2] / sum(replace(freq_df[1:4,2], is.na(freq_df[1:4,2]), 0)) * 100
    pct_$total <- sum(freq_df$Freq)
    pct_$assc <- i
    
    pct <- rbind(pct, pct_)
  }
  
  pct$gbd_i <- k
  
  df_name <- paste("cor_", k, sep = "")
  assign(df_name, pct)
}

cor_fin <- cbind(cor_0, cor_1, cor_2, cor_3, cor_4, cor_5, cor_6, cor_7)
cor_table <- rbind(cor_0, cor_1, cor_2, cor_3, cor_4, cor_5, cor_6, cor_7)
cor_table$pos <- cor_table$sig_pos + cor_table$insig_pos
cor_table$neg <- cor_table$sig_neg + cor_table$insig_neg

#total number of samples in the begining
length(unique(x$ID))
for (i in 1:7) {
  test <- subset(x, x$gbd_i==i)
  print(length(unique(test$ID)))
}

#Pie charts
for (i in c(0:7)) { #i: gbd
  data <- get(paste0("cor_", i))
  data <- data %>%
    dplyr::select(assc, sig_pos, insig_pos, sig_neg, insig_neg, total, gbd_i)
  
  for (j in c(1:6)) { #j: association
    pct <- as.numeric(data[j, 2:5])
    category <- names(data[, 2:5])
    df <- data.frame(pct = pct, category = category)
    df[5,] <- df[3,]
    df <- df[-3, ]
    
    df$category <- factor(df$category, levels = df$category)
    custom_colors <- c("dodgerblue3", alpha("dodgerblue3", alpha = 0.4), alpha("firebrick3", alpha = 0.4), "firebrick3")
    
    # Create a pie chart with custom colors
    par(mar = c(1, 1, 1, 1))
    p <- ggplot(data = df, aes(x = "", y = pct, fill = category)) +
      geom_bar(stat = "identity", color = "white", show.legend = FALSE) +
      scale_fill_manual(values = custom_colors) +
      coord_polar(theta = "y", start = 0) + 
      theme_void() +
      labs(title = NULL) +
      scale_y_reverse()
    ggsave(paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Study/Nature_cities/Revision_communications_E&E/Figures/Figure4/large_cor_", i, "_", j, ".pdf"), plot = p, width = 49, height = 49, units = "in")
    dev.off() 
    
  }}




#################################################
####### GHS/C40 Comparison: Figure 5 & 6 ########
#################################################
#data
pm25_new<-readRDS(file="/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/etc/corx.rds")
no2_new<-readRDS(file="/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/etc/cory.rds")
o3_new<-readRDS(file="/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/etc/corz.rds")
o3_new <- subset(o3_new, !is.na(o3_new$pwo3_13k))
o3_new <- subset(o3_new, !is.na(o3_new$pwo3_c40))
co2_new<-readRDS(file="/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/etc/corw.rds")

#scatter plot
#PM2.5
pm25 <- subset(pm25_new, pm25_new$year==2019)
a <- cor.test(pm25$pwpm_13k, pm25$pwpm_c40, method = "pearson")
est <- round(a$estimate, 4)
p <- round(a$p.value, 4)
p <- ifelse(p==0, "< 0.0001", paste0("=", p))
regression_line <- lm(pwpm_c40 ~ pwpm_13k, data = pm25)

pdf(file = paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Study/Nature_cities/Figures/Figure5/Figure5_A.pdf"),
    width = 10,  # width in inches
    height = 10) # height in inches
par(family = "Lato")
ggplot(pm25, aes(x = pwpm_13k, y = pwpm_c40)) +
  geom_point(color = "blue4", alpha = 0.5, size = 3.5) +  # Adjust the points
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black", size = 0.7) + 
  annotate("text", x = quantile(range(pm25$pwpm_13k), 0.3), y = quantile(range(pm25$pwpm_c40), 0.95), 
           label = paste("r =", est, ", p", p, "\ny=", round(regression_line$coefficients[1], 2), "+", round(regression_line$coefficients[2], 3), "x"), 
           hjust = .5, size = 8, color = "blue4") +
  xlab(bquote(PM[2.5] * " concentration from GHS-SMOD definition (" * mu * "g/m"^3 * ")")) +
  ylab(bquote(PM[2.5] * " concentration from C40 Cities definition (" * mu * "g/m"^3 * ")")) +
  theme(
    axis.text = element_text(size = 20),
    axis.title.y = element_text(size = 25, margin = margin(r = 20)),  
    axis.title.x = element_text(size = 25, margin = margin(t = 20)),
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))
dev.off()

#NO2
no2 <- subset(no2_new, no2_new$year==2019)
a <- cor.test(no2$pwno2_13k, no2$pwno2_c40, method = "pearson")
est <- round(a$estimate, 4)
p <- round(a$p.value, 4)
p <- ifelse(p==0, "< 0.0001", paste0("=", p))
regression_line <- lm(pwno2_c40 ~ pwno2_13k, data = no2)

pdf(file = paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Study/Nature_cities/Figures/Figure5/Figure5_B.pdf"),
    width = 10,  # width in inches
    height = 10) # height in inches
par(family = "Lato")
ggplot(no2, aes(x = pwno2_13k, y = pwno2_c40)) +
  geom_point(color = "blue4", alpha = 0.5, size = 3.5) +  # Adjust the points
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black", size = 0.7) + 
  annotate("text", x = quantile(range(no2$pwno2_13k), 0.3), y = quantile(range(no2$pwno2_c40), 0.95), label = paste("r =", est, ", p", p, "\ny=", round(regression_line$coefficients[1],2), "+", round(regression_line$coefficients[2],3), "x"), hjust = .5, size=8, color = "blue4") +
  xlab(bquote(NO[2] * " concentration from GHS-SMOD definition (ppb)")) +
  ylab(bquote(NO[2] * " concentration from C40 Cities definition (ppb)")) +
  theme(
    axis.text = element_text(size = 20),
    axis.title.y = element_text(size = 25, margin = margin(r = 20)),  
    axis.title.x = element_text(size = 25, margin = margin(t = 20)),  
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))
dev.off()

#O3
o3 <- subset(o3_new, o3_new$year==2019)
a <- cor.test(o3$pwo3_13k, o3$pwo3_c40, method = "pearson")
est <- round(a$estimate, 4)
p <- round(a$p.value, 4)
p <- ifelse(p==0, "< 0.0001", paste0("=", p))
regression_line <- lm(pwo3_c40 ~ pwo3_13k, data = o3)

pdf(file = paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Study/Nature_cities/Figures/Figure5/Figure5_C.pdf"),
    width = 10,  # width in inches
    height = 10) # height in inches
par(family = "Lato")
ggplot(o3, aes(x = pwo3_13k, y = pwo3_c40)) +
  geom_point(color = "blue4", alpha = 0.5, size = 3.5) +  # Adjust the points
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black", size = 0.7) + 
  annotate("text", x = quantile(range(o3$pwo3_13k), 0.3), y = quantile(range(o3$pwo3_c40), 0.95), label = paste("r =", est, ", p", p, "\ny=", round(regression_line$coefficients[1],2), "+", round(regression_line$coefficients[2],3), "x"), hjust = .5, size=8, color="blue4") +
  xlab(bquote(O[3] * " concentration from GHS-SMOD definition (ppb)")) +
  ylab(bquote(O[3] * " concentration from C40 Cities definition (ppb)")) +
  theme(
    axis.text = element_text(size = 20),
    axis.title.y = element_text(size = 25, margin = margin(r = 20)),  # Adjust the margin here
    axis.title.x = element_text(size = 25, margin = margin(t = 20)),
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))
dev.off()

#CO2
co2 <- subset(co2_new, co2_new$year==2019)
a <- cor.test(co2$co2_per_cap_13k, co2$co2_per_cap_c40, method = "pearson")
est <- round(a$estimate, 4)
p <- round(a$p.value, 4)
p <- ifelse(p==0, "< 0.0001", paste0("=", p))
regression_line <- lm(co2_per_cap_c40 ~ co2_per_cap_13k, data = co2)

pdf(file = paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Study/Nature_cities/Figures/Figure5/Figure5_D.pdf"),
    width = 10,  # width in inches
    height = 10) # height in inches
par(family = "Lato")
ggplot(co2, aes(x = co2_per_cap_13k, y = co2_per_cap_c40)) +
  geom_point(color = "blue4", alpha = 0.5, size = 3.5) +  # Adjust the points
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black", size = 0.7) + 
  annotate("text", x = quantile(range(co2$co2_per_cap_13k), 0.3), y = quantile(range(co2$co2_per_cap_c40), 0.85), label = paste("r =", est, ", p", p, "\ny=", round(regression_line$coefficients[1],2), "+", round(regression_line$coefficients[2],3), "x"), hjust = .5, size=8, color="blue4") +
  xlab(bquote(FFCO[2] * " per capita from GHS-SMOD definition (metric tons)")) +
  ylab(bquote(FFCO[2] * " per capita from C40 Cities definition (metric tons)")) +
  theme(
    axis.text = element_text(size = 20),
    axis.title.y = element_text(size = 24, margin = margin(r = 20)), 
    axis.title.x = element_text(size = 24, margin = margin(t = 20)),
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))
dev.off()



# % change (2005-2019)
#pm25
pm25_05 <- subset(pm25_new, (pm25_new$year>=2005 & pm25_new$year<=2007))
pm25_05 <- pm25_05 %>% group_by(id_13k, city) %>% summarize(mean_13k_05=mean(pwpm_13k), mean_c40_05=mean(pwpm_c40))
pm25_19 <- subset(pm25_new, (pm25_new$year>=2017 & pm25_new$year<=2019))
pm25_19 <- pm25_19 %>% group_by(id_13k, city) %>% summarize(mean_13k_19=mean(pwpm_13k), mean_c40_19=mean(pwpm_c40))

pm25_pct <- merge(pm25_05, pm25_19, by=c("id_13k", "city"))
pm25_pct$pct_chg_13k <- -(pm25_pct$mean_13k_05-pm25_pct$mean_13k_19)/pm25_pct$mean_13k_05 *100
pm25_pct$pct_chg_c40 <- -(pm25_pct$mean_c40_05-pm25_pct$mean_c40_19)/pm25_pct$mean_c40_05 *100
#no2
no2_05 <- subset(no2_new, (no2_new$year>=2005 & no2_new$year<=2007))
no2_05 <- no2_05 %>% group_by(id_13k, city) %>% summarize(mean_13k_05=mean(pwno2_13k), mean_c40_05=mean(pwno2_c40))
no2_19 <- subset(no2_new, (no2_new$year>=2017 & no2_new$year<=2019))
no2_19 <- no2_19 %>% group_by(id_13k, city) %>% summarize(mean_13k_19=mean(pwno2_13k), mean_c40_19=mean(pwno2_c40))

no2_pct <- merge(no2_05, no2_19, by=c("id_13k", "city"))
no2_pct$pct_chg_13k <- -(no2_pct$mean_13k_05-no2_pct$mean_13k_19)/no2_pct$mean_13k_05 *100
no2_pct$pct_chg_c40 <- -(no2_pct$mean_c40_05-no2_pct$mean_c40_19)/no2_pct$mean_c40_05 *100
#o3
o3_05 <- subset(o3_new, (o3_new$year>=2005 & o3_new$year<=2007))
o3_05 <- o3_05 %>% group_by(id_13k, city) %>% summarize(mean_13k_05=mean(pwo3_13k), mean_c40_05=mean(pwo3_c40))
o3_19 <- subset(o3_new, (o3_new$year>=2017 & o3_new$year<=2019))
o3_19 <- o3_19 %>% group_by(id_13k, city) %>% summarize(mean_13k_19=mean(pwo3_13k), mean_c40_19=mean(pwo3_c40))

o3_pct <- merge(o3_05, o3_19, by=c("id_13k", "city"))
o3_pct$pct_chg_13k <- -(o3_pct$mean_13k_05-o3_pct$mean_13k_19)/o3_pct$mean_13k_05 *100
o3_pct$pct_chg_c40 <- -(o3_pct$mean_c40_05-o3_pct$mean_c40_19)/o3_pct$mean_c40_05 *100
#co2
co2_05 <- subset(co2_new, (co2_new$year>=2005 & co2_new$year<=2007))
co2_05 <- co2_05 %>% group_by(id_13k, city) %>% summarize(mean_13k_05=mean(co2_per_cap_13k), mean_c40_05=mean(co2_per_cap_c40))
co2_19 <- subset(co2_new, (co2_new$year>=2017 & co2_new$year<=2019))
co2_19 <- co2_19 %>% group_by(id_13k, city) %>% summarize(mean_13k_19=mean(co2_per_cap_13k), mean_c40_19=mean(co2_per_cap_c40))

co2_pct <- merge(co2_05, co2_19, by=c("id_13k", "city"))
co2_pct$pct_chg_13k <- -(co2_pct$mean_13k_05-co2_pct$mean_13k_19)/co2_pct$mean_13k_05 *100
co2_pct$pct_chg_c40 <- -(co2_pct$mean_c40_05-co2_pct$mean_c40_19)/co2_pct$mean_c40_05 *100

#scatter plot
#PM2.5
a <- cor.test(pm25_pct$pct_chg_13k, pm25_pct$pct_chg_c40, method = "pearson")
est <- round(a$estimate, 4)
p <- round(a$p.value, 4)
p <- ifelse(p==0, "< 0.0001", paste0("=", p))
regression_line <- lm(pct_chg_c40 ~ pct_chg_13k, data = pm25_pct)

pdf(file = paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Study/Nature_cities/Figures/Figure6/Figure6_A.pdf"),
    width = 10,  # width in inches
    height = 10) # height in inches
par(family = "Lato")
ggplot(pm25_pct, aes(x = pct_chg_13k, y = pct_chg_c40)) +
  geom_point(color = "blue4", alpha = 0.5, size = 3.5) +  # Adjust the points
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black", size = 0.7) + 
  annotate("text", x = quantile(range(pm25_pct$pct_chg_13k), 0.3), y = quantile(range(pm25_pct$pct_chg_c40), 0.85), label = paste("r =", est, ", p", p, "\ny=", round(regression_line$coefficients[1],2), "+", round(regression_line$coefficients[2],3), "x"), hjust = .5, size=8, color="blue4") +
  xlab(bquote(PM[2.5] * " % change from GHS-SMOD definition")) +
  ylab(bquote(PM[2.5] * " % change from C40 Cities definition")) +
  theme(axis.text = element_text(size = 20),
        axis.title.y = element_text(size = 25, margin = margin(r = 20)),
        axis.title.x = element_text(size = 25, margin = margin(t = 20)),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))
dev.off()

#NO2
a <- cor.test(no2_pct$pct_chg_13k, no2_pct$pct_chg_c40, method = "pearson")
est <- round(a$estimate, 4)
p <- round(a$p.value, 4)
p <- ifelse(p==0, "< 0.0001", paste0("=", p))
regression_line <- lm(pct_chg_c40 ~ pct_chg_13k, data = no2_pct)

pdf(file = paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Study/Nature_cities/Figures/Figure6/Figure6_B.pdf"),
    width = 10,  # width in inches
    height = 10) # height in inches
par(family = "Lato")
ggplot(no2_pct, aes(x = pct_chg_13k, y = pct_chg_c40)) +
  geom_point(color = "blue4", alpha = 0.5, size = 3.5) +  # Adjust the points
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black", size = 0.7) + 
  annotate("text", x = quantile(range(no2_pct$pct_chg_13k), 0.3), y = quantile(range(no2_pct$pct_chg_c40), 0.85), label = paste("r =", est, ", p", p, "\ny=", round(regression_line$coefficients[1],2), "+", round(regression_line$coefficients[2],3), "x"), hjust = .5, size=8, color="blue4") +
  xlab(bquote(NO[2] * " % change from GHS-SMOD definition")) +
  ylab(bquote(NO[2] * " % change from C40 Cities definition")) +
  theme(axis.text = element_text(size = 20),
        axis.title.y = element_text(size = 25, margin = margin(r = 20)),
        axis.title.x = element_text(size = 25, margin = margin(t = 20)),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))
dev.off()

#O3
a <- cor.test(o3_pct$pct_chg_13k, o3_pct$pct_chg_c40, method = "pearson")
est <- round(a$estimate, 4)
p <- round(a$p.value, 4)
p <- ifelse(p==0, "< 0.0001", paste0("=", p))
regression_line <- lm(pct_chg_c40 ~ pct_chg_13k, data = o3_pct)

pdf(file = paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Study/Nature_cities/Figures/Figure6/Figure6_C.pdf"),
    width = 10,  # width in inches
    height = 10) # height in inches
par(family = "Lato")
ggplot(o3_pct, aes(x = pct_chg_13k, y = pct_chg_c40)) +
  geom_point(color = "blue4", alpha = 0.5, size = 3.5) +  # Adjust the points
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black", size = 0.7) + 
  annotate("text", x = quantile(range(o3_pct$pct_chg_13k), 0.3), y = quantile(range(o3_pct$pct_chg_c40), 0.85), label = paste("r =", est, ", p", p, "\ny=", round(regression_line$coefficients[1],2), "+", round(regression_line$coefficients[2],3), "x"), hjust = .5, size=8, color="blue4") +
  xlab(bquote(O[3] * " % change from GHS-SMOD definition")) +
  ylab(bquote(O[3] * " % change from C40 Cities definition")) +
  theme(axis.text = element_text(size = 20),
        axis.title.y = element_text(size = 25, margin = margin(r = 20)),
        axis.title.x = element_text(size = 25, margin = margin(t = 20)),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))
dev.off()

#CO2
a <- cor.test(co2_pct$pct_chg_13k, co2_pct$pct_chg_c40, method = "pearson")
est <- round(a$estimate, 4)
p <- round(a$p.value, 4)
p <- ifelse(p==0, "< 0.0001", paste0("=", p))
regression_line <- lm(pct_chg_c40 ~ pct_chg_13k, data = co2_pct)

pdf(file = paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Study/Nature_cities/Figures/Figure6/Figure6_D.pdf"),
    width = 10,  # width in inches
    height = 10) # height in inches
par(family = "Lato")
ggplot(co2_pct, aes(x = pct_chg_13k, y = pct_chg_c40)) +
  geom_point(color = "blue4", alpha = 0.5, size = 3.5) +  # Adjust the points
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black", size = 0.7) + 
  annotate("text", x = quantile(range(co2_pct$pct_chg_13k), 0.3), y = quantile(range(co2_pct$pct_chg_c40), 0.85), label = paste("r =", est, ", p", p, "\ny=", round(regression_line$coefficients[1],2),"+", round(regression_line$coefficients[2],3), "x"), hjust = .5, size=8, color="blue4") +
  xlab(bquote(FFCO[2] * " per capita % change from GHS-SMOD definition")) +
  ylab(bquote(FFCO[2] * " per capita % change from C40 Cities definition")) +
  theme(axis.text = element_text(size = 20),
        axis.title.y = element_text(size = 24, margin = margin(r = 20)),
        axis.title.x = element_text(size = 24, margin = margin(t = 20)),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))
dev.off()

#CO2: percent changes, excluding an outlier (c40 % change <= 300%)
co2_pct_ <- subset(co2_pct, co2_pct$pct_chg_c40<=300)
a <- cor.test(co2_pct_$pct_chg_13k, co2_pct_$pct_chg_c40, method = "pearson")
est <- round(a$estimate, 4)
p <- round(a$p.value, 4)
p <- ifelse(p==0, "<0.0001", paste0("=", p))
regression_line <- lm(pct_chg_c40 ~ pct_chg_13k, data = co2_pct_)

ggplot(co2_pct_, aes(x = pct_chg_13k, y = pct_chg_c40)) +
  geom_point(color = "blue4", alpha = 0.5, size = 3.5) +  # Adjust the points
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black", size = 0.7) + 
  annotate("text", x = quantile(range(co2_pct_$pct_chg_13k), 0.3), y = quantile(range(co2_pct_$pct_chg_c40), 0.85), label = paste("r =", est, ", p", p, "\ny=", round(regression_line$coefficients[1],2),"+", round(regression_line$coefficients[2],3), "x"), hjust = .5, size=8, color="blue4") +
  xlab(bquote(FFCO[2] * " per capita % change from GHS-SMOD definition")) +
  ylab(bquote(FFCO[2] * " per capita % change from C40 Cities definition")) +
  theme(axis.text = element_text(size = 20),
        axis.title.y = element_text(size = 25, margin = margin(r = 20),  family="Lato"),
        axis.title.x = element_text(size = 25, margin = margin(t = 20), family="Lato"))



#######################################
#### GHS/C40 boundary comparison ######
#######################################
#Compare boundaries in maps
aa <- subset(pm25_new, year==2019)

#1-48th 
plot_list <- list()

for (i in c(1:48)) {
  
  aaa <- aa[i,]
  cityname <- aaa$city
  
  p <- ggplot() +
    geom_sf(data = aaa$geo_13k, fill = alpha('turquoise', 0.5), color = NA) +
    geom_sf(data = aaa$geo_c40, fill = alpha('lightsalmon', 0.5), color = NA) +
    theme_void() +
    theme(plot.title = element_text(family = "Lato", size = 10, hjust = 0.5, face="bold"),
          plot.background = element_rect(fill = "white", color = NA), panel.border = element_blank()) +
    labs(title = paste0(cityname, "\n")) 
  
  plot_list[[i]] <- p
  
}

combined_plot <- grid.arrange(grobs = plot_list, nrow = 8, ncol = 6)

ggsave(filename = "/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Study/Nature_cities/Figures/FigureS4/FigureS4_1.png",
       plot = combined_plot,
       width = 8.5,  # Adjust as needed
       height = 11, # Adjust as needed
       dpi = 1000)  # Adjust as needed


#49-92nd
plot_list <- list()

for (i in c(49:92)) {
  
  aaa <- aa[i,]
  cityname <- aaa$city
  
  p <- ggplot() +
    geom_sf(data = aaa$geo_13k, fill = alpha('turquoise', 0.5), color = NA) +
    geom_sf(data = aaa$geo_c40, fill = alpha('lightsalmon', 0.5), color = NA) +
    theme_void() +
    theme(plot.title = element_text(family = "Lato", size = 10, hjust = 0.5, face="bold"),
          plot.background = element_rect(fill = "white", color = NA), panel.border = element_blank()) +
    labs(title = paste0(cityname, "\n")) 
  
  plot_list[[i-48]] <- p
  
}

combined_plot <- grid.arrange(grobs = plot_list, nrow = 8, ncol = 6)

# Save the combined plot
ggsave(filename = "/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Study/Adv_Sci/Co-authors/Figures/FigureS4_2.png",
       plot = combined_plot,
       width = 8.5,  # Adjust as needed
       height = 11, # Adjust as needed
       dpi = 1000)  # Adjust as needed





############################################################
####### Correlation analysis: Table S1-S3, Figure S2 #######
############################################################
pm25_new<-readRDS(file="/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/etc/corx1.rds")
no2_new<-readRDS(file="/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/etc/cory1.rds")
o3_new<-readRDS(file="/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/etc/corz1.rds")
o3_new <- subset(o3_new, !is.na(o3_new$pwo3_13k))
o3_new <- subset(o3_new, !is.na(o3_new$pwo3_c40))
co2_new<-readRDS(file="/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/etc/corw1.rds")

#summary statistics
a <- pm25_new %>% summarise(n=n(), pm25_13k_mean=mean(pwpm_13k), pm25_13k_sd=sd(pwpm_13k), pm25_13k_median=median(pwpm_13k), pm25_13k_min=min(pwpm_13k), pm25_13k_max=max(pwpm_13k), pm25_c40_mean=mean(pwpm_c40), pm25_c40_sd=sd(pwpm_c40), pm25_c40_median=median(pwpm_c40), pm25_c40_min=min(pwpm_c40), pm25_c40_max=max(pwpm_c40))
a <- t(a)

a1 <- no2_new %>% summarise(n=n(), no2_13k_mean=mean(pwno2_13k), no2_13k_sd=sd(pwno2_13k), no2_13k_median=median(pwno2_13k), no2_13k_min=min(pwno2_13k), no2_13k_max=max(pwno2_13k), no2_c40_mean=mean(pwno2_c40), no2_c40_sd=sd(pwno2_c40), no2_c40_median=median(pwno2_c40), no2_c40_min=min(pwno2_c40), no2_c40_max=max(pwno2_c40))
a1 <- t(a1)

a2 <- o3_new %>% summarise(n=n(), o3_13k_mean=mean(pwo3_13k), o3_13k_sd=sd(pwo3_13k), o3_13k_median=median(pwo3_13k), o3_13k_min=min(pwo3_13k), o3_13k_max=max(pwo3_13k), o3_c40_mean=mean(pwo3_c40), o3_c40_sd=sd(pwo3_c40), o3_c40_median=median(pwo3_c40), o3_c40_min=min(pwo3_c40), o3_c40_max=max(pwo3_c40))
a2 <- t(a2)

a3 <- co2_new %>% summarise(n=n(), co2_13k_mean=mean(co2_per_cap_13k), co2_13k_sd=sd(co2_per_cap_13k), co2_13k_median=median(co2_per_cap_13k), co2_13k_min=min(co2_per_cap_13k), co2_13k_max=max(co2_per_cap_13k), co2_c40_mean=mean(co2_per_cap_c40), co2_c40_sd=sd(co2_per_cap_c40), co2_c40_median=median(co2_per_cap_c40), co2_c40_min=min(co2_per_cap_c40), co2_c40_max=max(co2_per_cap_c40))
a3 <- t(a3)

a4 <- cbind(a, a1, a2, a3)

#WHO attainment
#PM2.5
a<-pm25_new
a$exc_c40 <- ifelse(a$pwpm_c40>5, 1, 0) #AQG: 5
a$exc_13k <- ifelse(a$pwpm_13k>5, 1, 0)
#a$exc_ui <- ifelse(a$pm25_ui>5, 1, 0)
a$exc_c40_IT4 <- ifelse(a$pwpm_c40>10, 1, 0) #IT4: 10
a$exc_13k_IT4 <- ifelse(a$pwpm_13k>10, 1, 0)
#a$exc_ui_IT4 <- ifelse(a$pm25_ui>10, 1, 0)
a$exc_c40_IT3 <- ifelse(a$pwpm_c40>15, 1, 0) #IT3: 15
a$exc_13k_IT3 <- ifelse(a$pwpm_13k>15, 1, 0)
#a$exc_ui_IT3 <- ifelse(a$pm25_ui>15, 1, 0)
a$exc_c40_IT2 <- ifelse(a$pwpm_c40>25, 1, 0) #IT2: 25
a$exc_13k_IT2 <- ifelse(a$pwpm_13k>25, 1, 0)
#a$exc_ui_IT2 <- ifelse(a$pm25_ui>25, 1, 0)
a$exc_c40_IT1 <- ifelse(a$pwpm_c40>35, 1, 0) #IT1: 35
a$exc_13k_IT1 <- ifelse(a$pwpm_13k>35, 1, 0)
#a$exc_ui_IT1 <- ifelse(a$pm25_ui>35, 1, 0)

exc_c40 <- sum(a$exc_c40) 
exc_13k <- sum(a$exc_13k) 
#exc_ui <- sum(a$exc_ui) #88
exc_c40_IT4 <- sum(a$exc_c40_IT4) 
exc_13k_IT4 <- sum(a$exc_13k_IT4) 
#exc_ui_IT4 <- sum(a$exc_ui_IT4) #60
exc_c40_IT3 <- sum(a$exc_c40_IT3) 
exc_13k_IT3 <- sum(a$exc_13k_IT3) 
#exc_ui_IT3 <- sum(a$exc_ui_IT3) #47
exc_c40_IT2 <- sum(a$exc_c40_IT2) 
exc_13k_IT2 <- sum(a$exc_13k_IT2) 
#exc_ui_IT2 <- sum(a$exc_ui_IT2) #29
exc_c40_IT1 <- sum(a$exc_c40_IT1) 
exc_13k_IT1 <- sum(a$exc_13k_IT1) 
#exc_ui_IT1 <- sum(a$exc_ui_IT1) #12

aa <- data.frame(exc_c40, exc_c40_IT4, exc_c40_IT3, exc_c40_IT2, exc_c40_IT1)
aa <- t(aa)
colnames(aa)[1] <- "C40"

aaa <- data.frame(exc_13k, exc_13k_IT4, exc_13k_IT3, exc_13k_IT2, exc_13k_IT1)
aaa <- t(aaa)
colnames(aaa)[1] <- "GHS-SMOD"

#aaaa <- data.frame(exc_ui, exc_ui_IT4, exc_ui_IT3, exc_ui_IT2, exc_ui_IT1)
#aaaa <- t(aaaa)
#colnames(aaaa)[1] <- "UI"

aaaaa<- cbind(aaa, aa)

#NO2
a<-no2_new
a$exc_c40 <- ifelse(a$pwno2_c40>5.14, 1, 0) #AQG: 10 = 5.14 ppb
a$exc_13k <- ifelse(a$pwno2_13k>5.14, 1, 0)
#a$exc_ui <- ifelse(a$no2_ui>5.14, 1, 0)
a$exc_c40_IT3 <- ifelse(a$pwno2_c40>10.63, 1, 0) #IT3: 20 = 10.63 ppb
a$exc_13k_IT3 <- ifelse(a$pwno2_13k>10.63, 1, 0)
#a$exc_ui_IT3 <- ifelse(a$no2_ui>10.63, 1, 0)
a$exc_c40_IT2 <- ifelse(a$pwno2_c40>15.95, 1, 0) #IT2: 30 = 15.95 ppb
a$exc_13k_IT2 <- ifelse(a$pwno2_13k>15.95, 1, 0)
#a$exc_ui_IT2 <- ifelse(a$no2_ui>15.95, 1, 0)
a$exc_c40_IT1 <- ifelse(a$pwno2_c40>21.27, 1, 0) #IT1: 40 = 21.27 ppb
a$exc_13k_IT1 <- ifelse(a$pwno2_13k>21.27, 1, 0)
#a$exc_ui_IT1 <- ifelse(a$no2_ui>21.27, 1, 0)

exc_c40 <- sum(a$exc_c40) #84
exc_13k <- sum(a$exc_13k) #88
#exc_ui <- sum(a$exc_ui) #88
exc_c40_IT3 <- sum(a$exc_c40_IT3) #53
exc_13k_IT3 <- sum(a$exc_13k_IT3) #72
#exc_ui_IT3 <- sum(a$exc_ui_IT3) #84
exc_c40_IT2 <- sum(a$exc_c40_IT2) #14
exc_13k_IT2 <- sum(a$exc_13k_IT2) #13
#exc_ui_IT2 <- sum(a$exc_ui_IT2) #53
exc_c40_IT1 <- sum(a$exc_c40_IT1) #3
exc_13k_IT1 <- sum(a$exc_13k_IT1) #0
#exc_ui_IT1 <- sum(a$exc_ui_IT1) #27

aa <- data.frame(exc_c40, exc_c40_IT3, exc_c40_IT2, exc_c40_IT1)
aa <- t(aa)
colnames(aa)[1] <- "C40"

aaa <- data.frame(exc_13k, exc_13k_IT3, exc_13k_IT2, exc_13k_IT1)
aaa <- t(aaa)
colnames(aaa)[1] <- "GHS-SMOD"

#aaaa <- data.frame(exc_ui, exc_ui_IT3, exc_ui_IT2, exc_ui_IT1)
#aaaa <- t(aaaa)
#colnames(aaaa)[1] <- "UI"

aaaaa<- cbind(aaa, aa)

#O3
a<-o3_new
a$exc_c40 <- ifelse(a$pwo3_c40>30.6, 1, 0) # AQG: 60 = 30.6 ppb
a$exc_13k <- ifelse(a$pwo3_13k>30.6, 1, 0)
exc_c40 <- sum(a$exc_c40) 
exc_13k <- sum(a$exc_13k) 

a$exc_c40_IT2 <- ifelse(a$pwo3_c40>35.7, 1, 0) # IT2: 70 = 35.7 ppb
a$exc_13k_IT2 <- ifelse(a$pwo3_13k>35.7, 1, 0)
exc_c40_IT2 <- sum(a$exc_c40_IT2) 
exc_13k_IT2 <- sum(a$exc_13k_IT2) 

a$exc_c40_IT1 <- ifelse(a$pwo3_c40>51.0, 1, 0) # AQG: 100 = 51.0 ppb
a$exc_13k_IT1 <- ifelse(a$pwo3_13k>51.0, 1, 0)
exc_c40_IT1 <- sum(a$exc_c40_IT1) 
exc_13k_IT1 <- sum(a$exc_13k_IT1) 

aa <- data.frame(exc_c40, exc_c40_IT2, exc_c40_IT1)
aa <- t(aa)
colnames(aa)[1] <- "C40"

aaa <- data.frame(exc_13k, exc_13k_IT2, exc_13k_IT1)
aaa <- t(aaa)
colnames(aaa)[1] <- "GHS-SMOD"

#aaaa <- data.frame(exc_ui, exc_ui_IT3, exc_ui_IT2, exc_ui_IT1)
#aaaa <- t(aaaa)
#colnames(aaaa)[1] <- "UI"

aaaaa<- cbind(aaa, aa)


#number of population breathing bad air quality
#PM2.5
#number of population with bad air
corx1<-readRDS(file="/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/etc/corx1.rds")
corx1$exc_c40 <- ifelse(corx1$pwpm_c40>5, 1, 0) #AQG: 5
corx1$exc_13k <- ifelse(corx1$pwpm_13k>5, 1, 0)
corx1$exc_c40_IT4 <- ifelse(corx1$pwpm_c40>10, 1, 0) #IT4: 10
corx1$exc_13k_IT4 <- ifelse(corx1$pwpm_13k>10, 1, 0)
corx1$exc_c40_IT3 <- ifelse(corx1$pwpm_c40>15, 1, 0) #IT3: 15
corx1$exc_13k_IT3 <- ifelse(corx1$pwpm_13k>15, 1, 0)
corx1$exc_c40_IT2 <- ifelse(corx1$pwpm_c40>25, 1, 0) #IT2: 25
corx1$exc_13k_IT2 <- ifelse(corx1$pwpm_13k>25, 1, 0)
corx1$exc_c40_IT1 <- ifelse(corx1$pwpm_c40>35, 1, 0) #IT1: 35
corx1$exc_13k_IT1 <- ifelse(corx1$pwpm_13k>35, 1, 0)

a<-corx1 %>% group_by(exc_c40) %>% summarise(n=n(), pop=sum(pop_c40))
aq_c40 <- subset(a, exc_c40==1)$pop
aa<-corx1 %>% group_by(exc_13k) %>% summarise(n=n(),pop=sum(pop_13k))
aq_13k <- subset(aa, exc_13k==1)$pop

a<-corx1 %>% group_by(exc_c40_IT4) %>% summarise(n=n(), pop=sum(pop_c40))
it4_c40 <- subset(a, exc_c40_IT4==1)$pop
aa<-corx1 %>% group_by(exc_13k_IT4) %>% summarise(n=n(),pop=sum(pop_13k))
it4_13k <- subset(aa, exc_13k_IT4==1)$pop

a<-corx1 %>% group_by(exc_c40_IT3) %>% summarise(n=n(), pop=sum(pop_c40))
it3_c40 <- subset(a, exc_c40_IT3==1)$pop
aa<-corx1 %>% group_by(exc_13k_IT3) %>% summarise(n=n(),pop=sum(pop_13k))
it3_13k <- subset(aa, exc_13k_IT3==1)$pop

a<-corx1 %>% group_by(exc_c40_IT2) %>% summarise(n=n(), pop=sum(pop_c40))
it2_c40 <- subset(a, exc_c40_IT2==1)$pop
aa<-corx1 %>% group_by(exc_13k_IT2) %>% summarise(n=n(),pop=sum(pop_13k))
it2_13k <- subset(aa, exc_13k_IT2==1)$pop

a<-corx1 %>% group_by(exc_c40_IT1) %>% summarise(n=n(), pop=sum(pop_c40))
it1_c40 <- subset(a, exc_c40_IT1==1)$pop
aa<-corx1 %>% group_by(exc_13k_IT1) %>% summarise(n=n(),pop=sum(pop_13k))
it1_13k <- subset(aa, exc_13k_IT1==1)$pop

values <- c(aq_13k, aq_c40, it4_13k, it4_c40, it3_13k, it3_c40, it2_13k, it2_c40, it1_13k, it1_c40)
table_pm <- matrix(values, nrow = 5, ncol = 2, byrow = TRUE)

#NO2
#number of population with bad air
cory1<-readRDS(file="/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/etc/cory1.rds")
cory1$exc_c40 <- ifelse(cory1$pwno2_c40>5.14, 1, 0) #AQG: 5.14
cory1$exc_13k <- ifelse(cory1$pwno2_13k>5.14, 1, 0)
cory1$exc_c40_IT3 <- ifelse(cory1$pwno2_c40>10.63, 1, 0) #IT3: 20 = 10.63 ppb
cory1$exc_13k_IT3 <- ifelse(cory1$pwno2_13k>10.63, 1, 0)
cory1$exc_c40_IT2 <- ifelse(cory1$pwno2_c40>15.95, 1, 0) #IT2: 30 = 15.95 ppb
cory1$exc_13k_IT2 <- ifelse(cory1$pwno2_13k>15.95, 1, 0)
cory1$exc_c40_IT1 <- ifelse(cory1$pwno2_c40>21.27, 1, 0) #IT1: 40 = 21.27 ppb
cory1$exc_13k_IT1 <- ifelse(cory1$pwno2_13k>21.27, 1, 0)


a<-cory1 %>% group_by(exc_c40) %>% summarise(n=n(), pop=sum(pop_c40))
aq_c40 <- subset(a, exc_c40==1)$pop
aa<-cory1 %>% group_by(exc_13k) %>% summarise(n=n(),pop=sum(pop_13k))
aq_13k <- subset(aa, exc_13k==1)$pop

a<-cory1 %>% group_by(exc_c40_IT3) %>% summarise(n=n(), pop=sum(pop_c40))
it3_c40 <- subset(a, exc_c40_IT3==1)$pop
aa<-cory1 %>% group_by(exc_13k_IT3) %>% summarise(n=n(),pop=sum(pop_13k))
it3_13k <- subset(aa, exc_13k_IT3==1)$pop

a<-cory1 %>% group_by(exc_c40_IT2) %>% summarise(n=n(), pop=sum(pop_c40))
it2_c40 <- subset(a, exc_c40_IT2==1)$pop
aa<-cory1 %>% group_by(exc_13k_IT2) %>% summarise(n=n(),pop=sum(pop_13k))
it2_13k <- subset(aa, exc_13k_IT2==1)$pop

a<-cory1 %>% group_by(exc_c40_IT1) %>% summarise(n=n(), pop=sum(pop_c40))
it1_c40 <- subset(a, exc_c40_IT1==1)$pop
aa<-cory1 %>% group_by(exc_13k_IT1) %>% summarise(n=n(),pop=sum(pop_13k))
it1_13k <- subset(aa, exc_13k_IT1==1)$pop

values <- c(aq_13k, aq_c40, it3_13k, it3_c40, it2_13k, it2_c40, it1_13k, it1_c40)
table_pm <- matrix(values, nrow = 4, ncol = 2, byrow = TRUE)


#O3
#number of population with bad air
corz1<-readRDS(file="/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/etc/corz1.rds")
corz1$exc_c40 <- ifelse(corz1$pwo3_c40>30.6, 1, 0) # AQG: 60 = 30.6 ppb
corz1$exc_13k <- ifelse(corz1$pwo3_13k>30.6, 1, 0)
corz1$exc_c40_IT2 <- ifelse(corz1$pwo3_c40>35.7, 1, 0) # IT2: 70 = 35.7 ppb
corz1$exc_13k_IT2 <- ifelse(corz1$pwo3_13k>35.7, 1, 0)
corz1$exc_c40_IT1 <- ifelse(corz1$pwo3_c40>51.0, 1, 0) # IT1: 100 = 51.0 ppb
corz1$exc_13k_IT1 <- ifelse(corz1$pwo3_13k>51.0, 1, 0)

a<-corz1 %>% group_by(exc_c40) %>% summarise(n=n(), pop=sum(pop_c40))
aq_c40 <- subset(a, exc_c40==1)$pop
aa<-corz1 %>% group_by(exc_13k) %>% summarise(n=n(),pop=sum(pop_13k))
aq_13k <- subset(aa, exc_13k==1)$pop

a<-corz1 %>% group_by(exc_c40_IT2) %>% summarise(n=n(), pop=sum(pop_c40))
it2_c40 <- subset(a, exc_c40_IT2==1)$pop
aa<-corz1 %>% group_by(exc_13k_IT2) %>% summarise(n=n(),pop=sum(pop_13k))
it2_13k <- subset(aa, exc_13k_IT2==1)$pop

a<-corz1 %>% group_by(exc_c40_IT1) %>% summarise(n=n(), pop=sum(pop_c40))
it1_c40 <- subset(a, exc_c40_IT1==1)$pop
aa<-corz1 %>% group_by(exc_13k_IT1) %>% summarise(n=n(),pop=sum(pop_13k))
it1_13k <- subset(aa, exc_13k_IT1==1)$pop

values <- c(aq_13k, aq_c40, it2_13k, it2_c40, it1_13k, it1_c40)
table_pm <- matrix(values, nrow = 3, ncol = 2, byrow = TRUE)




#Scatter plots
#checking concentration difference dist.
hist(pm25_new$dif)
hist(no2_new$dif)
hist(o3_new$dif)
hist(co2_new$dif) #looks normal

#checking area difference dist. 
pm25_new$area_dif1  <- (pm25_new$area_13k-pm25_new$area_c40)
no2_new$area_dif1 <- (no2_new$area_13k-no2_new$area_c40)
o3_new$area_dif1 <- (o3_new$area_13k-o3_new$area_c40)
co2_new$area_dif1 <- (co2_new$area_13k-co2_new$area_c40)

hist(pm25_new$area_dif1)
hist(no2_new$area_dif1)
hist(o3_new$area_dif1)
hist(co2_new$area_dif1) #needs spearman

#checking pop difference dist. 
pm25_new$pop_dif1 <- pm25_new$pop_13k-pm25_new$pop_c40
no2_new$pop_dif1 <- no2_new$pop_13k-no2_new$pop_c40
o3_new$pop_dif1 <- o3_new$pop_13k-o3_new$pop_c40
co2_new$pop_dif1 <- co2_new$pop_13k-co2_new$pop_c40

hist(pm25_new$pop_dif1)
hist(no2_new$pop_dif1)
hist(o3_new$pop_dif1)
hist(co2_new$pop_dif1) #needs transformation

pm25_new$pop_dif2 <- pm25_new$pop_dif1-min(pm25_new$pop_dif1)+1
no2_new$pop_dif2 <- no2_new$pop_dif1-min(no2_new$pop_dif1)+1
o3_new$pop_dif2 <- o3_new$pop_dif1-min(o3_new$pop_dif1)+1
co2_new$pop_dif2 <- co2_new$pop_dif1-min(co2_new$pop_dif1)+1 #make every value positive

hist(pm25_new$pop_dif2)
hist(no2_new$pop_dif2)
hist(o3_new$pop_dif2)
hist(co2_new$pop_dif2)

hist(log(pm25_new$pop_dif2))
hist(log(no2_new$pop_dif2))
hist(log(o3_new$pop_dif2))
hist(log(co2_new$pop_dif2)) #log transformation

pm25_new$pop_dif3 <- log(pm25_new$pop_dif2)
no2_new$pop_dif3 <- log(no2_new$pop_dif2)
o3_new$pop_dif3 <- log(o3_new$pop_dif2)
co2_new$pop_dif3 <- log(co2_new$pop_dif2)

#final variable: 
#concentration difference: dif
#area difference: area_dif1 w/ spearman correlation
#populatino difference: pop_dif3 = log(pm25_new$pop_dif2) (pop_dif2=pop_dif1-min >=0)
#else: log_area, log_pop, log_pop_den, moran, log_mnt (based on 13k definitions)

#CORRELATION analysis
var <- c("log_area", "area_dif1", "log_pop", "log_pop_den", "pop_dif2", "moran")
#empty frame
variable_names <- c("var", "est", "pval", "pol")
cor_fin <- data.frame(matrix(nrow = 0, ncol = length(variable_names)))
colnames(cor_fin) <- variable_names

#PM
est <- rep(1:6)
pval <- rep(1:6)
data <- pm25_new
for (k in 1:6) {
  cor_methods <- ifelse(var[k]=="area_dif1", "spearman", "pearson")
  data_ <- data[c("dif", paste(var[k]))]
  cor <- cor.test(data$dif, data_[,2], method=cor_methods)
  est[k] <- cor$estimate
  pval[k] <- cor$p.value 
  cor_fin <- as.data.frame(cbind(var, est, pval))
}
cor_fin$sig <- ifelse(cor_fin$pval<=0.05, "sig", "insig")
cor_fin$pol <- "pm"
cor_fin_pm <- cor_fin

#NO2
est <- rep(1:6)
pval <- rep(1:6)
data <- no2_new
for (k in 1:6) {
  cor_methods <- ifelse(var[k]=="area_dif1", "spearman", "pearson")
  data_ <- data[c("dif", paste(var[k]))]
  cor <- cor.test(data$dif, data_[,2], method=cor_methods)
  est[k] <- cor$estimate
  pval[k] <- cor$p.value 
  cor_fin <- as.data.frame(cbind(var, est, pval))
}
cor_fin$sig <- ifelse(cor_fin$pval<=0.05, "sig", "insig")
cor_fin$pol <- "no2"
cor_fin_no2 <- cor_fin

#O3
est <- rep(1:6)
pval <- rep(1:6)
data <- o3_new
for (k in 1:6) {
  cor_methods <- ifelse(var[k]=="area_dif1", "spearman", "pearson")
  data_ <- data[c("dif", paste(var[k]))]
  cor <- cor.test(data$dif, data_[,2], method=cor_methods)
  est[k] <- cor$estimate
  pval[k] <- cor$p.value 
  cor_fin <- as.data.frame(cbind(var, est, pval))
}
cor_fin$sig <- ifelse(cor_fin$pval<=0.05, "sig", "insig")
cor_fin$pol <- "o3"
cor_fin_o3 <- cor_fin

#CO2
est <- rep(1:6)
pval <- rep(1:6)
data <- co2_new
for (k in 1:6) {
  cor_methods <- ifelse(var[k]=="area_dif1", "spearman", "pearson")
  data_ <- data[c("dif", paste(var[k]))]
  cor <- cor.test(data$dif, data_[,2], method=cor_methods)
  est[k] <- cor$estimate
  pval[k] <- cor$p.value 
  cor_fin <- as.data.frame(cbind(var, est, pval))
}
cor_fin$sig <- ifelse(cor_fin$pval<=0.05, "sig", "insig")
cor_fin$pol <- "co2"
cor_fin_co2 <- cor_fin

cor_fin <- cbind(cor_fin_pm, cor_fin_no2, cor_fin_o3, cor_fin_co2)
write.csv(cbind(cor_fin_pm, cor_fin_no2, cor_fin_o3, cor_fin_co2), file = "/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/etc/cor_fin_new.csv", row.names = FALSE)

#Scatter plots
pdf(file = paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Study/Nature_cities/Figures/FigureS5/FigureS5_A.pdf"),
    width = 10,  # width in inches
    height = 10) # height in inches
par(family = "Lato")
ggplot(pm25_new, aes(x = as.numeric(dif), y = as.numeric(pop_dif1)/10^6)) +
  geom_point(color = "blue4", alpha = 0.5, size = 3.5) +  
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey30") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey30") +
  theme(text = element_text(size = 15))+
  scale_x_continuous(labels = scales::number_format(scale = 1)) +
  scale_y_continuous(labels = scales::number_format(scale = 1)) +
  xlab(bquote(Delta * PM[2.5] * " (" * mu * "g/m"^3 * ")")) +
  ylab(bquote(Delta * "Population (million people)")) +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        axis.text = element_text(size = 20),
        axis.title.y = element_text(size = 25, margin = margin(r = 20)), 
        axis.title.x = element_text(size = 25, margin = margin(t = 20)))
dev.off() 

pdf(file = paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Study/Nature_cities/Figures/FigureS5/FigureS5_B.pdf"),
    width = 10,  # width in inches
    height = 10) # height in inches
par(family = "Lato")
ggplot(no2_new, aes(x = as.numeric(dif), y = as.numeric(log_area))) +
  geom_point(color = "blue4", alpha = 0.5, size = 3.5) +  
  theme(text = element_text(size = 15)) +
  scale_x_continuous(labels = label_comma()) +  
  scale_y_continuous(labels = label_comma(big.mark = " ")) +
  xlab(bquote(Delta * NO[2] * " (ppb)")) +
  ylab(bquote("Log (total area, km"^2 * ")")) +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        axis.text = element_text(size = 20),
        axis.title.y = element_text(size = 25, margin = margin(r = 20)), 
        axis.title.x = element_text(size = 25, margin = margin(t = 20)))
dev.off() 

pdf(file = paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Study/Nature_cities/Figures/FigureS5/FigureS5_C.pdf"),
    width = 10,  # width in inches
    height = 10) # height in inches
par(family = "Lato")
ggplot(no2_new, aes(x = as.numeric(dif), y = as.numeric(area_dif1)/10^6)) +
  geom_point(color = "blue4", alpha = 0.5, size = 3.5) +  
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey30") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey30") +
  theme(text = element_text(size = 15)) +
  scale_x_continuous(labels = label_comma()) +  
  scale_y_continuous(labels = label_comma(big.mark = " ")) +
  xlab(bquote(Delta * NO[2] * " (ppb)")) +
  ylab(bquote(Delta * "Area (km"^2 * ")")) +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        axis.text = element_text(size = 20),
        axis.title.y = element_text(size = 25, margin = margin(r = 20)), 
        axis.title.x = element_text(size = 25, margin = margin(t = 20)))
dev.off() 

pdf(file = paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Study/Nature_cities/Figures/FigureS5/FigureS5_D.pdf"),
    width = 10,  # width in inches
    height = 10) # height in inches
par(family = "Lato")
ggplot(no2_new, aes(x = as.numeric(dif), y = as.numeric(pop_dif1)/10^6)) +
  geom_point(color = "blue4", alpha = 0.5, size = 3.5) +  
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey30") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey30") +
  theme(text = element_text(size = 15)) +
  scale_x_continuous(labels = label_comma()) +  
  scale_y_continuous(labels = label_comma(big.mark = " ")) +
  xlab(bquote(Delta * NO[2] * " (ppb)")) +
  ylab(bquote(Delta * "Population (million people)")) +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        axis.text = element_text(size = 20),
        axis.title.y = element_text(size = 25, margin = margin(r = 20)), 
        axis.title.x = element_text(size = 25, margin = margin(t = 20)))
dev.off() 

pdf(file = paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Study/Nature_cities/Figures/FigureS5/FigureS5_E.pdf"),
    width = 10,  # width in inches
    height = 10) # height in inches
par(family = "Lato")
ggplot(o3_new, aes(x = as.numeric(dif), y = as.numeric(area_dif1)/10^6)) +
  geom_point(color = "blue4", alpha = 0.5, size = 3.5) +  
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey30") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey30") +
  theme(text = element_text(size = 15))+
  scale_x_continuous(labels = scales::number_format(scale = 1)) +
  scale_y_continuous(labels = scales::number_format(scale = 1))         +
  xlab(bquote(Delta * O[3] * " (ppb)")) +
  ylab(bquote(Delta * "Area (km"^2 * ")")) +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        axis.text = element_text(size = 20),
        axis.title.y = element_text(size = 25, margin = margin(r = 20)), 
        axis.title.x = element_text(size = 25, margin = margin(t = 20)))
dev.off() 

pdf(file = paste0("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Study/Nature_cities/Figures/FigureS5/FigureS5_F.pdf"),
    width = 10,  # width in inches
    height = 10) # height in inches
par(family = "Lato")
ggplot(co2_new, aes(x = as.numeric(dif), y = as.numeric(area_dif1)/10^6)) +
  geom_point(color = "blue4", alpha = 0.5, size = 3.5) +  
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey30") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey30") +
  theme(text = element_text(size = 15))+
  scale_x_continuous(labels = scales::number_format(scale = 1)) +
  scale_y_continuous(labels = scales::number_format(scale = 1)) +
  xlab(bquote(Delta * FFCO[2] * " per capita (metric tons)")) +
  ylab(bquote(Delta * "Area (km"^2 * ")")) +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        axis.text = element_text(size = 20),
        axis.title.y = element_text(size = 25, margin = margin(r = 20)), 
        axis.title.x = element_text(size = 25, margin = margin(t = 20)))
dev.off() 





############################################################
####### Uncertainty for PM2.5 and O3: Table S4 #############
############################################################
#VARIANCE
pm <- read.csv('/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Study/Nature_cities/Revision_communications_EE/etc/pwpm_2019_uncertainty_var_absolute.csv')
pm <- read.csv('/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Study/Nature_cities/Revision_communications_EE/etc/pwpm_2019_uncertainty_var_absolute_squared.csv')
names(pm)[3] <- "var"
o3 <- read.csv('/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/Study/Nature_cities/Revision_communications_EE/etc/uncertainty/pwo3_2019_uncertainty_var.csv')
aa <- read.csv("/Users/k_sy_n_imac/Library/CloudStorage/Dropbox/GWU/Lab/13000cities/Pollution_emissions/etc/GBDsuperregion_final_final.csv")

pm <- merge(pm, aa, by="ID")
o3 <- merge(o3, aa, by="ID")

pm_var_global <- pm %>% group_by() %>% summarise(n=n(), var_mean=mean(var, na.rm=TRUE), var_median=median(var, na.rm=TRUE))
pm_var_regional <- pm %>% group_by(GBDSuperRegion) %>% summarise(n=n(), var_mean=mean(var, na.rm=TRUE), var_median=median(var, na.rm=TRUE))
o3_var_global <- o3 %>% group_by() %>% summarise(n=n(), var_mean=mean(var, na.rm=TRUE), var_median=median(var, na.rm=TRUE))
o3_var_regional <- o3 %>% group_by(GBDSuperRegion) %>% summarise(n=n(), var_mean=mean(var, na.rm=TRUE), var_median=median(var, na.rm=TRUE))














