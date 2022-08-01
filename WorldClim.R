###### Visualising WorldClim data for Tasmania
### Version 2.0 
#Following this guide:https://www.benjaminbell.co.uk/2018/01/extracting-data-and-making-climate-maps.html

library(raster)
library(rgdal)
library(maps)
library(tidyverse)

temp1 <- raster("Data/WorldClim/wc2.0_30s_tavg_01.tif")
temp2 <- raster("Data/WorldClim/wc2.0_30s_tavg_02.tif")
temp3 <- raster("Data/WorldClim/wc2.0_30s_tavg_03.tif")
temp4 <- raster("Data/WorldClim/wc2.0_30s_tavg_04.tif")
temp5 <- raster("Data/WorldClim/wc2.0_30s_tavg_05.tif")
temp6 <- raster("Data/WorldClim/wc2.0_30s_tavg_06.tif")
temp7 <- raster("Data/WorldClim/wc2.0_30s_tavg_07.tif")
temp8 <- raster("Data/WorldClim/wc2.0_30s_tavg_08.tif")
temp9 <- raster("Data/WorldClim/wc2.0_30s_tavg_09.tif")
temp10 <- raster("Data/WorldClim/wc2.0_30s_tavg_10.tif")
temp11 <- raster("Data/WorldClim/wc2.0_30s_tavg_11.tif")
temp12 <- raster("Data/WorldClim/wc2.0_30s_tavg_12.tif")

# Create a data.frame with sample site coordinates
site <- c("Manchester", "Liverpool", "Oxford", "London")
lon <- c(-2.24, -2.98, -1.25, -0.11)
lat <- c(53.47, 53.4, 51.74, 51.49)
samples <- data.frame(site, lon, lat, row.names="site")


# Extract data from WorldClim for your sites
temp.data <- samples 
temp.data$Jan <- extract(temp1, samples)
temp.data$Feb <- extract(temp2, samples)
temp.data$Mar <- extract(temp3, samples)
temp.data$Apr <- extract(temp4, samples)
temp.data$May <- extract(temp5, samples)
temp.data$Jun <- extract(temp6, samples)
temp.data$Jul <- extract(temp7, samples)
temp.data$Aug <- extract(temp8, samples)
temp.data$Sep <- extract(temp9, samples)
temp.data$Oct <- extract(temp10, samples)
temp.data$Nov <- extract(temp11, samples)
temp.data$Dec <- extract(temp12, samples)


#plotting world maps
tempcol <- colorRampPalette(c("purple", "blue", "skyblue", "green", "lightgreen", "yellow", "orange", "red", "darkred"))
plot(temp1, col=tempcol(100))

#Plotting for UK only
plot(temp1, xlim=c(-12, 4), ylim=c(48, 64), col=tempcol(100))
#But colouration is weird, so:
plot(temp1, xlim=c(-12, 4), ylim=c(48, 64), zlim=c(-10,30), col=tempcol(100))

###### For TAS!
#Zlim is for temp range
plot(temp1, xlim=c(144, 150), ylim=c(-44, -40), zlim=c(-10,30), col=tempcol(100))


### Trying to do aridity
#et is evapotranspiration.
#ai is aridity index.
#the 12 correspond to months.
#yr is annual average.
et1 <- raster("et0_01.tif")
et2 <- raster("et0_02.tif")
et3 <- raster("et0_03.tif")
et4 <- raster("et0_04.tif")
et5 <- raster("et0_05.tif")
et6 <- raster("et0_06.tif")
et7 <- raster("et0_07.tif")
et8 <- raster("et0_08.tif")
et9 <- raster("et0_09.tif")
et10 <- raster("et0_10.tif")
et11 <- raster("et0_11.tif")
et12 <- raster("et0_12.tif")
#Year
etyr <- raster("et0_yr.tif")

ai <- raster("ai_et0.tif")

#For Tas
#Evapotranspiration
plot(etyr, xlim=c(144, 150), ylim=c(-44, -40))
#Aridity Index
plot(ai, xlim=c(144, 150), ylim=c(-44, -40))

#Plotting for TMP. Tasman Peninsula.
#Evapotranspiration
plot(etyr, xlim=c(147.5, 148.2), ylim=c(-43.3, -42.8))
#Aridity Index
plot(ai, xlim=c(147.5, 148.2), ylim=c(-43.3, -42.8))
plot(Tas_PPT, xlim=c(147.5, 148.2), ylim=c(-43.3, -42.8))


#Cropping data to just have Tas

Tas_AIcrop <- crop(ai, c(144.5,149.1,-44,-40.6))
Tas_AIreal <- Tas_AIcrop/10000
Tas_PET <- crop(etyr, c(144.5,149.1,-44,-40.6))
Tas_PPT <- Tas_AIreal * Tas_PET
Tas_CMD <- Tas_PET-Tas_PPT
#CMD is Climate Moisture Deficient
# Pet = Potential Evapotranspiratino
# Ppt = precipitation

#All of these from AVH

amyg <- read_csv("E_amygdalina_Tasi.csv")
vimi <- read_csv("E_viminalis_Tasi.csv")
obli <- read_csv("E_obliqua_Tasi.csv")
ovat <- read_csv("E_ovata_Tasi.csv")

amyg <- read.csv("C:/Users/Alexandra/OneDrive - The University of Queensland/UQ Research/Eucalyptus drought experiment/R/E_amygdalina_Tasi.csv", header=T)


 #None of this is working

amyg$AI <- extract(x=ai, y=cbind('amyg$Longitude - processed','amyg$Latitude -processed'))
amyg$CMD <- extract(x=Tas_CMD, y=cbind(amyg$Longitude...processed, amyg$Latitude...processed))
amyg$PPT <- extract(x=Tas_PPT, y=cbind(amyg$Longitude...processed, amyg$Latitude...processed))


plot(Tas_CMD)
points(Latitude - processed~Longitude...processed, amyg, pch=16, col = 'red3')

plot(amyg)



# Extracting climate values

AusAIreal <- raster("/Users/Alexandra/OneDrive - The University of Queensland/UQ Research/Eucalyptus drought experiment/Files from Leander/AustraliaClimateRasters/Global PET and Aridity Index/AustraliaClimateRasters/Australia_AridityIndex_AIreal.grd")











