###### Visualising WorldClim data for Tasmanian Euc project

#### Load packages ####

library(raster)
library(rgdal)
library(maps)
library(tidyverse)

######### Temperature globally ####
#Following this guide:https://www.benjaminbell.co.uk/2018/01/extracting-data-and-making-climate-maps.html

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
temp.data$Jan <- raster::extract(temp1, samples)
temp.data$Feb <- raster::extract(temp2, samples)
temp.data$Mar <- raster::extract(temp3, samples)
temp.data$Apr <- raster::extract(temp4, samples)
temp.data$May <- raster::extract(temp5, samples)
temp.data$Jun <- raster::extract(temp6, samples)
temp.data$Jul <- raster::extract(temp7, samples)
temp.data$Aug <- raster::extract(temp8, samples)
temp.data$Sep <- raster::extract(temp9, samples)
temp.data$Oct <- raster::extract(temp10, samples)
temp.data$Nov <- raster::extract(temp11, samples)
temp.data$Dec <- raster::extract(temp12, samples)

#plotting world maps
tempcol <- colorRampPalette(c("purple", "blue", "skyblue", "green", "lightgreen", "yellow", "orange", "red", "darkred"))
plot(temp1, col=tempcol(100))

#Plotting for UK only
plot(temp1, xlim=c(-12, 4), ylim=c(48, 64), col=tempcol(100))
#But colouration is weird, so:
plot(temp1, xlim=c(-12, 4), ylim=c(48, 64), zlim=c(-10,30), col=tempcol(100))

#Zlim is for temp range
plot(temp1, xlim=c(144, 150), ylim=c(-44, -40), zlim=c(-10,30), col=tempcol(100))

###### PPT, PET and MD for Tasmania! #####

#et is evapotranspiration
#ai is aridity index
#the 12 correspond to months
#yr is annual average
et1 <- raster("Data/Climate data/et0_month/et0_01.tif")
et2 <- raster("Data/Climate data/et0_month/et0_02.tif")
et3 <- raster("Data/Climate data/et0_month/et0_03.tif")
et4 <- raster("Data/Climate data/et0_month/et0_04.tif")
et5 <- raster("Data/Climate data/et0_month/et0_05.tif")
et6 <- raster("Data/Climate data/et0_month/et0_06.tif")
et7 <- raster("Data/Climate data/et0_month/et0_07.tif")
et8 <- raster("Data/Climate data/et0_month/et0_08.tif")
et9 <- raster("Data/Climate data/et0_month/et0_09.tif")
et10 <- raster("Data/Climate data/et0_month/et0_10.tif")
et11 <- raster("Data/Climate data/et0_month/et0_11.tif")
et12 <- raster("Data/Climate data/et0_month/et0_12.tif")
#Year
etyr <- raster("Data/Climate data/et0_yr/et0_yr.tif")

ai <- raster("Data/Climate data/ai_et0/ai_et0.tif")


### Code from Leander ###
#Evapotranspiration
plot(etyr, xlim=c(144, 150), ylim=c(-44, -40))
#Aridity Index
plot(ai, xlim=c(144, 150), ylim=c(-44, -40))

#Plotting for TMP. Tasman Peninsula.
#Evapotranspiration
plot(etyr, xlim=c(147.5, 148.2), ylim=c(-43.3, -42.8))
#Aridity Index
plot(ai, xlim=c(147.5, 148.2), ylim=c(-43.3, -42.8))

#Cropping data to just have Tas
Tas_AIcrop <- crop(ai, c(144.5,149.1,-44,-40.6))
Tas_AIreal <- Tas_AIcrop/10000
Tas_PET <- crop(etyr, c(144.5,149.1,-44,-40.6))
Tas_PPT <- Tas_AIreal * Tas_PET
Tas_CMD <- Tas_PET-Tas_PPT
#CMD is Climate Moisture Deficient
# Pet = Potential Evapotranspiration
# Ppt = precipitation
plot(Tas_PPT, xlim=c(147.5, 148.2), ylim=c(-43.3, -42.8))

#All of these from AVH
## This was for plotting distribution data to identify sites
# amyg <- read_csv("Data/Tassie Eucs/E_amygdalina_Tasi/E_amygdalina_Tasi.csv")
# obli <- read_csv("Data/Tassie Eucs/E_obliqua_Tasi/E_obliqua_Tasi.csv")
# ovat <- read_csv("Data/Tassie Eucs/E_ovata_Tasi/E_ovata_Tasi.csv")
# vimi <- read_csv("Data/Tassie Eucs/E_viminalis_Tasi/E_viminalis_Tasi.csv")

# Extracting climate values
#AusAIreal <- raster("/Users/Alexandra/OneDrive - The University of Queensland/UQ Research/Eucalyptus drought experiment/Files from Leander/AustraliaClimateRasters/Global PET and Aridity Index/AustraliaClimateRasters/Australia_AridityIndex_AIreal.grd")

############ scratch extraction of random points ################

###This is how to extract values from a single location
rlat <- -41.18608
rlon <-  148.25668
ptsCMD <- raster::extract(x=Tas_CMD, y= cbind(rlon, rlat))
ptsAI <- raster::extract(x=Tas_AIreal, y=cbind(rlon, rlat))
ptsPPT <- raster::extract(x=Tas_PPT, y=cbind(rlon, rlat))

### plotting PPT, PET, AI, and CMD in one place
par(mfrow=c(2,2), mar=c(.5,.5,2, 5.5))
plot(Tas_PPT, xaxt="n", yaxt="n", main="PPT")
plot(Tas_PET, xaxt="n", yaxt="n", main="PET")
plot(Tas_AIreal, xaxt="n", yaxt="n", main="AI (PPT/PET)")
plot(Tas_CMD, xaxt="n", yaxt="n", main="AI (PET-PPT)")

# import Tasmanian reserve lat long (in decimal degrees) ###
ResTas <- read_csv("Data/site_coords.csv")

ResTas$AI <- raster::extract(x=Tas_AIreal, y= cbind(ResTas$Long,ResTas$Lat))
ResTas$PPT <- raster::extract(x=Tas_PPT, y= cbind(ResTas$Long,ResTas$Lat))
ResTas$PET <- raster::extract(x=Tas_PET, y= cbind(ResTas$Long,ResTas$Lat))
ResTas$CMD <- raster::extract(x=Tas_CMD, y= cbind(ResTas$Long,ResTas$Lat))

par(mfrow=c(1,1))
plot(Tas_AIreal)
#Size is based on aridity index!
points(Lat~Long, ResTas, pch=16, cex=AI, col='red')

#### plotting just tasman peninsula
plot(Tas_CMD, xlim=c(147.5,148.5), ylim=c(-43.5, -42.5))


