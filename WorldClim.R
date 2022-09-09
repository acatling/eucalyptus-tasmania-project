###### Visualising WorldClim data for Tasmanian Euc project

#### Load packages and data ####

## This script overrides some of the tidyverse functions
#possible solution - 
#reassign the function: select <- dplyr::select
library(raster)
#library(rgdal)
#library(maps)
library(tidyverse)

######### Temperature globally ####
#Following this guide:https://www.benjaminbell.co.uk/2018/01/extracting-data-and-making-climate-maps.html
#Do not currently have the data downloaded


# temp1 <- raster("Data/worldclim_v2_temp/wc2.0_30s_tavg_01.tif")
# temp2 <- raster("Data/worldclim_v2_temp/wc2.0_30s_tavg_02.tif")
# temp3 <- raster("Data/worldclim_v2_temp/wc2.0_30s_tavg_03.tif")
# temp4 <- raster("Data/worldclim_v2_temp/wc2.0_30s_tavg_04.tif")
# temp5 <- raster("Data/worldclim_v2_temp/wc2.0_30s_tavg_05.tif")
# temp6 <- raster("Data/worldclim_v2_temp/wc2.0_30s_tavg_06.tif")
# temp7 <- raster("Data/worldclim_v2_temp/wc2.0_30s_tavg_07.tif")
# temp8 <- raster("Data/worldclim_v2_temp/wc2.0_30s_tavg_08.tif")
# temp9 <- raster("Data/worldclim_v2_temp/wc2.0_30s_tavg_09.tif")
# temp10 <- raster("Data/worldclim_v2_temp/wc2.0_30s_tavg_10.tif")
# temp11 <- raster("Data/worldclim_v2_temp/wc2.0_30s_tavg_11.tif")
# temp12 <- raster("Data/worldclim_v2_temp/wc2.0_30s_tavg_12.tif")

# Create a data.frame with sample site coordinates
# site <- c("Manchester", "Liverpool", "Oxford", "London")
# lon <- c(-2.24, -2.98, -1.25, -0.11)
# lat <- c(53.47, 53.4, 51.74, 51.49)
# samples <- data.frame(site, lon, lat, row.names="site")

# Extract data from WorldClim for your sites
# temp.data <- samples 
# temp.data$Jan <- raster::extract(temp1, samples)
# temp.data$Feb <- raster::extract(temp2, samples)
# temp.data$Mar <- raster::extract(temp3, samples)
# temp.data$Apr <- raster::extract(temp4, samples)
# temp.data$May <- raster::extract(temp5, samples)
# temp.data$Jun <- raster::extract(temp6, samples)
# temp.data$Jul <- raster::extract(temp7, samples)
# temp.data$Aug <- raster::extract(temp8, samples)
# temp.data$Sep <- raster::extract(temp9, samples)
# temp.data$Oct <- raster::extract(temp10, samples)
# temp.data$Nov <- raster::extract(temp11, samples)
# temp.data$Dec <- raster::extract(temp12, samples)

#plotting world maps
# tempcol <- colorRampPalette(c("purple", "blue", "skyblue", "green", "lightgreen", "yellow", "orange", "red", "darkred"))
# plot(temp1, col=tempcol(100))

#Plotting for UK only
# plot(temp1, xlim=c(-12, 4), ylim=c(48, 64), col=tempcol(100))
#But colouration is weird, so:
# plot(temp1, xlim=c(-12, 4), ylim=c(48, 64), zlim=c(-10,30), col=tempcol(100))

#Zlim is for temp range
# plot(temp1, xlim=c(144, 150), ylim=c(-44, -40), zlim=c(-10,30), col=tempcol(100))

###### PPT, PET and MD for Tasmania! #####

#et is evapotranspiration
#ai is aridity index
#the 12 correspond to months
#yr is annual average

#Evapotranspiration
et1 <- raster("Data/Global-ET0_v3_monthly/et0_v3_01.tif")
et2 <- raster("Data/Global-ET0_v3_monthly/et0_v3_02.tif")
et3 <- raster("Data/Global-ET0_v3_monthly/et0_v3_03.tif")
et4 <- raster("Data/Global-ET0_v3_monthly/et0_v3_04.tif")
et5 <- raster("Data/Global-ET0_v3_monthly/et0_v3_05.tif")
et6 <- raster("Data/Global-ET0_v3_monthly/et0_v3_06.tif")
et7 <- raster("Data/Global-ET0_v3_monthly/et0_v3_07.tif")
et8 <- raster("Data/Global-ET0_v3_monthly/et0_v3_08.tif")
et9 <- raster("Data/Global-ET0_v3_monthly/et0_v3_09.tif")
et10 <- raster("Data/Global-ET0_v3_monthly/et0_v3_10.tif")
et11 <- raster("Data/Global-ET0_v3_monthly/et0_v3_11.tif")
et12 <- raster("Data/Global-ET0_v3_monthly/et0_v3_12.tif")
#Year
etyr <- raster("Data/Global-AI_ET0_v3_annual/et0_v3_yr.tif")
#plot(etyr, xlim=c(144, 150), ylim=c(-44, -40))

#Cropping data to just have Tas
tas_et1 <- crop(et1, c(144.5,149.1,-44,-40.6))
tas_et2 <- crop(et2, c(144.5,149.1,-44,-40.6))
tas_et3 <- crop(et3, c(144.5,149.1,-44,-40.6))
tas_et4 <- crop(et4, c(144.5,149.1,-44,-40.6))
tas_et5 <- crop(et5, c(144.5,149.1,-44,-40.6))
tas_et6 <- crop(et6, c(144.5,149.1,-44,-40.6))
tas_et7 <- crop(et7, c(144.5,149.1,-44,-40.6))
tas_et8 <- crop(et8, c(144.5,149.1,-44,-40.6))
tas_et9 <- crop(et9, c(144.5,149.1,-44,-40.6))
tas_et10 <- crop(et10, c(144.5,149.1,-44,-40.6))
tas_et11 <- crop(et11, c(144.5,149.1,-44,-40.6))
tas_et12 <- crop(et12, c(144.5,149.1,-44,-40.6))

### Aridity Index
aiyr <- raster("Data/Global-AI_ET0_v3_annual/ai_v3_yr.tif")
#plot(aiyr, xlim=c(144, 150), ylim=c(-44, -40))

## Monthly aridity index
ai1 <- raster("Data/Global-AI_v3_monthly/ai_v3_01.tif")
ai2 <- raster("Data/Global-AI_v3_monthly/ai_v3_02.tif")
ai3 <- raster("Data/Global-AI_v3_monthly/ai_v3_03.tif")
ai4 <- raster("Data/Global-AI_v3_monthly/ai_v3_04.tif")
ai5 <- raster("Data/Global-AI_v3_monthly/ai_v3_05.tif")
ai6 <- raster("Data/Global-AI_v3_monthly/ai_v3_06.tif")
ai7 <- raster("Data/Global-AI_v3_monthly/ai_v3_07.tif")
ai8 <- raster("Data/Global-AI_v3_monthly/ai_v3_08.tif")
ai9 <- raster("Data/Global-AI_v3_monthly/ai_v3_09.tif")
ai10 <- raster("Data/Global-AI_v3_monthly/ai_v3_10.tif")
ai11 <- raster("Data/Global-AI_v3_monthly/ai_v3_11.tif")
ai12 <- raster("Data/Global-AI_v3_monthly/ai_v3_12.tif")

#Cropping data to just have Tas
tas_ai1 <- crop(ai1, c(144.5,149.1,-44,-40.6))
tas_ai2 <- crop(ai2, c(144.5,149.1,-44,-40.6))
tas_ai3 <- crop(ai3, c(144.5,149.1,-44,-40.6))
tas_ai4 <- crop(ai4, c(144.5,149.1,-44,-40.6))
tas_ai5 <- crop(ai5, c(144.5,149.1,-44,-40.6))
tas_ai6 <- crop(ai6, c(144.5,149.1,-44,-40.6))
tas_ai7 <- crop(ai7, c(144.5,149.1,-44,-40.6))
tas_ai8 <- crop(ai8, c(144.5,149.1,-44,-40.6))
tas_ai9 <- crop(ai9, c(144.5,149.1,-44,-40.6))
tas_ai10 <- crop(ai10, c(144.5,149.1,-44,-40.6))
tas_ai11 <- crop(ai11, c(144.5,149.1,-44,-40.6))
tas_ai12 <- crop(ai12, c(144.5,149.1,-44,-40.6))

### Need to convert aridity values to  be real
tas_ai1_real <- tas_ai1/10000
tas_ai2_real <- tas_ai2/10000
tas_ai3_real <- tas_ai3/10000
tas_ai4_real <- tas_ai4/10000
tas_ai5_real <- tas_ai5/10000
tas_ai6_real <- tas_ai6/10000
tas_ai7_real <- tas_ai7/10000
tas_ai8_real <- tas_ai8/10000
tas_ai9_real <- tas_ai9/10000
tas_ai10_real <- tas_ai10/10000
tas_ai11_real <- tas_ai11/10000
tas_ai12_real <- tas_ai12/10000

### Calculate precipitation at monthly scale
tas_ppt1 <- tas_ai1_real*tas_et1
tas_ppt2 <- tas_ai2_real*tas_et2
tas_ppt3 <- tas_ai3_real*tas_et3
tas_ppt4 <- tas_ai4_real*tas_et4
tas_ppt5 <- tas_ai5_real*tas_et5
tas_ppt6 <- tas_ai6_real*tas_et6
tas_ppt7 <- tas_ai7_real*tas_et7
tas_ppt8 <- tas_ai8_real*tas_et8
tas_ppt9 <- tas_ai9_real*tas_et9
tas_ppt10 <- tas_ai10_real*tas_et10
tas_ppt11 <- tas_ai11_real*tas_et11
tas_ppt12 <- tas_ai12_real*tas_et12

### Calculate moisture deficit at monthly scale
tas_md1 <- tas_et1-tas_ppt1
tas_md2 <- tas_et2-tas_ppt2
tas_md3 <- tas_et3-tas_ppt3
tas_md4 <- tas_et4-tas_ppt4
tas_md5 <- tas_et5-tas_ppt5
tas_md6 <- tas_et6-tas_ppt6
tas_md7 <- tas_et7-tas_ppt7
tas_md8 <- tas_et8-tas_ppt8
tas_md9 <- tas_et9-tas_ppt9
tas_md10 <- tas_et10-tas_ppt10
tas_md11 <- tas_et11-tas_ppt11
tas_md12 <- tas_et12-tas_ppt12

### Annual scale
#CMD is Climate Moisture Deficient
# Pet = Potential Evapotranspiration
# Ppt = precipitation
Tas_AIcrop <- crop(aiyr, c(144.5,149.1,-44,-40.6))
Tas_AIreal <- Tas_AIcrop/10000
Tas_PET <- crop(etyr, c(144.5,149.1,-44,-40.6))
#Doing the below  step because AI is PPT/PET so multiplying by PET gives us PPT
Tas_PPT <- Tas_AIreal * Tas_PET
Tas_CMD <- Tas_PET-Tas_PPT

############ Calculating long-term MD for sites ################

###This is how to extract values from a single location
rlat <- -41.18608
rlon <-  148.25668
ptsCMD <- extract(x=Tas_CMD, y= cbind(rlon, rlat))
ptsAI <- extract(x=Tas_AIreal, y=cbind(rlon, rlat))
ptsPPT <- extract(x=Tas_PPT, y=cbind(rlon, rlat))

### plotting PPT, PET, AI, and CMD in one place
# par(mfrow=c(2,2), mar=c(.5,.5,2, 5.5))
# plot(Tas_PPT, xaxt="n", yaxt="n", main="PPT")
# plot(Tas_PET, xaxt="n", yaxt="n", main="PET")
# plot(Tas_AIreal, xaxt="n", yaxt="n", main="AI (PPT/PET)")
#plot(Tas_CMD, xaxt="n", yaxt="n", main="AI/CMD (PET-PPT)")

# import Tasmanian reserve lat long (in decimal degrees) ###
ResTas <- read_csv("Data/site_coords_updated.csv")

ResTas$AI <- extract(x=Tas_AIreal, y= cbind(ResTas$Centred_Long,ResTas$Centred_Lat))
ResTas$PPT <- extract(x=Tas_PPT, y= cbind(ResTas$Centred_Long,ResTas$Centred_Lat))
ResTas$PET <- extract(x=Tas_PET, y= cbind(ResTas$Centred_Long,ResTas$Centred_Lat))
ResTas$CMD <- extract(x=Tas_CMD, y= cbind(ResTas$Centred_Long,ResTas$Centred_Lat))

### These are the annual features of the site (PPT, PET, CMD)
simpleResTas <- ResTas %>% dplyr::select(Site, PPT, PET, CMD)

### Look at the extracted values
par(mfrow=c(1,1))
#plot(Tas_CMD, main="CMD (PET-PPT)")
##Size is based on aridity index!
#points(Lat~Long, ResTas, pch=16, cex=AI, col='red')

#### plotting just tasman peninsula
#plot(Tas_CMD, xlim=c(147.5,148.5), ylim=c(-43.5, -42.5))

#### Calculating period climate anomaly ####
# By site and by month
#Make a long dataframe with:
#Site | Period | md_norm | md_period | proportion
# proportion = md_period/md_norm

#md_norm is agnostic to year, but calculated as an average per month
#md_period matters which year

## These are the months (inclusive) that need to be summed to calculate
# proportion period climate/norm climate, by site
#Summing months where a majority of the month there was growth data
dates_growth_site <- read_csv("Data/dates_monthly_period_growth.csv")

#This is the dataset with the period climate data already in it
#period_climate

## Need to extract site-level climate norm data by month first
#Centred_Lat and Centred_Long are the coordinates that we want! 
#This is centred relative to where the actual focal trees are

#Curious how MD values vary from old coords and new ones:
ResTasMonthly <- read_csv("Data/site_coords_updated.csv")
#Can't have any extra columns because it will interfere with MD calculations below
ResTasMonthly <- ResTasMonthly %>% dplyr::select(Site, Centred_Lat, Centred_Long)
ResTasMonthly$CMD_1b <- raster::extract(x=tas_md1, y= cbind(ResTas$Centred_Long,ResTas$Centred_Lat))
ResTasMonthly$CMD_2b <- raster::extract(x=tas_md2, y= cbind(ResTas$Centred_Long,ResTas$Centred_Lat))
ResTasMonthly$CMD_3b <- raster::extract(x=tas_md3, y= cbind(ResTas$Centred_Long,ResTas$Centred_Lat))
ResTasMonthly$CMD_4b <- raster::extract(x=tas_md4, y= cbind(ResTas$Centred_Long,ResTas$Centred_Lat))
ResTasMonthly$CMD_5b <- raster::extract(x=tas_md5, y= cbind(ResTas$Centred_Long,ResTas$Centred_Lat))
ResTasMonthly$CMD_6b <- raster::extract(x=tas_md6, y= cbind(ResTas$Centred_Long,ResTas$Centred_Lat))
ResTasMonthly$CMD_7b <- raster::extract(x=tas_md7, y= cbind(ResTas$Centred_Long,ResTas$Centred_Lat))
ResTasMonthly$CMD_8b <- raster::extract(x=tas_md8, y= cbind(ResTas$Centred_Long,ResTas$Centred_Lat))
ResTasMonthly$CMD_9b <- raster::extract(x=tas_md9, y= cbind(ResTas$Centred_Long,ResTas$Centred_Lat))
ResTasMonthly$CMD_10b <- raster::extract(x=tas_md10, y= cbind(ResTas$Centred_Long,ResTas$Centred_Lat))
ResTasMonthly$CMD_11b <- raster::extract(x=tas_md11, y= cbind(ResTas$Centred_Long,ResTas$Centred_Lat))
ResTasMonthly$CMD_12b <- raster::extract(x=tas_md12, y= cbind(ResTas$Centred_Long,ResTas$Centred_Lat))

# import Site and Period table so that I can assign period 1 and 2 values directly
site_period <- period_climate %>% dplyr::select(Site, Period)
climate_diff <- left_join(ResTasMonthly, site_period)

## Summing them for each site based on growth period (dates_growth_site)
#From first month (including) to second month (excluding)
#Column 4 is January, 15 is December
climate_diff$norm_md <- 123

#Period 1 first:
#EPF
### EPF period 1 and 2 - feb to feb and march to march - give same values
# because it is one year 
climate_diff$norm_md[climate_diff$Site=="EPF" & climate_diff$Period == 1] <- sum(climate_diff[1,5:15], climate_diff[1,4])
climate_diff$norm_md[climate_diff$Site=="TMP" & climate_diff$Period == 1] <- sum(climate_diff[3,5:15], climate_diff[3,4:5])
climate_diff$norm_md[climate_diff$Site=="MER" & climate_diff$Period == 1] <- sum(climate_diff[5,5:15], climate_diff[5,4])
climate_diff$norm_md[climate_diff$Site=="DOG" & climate_diff$Period == 1] <- sum(climate_diff[7,5:15], climate_diff[7,4])
climate_diff$norm_md[climate_diff$Site=="GRA" & climate_diff$Period == 1] <- sum(climate_diff[9,5:15], climate_diff[9,4])
climate_diff$norm_md[climate_diff$Site=="FREY" & climate_diff$Period == 1] <- sum(climate_diff[11,5:15], climate_diff[11,4:5])
climate_diff$norm_md[climate_diff$Site=="BOF" & climate_diff$Period == 1] <- sum(climate_diff[13,9:15], climate_diff[13,4])

#Period 2
climate_diff$norm_md[climate_diff$Site=="EPF" & climate_diff$Period == 2] <- sum(climate_diff[1,6:15], climate_diff[1,4:5])
climate_diff$norm_md[climate_diff$Site=="TMP" & climate_diff$Period == 2] <- sum(climate_diff[3,6:15], climate_diff[3,4])
climate_diff$norm_md[climate_diff$Site=="MER" & climate_diff$Period == 2] <- sum(climate_diff[5,5:15], climate_diff[5,4:5])
climate_diff$norm_md[climate_diff$Site=="DOG" & climate_diff$Period == 2] <- sum(climate_diff[7,5:15], climate_diff[7,4])
climate_diff$norm_md[climate_diff$Site=="GRA" & climate_diff$Period == 2] <- sum(climate_diff[9,5:15], climate_diff[9,4])
climate_diff$norm_md[climate_diff$Site=="FREY" & climate_diff$Period == 2] <- sum(climate_diff[11,6:15], climate_diff[11,4:5])
climate_diff$norm_md[climate_diff$Site=="BOF" & climate_diff$Period == 2] <- sum(climate_diff[13,5:15], climate_diff[13,4:5])

climate_diff <- climate_diff %>% dplyr::select(Site, Period, norm_md)

### period climate is currently calculated to the day -
#I need to recalculate it at the month scale

#### Calculating MD for period climate at the monthly scale ####
## BOM dataframes are loaded in from data_preparations file
#Make a column for rainfall and pet data

climate_diff$period_ppt_by_month <- 123
climate_diff$period_pet_by_month <- 123

## EPF
#Calculate rainfall for period 1 (Feb 2020-Feb 2021: including (from first), excluding (to second))
epf_ppt_period1 <- bom_epf %>% subset(date >= "2020-02-01" & date <= "2021-01-31") %>%
  summarise(period1_rainfall = sum(`Rainfall amount (millimetres)`))
#Calculate rainfall for period 2 (March 2021 - March 2022)
epf_ppt_period2 <- bom_epf %>% subset(date >= "2021-03-01" & date <= "2022-02-28") %>%
  summarise(period2_rainfall = sum(`Rainfall amount (millimetres)`))
#Filling data in table
climate_diff <- within(climate_diff, period_ppt_by_month[Period == '1' & Site == 'EPF'] <- epf_ppt_period1)
climate_diff <- within(climate_diff, period_ppt_by_month[Period == '2' & Site == 'EPF'] <- epf_ppt_period2)
## TMP
tmp_ppt_period1 <- bom_tmp %>% subset(date >= "2020-02-01" & date <= "2021-02-28") %>%
  summarise(period1_rainfall = sum(`Rainfall amount (millimetres)`, na.rm=TRUE))
tmp_ppt_period2 <- bom_tmp %>% subset(date >= "2021-03-01" & date <= "2022-01-31") %>%
  summarise(period2_rainfall = sum(`Rainfall amount (millimetres)`, na.rm=TRUE))
climate_diff <- within(climate_diff, period_ppt_by_month[Period == '1' & Site == 'TMP'] <- tmp_ppt_period1)
climate_diff <- within(climate_diff, period_ppt_by_month[Period == '2' & Site == 'TMP'] <- tmp_ppt_period2)
## MER
mer_ppt_period1 <- bom_mer %>% subset(date >= "2020-02-01" & date <= "2021-01-31") %>%
  summarise(period1_rainfall = sum(`Rainfall amount (millimetres)`, na.rm = TRUE))
mer_ppt_period2 <- bom_mer %>% subset(date >= "2021-02-01" & date <= "2022-02-28") %>%
  summarise(period2_rainfall = sum(`Rainfall amount (millimetres)`, na.rm = TRUE))
climate_diff <- within(climate_diff, period_ppt_by_month[Period == '1' & Site == 'MER'] <- mer_ppt_period1)
climate_diff <- within(climate_diff, period_ppt_by_month[Period == '2' & Site == 'MER'] <- mer_ppt_period2)
## DOG
dog_ppt_period1 <- bom_dog %>% subset(date >= "2020-02-01" & date <= "2021-01-31") %>%
  summarise(period1_rainfall = sum(`Rainfall amount (millimetres)`, na.rm = TRUE))
dog_ppt_period2 <- bom_dog %>% subset(date >= "2021-02-01" & date <= "2022-02-28") %>%
  summarise(period2_rainfall = sum(`Rainfall amount (millimetres)`, na.rm = TRUE))
climate_diff <- within(climate_diff, period_ppt_by_month[Period == '1' & Site == 'DOG'] <- dog_ppt_period1)
climate_diff <- within(climate_diff, period_ppt_by_month[Period == '2' & Site == 'DOG'] <- dog_ppt_period2)
## GRA
gra_ppt_period1 <- bom_gra %>% subset(date >= "2020-02-01" & date <= "2021-01-31") %>%
  summarise(period1_rainfall = sum(`Rainfall amount (millimetres)`, na.rm = TRUE))
gra_ppt_period2 <- bom_gra %>% subset(date >= "2021-02-01" & date <= "2022-01-31") %>%
  summarise(period2_rainfall = sum(`Rainfall amount (millimetres)`, na.rm = TRUE))
climate_diff <- within(climate_diff, period_ppt_by_month[Period == '1' & Site == 'GRA'] <- gra_ppt_period1)
climate_diff <- within(climate_diff, period_ppt_by_month[Period == '2' & Site == 'GRA'] <- gra_ppt_period2)
## FREY
frey_ppt_period1 <- bom_frey %>% subset(date >= "2020-02-01" & date <= "2021-02-28") %>%
  summarise(period1_rainfall = sum(`Rainfall amount (millimetres)`, na.rm = TRUE))
frey_ppt_period2 <- bom_frey %>% subset(date >= "2021-03-01" & date <= "2022-02-28") %>%
  summarise(period2_rainfall = sum(`Rainfall amount (millimetres)`, na.rm = TRUE))
climate_diff <- within(climate_diff, period_ppt_by_month[Period == '1' & Site == 'FREY'] <- frey_ppt_period1)
climate_diff <- within(climate_diff, period_ppt_by_month[Period == '2' & Site == 'FREY'] <- frey_ppt_period2)
## BOF
bof_ppt_period1 <- bom_bof %>% subset(date >= "2020-06-01" & date <= "2021-01-31") %>%
  summarise(period1_rainfall = sum(`Rainfall amount (millimetres)`, na.rm = TRUE))
bof_ppt_period2 <- bom_bof %>% subset(date >= "2021-02-01" & date <= "2022-02-28") %>%
  summarise(period2_rainfall = sum(`Rainfall amount (millimetres)`, na.rm = TRUE))
climate_diff <- within(climate_diff, period_ppt_by_month[Period == '1' & Site == 'BOF'] <- bof_ppt_period1)
climate_diff <- within(climate_diff, period_ppt_by_month[Period == '2' & Site == 'BOF'] <- bof_ppt_period2)

# Some of these values are quite different - amount of rainfall that
# fell over exact dates and that fell at the monthly scale
#not a problem
#test <- period_climate %>% dplyr::select(Site, Period, period_rainfall)
#test2 <- climate_diff %>% dplyr::select(Site, Period, period_ppt_by_month)
#test3 <- left_join(test, test2)

#### Now for evapotranspiration
## EPF
epf_pet_period1 <- epf_evapo %>% subset(date >= "2020-02-01" & date <= "2021-01-31") %>%
  summarise(period1_pet = sum(evapotranspiration_mm, na.rm=TRUE))
epf_pet_period2 <- epf_evapo %>% subset(date >= "2021-03-01" & date <= "2022-02-28") %>%
  summarise(period2_pet = sum(evapotranspiration_mm, na.rm=TRUE))
climate_diff <- within(climate_diff, period_pet_by_month[Period == '1' & Site == 'EPF'] <- epf_pet_period1)
climate_diff <- within(climate_diff, period_pet_by_month[Period == '2' & Site == 'EPF'] <- epf_pet_period2)
## TMP
tmp_pet_period1 <- tmp_evapo %>% subset(date >= "2020-02-01" & date <= "2021-02-28") %>%
  summarise(period1_pet = sum(evapotranspiration_mm, na.rm=TRUE))
tmp_pet_period2 <- tmp_evapo %>% subset(date >= "2021-03-01" & date <= "2022-01-31") %>%
  summarise(period2_pet = sum(evapotranspiration_mm, na.rm=TRUE))
climate_diff <- within(climate_diff, period_pet_by_month[Period == '1' & Site == 'TMP'] <- tmp_pet_period1)
climate_diff <- within(climate_diff, period_pet_by_month[Period == '2' & Site == 'TMP'] <- tmp_pet_period2)
## MER
#Uses same dataset as DOG for evapotranspiration
mer_pet_period1 <- dog_evapo %>% subset(date >= "2020-02-01" & date <= "2021-01-31") %>%
  summarise(period1_pet = sum(evapotranspiration_mm, na.rm = TRUE))
mer_pet_period2 <- dog_evapo %>% subset(date >= "2021-02-01" & date <= "2022-02-28") %>%
  summarise(period2_pet = sum(evapotranspiration_mm, na.rm = TRUE))
climate_diff <- within(climate_diff, period_pet_by_month[Period == '1' & Site == 'MER'] <- mer_pet_period1)
climate_diff <- within(climate_diff, period_pet_by_month[Period == '2' & Site == 'MER'] <- mer_pet_period2)
## DOG
dog_pet_period1 <- dog_evapo %>% subset(date >= "2020-02-01" & date <= "2021-01-31") %>%
  summarise(period1_pet = sum(evapotranspiration_mm, na.rm = TRUE))
dog_pet_period2 <- dog_evapo %>% subset(date >= "2021-02-01" & date <= "2022-02-28") %>%
  summarise(period2_pet = sum(evapotranspiration_mm, na.rm = TRUE))
climate_diff <- within(climate_diff, period_pet_by_month[Period == '1' & Site == 'DOG'] <- dog_pet_period1)
climate_diff <- within(climate_diff, period_pet_by_month[Period == '2' & Site == 'DOG'] <- dog_pet_period2)
## GRA
gra_pet_period1 <- gra_evapo %>% subset(date >= "2020-02-01" & date <= "2021-01-31") %>%
  summarise(period1_pet = sum(evapotranspiration_mm, na.rm = TRUE))
gra_pet_period2 <- gra_evapo %>% subset(date >= "2021-02-01" & date <= "2022-01-31") %>%
  summarise(period2_pet = sum(evapotranspiration_mm, na.rm = TRUE))
climate_diff <- within(climate_diff, period_pet_by_month[Period == '1' & Site == 'GRA'] <- gra_pet_period1)
climate_diff <- within(climate_diff, period_pet_by_month[Period == '2' & Site == 'GRA'] <- gra_pet_period2)
## FREY
frey_pet_period1 <- frey_evapo %>% subset(date >= "2020-02-01" & date <= "2021-02-28") %>%
  summarise(period1_pet = sum(evapotranspiration_mm, na.rm = TRUE))
frey_pet_period2 <- frey_evapo %>% subset(date >= "2021-03-01" & date <= "2022-02-28") %>%
  summarise(period2_pet = sum(evapotranspiration_mm, na.rm = TRUE))
climate_diff <- within(climate_diff, period_pet_by_month[Period == '1' & Site == 'FREY'] <- frey_pet_period1)
climate_diff <- within(climate_diff, period_pet_by_month[Period == '2' & Site == 'FREY'] <- frey_pet_period2)
## BOF
bof_pet_period1 <- bof_evapo %>% subset(date >= "2020-06-01" & date <= "2021-01-31") %>%
  summarise(period1_pet = sum(evapotranspiration_mm, na.rm = TRUE))
bof_pet_period2 <- bof_evapo %>% subset(date >= "2021-02-01" & date <= "2022-02-28") %>%
  summarise(period2_pet = sum(evapotranspiration_mm, na.rm = TRUE))
climate_diff <- within(climate_diff, period_pet_by_month[Period == '1' & Site == 'BOF'] <- bof_pet_period1)
climate_diff <- within(climate_diff, period_pet_by_month[Period == '2' & Site == 'BOF'] <- bof_pet_period2)

#Calculate monthly period moisture deficit 
climate_diff$period_pet_by_month <- as.numeric(climate_diff$period_pet_by_month)
climate_diff$period_ppt_by_month <- as.numeric(climate_diff$period_ppt_by_month)
climate_diff <- climate_diff %>% mutate(monthly_period_md = period_pet_by_month - period_ppt_by_month)

## Simplify dataset
climate_diff <- climate_diff %>% dplyr::select(Site, Period, norm_md, monthly_period_md)

## Plot them
ggplot(climate_diff, aes (x = norm_md, y = monthly_period_md, colour = Site))+
  geom_point(cex=2, alpha =0.8)+
  theme_classic()

ggplot(climate_diff, aes (x = Site, y = norm_md, colour = Period))+
geom_point(cex=3, alpha =0.4)+
  theme_classic()

## Calculate anomaly!
#Check that these negatives make sense
climate_diff <- climate_diff %>% mutate(anomaly = monthly_period_md-norm_md)

ggplot(climate_diff, aes (x = Site, y = anomaly, colour = Period))+
  geom_point(cex=3, alpha =0.4)+
  theme_classic()







