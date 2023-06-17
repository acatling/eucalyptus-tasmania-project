### Eucalyptus Growth Tasmania Project 
# Alexandra Catling PhD Research
# Copy of data_preparations.R excluding climate variables
# This script will generate the finalised clean growth dataset with survey information
# For Tom McMahon to use for Honours research 2023

### Importing packages and functions ####
library(tidyverse)
library(ggplot2)
library(lme4)
library(MuMIn)
library(lmerTest)
library(emmeans)
library(DHARMa)
library(car)
select <- dplyr::select

#Read in functions
source("functions.R")

#### Importing and tidying data ####
## Importing growth data
growthdataraw <-read_csv("Data/growth_data.csv")
growthdata <- growthdataraw
## MER VIMI B4 and MER VIMI B5 incorrectly recorded as DOG B4 and B5
#Adjusting these. There are no DOG VIMI Bs!
# This the case for both initial dbh and growth measurements - trees may be mislabelled
growthdata <- within(growthdata, Site[Site == 'DOG' & Focal_sp == 'VIMI' & Plot == 'B' & Tree == '4'] <- 'MER')
growthdata <- within(growthdata, Site[Site == 'DOG' & Focal_sp == 'VIMI' & Plot == 'B' & Tree == '5'] <- 'MER')

## Importing initial DBH data
initialdbhdataraw <- read_csv("Data/initial_field_dbh_data.csv")
# Removing CII column - crown index that I never measured
initialdbhdata <- initialdbhdataraw %>% select(-CII)
#Creating column for abbreviated Focal_sp
initialdbhdata$Focal_sp <- initialdbhdata$Species
initialdbhdata <- within(initialdbhdata, Focal_sp[Species == "E. amygdalina"] <- "AMYG")
initialdbhdata <- within(initialdbhdata, Focal_sp[Species == "E. obliqua"] <- "OBLI")
initialdbhdata <- within(initialdbhdata, Focal_sp[Species == "E. ovata"] <- "OVAT")
initialdbhdata <- within(initialdbhdata, Focal_sp[Species == "E. viminalis"] <- "VIMI")

#Merging DBH data with growth data
growthdata <- left_join(growthdata, initialdbhdata, by = c("Site", "Focal_sp", "Plot", "Tree"))

#Note that DBH_cm is not numeric because of two NAs (can be dropped) and two UNs (unknown DBH but have growth data)

#Removing trees that I don't have growth data for at all (no GPS coords or removed) (7)
#and trees that were incorrectly IDd (6) total 13 trees
trees_to_remove <- read_csv("Data/trees_to_remove.csv")
growthdata <- anti_join(growthdata, trees_to_remove, by = c("Site", "Focal_sp", "Plot", "Tree"))
#Tree fall recorded for DOG AMYG B5 on 19/01/22, so removing growth data for period 2 and period 3
#Removed this note from community_surveys_data sheet too so that rows aren't duplicated for NCI calcs
growthdata <- within(growthdata, Growth[Site == "DOG" & Focal_sp == "AMYG" & Plot == "B" & Tree == "5" & Period == "3"] <- NA)
growthdata <- within(growthdata, Growth[Site == "DOG" & Focal_sp == "AMYG" & Plot == "B" & Tree == "5" & Period == "2"] <- NA)

## Want to calculate growth as a rate, as different number of days between notching and growth measurements
#this dates_growth_and_bom_sites file also has info on nearest BOM site
datesdataraw <- read_csv("Data/dates_growth_and_bom_sites.csv")

#Making period a factor
growthdata$Period <- as.factor(growthdata$Period)
datesdataraw$Period <- as.factor(datesdataraw$Period)
datesdata <- datesdataraw %>% select(Site, Period, Days_growth_since_notching)
#Merging with growth data
growthdata <- left_join(growthdata, datesdata, by = c("Site", "Period"))

#Different units for growth at the moment. 2021 and 2023 in mm and 2022 in cm. Converting 2022 growth cm to mm. 
growthdata$Growth[growthdata$Period==2] <- growthdata$Growth[growthdata$Period==2]*10

# Note that this is not true growth but instead the distance of dendrometer from time '0'
#so true growth for period 2 is 'growth' minus growth in period 1
#Creating a column for true growth
#Period 1 = growth
# Period 2 = growth period 2 - growth period 1
#Period 3 = growth period 3 - growth period 2
growthdata$true_growth <- growthdata$Growth
growthdata$true_growth[growthdata$Period==2] <- growthdata$Growth[growthdata$Period==2]-growthdata$Growth[growthdata$Period==1]
growthdata$true_growth[growthdata$Period==3] <- growthdata$Growth[growthdata$Period==3]-growthdata$Growth[growthdata$Period==2]

#Creating a column for days of growth in the period
#since number_days_growth refers to total/relative to time '0'
growthdata$days_in_period <- growthdata$Days_growth_since_notching
growthdata$days_in_period[growthdata$Period==2] <- growthdata$Days_growth_since_notching[growthdata$Period==2]-growthdata$Days_growth_since_notching[growthdata$Period==1]
growthdata$days_in_period[growthdata$Period==3] <- growthdata$Days_growth_since_notching[growthdata$Period==3]-growthdata$Days_growth_since_notching[growthdata$Period==2]

#Adjust the period 1 values that were notched in period 1 so therefore only have period 2 and 3 values:
#instead of removing, just leaving them as NAs (so that same number of rows for period 1 and 2)
# GRA OBLI A 10, DOG AMYG B8, EPF AMYG B6
##Need to adjust their period 2 values which are currently NAs, setting true growth value to growth value
# and yes need to use growth here, not true_growth, since growth only represents period 2 anyway
# had to remove them after calculations otherwise unequal number for period 2 - period 1
growthdata <- within(growthdata, true_growth[Site == "GRA" & Focal_sp == "OBLI" & Plot == "A" & Tree == "10" & Period == "2"] <- growthdata$Growth[Site == "GRA" & Focal_sp == "OBLI" & Plot == "A" & Tree == "10" & Period == "2"])
growthdata <- within(growthdata, true_growth[Site == "DOG" & Focal_sp == "AMYG" & Plot == "B" & Tree == "8" & Period == "2"] <- growthdata$Growth[Site == "DOG" & Focal_sp == "AMYG" & Plot == "B" & Tree == "8" & Period == "2"])
growthdata <- within(growthdata, true_growth[Site == "EPF" & Focal_sp == "AMYG" & Plot == "B" & Tree == "6" & Period == "2"] <- growthdata$Growth[Site == "EPF" & Focal_sp == "AMYG" & Plot == "B" & Tree == "6" & Period =="2"])

##Adjusting growth of DOG AMYG D1 that had damage to the band
#period 1 is fine
#period 3 is difference in distances between notch and left side of rightmost clamp/seal
#179.95 (p3) - 151.98 (p2) = 27.97
growthdata <- within(growthdata, true_growth[Site == "DOG" & Focal_sp == "AMYG" & Plot == "D" & Tree == "1" & Period == "3"] <- 27.97)
#Period 2 must be NA. Have good photos of band from period 1 and 2
growthdata <- within(growthdata, true_growth[Site == "DOG" & Focal_sp == "AMYG" & Plot == "D" & Tree == "1" & Period == "2"] <- NA)
#dont have initial dbh for period 3, this makes sense since we don't have growth for period 2

## Changing negative growth to zero
growthdata <- growthdata %>% mutate(growth_no_negs = true_growth,
                                    DBH_cm = `DBH (cm)`)
growthdata <- within(growthdata, growth_no_negs[growth_no_negs<0] <- '0')

#Calculate growth rate (mm/day)
growthdata$growth_no_negs <- as.numeric(growthdata$growth_no_negs)
growthdata <- growthdata %>% mutate(growth_rate = growth_no_negs/days_in_period)

#Making initial DBH (DBH_cm) numeric (inputted with some unknown characters)
growthdata$DBH_cm <- as.numeric(growthdata$DBH_cm)

### Importing community survey data ####
rawsurveydata <- read_csv("Data/community_surveys_data.csv", col_types = cols(.default = "?", Neighbour_DBH_cm_5 = col_double(), Neighbour_DBH_cm_6 = col_double(), Neighbour_DBH_cm_7 = col_double(), Neighbour_DBH_cm_8 = col_double(), Neighbour_DBH_cm_9 = col_double(), Neighbour_DBH_cm_10 = col_double(), Neighbour_DBH_cm_11 = col_double()))
#Parsing error using just read_csv because it is deciding what is in the column from first 1000 rows
# and there\\ trees with > 7 stems are after first 1000 rows

#Removing 5 NAs for Neighbour_DBH_cm where the DBH was not recorded
surveydata <- rawsurveydata %>% filter(!is.na(Neighbour_DBH_cm))
#GRA OBLI B 6 does not have survey data - accidentally recorded as A6 with two surveys, but there is a note
#adjusted directly in data
#TMP OBLI C6 dos not have survey data - accidentally labelled as A6, which is repeated
#adjusted directly in data (true C6 was recorded on 27/01/2022)
#Completed surveys for 310 plots

## Removing trees (no growth data or incorrectly IDd or loose band or extensive loose bark or overlapping neighbourhoods)
#EPF OVAT C1 and C2 right next to each other (4 m away), I couldn't decide which to study
#Same with #BOF VIMI B4 and B3 so removing the second one of each (VIMI B4 and OVAT C2) via trees_to_remove
#(since I can't have overlapping neighbourhoods, pseudo-replication)
surveydata <- anti_join(surveydata, trees_to_remove, by = c("Site", "Focal_sp", "Plot", "Tree"))
#EPF ovat c1 and c2 share only a quarter or less of overlapping neighbourhood, that's okay
#same with epf ovat b7 and b6,f fine

#Replacing NAs with 0s (to merge neighbour DBHs later)
surveydata <- within(surveydata, Neighbour_DBH_cm_2[is.na(Neighbour_DBH_cm_2)] <- '0')
surveydata <- within(surveydata, Neighbour_DBH_cm_3[is.na(Neighbour_DBH_cm_3)] <- '0')
surveydata <- within(surveydata, Neighbour_DBH_cm_4[is.na(Neighbour_DBH_cm_4)] <- '0')
surveydata <- within(surveydata, Neighbour_DBH_cm_5[is.na(Neighbour_DBH_cm_5)] <- '0')
surveydata <- within(surveydata, Neighbour_DBH_cm_6[is.na(Neighbour_DBH_cm_6)] <- '0')
surveydata <- within(surveydata, Neighbour_DBH_cm_7[is.na(Neighbour_DBH_cm_7)] <- '0')
surveydata <- within(surveydata, Neighbour_DBH_cm_8[is.na(Neighbour_DBH_cm_8)] <- '0')
surveydata <- within(surveydata, Neighbour_DBH_cm_9[is.na(Neighbour_DBH_cm_9)] <- '0')
surveydata <- within(surveydata, Neighbour_DBH_cm_10[is.na(Neighbour_DBH_cm_10)] <- '0')
surveydata <- within(surveydata, Neighbour_DBH_cm_11[is.na(Neighbour_DBH_cm_11)] <- '0')

#Adding another column with the sum of all DBHs across each row
#Columns were dbls, needed to run this for  below code to work
surveydata$Neighbour_DBH_cm <- as.numeric(surveydata$Neighbour_DBH_cm)
surveydata$Neighbour_DBH_cm_2 <- as.numeric(surveydata$Neighbour_DBH_cm_2)
surveydata$Neighbour_DBH_cm_3 <- as.numeric(surveydata$Neighbour_DBH_cm_3)
surveydata$Neighbour_DBH_cm_4 <- as.numeric(surveydata$Neighbour_DBH_cm_4)
surveydata$Neighbour_DBH_cm_5 <- as.numeric(surveydata$Neighbour_DBH_cm_5)
surveydata$Neighbour_DBH_cm_6 <- as.numeric(surveydata$Neighbour_DBH_cm_6)
surveydata$Neighbour_DBH_cm_7 <- as.numeric(surveydata$Neighbour_DBH_cm_7)
surveydata$Neighbour_DBH_cm_8 <- as.numeric(surveydata$Neighbour_DBH_cm_8)
surveydata$Neighbour_DBH_cm_9 <- as.numeric(surveydata$Neighbour_DBH_cm_9)
surveydata$Neighbour_DBH_cm_10 <- as.numeric(surveydata$Neighbour_DBH_cm_10)
surveydata$Neighbour_DBH_cm_11 <- as.numeric(surveydata$Neighbour_DBH_cm_11)

surveydata <- surveydata %>% mutate(Sum_nbh_DBH = `Neighbour_DBH_cm` + `Neighbour_DBH_cm_2` + `Neighbour_DBH_cm_3` + 
                                      `Neighbour_DBH_cm_4` + `Neighbour_DBH_cm_5` + 
                                      `Neighbour_DBH_cm_6` + `Neighbour_DBH_cm_7` + `Neighbour_DBH_cm_8` + 
                                      `Neighbour_DBH_cm_9` + `Neighbour_DBH_cm_10` + `Neighbour_DBH_cm_11`)

#### Calculating total DBH, NCI and number of neighbours ####
# Can't add DBHs, so need to calculate basal area and sum that
#Basal area = pi * radius^2
# = pi * (diameter/2)^2
#DBH is recorded in cm, calculating basal area in m^2
surveydata <- surveydata %>% mutate(nbh_basal_area = pi*(Sum_nbh_DBH/2)^2)

#Summing basal area across rows for each focal tree
# and converting to m^2
totalnbhdbhdata <- surveydata %>% group_by(Site, Focal_sp, Plot, Tree) %>%
  summarise(total_nbh_ba = (sum(nbh_basal_area))/10000)
#Merge total neighbour DBH back into dataframe
surveydata <- left_join(surveydata, totalnbhdbhdata)

#Doing the above but for number of neighbours
numbernbhdata <- surveydata %>% group_by(Site, Focal_sp, Plot, Tree) %>% 
  summarise(number_neighbours = n())
surveydata <- left_join(surveydata, numbernbhdata)

## Calculating NCIs using Heygi's competition index:
# NCI = sum from n to the ith neighbour of di/(d*dist)
#Where di = DBH of the ith neighbour tree (cm)
#d = DBH of focal tree (cm)
#dist = horizontal distance of neighbour from focal (m)

#Need to import DBH data to surveydata to calculate NCI
dbhdatatomerge <- growthdata %>% group_by(Site, Focal_sp, Plot, Tree) %>%
  filter(row_number() == 1) %>% select(Site, Focal_sp, Plot, Tree, DBH_cm)
surveydata <- left_join(surveydata, dbhdatatomerge)

#Calculating total, intra and inter NCI by focal tree
surveydata <- surveydata %>% mutate(NCI_nbh = Neighbour_DBH_cm/Neighbour_distance_m)

#Need to change Focal_sp names to full scientific names for matching to work
#Actually will do it the other way
surveydata <- within(surveydata, Neighbour_sp_ID[Neighbour_sp_ID == 'Eucalyptus amygdalina'] <- 'AMYG')
surveydata <- within(surveydata, Neighbour_sp_ID[Neighbour_sp_ID == 'Eucalyptus obliqua'] <- 'OBLI')
surveydata <- within(surveydata, Neighbour_sp_ID[Neighbour_sp_ID == 'Eucalyptus ovata'] <- 'OVAT')
surveydata <- within(surveydata, Neighbour_sp_ID[Neighbour_sp_ID == 'Eucalyptus viminalis'] <- 'VIMI')

surveydata$Matching <- surveydata$Focal_sp == surveydata$Neighbour_sp_ID
surveydata$Matching <- ifelse(surveydata$Matching == TRUE, 1, 0)

#Need na.rm = TRUE to remove rows with no known nbh distance resulting in NA nci_nbh
totalncidata <- surveydata %>% group_by(Site, Focal_sp, Plot, Tree) %>%
  summarise(total_nci = sum(NCI_nbh, na.rm = TRUE),
            intra_nci = sum(NCI_nbh[Matching == "1"], na.rm = TRUE),
            inter_nci = sum(NCI_nbh[Matching == "0"], na.rm = TRUE))

surveydata <- left_join(surveydata, totalncidata)

### Selecting columns to merge with growth data
surveysimple <- surveydata %>% group_by(Site, Focal_sp, Plot, Tree) %>% 
  filter(row_number() == 1) %>% select(Site, Focal_sp, Plot, Tree, 
                                       total_nbh_ba, number_neighbours, 
                                       total_nci, intra_nci, inter_nci)
#Merging neighbour data with growth rate data
#DOG OBLI B2 is meant to be D2, adjusted in survey notes
#GRA amyg c 1  was resurveyed, adjusted in survey notes
growthnbhdata <- left_join(growthdata, surveysimple, by = c("Site", "Focal_sp", "Plot", "Tree"))

### Adjusting names of some plots
#GRA OVAT A3 and GRA OVAT A4 - notes say 'this is a VIMI'
#these are in the VIMI A area. Renaming OVAT A3 = VIMI A10 and OVAT A4 = VIMI A11
#Changing these to VIMI and plot Z in growth notes
growthnbhdata <- within(growthnbhdata, Tree[Site == 'GRA' & Focal_sp == 'OVAT' & Plot == 'A' & Tree == '3'] <- '10')
growthnbhdata <- within(growthnbhdata, Focal_sp[Site == 'GRA' & Focal_sp == 'OVAT' & Plot == 'A' & Tree == '10'] <- 'VIMI')
growthnbhdata <- within(growthnbhdata, Tree[Site == 'GRA' & Focal_sp == 'OVAT' & Plot == 'A' & Tree == '4'] <- '11')
growthnbhdata <- within(growthnbhdata, Focal_sp[Site == 'GRA' & Focal_sp == 'OVAT' & Plot == 'A' & Tree == '11'] <- 'VIMI')

#EPF VIMI B1 is an ovata: this is the OVAT B area - let's call it OVAT B 10 (previously had it as OVAT Z1)
growthnbhdata <- within(growthnbhdata, Tree[Site == 'EPF' & Focal_sp == 'VIMI' & Plot == 'B' & Tree == '1'] <- '10')
growthnbhdata <- within(growthnbhdata, Focal_sp[Site == 'EPF' & Focal_sp == 'VIMI' & Plot == 'B' & Tree == '10'] <- 'OVAT')

#### Soil data ####
soildataraw <- read_csv("Data/soil_data.csv")

#Making nitrate_nitrogen <1 values 0 [so that I can sum them for total N]
soildata <- within(soildataraw, `Nitrate_Nitrogen_mg/kg`[`Nitrate_Nitrogen_mg/kg` == '<1'] <- "0")
#Need nitrate_nitrogen to be numeric, not sure why not working unless I remake column
soildata <- soildata %>% mutate(nitrate = `Nitrate_Nitrogen_mg/kg`,
                                ammonium = `Ammonium_Nitrogen_mg/kg`)
soildata$nitrate <- as.numeric(soildata$nitrate)
soildata <- soildata %>% mutate(total_nitrogen = nitrate + ammonium)
#Averaging soil pH values, actually will just use CaCl one for now
#Selecting and renaming columns for PCA
soildata <- soildata %>% select(Sample_ID, phosphorous = 'Phosphorus_Colwell_mg/kg', potassium = `Potassium_Colwell_mg/kg`, 
                                sulfur = `Sulfur_mg/kg`, carbon = `Organic_Carbon_%`, conductivity = `Conductivity_dS/m`, 
                                pH = `pH_Level_(CaCl2)`, 'total nitrogen' = total_nitrogen)

soildata_pca <- as.data.frame(soildata)
rownames(soildata_pca) <- soildata_pca$Sample_ID
soildata_pca <- as.data.frame(soildata_pca[,-1])
soil_pca <- princomp(soildata_pca, cor = TRUE)
summary(soil_pca)
loadings(soil_pca)
soildata$PC1<-soil_pca$scores[,1]
soildata$PC2<-soil_pca$scores[,2]
soildata$PC3<-soil_pca$scores[,3]

## Merge PC1 and PC2 into main dataset
pc1andpc2 <- soildata %>% select(PC1, PC2)
#Warning messages from below line are fine
soildata <- soildata %>% separate(Sample_ID, c("Site", "Plot"), sep = "([ ?-])")

## Make soil ID and match soil values to that soil ID
soildata <- soildata %>% unite("soil_id", Site:Plot, remove = "false")
growthnbhdata$soil_id <- 'ABC'

#Here the VIMI site is TMP-VI which needs to match growthnbhdata in join, rename it in both
###TMP
#obli A, B and C. Vimi alone VI-A.
#soildata <- within(soildata, Plot[Plot == "VI"] <- "TMP_VI-A")
#growthnbhdata <- within(growthnbhdata, Plot[Focal_sp == "VIMI" & Site == "TMP"] <- "VI-A")
growthnbhdata <- within(growthnbhdata, soil_id[Site=="TMP" & Focal_sp == "VIMI"] <- "TMP_VI")
growthnbhdata <- within(growthnbhdata, soil_id[Site=="TMP" & Focal_sp == "OBLI" & Plot == "A"] <- "TMP_A")
growthnbhdata <- within(growthnbhdata, soil_id[Site=="TMP" & Focal_sp == "OBLI" & Plot == "B"] <- "TMP_B")
growthnbhdata <- within(growthnbhdata, soil_id[Site=="TMP" & Focal_sp == "OBLI" & Plot == "C"] <- "TMP_C")

###FREY values should be same for all plots (measured at site-level)
growthnbhdata <- within(growthnbhdata, soil_id[Site=="FREY"] <- "FREY_NA")

###MER - for all species, 1=A, 2=C, 3=B
growthnbhdata <- within(growthnbhdata, soil_id[Site=="MER" & Plot == "A"] <- "MER_1")
growthnbhdata <- within(growthnbhdata, soil_id[Site=="MER" & Plot == "B"] <- "MER_2")
growthnbhdata <- within(growthnbhdata, soil_id[Site=="MER" & Plot == "C"] <- "MER_3")

###DOG - different plot names per species
#AMYG: 1=D, 2=C, 3=B. #OBLI: 1=F, 2=E, 3=D. #VIMI: 1=E, 2=D, 3=C.
growthnbhdata <- within(growthnbhdata, soil_id[Site=="DOG" & Focal_sp == "AMYG" & Plot == "B"] <- "DOG_3")
growthnbhdata <- within(growthnbhdata, soil_id[Site=="DOG" & Focal_sp == "AMYG" & Plot == "C"] <- "DOG_2")
growthnbhdata <- within(growthnbhdata, soil_id[Site=="DOG" & Focal_sp == "AMYG" & Plot == "D"] <- "DOG_1")
growthnbhdata <- within(growthnbhdata, soil_id[Site=="DOG" & Focal_sp == "OBLI" & Plot == "D"] <- "DOG_3")
growthnbhdata <- within(growthnbhdata, soil_id[Site=="DOG" & Focal_sp == "OBLI" & Plot == "E"] <- "DOG_2")
growthnbhdata <- within(growthnbhdata, soil_id[Site=="DOG" & Focal_sp == "OBLI" & Plot == "F"] <- "DOG_1")
growthnbhdata <- within(growthnbhdata, soil_id[Site=="DOG" & Focal_sp == "VIMI" & Plot == "C"] <- "DOG_3")
growthnbhdata <- within(growthnbhdata, soil_id[Site=="DOG" & Focal_sp == "VIMI" & Plot == "D"] <- "DOG_2")
growthnbhdata <- within(growthnbhdata, soil_id[Site=="DOG" & Focal_sp == "VIMI" & Plot == "E"] <- "DOG_1")

###GRA - A, B and C lines up with sampling for all species
growthnbhdata <- within(growthnbhdata, soil_id[Site=="GRA" & Plot == "A"] <- "GRA_A")
growthnbhdata <- within(growthnbhdata, soil_id[Site=="GRA" & Plot == "B"] <- "GRA_B")
growthnbhdata <- within(growthnbhdata, soil_id[Site=="GRA" & Plot == "C"] <- "GRA_C")

###EPF - varies by species
#AMYG: 1=C, 2=B, 3=A  #OVAT: 4=A, 2=B, 3=C. #VIMI: 4=D, 1=C, 2=B, 3=A
growthnbhdata <- within(growthnbhdata, soil_id[Site=="EPF" & Focal_sp == "AMYG" & Plot == "C"] <- "EPF_1")
growthnbhdata <- within(growthnbhdata, soil_id[Site=="EPF" & Focal_sp == "AMYG" & Plot == "B"] <- "EPF_2")
growthnbhdata <- within(growthnbhdata, soil_id[Site=="EPF" & Focal_sp == "AMYG" & Plot == "A"] <- "EPF_3")
growthnbhdata <- within(growthnbhdata, soil_id[Site=="EPF" & Focal_sp == "OVAT" & Plot == "A"] <- "EPF_4")
growthnbhdata <- within(growthnbhdata, soil_id[Site=="EPF" & Focal_sp == "OVAT" & Plot == "B"] <- "EPF_2")
growthnbhdata <- within(growthnbhdata, soil_id[Site=="EPF" & Focal_sp == "OVAT" & Plot == "C"] <- "EPF_3")
growthnbhdata <- within(growthnbhdata, soil_id[Site=="EPF" & Focal_sp == "VIMI" & Plot == "A"] <- "EPF_3")
growthnbhdata <- within(growthnbhdata, soil_id[Site=="EPF" & Focal_sp == "VIMI" & Plot == "B"] <- "EPF_2")
growthnbhdata <- within(growthnbhdata, soil_id[Site=="EPF" & Focal_sp == "VIMI" & Plot == "C"] <- "EPF_1")
growthnbhdata <- within(growthnbhdata, soil_id[Site=="EPF" & Focal_sp == "VIMI" & Plot == "D"] <- "EPF_4")

###BOF - varies by species
#AMYG: 1=B, 2=C and A, OVAT: 1=C, 2=D and B, 3=E, 4=A. 
#VIMI: 1=B, 2=C and A, 3=D-F: VIE1, VIF2, VIF3, VID1, VID2.
growthnbhdata <- within(growthnbhdata, soil_id[Site=="BOF" & Focal_sp == "AMYG" & Plot == "A"] <- "BOF_B2")
growthnbhdata <- within(growthnbhdata, soil_id[Site=="BOF" & Focal_sp == "AMYG" & Plot == "B"] <- "BOF_B1")
growthnbhdata <- within(growthnbhdata, soil_id[Site=="BOF" & Focal_sp == "AMYG" & Plot == "C"] <- "BOF_B2")
growthnbhdata <- within(growthnbhdata, soil_id[Site=="BOF" & Focal_sp == "OVAT" & Plot == "A"] <- "BOF_B4")
growthnbhdata <- within(growthnbhdata, soil_id[Site=="BOF" & Focal_sp == "OVAT" & Plot == "B"] <- "BOF_B2")
growthnbhdata <- within(growthnbhdata, soil_id[Site=="BOF" & Focal_sp == "OVAT" & Plot == "C"] <- "BOF_B1")
growthnbhdata <- within(growthnbhdata, soil_id[Site=="BOF" & Focal_sp == "OVAT" & Plot == "D"] <- "BOF_B2")
growthnbhdata <- within(growthnbhdata, soil_id[Site=="BOF" & Focal_sp == "OVAT" & Plot == "E"] <- "BOF_B3")
growthnbhdata <- within(growthnbhdata, soil_id[Site=="BOF" & Focal_sp == "VIMI" & Plot == "A"] <- "BOF_B2")
growthnbhdata <- within(growthnbhdata, soil_id[Site=="BOF" & Focal_sp == "VIMI" & Plot == "B"] <- "BOF_B1")
growthnbhdata <- within(growthnbhdata, soil_id[Site=="BOF" & Focal_sp == "VIMI" & Plot == "C"] <- "BOF_B2")
growthnbhdata <- within(growthnbhdata, soil_id[Site=="BOF" & Focal_sp == "VIMI" & Plot == "D"] <- "BOF_B3")
growthnbhdata <- within(growthnbhdata, soil_id[Site=="BOF" & Focal_sp == "VIMI" & Plot == "E"] <- "BOF_B3")
growthnbhdata <- within(growthnbhdata, soil_id[Site=="BOF" & Focal_sp == "VIMI" & Plot == "F"] <- "BOF_B3")

### Simplifying soil dataset to merge with growthnbhdata
soil_simple <- soildata %>% select(soil_id, PC1, PC2)
growthnbhdata <- left_join(growthnbhdata, soil_simple)

## Making a simplified dataset with summary data ####
## Calculate preceding focal DBH - size before each growth measurement
#This is just DBH_cm for period 1, but for period 2 should be DBH_cm + growth in period 1
#Growth is in mm so converting DBH to mm too
growthnbhdata <- growthnbhdata %>% mutate(DBH_mm = DBH_cm*10,
                                          preceding_dbh = DBH_mm)

growthnbhdata$preceding_dbh[growthnbhdata$Period==2] <- growthnbhdata$preceding_dbh[growthnbhdata$Period==1]+growthnbhdata$growth_no_negs[growthnbhdata$Period==1]

#Fixing preceding_dbh of trees that were notched late
# GRA OBLI A 10, DOG AMYG B8, EPF AMYG B6
growthnbhdata <- within(growthnbhdata, preceding_dbh[Site == 'EPF' & Focal_sp == 'AMYG' & Plot == 'B' & Tree == '6' & Period == 2] <- growthnbhdata$DBH_mm[Site == 'EPF' & Focal_sp == 'AMYG' & Plot == 'B' & Tree == '6' & Period == 1])
growthnbhdata <- within(growthnbhdata, preceding_dbh[Site == 'GRA' & Focal_sp == 'OBLI' & Plot == 'A' & Tree == '10' & Period == 2] <- growthnbhdata$DBH_mm[Site == 'GRA' & Focal_sp == 'OBLI' & Plot == 'A' & Tree == '10' & Period == 1])
growthnbhdata <- within(growthnbhdata, preceding_dbh[Site == 'DOG' & Focal_sp == 'AMYG' & Plot == 'B' & Tree == '8' & Period == 2] <- growthnbhdata$DBH_mm[Site == 'DOG' & Focal_sp == 'AMYG' & Plot == 'B' & Tree == '8' & Period == 1])

growthnbhdata$preceding_dbh[growthnbhdata$Period==3] <- growthnbhdata$preceding_dbh[growthnbhdata$Period==2]+growthnbhdata$growth_no_negs[growthnbhdata$Period==2]

### Simplifying dataset
growthnbhdata <- growthnbhdata %>% select(Site, Focal_sp, Plot, Tree, Period, Growth,
                                          preceding_dbh, growth_no_negs, growth_rate,
                                          total_nbh_ba, number_neighbours, total_nci, intra_nci, inter_nci,
                                          PC1, PC2)

#test <- growthnbhdata %>% filter(is.na(inter_nci))
#Check why I have 4 total NAs. Some may be resurveyed gra? Same as above, for now, removing them
growthnbhdata <- growthnbhdata %>% filter(!(is.na(total_nci)))

##Intra and inter have zero values so adding 1 before logging
growthnbhdata <- growthnbhdata %>% mutate(log_total_nci = log(total_nci))
growthnbhdata <- growthnbhdata %>% mutate(log_p1_intra_nci = log(intra_nci+1))
growthnbhdata <- growthnbhdata %>% mutate(log_p1_inter_nci = log(inter_nci+1))

## Standardising continuous predictors to a mean of 0 and SD of 1
#I want global standardised means and SDs across species for plot and model comparisions
#Scaling NCI
growthnbhdata <- growthnbhdata %>% mutate(std_total_nci = scale(log_total_nci, center = TRUE, scale = TRUE)[,1])
#Scaling intra and inter NCI by the mean and sd of total NCI
#to compare on the same scales when plotting (between each other and relative to total NCI)
growthnbhdata <- growthnbhdata %>% mutate(plot_intra_nci = (log_p1_intra_nci-mean(log_total_nci))/sd(log_total_nci))
growthnbhdata <- growthnbhdata %>% mutate(plot_inter_nci = (log_p1_inter_nci-mean(log_total_nci))/sd(log_total_nci))
##But I also want each to be standardised to zero for model comparisons.
growthnbhdata <- growthnbhdata %>% mutate(std_intra_nci = scale(log_p1_intra_nci, center = TRUE, scale = TRUE)[,1])
growthnbhdata <- growthnbhdata %>% mutate(std_inter_nci = scale(log_p1_inter_nci, center = TRUE, scale = TRUE)[,1])

#Scaling initial DBH / preceding dbh
#Happy that it is normally distributed!
growthnbhdata <- growthnbhdata %>% mutate(std_preceding_dbh = scale(preceding_dbh, center = TRUE, scale = TRUE)[,1])

#Scaling PC1 and PC2
growthnbhdata <- growthnbhdata %>% mutate(std_PC1 = scale(PC1, center = TRUE, scale = TRUE)[,1])
growthnbhdata <- growthnbhdata %>% mutate(std_PC2 = scale(PC2, center = TRUE, scale = TRUE)[,1])

#### Add categorical information for whether sites are wet or dry
growthnbhdata$site_climate <- growthnbhdata$Site
growthnbhdata <- within(growthnbhdata, site_climate[Site == "EPF" | Site == "GRA" | Site == "FREY"] <- 'dry')
growthnbhdata <- within(growthnbhdata, site_climate[Site == "BOF" | Site == "TMP" | Site == "DOG" | Site == "MER"] <- 'wet')

#Note that this onerowdata isn't quite right - not equal # of growth values for period 1 and 2
onerowdata <- growthnbhdata %>% filter(Period==2)

#Splitting data by species
amygdata <- growthnbhdata %>% filter(Focal_sp == 'AMYG')
oblidata <- growthnbhdata %>% filter(Focal_sp == 'OBLI')
ovatdata <- growthnbhdata %>% filter(Focal_sp == 'OVAT')
vimidata <- growthnbhdata %>% filter(Focal_sp == 'VIMI')

specieslist <- list(amygdata, oblidata, ovatdata, vimidata)
speciesnamelist <- c("E. amygdalina", "E. obliqua", "E. ovata", "E. viminalis")
speciesabbrevlist <- c("AMYG", "OBLI", "OVAT", "VIMI")

## Save dataframe as csv
#write_csv(growthnbhdata, "Output/growth_data_excluding_climate.csv")

