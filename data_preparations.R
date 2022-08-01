### Eucalyptus Growth Tasmania Project 
# Alexandra Catling PhD Research
# Started 26/02/2021

### Importing packages and functions ####
library(tidyverse)
library(ggplot2)
library(lme4)
library(MuMIn)
library(lmerTest)
library(emmeans)
library(DHARMa)
library(car)

#Read in functions
source("functions.R")

my_theme <- theme(axis.title.x = element_text(size = 16),
                  axis.title.y = element_text(size = 16),
                  axis.text = element_text(size = 16),
                  strip.text.x = element_text(size = 16),
                  legend.text = element_text(size = 14),
                  legend.title = element_blank())

#### Importing and tidying data ####
## Importing growth data
growthdataraw <-read_csv("Data/growth_data.csv")

## Importing initial DBH data
initialdbhdataraw <- read_csv("Data/initial_field_dbh_data.csv")
# Removing CII column - crown index that I never measured
initialdbhdata <- initialdbhdataraw %>% select(-CII)
#Merging DBH data with growth data
growthdata <- left_join(growthdataraw, initialdbhdata, by = c("Site", "Focal_sp", "Plot", "Tree"))

## Importing site characteristics - climate data and site names
sitechar <- read_csv("Data/tas_site_rainfall.csv")
#Merging with growth data
growthdata <- left_join(growthdata, sitechar)
#Note that DBH_cm is not numeric because of two NAs (can be dropped) and two UNs (unknown DBH but have growth data)

#Removing seven trees that I don't have growth data for at all (no GPS coords or removed)
trees_to_remove <- read_csv("Data/trees_to_remove.csv")
growthdata <- anti_join(growthdata, trees_to_remove, by = c("Site", "Focal_sp", "Plot", "Tree"))

## Want to calculate growth as a rate, as different number of days between notching and growth measurements
datesdataraw <- read_csv("Data/dates_growth_and_bom_sites.csv")
#Making period a factor
growthdata$Period <- as.factor(growthdata$Period)
datesdataraw$Period <- as.factor(datesdataraw$Period)
datesdata <- datesdataraw %>% select(Site, Period, Days_growth_since_notching)
#Merging with growth data
growthdata <- left_join(growthdata, datesdata, by = c("Site", "Period"))

#Different units for growth at the moment. 2021 in mm and 2022 in cm. Converting 2022 growth cm to mm.
# Multiplying growth for period 2 by 10 so that it is in mm as well.
# Note that this is not true growth but instead the distance of dendrometer from time '0'
#so true growth for period 2 is 'growth' minus growth in period 1
growthdata$Growth[growthdata$Period==2] <- growthdata$Growth[growthdata$Period==2]*10

#Creating a column for true growth
#Period 1 = growth
# Period 2 = growth period 2 - growth period 1
growthdata$true_growth <- growthdata$Growth
growthdata$true_growth[growthdata$Period==2] <- growthdata$Growth[growthdata$Period==2]-growthdata$Growth[growthdata$Period==1]

#Creating a column for days of growth in the period
#since number_days_growth refers to total/relative to time '0'
growthdata$days_in_period <- growthdata$Days_growth_since_notching
growthdata$days_in_period[growthdata$Period==2] <- growthdata$Days_growth_since_notching[growthdata$Period==2]-growthdata$Days_growth_since_notching[growthdata$Period==1]

#Removing these period 1 values that were notched in period 1 so only have period 2 values:
# GRA OBLI A 10, DOG AMYG B8, EPF AMYG B6
growthdata <- growthdata %>% filter(!((Period == "1" & Site == "GRA" & Focal_sp == "OBLI" & Plot == "A" & Tree == "10")))
growthdata <- growthdata %>% filter(!((Period == "1" & Site == "DOG" & Focal_sp == "AMYG" & Plot == "B" & Tree == "8")))
growthdata <- growthdata %>% filter(!((Period == "1" & Site == "EPF" & Focal_sp == "AMYG" & Plot == "B" & Tree == "6")))
#Need to adjust their period 2 values which are currently NAs, setting true growth value to growth value
# had to remove them after calculations otherwise unequal number for period 2 - period 1
growthdata <- within(growthdata, true_growth[Site == "GRA" & Focal_sp == "OBLI" & Plot == "A" & Tree == "10"] <- growthdata$Growth[Site == "GRA" & Focal_sp == "OBLI" & Plot == "A" & Tree == "10"])
growthdata <- within(growthdata, true_growth[Site == "DOG" & Focal_sp == "AMYG" & Plot == "B" & Tree == "8"] <- growthdata$Growth[Site == "DOG" & Focal_sp == "AMYG" & Plot == "B" & Tree == "8"])
growthdata <- within(growthdata, true_growth[Site == "EPF" & Focal_sp == "AMYG" & Plot == "B" & Tree == "6"] <- growthdata$Growth[Site == "EPF" & Focal_sp == "AMYG" & Plot == "B" & Tree == "6"])

## Changing negative growth to zero
growthdata <- growthdata %>% mutate(growth_no_negs = true_growth)
growthdata <- within(growthdata, growth_no_negs[growth_no_negs<0] <- '0')

#Calculate growth rate (mm/day)
growthdata$growth_no_negs <- as.numeric(growthdata$growth_no_negs)
growthdata <- growthdata %>% mutate(growth_rate = growth_no_negs/days_in_period)

#Making initial DBH (DBH_cm) numeric (inputted with some unknown characters)
#do this: remove unknown DBH values
growthdata$DBH_cm <- as.numeric(growthdata$DBH_cm)

#### Calculate period rainfall ####
# Using BOM site daily rainfall information
bom_epf <- read_csv("Data/EPF_all.csv")
bom_tmp <- read_csv("Data/TMP_all.csv")
bom_mer <- read_csv("Data/MER_all.csv")
bom_dog <- read_csv("Data/DOG_all.csv")
bom_gra <- read_csv("Data/GRA_all.csv")
bom_frey <- read_csv("Data/FREY_all.csv")
bom_bof <- read_csv("Data/BOF_all.csv")

#Calculate number of days between Approx_date_band_notched and Date_growth_measured for Period 1
# and for period 2: Date_growth_measured[Period=1] and Date_growth_measured[Period=2]
#from datesdataraw dataset

#Creating dataset to put period data into
period_rainfall_data <- datesdata %>% select(Site, Period) %>% mutate(period_rainfall = 1)

## To do - check NAs for all of them
## EPF 
# period 1: 06/02/2020 - 14/02/2021 period 2: 15/02/2021 - 16/02/2022
#Merge date info into one column called date
bom_epf$date <- as.Date(with(bom_epf, paste(Year, Month, Day,sep="-")), "%Y-%m-%d")
#Calculate rainfall for period 1
epf_rainfall_period1 <- bom_epf %>% subset(date >= "2020-02-06" & date <= "2021-02-14") %>%
  summarise(period1_rainfall = sum(`Rainfall amount (millimetres)`))
#Calculate rainfall for period 2
epf_rainfall_period2 <- bom_epf %>% subset(date >= "2021-02-15" & date <= "2022-02-16") %>%
  summarise(period2_rainfall = sum(`Rainfall amount (millimetres)`))
#Filling data in table
period_rainfall_data <- within(period_rainfall_data, period_rainfall[Period == '1' & Site == 'EPF'] <- epf_rainfall_period1)
period_rainfall_data <- within(period_rainfall_data, period_rainfall[Period == '2' & Site == 'EPF'] <- epf_rainfall_period2)

##TMP
# period 1: 29/01/2020 - 15/02/2021 period 2: 16/02/2021 - 13/02/2022
#Lots of NAs for period 2 - was it out of order? Backburning? Check this *
bom_tmp$date <- as.Date(with(bom_tmp, paste(Year, Month, Day,sep="-")), "%Y-%m-%d")
tmp_rainfall_period1 <- bom_tmp %>% subset(date >= "2020-01-29" & date <= "2021-02-15") %>%
  summarise(period1_rainfall = sum(`Rainfall amount (millimetres)`))
tmp_rainfall_period2 <- bom_tmp %>% subset(date >= "2021-02-16" & date <= "2022-02-13") %>%
  summarise(period2_rainfall = sum(`Rainfall amount (millimetres)`, na.rm=TRUE))
test <- bom_tmp %>% subset(date >= "2021-02-16" & date <= "2022-02-13")
period_rainfall_data <- within(period_rainfall_data, period_rainfall[Period == '1' & Site == 'TMP'] <- tmp_rainfall_period1)
period_rainfall_data <- within(period_rainfall_data, period_rainfall[Period == '2' & Site == 'TMP'] <- tmp_rainfall_period2)

##MER
# period 1: 31/01/2020 - 13/02/2021 period 2: 14/02/2021 - 18/02/2022
bom_mer$date <- as.Date(with(bom_mer, paste(Year, Month, Day,sep="-")), "%Y-%m-%d")
mer_rainfall_period1 <- bom_mer %>% subset(date >= "2020-01-31" & date <= "2021-02-13") %>%
  summarise(period1_rainfall = sum(`Rainfall amount (millimetres)`, na.rm=TRUE))
mer_rainfall_period2 <- bom_mer %>% subset(date >= "2021-02-14" & date <= "2022-02-18") %>%
  summarise(period2_rainfall = sum(`Rainfall amount (millimetres)`, na.rm=TRUE))
period_rainfall_data <- within(period_rainfall_data, period_rainfall[Period == '1' & Site == 'MER'] <- mer_rainfall_period1)
period_rainfall_data <- within(period_rainfall_data, period_rainfall[Period == '2' & Site == 'MER'] <- mer_rainfall_period2)

##DOG
# period 1: 30/01/2020 - 13/02/2021 period 2: 14/02/2021 - 18/02/2022
bom_dog$date <- as.Date(with(bom_dog, paste(Year, Month, Day,sep="-")), "%Y-%m-%d")
dog_rainfall_period1 <- bom_dog %>% subset(date >= "2020-01-30" & date <= "2021-02-13") %>%
  summarise(period1_rainfall = sum(`Rainfall amount (millimetres)`))
dog_rainfall_period2 <- bom_dog %>% subset(date >= "2021-02-14" & date <= "2022-02-18") %>%
  summarise(period2_rainfall = sum(`Rainfall amount (millimetres)`))
period_rainfall_data <- within(period_rainfall_data, period_rainfall[Period == '1' & Site == 'DOG'] <- dog_rainfall_period1)
period_rainfall_data <- within(period_rainfall_data, period_rainfall[Period == '2' & Site == 'DOG'] <- dog_rainfall_period2)

##GRA
# period 1: 07/02/2020 - 09/02/2021 period 2: 10/02/2021 - 13/02/2022
bom_gra$date <- as.Date(with(bom_gra, paste(Year, Month, Day,sep="-")), "%Y-%m-%d")
gra_rainfall_period1 <- bom_gra %>% subset(date >= "2020-02-07" & date <= "2021-02-09") %>%
  summarise(period1_rainfall = sum(`Rainfall amount (millimetres)`))
gra_rainfall_period2 <- bom_gra %>% subset(date >= "2021-02-10" & date <= "2022-02-13") %>%
  summarise(period2_rainfall = sum(`Rainfall amount (millimetres)`))
period_rainfall_data <- within(period_rainfall_data, period_rainfall[Period == '1' & Site == 'GRA'] <- gra_rainfall_period1)
period_rainfall_data <- within(period_rainfall_data, period_rainfall[Period == '2' & Site == 'GRA'] <- gra_rainfall_period2)

##FREY
# period 1: 05/02/2020 - 16/02/2021 period 2: 17/02/2021 - 23/02/2022
bom_frey$date <- as.Date(with(bom_frey, paste(Year, Month, Day,sep="-")), "%Y-%m-%d")
frey_rainfall_period1 <- bom_frey %>% subset(date >= "2020-02-05" & date <= "2021-02-16") %>%
  summarise(period1_rainfall = sum(`Rainfall amount (millimetres)`))
frey_rainfall_period2 <- bom_frey %>% subset(date >= "2021-02-17" & date <= "2022-02-23") %>%
  summarise(period2_rainfall = sum(`Rainfall amount (millimetres)`))
period_rainfall_data <- within(period_rainfall_data, period_rainfall[Period == '1' & Site == 'FREY'] <- frey_rainfall_period1)
period_rainfall_data <- within(period_rainfall_data, period_rainfall[Period == '2' & Site == 'FREY'] <- frey_rainfall_period2)

##BOF
# period 1: 05/06/2020 - 11/02/2021 period 2: 12/02/2021 - 24/02/2022
bom_bof$date <- as.Date(with(bom_bof, paste(Year, Month, Day,sep="-")), "%Y-%m-%d")
bof_rainfall_period1 <- bom_bof %>% subset(date >= "2020-01-29" & date <= "2021-02-15") %>%
  summarise(period1_rainfall = sum(`Rainfall amount (millimetres)`, na.rm = TRUE))
bof_rainfall_period2 <- bom_bof %>% subset(date >= "2021-02-16" & date <= "2022-02-13") %>%
  summarise(period2_rainfall = sum(`Rainfall amount (millimetres)`, na.rm = TRUE))
period_rainfall_data <- within(period_rainfall_data, period_rainfall[Period == '1' & Site == 'BOF'] <- bof_rainfall_period1)
period_rainfall_data <- within(period_rainfall_data, period_rainfall[Period == '2' & Site == 'BOF'] <- bof_rainfall_period2)

### Do this * Need to relativise these observed values with the expected rainfall amounts
#based on long-term monthly averages

#### Import period BOM evapotranspiration data calculated from as close by as possible
#Creating table to put evapotranspiration data into
period_evapo_data <- datesdata %>% select(Site, Period) %>% mutate(period_evapo = 1)

#BOF
bof_202006 <- read_csv("Data/BOM_evapotranspiration/BOF/st_helens_aerodrome-202006.csv")
bof_202007 <- read_csv("Data/BOM_evapotranspiration/BOF/st_helens_aerodrome-202007.csv")
bof_202008 <- read_csv("Data/BOM_evapotranspiration/BOF/st_helens_aerodrome-202008.csv")
bof_202009 <- read_csv("Data/BOM_evapotranspiration/BOF/st_helens_aerodrome-202009.csv")
bof_202010 <- read_csv("Data/BOM_evapotranspiration/BOF/st_helens_aerodrome-202010.csv")
bof_202011 <- read_csv("Data/BOM_evapotranspiration/BOF/st_helens_aerodrome-202011.csv")
bof_202012 <- read_csv("Data/BOM_evapotranspiration/BOF/st_helens_aerodrome-202012.csv")
bof_202101 <- read_csv("Data/BOM_evapotranspiration/BOF/st_helens_aerodrome-202101.csv")
bof_202102 <- read_csv("Data/BOM_evapotranspiration/BOF/st_helens_aerodrome-202102.csv")
bof_202103 <- read_csv("Data/BOM_evapotranspiration/BOF/st_helens_aerodrome-202103.csv")
bof_202104 <- read_csv("Data/BOM_evapotranspiration/BOF/st_helens_aerodrome-202104.csv")
bof_202105 <- read_csv("Data/BOM_evapotranspiration/BOF/st_helens_aerodrome-202105.csv")
bof_202106 <- read_csv("Data/BOM_evapotranspiration/BOF/st_helens_aerodrome-202106.csv")
bof_202107 <- read_csv("Data/BOM_evapotranspiration/BOF/st_helens_aerodrome-202107.csv")
bof_202108 <- read_csv("Data/BOM_evapotranspiration/BOF/st_helens_aerodrome-202108.csv")
bof_202109 <- read_csv("Data/BOM_evapotranspiration/BOF/st_helens_aerodrome-202109.csv")
bof_202110 <- read_csv("Data/BOM_evapotranspiration/BOF/st_helens_aerodrome-202110.csv")
bof_202111 <- read_csv("Data/BOM_evapotranspiration/BOF/st_helens_aerodrome-202111.csv")
bof_202112 <- read_csv("Data/BOM_evapotranspiration/BOF/st_helens_aerodrome-202112.csv")
bof_202201 <- read_csv("Data/BOM_evapotranspiration/BOF/st_helens_aerodrome-202201.csv")
bof_202202 <- read_csv("Data/BOM_evapotranspiration/BOF/st_helens_aerodrome-202202.csv")

# period 1: 05/06/2020 - 11/02/2021 period 2: 12/02/2021 - 24/02/2022
#Populate table with sum of values for period 1
period_evapo_data <- within(period_evapo_data, period_evapo[Site == 'BOF' & Period == '1'] <- sum(as.numeric(bof_202006[41,3]) + as.numeric(bof_202007[42,3]) + as.numeric(bof_202008[42,3]) + as.numeric(bof_202009[41,3]) + as.numeric(bof_202010[42,3]) + as.numeric(bof_202011[41,3]) + as.numeric(bof_202012[42,3])+ as.numeric(bof_202101[42,3])+ as.numeric(bof_202102[39,3])))
# And for period 2
## Trying it another way: merging all data into one dataframe with dates and daily evapotranspiration data
bof_202006_edit <- bof_202006[11:40, 1:3]
bof_202007_edit <- bof_202007[11:41, 1:3]
bof_202008_edit <- bof_202008[11:41, 1:3]
bof_202009_edit <- bof_202009[11:40, 1:3]
bof_202010_edit <- bof_202010[11:41, 1:3]
bof_202011_edit <- bof_202011[11:40, 1:3]
bof_202012_edit <- bof_202012[11:41, 1:3]
bof_202101_edit <- bof_202101[11:41, 1:3]
bof_202102_edit <- bof_202102[11:38, 1:3]
bof_202103_edit <- bof_202103[11:41, 1:3]
bof_202104_edit <- bof_202104[11:40, 1:3]
bof_202105_edit <- bof_202105[11:41, 1:3]
bof_202106_edit <- bof_202106[11:40, 1:3]
bof_202107_edit <- bof_202107[11:41, 1:3]
bof_202108_edit <- bof_202108[11:41, 1:3]
bof_202109_edit <- bof_202109[11:40, 1:3]
bof_202110_edit <- bof_202110[11:41, 1:3]
bof_202111_edit <- bof_202111[11:40, 1:3]
bof_202112_edit <- bof_202112[11:41, 1:3]
bof_202201_edit <- bof_202201[11:41, 1:3]
bof_202202_edit <- bof_202202[11:38, 1:3]

#Bind all months together
bof_evapo <- bind_rows(bof_202006_edit, bof_202007_edit, bof_202008_edit, bof_202009_edit, bof_202010_edit, bof_202011_edit, bof_202012_edit, bof_202101_edit, bof_202102_edit, bof_202103_edit, bof_202104_edit, bof_202105_edit, bof_202106_edit, bof_202107_edit, bof_202108_edit, bof_202109_edit, bof_202110_edit, bof_202111_edit, bof_202112_edit, bof_202201_edit, bof_202202_edit)
names(bof_evapo) <- c("BOM_site", "date", "evapotranspiration_mm")
#Change to Date format
bof_evapo$date <- as.Date(bof_evapo$date, "%d/%m/%Y")
#Change evapo to numeric
bof_evapo$evapotranspiration_mm <- as.numeric(bof_evapo$evapotranspiration_mm)

#Calculate evapotranspiration for period 1 and period 2
bof_evapo_period1 <- bof_evapo %>% subset(date >= "2020-01-29" & date <= "2021-02-15") %>%
  summarise(period1_evapo = sum(evapotranspiration_mm, na.rm = TRUE))
bof_evapo_period2 <- bof_evapo %>% subset(date >= "2021-02-16" & date <= "2022-02-13") %>%
  summarise(period2_evapo = sum(evapotranspiration_mm, na.rm = TRUE))
period_evapo_data <- within(period_evapo_data, period_evapo[Period == '1' & Site == 'BOF'] <- bof_evapo_period1)
period_evapo_data <- within(period_evapo_data, period_evapo[Period == '2' & Site == 'BOF'] <- bof_evapo_period2)

#FREY
# period 1: 05/02/2020 - 16/02/2021 period 2: 17/02/2021 - 23/02/2022
frey_202002 <- read_csv("Data/BOM_evapotranspiration/frey/friendly_beaches-202002.csv")
frey_202003 <- read_csv("Data/BOM_evapotranspiration/frey/friendly_beaches-202003.csv")
frey_202004 <- read_csv("Data/BOM_evapotranspiration/frey/friendly_beaches-202004.csv")
frey_202005 <- read_csv("Data/BOM_evapotranspiration/frey/friendly_beaches-202005.csv")
frey_202006 <- read_csv("Data/BOM_evapotranspiration/frey/friendly_beaches-202006.csv")
frey_202007 <- read_csv("Data/BOM_evapotranspiration/frey/friendly_beaches-202007.csv")
frey_202008 <- read_csv("Data/BOM_evapotranspiration/frey/friendly_beaches-202008.csv")
frey_202009 <- read_csv("Data/BOM_evapotranspiration/frey/friendly_beaches-202009.csv")
frey_202010 <- read_csv("Data/BOM_evapotranspiration/frey/friendly_beaches-202010.csv")
frey_202011 <- read_csv("Data/BOM_evapotranspiration/frey/friendly_beaches-202011.csv")
frey_202012 <- read_csv("Data/BOM_evapotranspiration/frey/friendly_beaches-202012.csv")
frey_202101 <- read_csv("Data/BOM_evapotranspiration/frey/friendly_beaches-202101.csv")
frey_202102 <- read_csv("Data/BOM_evapotranspiration/frey/friendly_beaches-202102.csv")
frey_202103 <- read_csv("Data/BOM_evapotranspiration/frey/friendly_beaches-202103.csv")
frey_202104 <- read_csv("Data/BOM_evapotranspiration/frey/friendly_beaches-202104.csv")
frey_202105 <- read_csv("Data/BOM_evapotranspiration/frey/friendly_beaches-202105.csv")
frey_202106 <- read_csv("Data/BOM_evapotranspiration/frey/friendly_beaches-202106.csv")
frey_202107 <- read_csv("Data/BOM_evapotranspiration/frey/friendly_beaches-202107.csv")
frey_202108 <- read_csv("Data/BOM_evapotranspiration/frey/friendly_beaches-202108.csv")
frey_202109 <- read_csv("Data/BOM_evapotranspiration/frey/friendly_beaches-202109.csv")
frey_202110 <- read_csv("Data/BOM_evapotranspiration/frey/friendly_beaches-202110.csv")
frey_202111 <- read_csv("Data/BOM_evapotranspiration/frey/friendly_beaches-202111.csv")
frey_202112 <- read_csv("Data/BOM_evapotranspiration/frey/friendly_beaches-202112.csv")
frey_202201 <- read_csv("Data/BOM_evapotranspiration/frey/friendly_beaches-202201.csv")
frey_202202 <- read_csv("Data/BOM_evapotranspiration/frey/friendly_beaches-202202.csv")

frey_202002_edit <- frey_202002[11:39, 1:3]
frey_202003_edit <- frey_202003[11:41, 1:3]
frey_202004_edit <- frey_202004[11:40, 1:3]
frey_202005_edit <- frey_202005[11:41, 1:3]
frey_202006_edit <- frey_202006[11:40, 1:3]
frey_202007_edit <- frey_202007[11:41, 1:3]
frey_202008_edit <- frey_202008[11:41, 1:3]
frey_202009_edit <- frey_202009[11:40, 1:3]
frey_202010_edit <- frey_202010[11:41, 1:3]
frey_202011_edit <- frey_202011[11:40, 1:3]
frey_202012_edit <- frey_202012[11:41, 1:3]
frey_202101_edit <- frey_202101[11:41, 1:3]
frey_202102_edit <- frey_202102[11:38, 1:3]
frey_202103_edit <- frey_202103[11:41, 1:3]
frey_202104_edit <- frey_202104[11:40, 1:3]
frey_202105_edit <- frey_202105[11:41, 1:3]
frey_202106_edit <- frey_202106[11:40, 1:3]
frey_202107_edit <- frey_202107[11:41, 1:3]
frey_202108_edit <- frey_202108[11:41, 1:3]
frey_202109_edit <- frey_202109[11:40, 1:3]
frey_202110_edit <- frey_202110[11:41, 1:3]
frey_202111_edit <- frey_202111[11:40, 1:3]
frey_202112_edit <- frey_202112[11:41, 1:3]
frey_202201_edit <- frey_202201[11:41, 1:3]
frey_202202_edit <- frey_202202[11:38, 1:3]

frey_evapo <- bind_rows(frey_202002_edit, frey_202003_edit, frey_202004_edit, frey_202005_edit, frey_202006_edit, frey_202007_edit, frey_202008_edit, frey_202009_edit, frey_202010_edit, frey_202011_edit, frey_202012_edit, frey_202101_edit, frey_202102_edit, frey_202103_edit, frey_202104_edit, frey_202105_edit, frey_202106_edit, frey_202107_edit, frey_202108_edit, frey_202109_edit, frey_202110_edit, frey_202111_edit, frey_202112_edit, frey_202201_edit, frey_202202_edit)
names(frey_evapo) <- c("BOM_site", "date", "evapotranspiration_mm")
frey_evapo$date <- as.Date(frey_evapo$date, "%d/%m/%Y")
frey_evapo$evapotranspiration_mm <- as.numeric(frey_evapo$evapotranspiration_mm)

frey_evapo_period1 <- frey_evapo %>% subset(date >= "2020-02-05" & date <= "2021-02-16") %>%
  summarise(period1_evapo = sum(evapotranspiration_mm, na.rm = TRUE))
frey_evapo_period2 <- frey_evapo %>% subset(date >= "2021-02-17" & date <= "2022-02-23") %>%
  summarise(period2_evapo = sum(evapotranspiration_mm, na.rm = TRUE))
period_evapo_data <- within(period_evapo_data, period_evapo[Period == '1' & Site == 'FREY'] <- frey_evapo_period1)
period_evapo_data <- within(period_evapo_data, period_evapo[Period == '2' & Site == 'FREY'] <- frey_evapo_period2)

##GRA
# period 1: 07/02/2020 - 09/02/2021 period 2: 10/02/2021 - 13/02/2022
gra_202002 <- read_csv("Data/BOM_evapotranspiration/gra/campania_(kincora)-202002.csv")
gra_202003 <- read_csv("Data/BOM_evapotranspiration/gra/campania_(kincora)-202003.csv")
gra_202004 <- read_csv("Data/BOM_evapotranspiration/gra/campania_(kincora)-202004.csv")
gra_202005 <- read_csv("Data/BOM_evapotranspiration/gra/campania_(kincora)-202005.csv")
gra_202006 <- read_csv("Data/BOM_evapotranspiration/gra/campania_(kincora)-202006.csv")
gra_202007 <- read_csv("Data/BOM_evapotranspiration/gra/campania_(kincora)-202007.csv")
gra_202008 <- read_csv("Data/BOM_evapotranspiration/gra/campania_(kincora)-202008.csv")
gra_202009 <- read_csv("Data/BOM_evapotranspiration/gra/campania_(kincora)-202009.csv")
gra_202010 <- read_csv("Data/BOM_evapotranspiration/gra/campania_(kincora)-202010.csv")
gra_202011 <- read_csv("Data/BOM_evapotranspiration/gra/campania_(kincora)-202011.csv")
gra_202012 <- read_csv("Data/BOM_evapotranspiration/gra/campania_(kincora)-202012.csv")
gra_202101 <- read_csv("Data/BOM_evapotranspiration/gra/campania_(kincora)-202101.csv")
gra_202102 <- read_csv("Data/BOM_evapotranspiration/gra/campania_(kincora)-202102.csv")
gra_202103 <- read_csv("Data/BOM_evapotranspiration/gra/campania_(kincora)-202103.csv")
gra_202104 <- read_csv("Data/BOM_evapotranspiration/gra/campania_(kincora)-202104.csv")
gra_202105 <- read_csv("Data/BOM_evapotranspiration/gra/campania_(kincora)-202105.csv")
gra_202106 <- read_csv("Data/BOM_evapotranspiration/gra/campania_(kincora)-202106.csv")
gra_202107 <- read_csv("Data/BOM_evapotranspiration/gra/campania_(kincora)-202107.csv")
gra_202108 <- read_csv("Data/BOM_evapotranspiration/gra/campania_(kincora)-202108.csv")
gra_202109 <- read_csv("Data/BOM_evapotranspiration/gra/campania_(kincora)-202109.csv")
gra_202110 <- read_csv("Data/BOM_evapotranspiration/gra/campania_(kincora)-202110.csv")
gra_202111 <- read_csv("Data/BOM_evapotranspiration/gra/campania_(kincora)-202111.csv")
gra_202112 <- read_csv("Data/BOM_evapotranspiration/gra/campania_(kincora)-202112.csv")
gra_202201 <- read_csv("Data/BOM_evapotranspiration/gra/campania_(kincora)-202201.csv")
gra_202202 <- read_csv("Data/BOM_evapotranspiration/gra/campania_(kincora)-202202.csv")

gra_202002_edit <- gra_202002[11:39, 1:3]
gra_202003_edit <- gra_202003[11:41, 1:3]
gra_202004_edit <- gra_202004[11:40, 1:3]
gra_202005_edit <- gra_202005[11:41, 1:3]
gra_202006_edit <- gra_202006[11:40, 1:3]
gra_202007_edit <- gra_202007[11:41, 1:3]
gra_202008_edit <- gra_202008[11:41, 1:3]
gra_202009_edit <- gra_202009[11:40, 1:3]
gra_202010_edit <- gra_202010[11:41, 1:3]
gra_202011_edit <- gra_202011[11:40, 1:3]
gra_202012_edit <- gra_202012[11:41, 1:3]
gra_202101_edit <- gra_202101[11:41, 1:3]
gra_202102_edit <- gra_202102[11:38, 1:3]
gra_202103_edit <- gra_202103[11:41, 1:3]
gra_202104_edit <- gra_202104[11:40, 1:3]
gra_202105_edit <- gra_202105[11:41, 1:3]
gra_202106_edit <- gra_202106[11:40, 1:3]
gra_202107_edit <- gra_202107[11:41, 1:3]
gra_202108_edit <- gra_202108[11:41, 1:3]
gra_202109_edit <- gra_202109[11:40, 1:3]
gra_202110_edit <- gra_202110[11:41, 1:3]
gra_202111_edit <- gra_202111[11:40, 1:3]
gra_202112_edit <- gra_202112[11:41, 1:3]
gra_202201_edit <- gra_202201[11:41, 1:3]
gra_202202_edit <- gra_202202[11:38, 1:3]

gra_evapo <- bind_rows(gra_202002_edit, gra_202003_edit, gra_202004_edit, gra_202005_edit, gra_202006_edit, gra_202007_edit, gra_202008_edit, gra_202009_edit, gra_202010_edit, gra_202011_edit, gra_202012_edit, gra_202101_edit, gra_202102_edit, gra_202103_edit, gra_202104_edit, gra_202105_edit, gra_202106_edit, gra_202107_edit, gra_202108_edit, gra_202109_edit, gra_202110_edit, gra_202111_edit, gra_202112_edit, gra_202201_edit, gra_202202_edit)
names(gra_evapo) <- c("BOM_site", "date", "evapotranspiration_mm")
gra_evapo$date <- as.Date(gra_evapo$date, "%d/%m/%Y")
gra_evapo$evapotranspiration_mm <- as.numeric(gra_evapo$evapotranspiration_mm)

gra_evapo_period1 <- gra_evapo %>% subset(date >= "2020-02-07" & date <= "2021-02-09") %>%
  summarise(period1_evapo = sum(evapotranspiration_mm, na.rm = TRUE))
gra_evapo_period2 <- gra_evapo %>% subset(date >= "2021-02-10" & date <= "2022-02-13") %>%
  summarise(period2_evapo = sum(evapotranspiration_mm, na.rm = TRUE))
period_evapo_data <- within(period_evapo_data, period_evapo[Period == '1' & Site == 'GRA'] <- gra_evapo_period1)
period_evapo_data <- within(period_evapo_data, period_evapo[Period == '2' & Site == 'GRA'] <- gra_evapo_period2)

##DOG
# period 1: 30/01/2020 - 13/02/2021 period 2: 14/02/2021 - 18/02/2022
dog_202001 <- read_csv("Data/BOM_evapotranspiration/dog/sheffield_school_farm-202002.csv")
dog_202002 <- read_csv("Data/BOM_evapotranspiration/dog/sheffield_school_farm-202002.csv")
dog_202003 <- read_csv("Data/BOM_evapotranspiration/dog/sheffield_school_farm-202003.csv")
dog_202004 <- read_csv("Data/BOM_evapotranspiration/dog/sheffield_school_farm-202004.csv")
dog_202005 <- read_csv("Data/BOM_evapotranspiration/dog/sheffield_school_farm-202005.csv")
dog_202006 <- read_csv("Data/BOM_evapotranspiration/dog/sheffield_school_farm-202006.csv")
dog_202007 <- read_csv("Data/BOM_evapotranspiration/dog/sheffield_school_farm-202007.csv")
dog_202008 <- read_csv("Data/BOM_evapotranspiration/dog/sheffield_school_farm-202008.csv")
dog_202009 <- read_csv("Data/BOM_evapotranspiration/dog/sheffield_school_farm-202009.csv")
dog_202010 <- read_csv("Data/BOM_evapotranspiration/dog/sheffield_school_farm-202010.csv")
dog_202011 <- read_csv("Data/BOM_evapotranspiration/dog/sheffield_school_farm-202011.csv")
dog_202012 <- read_csv("Data/BOM_evapotranspiration/dog/sheffield_school_farm-202012.csv")
dog_202101 <- read_csv("Data/BOM_evapotranspiration/dog/sheffield_school_farm-202101.csv")
dog_202102 <- read_csv("Data/BOM_evapotranspiration/dog/sheffield_school_farm-202102.csv")
dog_202103 <- read_csv("Data/BOM_evapotranspiration/dog/sheffield_school_farm-202103.csv")
dog_202104 <- read_csv("Data/BOM_evapotranspiration/dog/sheffield_school_farm-202104.csv")
dog_202105 <- read_csv("Data/BOM_evapotranspiration/dog/sheffield_school_farm-202105.csv")
dog_202106 <- read_csv("Data/BOM_evapotranspiration/dog/sheffield_school_farm-202106.csv")
dog_202107 <- read_csv("Data/BOM_evapotranspiration/dog/sheffield_school_farm-202107.csv")
dog_202108 <- read_csv("Data/BOM_evapotranspiration/dog/sheffield_school_farm-202108.csv")
dog_202109 <- read_csv("Data/BOM_evapotranspiration/dog/sheffield_school_farm-202109.csv")
dog_202110 <- read_csv("Data/BOM_evapotranspiration/dog/sheffield_school_farm-202110.csv")
dog_202111 <- read_csv("Data/BOM_evapotranspiration/dog/sheffield_school_farm-202111.csv")
dog_202112 <- read_csv("Data/BOM_evapotranspiration/dog/sheffield_school_farm-202112.csv")
dog_202201 <- read_csv("Data/BOM_evapotranspiration/dog/sheffield_school_farm-202201.csv")
dog_202202 <- read_csv("Data/BOM_evapotranspiration/dog/sheffield_school_farm-202202.csv")

dog_202001_edit <- dog_202001[11:41, 1:3]
dog_202002_edit <- dog_202002[11:39, 1:3]
dog_202003_edit <- dog_202003[11:41, 1:3]
dog_202004_edit <- dog_202004[11:40, 1:3]
dog_202005_edit <- dog_202005[11:41, 1:3]
dog_202006_edit <- dog_202006[11:40, 1:3]
dog_202007_edit <- dog_202007[11:41, 1:3]
dog_202008_edit <- dog_202008[11:41, 1:3]
dog_202009_edit <- dog_202009[11:40, 1:3]
dog_202010_edit <- dog_202010[11:41, 1:3]
dog_202011_edit <- dog_202011[11:40, 1:3]
dog_202012_edit <- dog_202012[11:41, 1:3]
dog_202101_edit <- dog_202101[11:41, 1:3]
dog_202102_edit <- dog_202102[11:38, 1:3]
dog_202103_edit <- dog_202103[11:41, 1:3]
dog_202104_edit <- dog_202104[11:40, 1:3]
dog_202105_edit <- dog_202105[11:41, 1:3]
dog_202106_edit <- dog_202106[11:40, 1:3]
dog_202107_edit <- dog_202107[11:41, 1:3]
dog_202108_edit <- dog_202108[11:41, 1:3]
dog_202109_edit <- dog_202109[11:40, 1:3]
dog_202110_edit <- dog_202110[11:41, 1:3]
dog_202111_edit <- dog_202111[11:40, 1:3]
dog_202112_edit <- dog_202112[11:41, 1:3]
dog_202201_edit <- dog_202201[11:41, 1:3]
dog_202202_edit <- dog_202202[11:38, 1:3]

dog_evapo <- bind_rows(dog_202001_edit, dog_202002_edit, dog_202003_edit, dog_202004_edit, dog_202005_edit, dog_202006_edit, dog_202007_edit, dog_202008_edit, dog_202009_edit, dog_202010_edit, dog_202011_edit, dog_202012_edit, dog_202101_edit, dog_202102_edit, dog_202103_edit, dog_202104_edit, dog_202105_edit, dog_202106_edit, dog_202107_edit, dog_202108_edit, dog_202109_edit, dog_202110_edit, dog_202111_edit, dog_202112_edit, dog_202201_edit, dog_202202_edit)
names(dog_evapo) <- c("BOM_site", "date", "evapotranspiration_mm")
dog_evapo$date <- as.Date(dog_evapo$date, "%d/%m/%Y")
dog_evapo$evapotranspiration_mm <- as.numeric(dog_evapo$evapotranspiration_mm)

dog_evapo_period1 <- dog_evapo %>% subset(date >= "2020-01-30" & date <= "2021-02-13") %>%
  summarise(period1_evapo = sum(evapotranspiration_mm, na.rm = TRUE))
dog_evapo_period2 <- dog_evapo %>% subset(date >= "2021-02-14" & date <= "2022-02-18") %>%
  summarise(period2_evapo = sum(evapotranspiration_mm, na.rm = TRUE))
period_evapo_data <- within(period_evapo_data, period_evapo[Period == '1' & Site == 'DOG'] <- dog_evapo_period1)
period_evapo_data <- within(period_evapo_data, period_evapo[Period == '2' & Site == 'DOG'] <- dog_evapo_period2)

##MER
#Uses same dataset as DOG for evapotranspiration
# period 1: 31/01/2020 - 13/02/2021 period 2: 14/02/2021 - 18/02/2022
mer_evapo_period1 <- dog_evapo %>% subset(date >= "2020-01-31" & date <= "2021-02-13") %>%
  summarise(period1_evapo = sum(evapotranspiration_mm, na.rm = TRUE))
mer_evapo_period2 <- dog_evapo %>% subset(date >= "2021-02-14" & date <= "2022-02-18") %>%
  summarise(period2_evapo = sum(evapotranspiration_mm, na.rm = TRUE))
period_evapo_data <- within(period_evapo_data, period_evapo[Period == '1' & Site == 'MER'] <- mer_evapo_period1)
period_evapo_data <- within(period_evapo_data, period_evapo[Period == '2' & Site == 'MER'] <- mer_evapo_period2)

## TMP
# period 1: 29/01/2020 - 15/02/2021 period 2: 16/02/2021 - 13/02/2022
tmp_202001 <- read_csv("Data/BOM_evapotranspiration/tmp/tasman_island-202002.csv")
tmp_202002 <- read_csv("Data/BOM_evapotranspiration/tmp/tasman_island-202002.csv")
tmp_202003 <- read_csv("Data/BOM_evapotranspiration/tmp/tasman_island-202003.csv")
tmp_202004 <- read_csv("Data/BOM_evapotranspiration/tmp/tasman_island-202004.csv")
tmp_202005 <- read_csv("Data/BOM_evapotranspiration/tmp/tasman_island-202005.csv")
tmp_202006 <- read_csv("Data/BOM_evapotranspiration/tmp/tasman_island-202006.csv")
tmp_202007 <- read_csv("Data/BOM_evapotranspiration/tmp/tasman_island-202007.csv")
tmp_202008 <- read_csv("Data/BOM_evapotranspiration/tmp/tasman_island-202008.csv")
tmp_202009 <- read_csv("Data/BOM_evapotranspiration/tmp/tasman_island-202009.csv")
tmp_202010 <- read_csv("Data/BOM_evapotranspiration/tmp/tasman_island-202010.csv")
tmp_202011 <- read_csv("Data/BOM_evapotranspiration/tmp/tasman_island-202011.csv")
tmp_202012 <- read_csv("Data/BOM_evapotranspiration/tmp/tasman_island-202012.csv")
tmp_202101 <- read_csv("Data/BOM_evapotranspiration/tmp/tasman_island-202101.csv")
tmp_202102 <- read_csv("Data/BOM_evapotranspiration/tmp/tasman_island-202102.csv")
tmp_202103 <- read_csv("Data/BOM_evapotranspiration/tmp/tasman_island-202103.csv")
tmp_202104 <- read_csv("Data/BOM_evapotranspiration/tmp/tasman_island-202104.csv")
tmp_202105 <- read_csv("Data/BOM_evapotranspiration/tmp/tasman_island-202105.csv")
tmp_202106 <- read_csv("Data/BOM_evapotranspiration/tmp/tasman_island-202106.csv")
tmp_202107 <- read_csv("Data/BOM_evapotranspiration/tmp/tasman_island-202107.csv")
tmp_202108 <- read_csv("Data/BOM_evapotranspiration/tmp/tasman_island-202108.csv")
tmp_202109 <- read_csv("Data/BOM_evapotranspiration/tmp/tasman_island-202109.csv")
tmp_202110 <- read_csv("Data/BOM_evapotranspiration/tmp/tasman_island-202110.csv")
tmp_202111 <- read_csv("Data/BOM_evapotranspiration/tmp/tasman_island-202111.csv")
tmp_202112 <- read_csv("Data/BOM_evapotranspiration/tmp/tasman_island-202112.csv")
tmp_202201 <- read_csv("Data/BOM_evapotranspiration/tmp/tasman_island-202201.csv")
tmp_202202 <- read_csv("Data/BOM_evapotranspiration/tmp/tasman_island-202202.csv")

tmp_202001_edit <- tmp_202001[11:41, 1:3]
tmp_202002_edit <- tmp_202002[11:39, 1:3]
tmp_202003_edit <- tmp_202003[11:41, 1:3]
tmp_202004_edit <- tmp_202004[11:40, 1:3]
tmp_202005_edit <- tmp_202005[11:41, 1:3]
tmp_202006_edit <- tmp_202006[11:40, 1:3]
tmp_202007_edit <- tmp_202007[11:41, 1:3]
tmp_202008_edit <- tmp_202008[11:41, 1:3]
tmp_202009_edit <- tmp_202009[11:40, 1:3]
tmp_202010_edit <- tmp_202010[11:41, 1:3]
tmp_202011_edit <- tmp_202011[11:40, 1:3]
tmp_202012_edit <- tmp_202012[11:41, 1:3]
tmp_202101_edit <- tmp_202101[11:41, 1:3]
tmp_202102_edit <- tmp_202102[11:38, 1:3]
tmp_202103_edit <- tmp_202103[11:41, 1:3]
tmp_202104_edit <- tmp_202104[11:40, 1:3]
tmp_202105_edit <- tmp_202105[11:41, 1:3]
tmp_202106_edit <- tmp_202106[11:40, 1:3]
tmp_202107_edit <- tmp_202107[11:41, 1:3]
tmp_202108_edit <- tmp_202108[11:41, 1:3]
tmp_202109_edit <- tmp_202109[11:40, 1:3]
tmp_202110_edit <- tmp_202110[11:41, 1:3]
tmp_202111_edit <- tmp_202111[11:40, 1:3]
tmp_202112_edit <- tmp_202112[11:41, 1:3]
tmp_202201_edit <- tmp_202201[11:41, 1:3]
tmp_202202_edit <- tmp_202202[11:38, 1:3]

tmp_evapo <- bind_rows(tmp_202001_edit, tmp_202002_edit, tmp_202003_edit, tmp_202004_edit, tmp_202005_edit, tmp_202006_edit, tmp_202007_edit, tmp_202008_edit, tmp_202009_edit, tmp_202010_edit, tmp_202011_edit, tmp_202012_edit, tmp_202101_edit, tmp_202102_edit, tmp_202103_edit, tmp_202104_edit, tmp_202105_edit, tmp_202106_edit, tmp_202107_edit, tmp_202108_edit, tmp_202109_edit, tmp_202110_edit, tmp_202111_edit, tmp_202112_edit, tmp_202201_edit, tmp_202202_edit)
names(tmp_evapo) <- c("BOM_site", "date", "evapotranspiration_mm")
tmp_evapo$date <- as.Date(tmp_evapo$date, "%d/%m/%Y")
tmp_evapo$evapotranspiration_mm <- as.numeric(tmp_evapo$evapotranspiration_mm)

tmp_evapo_period1 <- tmp_evapo %>% subset(date >= "2020-01-29" & date <= "2021-02-15") %>%
  summarise(period1_evapo = sum(evapotranspiration_mm, na.rm = TRUE))
tmp_evapo_period2 <- tmp_evapo %>% subset(date >= "2021-02-16" & date <= "2022-02-13") %>%
  summarise(period2_evapo = sum(evapotranspiration_mm, na.rm = TRUE))
period_evapo_data <- within(period_evapo_data, period_evapo[Period == '1' & Site == 'TMP'] <- tmp_evapo_period1)
period_evapo_data <- within(period_evapo_data, period_evapo[Period == '2' & Site == 'TMP'] <- tmp_evapo_period2)

## EPF
# period 1: 06/02/2020 - 14/02/2021 period 2: 15/02/2021 - 16/02/2022
epf_202002 <- read_csv("Data/BOM_evapotranspiration/epf/cressy_research_station-202002.csv")
epf_202003 <- read_csv("Data/BOM_evapotranspiration/epf/cressy_research_station-202003.csv")
epf_202004 <- read_csv("Data/BOM_evapotranspiration/epf/cressy_research_station-202004.csv")
epf_202005 <- read_csv("Data/BOM_evapotranspiration/epf/cressy_research_station-202005.csv")
epf_202006 <- read_csv("Data/BOM_evapotranspiration/epf/cressy_research_station-202006.csv")
epf_202007 <- read_csv("Data/BOM_evapotranspiration/epf/cressy_research_station-202007.csv")
epf_202008 <- read_csv("Data/BOM_evapotranspiration/epf/cressy_research_station-202008.csv")
epf_202009 <- read_csv("Data/BOM_evapotranspiration/epf/cressy_research_station-202009.csv")
epf_202010 <- read_csv("Data/BOM_evapotranspiration/epf/cressy_research_station-202010.csv")
epf_202011 <- read_csv("Data/BOM_evapotranspiration/epf/cressy_research_station-202011.csv")
epf_202012 <- read_csv("Data/BOM_evapotranspiration/epf/cressy_research_station-202012.csv")
epf_202101 <- read_csv("Data/BOM_evapotranspiration/epf/cressy_research_station-202101.csv")
epf_202102 <- read_csv("Data/BOM_evapotranspiration/epf/cressy_research_station-202102.csv")
epf_202103 <- read_csv("Data/BOM_evapotranspiration/epf/cressy_research_station-202103.csv")
epf_202104 <- read_csv("Data/BOM_evapotranspiration/epf/cressy_research_station-202104.csv")
epf_202105 <- read_csv("Data/BOM_evapotranspiration/epf/cressy_research_station-202105.csv")
epf_202106 <- read_csv("Data/BOM_evapotranspiration/epf/cressy_research_station-202106.csv")
epf_202107 <- read_csv("Data/BOM_evapotranspiration/epf/cressy_research_station-202107.csv")
epf_202108 <- read_csv("Data/BOM_evapotranspiration/epf/cressy_research_station-202108.csv")
epf_202109 <- read_csv("Data/BOM_evapotranspiration/epf/cressy_research_station-202109.csv")
epf_202110 <- read_csv("Data/BOM_evapotranspiration/epf/cressy_research_station-202110.csv")
epf_202111 <- read_csv("Data/BOM_evapotranspiration/epf/cressy_research_station-202111.csv")
epf_202112 <- read_csv("Data/BOM_evapotranspiration/epf/cressy_research_station-202112.csv")
epf_202201 <- read_csv("Data/BOM_evapotranspiration/epf/cressy_research_station-202201.csv")
epf_202202 <- read_csv("Data/BOM_evapotranspiration/epf/cressy_research_station-202202.csv")

epf_202002_edit <- epf_202002[11:39, 1:3]
epf_202003_edit <- epf_202003[11:41, 1:3]
epf_202004_edit <- epf_202004[11:40, 1:3]
epf_202005_edit <- epf_202005[11:41, 1:3]
epf_202006_edit <- epf_202006[11:40, 1:3]
epf_202007_edit <- epf_202007[11:41, 1:3]
epf_202008_edit <- epf_202008[11:41, 1:3]
epf_202009_edit <- epf_202009[11:40, 1:3]
epf_202010_edit <- epf_202010[11:41, 1:3]
epf_202011_edit <- epf_202011[11:40, 1:3]
epf_202012_edit <- epf_202012[11:41, 1:3]
epf_202101_edit <- epf_202101[11:41, 1:3]
epf_202102_edit <- epf_202102[11:38, 1:3]
epf_202103_edit <- epf_202103[11:41, 1:3]
epf_202104_edit <- epf_202104[11:40, 1:3]
epf_202105_edit <- epf_202105[11:41, 1:3]
epf_202106_edit <- epf_202106[11:40, 1:3]
epf_202107_edit <- epf_202107[11:41, 1:3]
epf_202108_edit <- epf_202108[11:41, 1:3]
epf_202109_edit <- epf_202109[11:40, 1:3]
epf_202110_edit <- epf_202110[11:41, 1:3]
epf_202111_edit <- epf_202111[11:40, 1:3]
epf_202112_edit <- epf_202112[11:41, 1:3]
epf_202201_edit <- epf_202201[11:41, 1:3]
epf_202202_edit <- epf_202202[11:38, 1:3]

epf_evapo <- bind_rows(epf_202002_edit, epf_202003_edit, epf_202004_edit, epf_202005_edit, epf_202006_edit, epf_202007_edit, epf_202008_edit, epf_202009_edit, epf_202010_edit, epf_202011_edit, epf_202012_edit, epf_202101_edit, epf_202102_edit, epf_202103_edit, epf_202104_edit, epf_202105_edit, epf_202106_edit, epf_202107_edit, epf_202108_edit, epf_202109_edit, epf_202110_edit, epf_202111_edit, epf_202112_edit, epf_202201_edit, epf_202202_edit)
names(epf_evapo) <- c("BOM_site", "date", "evapotranspiration_mm")
epf_evapo$date <- as.Date(epf_evapo$date, "%d/%m/%Y")
epf_evapo$evapotranspiration_mm <- as.numeric(epf_evapo$evapotranspiration_mm)

epf_evapo_period1 <- epf_evapo %>% subset(date >= "2020-02-06" & date <= "2021-02-14") %>%
  summarise(period1_evapo = sum(evapotranspiration_mm, na.rm = TRUE))
epf_evapo_period2 <- epf_evapo %>% subset(date >= "2021-02-15" & date <= "2022-02-16") %>%
  summarise(period2_evapo = sum(evapotranspiration_mm, na.rm = TRUE))
period_evapo_data <- within(period_evapo_data, period_evapo[Period == '1' & Site == 'EPF'] <- epf_evapo_period1)
period_evapo_data <- within(period_evapo_data, period_evapo[Period == '2' & Site == 'EPF'] <- epf_evapo_period2)

### Merge period evapotranspiration data with rainfall data ##
period_climate <- left_join(period_rainfall_data, period_evapo_data)
#Calculate period moisture deficit (evapotranspiration - precipitation)
period_climate$period_evapo <- as.numeric(period_climate$period_evapo)
period_climate$period_rainfall <- as.numeric(period_climate$period_rainfall)
period_climate <- period_climate %>% mutate(period_md = period_evapo - period_rainfall)

## Merging into growth data
growthdata <- left_join(growthdata, period_climate)

## Importing community survey data and merging it with growthalldata
rawsurveydata <- read_csv("Data/community_surveys_data.csv", col_types = cols(.default = "?", Neighbour_DBH_cm_7 = col_double(), Neighbour_DBH_cm_8 = col_double(), Neighbour_DBH_cm_9 = col_double(), Neighbour_DBH_cm_10 = col_double(), Neighbour_DBH_cm_11 = col_double()))
#Parsing error using just read_csv because it is deciding what is in the column from first 1000 rows
# and there trees with > 7 stems are after first 1000 rows

#Removing 5 NAs for Neighbour_DBH_cm where the DBH was not recorded
surveydata <- rawsurveydata %>% filter(!is.na(Neighbour_DBH_cm))

### Checking survey notes
#All notes on any row:
#test <- surveydata %>% filter(!((is.na(Notes_surveys))))
#All notes about focal were on first row for that plot:
#Completed surveys for 310 plots
#test2 <- surveydata %>% group_by(Site, Focal_sp, Plot, Tree) %>% filter(row_number() == 1)

#GRA VIMI B3 - Notes 'Focal definitely a GLOB'. Removing this and this:
#TMP OBLI B5 - Melaleuca
#There were other melaleucas and other focal species that were incorrectly IDd that I did not survey
#These will be dropped when I merge growth and survey data? Shouldn't be in models, but check them * to do *
growthdata <- growthdata %>% filter(!((Site == "GRA" & Focal_sp == "VIMI" & Plot == "B" & Tree == "3")))
surveydata <- surveydata %>% filter(!((Site == "GRA" & Focal_sp == "VIMI" & Plot == "B" & Tree == "3")))

growthdata <- growthdata %>% filter(!((Site == "TMP" & Focal_sp == "OBLI" & Plot == "B" & Tree == "5")))
surveydata <- surveydata %>% filter(!((Site == "TMP" & Focal_sp == "OBLI" & Plot == "B" & Tree == "5")))

#Replacing NAs with 0s (to merge neighbour DBHs later)
#test <- surveydata %>% replace(is.na(.), 0) #not working :(
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

#### Plotting data! To see how much survey info (for fun)
# ggplot(surveydata, aes(x = Neighbour_sp_ID, y = log(Sum_nbh_DBH)))+
#   geom_jitter(alpha=0.1)+
#   theme_classic()+
#   theme(axis.text.x = element_text(angle = 90))

# test <- surveydata %>% filter(Neighbour_distance_m < 10)
# dev.off()
# pdf("Test_neighbour_fig.pdf")
# ggplot(test, aes(x = Neighbour_distance_m, y = log(Sum_nbh_DBH)))+
#   geom_point(aes(col = Neighbour_sp_ID), alpha=0.1)+
#   theme_classic()+
#   theme(legend.position = "bottom", legend.key.size = unit(0.2, "cm"), legend.title = NULL)
# dev.off()
# # legend.key.width=unit(0.01,"cm")
####

#### Calculating total DBH of neighbours and number of neighbours
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
#examples, 40 cm DBH focal, 20 cm DBH neighbour, 1 m away: 0.005
#or 9 m away: 0.00056

#Need to import DBH data to surveydata to calculate NCI
dbhdatatomerge <- growthdata %>% group_by(Site, Focal_sp, Plot, Tree) %>%
  filter(row_number() == 1) %>% select(Site, Focal_sp, Plot, Tree, DBH_cm)
surveydata <- left_join(surveydata, dbhdatatomerge)
#Calculating NCI at individual neighbour level
surveydata <- surveydata %>% mutate(NCI_nbh = Neighbour_DBH_cm/(DBH_cm*Neighbour_distance_m))
testing <- surveydata
#Calculating total NCI by focal tree
totalncidata <- surveydata %>% group_by(Site, Focal_sp, Plot, Tree) %>%
  summarise(total_nci = sum(NCI_nbh, na.rm = TRUE))
surveydata <- left_join(surveydata, totalncidata)

### Selecting columns to merge with growth data
surveysimple <- surveydata %>% group_by(Site, Focal_sp, Plot, Tree) %>% 
  filter(row_number() == 1) %>% select(Site, Focal_sp, Plot, Tree, 
                                       total_nbh_ba, number_neighbours, total_nci)

#Merging neighbour data with growth rate data
#get NAs in total_nci after merging growthdata and surveysimple here
#13 trees that are in growth data but not in surveysimple
#some rows are NA because either no nbh dbh, no focal dbh or no nbh distance
# here **
growthnbhdata <- left_join(growthdata, surveysimple, by = c("Site", "Focal_sp", "Plot", "Tree"))

test <- anti_join(growthdata, surveysimple)
## Making a simplified dataset with summary data
growthnbhdata <- growthnbhdata %>% select(Site, Focal_sp, Plot, Tree, Period, Growth,
                                          DBH_cm, Site_name, PPT, PET, MD, period_rainfall, period_md,
                                          growth_no_negs, growth_rate,
                                          total_nbh_ba, number_neighbours, total_nci)

## Standardising NCI to a mean of 0 and SD of 1, first creating logged version
growthnbhdata <- growthnbhdata %>% mutate(log_nci = log(total_nci))
#Figure out why scaling nci isn't working!! *
growthnbhdata$std_nci <- scale(growthnbhdata$log_nci, center = TRUE, scale = TRUE)[,1]
#Scaling initial DBH too
growthnbhdata <- growthnbhdata %>% mutate(sqrt_dbh = sqrt(DBH_cm))
growthnbhdata$std_dbh <- scale(growthnbhdata$sqrt_dbh, center = TRUE, scale = TRUE)[,1]
#Scaling PPT
growthnbhdata$std_ppt <- scale(growthnbhdata$PPT, center = TRUE, scale = TRUE)[,1]
#Scaling MD
growthnbhdata$std_md <- scale(growthnbhdata$MD, center = TRUE, scale = TRUE)[,1]
#Scaling period rainfall
growthnbhdata$period_rainfall <- as.numeric(growthnbhdata$period_rainfall)
growthnbhdata$std_prain <- scale(growthnbhdata$period_rainfall, center = TRUE, scale = TRUE)[,1]
growthnbhdata$std_period_md <- scale(growthnbhdata$period_md, center = TRUE, scale = TRUE)[,1]

##Adjusting names of some plots
#GRA OVAT A 3 and GRA OVAT A 4 - notes say 'this is a VIMI'
#Changing these to VIMI and plot Z in growth notes
growthnbhdata <- within(growthnbhdata, Plot[Site == 'GRA' & Focal_sp == 'OVAT' & Plot == 'A' & Tree == '3'] <- 'Z')
growthnbhdata <- within(growthnbhdata, Focal_sp[Site == 'GRA' & Focal_sp == 'OVAT' & Plot == 'Z' & Tree == '3'] <- 'VIMI')

growthnbhdata <- within(growthnbhdata, Plot[Site == 'GRA' & Focal_sp == 'OVAT' & Plot == 'A' & Tree == '4'] <- 'Z')
growthnbhdata <- within(growthnbhdata, Focal_sp[Site == 'GRA' & Focal_sp == 'OVAT' & Plot == 'Z' & Tree == '4'] <- 'VIMI')

## Removing dodgy values where the band came off etc. See notes, many things to adjust.
# do this!
#EPF VIMI B1 is an ovata. Updating this to OVAT Z1 (adjust plot later!)
growthnbhdata <- within(growthnbhdata, Plot[Site == 'EPF' & Focal_sp == 'VIMI' & Plot == 'B' & Tree == '1'] <- 'Z')
growthnbhdata <- within(growthnbhdata, Focal_sp[Site == 'EPF' & Focal_sp == 'VIMI' & Plot == 'Z' & Tree == '1'] <- 'OVAT')

onerowdata <- growthnbhdata %>% filter(Period==1)
