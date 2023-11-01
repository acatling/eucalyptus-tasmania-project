### Alexandra Catling Eucalyptus research 2023 
## Script for soil variables and observed climate

#### Calculate period climate ####
# Using BOM site daily rainfall information
#EPF_all was up to 2022, EPF_all_2023 includes 2023
bom_epf <- read_csv("Data/EPF_all_2023.csv")
bom_tmp <- read_csv("Data/TMP_all_2023.csv")
bom_mer <- read_csv("Data/MER_all_2023.csv")
bom_dog <- read_csv("Data/DOG_all_2023.csv")
bom_gra <- read_csv("Data/GRA_all_2023.csv")
bom_frey <- read_csv("Data/FREY_all_2023.csv")
bom_bof <- read_csv("Data/BOF_all_2023.csv")

#Creating dataset to put period data into
period_rainfall_data <- datesdata %>% select(Site, Period) %>% mutate(period_rainfall = 1)

## To do - check NAs for all of them
## EPF 
# period 1: 06/02/2020 - 14/02/2021 period 2: 15/02/2021 - 16/02/2022
# period 3: 17/02/2022 - 21/02/2023
#Merge date info into one column called date
bom_epf$date <- as.Date(with(bom_epf, paste(Year, Month, Day,sep="-")), "%Y-%m-%d")
#Calculate rainfall for period 1
epf_rainfall_period1 <- bom_epf %>% subset(date >= "2020-02-06" & date <= "2021-02-14") %>%
  summarise(period1_rainfall = sum(`Rainfall amount (millimetres)`, na.rm=T))
#Calculate rainfall for period 2
epf_rainfall_period2 <- bom_epf %>% subset(date >= "2021-02-15" & date <= "2022-02-16") %>%
  summarise(period2_rainfall = sum(`Rainfall amount (millimetres)`, na.rm=T))
#Calculate rainfall for period 3
epf_rainfall_period3 <- bom_epf %>% subset(date >= "2022-02-17" & date <= "2023-02-21") %>%
  summarise(period3_rainfall = sum(`Rainfall amount (millimetres)`))
#Filling data in table
period_rainfall_data <- within(period_rainfall_data, period_rainfall[Period == '1' & Site == 'EPF'] <- epf_rainfall_period1)
period_rainfall_data <- within(period_rainfall_data, period_rainfall[Period == '2' & Site == 'EPF'] <- epf_rainfall_period2)
period_rainfall_data <- within(period_rainfall_data, period_rainfall[Period == '3' & Site == 'EPF'] <- epf_rainfall_period3)

##TMP
# period 1: 29/01/2020 - 15/02/2021 period 2: 16/02/2021 - 13/02/2022
# period 3: 14/02/2022 - 19/02/2023
#Lots of NAs for period 2 - was it out of order? Backburning? Check this *
bom_tmp$date <- as.Date(with(bom_tmp, paste(Year, Month, Day,sep="-")), "%Y-%m-%d")
tmp_rainfall_period1 <- bom_tmp %>% subset(date >= "2020-01-29" & date <= "2021-02-15") %>%
  summarise(period1_rainfall = sum(`Rainfall amount (millimetres)`, na.rm=T))
tmp_rainfall_period2 <- bom_tmp %>% subset(date >= "2021-02-16" & date <= "2022-02-13") %>%
  summarise(period2_rainfall = sum(`Rainfall amount (millimetres)`, na.rm=TRUE))
tmp_rainfall_period3 <- bom_tmp %>% subset(date >= "2022-02-14" & date <= "2023-02-19") %>%
  summarise(period3_rainfall = sum(`Rainfall amount (millimetres)`, na.rm=TRUE))
period_rainfall_data <- within(period_rainfall_data, period_rainfall[Period == '1' & Site == 'TMP'] <- tmp_rainfall_period1)
period_rainfall_data <- within(period_rainfall_data, period_rainfall[Period == '2' & Site == 'TMP'] <- tmp_rainfall_period2)
period_rainfall_data <- within(period_rainfall_data, period_rainfall[Period == '3' & Site == 'TMP'] <- tmp_rainfall_period3)

##MER
# period 1: 31/01/2020 - 13/02/2021 period 2: 14/02/2021 - 18/02/2022
# period 3: 19/02/2022 - 23/02/2023
bom_mer$date <- as.Date(with(bom_mer, paste(Year, Month, Day,sep="-")), "%Y-%m-%d")
mer_rainfall_period1 <- bom_mer %>% subset(date >= "2020-01-31" & date <= "2021-02-13") %>%
  summarise(period1_rainfall = sum(`Rainfall amount (millimetres)`, na.rm=TRUE))
mer_rainfall_period2 <- bom_mer %>% subset(date >= "2021-02-14" & date <= "2022-02-18") %>%
  summarise(period2_rainfall = sum(`Rainfall amount (millimetres)`, na.rm=TRUE))
mer_rainfall_period3 <- bom_mer %>% subset(date >= "2022-02-19" & date <= "2023-02-23") %>%
  summarise(period3_rainfall = sum(`Rainfall amount (millimetres)`, na.rm=TRUE))
period_rainfall_data <- within(period_rainfall_data, period_rainfall[Period == '1' & Site == 'MER'] <- mer_rainfall_period1)
period_rainfall_data <- within(period_rainfall_data, period_rainfall[Period == '2' & Site == 'MER'] <- mer_rainfall_period2)
period_rainfall_data <- within(period_rainfall_data, period_rainfall[Period == '3' & Site == 'MER'] <- mer_rainfall_period3)

##DOG
# period 1: 30/01/2020 - 13/02/2021 period 2: 14/02/2021 - 18/02/2022
# period 3: 19/02/2022 - 23/02/2023
bom_dog$date <- as.Date(with(bom_dog, paste(Year, Month, Day,sep="-")), "%Y-%m-%d")
dog_rainfall_period1 <- bom_dog %>% subset(date >= "2020-01-30" & date <= "2021-02-13") %>%
  summarise(period1_rainfall = sum(`Rainfall amount (millimetres)`))
dog_rainfall_period2 <- bom_dog %>% subset(date >= "2021-02-14" & date <= "2022-02-18") %>%
  summarise(period2_rainfall = sum(`Rainfall amount (millimetres)`))
dog_rainfall_period3 <- bom_dog %>% subset(date >= "2022-02-19" & date <= "2023-02-23") %>%
  summarise(period3_rainfall = sum(`Rainfall amount (millimetres)`, na.rm=T))
period_rainfall_data <- within(period_rainfall_data, period_rainfall[Period == '1' & Site == 'DOG'] <- dog_rainfall_period1)
period_rainfall_data <- within(period_rainfall_data, period_rainfall[Period == '2' & Site == 'DOG'] <- dog_rainfall_period2)
period_rainfall_data <- within(period_rainfall_data, period_rainfall[Period == '3' & Site == 'DOG'] <- dog_rainfall_period3)

##GRA
# period 1: 07/02/2020 - 09/02/2021 period 2: 10/02/2021 - 13/02/2022
# period 3: 14/02/2022 - 20/02/2023
bom_gra$date <- as.Date(with(bom_gra, paste(Year, Month, Day,sep="-")), "%Y-%m-%d")
gra_rainfall_period1 <- bom_gra %>% subset(date >= "2020-02-07" & date <= "2021-02-09") %>%
  summarise(period1_rainfall = sum(`Rainfall amount (millimetres)`))
gra_rainfall_period2 <- bom_gra %>% subset(date >= "2021-02-10" & date <= "2022-02-13") %>%
  summarise(period2_rainfall = sum(`Rainfall amount (millimetres)`))
gra_rainfall_period3 <- bom_gra %>% subset(date >= "2022-02-14" & date <= "2023-02-20") %>%
  summarise(period3_rainfall = sum(`Rainfall amount (millimetres)`))
period_rainfall_data <- within(period_rainfall_data, period_rainfall[Period == '1' & Site == 'GRA'] <- gra_rainfall_period1)
period_rainfall_data <- within(period_rainfall_data, period_rainfall[Period == '2' & Site == 'GRA'] <- gra_rainfall_period2)
period_rainfall_data <- within(period_rainfall_data, period_rainfall[Period == '3' & Site == 'GRA'] <- gra_rainfall_period3)

##FREY
# period 1: 05/02/2020 - 16/02/2021 period 2: 17/02/2021 - 23/02/2022
# period 3: 24/02/2022 - 22/02/2023
bom_frey$date <- as.Date(with(bom_frey, paste(Year, Month, Day,sep="-")), "%Y-%m-%d")
frey_rainfall_period1 <- bom_frey %>% subset(date >= "2020-02-05" & date <= "2021-02-16") %>%
  summarise(period1_rainfall = sum(`Rainfall amount (millimetres)`))
frey_rainfall_period2 <- bom_frey %>% subset(date >= "2021-02-17" & date <= "2022-02-23") %>%
  summarise(period2_rainfall = sum(`Rainfall amount (millimetres)`))
frey_rainfall_period3 <- bom_frey %>% subset(date >= "2022-02-24" & date <= "2023-02-22") %>%
  summarise(period3_rainfall = sum(`Rainfall amount (millimetres)`))
period_rainfall_data <- within(period_rainfall_data, period_rainfall[Period == '1' & Site == 'FREY'] <- frey_rainfall_period1)
period_rainfall_data <- within(period_rainfall_data, period_rainfall[Period == '2' & Site == 'FREY'] <- frey_rainfall_period2)
period_rainfall_data <- within(period_rainfall_data, period_rainfall[Period == '3' & Site == 'FREY'] <- frey_rainfall_period3)

##BOF
# period 1: 05/06/2020 - 11/02/2021 period 2: 12/02/2021 - 24/02/2022
# period 3: 25/02/2022 - 22/02/2023
bom_bof$date <- as.Date(with(bom_bof, paste(Year, Month, Day,sep="-")), "%Y-%m-%d")
bof_rainfall_period1 <- bom_bof %>% subset(date >= "2020-01-29" & date <= "2021-02-15") %>%
  summarise(period1_rainfall = sum(`Rainfall amount (millimetres)`, na.rm = TRUE))
bof_rainfall_period2 <- bom_bof %>% subset(date >= "2021-02-16" & date <= "2022-02-13") %>%
  summarise(period2_rainfall = sum(`Rainfall amount (millimetres)`, na.rm = TRUE))
bof_rainfall_period3 <- bom_bof %>% subset(date >= "2022-02-25" & date <= "2023-02-22") %>%
  summarise(period3_rainfall = sum(`Rainfall amount (millimetres)`, na.rm = TRUE))
period_rainfall_data <- within(period_rainfall_data, period_rainfall[Period == '1' & Site == 'BOF'] <- bof_rainfall_period1)
period_rainfall_data <- within(period_rainfall_data, period_rainfall[Period == '2' & Site == 'BOF'] <- bof_rainfall_period2)
period_rainfall_data <- within(period_rainfall_data, period_rainfall[Period == '3' & Site == 'BOF'] <- bof_rainfall_period3)

###relativise these observed values with the expected rainfall amounts based on long-term monthly averages
#Only relativise if long-term and short-term are correlated, not doing this

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
bof_202203 <- read_csv("Data/BOM_evapotranspiration/BOF/st_helens_aerodrome-202203.csv")
bof_202204 <- read_csv("Data/BOM_evapotranspiration/BOF/st_helens_aerodrome-202204.csv")
bof_202205 <- read_csv("Data/BOM_evapotranspiration/BOF/st_helens_aerodrome-202205.csv")
bof_202206 <- read_csv("Data/BOM_evapotranspiration/BOF/st_helens_aerodrome-202206.csv")
bof_202207 <- read_csv("Data/BOM_evapotranspiration/BOF/st_helens_aerodrome-202207.csv")
bof_202208 <- read_csv("Data/BOM_evapotranspiration/BOF/st_helens_aerodrome-202208.csv")
bof_202209 <- read_csv("Data/BOM_evapotranspiration/BOF/st_helens_aerodrome-202209.csv")
bof_202210 <- read_csv("Data/BOM_evapotranspiration/BOF/st_helens_aerodrome-202210.csv")
bof_202211 <- read_csv("Data/BOM_evapotranspiration/BOF/st_helens_aerodrome-202211.csv")
bof_202212 <- read_csv("Data/BOM_evapotranspiration/BOF/st_helens_aerodrome-202212.csv")
bof_202301 <- read_csv("Data/BOM_evapotranspiration/BOF/st_helens_aerodrome-202301.csv")
bof_202302 <- read_csv("Data/BOM_evapotranspiration/BOF/st_helens_aerodrome-202302.csv")

# period 1: 05/06/2020 - 11/02/2021 period 2: 12/02/2021 - 24/02/2022
# period 3: 25/02/2022 - 22/02/2023
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
bof_202203_edit <- bof_202203[11:41, 1:3]
bof_202204_edit <- bof_202204[11:40, 1:3]
bof_202205_edit <- bof_202205[11:41, 1:3]
bof_202206_edit <- bof_202206[11:40, 1:3]
bof_202207_edit <- bof_202207[11:41, 1:3]
bof_202208_edit <- bof_202208[11:41, 1:3]
bof_202209_edit <- bof_202209[11:40, 1:3]
bof_202210_edit <- bof_202210[11:41, 1:3]
bof_202211_edit <- bof_202211[11:40, 1:3]
bof_202212_edit <- bof_202212[11:41, 1:3]
bof_202301_edit <- bof_202301[11:41, 1:3]
bof_202302_edit <- bof_202302[11:38, 1:3]

#Bind all months together
bof_evapo <- bind_rows(bof_202006_edit, bof_202007_edit, bof_202008_edit, bof_202009_edit, bof_202010_edit, bof_202011_edit, bof_202012_edit, 
                       bof_202101_edit, bof_202102_edit, bof_202103_edit, bof_202104_edit, bof_202105_edit, bof_202106_edit, bof_202107_edit, bof_202108_edit, bof_202109_edit, bof_202110_edit, bof_202111_edit, bof_202112_edit, 
                       bof_202201_edit, bof_202202_edit, bof_202203_edit, bof_202204_edit, bof_202205_edit, bof_202206_edit, bof_202207_edit, bof_202208_edit, bof_202209_edit, bof_202210_edit, bof_202211_edit, bof_202212_edit, 
                       bof_202301_edit, bof_202302_edit)
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
bof_evapo_period3 <- bof_evapo %>% subset(date >= "2022-02-25" & date <= "2023-02-22") %>%
  summarise(period3_evapo = sum(evapotranspiration_mm, na.rm = TRUE))
period_evapo_data <- within(period_evapo_data, period_evapo[Period == '1' & Site == 'BOF'] <- bof_evapo_period1)
period_evapo_data <- within(period_evapo_data, period_evapo[Period == '2' & Site == 'BOF'] <- bof_evapo_period2)
period_evapo_data <- within(period_evapo_data, period_evapo[Period == '3' & Site == 'BOF'] <- bof_evapo_period3)

#FREY
# period 1: 05/02/2020 - 16/02/2021 period 2: 17/02/2021 - 23/02/2022
# period 3: 24/02/2022 - 22/02/2023
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
frey_202203 <- read_csv("Data/BOM_evapotranspiration/frey/friendly_beaches-202203.csv")
frey_202204 <- read_csv("Data/BOM_evapotranspiration/frey/friendly_beaches-202204.csv")
frey_202205 <- read_csv("Data/BOM_evapotranspiration/frey/friendly_beaches-202205.csv")
frey_202206 <- read_csv("Data/BOM_evapotranspiration/frey/friendly_beaches-202206.csv")
frey_202207 <- read_csv("Data/BOM_evapotranspiration/frey/friendly_beaches-202207.csv")
frey_202208 <- read_csv("Data/BOM_evapotranspiration/frey/friendly_beaches-202208.csv")
frey_202209 <- read_csv("Data/BOM_evapotranspiration/frey/friendly_beaches-202209.csv")
frey_202210 <- read_csv("Data/BOM_evapotranspiration/frey/friendly_beaches-202210.csv")
frey_202211 <- read_csv("Data/BOM_evapotranspiration/frey/friendly_beaches-202211.csv")
frey_202212 <- read_csv("Data/BOM_evapotranspiration/frey/friendly_beaches-202212.csv")
frey_202301 <- read_csv("Data/BOM_evapotranspiration/frey/friendly_beaches-202301.csv")
frey_202302 <- read_csv("Data/BOM_evapotranspiration/frey/friendly_beaches-202302.csv")

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
frey_202203_edit <- frey_202203[11:41, 1:3]
frey_202204_edit <- frey_202204[11:40, 1:3]
frey_202205_edit <- frey_202205[11:41, 1:3]
frey_202206_edit <- frey_202206[11:40, 1:3]
frey_202207_edit <- frey_202207[11:41, 1:3]
frey_202208_edit <- frey_202208[11:41, 1:3]
frey_202209_edit <- frey_202209[11:40, 1:3]
frey_202210_edit <- frey_202210[11:41, 1:3]
frey_202211_edit <- frey_202211[11:40, 1:3]
frey_202212_edit <- frey_202212[11:41, 1:3]
frey_202301_edit <- frey_202301[11:41, 1:3]
frey_202302_edit <- frey_202302[11:38, 1:3]

frey_evapo <- bind_rows(frey_202002_edit, frey_202003_edit, frey_202004_edit, frey_202005_edit, frey_202006_edit, frey_202007_edit, frey_202008_edit, frey_202009_edit, frey_202010_edit, frey_202011_edit, frey_202012_edit, 
                        frey_202101_edit, frey_202102_edit, frey_202103_edit, frey_202104_edit, frey_202105_edit, frey_202106_edit, frey_202107_edit, frey_202108_edit, frey_202109_edit, frey_202110_edit, frey_202111_edit, frey_202112_edit, 
                        frey_202201_edit, frey_202202_edit, frey_202203_edit, frey_202204_edit, frey_202205_edit, frey_202206_edit, frey_202207_edit, frey_202208_edit, frey_202209_edit, frey_202210_edit, frey_202211_edit, frey_202212_edit, 
                        frey_202301_edit, frey_202302_edit)
names(frey_evapo) <- c("BOM_site", "date", "evapotranspiration_mm")
frey_evapo$date <- as.Date(frey_evapo$date, "%d/%m/%Y")
frey_evapo$evapotranspiration_mm <- as.numeric(frey_evapo$evapotranspiration_mm)

frey_evapo_period1 <- frey_evapo %>% subset(date >= "2020-02-05" & date <= "2021-02-16") %>%
  summarise(period1_evapo = sum(evapotranspiration_mm, na.rm = TRUE))
frey_evapo_period2 <- frey_evapo %>% subset(date >= "2021-02-17" & date <= "2022-02-23") %>%
  summarise(period2_evapo = sum(evapotranspiration_mm, na.rm = TRUE))
frey_evapo_period3 <- frey_evapo %>% subset(date >= "2022-02-24" & date <= "2023-02-22") %>%
  summarise(period3_evapo = sum(evapotranspiration_mm, na.rm = TRUE))
period_evapo_data <- within(period_evapo_data, period_evapo[Period == '1' & Site == 'FREY'] <- frey_evapo_period1)
period_evapo_data <- within(period_evapo_data, period_evapo[Period == '2' & Site == 'FREY'] <- frey_evapo_period2)
period_evapo_data <- within(period_evapo_data, period_evapo[Period == '3' & Site == 'FREY'] <- frey_evapo_period3)

##GRA
# period 1: 07/02/2020 - 09/02/2021 period 2: 10/02/2021 - 13/02/2022
# period 3: 14/02/2022 - 20/02/2023

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
gra_202203 <- read_csv("Data/BOM_evapotranspiration/gra/campania_(kincora)-202203.csv")
gra_202204 <- read_csv("Data/BOM_evapotranspiration/gra/campania_(kincora)-202204.csv")
gra_202205 <- read_csv("Data/BOM_evapotranspiration/gra/campania_(kincora)-202205.csv")
gra_202206 <- read_csv("Data/BOM_evapotranspiration/gra/campania_(kincora)-202206.csv")
gra_202207 <- read_csv("Data/BOM_evapotranspiration/gra/campania_(kincora)-202207.csv")
gra_202208 <- read_csv("Data/BOM_evapotranspiration/gra/campania_(kincora)-202208.csv")
gra_202209 <- read_csv("Data/BOM_evapotranspiration/gra/campania_(kincora)-202209.csv")
gra_202210 <- read_csv("Data/BOM_evapotranspiration/gra/campania_(kincora)-202210.csv")
gra_202211 <- read_csv("Data/BOM_evapotranspiration/gra/campania_(kincora)-202211.csv")
gra_202212 <- read_csv("Data/BOM_evapotranspiration/gra/campania_(kincora)-202212.csv")
gra_202301 <- read_csv("Data/BOM_evapotranspiration/gra/campania_(kincora)-202301.csv")
gra_202302 <- read_csv("Data/BOM_evapotranspiration/gra/campania_(kincora)-202302.csv")

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
gra_202203_edit <- gra_202203[11:41, 1:3]
gra_202204_edit <- gra_202204[11:40, 1:3]
gra_202205_edit <- gra_202205[11:41, 1:3]
gra_202206_edit <- gra_202206[11:40, 1:3]
gra_202207_edit <- gra_202207[11:41, 1:3]
gra_202208_edit <- gra_202208[11:41, 1:3]
gra_202209_edit <- gra_202209[11:40, 1:3]
gra_202210_edit <- gra_202210[11:41, 1:3]
gra_202211_edit <- gra_202211[11:40, 1:3]
gra_202212_edit <- gra_202212[11:41, 1:3]
gra_202301_edit <- gra_202301[11:41, 1:3]
gra_202302_edit <- gra_202302[11:38, 1:3]

gra_evapo <- bind_rows(gra_202002_edit, gra_202003_edit, gra_202004_edit, gra_202005_edit, gra_202006_edit, gra_202007_edit, gra_202008_edit, gra_202009_edit, gra_202010_edit, gra_202011_edit, gra_202012_edit, 
                       gra_202101_edit, gra_202102_edit, gra_202103_edit, gra_202104_edit, gra_202105_edit, gra_202106_edit, gra_202107_edit, gra_202108_edit, gra_202109_edit, gra_202110_edit, gra_202111_edit, gra_202112_edit, 
                       gra_202201_edit, gra_202202_edit, gra_202203_edit, gra_202204_edit, gra_202205_edit, gra_202206_edit, gra_202207_edit, gra_202208_edit, gra_202209_edit, gra_202210_edit, gra_202211_edit, gra_202212_edit, 
                       gra_202301_edit, gra_202302_edit)
names(gra_evapo) <- c("BOM_site", "date", "evapotranspiration_mm")
gra_evapo$date <- as.Date(gra_evapo$date, "%d/%m/%Y")
gra_evapo$evapotranspiration_mm <- as.numeric(gra_evapo$evapotranspiration_mm)

gra_evapo_period1 <- gra_evapo %>% subset(date >= "2020-02-07" & date <= "2021-02-09") %>%
  summarise(period1_evapo = sum(evapotranspiration_mm, na.rm = TRUE))
gra_evapo_period2 <- gra_evapo %>% subset(date >= "2021-02-10" & date <= "2022-02-13") %>%
  summarise(period2_evapo = sum(evapotranspiration_mm, na.rm = TRUE))
gra_evapo_period3 <- gra_evapo %>% subset(date >= "2022-02-14" & date <= "2023-02-20") %>%
  summarise(period3_evapo = sum(evapotranspiration_mm, na.rm = TRUE))
period_evapo_data <- within(period_evapo_data, period_evapo[Period == '1' & Site == 'GRA'] <- gra_evapo_period1)
period_evapo_data <- within(period_evapo_data, period_evapo[Period == '2' & Site == 'GRA'] <- gra_evapo_period2)
period_evapo_data <- within(period_evapo_data, period_evapo[Period == '3' & Site == 'GRA'] <- gra_evapo_period3)

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
dog_202203 <- read_csv("Data/BOM_evapotranspiration/dog/sheffield_school_farm-202203.csv")
dog_202204 <- read_csv("Data/BOM_evapotranspiration/dog/sheffield_school_farm-202204.csv")
dog_202205 <- read_csv("Data/BOM_evapotranspiration/dog/sheffield_school_farm-202205.csv")
dog_202206 <- read_csv("Data/BOM_evapotranspiration/dog/sheffield_school_farm-202206.csv")
dog_202207 <- read_csv("Data/BOM_evapotranspiration/dog/sheffield_school_farm-202207.csv")
dog_202208 <- read_csv("Data/BOM_evapotranspiration/dog/sheffield_school_farm-202208.csv")
dog_202209 <- read_csv("Data/BOM_evapotranspiration/dog/sheffield_school_farm-202209.csv")
dog_202210 <- read_csv("Data/BOM_evapotranspiration/dog/sheffield_school_farm-202210.csv")
dog_202211 <- read_csv("Data/BOM_evapotranspiration/dog/sheffield_school_farm-202211.csv")
dog_202212 <- read_csv("Data/BOM_evapotranspiration/dog/sheffield_school_farm-202212.csv")
dog_202301 <- read_csv("Data/BOM_evapotranspiration/dog/sheffield_school_farm-202301.csv")
dog_202302 <- read_csv("Data/BOM_evapotranspiration/dog/sheffield_school_farm-202302.csv")

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
dog_202203_edit <- dog_202203[11:41, 1:3]
dog_202204_edit <- dog_202204[11:40, 1:3]
dog_202205_edit <- dog_202205[11:41, 1:3]
dog_202206_edit <- dog_202206[11:40, 1:3]
dog_202207_edit <- dog_202207[11:41, 1:3]
dog_202208_edit <- dog_202208[11:41, 1:3]
dog_202209_edit <- dog_202209[11:40, 1:3]
dog_202210_edit <- dog_202210[11:41, 1:3]
dog_202211_edit <- dog_202211[11:40, 1:3]
dog_202212_edit <- dog_202212[11:41, 1:3]
dog_202301_edit <- dog_202301[11:41, 1:3]
dog_202302_edit <- dog_202302[11:38, 1:3]

dog_evapo <- bind_rows(dog_202001_edit, dog_202002_edit, dog_202003_edit, dog_202004_edit, dog_202005_edit, dog_202006_edit, dog_202007_edit, dog_202008_edit, dog_202009_edit, dog_202010_edit, dog_202011_edit, dog_202012_edit, 
                       dog_202101_edit, dog_202102_edit, dog_202103_edit, dog_202104_edit, dog_202105_edit, dog_202106_edit, dog_202107_edit, dog_202108_edit, dog_202109_edit, dog_202110_edit, dog_202111_edit, dog_202112_edit, 
                       dog_202201_edit, dog_202202_edit, dog_202203_edit, dog_202204_edit, dog_202205_edit, dog_202206_edit, dog_202207_edit, dog_202208_edit, dog_202209_edit, dog_202210_edit, dog_202211_edit, dog_202212_edit, 
                       dog_202301_edit, dog_202302_edit)
names(dog_evapo) <- c("BOM_site", "date", "evapotranspiration_mm")
dog_evapo$date <- as.Date(dog_evapo$date, "%d/%m/%Y")
dog_evapo$evapotranspiration_mm <- as.numeric(dog_evapo$evapotranspiration_mm)

dog_evapo_period1 <- dog_evapo %>% subset(date >= "2020-01-30" & date <= "2021-02-13") %>%
  summarise(period1_evapo = sum(evapotranspiration_mm, na.rm = TRUE))
dog_evapo_period2 <- dog_evapo %>% subset(date >= "2021-02-14" & date <= "2022-02-18") %>%
  summarise(period2_evapo = sum(evapotranspiration_mm, na.rm = TRUE))
dog_evapo_period3 <- dog_evapo %>% subset(date >= "2022-02-19" & date <= "2023-02-23") %>%
  summarise(period3_evapo = sum(evapotranspiration_mm, na.rm = TRUE))
period_evapo_data <- within(period_evapo_data, period_evapo[Period == '1' & Site == 'DOG'] <- dog_evapo_period1)
period_evapo_data <- within(period_evapo_data, period_evapo[Period == '2' & Site == 'DOG'] <- dog_evapo_period2)
period_evapo_data <- within(period_evapo_data, period_evapo[Period == '3' & Site == 'DOG'] <- dog_evapo_period3)

##MER
#Uses same dataset as DOG for evapotranspiration
# period 1: 31/01/2020 - 13/02/2021 period 2: 14/02/2021 - 18/02/2022
mer_evapo_period1 <- dog_evapo %>% subset(date >= "2020-01-31" & date <= "2021-02-13") %>%
  summarise(period1_evapo = sum(evapotranspiration_mm, na.rm = TRUE))
mer_evapo_period2 <- dog_evapo %>% subset(date >= "2021-02-14" & date <= "2022-02-18") %>%
  summarise(period2_evapo = sum(evapotranspiration_mm, na.rm = TRUE))
mer_evapo_period3 <- dog_evapo %>% subset(date >= "2022-02-19" & date <= "2023-02-23") %>%
  summarise(period3_evapo = sum(evapotranspiration_mm, na.rm = TRUE))
period_evapo_data <- within(period_evapo_data, period_evapo[Period == '1' & Site == 'MER'] <- mer_evapo_period1)
period_evapo_data <- within(period_evapo_data, period_evapo[Period == '2' & Site == 'MER'] <- mer_evapo_period2)
period_evapo_data <- within(period_evapo_data, period_evapo[Period == '3' & Site == 'MER'] <- mer_evapo_period3)

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
tmp_202203 <- read_csv("Data/BOM_evapotranspiration/tmp/tasman_island-202203.csv")
tmp_202204 <- read_csv("Data/BOM_evapotranspiration/tmp/tasman_island-202204.csv")
tmp_202205 <- read_csv("Data/BOM_evapotranspiration/tmp/tasman_island-202205.csv")
tmp_202206 <- read_csv("Data/BOM_evapotranspiration/tmp/tasman_island-202206.csv")
tmp_202207 <- read_csv("Data/BOM_evapotranspiration/tmp/tasman_island-202207.csv")
tmp_202208 <- read_csv("Data/BOM_evapotranspiration/tmp/tasman_island-202208.csv")
tmp_202209 <- read_csv("Data/BOM_evapotranspiration/tmp/tasman_island-202209.csv")
tmp_202210 <- read_csv("Data/BOM_evapotranspiration/tmp/tasman_island-202210.csv")
tmp_202211 <- read_csv("Data/BOM_evapotranspiration/tmp/tasman_island-202211.csv")
tmp_202212 <- read_csv("Data/BOM_evapotranspiration/tmp/tasman_island-202212.csv")
tmp_202301 <- read_csv("Data/BOM_evapotranspiration/tmp/tasman_island-202301.csv")
tmp_202302 <- read_csv("Data/BOM_evapotranspiration/tmp/tasman_island-202302.csv")

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
tmp_202203_edit <- tmp_202203[11:41, 1:3]
tmp_202204_edit <- tmp_202204[11:40, 1:3]
tmp_202205_edit <- tmp_202205[11:41, 1:3]
tmp_202206_edit <- tmp_202206[11:40, 1:3]
tmp_202207_edit <- tmp_202207[11:41, 1:3]
tmp_202208_edit <- tmp_202208[11:41, 1:3]
tmp_202209_edit <- tmp_202209[11:40, 1:3]
tmp_202210_edit <- tmp_202210[11:41, 1:3]
tmp_202211_edit <- tmp_202211[11:40, 1:3]
tmp_202212_edit <- tmp_202212[11:41, 1:3]
tmp_202301_edit <- tmp_202301[11:41, 1:3]
tmp_202302_edit <- tmp_202302[11:38, 1:3]

tmp_evapo <- bind_rows(tmp_202001_edit, tmp_202002_edit, tmp_202003_edit, tmp_202004_edit, tmp_202005_edit, tmp_202006_edit, tmp_202007_edit, tmp_202008_edit, tmp_202009_edit, tmp_202010_edit, tmp_202011_edit, tmp_202012_edit, 
                       tmp_202101_edit, tmp_202102_edit, tmp_202103_edit, tmp_202104_edit, tmp_202105_edit, tmp_202106_edit, tmp_202107_edit, tmp_202108_edit, tmp_202109_edit, tmp_202110_edit, tmp_202111_edit, tmp_202112_edit, 
                       tmp_202201_edit, tmp_202202_edit, tmp_202203_edit, tmp_202204_edit, tmp_202205_edit, tmp_202206_edit, tmp_202207_edit, tmp_202208_edit, tmp_202209_edit, tmp_202210_edit, tmp_202211_edit, tmp_202212_edit, 
                       tmp_202301_edit, tmp_202302_edit)
names(tmp_evapo) <- c("BOM_site", "date", "evapotranspiration_mm")
tmp_evapo$date <- as.Date(tmp_evapo$date, "%d/%m/%Y")
tmp_evapo$evapotranspiration_mm <- as.numeric(tmp_evapo$evapotranspiration_mm)

tmp_evapo_period1 <- tmp_evapo %>% subset(date >= "2020-01-29" & date <= "2021-02-15") %>%
  summarise(period1_evapo = sum(evapotranspiration_mm, na.rm = TRUE))
tmp_evapo_period2 <- tmp_evapo %>% subset(date >= "2021-02-16" & date <= "2022-02-13") %>%
  summarise(period2_evapo = sum(evapotranspiration_mm, na.rm = TRUE))
tmp_evapo_period3 <- tmp_evapo %>% subset(date >= "2022-02-14" & date <= "2023-02-19") %>%
  summarise(period3_evapo = sum(evapotranspiration_mm, na.rm = TRUE))
period_evapo_data <- within(period_evapo_data, period_evapo[Period == '1' & Site == 'TMP'] <- tmp_evapo_period1)
period_evapo_data <- within(period_evapo_data, period_evapo[Period == '2' & Site == 'TMP'] <- tmp_evapo_period2)
period_evapo_data <- within(period_evapo_data, period_evapo[Period == '3' & Site == 'TMP'] <- tmp_evapo_period3)

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
epf_202203 <- read_csv("Data/BOM_evapotranspiration/epf/cressy_research_station-202203.csv")
epf_202204 <- read_csv("Data/BOM_evapotranspiration/epf/cressy_research_station-202204.csv")
epf_202205 <- read_csv("Data/BOM_evapotranspiration/epf/cressy_research_station-202205.csv")
epf_202206 <- read_csv("Data/BOM_evapotranspiration/epf/cressy_research_station-202206.csv")
epf_202207 <- read_csv("Data/BOM_evapotranspiration/epf/cressy_research_station-202207.csv")
epf_202208 <- read_csv("Data/BOM_evapotranspiration/epf/cressy_research_station-202208.csv")
epf_202209 <- read_csv("Data/BOM_evapotranspiration/epf/cressy_research_station-202209.csv")
epf_202210 <- read_csv("Data/BOM_evapotranspiration/epf/launceston_airport-202210.csv")
epf_202211 <- read_csv("Data/BOM_evapotranspiration/epf/launceston_airport-202211.csv")
epf_202212 <- read_csv("Data/BOM_evapotranspiration/epf/launceston_airport-202212.csv")
epf_202301 <- read_csv("Data/BOM_evapotranspiration/epf/cressy_(brumbys_creek)-202301.csv")
epf_202302 <- read_csv("Data/BOM_evapotranspiration/epf/cressy_(brumbys_creek)-202302.csv")

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
epf_202203_edit <- epf_202203[11:41, 1:3]
epf_202204_edit <- epf_202204[11:40, 1:3]
epf_202205_edit <- epf_202205[11:41, 1:3]
epf_202206_edit <- epf_202206[11:40, 1:3]
epf_202207_edit <- epf_202207[11:41, 1:3]
epf_202208_edit <- epf_202208[11:41, 1:3]
epf_202209_edit <- epf_202209[11:40, 1:3]
epf_202210_edit <- epf_202210[11:41, 1:3]
epf_202211_edit <- epf_202211[11:40, 1:3]
epf_202212_edit <- epf_202212[11:41, 1:3]
epf_202301_edit <- epf_202301[11:41, 1:3]
epf_202302_edit <- epf_202302[11:38, 1:3]

epf_evapo <- bind_rows(epf_202002_edit, epf_202003_edit, epf_202004_edit, epf_202005_edit, epf_202006_edit, epf_202007_edit, epf_202008_edit, epf_202009_edit, epf_202010_edit, epf_202011_edit, epf_202012_edit, 
                       epf_202101_edit, epf_202102_edit, epf_202103_edit, epf_202104_edit, epf_202105_edit, epf_202106_edit, epf_202107_edit, epf_202108_edit, epf_202109_edit, epf_202110_edit, epf_202111_edit, epf_202112_edit, 
                       epf_202201_edit, epf_202202_edit, epf_202203_edit, epf_202204_edit, epf_202205_edit, epf_202206_edit, epf_202207_edit, epf_202208_edit, epf_202209_edit, epf_202210_edit, epf_202211_edit, epf_202212_edit, 
                       epf_202301_edit, epf_202302_edit)
names(epf_evapo) <- c("BOM_site", "date", "evapotranspiration_mm")
epf_evapo$date <- as.Date(epf_evapo$date, "%d/%m/%Y")
epf_evapo$evapotranspiration_mm <- as.numeric(epf_evapo$evapotranspiration_mm)

epf_evapo_period1 <- epf_evapo %>% subset(date >= "2020-02-06" & date <= "2021-02-14") %>%
  summarise(period1_evapo = sum(evapotranspiration_mm, na.rm = TRUE))
epf_evapo_period2 <- epf_evapo %>% subset(date >= "2021-02-15" & date <= "2022-02-16") %>%
  summarise(period2_evapo = sum(evapotranspiration_mm, na.rm = TRUE))
epf_evapo_period3 <- epf_evapo %>% subset(date >= "2022-02-17" & date <= "2023-02-21") %>%
  summarise(period3_evapo = sum(evapotranspiration_mm, na.rm = TRUE))
period_evapo_data <- within(period_evapo_data, period_evapo[Period == '1' & Site == 'EPF'] <- epf_evapo_period1)
period_evapo_data <- within(period_evapo_data, period_evapo[Period == '2' & Site == 'EPF'] <- epf_evapo_period2)
period_evapo_data <- within(period_evapo_data, period_evapo[Period == '3' & Site == 'EPF'] <- epf_evapo_period3)

### Merge period evapotranspiration data with rainfall data ##
period_climate <- left_join(period_rainfall_data, period_evapo_data)
#Calculate period moisture deficit (evapotranspiration - precipitation)
period_climate$period_evapo <- as.numeric(period_climate$period_evapo)
period_climate$period_rainfall <- as.numeric(period_climate$period_rainfall)
period_climate <- period_climate %>% mutate(period_md = period_evapo - period_rainfall)

## Merging into growth data
growthdata <- left_join(growthdata, period_climate)


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

