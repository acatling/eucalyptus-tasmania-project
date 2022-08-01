######## Leander's data stand basal area Tasmania 2016
####### And Leander's DBH data

library(tidyverse)
library(ggplot2)

eucstandba <- read_csv("Data/Leander_stand_basal_area_data_Tas.csv")

#Below gives me StandBA for each Species at each of their Sites

eucstandbamean <- eucstandba %>% select(Species, Site, StandBA) %>%
  group_by(Species, Site) %>%
  summarise(meanstandBA = mean(StandBA, na.rm=TRUE), sdstandBA = sd(StandBA, na.rm=TRUE))

#Visualising it all
ggplot(eucstandba, aes(x = Site, y= StandBA))+
  geom_jitter()+
  theme_bw()

#Each species - not very informative without error bars
ggplot(eucstandbamean, aes(x = Site, y= meanstandBA, group = Species))+
  geom_point(aes(colour = Species))+
  geom_line(aes(colour = Species))+
  theme_bw()

#My attempt with error bars:
#Not sure how to get individual means and error bars for species
#Not quite right
  ggplot(eucstandba, aes(x = Site, y= StandBA, group = Species))+
  geom_jitter(aes(colour = Species))+
  theme_bw()+
  geom_point(stat="summary", fun.y="mean",size=2) +
  geom_errorbar(stat="summary", fun.data="mean_se",width=0,size=0.8)


#Boxplot per site
ggplot(eucstandbamean, aes(x = Site, y= meanstandBA))+
  geom_boxplot()+
  geom_jitter(aes(colour = Species))+
  theme_bw()


################ DBH data ################ 
eucdbh <- read_csv("Data/Leander_euc_DBH_R.csv")

ggplot(eucdbh, aes(x = Species, y= TreeDBH))+
  geom_boxplot()+
  geom_jitter(aes(colour = Species))+
  theme_bw() +
  ylab("DBH (cm)")
