### Lee's trait data from 2020 New Phytologist paper ####

library(tidyverse)
library(ggplot2)
library(MuMIn)
library(lmerTest)
library(DHARMa)

traitdata <-read_csv("Data/anderegg_data_2020_paper_aridity.csv")

#Calculate means per plant
traitdata <- traitdata %>% group_by(Species, Site, Plot, Tree) %>%
  mutate(tree_avg_LMA = mean(LMA, na.rm=T),
         tree_avg_LDMC = mean(LDMC, na.rm=T),
         tree_avg_huber = mean(Al_As, na.rm=T),
         tree_avg_WD = mean(WD, na.rm=T))

#Filter to Tassie only
traitdata_tas <- traitdata %>% filter(Species == "AMYG" | Species == "OBLI" | 
                                        Species == "OVAT" | Species == "VIMI")
#One row per tree
traitdata_tas_onerow <- traitdata_tas %>% group_by(Species, Site, Plot, Tree) %>%
  filter(row_number() == 1)

#Calculate means per species
sp_summary <- traitdata %>% group_by(Species) %>% 
  summarise(sp_avg_LMA = mean(tree_avg_LMA, na.rm=T),
            sp_sd_LMA = sd(tree_avg_LMA, na.rm=T),
         sp_avg_huber = mean(tree_avg_huber, na.rm=T),
         sp_sd_huber = sd(tree_avg_huber, na.rm=T),
         sp_avg_LDMC = mean(tree_avg_LDMC, na.rm=T),
         sp_sd_LDMC = sd(tree_avg_LDMC, na.rm=T),
         sp_avg_WD = mean(tree_avg_WD, na.rm=T),
         sp_sd_WD = sd(tree_avg_WD, na.rm=T))
#Filter to just Tassie species
sp_summary_tas <- sp_summary %>% filter(Species == "AMYG" | Species == "OBLI" | 
                                        Species == "OVAT" | Species == "VIMI")

#LMA
ggplot(sp_summary_tas, aes(x = Species, y = sp_avg_LMA))+
  geom_point()+
  geom_errorbar(aes(ymin = sp_avg_LMA-sp_sd_LMA, ymax = sp_avg_LMA+sp_sd_LMA, width = 0.3), cex=1)+
  theme_classic()

#LDMC
ggplot(sp_summary_tas, aes(x = Species, y = sp_avg_LDMC))+
  geom_point()+
  geom_errorbar(aes(ymin = sp_avg_LDMC-sp_sd_LDMC, ymax = sp_avg_LDMC+sp_sd_LDMC, width = 0.3), cex=1)+
  theme_classic()

#Huber
ggplot(sp_summary_tas, aes(x = Species, y = sp_avg_huber))+
  geom_point()+
  geom_errorbar(aes(ymin = sp_avg_huber-sp_sd_huber, ymax = sp_avg_huber+sp_sd_huber, width = 0.3), cex=1)+
  theme_classic()

#WD
ggplot(sp_summary_tas, aes(x = Species, y = sp_avg_WD))+
  geom_point()+
  geom_errorbar(aes(ymin = sp_avg_WD-sp_sd_WD, ymax = sp_avg_WD+sp_sd_WD, width = 0.3), cex=1)+
  theme_classic()

## Testing if WD is significantly lower for OBLI
modelwd <- aov(tree_avg_WD ~ Species, traitdata_tas_onerow)
summary(modelwd)
TukeyHSD(modelwd)

##Splitting this data by wet and dry sites and seeing if there are differences/
#evidence for adaptation in traits

#plot MD
hist(traitdata_tas_onerow$MDc)
#Long-term mean MD from my dataset is 309

test <- traitdata_tas_onerow %>% select(Site, Plot, MDc) %>%
  group_by(Site, Plot) %>% filter(row_number() == 1)

mydatatest <- growthnbhdata %>% select(Site, Plot, CMD) %>%
  group_by(Site) %>% filter(row_number() == 1)
#We have the same rank order but different averages
#assigning wet and dry categories
#Lee's average MD is -107, mine is 260

traitdata_tas_onerow$site_climate <- 1 
traitdata_tas_onerow <- within(traitdata_tas_onerow, site_climate[traitdata_tas_onerow$MDc>107] <- 'dry')
traitdata_tas_onerow <- within(traitdata_tas_onerow, site_climate[site_climate == 1] <- 'wet')

#WD
ggplot(traitdata_tas_onerow, aes(x = Species, y = tree_avg_WD, colour=site_climate))+
  geom_boxplot()+
  geom_point(position=position_jitterdodge(), alpha = 0.5)+
  theme_classic()
#Huber
ggplot(traitdata_tas_onerow, aes(x = Species, y = tree_avg_huber, colour=site_climate))+
  geom_boxplot()+
  geom_point(position=position_jitterdodge(), alpha = 0.5)+
  theme_classic()
#LMA
ggplot(traitdata_tas_onerow, aes(x = Species, y = tree_avg_LMA, colour=site_climate))+
  geom_boxplot()+
  geom_point(position=position_jitterdodge(), alpha = 0.5)+
  theme_classic()
#LDMC
ggplot(traitdata_tas_onerow, aes(x = Species, y = tree_avg_LDMC, colour=site_climate))+
  geom_boxplot()+
  geom_point(position=position_jitterdodge(), alpha = 0.5)+
  theme_classic()

  