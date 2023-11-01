#Mayfield Thesis
#Tasmanian Tree Growth Version 2 (July onwards)
#Tom McMahon 2023

## Script from Tom late August 2023

##### Load in packages ####
library(ggplot2)
library(dplyr)
library(crayon)
library(tidyverse)
#library(ftExtra)
# library(AggregateR) #don't have
library(lme4)
library(MuMIn)
library(lmerTest)
library(emmeans)
library(DHARMa)
library(car)
library(base)
# library(here) #don't have
library(vegan)
library(performance)
library(see)
library(patchwork)
# library(coefplot) #don't have
# library(sjPlot) #error
library(sjlabelled)
library(sjmisc) 
# library(effects) #don't have

###ACCURATE ID"S ONLY IN THIS SECTION - see bottom for all understorey data ####
#Import understorey data CSV (data only for sites with accurate ID's)
understorey <- read_csv("Data/tom_understory_export.csv")

#Create Biomass Column
understorey$biomass_cm2 <- understorey$Height_cm*understorey$Width_cm*understorey$Individuals

#Sum of biomass per focal tree
biomass <- aggregate(x = understorey$biomass_cm2, by = list(understorey$Focal_tree), FUN = sum, na.rm = TRUE)

#Create genus + species column
understorey$spp <- paste(understorey$Genus, understorey$Species, sep = " ", collapse = NULL)

#Species richness per focal tree
rich <- understorey %>% drop_na(spp, Individuals) %>% group_by(Focal_tree, spp) %>% summarise (spp_richness = n()) %>% as.data.frame(rich)
presence <- table(rich$spp, rich$Focal_tree)
#sum of each column to get spp. richness
plotrich <- colSums(presence)

#Import CLEANED growth data
growth <- read_csv("Data/tom_growth_data_excluding_climate.csv")

#Changing names in growth data to match mine
growth$Focal_sp <- replace(growth$Focal_sp, growth$Focal_sp == "OVAT", "OVA")
growth$Focal_sp <- replace(growth$Focal_sp, growth$Focal_sp == "VIMI", "VIM")
growth$Focal_sp <- replace(growth$Focal_sp, growth$Focal_sp == "OBLI", "OBQ")                                                         
growth$Site <- replace(growth$Site, growth$Site == "TMP", "TNP")                                                           
growth$Site <- replace(growth$Site, growth$Site == "GRA", "GR")                                                               
growth$Focal_tree <- paste(growth$Site,growth$Focal_sp,growth$Plot,growth$Tree, sep = "")

#Create growth v spp. richness data frame
plotrichdf <- data.frame(plotrich)
plotrichdf$Focal_tree <- row.names(plotrichdf)
#So now I've got a df with richness and one with growth
#Combine the two
#AC: Need to check that merge doesn't lose data
# test <- anti_join(plotrichdf, growth)
# There are 11 focal trees that don't match: DOGOBQB2, DOGOBQD7, EPFOVAB1,
# EPFOVAC2, EPFVIMB1, EPFVIMC4, GROVAA3, GROVAA4, GRVIMB3, TNPOBQA6, TNPOBQB2
# Need to figure out why. Currently they are not included **

#left_join (Tidyverse)
#newdataframe <- left_join(dataa, datab, by='Focal_tree')
#anti_join (Tidyverse)

growthrich <- merge(plotrichdf, growth, on='Focal_tree')

#Creating a site column in growthrich data frame
sitesdf <- data.frame(understorey$Focal_tree, understorey$Site, understorey$Focal_sp, understorey$Plot)
sitesdf <- sitesdf %>% rename(Focal_tree = understorey.Focal_tree)
sitesdf <- unique(sitesdf)
## Below merge is losing 13 focal trees. Need to figure out why**
# Same as 11 above plus GRAMYGB1 and an NA?!
growthrich <- merge(sitesdf, growthrich, on= 'Focal_tree')

#Create growth v biomass data frame
biomass = biomass[-1,]
biomass$Focal_tree = NULL
colnames(biomass)[1] ="Focal_tree"
#So now I've got a df with biomass and one with growth
#Combine the two
## 11 from above not merging **
growthbiomass <- merge(biomass, growth, on='Focal_tree')
colnames(growthbiomass)[2] ="Biomass"

##### Splitting data into groups ####
#Family richness per focal tree
famrich <- understorey %>% drop_na(Family, Individuals) %>% group_by(Focal_tree, Family) %>% summarise (family_richness = n()) %>% as.data.frame(famrich)
famrich = famrich[-1,]

#Creating a growth form and N indicator group

#Sum of family biomass per focal tree
fambiomass = aggregate(biomass_cm2 ~ Focal_tree + Family, data = understorey, sum, na.rm=TRUE, na.action=NULL)
fambiomass = fambiomass[-c(1,506),]
test <- aggregate(x = understorey$biomass_cm2, by = list(understorey$Focal_tree, understorey$Family), FUN = sum, na.rm = TRUE)

#Splitting data into only nitrogen families
faboideaebiomass <- fambiomass %>% filter(Family == 'Fabaceae - Faboideae')
mimosoieaebiomass <- fambiomass %>% filter(Family == 'Fabaceae - Mimosoideae')
rhamnaceaebiomass <- fambiomass %>% filter(Family == 'Rhamnaceae')
noNbiomass <- fambiomass %>% filter(Family == '')
noNbiomass = noNbiomass[-c(1),]

#Joining into one nitrogen fixating group
nitrogenbiomass <- rbind(faboideaebiomass, mimosoieaebiomass, rhamnaceaebiomass, noNbiomass)
nitrogenbiomass <- aggregate(x = nitrogenbiomass$biomass_cm2, by = list(nitrogenbiomass$Focal_tree), FUN = sum, na.rm = TRUE)
colnames(nitrogenbiomass)[1] ="Focal_tree"
colnames(nitrogenbiomass)[2] ="Nitrogen_biomass"

#merging nitrogen fixing df and annual growth
#AC* - Lose 5 rows from the merge, EPFOVAB1, #EPFOVAC2, EPFVIMB1, EPFVIMC4, GRVIMB3
#Replacing below line with a left_join instead
#growthfambiomass <- merge(nitrogenbiomass, growth, on='Focal_tree')
growthfambiomass <- left_join(growth, nitrogenbiomass, by='Focal_tree')

#Now - to do the same for other "groups" of understorey plants
#Ie. Shrubs, heaths, ferns, graminoids

#Split data into families
Alstroemeriaceaebiomass <- fambiomass %>% filter(Family == 'Alstroemeriaceae')
Asphodelaceaebiomass <- fambiomass %>% filter(Family == 'Asphodelaceae')
Asteraceaebiomass <- fambiomass %>% filter(Family == 'Asteraceae')

Blechnaceaebiomass <- fambiomass %>% filter(Family == 'Blechnaceae')
Campanulaceaebiomass <- fambiomass %>% filter(Family == 'Campanulaceae')
Cyperaceaebiomass <- fambiomass %>% filter(Family == 'Cyperaceae')

Dennstaedtiaceaebiomass <- fambiomass %>% filter(Family == 'Dennstaedtiaceae')
Dilleniaceaebiomass <- fambiomass %>% filter(Family == 'Dilleniaceae')
Dryopteridaceaebiomass <- fambiomass %>% filter(Family == 'Dryopteridaceae')

Ericaceaebiomass <- fambiomass %>% filter(Family == 'Ericaceae')
Gleicheniaceaebiomass <- fambiomass %>% filter(Family == 'Gleicheniaceae')
Hypericaceaebiomass <- fambiomass %>% filter(Family == 'Asteraceae')

Laxmaniaceaebiomass <- fambiomass %>% filter(Family == 'Laxmaniaceae')
Lycopodiaceaebiomass <- fambiomass %>% filter(Family == 'Lycopodiaceae')
Myrtaceaebiomass <- fambiomass %>% filter(Family == 'Myrtaceae')

Pittosporaceaebiomass <- fambiomass %>% filter(Family == 'Pittosporaceae')
Plantaginaceaebiomass <- fambiomass %>% filter(Family == 'Plantaginaceae')
Poaceaebiomass <- fambiomass %>% filter(Family == 'Poaceae')

Proteaceaebiomass <- fambiomass %>% filter(Family == 'Proteaceae')
Rosaceaebiomass <- fambiomass %>% filter(Family == 'Rosaceae')
Thymelaeaceaebiomass <- fambiomass %>% filter(Family == 'Thymelaeaceae')


#Joining into plant form groups
shrubbiomass <- rbind(Myrtaceaebiomass, Pittosporaceaebiomass, Proteaceaebiomass, Thymelaeaceaebiomass, noNbiomass)
forbbiomass <- rbind(Alstroemeriaceaebiomass, Asteraceaebiomass, Campanulaceaebiomass, Dilleniaceaebiomass, Hypericaceaebiomass, Lycopodiaceaebiomass, Plantaginaceaebiomass, Rosaceaebiomass, noNbiomass)
graminoidbiomass <- rbind(Asphodelaceaebiomass, Cyperaceaebiomass, Laxmaniaceaebiomass, Poaceaebiomass, noNbiomass)
heathbiomass <- rbind(Ericaceaebiomass, noNbiomass)
fernbiomass <- rbind(Blechnaceaebiomass, Dryopteridaceaebiomass, Gleicheniaceaebiomass, Dennstaedtiaceaebiomass, noNbiomass)

#Cleaning up plant form data frames
shrubbiomass <- aggregate(x = shrubbiomass$biomass_cm2, by = list(shrubbiomass$Focal_tree), FUN = sum, na.rm = TRUE)
colnames(shrubbiomass)[1] ="Focal_tree"
colnames(shrubbiomass)[2] ="Shrub_biomass"

forbbiomass <- aggregate(x = forbbiomass$biomass_cm2, by = list(forbbiomass$Focal_tree), FUN = sum, na.rm = TRUE)
colnames(forbbiomass)[1] ="Focal_tree"
colnames(forbbiomass)[2] ="Forb_biomass"

graminoidbiomass <- aggregate(x = graminoidbiomass$biomass_cm2, by = list(graminoidbiomass$Focal_tree), FUN = sum, na.rm = TRUE)
colnames(graminoidbiomass)[1] ="Focal_tree"
colnames(graminoidbiomass)[2] ="Graminoid_biomass"

heathbiomass <- aggregate(x = heathbiomass$biomass_cm2, by = list(heathbiomass$Focal_tree), FUN = sum, na.rm = TRUE)
colnames(heathbiomass)[1] ="Focal_tree"
colnames(heathbiomass)[2] ="Heath_biomass"

fernbiomass <- aggregate(x = fernbiomass$biomass_cm2, by = list(fernbiomass$Focal_tree), FUN = sum, na.rm = TRUE)
colnames(fernbiomass)[1] ="Focal_tree"
colnames(fernbiomass)[2] ="Fern_biomass"

#AC - Why do so many of these rows have 0 biomass? From all form dataframes
# but still a subset of the total # of trees
#Maybe it's where something was recorded by not measured? Eg. tiny grass (or accidentally)
#Yes, that is fine

#merging plant form dfs and annual growth
#AC - below doesn't work because we end up with 0 rows
#We need to left_join to add additional info the original list of trees with growth rates
#Replacing below rows with left_joins 
# growthformbiomass <- merge(growthfambiomass, shrubbiomass, on='Focal_tree')
# growthformbiomass <- merge(growthformbiomass, forbbiomass, on='Focal_tree')
# growthformbiomass <- merge(growthformbiomass, graminoidbiomass, on='Focal_tree')
# growthformbiomass <- merge(growthformbiomass, heathbiomass, on='Focal_tree')
# growthformbiomass <- merge(growthformbiomass, fernbiomass, on='Focal_tree')
growthformbiomass <- left_join(growthfambiomass, shrubbiomass, by='Focal_tree')
growthformbiomass <- left_join(growthformbiomass, forbbiomass, by='Focal_tree')
growthformbiomass <- left_join(growthformbiomass, graminoidbiomass, by='Focal_tree')
growthformbiomass <- left_join(growthformbiomass, heathbiomass, by='Focal_tree')
growthformbiomass <- left_join(growthformbiomass, fernbiomass, by='Focal_tree')

#growthformbiomass contains annual growth and form biomass of each focal tree

#AC - NOTE we also need to changes NAs to 0 for modelling**
growthformbiomass <- within(growthformbiomass, Nitrogen_biomass[is.na(Nitrogen_biomass)] <- '0')
growthformbiomass <- within(growthformbiomass, Shrub_biomass[is.na(Shrub_biomass)] <- '0')
growthformbiomass <- within(growthformbiomass, Forb_biomass[is.na(Forb_biomass)] <- '0')
growthformbiomass <- within(growthformbiomass, Graminoid_biomass[is.na(Graminoid_biomass)] <- '0')
growthformbiomass <- within(growthformbiomass, Heath_biomass[is.na(Heath_biomass)] <- '0')
growthformbiomass <- within(growthformbiomass, Fern_biomass[is.na(Fern_biomass)] <- '0')

#merge growthformbiomass with richness and total biomass
colnames(biomass)[2] ="Total_biomass"
#AC- again changing these to left_joins
#growthformbiomass <- merge(growthformbiomass, biomass, on='Focal_tree')
#growthformbiomass <- merge(growthformbiomass, plotrichdf, on='Focal_tree')
growthformbiomass <- left_join(growthformbiomass, biomass, by='Focal_tree')
growthformbiomass <- left_join(growthformbiomass, plotrichdf, by='Focal_tree')

#growthformbiomass now contains annual growth, total biomass, richness and form biomass of each focal tree

#Now to split the "growthformbiomass" data set into each focal species so we can run separate models for them
amygdalinadf <- growthformbiomass %>% filter(Focal_sp == 'AMYG')
obliquadf <- growthformbiomass %>% filter(Focal_sp == 'OBQ')
ovatadf <- growthformbiomass %>% filter(Focal_sp == 'OVA')
viminalisdf <- growthformbiomass %>% filter(Focal_sp == 'VIM')

#Import soil and canopy data into workspace
soilcanopy <- read_csv("Data/tom_canopy_soil.csv")
#AC - colour did not import properly

#Merge soilcanopy with complete biomass and richness data
#AC - ALLgrowthrich does not exist, assuming growthformbiomass
#Need to left_join but should select relevant columns first and rename them
#found it below, moving this further down
## TO DO** these all exist, but out of order. Fix*
#soilcanopy <- merge(ALLgrowthrich, soilcanopy, on='Focal_tree')

###### Complete understorey data #####

#Import CLEANED growth data
growth <- read_csv("Data/tom_growth_data_excluding_climate.csv")

#Changing names in growth data to match mine
growth$Focal_sp <- replace(growth$Focal_sp, growth$Focal_sp == "OVAT", "OVA")
growth$Focal_sp <- replace(growth$Focal_sp, growth$Focal_sp == "VIMI", "VIM")
growth$Focal_sp <- replace(growth$Focal_sp, growth$Focal_sp == "OBLI", "OBQ")                                                         
growth$Site <- replace(growth$Site, growth$Site == "TMP", "TNP")                                                           
growth$Site <- replace(growth$Site, growth$Site == "GRA", "GR")                                                               
growth$Focal_tree <- paste(growth$Site,growth$Focal_sp,growth$Plot,growth$Tree, sep = "")

#Create growth v spp. richness data frame
ALLplotrichdf <- data.frame(ALLplotrich)
plotrichdf$Focal_tree <- row.names(plotrichdf)
#So now I've got a df with richness and one with growth
#Combine the two - need to make ALLplotrich have a focal_tree column
ALLplotrichdf$Focal_tree <- row.names(ALLplotrichdf)
#AC - merging. ALLplotrich: 271 obs. growth: 916 obs. Merged ALLgrowthrich: 780 obs.
# To fix*
ALLgrowthrich <- merge(ALLplotrichdf, growth, on='Focal_tree')

#Creating a site column in ALLgrowthrich data frame
ALLsitesdf <- data.frame(ALLunderstorey$Focal_tree, ALLunderstorey$Site, ALLunderstorey$Focal_sp, ALLunderstorey$Plot)
ALLsitesdf <- ALLsitesdf %>% rename(Focal_tree = ALLunderstorey.Focal_tree)
ALLsitesdf <- unique(ALLsitesdf)
ALLgrowthrich <- merge(ALLsitesdf, ALLgrowthrich, on= 'Focal_tree')

#Create growth v biomass data frame
ALLbiomass$Focal_tree = NULL
colnames(ALLbiomass)[1] ="Focal_tree"
#So now I've got a df with biomass and one with growth
#Combine the two
ALLgrowthbiomass <- merge(ALLbiomass, growth, on='Focal_tree')
colnames(ALLgrowthbiomass)[2] ="Biomass"

#Split richness and biomass data frames into each focal species
#But first add richness and biomass into one df
ALLrichbiomass <- merge(ALLplotrichdf, ALLgrowthbiomass, on='Focal_tree')
ALLamygdalinadf <- ALLrichbiomass %>% filter(Focal_sp == 'AMYG')
ALLobliquadf <- ALLrichbiomass %>% filter(Focal_sp == 'OBQ')
ALLovatadf <- ALLrichbiomass %>% filter(Focal_sp == 'OVA')
ALLviminalisdf <- ALLrichbiomass %>% filter(Focal_sp == 'VIM')

#Import understorey data CSV (ALL data)
ALLunderstorey <- read_csv("Data/tom_all_data.csv")

#Create Biomass Column
ALLunderstorey$biomass_cm2 <- ALLunderstorey$Height_cm*ALLunderstorey$Width_cm*ALLunderstorey$Individuals

#Sum of biomass per focal tree
ALLbiomass <- aggregate(x = ALLunderstorey$biomass_cm2, by = list(ALLunderstorey$Focal_tree), FUN = sum, na.rm = TRUE)

#Create genus + species column
ALLunderstorey$spp <- paste(ALLunderstorey$Genus, ALLunderstorey$Species, sep = " ", collapse = NULL)

#Species richness per focal tree
ALLrich <- ALLunderstorey %>% drop_na(spp, Individuals) %>% group_by(Focal_tree, spp) %>% summarise (spp_richness = n()) %>% as.data.frame(ALLrich)
ALLpresence <- table(ALLrich$spp, ALLrich$Focal_tree)
#sum of each column to get spp. richness
ALLplotrich <- colSums(ALLpresence)

#Shannon's Index (note difference in capitals)
ALLshannon <- merge(ALLrich, ALLgrowthrich, on='Focal_tree')
ALLShannon <- subset(ALLshannon, Period < 2)
ALLShannon <- ALLShannon[,c('Focal_tree','spp','spp_richness','ALLplotrich')]

ALLabundance <- aggregate(x = ALLShannon$spp_richness, by = list(ALLShannon$Focal_tree), FUN = sum, na.rm = TRUE)
colnames(ALLabundance)[1] ="Focal_tree"
colnames(ALLabundance)[2] ="Abundance"
ALLShannon <- merge(ALLShannon, ALLabundance, on='Focal_tree')

ALLShannon$Pi <- ALLShannon$spp_richness/ALLShannon$Abundance
ALLShannon$ln_Pi <- log(ALLShannon$Pi)
ALLShannon$Pixln_Pi <- ALLShannon$Pi * ALLShannon$ln_Pi
#Shannon index per focal tree (ie. (sum of Pi*ln(Pi)) * -1 )
Shannon <- aggregate(x = ALLShannon$Pixln_Pi, by = list(ALLShannon$Focal_tree), FUN = sum, na.rm = TRUE)
colnames(Shannon)[1] ="Focal_tree"
colnames(Shannon)[2] ="NEGShannon"
Shannon$Shannon = Shannon$NEGShannon * -1

##### MODELLING ##########
#####################################################################################################################

#Growth ~ Richness (SIG)
ggplot(ALLgrowthrich, aes(x = ALLplotrich, y = sqrt(growth_rate))) + 
  labs(x = "Neighbourhood Species Richness", y = "Squareroot [Annual Focal Tree Growth Rate (mm/day)]") + 
  geom_smooth(method = "lm") +
  geom_point(alpha = 0.2) + 
  theme_classic()
#Fit the model
lmALLgrowthrich <- lmer(sqrt(growth_rate) ~ sqrt(ALLplotrich) + (1|Site/Focal_sp), data = ALLgrowthrich)
#AC - separate random effects of site and Focal species. And plot nested within site
# And tree nested within plot bcause we hav repeated measures (3x growth per tree), i.=e:
lmALLgrowthrichAC <- lmer(sqrt(growth_rate) ~ sqrt(ALLplotrich) + (1|Site/Plot/Tree) + (1|Focal_sp), data = ALLgrowthrich)
summary(lmALLgrowthrichAC)
#
anova(lmALLgrowthrich)
check_model(lmALLgrowthrich, check = c("ncv", "outliers", "qq", "normality"))
summary(lmALLgrowthrich)
#Plot the actual model outputs ie. not ggplot
coefplot(lmALLgrowthrich) #NA
plot_model(lmALLgrowthrich) #NA
plot(Effect(c("ALLplotrich"))), lmALLgrowthrich)

#Run a separate model for EACH focal species
lmamygrich <- lmer(sqrt(growth_rate) ~ sqrt(ALLplotrich) + (1|Site/Plot/Tree), data = ALLamygdalinadf)
check_model(lmamygrich, check = c("ncv", "outliers", "qq", "normality"))
#All good
summary(lmamygrich)
lmobliquarich <- lmer(sqrt(growth_rate) ~ sqrt(ALLplotrich) + (1|Site/Plot/Tree), data = ALLobliquadf)
check_model(lmobliquarich, check = c("ncv", "outliers", "qq", "normality"))
#All good
summary(lmobliquarich)
lmovatarich <- lmer(sqrt(growth_rate) ~ sqrt(ALLplotrich) + (1|Site/Plot/Tree), data = ALLovatadf)
check_model(lmovatarich, check = c("ncv", "outliers", "qq", "normality"))
#All good
summary(lmovatarich)
lmviminalisrich <- lmer(sqrt(growth_rate) ~ sqrt(ALLplotrich) + (1|Site/Plot/Tree), data = ALLviminalisdf)
check_model(lmviminalisrich, check = c("ncv", "outliers", "qq", "normality"))
#All good
summary(lmviminalisrich)

#Growth ~ Biomass (SIG)
#Plotting annual growth vs biomass
ggplot(ALLgrowthbiomass, aes(x = sqrt(Biomass), y = sqrt(growth_rate))) + 
  labs(x = "Neighbourhood Biomass (cm2, sqrt)", y = "Mean Annual Focal Tree Growth Rate (mm/year, sqrt)") + 
  geom_smooth(method = "lm") +
  geom_point(alpha = 0.2) + 
  theme_classic()
#Fit the model
lmALLgrowthbiomass <- lmer(sqrt(growth_rate) ~ sqrt(Biomass) + (1|Site/Focal_sp), data = ALLgrowthbiomass)
check_model(lmALLgrowthbiomass, check = c("ncv", "outliers", "qq", "normality"))
summary(lmALLgrowthbiomass)

#Run a separate model for EACH focal species
lmamygbiomass <- lmer(sqrt(growth_rate) ~ sqrt(Biomass) + (1|Site/Plot/Tree), data = ALLamygdalinadf)
check_model(lmamygbiomass, check = c("ncv", "outliers", "qq", "normality"))
#All good
summary(lmamygbiomass)
lmobliquabiomass <- lmer(sqrt(growth_rate) ~ sqrt(Biomass) + (1|Site/Plot/Tree), data = ALLobliquadf)
check_model(lmobliquabiomass, check = c("ncv", "outliers", "qq", "normality"))
#All good
summary(lmobliquabiomass)
lmovatabiomass <- lmer(sqrt(growth_rate) ~ sqrt(Biomass) + (1|Site/Plot/Tree), data = ALLovatadf)
check_model(lmovatabiomass, check = c("ncv", "outliers", "qq", "normality"))
#All good
summary(lmovatabiomass)
lmviminalisbiomass <- lmer(sqrt(growth_rate) ~ sqrt(Biomass) + (1|Site/Plot/Tree), data = ALLviminalisdf)
check_model(lmviminalisbiomass, check = c("ncv", "outliers", "qq", "normality"))
#All good
summary(lmviminalisbiomass)

#Growth ~ Composition (graminoid, heath & fern SIG)
#Plotting each plant form biomass vs annual growth
############################################################################################################################

#Growth ~ Shrub Biomass (VIM IS SIG)
ggplot(growthformbiomass, aes(x = sqrt(Shrub_biomass), y = sqrt(growth_rate), colour = Focal_sp)) + 
  labs(x = expression("Shrub Biomass (cm2, sqrt)"), y = "Mean Annual Focal Tree Growth Rate (mm/day, sqrt)") + 
  geom_smooth(method = "lm") +
  geom_point(alpha = 0.2) + 
  theme_classic()
#Fit the models for each focal species
lmamygshrub <- lmer(sqrt(growth_rate) ~ sqrt(Shrub_biomass) + (1|Site/Plot/Tree), data = amygdalinadf)
check_model(lmamygshrub, check = c("ncv", "outliers", "qq", "normality"))
#All good
summary(lmamygshrub)
lmobliquashrub <- lmer(sqrt(growth_rate) ~ sqrt(Shrub_biomass) + (1|Site/Plot/Tree), data = obliquadf)
check_model(lmobliquashrub, check = c("ncv", "outliers", "qq", "normality"))
#All good
summary(lmobliquashrub)
lmovatashrub <- lmer(sqrt(growth_rate) ~ sqrt(Shrub_biomass) + (1|Site/Plot/Tree), data = ovatadf)
check_model(lmovatashrub, check = c("ncv", "outliers", "qq", "normality"))
#All good
summary(lmovatashrub)
lmviminalisshrub <- lmer(sqrt(growth_rate) ~ sqrt(Shrub_biomass) + (1|Site/Plot/Tree), data = viminalisdf)
check_model(lmviminalisshrub, check = c("ncv", "outliers", "qq", "normality"))
#All good
summary(lmviminalisshrub)

#Not splitting into species
ggplot(growthformbiomass, aes(x = sqrt(Shrub_biomass), y = sqrt(growth_rate))) + 
  labs(x = expression("Shrub Biomass (cm2, sqrt)"), y = "Mean Annual Focal Tree Growth Rate (mm/day, sqrt)") + 
  geom_smooth(method = "lm") +
  geom_point(alpha = 0.2) + 
  theme_classic()
#Fit the model
lmgrowthshrub <- lmer(sqrt(growth_rate) ~ sqrt(Shrub_biomass) + (1|Site/Plot/Tree), data = growthformbiomass)
check_model(lmgrowthshrub, check = c("ncv", "outliers", "qq", "normality"))
#All good
summary(lmgrowthshrub)

############################################################################################################################

#Growth ~ Forb Biomass (NONE SIG)
ggplot(growthformbiomass, aes(x = sqrt(Forb_biomass), y = sqrt(growth_rate), colour = Focal_sp)) + 
  labs(x = "Forb Biomass (cm2, sqrt)", y = "Mean Annual Focal Tree Growth Rate (mm/day, sqrt)") + 
  geom_smooth(method = "lm") +
  geom_point(alpha = 0.2) + 
  theme_classic()
#Fit the models for each focal species
lmamygforb <- lmer(sqrt(growth_rate) ~ sqrt(Forb_biomass) + (1|Site/Plot/Tree), data = amygdalinadf)
check_model(lmamygforb, check = c("ncv", "outliers", "qq", "normality"))
#All good
summary(lmamygforb)
lmobliquaforb <- lmer(sqrt(growth_rate) ~ sqrt(Forb_biomass) + (1|Site/Plot/Tree), data = obliquadf)
check_model(lmobliquaforb, check = c("ncv", "outliers", "qq", "normality"))
#All good
summary(lmobliquaforb)
lmviminalisforb <- lmer(sqrt(growth_rate) ~ sqrt(Forb_biomass) + (1|Site/Plot/Tree), data = viminalisdf)
check_model(lmviminalisforb, check = c("ncv", "outliers", "qq", "normality"))
#All good
summary(lmviminalisforb)
lmovataforb <- lmer(sqrt(growth_rate) ~ sqrt(Forb_biomass) + (1|Site/Plot/Tree), data = ovatadf)
check_model(lmovataforb, check = c("ncv", "outliers", "qq", "normality"))
#All good
summary(lmovataforb)

#Not splitting into species
ggplot(growthformbiomass, aes(x = sqrt(Forb_biomass), y = sqrt(growth_rate))) + 
  labs(x = "Forb Biomass (cm2, sqrt)", y = "Mean Annual Focal Tree Growth Rate (mm/day, sqrt)") + 
  geom_smooth(method = "lm") +
  geom_point(alpha = 0.2) + 
  theme_classic()
#Fit the model
lmgrowthforb <- lmer(sqrt(growth_rate) ~ sqrt(Forb_biomass) + (1|Site/Plot/Tree), data = growthformbiomass)
check_model(lmgrowthforb, check = c("ncv", "outliers", "qq", "normality"))
#All good
summary(lmgrowthforb)
####################################################################################################################################

#Growth ~ Graminoid Biomass (Obliqua is SIG)
ggplot(growthformbiomass, aes(x = sqrt(Graminoid_biomass), y = sqrt(growth_rate), colour = Focal_sp)) + 
  labs(x = "Graminoid Biomass (cm2, sqrt)", y = "Mean Annual Focal Tree Growth Rate (mm/day, sqrt)") + 
  geom_smooth(method = "lm") +
  geom_point(alpha = 0.2) + 
  theme_classic()
#Fit the models for each focal species
lmamyggraminoid <- lmer(sqrt(growth_rate) ~ sqrt(Graminoid_biomass) + (1|Site/Plot/Tree), data = amygdalinadf)
check_model(lmamyggraminoid, check = c("ncv", "outliers", "qq", "normality"))
#All good
summary(lmamyggraminoid)
lmobliquagraminoid <- lmer(sqrt(growth_rate) ~ sqrt(Graminoid_biomass) + (1|Site/Plot/Tree), data = obliquadf)
check_model(lmobliquagraminoid, check = c("ncv", "outliers", "qq", "normality"))
#All good
summary(lmobliquagraminoid)
lmovatagraminoid <- lmer(sqrt(growth_rate) ~ sqrt(Graminoid_biomass) + (1|Site/Plot/Tree), data = ovatadf)
check_model(lmovatagraminoid, check = c("ncv", "outliers", "qq", "normality"))
#All good
summary(lmovatagraminoid)
lmviminalisgraminoid <- lmer(sqrt(growth_rate) ~ sqrt(Graminoid_biomass) + (1|Site/Plot/Tree), data = viminalisdf)
check_model(lmviminalisgraminoid, check = c("ncv", "outliers", "qq", "normality"))
#All good
summary(lmviminalisgraminoid)

#Not splitting into species
ggplot(growthformbiomass, aes(x = sqrt(Graminoid_biomass), y = sqrt(growth_rate))) + 
  labs(x = "Graminoid Biomass (cm2, sqrt)", y = "Mean Annual Focal Tree Growth Rate (mm/day, sqrt)") + 
  geom_smooth(method = "lm") +
  geom_point(alpha = 0.2) + 
  theme_classic()
#Fit the model
lmgrowthgraminoid <- lmer(sqrt(growth_rate) ~ sqrt(Graminoid_biomass) + (1|Site/Plot/Tree), data = growthformbiomass)
check_model(lmgrowthgraminoid, check = c("ncv", "outliers", "qq", "normality"))
#All good
summary(lmgrowthgraminoid)

#################################################################################################################################

#Growth ~ Heath Biomass (None SIG)
ggplot(growthformbiomass, aes(x = sqrt(Heath_biomass), y = sqrt(growth_rate), colour = Focal_sp)) + 
  labs(x = "Heath Biomass (cm2, sqrt)", y = "Mean Annual Focal Tree Growth Rate (mm/day, sqrt)") + 
  geom_smooth(method = "lm") +
  geom_point(alpha = 0.2) + 
  theme_classic()
#Fit the models for each focal species
lmamygheath <- lmer(sqrt(growth_rate) ~ sqrt(Heath_biomass) + (1|Site/Plot/Tree), data = amygdalinadf)
check_model(lmamygheath, check = c("ncv", "outliers", "qq", "normality"))
#All good
summary(lmamygheath)
lmobliquaheath <- lmer(sqrt(growth_rate) ~ sqrt(Heath_biomass) + (1|Site/Plot/Tree), data = obliquadf)
check_model(lmobliquaheath, check = c("ncv", "outliers", "qq", "normality"))
#All good
summary(lmobliquaheath)
lmovataheath <- lmer(sqrt(growth_rate) ~ sqrt(Heath_biomass) + (1|Site/Plot/Tree), data = ovatadf)
check_model(lmovataheath, check = c("ncv", "outliers", "qq", "normality"))
#All good
summary(lmovataheath)
lmviminalisheath <- lmer(sqrt(growth_rate) ~ sqrt(Heath_biomass) + (1|Site/Plot/Tree), data = viminalisdf)
check_model(lmviminalisheath, check = c("ncv", "outliers", "qq", "normality"))
#All good
summary(lmviminalisheath)

#Not splitting into species
ggplot(growthformbiomass, aes(x = sqrt(Heath_biomass), y = sqrt(growth_rate))) + 
  labs(x = "Heath Biomass (cm2, sqrt)", y = "Mean Annual Focal Tree Growth Rate (mm/day, sqrt)") + 
  geom_smooth(method = "lm") +
  geom_point(alpha = 0.2) + 
  theme_classic()
#Fit the model
lmgrowthheath <- lmer(sqrt(growth_rate) ~ sqrt(Heath_biomass) + (1|Site/Plot/Tree), data = growthformbiomass)
check_model(lmgrowthheath, check = c("ncv", "outliers", "qq", "normality"))
#All good
summary(lmgrowthheath)

###########################################################################################################################

#Growth ~ Fern Biomass (AMYG + combined is SIG)
ggplot(growthformbiomass, aes(x = sqrt(Fern_biomass), y = sqrt(growth_rate), colour = Focal_sp)) + 
  labs(x = "Fern Biomass (cm2, sqrt)", y = "Mean Annual Focal Tree Growth Rate (mm/day, sqrt)") + 
  geom_smooth(method = "lm") +
  geom_point(alpha = 0.2) + 
  theme_classic()
#Fit the models for each focal species
lmamygfern <- lmer(sqrt(growth_rate) ~ sqrt(Fern_biomass) + (1|Site/Plot/Tree), data = amygdalinadf)
check_model(lmamygfern, check = c("ncv", "outliers", "qq", "normality"))
#All good
summary(lmamygfern)
lmobliquafern <- lmer(sqrt(growth_rate) ~ sqrt(Fern_biomass) + (1|Site/Plot/Tree), data = obliquadf)
check_model(lmobliquafern, check = c("ncv", "outliers", "qq", "normality"))
#All good
summary(lmobliquafern)
lmovatafern <- lmer(sqrt(growth_rate) ~ sqrt(Fern_biomass) + (1|Site/Plot/Tree), data = ovatadf)
check_model(lmovatafern, check = c("ncv", "outliers", "qq", "normality"))
#All good
summary(lmovatafern)
lmviminalisfern <- lmer(sqrt(growth_rate) ~ sqrt(Fern_biomass) + (1|Site/Plot/Tree), data = viminalisdf)
check_model(lmviminalisfern, check = c("ncv", "outliers", "qq", "normality"))
#All good
summary(lmviminalisfern)

#Not splitting into species
ggplot(growthformbiomass, aes(x = sqrt(Fern_biomass), y = sqrt(growth_rate))) + 
  labs(x = "Fern Biomass (cm2, sqrt)", y = "Mean Annual Focal Tree Growth Rate (mm/day, sqrt)") + 
  geom_smooth(method = "lm") +
  geom_point(alpha = 0.2) + 
  theme_classic()
#Fit the model
lmgrowthfern <- lmer(sqrt(growth_rate) ~ sqrt(Fern_biomass) + (1|Site/Plot/Tree), data = growthformbiomass)
check_model(lmgrowthfern, check = c("ncv", "outliers", "qq", "normality"))
#All good
summary(lmgrowthfern)

#######################################################################################################################################

#Growth ~ Nitrogen Biomass (NOT SIG)
ggplot(growthformbiomass, aes(x = sqrt(Nitrogen_biomass), y = sqrt(growth_rate), colour = Focal_sp)) + 
  labs(x = "Nitrogen Fixating Flora Biomass (cm2, sqrt)", y = "Mean Annual Focal Tree Growth Rate (mm/day, sqrt)") + 
  geom_smooth(method = "lm") +
  geom_point(alpha = 0.2) + 
  theme_classic()
#Fit the model for each focal species
lmamygnitrogen <- lmer(sqrt(growth_rate) ~ sqrt(Nitrogen_biomass) + (1|Site/Plot/Tree), data = amygdalinadf)
check_model(lmamygnitrogen, check = c("ncv", "outliers", "qq", "normality"))
#All good
summary(lmamygnitrogen)
lmobliquanitrogen <- lmer(sqrt(growth_rate) ~ sqrt(Nitrogen_biomass) + (1|Site/Plot/Tree), data = obliquadf)
check_model(lmobliquanitrogen, check = c("ncv", "outliers", "qq", "normality"))
#All good
summary(lmobliquanitrogen)
lmovatanitrogen <- lmer(sqrt(growth_rate) ~ sqrt(Nitrogen_biomass) + (1|Site/Plot/Tree), data = ovatadf)
check_model(lmovatanitrogen, check = c("ncv", "outliers", "qq", "normality"))
#All good
summary(lmovatanitrogen)
lmviminalisnitrogen <- lmer(sqrt(growth_rate) ~ sqrt(Nitrogen_biomass) + (1|Site/Plot/Tree), data = viminalisdf)
check_model(lmviminalisnitrogen, check = c("ncv", "outliers", "qq", "normality"))
#All good
summary(lmviminalisnitrogen)

#Not splitting into species
ggplot(growthformbiomass, aes(x = sqrt(Nitrogen_biomass), y = sqrt(growth_rate))) + 
  labs(x = "Nitrogen Fixating Flora Biomass (cm2, sqrt)", y = "Mean Annual Focal Tree Growth Rate (mm/day, sqrt)") + 
  geom_smooth(method = "lm") +
  geom_point(alpha = 0.2) + 
  theme_classic()
#Fit the model
lmgrowthnitrogen <- lmer(sqrt(growth_rate) ~ sqrt(Nitrogen_biomass) + (1|Site/Plot/Tree), data = growthformbiomass)
check_model(lmgrowthnitrogen, check = c("ncv", "outliers", "qq", "normality"))
#All good
summary(lmgrowthnitrogen)
#####################################################################################################################################

#Growth ~ Soil/Canopy variables

#Growth ~ Canopy Cover
ggplot(soilcanopy, aes(x = sqrt(CC....), y = sqrt(growth_rate), colour = Focal_sp)) + 
  labs(x = "Focal tree canopy cover (%, sqrt)", y = "Mean Annual Focal Tree Growth Rate (mm/day, sqrt)") + 
  geom_smooth(method = "lm") +
  geom_point(alpha = 0.2) + 
  theme_classic()
#Fit the model
lmgrowthCC <- lmer(sqrt(growth_rate) ~ sqrt(CC....) + (1|Site/Plot/Tree), data = soilcanopy)
check_model(lmgrowthCC, check = c("ncv", "outliers", "qq", "normality"))
#All good
summary(lmgrowthCC)

#Growth ~ Soil pH
ggplot(soilcanopy, aes(x = Soil.pH, y = sqrt(growth_rate), colour = Focal_sp)) + 
  labs(x = "Focal tree Soil pH", y = "Mean Annual Focal Tree Growth Rate (mm/day, sqrt)") + 
  geom_smooth(method = "lm") +
  geom_point(alpha = 0.2) + 
  theme_classic()
#Fit the model
lmgrowthpH <- lmer(sqrt(growth_rate) ~ Soil.pH + (1|Site/Plot/Tree), data = soilcanopy)
check_model(lmgrowthpH, check = c("ncv", "outliers", "qq", "normality"))
#All good
summary(lmgrowthpH)

#####################################################################################################################################

#Graminoids ~ Climate

Graminoidclimate <- subset(growthformbiomass, Period < 2)
ggplot(Graminoidclimate, aes(x = site_climate, y = sqrt(Graminoid_biomass))) + 
  labs(x = "Site Climate", y = "Focal Tree Graminoid Biomass (cm2, sqrt)") + 
  geom_col() +
  theme_classic()

ggplot(Graminoidclimate) +
  aes(x = site_climate, y = Graminoid_biomass, color = Focal_sp) +
  geom_jitter(width = 0.15)

#Fit the model (incomplete)
lmGraminoidclimate <- 
  
  
  