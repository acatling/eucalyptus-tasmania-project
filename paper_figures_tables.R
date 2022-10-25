### Eucalyptus Growth Tasmania Project 
# Alexandra Catling PhD Research
# Code for reproducing figures

###### Importing data, functions and packages ####
source("data_preparations.R")
source("functions.R")

library(kableExtra)
library(sf)
library(ggrepel)
library(ggfortify)

#### PCA of soil ####
summary(soil_pca)
dev.off()
pdf("Output/soil-pca.pdf")
autoplot(soil_pca, label = TRUE, shape = F,
         loadings = TRUE, loadings.colour = 'slateblue', 
         loadings.label.repel = TRUE, loadings.label.size = 5,
         loadings.label = TRUE, loadings.label.colour = 'slateblue')+
  xlab("PC1 (59.3%)")+
  ylab("PC2 (15.7%)")+
  theme_bw()+
  theme(axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text = element_text(size = 16))
dev.off()

#### Making a map of study sites ####
#Read in the SA2 shapefile downloaded from the ABS
#Data from ABS localities
# https://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/1270.0.55.001July%202016?OpenDocument
#Following this guide: https://medium.com/analytics-vidhya/mapping-australia-in-r-6ce092c48b49
ausplotdata <- read_sf("Data/SA2_2016_AUST.shp")

#filter the Australian SA2 shapefile for only Tas
tasplotdata <- ausplotdata %>% filter(STE_NAME16 == "Tasmania")

#import a shapefile of state boundaries
aus_state_data <- read_sf("Data/STE_2016_AUST.shp")
#filter for Tas
tas_state_data <- aus_state_data %>% filter(STE_NAME16 == "Tasmania")

#make a new dataset of cities in Australia (googled the locations)
tas_cities <- tribble(
  ~city, ~lat, ~long, 
  "Hobart",-42.881520, 147.326839,
  "Launceston", -41.439881, 147.136506)

#convert those two columns to geometry column with the st_as_sf() function. Google Maps uses the coordinate reference system 4326 (the GPS system).
tas_cities_geometry <- tas_cities %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326)

#making dataframe for my study sites
site_coords <- tribble(
  ~site, ~lat, ~long, ~site_climate,
  "EPF", -41.77497, 147.3183, "dry",
  "TMP", -43.144035, 147.962669, "wet",
  "MER", -41.56674,	146.24294, "wet",
  "DOG", -41.5385778,	146.324706, "wet",
  "GRA", -42.55066,	147.45885, "dry",
  "FREY", -41.94839,	148.13788, "dry",
  "BOF", -41.186032,	148.189604, "wet")

### Making a plot of Tas with study sites marked
#Need to convert raster to a dataframe first
rdf <- as.data.frame(Tas_CMD, xy=TRUE)
names(rdf)[3] <- 'CMD'

dev.off()
pdf("Output/Tas_study_sites_map.pdf")
ggplot()+
  geom_raster(data =rdf, aes(x, y, fill = CMD), alpha = 0.8)+
  geom_sf(data=tas_state_data, fill = NA) +
  geom_point(data=site_coords, aes(x = long, y = lat, shape = site_climate), size = 4, pch = 18) +  
  xlab("Longitude")+
  ylab("Latitude") +
  xlim(144.5,148.5)+
  ylim(-43.7, -40.5)+
  theme_classic()+
  theme(legend.position="bottom",
        panel.border = element_rect(colour = "black", fill=NA, size = 1.2),
        legend.key.width=unit(1.5, "cm"),
        legend.title.align = 0.5,
        legend.text = element_text(size = 12),
        legend.title = element_text(size=14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text = element_text(size = 12))+
  scale_fill_viridis_c(option="H", na.value="white", 
                       breaks = c(-1500, -1000, -500, 0, 500),
                       guide = guide_colorbar(frame.colour = "black", frame.linewidth = 1.7, 
                                              title.vjust = 0.8))
dev.off()

###### Research question 1: climate vs competition main models #####
# How does sensitivity of growth to CMD vary with NCI?

####  Main models by species
### Total nci only model:
#growth ~ NCI + long-term climate + climate anomaly + period climate:NCI +
#initial DBH + initial DBH:NCI + initial DBH:period climate + PC1 + (1|Site/Plot/Tree)

### Intra and inter nci model:
#growth ~ NCI intra + NCI inter + long-term climate + period climate + 
#NCI intra*period climate + NCI inter*period climate +
#initial DBH + initial DBH*period climate + initial DBH*NCI intra + 
#initial DBH*NCI inter + (1|Site/Plot/Tree)

### AMYG ####
#norm md: long-term 'norm' md calculated for growing months
#anomaly is period md calculated for growing months - norm_md

amygmod1 <- lmer(sqrt(growth_rate) ~ std_total_nci + std_norm_md + std_anomaly + 
                   std_norm_md:std_total_nci + std_preceding_dbh + std_preceding_dbh:std_total_nci +
                   std_preceding_dbh:std_norm_md + std_PC1 + (1|Site/Plot/Tree), amygdata)
amygmod1dharma <- simulateResiduals(amygmod1)
plot(amygmod1dharma)
#terrible residuals
summary(amygmod1)
vif(amygmod1)
#great
plot(amygmod1)

### OBLI ####
oblimod1 <- lmer(sqrt(growth_rate) ~ std_total_nci + std_norm_md + std_anomaly + 
                   std_norm_md:std_total_nci + std_preceding_dbh + std_preceding_dbh:std_total_nci + 
                   std_preceding_dbh:std_norm_md + std_PC1 + (1|Site/Plot/Tree), oblidata)
oblimod1dharma <- simulateResiduals(oblimod1)
plot(oblimod1dharma)
#great
summary(oblimod1)
vif(oblimod1)
#great

### OVAT ####
ovatmod1 <- lmer(sqrt(growth_rate) ~ std_total_nci + std_norm_md + std_anomaly + 
                   std_norm_md:std_total_nci + std_preceding_dbh + std_preceding_dbh:std_total_nci + 
                   std_preceding_dbh:std_norm_md + std_PC1 + (1|Site/Plot/Tree), ovatdata)
ovatmod1dharma <- simulateResiduals(ovatmod1)
plot(ovatmod1dharma)
#terrible residuals and deviation from normality
#hist(sqrt(ovatdata$growth_rate))
summary(ovatmod1)
vif(ovatmod1)
#great
plot(ovatmod1)
### 
qqp(ranef(ovatmod1)$`Plot:Site`[,1])

### VIMI ####
vimimod1 <- lmer(sqrt(growth_rate) ~ std_total_nci + std_norm_md + std_anomaly + 
                   std_norm_md:std_total_nci + std_preceding_dbh + std_preceding_dbh:std_total_nci + 
                   std_preceding_dbh:std_norm_md + std_PC1 + (1|Site/Plot/Tree), vimidata)
vimimod1dharma <- simulateResiduals(vimimod1)
plot(vimimod1dharma)
#great
summary(vimimod1)
vif(vimimod1)
#great

#### Table of model output ####
## Extracting values for all in a loop
model_list <- list(amygmod1, oblimod1, ovatmod1, vimimod1)
effects = lapply(1:length(model_list), function(x) {
  as.data.frame(coef(summary(model_list[[x]]))) %>% mutate(Species=paste0(x))})
effects_table <- do.call("rbind", effects)

#Make rownames a column 
effects_table <- cbind(Effect = rownames(effects_table), effects_table)
#Remove rownames
rownames(effects_table) <- NULL

#Renaming effects since loop adding values to ends
effects_table$Effect[startsWith(effects_table$Effect, '(Intercept)')] <- 'Intercept'
effects_table$Effect[startsWith(effects_table$Effect, 'std_PC1')] <- 'std_PC1'
effects_table$Effect[startsWith(effects_table$Effect, 'std_norm_md:std_preceding_dbh')] <- 'norm_md:std_preceding_dbh'
effects_table$Effect[startsWith(effects_table$Effect, 'std_norm_md')] <- 'std_norm_md'
effects_table$Effect[startsWith(effects_table$Effect, 'std_anomaly')] <- 'std_anomaly'
effects_table$Effect[startsWith(effects_table$Effect, 'std_preceding_dbh')] <- 'std_preceding_dbh'
effects_table$Effect[startsWith(effects_table$Effect, 'std_total_nci:std_norm_md')] <- 'total_nci:std_norm_md'
effects_table$Effect[startsWith(effects_table$Effect, 'std_total_nci:std_preceding_dbh')] <- 'total_nci:std_preceding_dbh'
effects_table$Effect[startsWith(effects_table$Effect, 'std_total_nci')] <- 'std_total_nci'

effects_table <- within(effects_table, Species[Species == '1'] <- 'Eucalyptus amygdalina')
effects_table <- within(effects_table, Species[Species == '2'] <- 'Eucalyptus obliqua')
effects_table <- within(effects_table, Species[Species == '3'] <- 'Eucalyptus ovata')
effects_table <- within(effects_table, Species[Species == '4'] <- 'Eucalyptus viminalis')
#Renaming columns
effects_table <- effects_table %>% select(Species, Effect, Estimate, 'SE' = 'Std. Error', 'p_value' = 'Pr(>|t|)')

#Making column with Estimate (+/- SE) and p value asterisks all combined
#effects_table$collated <- sprintf("%1.1f ± %1.1f", effects_table$Estimate, effects_table$SE)

#Add column for asterisks based on below function
effects_table <- effects_table %>% mutate(p_asterisks = case_when(p_value >=0.05~"",
                                                                  p_value <0.001~"***",
                                                                  p_value <0.01~"**",
                                                                  p_value <0.05~"*"))
effects_table$collated <- sprintf("%1.3f ± %1.2f%s", effects_table$Estimate, effects_table$SE, effects_table$p_asterisks)

growth_effects_kbl <- effects_table %>% select(Species, Effect, collated)

growth_effects_kbl <- growth_effects_kbl %>% group_by(Species) %>% mutate(row = row_number()) %>%
  pivot_wider(names_from = Species, values_from = collated) %>% select(-row)

#Plotting with kableR
#Marginal r squareds from below table
growth_effects_kbl %>% mutate(Effect = c("Intercept", "Total NCI", "Mean MD", "MD anomaly", "Preceding DBH", "PC1", "Total NCI:Mean MD", "Total NCI:Preceding DBH", "Mean MD:Preceding DBH")) %>%
  kbl(align = 'lcccc', caption = "<b>Table 2</b>. Model output with Estimate ± SE for each species modelled separately. Marginal r-squared values for each model reported. NCI is neighbourhood crowding index from all neighbours.
  Mean MD is long-term mean moisture deficit summed over the growing season. MD anomaly is the difference between mean MD and observed MD. Preceding DBH is the size of each focal stem at the start of each growth period.
  Low values of PC1 represent low soil fertility and conductivity. Asterisks denote significance: * p<0.05, ** p<0.01, *** p<0.001") %>%
  add_header_above(c(" "=1, "R^2=0.20"=1, "R^2=0.21"=1, "R^2=0.25"=1, "R^2=0.19"=1), align = c("l", "c", "c", "c", "c")) %>%
  kable_classic(full_width = T, html_font = "Times", font_size = 12) %>%
  #kable_styling(font_size = 12) %>%
  row_spec(0, italic = T)

#### Table of r squared values from models ####
rsquaredtable <- matrix(ncol=2, nrow = 4)
colnames(rsquaredtable) <- c('Species', 'Marginal R squared')
rsquaredtable <- as.data.frame(rsquaredtable)

rsquaredtable[1,1] <- 'Eucalyptus amygdalina'
rsquaredtable[2,1] <- 'Eucalyptus obliqua'
rsquaredtable[3,1] <- 'Eucalyptus ovata'
rsquaredtable[4,1] <- 'Eucalyptus viminalis'
rsquaredtable[1,2] <- r.squaredGLMM(amygmod1)[1,1]
rsquaredtable[2,2] <- r.squaredGLMM(oblimod1)[1,1]
rsquaredtable[3,2] <- r.squaredGLMM(ovatmod1)[1,1]
rsquaredtable[4,2] <- r.squaredGLMM(vimimod1)[1,1]

rsquaredtable %>% kbl(digits = 2) %>%
  kable_classic(full_width = F, html_font = "Times", font_size = 12)

#### Table of sample sizes and site climate ####
#simpleResTas has annual long-term PPT, PET and MD
#climate_diff has period-level long-term MD and period MD, and anomaly
climatetable <- left_join(climate_diff, simpleResTas)
#Table with period-level long-term MD and actual/period MD and anomaly
climatetable <- climatetable %>% select(Site, Period, PPT, norm_md, monthly_period_md, anomaly)
#pivot wider
periodnormwide <- climatetable %>% pivot_wider(id_cols = Site, names_from = Period, values_from = norm_md)
colnames(periodnormwide) <- c('Site', 'Norm MD 1', 'Norm MD 2')
periodactualwide <- climatetable %>% pivot_wider(id_cols = Site, names_from = Period, values_from = monthly_period_md)
colnames(periodactualwide) <- c('Site', 'Actual MD 1', 'Actual MD 2')
anomalydata <- climate_diff %>% pivot_wider(id_cols = Site, names_from = Period, values_from = anomaly)
colnames(anomalydata) <- c('Site', 'Anomaly 1', 'Anomaly 2')
climatetable2 <- left_join(periodnormwide, periodactualwide)
site_ppt <- climatetable %>% select(Site, PPT)
colnames(site_ppt) <- c('Site', 'Norm PPT')
climatetable2 <- left_join(site_ppt, climatetable2) %>% group_by(Site) %>% filter(row_number() == 1)
climatetable2 <- left_join(climatetable2, anomalydata)
#Rearranging rows by site from driest to wettest
climatetable2 <- climatetable2 %>% arrange(`Norm PPT`)
#Rearranging columns
col_order <- c('Site', 'Norm PPT', 'Norm MD 1', 'Actual MD 1', 'Anomaly 1', 'Norm MD 2', 'Actual MD 2', 'Anomaly 2')
climatetable2 <- climatetable2[, col_order]
climatetable2 <- climatetable2 %>% select(-(`Norm PPT`))
#Can't figure out how to do names like this, problem with duplicated col names
colnames(climatetable2) <- c('Site', 'Mean MD', 'Observed MD', 'Anomaly', 'Mean MD', 'Observed MD', 'Anomaly')

climatetable2 %>% 
  kbl(caption = "<b>Supplementary Information 2</b>. Long-term mean moisture deficit (MD) and observed MD summed by month over the growing period and 
  their difference ('anomaly') at each site by census period (1 or 2). Long-term MD values were downloaded 
  from the Global Aridity and PET Database based on 1970-2000 WorldClim climate averages (Zomer <i>et al.</i> 2022). Observed MD values were downloaded from 
  the Bureau of Meteorology (2022). MER is Mersey River Conservation Area, DOG is Dogs Head Regional Reserve, TMP is Tasman National Park, BOF is Doctors Peak Regional Reserve, 
      EPF is Tom Gibson Nature Reserve, FREY is Apslawn Forest Reserve and GRA is Gravelly Ridge Conservation Area.", digits = 0) %>%
  kable_classic(full_width = T, html_font = "Times") %>%
  add_header_above(c(" " = 1, "Census Period 1" = 3, "Census Period 2" = 3))

## Original table:
samplesizes <- onerowdata %>% group_by(Site, Focal_sp) %>% 
  summarise(number_focals = n())
samplesizes_long <- samplesizes %>% pivot_wider(id_cols = Site, names_from = Focal_sp, values_from = number_focals)
site_climate <- onerowdata %>% group_by(Site, PPT, CMD) %>% 
  select(Site, PPT, CMD, period_rainfall, period_md) %>% filter(row_number() == 1)
site_table <- left_join(site_climate, samplesizes_long)
site_table <- site_table %>% replace(is.na(.), 0)
#Rearranging rows by site from wettest to driest
site_table <- site_table %>% arrange(desc(CMD))
#Renaming column names
colnames(site_table) <- c('Site', 'Mean PPT', "Mean MD", "Period PPT", "Period MD", "E. amygdalina", "E. ovata", "E. viminalis", "E. obliqua")

# site_table %>%
#   kbl(caption = "<b>Supplementary 1</b>. Number of focal trees and climate at each site ordered from driest to wettest based on mean annual moisture deficit (MD). Mean annual precipitation (PPT) and MD downloaded 
#   from the Global Aridity and PET Database (Zomer et al. 2006) based on 1960-1990 WorldClim climate averages (Hijmans et al. 2005). PPT and MD over the study period 
#   downloaded from BOM (ref*). MER is Mersey River Conservation Area, DOG is Dogs Head Regional Reserve, TMP is Tasman National Park, BOF is Doctors Peak Regional Reserve, 
#       EPF is Tom Gibson Nature Reserve, FREY is Apslawn Forest Reserve and GRA is Gravelly Ridge Conservation Area.", digits = 0) %>%
#   kable_classic(full_width = F, html_font = "Times") %>%
#   row_spec(0, italic = T) %>%
#   add_header_above(c(" " = 5, "Number of focal trees" = 4))

####  Table with just norm PPT and sample sizes
site_table2 <- left_join(site_ppt, samplesizes_long) %>% replace(is.na(.), 0) %>% 
  group_by(Site) %>% filter(row_number() == 1)
site_table2 <- site_table2 %>% arrange(`Norm PPT`)
colnames(site_table2) <- c('Site', 'PPT', "E. amygdalina", "E. ovata", "E. viminalis", "E. obliqua")

site_table2 %>%
  kbl(caption = "<b>Table 1</b>. Number of focal trees and mean annual precipitation (PPT) at each site. PPT values were downloaded 
  from the Global Aridity and PET Database based on 1970-2000 WorldClim climate averages (Zomer <i>et al.</i> 2022). 
  MER is Mersey River Conservation Area, DOG is Dogs Head Regional Reserve, TMP is Tasman National Park, BOF is Doctors Peak Regional Reserve, 
      EPF is Tom Gibson Nature Reserve, FREY is Apslawn Forest Reserve and GRA is Gravelly Ridge Conservation Area.", digits = 0) %>%
  kable_classic(full_width = T, html_font = "Times", font_size = 12) %>%
  row_spec(0, italic = T) %>%
  add_header_above(c(" " = 2, "Number of focal trees" = 4))

#### Research question 2: Intraspecific and interspecific NCI models ####
#Not allowing size-effects of intra and interspecific crowding, too many parameters

amygmod2 <- lmer(sqrt(growth_rate) ~ std_intra_nci + std_inter_nci + std_norm_md + 
                   std_anomaly + std_norm_md:std_intra_nci + std_norm_md:std_inter_nci + 
                   std_preceding_dbh + std_PC1 + (1|Site/Plot/Tree), amygdata)
amygmod2dharma <- simulateResiduals(amygmod2)
plot(amygmod2dharma)
#terrible residuals
summary(amygmod2)
vif(amygmod2)
#great

oblimod2 <- lmer(sqrt(growth_rate) ~ std_intra_nci + std_inter_nci + std_norm_md + 
                   std_anomaly + std_norm_md:std_intra_nci + std_norm_md:std_inter_nci + 
                   std_preceding_dbh + std_PC1 + (1|Site/Plot/Tree), oblidata)
oblimod2dharma <- simulateResiduals(oblimod2)
plot(oblimod2dharma)
#great
summary(oblimod2)
vif(oblimod2)
#great

ovatmod2 <- lmer(sqrt(growth_rate) ~ std_intra_nci + std_inter_nci + std_norm_md + 
                   std_anomaly + std_norm_md:std_intra_nci + std_norm_md:std_inter_nci + 
                   std_preceding_dbh + std_PC1 + (1|Site/Plot/Tree), ovatdata)
ovatmod2dharma <- simulateResiduals(ovatmod2)
plot(ovatmod2dharma)
#terrible residuals and deviation - too few data?
summary(ovatmod2)
vif(ovatmod2)
#great
plot(ovatmod2)

vimimod2 <- lmer(sqrt(growth_rate) ~ std_intra_nci + std_inter_nci + std_norm_md + 
                   std_anomaly + std_norm_md:std_intra_nci + std_norm_md:std_inter_nci + 
                   std_preceding_dbh + std_PC1 + (1|Site/Plot/Tree), vimidata)
vimimod2dharma <- simulateResiduals(vimimod2)
plot(vimimod2dharma)
#great
summary(vimimod2)
vif(vimimod2)
#great

#### Table of intra and inter model output ####
## Extracting values for all in a loop
model_list2 <- list(amygmod2, oblimod2, ovatmod2, vimimod2)
effects = lapply(1:length(model_list2), function(x) {
  as.data.frame(coef(summary(model_list2[[x]]))) %>% mutate(Species=paste0(x))})
effects_table <- do.call("rbind", effects)

#Make rownames a column 
effects_table <- cbind(Effect = rownames(effects_table), effects_table)
#Remove rownames
rownames(effects_table) <- NULL

#Renaming effects since loop adding values to ends
effects_table$Effect[startsWith(effects_table$Effect, '(Intercept)')] <- 'Intercept'
effects_table$Effect[startsWith(effects_table$Effect, 'std_PC1')] <- 'std_PC1'
effects_table$Effect[startsWith(effects_table$Effect, 'std_norm_md:std_preceding_dbh')] <- 'norm_md:std_preceding_dbh'
effects_table$Effect[startsWith(effects_table$Effect, 'std_norm_md')] <- 'std_norm_md'
effects_table$Effect[startsWith(effects_table$Effect, 'std_anomaly')] <- 'std_anomaly'
effects_table$Effect[startsWith(effects_table$Effect, 'std_preceding_dbh')] <- 'std_preceding_dbh'
effects_table$Effect[startsWith(effects_table$Effect, 'std_intra_nci:std_norm_md')] <- 'intra_nci:std_norm_md'
effects_table$Effect[startsWith(effects_table$Effect, 'std_inter_nci:std_norm_md')] <- 'inter_nci:std_norm_md'
effects_table$Effect[startsWith(effects_table$Effect, 'std_intra_nci')] <- 'std_intra_nci'
effects_table$Effect[startsWith(effects_table$Effect, 'std_inter_nci')] <- 'std_inter_nci'

effects_table <- within(effects_table, Species[Species == '1'] <- 'Eucalyptus amygdalina')
effects_table <- within(effects_table, Species[Species == '2'] <- 'Eucalyptus obliqua')
effects_table <- within(effects_table, Species[Species == '3'] <- 'Eucalyptus ovata')
effects_table <- within(effects_table, Species[Species == '4'] <- 'Eucalyptus viminalis')
#Renaming columns
effects_table <- effects_table %>% select(Species, Effect, Estimate, 'SE' = 'Std. Error', 'p_value' = 'Pr(>|t|)')

#Making column with Estimate (+/- SE) and p value asterisks all combined
#effects_table$collated <- sprintf("%1.1f ± %1.1f", effects_table$Estimate, effects_table$SE)

#Add column for asterisks based on below function
effects_table <- effects_table %>% mutate(p_asterisks = case_when(p_value >=0.05~"",
                                                                  p_value <0.001~"***",
                                                                  p_value <0.01~"**",
                                                                  p_value <0.05~"*"))
effects_table$collated <- sprintf("%1.3f ± %1.2f%s", effects_table$Estimate, effects_table$SE, effects_table$p_asterisks)

growth_effects_kbl <- effects_table %>% select(Species, Effect, collated)

growth_effects_kbl <- growth_effects_kbl %>% group_by(Species) %>% mutate(row = row_number()) %>%
  pivot_wider(names_from = Species, values_from = collated) %>% select(-row)

#Plotting with kableR
#Marginal r squareds from below table
growth_effects_kbl %>% mutate(Effect = c("Intercept", "Conspecific NCI", "Heterospecific NCI", "Mean MD", "MD anomaly", "Preceding DBH", "PC1", "Conspecific NCI:Mean MD", "Heterospecific NCI:Mean MD")) %>%
  kbl(align = 'lcccc', caption = "<b>Supplementary Information 3</b>. Model output with Estimate ± SE for each species modelled separately. Marginal r-squared values for each model reported. 
  NCI is neighbourhood crowding index calculated separately for conspecific or heterospecific neighbours. Mean MD is long-term mean moisture deficit summed over the growing season. 
  MD anomaly is the difference between mean MD and observed MD. Preceding DBH is the size of each focal stem at the start of each growth period.
  Low values of PC1 represent low soil fertility and conductivity. Asterisks denote significance: * p<0.05, ** p<0.01, *** p<0.001") %>%
  add_header_above(c(" "=1, "R^2=0.15"=1, "R^2=0.18"=1, "R^2=0.26"=1, "R^2=0.20"=1), align = c("l", "c", "c", "c", "c")) %>%
  kable_classic(full_width = T, html_font = "Times", font_size = 12) %>%
  row_spec(0, italic = T)
