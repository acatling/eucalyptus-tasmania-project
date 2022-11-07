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

### How correlated are MD anomaly and long-term mean MD? Only two sites really

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

effects_table <- within(effects_table, Species[Species == '1'] <- 'E. amygdalina')
effects_table <- within(effects_table, Species[Species == '2'] <- 'E. obliqua')
effects_table <- within(effects_table, Species[Species == '3'] <- 'E. ovata')
effects_table <- within(effects_table, Species[Species == '4'] <- 'E. viminalis')
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
#caption: <b>Table 2</b>. Model output with Estimate ± SE (standard error) for each species modelled separately. Marginal R-squared values reported. NCI is neighbourhood crowding index from all neighbours.
# Mean MD is long-term mean moisture deficit summed over the growth period. MD anomaly is the difference between mean MD and observed MD. Preceding DBH is the size of each focal stem at the start of each growth period.
# Low values of PC1 represent low soil fertility and conductivity.
growth_effects_kbl %>% mutate(Effect = c("Intercept", "Total NCI", "Mean MD", "MD anomaly", "Preceding DBH", "PC1", "Total NCI:Mean MD", "Total NCI:Preceding DBH", "Mean MD:Preceding DBH")) %>%
  kbl(align = 'lcccc', caption = "") %>%
  add_header_above(c(" "=1, "R^2=0.20"=1, "R^2=0.21"=1, "R^2=0.25"=1, "R^2=0.19"=1), align = c("l", "c", "c", "c", "c")) %>%
  kable_classic(full_width = T, html_font = "Times", font_size = 12) %>%
  kable_styling(font_size = 14) %>%
  row_spec(0, italic = T) %>%
  footnote(general = "Continuous predictors are scaled to a mean of 0. Mean MD is 308.8 mm per growth period. Mean MD anomaly is -336.6 mm per growth period. Mean preceding DBH is 368.2 mm.", general_title="")
#Can have multiple footnotes with c(1, 2)
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
  kbl(caption = "<b>Supplementary Information 2</b>. Long-term mean moisture deficit (MD) and observed MD summed by month over the growth period and 
  their difference ('anomaly') at each site by growth period (1 or 2). Long-term MD values were downloaded 
  from the Global Aridity and PET Database based on 1970-2000 WorldClim climate averages (Zomer <i>et al.</i> 2022). Observed MD values were downloaded from 
  the Bureau of Meteorology (2022). MER is Mersey River Conservation Area, DOG is Dogs Head Regional Reserve, TMP is Tasman National Park, BOF is Doctors Peak Regional Reserve, 
      EPF is Tom Gibson Nature Reserve, FREY is Apslawn Forest Reserve and GRA is Gravelly Ridge Conservation Area.", digits = 0) %>%
  kable_classic(full_width = T, html_font = "Times") %>%
  add_header_above(c(" " = 1, "Growth Period 1" = 3, "Growth Period 2" = 3))

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
  from the Global Aridity and PET Database based on 1970-2000 WorldClim climate averages (Zomer <i>et al.</i> 2022). Refer to Supp. Info. 2 for site full names.", 
      digits = 0, align = "lccccc") %>%
  kable_classic(full_width = T, html_font = "Times", font_size = 16) %>%
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

effects_table <- within(effects_table, Species[Species == '1'] <- 'E. amygdalina')
effects_table <- within(effects_table, Species[Species == '2'] <- 'E. obliqua')
effects_table <- within(effects_table, Species[Species == '3'] <- 'E. ovata')
effects_table <- within(effects_table, Species[Species == '4'] <- 'E. viminalis')
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
  kbl(align = 'lcccc', caption = "<b>Supplementary Information 3</b>. Model output with Estimate ± SE (standard error) for each species modelled separately. Marginal R-squared values reported. 
  NCI is neighbourhood crowding index for conspecific or heterospecific neighbours. Mean MD is long-term mean moisture deficit summed over the growth period. 
  MD anomaly is the difference between mean MD and observed MD. Preceding DBH is the size of each focal stem at the start of each growth period.
  Low values of PC1 represent low soil fertility and conductivity. Asterisks denote significance: * p<0.05, ** p<0.01, *** p<0.001") %>%
  add_header_above(c(" "=1, "R^2=0.15"=1, "R^2=0.18"=1, "R^2=0.26"=1, "R^2=0.20"=1), align = c("l", "c", "c", "c", "c")) %>%
  kable_classic(full_width = T, html_font = "Times", font_size = 16) %>%
  kable_styling()%>%
  row_spec(0, italic = T) %>%
  footnote(general = "Continuous predictors are scaled to a mean of 0, representing 308.8 mm deficit per growth period for mean MD", general_title="")

#### Panel plot of growth ~ MD anomaly ####
dev.off()
pdf("Output/panel_growth~md_anomaly.pdf", width=21, height=21)
par(mfrow=c(2,2), mar=c(8, 8, 5, 1))
for(i in 1:length(specieslist)){
  plotted.data<-as.data.frame(specieslist[i])
  plot(sqrt(growth_rate) ~ std_anomaly, pch = 19, col = alpha('grey9', 0.3), ylab="", ylim=c(0,0.52), xlim=c(-2.3,2.3), xlab="",  tck=-0.01, cex=3, cex.axis = 3, padj=0.5, plotted.data)
  mtext(paste(letters[i], ")", sep=""), side=2, padj=-9,las=1, cex=4)
  title(main=bquote(italic(.(speciesnamelist[i]))), cex.main=4)
  model<-lmer(sqrt(growth_rate) ~ std_total_nci + std_norm_md + std_anomaly + 
                std_norm_md:std_total_nci + std_preceding_dbh + std_preceding_dbh:std_total_nci +
                std_preceding_dbh:std_norm_md + std_PC1 + (1|Site/Plot/Tree), plotted.data)
  x_to_plot<-seq.func(plotted.data$std_anomaly)
  #mean md - black
  preddata <- with(model, data.frame(1, 0, 0, x_to_plot, 0, 0, 0*0, 0*0, 0*0))
  plotted.pred <- glmm.predict(mod = model, newdat = preddata, se.mult = 1.96, logit_link=FALSE, log_link=FALSE, glmmTMB=FALSE)
  plot.CI.func(x.for.plot = x_to_plot, pred = plotted.pred$y, upper = plotted.pred$upper, lower = plotted.pred$lower, env.colour = "grey1", env.trans = 50, line.colour = "black", line.weight = 2, line.type = 1)
}
mtext("Growth rate (mm/day, sqrt)", side=2, outer=T, cex=3, adj=0.16, line=-4)
mtext("Growth rate (mm/day, sqrt)", side=2, outer=T, cex=3, adj=0.9, line=-4)
mtext("MD anomaly (standardised)", side=1, outer=T, cex=3, adj=0.9, line=-2)
mtext("MD anomaly (standardised)", side=1, outer=T, cex=3, adj=0.2, line=-2)
dev.off()

#### Panel plot of growth ~ NCI given low and high MD ####
#x axis mtext more negative is higher up
#y axis mtext more negative is closer in

### Need this reset function before legend:
reset <- function() {
  par(mfrow=c(1, 1), oma=rep(0, 4), mar=rep(0, 4), new=TRUE)
  plot(0:1, 0:1, type="n", xlab="", ylab="", axes=FALSE)
}

dev.off()
pdf("Output/panel_growth~NCI+norm.pdf", width=21, height=21)
par(oma=c(8,4,3,1), mfrow=c(2,2), mar=c(7, 6, 4, 1))
for(i in 1:length(specieslist)){
  plotted.data<-as.data.frame(specieslist[i])
  plot(sqrt(growth_rate) ~ std_total_nci, pch = 19, col = alpha(ifelse(std_period_md>0, "#CC79A7", "#0072B2"), 0.4), ylab="", ylim=c(0,0.52), xlim=c(-4,2.5), xlab="",  tck=-0.01, cex=3, cex.axis = 3, padj=0.5, plotted.data)
  mtext(paste(letters[i], ")", sep=""), side=2, padj=-8.5,las=1, cex=4)
  title(main=bquote(italic(.(speciesnamelist[i]))), line=2, cex.main=4)
  model<-lmer(sqrt(growth_rate) ~ std_total_nci + std_norm_md + std_anomaly + 
                std_norm_md:std_total_nci + std_preceding_dbh + std_preceding_dbh:std_total_nci +
                std_preceding_dbh:std_norm_md + std_PC1 + (1|Site/Plot/Tree), plotted.data)
  x_to_plot_low<-seq.func(plotted.data$std_total_nci[plotted.data$std_period_md<0])
  #x_to_plot_mean<-seq.func(plotted.data$std_total_nci)
  x_to_plot_high<-seq.func(plotted.data$std_total_nci[plotted.data$std_period_md>0])
  #low md - wet - blue
  preddata <- with(model, data.frame(1, x_to_plot_low, -1, 0, 0, 0, x_to_plot_low*-1, x_to_plot_low*0, -1*0))
  plotted.pred <- glmm.predict(mod = model, newdat = preddata, se.mult = 1.96, logit_link=FALSE, log_link=FALSE, glmmTMB=FALSE)
  plot.CI.func(x.for.plot = x_to_plot_low, pred = plotted.pred$y, upper = plotted.pred$upper, lower = plotted.pred$lower, env.colour = "#0072B2", env.trans = 50, line.colour = "#0072B2", line.weight = 2, line.type = 1)
 #high md - dry - pink
  preddata <- with(model, data.frame(1, x_to_plot_high, 1, 0, 0, 0, x_to_plot_high*1, x_to_plot_high*0, 1*0))
  plotted.pred <- glmm.predict(mod = model, newdat = preddata, se.mult = 1.96, logit_link=FALSE, log_link=FALSE, glmmTMB=FALSE)
  plot.CI.func(x.for.plot = x_to_plot_high, pred = plotted.pred$y, upper = plotted.pred$upper, lower = plotted.pred$lower, env.colour = "#CC79A7", env.trans = 50, line.colour = "#CC79A7", line.weight = 2, line.type = 1)
}
mtext("Growth rate (mm/day, sqrt)", side=2, outer=T, cex=3.8, adj=0.12, line=-2)
mtext("Growth rate (mm/day, sqrt)", side=2, outer=T, cex=3.8, adj=0.94, line=-2)
mtext("NCI (standardised, log)", side=1, outer=T, cex=3.8, adj=0.93, line=-1.9)
mtext("NCI (standardised, log)", side=1, outer=T, cex=3.8, adj=0.15, line=-1.9)
reset()
legend("bottom", title=NULL, horiz=T, legend=c("Low mean MD", "High mean MD"),
       col=c("#0072B2", "#CC79A7"), pch=19, cex=4, bty="n")
dev.off()

### Back-transformed axes
dev.off()
pdf("Output/panel_growth~NCI+norm2.pdf", width=21, height=21)
par(oma=c(8,4,3,1), mfrow=c(2,2), mar=c(7, 6, 4, 1))
for(i in 1:length(specieslist)){
  plotted.data<-as.data.frame(specieslist[i])
  plot(growth_rate ~ std_total_nci, pch = 19, col = alpha(ifelse(std_period_md>0, "#CC79A7", "#0072B2"), 0.4), ylab="", ylim=c(0,0.52), xlim=c(-4,2.5), xlab="",  tck=-0.01, cex=3, cex.axis = 3, padj=0.5, plotted.data)
  mtext(paste(letters[i], ")", sep=""), side=2, padj=-8.5,las=1, cex=4)
  title(main=bquote(italic(.(speciesnamelist[i]))), line=2, cex.main=4)
  model<-lmer(sqrt(growth_rate) ~ std_total_nci + std_norm_md + std_anomaly + 
                std_norm_md:std_total_nci + std_preceding_dbh + std_preceding_dbh:std_total_nci +
                std_preceding_dbh:std_norm_md + std_PC1 + (1|Site/Plot/Tree), plotted.data)
  x_to_plot_low<-seq.func(plotted.data$std_total_nci[plotted.data$std_period_md<0])
  #x_to_plot_mean<-seq.func(plotted.data$std_total_nci)
  x_to_plot_high<-seq.func(plotted.data$std_total_nci[plotted.data$std_period_md>0])
  #low md - wet - blue
  preddata <- with(model, data.frame(1, x_to_plot_low, -1, 0, 0, 0, x_to_plot_low*-1, x_to_plot_low*0, -1*0))
  plotted.pred <- glmm.predict(mod = model, newdat = preddata, se.mult = 1.96, logit_link=FALSE, log_link=FALSE, glmmTMB=FALSE)^2
  plot.CI.func(x.for.plot = x_to_plot_low, pred = plotted.pred$y, upper = plotted.pred$upper, lower = plotted.pred$lower, env.colour = "#0072B2", env.trans = 50, line.colour = "#0072B2", line.weight = 2, line.type = 1)
  #high md - dry - pink
  preddata <- with(model, data.frame(1, x_to_plot_high, 1, 0, 0, 0, x_to_plot_high*1, x_to_plot_high*0, 1*0))
  plotted.pred <- glmm.predict(mod = model, newdat = preddata, se.mult = 1.96, logit_link=FALSE, log_link=FALSE, glmmTMB=FALSE)^2
  plot.CI.func(x.for.plot = x_to_plot_high, pred = plotted.pred$y, upper = plotted.pred$upper, lower = plotted.pred$lower, env.colour = "#CC79A7", env.trans = 50, line.colour = "#CC79A7", line.weight = 2, line.type = 1)
}
mtext("Growth rate (mm/day)", side=2, outer=T, cex=3.8, adj=0.12, line=-2)
mtext("Growth rate (mm/day)", side=2, outer=T, cex=3.8, adj=0.94, line=-2)
mtext("NCI (standardised, log)", side=1, outer=T, cex=3.8, adj=0.93, line=-1.9)
mtext("NCI (standardised, log)", side=1, outer=T, cex=3.8, adj=0.15, line=-1.9)
reset()
legend("bottom", title=NULL, horiz=T, legend=c("Low mean MD", "High mean MD"),
       col=c("#0072B2", "#CC79A7"), pch=19, cex=4, bty="n")
dev.off()

### Simplifying glmm.predict, back-transformed
dev.off()
pdf("Output/panel_growth~NCI+norm3.pdf", width=21, height=21)
par(oma=c(8,4,3,1), mfrow=c(2,2), mar=c(7, 6, 4, 1))
for(i in 1:length(specieslist)){
  plotted.data<-as.data.frame(specieslist[i])
  plot(growth_rate ~ std_total_nci, pch = 19, col = alpha(ifelse(std_period_md>0, "#CC79A7", "#0072B2"), 0.4), ylab="", ylim=c(0,0.26), xlim=c(-4,2.5), xlab="",  tck=-0.01, cex=3, cex.axis = 3, padj=0.5, plotted.data)
  mtext(paste(letters[i], ")", sep=""), side=2, padj=-8.5,las=1, cex=4)
  title(main=bquote(italic(.(speciesnamelist[i]))), line=2, cex.main=4)
  model<-lmer(sqrt(growth_rate) ~ std_total_nci + std_norm_md + std_anomaly + 
                std_norm_md:std_total_nci + std_preceding_dbh + std_preceding_dbh:std_total_nci +
                std_preceding_dbh:std_norm_md + std_PC1 + (1|Site/Plot/Tree), plotted.data)
  x_to_plot_low<-seq.func(plotted.data$std_total_nci[plotted.data$std_period_md<0])
  #x_to_plot_mean<-seq.func(plotted.data$std_total_nci)
  x_to_plot_high<-seq.func(plotted.data$std_total_nci[plotted.data$std_period_md>0])
  #low md - wet - blue
  plotted.pred <- glmm.predict(mod = model, newdat = data.frame(1, x_to_plot_low, -1, 0, 0, 0, x_to_plot_low*-1, x_to_plot_low*0, -1*0), se.mult = 1.96, logit_link=FALSE, log_link=FALSE, glmmTMB=FALSE)^2
  plot.CI.func(x.for.plot = x_to_plot_low, pred = plotted.pred$y, upper = plotted.pred$upper, lower = plotted.pred$lower, env.colour = "#0072B2", env.trans = 50, line.colour = "#0072B2", line.weight = 2, line.type = 1)
  #high md - dry - pink
  plotted.pred <- glmm.predict(mod = model, newdat = data.frame(1, x_to_plot_high, 1, 0, 0, 0, x_to_plot_high*1, x_to_plot_high*0, 1*0), se.mult = 1.96, logit_link=FALSE, log_link=FALSE, glmmTMB=FALSE)^2
  plot.CI.func(x.for.plot = x_to_plot_high, pred = plotted.pred$y, upper = plotted.pred$upper, lower = plotted.pred$lower, env.colour = "#CC79A7", env.trans = 50, line.colour = "#CC79A7", line.weight = 2, line.type = 1)
}
mtext("Growth rate (mm/day)", side=2, outer=T, cex=3.8, adj=0.12, line=-2)
mtext("Growth rate (mm/day)", side=2, outer=T, cex=3.8, adj=0.94, line=-2)
mtext("NCI (standardised, log)", side=1, outer=T, cex=3.8, adj=0.93, line=-1.9)
mtext("NCI (standardised, log)", side=1, outer=T, cex=3.8, adj=0.15, line=-1.9)
reset()
legend("bottom", title=NULL, horiz=T, legend=c("Low mean MD", "High mean MD"),
       col=c("#0072B2", "#CC79A7"), pch=19, cex=4, bty="n")
dev.off()

### Troubleshooting why ovata model predictions don't quite intersect the data
#ovatdata
plot(growth_rate ~ std_total_nci, pch = 19, col = alpha(ifelse(std_period_md>0, "#CC79A7", "#0072B2"), 0.4), 
     ylab="", ylim=c(0,0.26), xlim=c(-4,2.5), xlab="",  tck=-0.01, cex=3, cex.axis = 3, padj=0.5, ovatdata)
model<-lmer(sqrt(growth_rate) ~ std_total_nci + std_norm_md +
              std_norm_md:std_total_nci + std_preceding_dbh + std_preceding_dbh:std_total_nci +
              std_preceding_dbh:std_norm_md + std_PC1 + (1|Site/Plot/Tree), ovatdata)
x_to_plot_high<-seq.func(ovatdata$std_total_nci[ovatdata$std_period_md>0])
plotted.pred <- glmm.predict(mod = model, newdat = data.frame(1, x_to_plot_high, 1, 0, 0, x_to_plot_high*1, x_to_plot_high*0, 1*0), se.mult = 1.96, logit_link=FALSE, log_link=FALSE, glmmTMB=FALSE)^2
plot.CI.func(x.for.plot = x_to_plot_high, pred = plotted.pred$y, upper = plotted.pred$upper, lower = plotted.pred$lower, env.colour = "#CC79A7", env.trans = 50, line.colour = "#CC79A7", line.weight = 2, line.type = 1)
x_to_plot_low<-seq.func(ovatdata$std_total_nci[ovatdata$std_period_md<0])
plotted.pred <- glmm.predict(mod = model, newdat = data.frame(1, x_to_plot_high, -1, 0, 0, x_to_plot_high*-1, x_to_plot_high*0, -1*0), se.mult = 1.96, logit_link=FALSE, log_link=FALSE, glmmTMB=FALSE)^2
plot.CI.func(x.for.plot = x_to_plot_low, pred = plotted.pred$y, upper = plotted.pred$upper, lower = plotted.pred$lower, env.colour = "#0072B2", env.trans = 50, line.colour = "#0072B2", line.weight = 2, line.type = 1)

mean(growthnbhdata$preceding_dbh, na.rm=T)
mean(ovatdata$preceding_dbh, na.rm=T)
mean(growthnbhdata$PC1, na.rm=T)
mean(ovatdata$PC1, na.rm=T)
mean(growthnbhdata$total_nci, na.rm=T)
mean(ovatdata$total_nci, na.rm=T)
mean(growthnbhdata$std_anomaly, na.rm=T)
mean(ovatdata$std_anomaly, na.rm=T)

##It's the intercept that is much lower than others? No...
mean(ovatdata$growth_rate)
mean(amygdata$growth_rate, na.rm=T)
mean(oblidata$growth_rate, na.rm=T)
mean(vimidata$growth_rate)

##Multicollinearity? Yes, some between MD anomaly and norm_md because only 3 sites
ggplot(ovatdata, aes(x= std_anomaly, y=std_norm_md))+
  geom_point(aes(colour = Site))+
  geom_smooth(method="lm")+
  theme_classic()

#### Panel plot of growth ~ MD given low and high NCI ####
dev.off()
pdf("Output/panel_growth~MD+NCI.pdf", width=21, height=21)
par(mfrow=c(2,2), mar=c(8, 8, 5, 1))
for(i in 1:length(specieslist)){
  plotted.data<-as.data.frame(specieslist[i])
  plot(sqrt(growth_rate) ~ std_norm_md, pch = 19, col = alpha(ifelse(std_total_nci>0, "forestgreen", "goldenrod1"), 0.4), ylab="", xlab="",  tck=-0.01, cex=3, cex.axis = 3, padj=0.5, plotted.data)
  mtext(paste(letters[i], ")", sep=""), side=2, padj=-9,las=1, cex=4)
  title(main=bquote(italic(.(speciesnamelist[i]))), cex.main=4)
  model<-lmer(sqrt(growth_rate) ~ std_total_nci + std_norm_md + std_anomaly + 
                std_norm_md:std_total_nci + std_preceding_dbh + std_preceding_dbh:std_total_nci +
                std_preceding_dbh:std_norm_md + std_PC1 + (1|Site/Plot/Tree), plotted.data)
  x_to_plot_low<-seq.func(plotted.data$std_norm_md[plotted.data$std_total_nci<0])
  x_to_plot_high<-seq.func(plotted.data$std_norm_md[plotted.data$std_total_nci>0])
  #low nci - yellow/goldenrod1
  preddata <- with(model, data.frame(1, -1, x_to_plot_low, 0, 0, 0, -1*x_to_plot_low, -1*0, x_to_plot_low*0))
  plotted.pred <- glmm.predict(mod = model, newdat = preddata, se.mult = 1.96, logit_link=FALSE, log_link=FALSE, glmmTMB=FALSE)
  plot.CI.func(x.for.plot = x_to_plot_low, pred = plotted.pred$y, upper = plotted.pred$upper, lower = plotted.pred$lower, env.colour = "goldenrod1", env.trans = 50, line.colour = "goldenrod1", line.weight = 2, line.type = 1)
  #high nci - green
  preddata <- with(model, data.frame(1, 1, x_to_plot_high, 0, 0, 0, 1*x_to_plot_high, 1*0, x_to_plot_high*0))
  plotted.pred <- glmm.predict(mod = model, newdat = preddata, se.mult = 1.96, logit_link=FALSE, log_link=FALSE, glmmTMB=FALSE)
  plot.CI.func(x.for.plot = x_to_plot_high, pred = plotted.pred$y, upper = plotted.pred$upper, lower = plotted.pred$lower, env.colour = "forestgreen", env.trans = 50, line.colour = "forestgreen", line.weight = 2, line.type = 1)
}
mtext("Growth rate (mm/day, sqrt)", side=2, outer=T, cex=4, adj=0.11, line=-4)
mtext("Growth rate (mm/day, sqrt)", side=2, outer=T, cex=4, adj=0.91, line=-4)
mtext("Long-term MD (standardised)", side=1, outer=T, cex=4, adj=0.93, line=-2)
mtext("Long-term MD (standardised)", side=1, outer=T, cex=4, adj=0.15, line=-2)
dev.off()
#### Panel plot of growth ~ con and het neighbourhood crowding ####

dev.off()
pdf("Output/panel_growth~con+het_NCI.pdf", width=21, height=21)
par(oma=c(8,4,3,1), mfrow=c(2,2), mar=c(7, 6, 4, 1))
for(i in 1:length(specieslist)){
  plotted.data<-as.data.frame(specieslist[i])
  plot(sqrt(growth_rate) ~ std_intra_nci, pch = 19, col = alpha("darkgoldenrod2", 0.4), ylab="", ylim=c(0,0.52), xlim=c(-6,2.5), xlab="",  tck=-0.01, cex=3, cex.axis = 3, padj=0.5, plotted.data)
  points(sqrt(growth_rate) ~ std_inter_nci, pch=19, col= alpha("mediumpurple",0.4), cex =3, plotted.data)
  mtext(paste(letters[i], ")", sep=""), side=2, padj=-8.5,las=1, cex=4)
  title(main=bquote(italic(.(speciesnamelist[i]))), line=2, cex.main=4)
  model<-lmer(sqrt(growth_rate) ~ std_intra_nci + std_inter_nci + std_norm_md + 
               std_anomaly + std_norm_md:std_intra_nci + std_norm_md:std_inter_nci + 
               std_preceding_dbh + std_PC1 + (1|Site/Plot/Tree), plotted.data)
  x_to_plot_intra<-seq.func(plotted.data$std_intra_nci)
  x_to_plot_inter<-seq.func(plotted.data$std_inter_nci)
  #intra - pink
  preddata <- with(model, data.frame(1, x_to_plot_intra, 0, 0, 0, 0, 0, x_to_plot_intra*0, 0*0))
  plotted.pred <- glmm.predict(mod = model, newdat = preddata, se.mult = 1.96, logit_link=FALSE, log_link=FALSE, glmmTMB=FALSE)
  plot.CI.func(x.for.plot = x_to_plot_intra, pred = plotted.pred$y, upper = plotted.pred$upper, lower = plotted.pred$lower, env.colour = "darkgoldenrod1", env.trans = 50, line.colour = "darkgoldenrod1", line.weight = 2, line.type = 1)
  #inter - blue
  preddata <- with(model, data.frame(1, 0, x_to_plot_inter, 0, 0, 0, 0, 0*0, x_to_plot_inter*0))
  plotted.pred <- glmm.predict(mod = model, newdat = preddata, se.mult = 1.96, logit_link=FALSE, log_link=FALSE, glmmTMB=FALSE)
  plot.CI.func(x.for.plot = x_to_plot_inter, pred = plotted.pred$y, upper = plotted.pred$upper, lower = plotted.pred$lower, env.colour = "mediumpurple", env.trans = 50, line.colour = "mediumpurple", line.weight = 2, line.type = 1)
}
mtext("Growth rate (mm/day, sqrt)", side=2, outer=T, cex=3.8, adj=0.12, line=-2)
mtext("Growth rate (mm/day, sqrt)", side=2, outer=T, cex=3.8, adj=0.94, line=-2)
mtext("NCI (standardised, log)", side=1, outer=T, cex=3.8, adj=0.93, line=-1.9)
mtext("NCI (standardised, log)", side=1, outer=T, cex=3.8, adj=0.15, line=-1.9)
reset()
legend("bottom", title=NULL, horiz=T, legend=c("Conspecific", "Heterospecific"),
       col=c("darkgoldenrod2", "mediumpurple"), pch=19, cex=4, bty="n")
dev.off()

#### Panel plot of growth ~ focal tree size ####
dev.off()
pdf("Output/panel_growth~size.pdf", width=21, height=21)
par(mfrow=c(2,2), mar=c(8, 8, 5, 1))
for(i in 1:length(specieslist)){
  plotted.data<-as.data.frame(specieslist[i])
  plot(sqrt(growth_rate) ~ std_preceding_dbh, pch = 19, col = alpha('grey9', 0.3), ylab="", ylim=c(0,0.52), xlim=c(-2.2,2.9), xlab="",  tck=-0.01, cex=3, cex.axis = 3, padj=0.5, plotted.data)
  mtext(paste(letters[i], ")", sep=""), side=2, padj=-9,las=1, cex=4)
  title(main=bquote(italic(.(speciesnamelist[i]))), cex.main=4)
  model<-lmer(sqrt(growth_rate) ~ std_total_nci + std_norm_md + std_anomaly + 
                std_norm_md:std_total_nci + std_preceding_dbh + std_preceding_dbh:std_total_nci +
                std_preceding_dbh:std_norm_md + std_PC1 + (1|Site/Plot/Tree), plotted.data)
  x_to_plot<-seq.func(plotted.data$std_preceding_dbh)
  #mean - black
  preddata <- with(model, data.frame(1, 0, 0, 0,x_to_plot, 0, 0*0, 0*x_to_plot, 0*x_to_plot))
  plotted.pred <- glmm.predict(mod = model, newdat = preddata, se.mult = 1.96, logit_link=FALSE, log_link=FALSE, glmmTMB=FALSE)
  plot.CI.func(x.for.plot = x_to_plot, pred = plotted.pred$y, upper = plotted.pred$upper, lower = plotted.pred$lower, env.colour = "grey1", env.trans = 50, line.colour = "black", line.weight = 2, line.type = 1)
}
mtext("Growth rate (mm/day, sqrt)", side=2, outer=T, cex=3, adj=0.16, line=-4)
mtext("Growth rate (mm/day, sqrt)", side=2, outer=T, cex=3, adj=0.9, line=-4)
mtext("Preceding DBH (standardised)", side=1, outer=T, cex=3, adj=0.9, line=-2)
mtext("Preceding DBH (standardised)", side=1, outer=T, cex=3, adj=0.2, line=-2)
dev.off()

##### Panel plot of growth ~ NCI given size ####

dev.off()
pdf("Output/panel_growth~NCI+norm.pdf", width=21, height=21)
par(oma=c(8,4,3,1), mfrow=c(2,2), mar=c(7, 6, 4, 1))
for(i in 1:length(specieslist)){
  plotted.data<-as.data.frame(specieslist[i])
  plot(sqrt(growth_rate) ~ std_total_nci, pch = 19, col = alpha(ifelse(std_period_md>0, "#CC79A7", "#0072B2"), 0.4), ylab="", ylim=c(0,0.52), xlim=c(-4,2.5), xlab="",  tck=-0.01, cex=3, cex.axis = 3, padj=0.5, plotted.data)
  mtext(paste(letters[i], ")", sep=""), side=2, padj=-8.5,las=1, cex=4)
  title(main=bquote(italic(.(speciesnamelist[i]))), line=2, cex.main=4)
  model<-lmer(sqrt(growth_rate) ~ std_total_nci + std_norm_md + std_anomaly + 
                std_norm_md:std_total_nci + std_preceding_dbh + std_preceding_dbh:std_total_nci +
                std_preceding_dbh:std_norm_md + std_PC1 + (1|Site/Plot/Tree), plotted.data)
  x_to_plot_low<-seq.func(plotted.data$std_total_nci[plotted.data$std_period_md<0])
  #x_to_plot_mean<-seq.func(plotted.data$std_total_nci)
  x_to_plot_high<-seq.func(plotted.data$std_total_nci[plotted.data$std_period_md>0])
  #low md - wet - blue
  preddata <- with(model, data.frame(1, x_to_plot_low, -1, 0, 0, 0, x_to_plot_low*-1, x_to_plot_low*0, -1*0))
  plotted.pred <- glmm.predict(mod = model, newdat = preddata, se.mult = 1.96, logit_link=FALSE, log_link=FALSE, glmmTMB=FALSE)
  plot.CI.func(x.for.plot = x_to_plot_low, pred = plotted.pred$y, upper = plotted.pred$upper, lower = plotted.pred$lower, env.colour = "#0072B2", env.trans = 50, line.colour = "#0072B2", line.weight = 2, line.type = 1)
  #high md - dry - pink
  preddata <- with(model, data.frame(1, x_to_plot_high, 1, 0, 0, 0, x_to_plot_high*1, x_to_plot_high*0, 1*0))
  plotted.pred <- glmm.predict(mod = model, newdat = preddata, se.mult = 1.96, logit_link=FALSE, log_link=FALSE, glmmTMB=FALSE)
  plot.CI.func(x.for.plot = x_to_plot_high, pred = plotted.pred$y, upper = plotted.pred$upper, lower = plotted.pred$lower, env.colour = "#CC79A7", env.trans = 50, line.colour = "#CC79A7", line.weight = 2, line.type = 1)
}
mtext("Growth rate (mm/day, sqrt)", side=2, outer=T, cex=3.8, adj=0.12, line=-2)
mtext("Growth rate (mm/day, sqrt)", side=2, outer=T, cex=3.8, adj=0.94, line=-2)
mtext("NCI (standardised, log)", side=1, outer=T, cex=3.8, adj=0.93, line=-1.9)
mtext("NCI (standardised, log)", side=1, outer=T, cex=3.8, adj=0.15, line=-1.9)
reset()
legend("bottom", title=NULL, horiz=T, legend=c("Low mean MD", "High mean MD"),
       col=c("#0072B2", "#CC79A7"), pch=19, cex=4, bty="n")
dev.off()

dev.off()
pdf("Output/panel_growth~NCI+size.pdf", width=21, height=21)
par(oma=c(8,4,3,1), mfrow=c(2,2), mar=c(7, 6, 4, 1))
for(i in 1:length(specieslist)){
  plotted.data<-as.data.frame(specieslist[i])
  plot(sqrt(growth_rate) ~ std_total_nci, pch = 19, col = alpha(ifelse(std_preceding_dbh>0, "#D55E00", "#009E73"), 0.4), ylab="", ylim=c(0,0.52), xlim=c(-4,2.5), xlab="",  tck=-0.01, cex=3, cex.axis = 3, padj=0.5, plotted.data)
  mtext(paste(letters[i], ")", sep=""), side=2, padj=-8.5,las=1, cex=4)
  title(main=bquote(italic(.(speciesnamelist[i]))), line=2, cex.main=4)
  model<-lmer(sqrt(growth_rate) ~ std_total_nci + std_norm_md + std_anomaly + 
                std_norm_md:std_total_nci + std_preceding_dbh + std_preceding_dbh:std_total_nci +
                std_preceding_dbh:std_norm_md + std_PC1 + (1|Site/Plot/Tree), plotted.data)
  x_to_plot_low<-seq.func(plotted.data$std_total_nci[plotted.data$std_preceding_dbh<0])
  x_to_plot_high<-seq.func(plotted.data$std_total_nci[plotted.data$std_preceding_dbh>0])
  #low size - small - blue
  preddata <- with(model, data.frame(1, x_to_plot_low, 0, 0, -1, 0, x_to_plot_low*0, x_to_plot_low*-1, 0*-1))
  plotted.pred <- glmm.predict(mod = model, newdat = preddata, se.mult = 1.96, logit_link=FALSE, log_link=FALSE, glmmTMB=FALSE)
  plot.CI.func(x.for.plot = x_to_plot_low, pred = plotted.pred$y, upper = plotted.pred$upper, lower = plotted.pred$lower, env.colour = "#009E73", env.trans = 50, line.colour = "#009E73", line.weight = 2, line.type = 1)
  #high size - big - pink
  preddata <- with(model, data.frame(1, x_to_plot_high, 0, 0, 1, 0, x_to_plot_high*0, x_to_plot_high*1, 0*1))
  plotted.pred <- glmm.predict(mod = model, newdat = preddata, se.mult = 1.96, logit_link=FALSE, log_link=FALSE, glmmTMB=FALSE)
  plot.CI.func(x.for.plot = x_to_plot_high, pred = plotted.pred$y, upper = plotted.pred$upper, lower = plotted.pred$lower, env.colour = "#D55E00", env.trans = 50, line.colour = "#D55E00", line.weight = 2, line.type = 1)
}
mtext("Growth rate (mm/day, sqrt)", side=2, outer=T, cex=3.8, adj=0.12, line=-2)
mtext("Growth rate (mm/day, sqrt)", side=2, outer=T, cex=3.8, adj=0.94, line=-2)
mtext("NCI (standardised, log)", side=1, outer=T, cex=3.8, adj=0.93, line=-1.9)
mtext("NCI (standardised, log)", side=1, outer=T, cex=3.8, adj=0.15, line=-1.9)
reset()
legend("bottom", title=NULL, horiz=T, legend=c("Low preceding DBH", "High preceding DBH"),
       col=c("#009E73", "#D55E00"), pch=19, cex=4, bty="n")
dev.off()

##### Panel plot of growth ~ size given NCI ####
dev.off()
pdf("Output/panel_growth~size+NCI.pdf", width=21, height=21)
par(mfrow=c(2,2), mar=c(8, 8, 5, 1))
for(i in 1:length(specieslist)){
  plotted.data<-as.data.frame(specieslist[i])
  plot(sqrt(growth_rate) ~ std_preceding_dbh, pch = 19, col = alpha(ifelse(std_total_nci>0, "#CC79A7", "#0072B2"), 0.4), ylab="", ylim=c(0,0.52), xlim=c(-2.2,2.9), xlab="",  tck=-0.01, cex=3, cex.axis = 3, padj=0.5, plotted.data)
  mtext(paste(letters[i], ")", sep=""), side=2, padj=-9,las=1, cex=4)
  title(main=bquote(italic(.(speciesnamelist[i]))), cex.main=4)
  model<-lmer(sqrt(growth_rate) ~ std_total_nci + std_norm_md + std_anomaly + 
                std_norm_md:std_total_nci + std_preceding_dbh + std_preceding_dbh:std_total_nci +
                std_preceding_dbh:std_norm_md + std_PC1 + (1|Site/Plot/Tree), plotted.data)
  x_to_plot_low<-seq.func(plotted.data$std_preceding_dbh[plotted.data$std_total_nci<0])
  x_to_plot_high<-seq.func(plotted.data$std_preceding_dbh[plotted.data$std_total_nci>0])
  #low NCI - small - blue
  preddata <- with(model, data.frame(1, -1, 0, 0,x_to_plot_low, 0, -1*0, -1*x_to_plot_low, 0*x_to_plot_low))
  plotted.pred <- glmm.predict(mod = model, newdat = preddata, se.mult = 1.96, logit_link=FALSE, log_link=FALSE, glmmTMB=FALSE)
  plot.CI.func(x.for.plot = x_to_plot_low, pred = plotted.pred$y, upper = plotted.pred$upper, lower = plotted.pred$lower, env.colour = "#0072B2", env.trans = 50, line.colour = "#0072B2", line.weight = 2, line.type = 1)
  #high NCI - big - pink
  preddata <- with(model, data.frame(1, 1, 0, 0,x_to_plot_high, 0, 1*0, 1*x_to_plot_high, 0*x_to_plot_low))
  plotted.pred <- glmm.predict(mod = model, newdat = preddata, se.mult = 1.96, logit_link=FALSE, log_link=FALSE, glmmTMB=FALSE)
  plot.CI.func(x.for.plot = x_to_plot_high, pred = plotted.pred$y, upper = plotted.pred$upper, lower = plotted.pred$lower, env.colour = "#CC79A7", env.trans = 50, line.colour = "#CC79A7", line.weight = 2, line.type = 1)
}
mtext("Growth rate (mm/day, sqrt)", side=2, outer=T, cex=4, adj=0.12, line=-4)
mtext("Growth rate (mm/day, sqrt)", side=2, outer=T, cex=4, adj=0.92, line=-4)
mtext("Preceding DBH (standardised)", side=1, outer=T, cex=4, adj=0.98, line=-2)
mtext("Preceding DBH (standardised)", side=1, outer=T, cex=4, adj=0.11, line=-2)
dev.off()
#### Panel plot of growth ~ het crowding given MD ####
dev.off()
pdf("Output/panel_growth~het_NCI+norm_MD.pdf", width=21, height=21)
par(oma=c(8,4,3,1), mfrow=c(2,2), mar=c(7, 6, 4, 1))
for(i in 1:length(specieslist)){
  plotted.data<-as.data.frame(specieslist[i])
  plot(sqrt(growth_rate) ~ std_inter_nci, pch = 19, col = alpha(ifelse(std_period_md>0, "#CC79A7", "#0072B2"), 0.4), ylab="", ylim=c(0,0.52), xlim=c(-6,2.5), xlab="",  tck=-0.01, cex=3, cex.axis = 3, padj=0.5, plotted.data)
  mtext(paste(letters[i], ")", sep=""), side=2, padj=-9,las=1, cex=4)
  title(main=bquote(italic(.(speciesnamelist[i]))), cex.main=4)
  model<-lmer(sqrt(growth_rate) ~ std_intra_nci + std_inter_nci + std_norm_md + 
                std_anomaly + std_norm_md:std_intra_nci + std_norm_md:std_inter_nci + 
                std_preceding_dbh + std_PC1 + (1|Site/Plot/Tree), plotted.data)
  x_to_plot_low<-seq.func(plotted.data$std_inter_nci[plotted.data$std_period_md<0])
  x_to_plot_high<-seq.func(plotted.data$std_inter_nci[plotted.data$std_period_md>0])
  #low md - wet - blue
  preddata <- with(model, data.frame(1, 0, x_to_plot_low, -1, 0, 0, 0, 0*-1, x_to_plot_low*-1))
  plotted.pred <- glmm.predict(mod = model, newdat = preddata, se.mult = 1.96, logit_link=FALSE, log_link=FALSE, glmmTMB=FALSE)
  plot.CI.func(x.for.plot = x_to_plot_low, pred = plotted.pred$y, upper = plotted.pred$upper, lower = plotted.pred$lower, env.colour = "#0072B2", env.trans = 50, line.colour = "#0072B2", line.weight = 2)
  #high md - dry - pink
  preddata <- with(model, data.frame(1, 0, x_to_plot_high, 1, 0, 0, 0, 0*1, x_to_plot_high*1))
  plotted.pred <- glmm.predict(mod = model, newdat = preddata, se.mult = 1.96, logit_link=FALSE, log_link=FALSE, glmmTMB=FALSE)
  plot.CI.func(x.for.plot = x_to_plot_high, pred = plotted.pred$y, upper = plotted.pred$upper, lower = plotted.pred$lower, env.colour = "#CC79A7", env.trans = 50, line.colour = "#CC79A7", line.weight = 2)
}
mtext("Growth rate (mm/day, sqrt)", side=2, outer=T, cex=3.8, adj=0.12, line=-2)
mtext("Growth rate (mm/day, sqrt)", side=2, outer=T, cex=3.8, adj=0.94, line=-2)
mtext("Het. NCI (standardised, log+1)", side=1, outer=T, cex=3.8, adj=0.93, line=-1.9)
mtext("Het. NCI (standardised, log+1)", side=1, outer=T, cex=3.8, adj=0.15, line=-1.9)
reset()
legend("bottom", title=NULL, horiz=T, legend=c("Low mean MD", "High mean MD"),
       col=c("#0072B2", "#CC79A7"), pch=19, cex=4, bty="n")
dev.off()

#### Panel plot of growth ~ con crowding given MD ####

dev.off()
pdf("Output/panel_growth~con_NCI+norm_MD.pdf", width=21, height=21)
par(oma=c(8,4,3,1), mfrow=c(2,2), mar=c(7, 6, 4, 1))
for(i in 1:length(specieslist)){
  plotted.data<-as.data.frame(specieslist[i])
  plot(sqrt(growth_rate) ~ std_intra_nci, pch = 19, col = alpha(ifelse(std_period_md>0, "#CC79A7", "#0072B2"), 0.4), ylab="", ylim=c(0,0.52), xlim=c(-6,2.5), xlab="",  tck=-0.01, cex=3, cex.axis = 3, padj=0.5, plotted.data)
  mtext(paste(letters[i], ")", sep=""), side=2, padj=-9,las=1, cex=4)
  title(main=bquote(italic(.(speciesnamelist[i]))), cex.main=4)
  model<-lmer(sqrt(growth_rate) ~ std_intra_nci + std_inter_nci + std_norm_md + 
                std_anomaly + std_norm_md:std_intra_nci + std_norm_md:std_inter_nci + 
                std_preceding_dbh + std_PC1 + (1|Site/Plot/Tree), plotted.data)
  x_to_plot_low<-seq.func(plotted.data$std_intra_nci[plotted.data$std_period_md<0])
  x_to_plot_high<-seq.func(plotted.data$std_intra_nci[plotted.data$std_period_md>0])
  #low md - wet - blue
  preddata <- with(model, data.frame(1, x_to_plot_low, 0, -1, 0, 0, 0, x_to_plot_low*-1, 0*-1))
  plotted.pred <- glmm.predict(mod = model, newdat = preddata, se.mult = 1.96, logit_link=FALSE, log_link=FALSE, glmmTMB=FALSE)
  plot.CI.func(x.for.plot = x_to_plot_low, pred = plotted.pred$y, upper = plotted.pred$upper, lower = plotted.pred$lower, env.colour = "#0072B2", env.trans = 50, line.colour = "#0072B2", line.weight = 2)
  #high md - dry - pink
  preddata <- with(model, data.frame(1, x_to_plot_high, 0, 1, 0, 0, 0, x_to_plot_high*1, 0*1))
  plotted.pred <- glmm.predict(mod = model, newdat = preddata, se.mult = 1.96, logit_link=FALSE, log_link=FALSE, glmmTMB=FALSE)
  plot.CI.func(x.for.plot = x_to_plot_high, pred = plotted.pred$y, upper = plotted.pred$upper, lower = plotted.pred$lower, env.colour = "#CC79A7", env.trans = 50, line.colour = "#CC79A7", line.weight = 2)
}
mtext("Growth rate (mm/day, sqrt)", side=2, outer=T, cex=3.8, adj=0.12, line=-2)
mtext("Growth rate (mm/day, sqrt)", side=2, outer=T, cex=3.8, adj=0.94, line=-2)
mtext("Con. NCI (standardised, log+1)", side=1, outer=T, cex=3.8, adj=0.93, line=-1.9)
mtext("Con. NCI (standardised, log+1)", side=1, outer=T, cex=3.8, adj=0.15, line=-1.9)
reset()
legend("bottom", title=NULL, horiz=T, legend=c("Low mean MD", "High mean MD"),
       col=c("#0072B2", "#CC79A7"), pch=19, cex=4, bty="n")
dev.off()

#### Predictive plots of growth ~ wet and dry ####
#Standard size (mean), standard NCI, standard PC1
#Wet will be 1 sd above long-term MD mean, dry will be 1 sd below

#amyg
model<-lmer(sqrt(growth_rate) ~ std_total_nci + std_norm_md + std_anomaly + 
              std_norm_md:std_total_nci + std_preceding_dbh + std_preceding_dbh:std_total_nci +
              std_preceding_dbh:std_norm_md + std_PC1 + (1|Site/Plot/Tree), amygdata)
## you can make multiple predictions at once - here I made the predictions for md =1 and md=-1
amyg_md_pred<-glmm.predict(mod=model, newdat=data.frame(1, 0, c(1,-1), 0, 0, 0, 0*c(1,-1), 0*0, c(1,-1)*0), 
                                     se.mult=1.96, logit_link=FALSE, log_link=FALSE, glmmTMB=FALSE)^2
## add the md category (like you did)
amyg_md_pred$md_category<-c("dry", "wet")
#add species name
amyg_md_pred$Focal_sp <- 'E. amygdalina'


#obli
model<-lmer(sqrt(growth_rate) ~ std_total_nci + std_norm_md + std_anomaly + 
              std_norm_md:std_total_nci + std_preceding_dbh + std_preceding_dbh:std_total_nci +
              std_preceding_dbh:std_norm_md + std_PC1 + (1|Site/Plot/Tree), oblidata)
obli_md_pred<-glmm.predict(mod=model, newdat=data.frame(1, 0, c(1,-1), 0, 0, 0, 0*c(1,-1), 0*0, c(1,-1)*0), 
                           se.mult=1.96, logit_link=FALSE, log_link=FALSE, glmmTMB=FALSE)^2
obli_md_pred$md_category<-c("dry", "wet")
obli_md_pred$Focal_sp <- 'E. obliqua'

#ovat
model<-lmer(sqrt(growth_rate) ~ std_total_nci + std_norm_md + std_anomaly +
              std_norm_md:std_total_nci + std_preceding_dbh + std_preceding_dbh:std_total_nci +
              std_preceding_dbh:std_norm_md + std_PC1 + (1|Site/Plot/Tree), ovatdata)
ovat_md_pred<-glmm.predict(mod=model, newdat=data.frame(1, 0, c(1,-1), 0, 0, 0, 0*c(1,-1), 0*0, c(1,-1)*0), 
                           se.mult=1.96, logit_link=FALSE, log_link=FALSE, glmmTMB=FALSE)^2
ovat_md_pred$md_category<-c("dry", "wet")
ovat_md_pred$Focal_sp <- 'E. ovata'

#vimi
model<-lmer(sqrt(growth_rate) ~ std_total_nci + std_norm_md + std_anomaly + 
              std_norm_md:std_total_nci + std_preceding_dbh + std_preceding_dbh:std_total_nci +
              std_preceding_dbh:std_norm_md + std_PC1 + (1|Site/Plot/Tree), vimidata)
vimi_md_pred<-glmm.predict(mod=model, newdat=data.frame(1, 0, c(1,-1), 0, 0, 0, 0*c(1,-1), 0*0, c(1,-1)*0), 
                           se.mult=1.96, logit_link=FALSE, log_link=FALSE, glmmTMB=FALSE)^2
vimi_md_pred$md_category<-c("dry", "wet")
vimi_md_pred$Focal_sp <- 'E. viminalis'

#Merge them all
pred_md_all <- rbind(amyg_md_pred, obli_md_pred, ovat_md_pred, vimi_md_pred)

## Plot them
ggplot(pred_md_all, aes(x = Focal_sp, y = y, colour=md_category))+
  geom_point(position = position_dodge(0.8), cex=2.5)+
  geom_errorbar(aes(ymin = lower, ymax = upper, width = 0.3), position = position_dodge(0.8), cex=1)+
  ylab("Growth rate (mm/day)")+
  xlab("Species")+
  theme_classic()+
  my_theme+
  theme(axis.ticks.x = element_blank(),
  axis.text.x = element_text(face = "italic"))

 # theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
#mean(amygdata$growth_rate[amygdata$std_preceding_dbh>0], na.rm=T)

#### Predictive plots of growth ~ high and low NCI ####
#Standard size (mean), standard long-term MD, standard PC1, standard MD anomaly
#High NCI will be 1 sd above total NCI mean, dry will be 1 sd below

#amyg
model<-lmer(sqrt(growth_rate) ~ std_total_nci + std_norm_md + std_anomaly + 
              std_norm_md:std_total_nci + std_preceding_dbh + std_preceding_dbh:std_total_nci +
              std_preceding_dbh:std_norm_md + std_PC1 + (1|Site/Plot/Tree), amygdata)
## you can make multiple predictions at once - here I made the predictions for md =1 and md=-1
amyg_pred_nci<-glmm.predict(mod=model, newdat=data.frame(1, c(1,-1), 0, 0, 0, 0, c(1,-1)*0, c(1,-1)*0, 0*0), 
                               se.mult=1.96, logit_link=FALSE, log_link=FALSE, glmmTMB=FALSE)^2
## add the md category (like you did)
amyg_pred_nci$nci_category<-c("high NCI", "low NCI")
#add species name
amyg_pred_nci$Focal_sp <- 'E. amygdalina'


#obli
model<-lmer(sqrt(growth_rate) ~ std_total_nci + std_norm_md + std_anomaly + 
              std_norm_md:std_total_nci + std_preceding_dbh + std_preceding_dbh:std_total_nci +
              std_preceding_dbh:std_norm_md + std_PC1 + (1|Site/Plot/Tree), oblidata)
obli_pred_nci<-glmm.predict(mod=model, newdat=data.frame(1, c(1,-1), 0, 0, 0, 0, c(1,-1)*0, c(1,-1)*0, 0*0), 
                               se.mult=1.96, logit_link=FALSE, log_link=FALSE, glmmTMB=FALSE)^2
obli_pred_nci$nci_category<-c("high NCI", "low NCI")
obli_pred_nci$Focal_sp <- 'E. obliqua'

#ovat
model<-lmer(sqrt(growth_rate) ~ std_total_nci + std_norm_md + std_anomaly +
              std_norm_md:std_total_nci + std_preceding_dbh + std_preceding_dbh:std_total_nci +
              std_preceding_dbh:std_norm_md + std_PC1 + (1|Site/Plot/Tree), ovatdata)
ovat_pred_nci<-glmm.predict(mod=model, newdat=data.frame(1, c(1,-1), 0, 0, 0, 0, c(1,-1)*0, c(1,-1)*0, 0*0), 
                               se.mult=1.96, logit_link=FALSE, log_link=FALSE, glmmTMB=FALSE)^2
ovat_pred_nci$nci_category<-c("high NCI", "low NCI")
ovat_pred_nci$Focal_sp <- 'E. ovata'

#vimi
model<-lmer(sqrt(growth_rate) ~ std_total_nci + std_norm_md + std_anomaly + 
              std_norm_md:std_total_nci + std_preceding_dbh + std_preceding_dbh:std_total_nci +
              std_preceding_dbh:std_norm_md + std_PC1 + (1|Site/Plot/Tree), vimidata)
vimi_pred_nci<-glmm.predict(mod=model, newdat=data.frame(1, c(1,-1), 0, 0, 0, 0, c(1,-1)*0, c(1,-1)*0, 0*0), 
                               se.mult=1.96, logit_link=FALSE, log_link=FALSE, glmmTMB=FALSE)^2
vimi_pred_nci$nci_category<-c("high NCI", "low NCI")
vimi_pred_nci$Focal_sp <- 'E. viminalis'

#Merge them all
pred_nci_all <- rbind(amyg_pred_nci, obli_pred_nci, ovat_pred_nci, vimi_pred_nci)

## Plot them
ggplot(pred_nci_all, aes(x = Focal_sp, y = y, colour=nci_category))+
  geom_point(position = position_dodge(0.8), cex=2.5)+
  geom_errorbar(aes(ymin = lower, ymax = upper, width = 0.3), position = position_dodge(0.8), cex=1)+
  ylab("Growth rate (mm/day)")+
  xlab("Species")+
  theme_classic()+
  my_theme+
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_text(face = "italic"))
max(ovatdata$growth_rate)

#### Predictive plots of growth ~ small and large size ####
#Standard long-term MD, standard PC1, standard NCI, standard MD anomaly
#High size will be 1 sd above preceding dbh mean, small will be 1 sd below

#amyg
model<-lmer(sqrt(growth_rate) ~ std_total_nci + std_norm_md + std_anomaly + 
              std_norm_md:std_total_nci + std_preceding_dbh + std_preceding_dbh:std_total_nci +
              std_preceding_dbh:std_norm_md + std_PC1 + (1|Site/Plot/Tree), amygdata)
## you can make multiple predictions at once - here I made the predictions for md =1 and md=-1
amyg_pred_dbh<-glmm.predict(mod=model, newdat=data.frame(1, 0, 0, 0, c(1,-1), 0, 0*0, 0*c(1,-1), 0*c(1,-1)), 
                            se.mult=1.96, logit_link=FALSE, log_link=FALSE, glmmTMB=FALSE)^2
## add the md category (like you did)
amyg_pred_dbh$dbh_category<-c("big", "small")
#add species name
amyg_pred_dbh$Focal_sp <- 'E. amygdalina'


#obli
model<-lmer(sqrt(growth_rate) ~ std_total_nci + std_norm_md + std_anomaly + 
              std_norm_md:std_total_nci + std_preceding_dbh + std_preceding_dbh:std_total_nci +
              std_preceding_dbh:std_norm_md + std_PC1 + (1|Site/Plot/Tree), oblidata)
obli_pred_dbh<-glmm.predict(mod=model, newdat=data.frame(1, 0, 0, 0, c(1,-1), 0, 0*0, 0*c(1,-1), 0*c(1,-1)), 
                            se.mult=1.96, logit_link=FALSE, log_link=FALSE, glmmTMB=FALSE)^2
obli_pred_dbh$dbh_category<-c("big", "small")
obli_pred_dbh$Focal_sp <- 'E. obliqua'

#ovat
model<-lmer(sqrt(growth_rate) ~ std_total_nci + std_norm_md + std_anomaly +
              std_norm_md:std_total_nci + std_preceding_dbh + std_preceding_dbh:std_total_nci +
              std_preceding_dbh:std_norm_md + std_PC1 + (1|Site/Plot/Tree), ovatdata)
ovat_pred_dbh<-glmm.predict(mod=model, newdat=data.frame(1, 0, 0, 0, c(1,-1), 0, 0*0, 0*c(1,-1), 0*c(1,-1)), 
                            se.mult=1.96, logit_link=FALSE, log_link=FALSE, glmmTMB=FALSE)^2
ovat_pred_dbh$dbh_category<-c("big", "small")
ovat_pred_dbh$Focal_sp <- 'E. ovata'

#vimi
model<-lmer(sqrt(growth_rate) ~ std_total_nci + std_norm_md + std_anomaly + 
              std_norm_md:std_total_nci + std_preceding_dbh + std_preceding_dbh:std_total_nci +
              std_preceding_dbh:std_norm_md + std_PC1 + (1|Site/Plot/Tree), vimidata)
vimi_pred_dbh<-glmm.predict(mod=model, newdat=data.frame(1, 0, 0, 0, c(1,-1), 0, 0*0, 0*c(1,-1), 0*c(1,-1)), 
                            se.mult=1.96, logit_link=FALSE, log_link=FALSE, glmmTMB=FALSE)^2
vimi_pred_dbh$dbh_category<-c("big", "small")
vimi_pred_dbh$Focal_sp <- 'E. viminalis'

#Merge them all
pred_dbh_all <- rbind(amyg_pred_dbh, obli_pred_dbh, ovat_pred_dbh, vimi_pred_dbh)

## Plot them
ggplot(pred_dbh_all, aes(x = Focal_sp, y = y, colour=dbh_category))+
  geom_point(position = position_dodge(0.8), cex=2.5)+
  geom_errorbar(aes(ymin = lower, ymax = upper, width = 0.3), position = position_dodge(0.8), cex=1)+
  ylab("Growth rate (mm/day)")+
  xlab("Species")+
  theme_classic()+
  my_theme+
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_text(face = "italic"))
max(ovatdata$growth_rate)

#### Plotting predictive plots all together ####
#pred_md_all, pred_nci_all, pred_dbh_all
#md/nci/dbh_category
#growth_rate is just 'y'

###Using base R so I need to assign dummy x values to distribute along x axis
pred_md_all$x <- c(0.5, 1, 2, 2.5, 3.5, 4, 5, 5.5)
pred_nci_all$x <- c(0.5, 1, 2, 2.5, 3.5, 4, 5, 5.5)
pred_dbh_all$x <- c(0.5, 1, 2, 2.5, 3.5, 4, 5, 5.5)

#md_plot <-
dev.off()
pdf("Output/panel_predicted_growth.pdf", width=21, height=21)
par(oma=c(12,8,3,1), mfrow=c(3,1), mar=c(1, 4, 4, 1))
#MD plot
plot(y ~ x, pch = ifelse(pred_md_all$md_category == 'wet', 19, 15), ylab="", ylim=c(0,0.13), xlab="", 
     tck=-0.01, cex=6, cex.axis = 5, xaxt="n", bty="l", pred_md_all)
#Add error bars
arrows(x0=pred_md_all$x, y0=pred_md_all$lower, x1=pred_md_all$x, y1=pred_md_all$upper, code=3, angle=90, length=0.1, lwd=5)
legend("topright", title= expression(bold('Long-term MD')), horiz=F, legend=c("Low (wet)", "High (dry)"),
       pch=c(19, 15), cex=4, bty="n")
mtext(expression(bold("A)")), side=1, cex=3.8, adj=0.005, padj=-8.1, line=-2)
#NCI plot
plot(y ~ x, pch = ifelse(pred_nci_all$nci_category == 'low NCI', 19, 15), ylab="", ylim=c(0,0.13), xlab="", 
     tck=-0.01, cex=6, cex.axis = 5, xaxt="n", bty="l", pred_nci_all)
#Add error bars
arrows(x0=pred_nci_all$x, y0=pred_nci_all$lower, x1=pred_nci_all$x, y1=pred_nci_all$upper, code=3, angle=90, length=0.1, lwd=5)
legend("topright", title= expression(bold('Total NCI')), horiz=F, legend=c("Low (sparse)", "High (dense)"),
       pch=c(19, 15), cex=4, bty="n")
mtext(expression(bold("B)")), side=1, cex=3.8, adj=0.005, padj= -8.1, line=-2)
#DBH plot
plot(y ~ x, pch = ifelse(pred_dbh_all$dbh_category == 'small', 19, 15), ylab="", ylim=c(0,0.13), xlab="", 
     tck=-0.01, cex=6, cex.axis = 5, xaxt="n", bty="l", pred_dbh_all)
#Add error bars
arrows(x0=pred_dbh_all$x, y0=pred_dbh_all$lower, x1=pred_dbh_all$x, y1=pred_dbh_all$upper, code=3, angle=90, length=0.1, lwd=5)
legend("topright", title= expression(bold('Preceding DBH')), horiz=F, legend=c("Low (small)", "High (big)"),
       pch=c(19, 15), cex=4, bty="n")
mtext(expression(bold("C)")), side=1, cex=3.8, adj=0.005, padj= -8.1, line=-2)
mtext("Growth rate (mm/day)", side=2, outer=T, cex=3.8, line=4)
mtext("Focal species", side=1, outer=T, cex=3.8, line=10)
mtext(expression(italic("E. amygdalina")), side=1, outer=T, cex=3.8, adj=0.03, line=4)
mtext(expression(italic("E. obliqua")), side=1, outer=T, cex=3.8, adj=0.35, line=4)
mtext(expression(italic("E. ovata")), side=1, outer=T, cex=3.8, adj=0.65, line=3)
mtext(expression(italic("E. viminalis")), side=1, outer=T, cex=3.8, adj=0.99, line=3)
dev.off()

#box(lwd=2)
#If code = 3 a head is drawn at both ends of the arrow


