### Eucalyptus Growth Tasmania Project 
# Alexandra Catling PhD Research
# Started 26/02/2021

#### Importing data and packages ####
source("data_preparations.R")

library(kableExtra)
library(sf)
library(dplyr)

#### Visualising distributions of data ####
#square root transformation for growth values, growth rates, 
#initial DBH, total neighbour basal area!
#Log is best for the total number of neighbours

#Growth
hist(growthnbhdata$growth_no_negs)
hist(growthnbhdata$growth_no_negs[growthnbhdata$Period==1])
hist(growthnbhdata$growth_no_negs[growthnbhdata$Period==2])
hist(sqrt(growthnbhdata$growth_no_negs))
#Growth rate
hist(growthnbhdata$growth_rate)
hist(growthnbhdata$growth_rate[growthnbhdata$Period==1])
hist(growthnbhdata$growth_rate[growthnbhdata$Period==2])
hist(sqrt(growthnbhdata$growth_rate))
#DBH
hist(growthnbhdata$DBH_cm)
hist(sqrt(growthnbhdata$DBH_cm))
## Number of neighbours and neighbour DBH (this is plotting 2x data points, period 1 and period 2, not a problem)
hist(growthnbhdata$total_nbh_ba)
hist(sqrt(growthnbhdata$total_nbh_ba))
hist(growthnbhdata$number_neighbours)
hist(log(growthnbhdata$number_neighbours))

#PPT, PET, MD - all not normally distributed
hist(growthnbhdata$PPT)
hist(growthnbhdata$PET)
hist(growthnbhdata$MD)
hist(growthnbhdata$period_rainfall)
hist(growthnbhdata$period_md)

#Neighbourhood crowding
hist(growthnbhdata$total_nci)
#Check this 40 - I assume from an estimate that I need to remove? Similar for 10-20
#do this*
hist(log(growthnbhdata$total_nci))
hist(growthnbhdata$intra_nci)
hist(log(growthnbhdata$intra_nci))
hist(growthnbhdata$inter_nci)
hist(log(growthnbhdata$inter_nci))

##### Is initial DBH evenly distributed across species? ####
# There are two trees that I have growth data for but no initial DBH measurements - "UN"
# Have to remove these
# dbhnumeric <- growthnbhdata %>% filter(DBH_cm != "UN") %>% 
#   group_by(Site, Focal_sp, Plot, Tree) %>% filter(row_number() == 1)
# dbhnumeric$DBH_cm <- as.numeric(dbhnumeric$DBH_cm)

ggplot(onerowdata, aes(x = Focal_sp, y = sqrt(DBH_cm)))+
  geom_boxplot()+
  geom_jitter(width = 0.2, alpha = 0.5, colour = "forestgreen")+
  theme_classic()+
  ylab("sqrt(Initial DBH (cm))")+
  xlab("Focal species")+
  my_theme

#One-way ANOVA to see, are there differences in initial DBH between species?
modeldbh1 <- aov(sqrt(DBH_cm) ~ Focal_sp, data = onerowdata)
summary(modeldbh1)
#No, there aren't
TukeyHSD(modeldbh1)

#Summarising means, range and sd:
#Initial DBH
meandbh <- dbhnumeric %>%
  group_by(Focal_sp) %>%
  summarise(min_dbh = min(DBH_cm),
            max_dbh = max(DBH_cm),
            mean_dbh = mean(DBH_cm),
            sd_dbh = sd(DBH_cm))

### For precipitation:
min(dbhnumeric$PPT)
max(dbhnumeric$PPT)
mean(dbhnumeric$PPT)
sd(dbhnumeric$PPT)
######### Does initial DBH predict growth rate? ####
#shape = Period
ggplot(growthnbhdata, aes(x = sqrt(DBH_cm), y = sqrt(growth_rate)))+
  geom_point(aes(colour = Period), alpha = 0.5)+
  geom_smooth(colour = "grey24", method = "lm")+
  theme_bw()+
  xlab("sqrt(Initial DBH (cm))")+
  ylab("sqrt(Growth rate (mm/day))")+
  facet_wrap(~ Focal_sp, ncol = 2, nrow = 2, scales = "fixed")+
  theme_classic()+
  theme(axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text = element_text(size = 16),
        strip.text.x = element_text(size = 16),
        legend.text = element_text(size = 10))

#Fit a linear model for the fixed effect of initial DBH on Growth rate
moddbhamyg <- lmer(sqrt(growth_rate) ~ sqrt(DBH_cm) + 
                     (1|Site/Plot/Tree), amygdata)
summary(moddbhamyg)
amygdharma <- simulateResiduals(moddbhamyg)
plot(amygdharma)
#ns, residuals good

moddbhobli <- lmer(sqrt(growth_rate) ~ sqrt(DBH_cm) + 
                     (1|Site/Plot/Tree), oblidata)
summary(moddbhobli)
oblidharma <- simulateResiduals(moddbhobli)
plot(oblidharma)
#p<0.01, residuals good

moddbhovat <- lmer(sqrt(growth_rate) ~ sqrt(DBH_cm) + 
                     (1|Site/Plot/Tree), ovatdata)
summary(moddbhovat)
ovatdharma <- simulateResiduals(moddbhovat)
plot(ovatdharma)
#ns, residuals okay -- significant deviation

moddbhvimi <- lmer(sqrt(growth_rate) ~ sqrt(DBH_cm) + 
                     (1|Site/Plot/Tree), vimidata)
summary(moddbhvimi)
vimidharma <- simulateResiduals(moddbhvimi)
plot(vimidharma)
#p<0.01, residuals good


####### Do growth rates differ across species? ####
#Growth rate by period
meangrowth <- growthnbhdata %>%
  group_by(Focal_sp) %>%
  summarise(mean_growth_rate_period1 = mean(growth_rate[Period == 1]),
            sd_growth_rate_period1 = sd(growth_rate[Period == 1]),
            mean_growth_rate_period2 = mean(growth_rate[Period == 2]),
            sd_growth_rate_period2 = sd(growth_rate[Period == 2]))

ggplot(growthnbhdata, aes(x = Focal_sp, y = sqrt(growth_rate)))+
  geom_boxplot()+
  geom_jitter(aes(colour = Period), width = 0.2, alpha = 0.5)+
  theme_classic()+
  ylab("sqrt(Growth rate (mm/day))")+
  xlab("Focal species")+
  my_theme

#One-way ANOVA to see if there are differences in growth rate between species
modelgr1 <- aov(sqrt(growth_rate) ~ Focal_sp, data = growthnbhdata)
summary(modelgr1)
#Yes, there are
TukeyHSD(modelgr1)
#E. viminalis grows at a significantly faster rate than other species
# E. amygdalina, E. ovata and E. obliqua all grow at similar rates

## Do growth rates differ by period?
ggplot(growthnbhdata, aes(x = Period, y = sqrt(growth_rate)))+
  geom_boxplot()+
  geom_jitter(width = 0.2, alpha = 0.5)+
  theme_classic()+
  ylab("sqrt(growth rate (mm/day))")+
  xlab("Period")+
  my_theme

## More informative to look within species
ggplot(growthnbhdata, aes(x = Focal_sp, y = sqrt(growth_rate), colour = Period))+
  geom_boxplot()+
  geom_point(position=position_jitterdodge(), width = 0.2, alpha = 0.5)+
  theme_classic()+
  ylab("sqrt(Growth rate (mm/day))")+
  xlab("Focal species")+
  my_theme

modelperiod <- aov(sqrt(growth_rate) ~ Period, vimidata)
summary(modelperiod)
#no for amyg, yes for obli, no for ovat, no for vimi

###### What neighbours do we find at each site? ####
## Do this in a loop for all sites:
listnbhgravelly <- surveydata %>% filter(Site == "GRA") %>% 
  group_by(Neighbour_sp_ID) %>% filter(row_number() == 1) %>%
  select(Site, Neighbour_sp_ID)
## Or just this but not sorted:
listneighboursbysite <- surveydata %>% group_by(Site, Neighbour_sp_ID) %>% filter(row_number() == 1) %>%
  select(Site, Neighbour_sp_ID)
##### Do NCI, basal area or density (number of neighbours) vary by site? ####
### NCI
growthnbhdata %>% mutate(Site = fct_reorder(Site, desc(MD))) %>%
  ggplot(aes(x = Site, y = total_nbh_ba))+
  geom_boxplot()+
  geom_jitter(alpha = 0.4, colour = "dodgerblue")+
  ylab("log(total basal area)")+
  theme_classic()

### BASAL AREA
#First line of code here reorders Sites according to MD values, desc means descending
growthnbhdata %>% mutate(Site = fct_reorder(Site, desc(MD))) %>%
  ggplot(aes(x = Site, y = log_total_nci))+
  geom_boxplot()+
  geom_jitter(alpha = 0.4, colour = "dodgerblue")+
  ylab("log(total NCI)")+
  theme_classic()

modelsite1 <- aov(total_nbh_ba ~ Site, growthnbhdata)
summary(modelsite1)
TukeyHSD(modelsite1)
#Yes, definitely differences in total basal area by site
# in directions we would expect. More basal area at wettest site, lowest at driest site

#### How do NCI, basal area and density vary with climate? ####
ggplot(growthnbhdata, aes(x = MD, y = total_nbh_ba))+
  geom_jitter(alpha = 0.2, width = 4)+
  geom_smooth(method="lm")+
  geom_errorbar(stat="summary", fun.data="mean_se",width=1.5,size=1.2, colour = 'red')+
  theme_classic()

#Ideally this would have fixed effects for soil properties too
modelmd1 <- lm(total_nbh_ba ~ MD, growthnbhdata)
summary(modelmd1)
#Yes, certainly more basal area of neighbours at wetter sites

## NUMBER OF NEIGHBOURS
growthnbhdata %>% mutate(Site = fct_reorder(Site, desc(MD))) %>%
  ggplot(aes(x = Site, y = log(number_neighbours)))+
  geom_boxplot()+
  geom_jitter(alpha = 0.4, colour = "dodgerblue")+
  theme_classic()

modelsite2 <- aov(log(number_neighbours) ~ Site, growthnbhdata)
summary(modelsite2)
#Differences between sites in number of neighbours
TukeyHSD(modelsite2)

#PPT
ggplot(growthnbhdata, aes(x = PPT, y = log(number_neighbours)))+
  geom_jitter(alpha = 0.4, colour = "dodgerblue", width = 5)+
  geom_smooth()+
  geom_errorbar(stat="summary", fun.data="mean_se",width=0,size=0.8)+
  theme_classic()
#The number of neighbours increases with PPT

modelppt1 <- lm(log(number_neighbours) ~ PPT, growthnbhdata)
summary(modelppt1)
#Suggests a difference in number of neighbours with PPT, adjusted R-squared ~0.3

#PET
ggplot(growthnbhdata, aes(x = PET, y = log(number_neighbours)))+
  geom_point(alpha = 0.4, colour = "dodgerblue")+
  geom_smooth(method = "lm")+
  geom_errorbar(stat="summary", fun.data="mean_se",width=0,size=0.8)+
  theme_classic()

modelpet1 <- lm(log(number_neighbours) ~ PET, growthnbhdata)
summary(modelpet1)
#Suggests a difference in number of neighbours with PET, adjusted R-squared ~0.31

#MD
ggplot(growthnbhdata, aes(x = MD, y = log(number_neighbours)))+
  geom_point(alpha = 0.4, colour = "dodgerblue")+
  geom_smooth()+
  geom_errorbar(stat="summary", fun.data="mean_se",width=0,size=0.8)+
  theme_classic()

modelmd1 <- lm(log(number_neighbours) ~ MD, growthnbhdata)
summary(modelmd1)
#Suggests a difference in number of neighbours with MD, adjusted R-squared ~0.3

############ Another way of plotting above
# Number of neighbours against PPT/PET/MD
ggplot(growthnbhdata, aes(x = PPT, y = log(number_neighbours), group = Site))+
  geom_boxplot()+
  geom_jitter(alpha = 0.4, colour = "dodgerblue", position = (position_jitter(width = 5)))+
  theme_classic()
ggplot(growthnbhdata, aes(x = PET, y = log(number_neighbours), group = Site))+
  geom_boxplot()+
  geom_jitter(alpha = 0.4, colour = "dodgerblue")+
  theme_classic()
ggplot(growthnbhdata, aes(x = MD, y = log(number_neighbours), group = Site))+
  geom_boxplot()+
  geom_jitter(alpha = 0.4, colour = "dodgerblue", position = (position_jitter(width = 5)))+
  theme_classic()
#Plotting neighbour basal area against PPT/PET/MD
ggplot(growthnbhdata, aes(x = PPT, y = sqrt(total_nbh_ba), group = Site))+
  geom_boxplot()+
  geom_jitter(alpha = 0.4, colour = "dodgerblue", position = (position_jitter(width = 5)))+
  theme_classic()
ggplot(growthnbhdata, aes(x = PET, y = sqrt(total_nbh_ba), group = Site))+
  geom_boxplot()+
  geom_jitter(alpha = 0.4, colour = "dodgerblue", position = (position_jitter(width = 5)))+
  theme_classic()
ggplot(growthnbhdata, aes(x = MD, y = sqrt(total_nbh_ba), group = Site))+
  geom_boxplot()+
  geom_jitter(alpha = 0.4, colour = "dodgerblue", position = (position_jitter(width = 5)))+
  theme_classic()

#MD and NCI correlated
dev.off()
pdf("Output/nci~md.pdf", width=21, height=21)
par(pty="s")
growthnbhdata %>% filter(Period == 2) %>%
  ggplot(aes(x = std_md, y = log_total_nci))+
  geom_jitter(alpha = 0.4, width = 0.05)+
  geom_smooth(method="lm")+
  xlab("Moisture deficit (mean annual precipitation - mean annual evapotranspiration, standardised)")+
  ylab("log(Neighbourhood crowding index)")+
  theme_classic()
dev.off()

modelpptnci <- lmer(log_total_nci ~ std_md + (1|Site/Plot/Tree), onerowdata)
summary(modelpptnci)
r.squaredGLMM(modelpptnci)


### Correlations with period climate
#### neighbour info
#basal area not correlated with period climate
ggplot(growthnbhdata, aes(x = period_md, y = sqrt(total_nbh_ba), colour = Period))+
  geom_point()+
  geom_smooth(method="lm")+
  theme_classic()
#number of neighbours is correlated with period climate
ggplot(growthnbhdata, aes(x = period_md, y = log(number_neighbours+1), colour = Period))+
  geom_point()+
  geom_smooth(method="lm")+
  theme_classic()

#number of neighbours is correlated with period climate
ggplot(growthnbhdata, aes(x = period_md, y = log_total_nci, colour = Period))+
  geom_point()+
  geom_smooth(method="lm")+
  theme_classic()

#### Relationships between climate variables ####
#Standardised by day, 365 for annual climate, days_in_period for period climate

### Climate
ggplot(growthnbhdata, aes(x = daily_ppt, y = daily_period_rainfall))+
  geom_point()+
  geom_smooth(method="lm")+
  ylab("daily period rainfall")+
  xlab("daily long-term rainfall")+
  theme_classic()

ggplot(growthnbhdata, aes(x = daily_md, y = daily_period_md))+
  geom_point()+
  geom_smooth(method="lm")+
  ylab("daily period md")+
  xlab("daily long-term md")+
  theme_classic()

#### Plotting growth ~ period rainfall ####

dev.off()
pdf("Output/growth_rate~period_rainfall.pdf", width=21, height=21)
par(pty="s")
ggplot(growthnbhdata, aes(x = period_rainfall, y = sqrt(growth_rate), colour = Period))+
  geom_jitter(alpha = 0.5, cex = 3)+
  geom_smooth(method="lm")+
  xlab("Rainfall in growth period (mm)")+
  ylab("sqrt(growth rate (mm/day))")+
  theme_classic()+
  theme(text = element_text(size = 20))
dev.off()

#May be quadratic response to rainfall.
#Not how my hypothesis is set up
modelperiodppt <- lmer(sqrt(growth_rate) ~ std_prain + I(std_prain^2) + (1|Site/Plot/Tree), growthnbhdata)
summary(modelperiodppt)

##### Research questions: Climate vs competition #####
# How does sensitivity of growth to climate vary with competition?
#only looking at MD

#Create plot with growth rate on y axis, moisture on x axis
# and two fitted lines and CIs for sparse neighbourhood and dense neighbourhood
with(growthnbhdata, plot(sqrt(growth_rate) ~ MD))
with(growthnbhdata, plot(sqrt(growth_rate) ~ log(total_nci)))

#### Plot the number of conspecifics and heterospecifics for each sp ####
ggplot(onerowdata, aes(x = sqrt(growth_rate), y = std_intra_nci))+
  geom_point(alpha = 0.3)+
  ylab("standardised log(intra NCI+1)")+
  theme_classic()+
  facet_wrap(~Focal_sp)

ggplot(onerowdata, aes(x = sqrt(growth_rate), y = std_inter_nci))+
  geom_point(alpha = 0.3)+
  ylab("standardised log(inter NCI+1)")+
  theme_classic()+
  facet_wrap(~Focal_sp)

#### Main models by species ####
### Total nci only model:
#growth ~ NCI + long-term climate + period climate + NCI*period climate +
#initial DBH + initial DBH*NCI + initial DBH*period climate +(1|Site/Plot/Tree)

### Intra and inter nci model:
#growth ~ NCI intra + NCI inter + long-term climate + period climate + 
#NCI intra*period climate + NCI inter*period climate +
#initial DBH + initial DBH*period climate + initial DBH*NCI intra + 
#initial DBH*NCI inter + (1|Site/Plot/Tree)

### AMYG ####
amygmod1 <- lmer(sqrt(growth_rate) ~ std_total_nci + std_md + std_period_md + 
                   std_total_nci*std_period_md + DBH_cm + DBH_cm*std_total_nci +
                   DBH_cm*std_period_md + (1|Site/Plot/Tree), amygdata)
amygmod1dharma <- simulateResiduals(amygmod1)
plot(amygmod1dharma)
#great
summary(amygmod1)
vif(amygmod1)
#bad vifs for NCI and period MD
#plot(amygmod1)

## Try removing period md (could replace with anomalies)
# amygmod1 <- lmer(sqrt(growth_rate) ~ std_total_nci + std_md + 
#                     + DBH_cm + DBH_cm*std_total_nci +
#                    (1|Site/Plot/Tree), amygdata)
# vif(amygmod1)
#Why are DBH_cm and NCI correlated?!
ggplot(amygdata, aes(x = DBH_cm, y = std_total_nci))+
  geom_point()+
  geom_smooth(method='lm')+
  theme_classic()

## Try removing long-term md?
# amygmod1 <- lmer(sqrt(growth_rate) ~ std_total_nci + std_period_md + 
#                    std_total_nci*std_period_md + DBH_cm + DBH_cm*std_total_nci +
#                    DBH_cm*std_period_md + (1|Site/Plot/Tree), amygdata)
# vif(amygmod1)
#still problems with NCI and period_md and DBH_cm

## Try removing NCI
# amygmod1 <- lmer(sqrt(growth_rate) ~ std_md + std_period_md + 
#                    + DBH_cm +
#                    DBH_cm*std_period_md + (1|Site/Plot/Tree), amygdata)
# vif(amygmod1)
#Now period md and DBH are correlated? wah
ggplot(amygdata, aes(x = DBH_cm, y = period_md))+
  geom_point()+
  geom_smooth(method='lm')+
  theme_classic()
#No it's not!
## solution: correlation between NCI and long-term climate is fine.
# correlation between NCI and period climate is definitely fine. Ignoring these two
# correlations between long-term and period climate are not good - 
# so will model period climate as anomalies, as John suggested
# actually I think that is fine too when I don't fit a linear regression to it 

### intraspecific and interspecific NCIs model, mod2
amygmod2 <- lmer(sqrt(growth_rate) ~ std_intra_nci + std_inter_nci + std_md + 
                   std_period_md + std_intra_nci*std_period_md + std_inter_nci*std_period_md + 
                   DBH_cm + DBH_cm*std_intra_nci + DBH_cm*std_inter_nci +
                   DBH_cm*std_period_md + (1|Site/Plot/Tree), amygdata)
amygmod2dharma <- simulateResiduals(amygmod2)
plot(amygmod2dharma)
#great
summary(amygmod2)
vif(amygmod1)
#bad vifs for NCI and period MD

### OBLI ####
oblimod1 <- lmer(sqrt(growth_rate) ~ std_total_nci + std_md + std_period_md + 
                   std_total_nci*std_period_md + DBH_cm + DBH_cm*std_total_nci +
                   DBH_cm*std_period_md + (1|Site/Plot/Tree), oblidata)
oblimod1dharma <- simulateResiduals(oblimod1)
plot(oblimod1dharma)
#great
summary(oblimod1)
vif(oblimod1)
#very bad vifs for NCI and period MD
#plot(oblimod1)
oblimod2 <- lmer(sqrt(growth_rate) ~ std_intra_nci + std_inter_nci + std_md + 
                   std_period_md + std_intra_nci*std_period_md + std_inter_nci*std_period_md + 
                   DBH_cm + DBH_cm*std_intra_nci + DBH_cm*std_inter_nci +
                   DBH_cm*std_period_md + (1|Site/Plot/Tree), oblidata)
oblimod2dharma <- simulateResiduals(oblimod2)
plot(oblimod2dharma)
#great
summary(oblimod2)
vif(oblimod1)
#very bad vifs for NCI and period MD

### OVAT ####
ovatmod1 <- lmer(sqrt(growth_rate) ~ std_total_nci + std_md + std_period_md + 
                   std_total_nci*std_period_md + DBH_cm + DBH_cm*std_total_nci +
                   DBH_cm*std_period_md + (1|Site/Plot/Tree), ovatdata)
ovatmod1dharma <- simulateResiduals(ovatmod1)
plot(ovatmod1dharma)
#residuals not great
summary(ovatmod1)
vif(ovatmod1)
#bad vifs for NCI and period MD
#plot(ovatmod1)
ovatmod2 <- lmer(sqrt(growth_rate) ~ std_intra_nci + std_inter_nci + std_md + 
                   std_period_md + std_intra_nci*std_period_md + std_inter_nci*std_period_md + 
                   DBH_cm + DBH_cm*std_intra_nci + DBH_cm*std_inter_nci +
                   DBH_cm*std_period_md + (1|Site/Plot/Tree), ovatdata)
ovatmod2dharma <- simulateResiduals(ovatmod2)
plot(ovatmod2dharma)
#great
summary(ovatmod2)
vif(ovatmod1)
#very bad vifs for NCI and period MD

### VIMI ####
vimimod1 <- lmer(sqrt(growth_rate) ~ std_total_nci + std_md + std_period_md + 
                   std_total_nci*std_period_md + DBH_cm + DBH_cm*std_total_nci +
                   DBH_cm*std_period_md + (1|Site/Plot/Tree), vimidata)
vimimod1dharma <- simulateResiduals(vimimod1)
plot(vimimod1dharma)
#okay
summary(vimimod1)
vif(vimimod1)
#very bad vifs for NCI, bad for period MD
#plot(vimimod1)
vimimod2 <- lmer(sqrt(growth_rate) ~ std_intra_nci + std_inter_nci + std_md + 
                   std_period_md + std_intra_nci*std_period_md + std_inter_nci*std_period_md + 
                   DBH_cm + DBH_cm*std_intra_nci + DBH_cm*std_inter_nci +
                   DBH_cm*std_period_md + (1|Site/Plot/Tree), vimidata)
vimimod2dharma <- simulateResiduals(vimimod2)
plot(vimimod2dharma)
#okay
summary(vimimod2)
vif(vimimod1)
#very bad vifs for NCI, bad for period MD




### Try removing terms to see which ones are correlated/if removing one changes others'results?

#without neighbours

#without long-term climate

#without period climate


#### Plotting growth rate in response to NCI from model ####
dev.off()
pdf("Output/growth_rate~NCI.pdf", width=21, height=21)
par(pty="s")
plot(sqrt(growth_rate) ~ std_total_nci, pch=19, col=alpha("grey60", 0.3), ylab="sqrt(Growth rate (mm/day))", xlab="Neighbourhood crowding index (standardised, sqrt)", tck=-0.01, cex= 2, cex.lab = 2, cex.axis = 2, growthnbhdata)
model<-lmer(sqrt(growth_rate) ~ std_dbh + Focal_sp + std_ppt + 
              std_total_nci + std_dbh*std_total_nci + Focal_sp:std_dbh + std_ppt:std_total_nci +
              (1|Site/Plot/Tree), growthnbhdata)
x_to_plot<-seq.func(growthnbhdata$std_total_nci)
#mean NCI - black
preddata <- with(model, data.frame(1, 0, 0, 0, 0, 0, x_to_plot, 0*x_to_plot, 0*0, 0*0, 0*0, 0*x_to_plot))
plotted.pred <- glmm.predict(mod = model, newdat = preddata, se.mult = 1.96, logit_link=FALSE, log_link=FALSE, glmmTMB=FALSE)
plot.CI.func(x.for.plot = x_to_plot, pred = plotted.pred$y, upper = plotted.pred$upper, lower = plotted.pred$lower, env.colour = "grey1", env.trans = 50, line.colour = "black", line.weight = 2, line.type = 1)
dev.off()

#### Plotting interaction between two continuous variables ####
#No significant interaction between PC1 and NCI
# PPT is x
# NCI is set at -1, 0 or 1 (1 sd low, mean, or 1 sd high)
# with(growthnbhdata, plot(jitter(sqrt(growth_rate), amount = 0.05) ~ std_ppt, col = ifelse(std_nci>0, "red", "blue")))
# #One sd low NCI
# curve(cbind(1, 0, 0, 0, 0, x, -1, 0*0, 0*0, 0*0, 0*0, x*-1)%*%fixef(model1), add = TRUE, col = "red")
# #One sd high NCI
# curve(cbind(1, 0, 0, 0, 0, x, 1, 0*0, 0*0, 0*0, 0*0, x*1)%*%fixef(model1), add = TRUE, col = "blue")
# #Mean NCI
# curve(cbind(1, 0, 0, 0, 0, x, 0, 0*0, 0*0, 0*0, 0*0, x*0)%*%fixef(model1), add = TRUE, col = "blue")
dev.off()
pdf("Output/growth_rate~PPT+NCI.pdf", width=21, height=21)
par(pty="s")
plot(sqrt(growth_rate) ~ jitter(std_ppt, 1), pch=19, col=alpha("grey60", 0.2), ylab="sqrt(Growth rate (mm/day))", xlab="Mean annual precipitation (standardised)", tck=-0.01, cex= 2, cex.lab = 2, cex.axis = 2, growthnbhdata)
model<-lmer(sqrt(growth_rate) ~ std_dbh + Focal_sp + std_ppt + 
              std_nci + std_dbh*std_nci + Focal_sp:std_dbh + std_ppt:std_nci +
              (1|Site/Plot/Tree), growthnbhdata)
x_to_plot<-seq.func(growthnbhdata$std_ppt)
#mean NCI - black
preddata <- with(model, data.frame(1, 0, 0, 0, 0, x_to_plot, 0, 0*0, 0*0, 0*0, 0*0, x_to_plot*0))
plotted.pred <- glmm.predict(mod = model, newdat = preddata, se.mult = 1.96, logit_link=FALSE, log_link=FALSE, glmmTMB=FALSE)
plot.CI.func(x.for.plot = x_to_plot, pred = plotted.pred$y, upper = plotted.pred$upper, lower = plotted.pred$lower, env.colour = "grey1", env.trans = 50, line.colour = "black", line.weight = 2, line.type = 1)
#low NCI - blue
preddata <- with(model, data.frame(1, 0, 0, 0, 0, x_to_plot, -1, 0*0, 0*0, 0*0, 0*0, x_to_plot*-1))
plotted.pred <- glmm.predict(mod = model, newdat = preddata, se.mult = 1.96, logit_link=FALSE, log_link=FALSE, glmmTMB=FALSE)
plot.CI.func(x.for.plot = x_to_plot, pred = plotted.pred$y, upper = plotted.pred$upper, lower = plotted.pred$lower, env.colour = "grey1", env.trans = 50, line.colour = "blue", line.weight = 2, line.type = 1)
#high NCI - forest green
preddata <- with(model, data.frame(1, 0, 0, 0, 0, x_to_plot, 1, 0*0, 0*0, 0*0, 0*0, x_to_plot*1))
plotted.pred <- glmm.predict(mod = model, newdat = preddata, se.mult = 1.96, logit_link=FALSE, log_link=FALSE, glmmTMB=FALSE)
plot.CI.func(x.for.plot = x_to_plot, pred = plotted.pred$y, upper = plotted.pred$upper, lower = plotted.pred$lower, env.colour = "grey1", env.trans = 50, line.colour = "forestgreen", line.weight = 2, line.type = 1)
dev.off()

#### Growth rate ~ NCI in high or low ppt sites ####
dev.off()
pdf("Output/growth_rate~NCI+PPT.pdf", width=21, height=21)
par(pty="s")
plot(sqrt(growth_rate) ~ std_nci, pch=19, col = alpha(ifelse(std_ppt>0, "forestgreen", "red"), 0.4), ylab="sqrt(Growth rate (mm/day))", xlab="Neighbourhood crowding index (standardised, sqrt)", tck=-0.01, cex= 2, cex.lab = 2, cex.axis = 2, growthnbhdata)
model<-lmer(sqrt(growth_rate) ~ std_dbh + Focal_sp + std_ppt + 
              std_nci + std_dbh*std_nci + Focal_sp:std_dbh + std_ppt:std_nci +
              (1|Site/Plot/Tree), growthnbhdata)
x_to_plot<-seq.func(growthnbhdata$std_nci)
#low ppt - red
preddata <- with(model, data.frame(1, 0, 0, 0, 0, -1, x_to_plot, 0*x_to_plot, 0*0, 0*0, 0*0, -1*x_to_plot))
plotted.pred <- glmm.predict(mod = model, newdat = preddata, se.mult = 1.96, logit_link=FALSE, log_link=FALSE, glmmTMB=FALSE)
plot.CI.func(x.for.plot = x_to_plot, pred = plotted.pred$y, upper = plotted.pred$upper, lower = plotted.pred$lower, env.colour = "grey1", env.trans = 50, line.colour = "red", line.weight = 2, line.type = 1)
#high ppt - forest green
preddata <- with(model, data.frame(1, 0, 0, 0, 0, 1, x_to_plot, 0*x_to_plot, 0*0, 0*0, 0*0, 1*x_to_plot))
plotted.pred <- glmm.predict(mod = model, newdat = preddata, se.mult = 1.96, logit_link=FALSE, log_link=FALSE, glmmTMB=FALSE)
plot.CI.func(x.for.plot = x_to_plot, pred = plotted.pred$y, upper = plotted.pred$upper, lower = plotted.pred$lower, env.colour = "grey1", env.trans = 50, line.colour = "forestgreen", line.weight = 2, line.type = 1)
dev.off()
## Do this* want to plot these lines for everything 'dry' vs everything 'wet'
# so where ppt >0 or <1, or based on how I split them - instead of relative to the mean.

hist(growthnbhdata$std_ppt)
hist(growthnbhdata$PPT)
mean(growthnbhdata$PPT)

##### Do neighbour basal area or density influence growth? ####
#Basal area
ggplot(growthnbhdata, aes(x = sqrt(total_nbh_ba), y = sqrt(growth_rate)))+
  geom_point(alpha=0.4)+
  geom_smooth(method="lm", colour = "forestgreen")+
  xlab("sqrt(Total neighbour basal area (m^2))")+
  ylab("sqrt(Growth rate (mm/day))")+
  theme_classic()+
  my_theme+
  facet_wrap(~Focal_sp)

#Number of neighbours
ggplot(growthnbhdata, aes(x = log(number_neighbours), y = sqrt(growth_rate)))+
  geom_point(alpha=0.4)+
  geom_smooth(method="lm", colour = "forestgreen")+
  xlab("sqrt(Total neighbour basal area (m^2))")+
  ylab("sqrt(Growth rate (mm/day))")+
  theme_classic()+
  my_theme+
  facet_wrap(~Focal_sp)

#### Making a map of study sites ####
#Read in the SA2 shapefile downloaded from the ABS
#Data from ABS localities
# https://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/1270.0.55.001July%202016?OpenDocument
#Following this guide: https://medium.com/analytics-vidhya/mapping-australia-in-r-6ce092c48b49

ausplotdata <- read_sf("Data/SA2_2016_AUST.shp")
head(ausplotdata)

#filter the Australian SA2 shapefile for only Tas
tasplotdata <- ausplotdata %>% filter(STE_NAME16 == "Tasmania")

ggplot()+
  geom_sf(data = ausplotdata)+
  xlab("Longitude")+
  ylab("Latitude") +
  xlim(110,155)+
  theme_classic()

ggplot()+
  geom_sf(data = ausplotdata)+
  geom_sf(data = tasplotdata, fill = "blue") +
  xlab("Longitude")+
  ylab("Latitude") +
  xlim(110,155)+
  theme_classic()

ggplot()+
  geom_sf(data = tasplotdata) +
  xlab("Longitude")+
  ylab("Latitude") +
  theme_classic()

#import a shapefile of state boundaries
aus_state_data <- read_sf("Data/STE_2016_AUST.shp")

ggplot()+
  geom_sf(data = aus_state_data)+
  theme_classic()

#make a new dataset of cities in Australia (google the locations)
#"West Perenjori Nature Reserve", -29.46703, 116.20600
tas_cities <- tribble(
  ~city, ~lat, ~long, 
  "Hobart",-42.881520, 147.326839,
  "Launceston", -41.439881, 147.136506)

#convert those two columns to geometry column with the st_as_sf() function. Google Maps uses the coordinate reference system 4326 (the GPS system).
tas_cities_geometry <- tas_cities %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326)

ggplot() +
  geom_sf(data=aus_state_data)+
  geom_sf(data=tas_cities_geometry, size=1)+
  geom_sf_label(data=tas_cities_geometry, aes(label = city))+
  xlim(110,155)+
  xlab("Longitude") +
  ylab("Latitude") + 
  theme_bw()

#making dataframe for my study sites
site_coords <- tribble(
  ~site, ~lat, ~long, 
  "EPF", -41.77497, 147.3183,
  "TMP", -43.144035, 147.962669,
  "MER", -41.56674,	146.24294,
  "DOG", -41.5385778,	146.324706,
  "GRA", -42.55066,	147.45885,
  "FREY", -41.94839,	148.13788,
  "BOF", -41.186032,	148.189604)

#Making a plot of Tas with study sites marked
dev.off()
pdf("Output/Tas_study_sites_map.pdf")
ggplot()+
  geom_sf(data = tasplotdata, fill = "white") +
  geom_sf_label(data=tas_cities_geometry, aes(label = city))+
  geom_point(data = site_coords, aes(x = long, y = lat), size = 4, colour = "red", pch = 18) +  
  xlab("Longitude")+
  ylab("Latitude") +
  xlim(144.5,148.5)+
  ylim(-43.7, -40.5)+
  theme_classic()+
  theme(panel.border = element_rect(colour = "black", fill=NA, size = 1.5))
dev.off()

#### Table of sample sizes and site climate ####
samplesizes <- onerowdata %>% group_by(Site, Focal_sp) %>% 
  summarise(number_focals = n())
samplesizes_long <- samplesizes %>% pivot_wider(id_cols = Site, names_from = Focal_sp, values_from = number_focals)
site_climate <- onerowdata %>% group_by(Site, PPT, MD) %>% 
  select(Site, PPT, MD, period_rainfall, period_md) %>% filter(row_number() == 1)
site_table <- left_join(site_climate, samplesizes_long)
site_table <- site_table %>% replace(is.na(.), 0)
#Rearranging rows by site from wettest to driest
site_table <- site_table %>% arrange(desc(MD))
#Renaming column names
colnames(site_table) <- c('Site', 'Mean PPT', "Mean MD", "Period PPT", "Period MD", "E. amygdalina", "E. ovata", "E. viminalis", "E. obliqua")

site_table %>%
  kbl(caption = "<b>Supplementary 1</b>. Number of focal trees and climate at each site ordered from driest to wettest based on mean annual moisture deficit (MD). Mean annual precipitation (PPT) and MD downloaded 
  from the Global Aridity and PET Database (Zomer et al. 2006) based on 1960-1990 WorldClim climate averages (Hijmans et al. 2005). PPT and MD over the study period 
  downloaded from BOM (ref*). MER is Mersey River Conservation Area, DOG is Dogs Head Regional Reserve, TMP is Tasman National Park, BOF is Doctors Peak Regional Reserve, 
      EPF is Tom Gibson Nature Reserve, FREY is Apslawn Forest Reserve and GRA is Gravelly Ridge Conservation Area.", digits = 0) %>%
  kable_classic(full_width = F, html_font = "Times") %>%
  row_spec(0, italic = T) %>%
  add_header_above(c(" " = 5, "Number of focal trees" = 4))


### BELOW HERE NEEDS TO BE UPDATED ####
specieslist <- c("AMYG", "OBLI", "OVAT", "VIMI")
specieslong <- c("Eucalyptus amygdalina", "Eucalyptus obliqua", "Eucalyptus ovata", "Eucalyptus viminalis")
for(i in 1:length(specieslist)){
  nam <- paste0("nbhdata", specieslist[i])
  assign(nam, filter(growthnbhdata, Focal_sp == specieslist[i]))
  print(specieslist[i])
  print(
    summary(
      lm(growth_rate ~ log(number_neighbours), data = filter(growthnbhdata, Focal_sp == specieslist[i]))))
}

ggplot(growthnbhsimple, aes(x = log(Total_nbh_DBH), y = gr_avg))+
  geom_jitter(alpha=0.5)+
  geom_smooth(method="lm", colour = "forestgreen")+
  theme_classic()+
  facet_wrap(~ Focal_sp, ncol = 2, nrow = 2, scales="fixed")+
  xlab("Total neighbour DBH (cm)")+
  ylab("Growth rate (mm/day)")+
  my_theme

### Making my own predicted line from model
dbhnmod4 <- lm(growth_rate ~ log(Total_nbh_DBH), growthnbhsimple)
summary(dbhnmod1)
test4 <- growthnbhsimple %>% filter(!is.na(growth_rate)) %>% filter(growth_rate != "UN")
test4$predlm = predict(dbhnmod1)
predslm = predict(dbhnmod1, interval = "confidence")
head(predslm)
test4lm <- cbind(test4, predslm)
#No idea why cbind isn't working!### STUCK HERE
head(testlm)

#Not that fill changes the ribbon colour and 'colour' changes the edges of the ribbon
ggplot(testinglm, aes(x = log(DBH_cm), y = growth_rate)) +
  geom_point(alpha = 0.4) +
  geom_ribbon( aes(ymin = lwr, ymax = upr), alpha = .15, fill = "forestgreen") +
  geom_line( aes(y = fit), size = 1, colour = "forestgreen")+
  xlab("Initial DBH (cm)")+
  ylab("Growth rate (mm/day)")+
  theme_classic()+
  my_theme


ggplot(growthnbhsimple, aes(x = log(Total_nbh_DBH), y = growth_rate))+
  geom_point(alpha=0.4)+
  geom_smooth(method="lm", colour = "forestgreen")+
  geom_point(aes(colour = Focal_sp))+
  xlab("Total neighbour DBH (cm)")+
  ylab("Growth rate (mm/day)")+
  theme_classic()+
  my_theme

#To get confidence intervals
predslm = predict(model1.1, interval = "confidence")
head(predslm)

testlm = cbind(test, predslm)
head(testlm)

#Number of neighbours
ggplot(growthnbhsimple, aes(x = log(No_neighbours), y = gr_avg))+
  geom_point(alpha = 0.4)+
  geom_smooth(method="lm")+
  xlab("Total neighbour abundance")+
  ylab("Growth rate (mm/day)")+
  theme_classic()+
  my_theme

nonmod1 <- lm(growth_rate ~ log(No_neighbours+1), growthnbhsimple)
summary(nonmod1)
r.squaredGLMM(nonmod1)

for(i in 1:length(specieslist)){
  print(specieslist[i])
  print(
    summary(
      lm(growth_rate ~ log(No_neighbours+1), data = filter(growthnbhsimple, Focal_sp == specieslist[i]))))
}

amygnbhmod1 <- lm(growth_rate ~ log(No_neighbours), nbhdataAMYG)
summary(amygnbhmod1)
r.squaredGLMM(amygnbhmod1)

ggplot(growthnbhsimple, aes(x = log(No_neighbours), y = growth_rate))+
  geom_point(alpha=0.5)+
  geom_smooth(method="lm")+
  theme_classic()+
  facet_wrap(~ Focal_sp, ncol = 2, nrow = 2, scales="fixed")+
  xlab("log(Neighbour abundance+1)")+
  ylab("Growth rate (mm/day)")+
  my_theme

#### Looking at intra- and inter-specific neighbour effects on growth ####
head(growthnbhdata)
#Need to rename Focal_sp to do matching with Neighbour_sp_ID
growthnbhdata <- within(growthnbhdata, Focal_sp[Focal_sp == "AMYG"] <- 'Eucalyptus amygdalina')
growthnbhdata <- within(growthnbhdata, Focal_sp[Focal_sp == "OBLI"] <- 'Eucalyptus obliqua')
growthnbhdata <- within(growthnbhdata, Focal_sp[Focal_sp == "OVAT"] <- 'Eucalyptus ovata')
growthnbhdata <- within(growthnbhdata, Focal_sp[Focal_sp == "VIMI"] <- 'Eucalyptus viminalis')

growthnbhdata$Matching <- growthnbhdata$Focal_sp == growthnbhdata$Neighbour_sp_ID
growthnbhdata$Matching <- ifelse(growthnbhdata$Matching == TRUE, 1, 0)

#Can't sum NAs, so making all Neighbour_DBH_cm NAs 0s.
growthnbhdata <- growthnbhdata %>% mutate(Neighbour_DBH_cm_2 = ifelse(is.na(Neighbour_DBH_cm_2), 0, Neighbour_DBH_cm_2),
                                    Neighbour_DBH_cm_3 = ifelse(is.na(Neighbour_DBH_cm_3), 0, Neighbour_DBH_cm_3),
                                    Neighbour_DBH_cm_4 = ifelse(is.na(Neighbour_DBH_cm_4), 0, Neighbour_DBH_cm_4),
                                    Neighbour_DBH_cm_5 = ifelse(is.na(Neighbour_DBH_cm_5), 0, Neighbour_DBH_cm_5),
                                    Neighbour_DBH_cm_6 = ifelse(is.na(Neighbour_DBH_cm_6), 0, Neighbour_DBH_cm_6),
                                    Neighbour_DBH_cm_7 = ifelse(is.na(Neighbour_DBH_cm_7), 0, Neighbour_DBH_cm_7),
                                    Neighbour_DBH_cm_8 = ifelse(is.na(Neighbour_DBH_cm_8), 0, Neighbour_DBH_cm_8))

growthnbhmatch <- growthnbhdata %>% group_by(Site, Focal_sp, Plot, Tree) %>%
  mutate(Intra_abundance = sum(Neighbour_DBH_cm[Matching == "1"] + Neighbour_DBH_cm_2[Matching == "1"] +
                              Neighbour_DBH_cm_3[Matching == "1"] + Neighbour_DBH_cm_4[Matching == "1"] +
                                Neighbour_DBH_cm_5[Matching == "1"] + Neighbour_DBH_cm_6[Matching == "1"] +
                                Neighbour_DBH_cm_7[Matching == "1"] + Neighbour_DBH_cm_8[Matching == "1"]))
growthnbhmatch <- growthnbhmatch %>% group_by(Site, Focal_sp, Plot, Tree) %>%
  mutate(Inter_abundance = sum(Neighbour_DBH_cm[Matching == "0"] + Neighbour_DBH_cm_2[Matching == "0"] +
                                 Neighbour_DBH_cm_3[Matching == "0"] + Neighbour_DBH_cm_4[Matching == "0"] +
                                 Neighbour_DBH_cm_5[Matching == "0"] + Neighbour_DBH_cm_6[Matching == "0"] +
                                 Neighbour_DBH_cm_7[Matching == "0"] + Neighbour_DBH_cm_8[Matching == "0"]))
#Turning into a dataset with one row per subplot
growthmatchsimple <- growthnbhmatch %>% group_by(Site, Focal_sp, Plot, Tree) %>%
                                        filter(row_number() == 1)
growthmatchsimple <- growthmatchsimple %>% select(Site, Focal_sp, Plot, Tree, 
                                                  Site_name, PPT, PET, MD, growth_rate, 
                                                  DBH_cm, Total_nbh_DBH, No_neighbours, 
                                                  log_no_neighbours, Intra_abundance, Inter_abundance)

#Filtering out NAs
growthmatchsimple <- growthmatchsimple %>% filter(!is.na(Intra_abundance)) %>% filter(!(is.na(Inter_abundance)))

ggplot(growthmatchsimple, aes(x = log(Intra_abundance+1), y = growth_rate))+
  geom_jitter(alpha = 0.4, width = 0.05)+
  geom_smooth(method="lm")+
  ylab("Growth rate (mm/day)")+
  xlab("Intraspecific neighbour abundance")+
  theme_classic()+
  my_theme+
  theme(strip.text.x = element_text(size = 11, face = "italic"))+
  facet_wrap(vars(Focal_sp))

ggplot(growthmatchsimple, aes(x = log(Inter_abundance+1), y = growth_rate))+
  geom_jitter(alpha = 0.4, width = 0.05)+
  geom_smooth(method="lm")+
  ylab("Growth rate (mm/day)")+
  xlab("Interspecific neighbour abundance")+
  theme_classic()+
  my_theme+
  theme(strip.text.x = element_text(size = 11, face = "italic"))+
  facet_wrap(vars(Focal_sp))

# Plotting them on the same graph
ggplot(growthmatchsimple)+
  geom_jitter(aes(x = log(Inter_abundance+1), y = growth_rate, colour = "Interspecific"), alpha = 0.4, width = 0.05)+
  geom_jitter(aes(x = log(Intra_abundance+1), y = growth_rate, colour = "Intraspecific"), alpha = 0.4, width = 0.05)+
  geom_smooth(aes(x = log(Inter_abundance+1), y = growth_rate, colour = "Interspecific"), method = "lm")+
  geom_smooth(aes(x = log(Intra_abundance+1), y = growth_rate, colour = "Intraspecific"), method = "lm")+
  ylab("Growth rate (mm/day)")+
  xlab("log(Neighbour abundance+1)")+
  theme_classic()+
  scale_colour_manual(values = c("Interspecific"="forestgreen", "Intraspecific"="orchid"), name = NULL)+
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text = element_text(size = 12),
        strip.text.x = element_text(size = 12, face = "italic"),
       # legend.text = element_text(size = 10),
       # legend.title = element_blank(),
        legend.position = "none")+
  facet_wrap(vars(Focal_sp))

############## Looking at just total neighbour abundance for Holsworth
#Can't figure it out :(

ggplot(growthmatchsimple, aes(x = log(No_neighbours), y = growth_rate))+
  geom_jitter(aes(colour = Focal_sp), alpha = 0.6, width = 0.05)+
  geom_smooth(colour = "grey24", method = "lm")+
  ylab("Growth rate (mm/day)")+
  xlab("log(Neighbour abundance)")+
  theme_classic()+
  theme(axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12),
        strip.text.x = element_text(size = 12, face = "italic"),
        # legend.text = element_text(size = 10),
        # legend.title = element_blank(),
        legend.position = "none")+
  facet_wrap(vars(Focal_sp))


#Plotting as above but by MD values instead - remember this is WET to DRY
ggplot(aes(x = MD, y = growth_rate), data = dbhnumeric) +
  geom_jitter(aes(colour = Focal_sp), alpha = 0.3, width = 10)+
  geom_smooth(colour = "grey24", method = "lm")+
  xlab("Aridity / Moisture deficit (mm)")+
  ylab("Growth rate (mm/day)")+
  theme_classic()+
  theme(axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12),
        strip.text.x = element_text(size = 12, face = "italic"),
        legend.position = "none")+
  facet_wrap(vars(Focal_sp))
# legend.text = element_text(size = 14),
# legend.title = element_blank()
# legend.position = "none"


intramod1 <- lm(growth_rate ~ log(Intra_abundance+1), growthmatchsimple)
summary(intramod1)
r.squaredGLMM(intramod1)
intermod1 <- lm(growth_rate ~ log(Inter_abundance+1), growthmatchsimple)
summary(intermod1)
r.squaredGLMM(intermod1)

specieslong <- c("Eucalyptus amygdalina", "Eucalyptus obliqua", "Eucalyptus ovata", "Eucalyptus viminalis")
for(i in 1:length(specieslong)){
  nam <- paste0("matchdata", specieslong[i])
  assign(nam, filter(growthmatchsimple, Focal_sp == specieslong[i]))
  print(specieslong[i])
  print(
    summary(
      lm(growth_rate ~ log(Inter_abundance+1), data = filter(growthmatchsimple, Focal_sp == specieslong[i]))))
}

ovatintra1 <- lm(growth_rate ~ log(Inter_abundance+1), data = filter(growthmatchsimple, Focal_sp == "Eucalyptus ovata"))
summary(ovatinter1)



##########Stats! Can't look at differences in water availability yet, not enough sites
mod1 <- lmer(growth_rate ~ log(Total_nbh_DBH) + Focal_sp + (1|DBH_cm) + (1|Plot), data = growthnbhsimple)
summary(mod1)
par(mfrow = c(2,2))
plot(mod1)

#Or is this the way to do the random effect? Really not sure. Not converging.
#mod4 <- lmer(log_growth10 ~ Total_nbh_DBH + Focal_sp + (Total_nbh_DBH|Site),
    #         data=growthnbhdata)
#summary(mod4)
#r.squaredGLMM(mod4)
##########From Piet's Tansley example
# NOT SURE ABOUT ANY OF THE BELOW!
#Add in a column of mean-centre temperatures using the scale function. Mean-centring of the
#environmental variable means that intercepts reflect average values for the population and individuals 
#So adding in neighbour crowding mean-centred column...
#Mean-centre the x variable (temperature)
# From here on, use the mean-centred temperature (ctemperature)
  
growthnbhsimple$cnbhdbh <- scale(growthnbhsimple$Total_nbh_DBH)
head(growthnbhdata)

#Right now the below isn't working with my model including Focal_sp
nbh_pred <- data.frame(cnbhdbh = seq(from =
               min(growthnbhsimple$cnbhdbh), to = max(growthnbhsimple$cnbhdbh), 
               length.out = 50))
nbh_pred$fit1.1 <- predict(mod1, newdata = nbh_pred, re.form =
                                     NA)
ggplot(nbh_pred, aes(x = cnbhdbh, y = fit1.1))+
  geom_line(data = growthnbhdata, aes(y = log_growth10, colour = Focal_sp))+
  geom_line(size = 2)+
    theme_classic()

##################### Modelling and plotting species as a random effect ####
mod3.1 <- lmer(log_growth10 ~ cnbhdbh + (1|Site/Plot) + (1|Focal_sp),
             data = growthnbhsimple)
summary(mod3.1)
r.squaredGLMM(mod3.1)
# Predict values based on the model fit using the predict function
nbh_pred$fit3.1 <- predict(mod3.1, newdata = nbh_pred, re.form =
                                         NA)
# Make a prediction for the population-level mean reaction norm
# and append it to the flowerdata dataset
growthnbhdata$pred_all3.1 <- predict(mod3.1, re.form = NA)
# Make predictions for each genotype-level reaction norm
growthnbhdata$pred_sp3.1 <- predict(mod3.1, re.form = ~(1|Focal_sp))
# Plot predicted genotype reaction norms over the raw data, along with the overall mean
ggplot(nbh_pred, aes(x = cnbhdbh, y = fit3.1)) +
  geom_line(data = growthnbhdata, aes(y = pred_sp3.1, group = Focal_sp, colour =
                                       Focal_sp), lty = 2) +
  geom_line(data = growthnbhdata,
              aes(y = log_growth10, group = Focal_sp, colour = Focal_sp)) +
  geom_line(size = 2) +
  theme_classic()
#This isn't allowing the slopes of the species to vary with nbh DBH though,
#so it really isn't great!!!
#So let's try a model that allows the slopes of the random species regressions to 
# vary across mean-centred nbh dbh

mod3.2 <- lmer(log_growth10 ~ cnbhdbh + (1|Site/Plot) + (1+cnbhdbh|Focal_sp),
               data = growthnbhdata)
summary(mod3.2)
r.squaredGLMM(mod3.2)
# Predict values based on the model fit using the predict function
nbh_pred$fit3.2 <- predict(mod3.2, newdata = nbh_pred, re.form =
                             NA)
# Make a prediction for the population-level mean reaction norm and append it to the flowerdata dataset
growthnbhdata$pred_all3.2 <- predict(mod3.2, re.form = NA)
# Make predictions for each genotype-level reaction norm
growthnbhdata$pred_sp3.2 <- predict(mod3.2, re.form = ~(1+cnbhdbh|Focal_sp))
# Plot predicted genotype reaction norms over the raw data, along with the overall mean
ggplot(nbh_pred, aes(x = cnbhdbh, y = fit3.2)) +
  geom_line(data = growthnbhdata, aes(y = pred_sp3.2, group = Focal_sp, colour =
                                        Focal_sp), lty = 2) +
  geom_line(data = growthnbhdata,
            aes(y = log_growth10, group = Focal_sp, colour = Focal_sp)) +
  geom_line(size = 2) +
  theme_classic()+
  ylab("log(Growth(mm))") + xlab("Mean-centred neighbour DBH")

#These slopes look great but just realised that they are sitting too high above the data
#From a quick google, I may need to build a partial residuals plot?
# To account for the effects of all of the control variables?
#https://cran.r-project.org/web/packages/jtools/vignettes/effect_plot.html

jtools::effect_plot(mod3.2, pred = cnbhdbh, interval = TRUE, partial.residuals = TRUE)

# Does adding Focal_sp as a random intercept and slope further improve model fit?
# Likelihood ratio test
chi2 <- 2*(summary(mod3.2)$logLik - summary(mod3.1)$logLik)
# The df difference between models can be checked by looking at the df within the
#models being compared
summary(mod3.1)$logLik
summary(mod3.2)$logLik
#Note that between model1.3 and model1.4 there is a change of 2 df, so the
# pchisq change needs to be specified with 2 df rather than 1 as in previous comparisons.
1-pchisq(chi2, 2)
AIC(mod3, mod3.1, mod3.2)
#Both AIC and LRT suggest that mod.3.2 is a better fit

############ Visualising more basic trends ####

#Plot growth by species by site
ggplot(dbhnumeric, aes(x = Focal_sp, y = growth_rate, color=Site)) +
  geom_boxplot()+
  #geom_point(position = (position_jitter(width = .2)))+
  theme_bw()+
facet_wrap(~ Focal_sp, ncol = 2, nrow = 2, scales="free")
#Why is there so little growth for AMYG at BOF?
growthAMYGBOF <- growthalldata %>% filter(Site == "BOF", Focal_sp == 'AMYG')

#Plot growth by site
#First line of code here reorders Sites according to MD values (so from driest to wettest!)
dbhnumeric %>% mutate(Site = fct_reorder(Site, MD)) %>%
  ggplot(aes(x = Site, y = growth_rate)) +
  geom_boxplot()+
  geom_jitter(alpha = 0.4, aes(colour = Focal_sp))+
  theme_bw()

#And separated by species
dbhnumeric %>% mutate(Site = fct_reorder(Site, MD)) %>%
  ggplot(aes(x = Site, y = growth_rate)) +
  geom_boxplot()+
  geom_jitter(alpha = 0.4)+
  theme_bw()+
  facet_wrap(~ Focal_sp, ncol = 2, nrow = 2, scales = "free")

#Plotting as above but by MD values instead - remember this is WET to DRY
  ggplot(aes(x = MD, y = growth_rate), data = dbhnumeric) +
    geom_jitter(aes(colour = Focal_sp), alpha = 0.4, width = 10)+
    geom_smooth(colour = "grey24", method = "lm")+
    xlab("Moisture deficit (mm)")+
    ylab("Growth rate (mm/day)")+
    theme_classic()+
    theme(axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          axis.text = element_text(size = 14),
          strip.text.x = element_text(size = 11, face = "italic"),
          legend.position = "none")+
    facet_wrap(vars(Focal_sp))
  
  
  ggplot(dbhnumeric, aes(x = MD, y = growth_rate))+
    geom_jitter(aes(color = Focal_sp), alpha = 0.6, width = 10)+
  geom_point(stat="summary", fun.y="mean",size=2) +
    geom_errorbar(stat="summary", fun.data="mean_se", size=0.8)+
    xlab("Moisture deficit (mm)")+
    ylab("Growth rate (mm/day)")+
    theme_classic()+
    my_theme

modelmd1 <- lm(growth_rate ~ MD, dbhnumeric)
summary(modelmd1)

for(i in 1:length(specieslist)){
  nam <- paste0("matchdata", specieslist[i])
  assign(nam, filter(dbhnumeric, Focal_sp == specieslist[i]))
  print(specieslist[i])
  print(
    summary(
      lm(growth_rate ~ MD, data = filter(dbhnumeric, Focal_sp == specieslist[i]))))
}

#R squared for ovata relationship
ovatmd1 <- lm(growth_rate ~ MD, data = filter(dbhnumeric, Focal_sp == "OVAT"))
summary(ovatmd1)
r.squaredGLMM(ovatmd1)

## Note wrong datasets from here on

#Filtering out GRA site
growthGRA <- growthdata2 %>% filter(Site == "GRA")
ggplot(growthGRA, aes(x = Growth_mm, y = Species)) +
  geom_boxplot()+
  geom_jitter(color = 'orange', position = (position_jitter(height = .2)))+
  theme_bw()
modelgra1 <- aov(Growth_mm ~ Species, data = growthGRA)
summary(modelgra1)
#There are signif. differences between Sites (but don't know which ones yet)

vimiGRA <- growthGRA %>% filter(Species == "E. viminalis") %>% select("Growth_mm")
amygGRA <- growthGRA %>% filter(Species == "E. amygdalina") %>% select("Growth_mm")
ovatGRA <- growthGRA %>% filter(Species == "E. ovata") %>% select("Growth_mm")
obliGRA <- growthGRA %>% filter(Species == "E. obliqua") %>% select("Growth_mm")
t.test(vimiGRA, amygGRA)
t.test(vimiGRA, ovatGRA)
t.test(obliGRA, ovatGRA)
t.test(vimiGRA, obliGRA)
t.test(amygGRA, obliGRA)
# So VIMI and AMYG grew signif. more than OVAT and OBLI, but not diff. from each other, and OBAT and OBLI not diff. from each other

meangrowth <- growthdata %>%
  group_by(Focal_sp) %>%
  summarise(mean_growth = mean(Growth_mm, na.rm= TRUE),
            sd_growth = sd(Growth_mm, na.rm=TRUE))

meangrowthbysite <- growthdata %>%
  group_by(Site, Focal_sp) %>%
  summarise(mean_growth = mean(Growth_mm, na.rm= TRUE),
            sd_growth = sd(Growth_mm, na.rm=TRUE))
#For Gravelly only
meangrowthGRA <- growthGRA %>%
  group_by(Species) %>%
  summarise(mean_wue = mean(Growth_mm, na.rm= TRUE),
            sd_wue = sd(Growth_mm, na.rm=TRUE))

#Stats -- all very confused, not sure which models to use
model1 <- lm(Growth_mm ~ Species, data = growthalldata)
summary(model1)

#Growth as a function of rainfall?
model2 <- lm(Growth_mm ~ PPT + Species, data = growthalldata)
summary(model2)

model3 <- lmer(Growth_mm ~ PPT + Species + (1|DBH_cm), data = growthalldata)
summary(model3)

model4 <- lm(Growth_mm ~ PPT + Species + (1|DBH_cm) + PPT*Species, data = growthalldata)
summary(model4)

model6 <- lmer(Growth_mm ~ PPT + (1|Species) + PPT*Species, data = growthalldata)
summary(model6)


#### Growth as a function of rainfall (PPT), PET or MD ####

ggplot(growthalldata, aes(x = PPT, y = Growth_mm))+
  geom_jitter(aes(color = Focal_sp))+
  geom_smooth()+
  theme_bw()+
  geom_errorbar(stat="summary", fun.data="mean_se",width=0,size=0.8)

ggplot(growthalldata, aes(x = MD, y = Growth_mm))+
  geom_point(aes(colour = Site), size=1.6) +
  theme_bw() +
  geom_point(stat="summary", fun.y="mean",size=2) +
  geom_errorbar(stat="summary", fun.data="mean_se",width=0,size=0.8)+
  facet_wrap(~ Focal_sp, ncol = 2, nrow = 2, scales = "free")

ggplot(amygdata, aes(x = PPT, y = Growth_mm))+
  geom_jitter(aes(color = Site))+
  geom_smooth()+
  theme_bw()+
  ggtitle("AMYG")
ggplot(oblidata, aes(x = PPT, y = Growth_mm))+
  geom_jitter(aes(color = Site))+
  geom_smooth()+
  theme_bw()+
  ggtitle("OBLI")
ggplot(ovatdata, aes(x = PPT, y = Growth_mm))+
  geom_jitter(aes(color = Site))+
  geom_smooth()+
  theme_bw()+
  ggtitle("OVAT")
ggplot(vimidata, aes(x = PPT, y = Growth_mm))+
  geom_jitter(aes(color = Site))+
  geom_smooth()+
  theme_bw()+
  ggtitle("VIMI")

pptmodel1 <- lm(Growth_mm ~ PPT, growthalldata)
summary(pptmodel1)

pptmodel1 <- aov(Growth_mm ~ PPT + Species, growthalldata)
summary(pptmodel1)

ggplot(growthalldata, aes(x = PPT, y = Growth_mm))+
  geom_jitter(aes(color = Site))+
  geom_smooth()+
  theme_bw()+
facet_wrap(~Species, ncol = 2, nrow = 2, scales="free") 


#### Basic linear model #### Stuff from Honours year, it works but not sure how to get CI yet
# Fit a linear model for the fixed effect of initial DBH on Growth
model1.1 <- lm(growth_rate ~ log_DBH, data = test)

# Check model summary and R squared values
summary(model1.1)

# Plot the raw data and overlay the fit of Model1.1
ggplot(dbhnumeric, aes(x = log_DBH, y = growth_rate))+ 
  geom_point(alpha = 0.4)+
  geom_abline(intercept = model1.1[1]$coefficients[1], 
              slope = model1.1[1]$coefficients[2], lwd = 1.2, colour = "forestgreen") + 
  theme_classic()
