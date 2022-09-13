### Eucalyptus Growth Tasmania Project 
# Alexandra Catling PhD Research
# Started 26/02/2021

###### Importing data and packages ####
source("data_preparations.R")

library(kableExtra)
library(sf)
#library(dplyr)
library(ggfortify)

#### PCA of soil ####
summary(soil_pca)
dev.off()
pdf("Output/soil-pca.pdf")
autoplot(soil_pca, label = TRUE, shape = TRUE,
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

## Plotting growth rate responses to PC1 and PC2
ggplot(growthnbhdata, aes(x = std_PC1, y = sqrt(growth_rate)))+
  geom_point(aes(colour = Period),  alpha = 0.5)+
  geom_smooth(method="lm", aes(colour = Period))+
  theme_classic()+
  ylab("sqrt(Growth rate (mm/day))")+
  xlab("std_PC1")+
  my_theme+
  facet_wrap(~Focal_sp)
ggplot(growthnbhdata, aes(x = std_PC2, y = sqrt(growth_rate)))+
  geom_point(aes(colour = Period),  alpha = 0.5)+
  geom_smooth(method="lm", aes(colour = Period))+
  theme_classic()+
  ylab("sqrt(Growth rate (mm/day))")+
  xlab("std_PC1")+
  my_theme+
  facet_wrap(~Focal_sp)

### Plotting GPS coordinates of trees
#Just check Google My Maps!
# mapfocaldata <- growthdata %>% select(Focal_sp, Site, Plot, Lat, Lon)
# my_sf <- st_as_sf(mapfocaldata, coords = c('Lon', 'Lat'))
# my_sf <- st_set_crs(my_sf, crs = 4326)
test <- my_sf %>% filter(Focal_sp == "VIMI", Site == "TMP")
view(test)
  ggplot(test) +
  geom_sf()+
  theme_bw()

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

#### Is initial DBH evenly distributed across species? ####
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
#### Does initial DBH predict growth rate? ####
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


#### Do growth rates differ across species? ####
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

#### What neighbours do we find at each site? ####
## Do this in a loop for all sites:
listnbhgravelly <- surveydata %>% filter(Site == "GRA") %>% 
  group_by(Neighbour_sp_ID) %>% filter(row_number() == 1) %>%
  select(Site, Neighbour_sp_ID)
## Or just this but not sorted:
listneighboursbysite <- surveydata %>% group_by(Site, Neighbour_sp_ID) %>% filter(row_number() == 1) %>%
  select(Site, Neighbour_sp_ID)
#### Do NCI, basal area or density (number of neighbours) vary by site? ####
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


#### Simple quadratic plots growth ~ NCI, period MD, long-term MD ####

### period rainfall
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

### period MD quadratic relationship?
## Only AMYG quadratic, others linear
for (i in 1:length(speciesabbrevlist)){
  print(speciesabbrevlist[i])
  growthquadperiodmd <- lmer(sqrt(growth_rate) ~ std_period_md + I(std_period_md^2) + (1|Site/Plot/Tree), data = filter(growthnbhdata, Focal_sp == speciesabbrevlist[i]))
  print(summary(growthquadperiodmd))
}
#Plotting if quadratic to double check
#This produces a plot that plots quadratic responses if the quadratic term is < 0.05, otherwise linear plot
dev.off()
pdf("Output/growth~period_md_if_quad.pdf", width=21, height=21)
par(mfrow=c(2,2))
par(mar=c(4,6,2,1))
par(pty="s")
for(i in 1:length(specieslist)){
  plotted.data<-as.data.frame(specieslist[i])
  plot(jitter(sqrt(plotted.data$growth_rate), amount = 0.05) ~ plotted.data$std_period_md, pch=19, col="grey60", ylab="Growth rate (sqrt, standardised)", xlab="Period MD (standardised)", cex.lab=2, cex.axis=2.00,tck=-0.01)
  mtext(paste(letters[i], ")", sep=""), side=2,line=1,adj=1.5,las=1, padj=-13, cex=1.5)
  title(main=bquote(italic(.(speciesnamelist[i]))), cex.main=2.5)
  x_to_plot<-seq.func(plotted.data$std_period_md) 
  model<-lmer(sqrt(growth_rate) ~ std_period_md + I(std_period_md^2) + (1|Site/Plot/Tree), plotted.data)
  model2<-lmer(sqrt(growth_rate) ~ std_period_md + (1|Site/Plot/Tree), plotted.data)
    if(summary(model)$coefficients[3,5]<0.05){
    preddata <- with(model, data.frame(1, x_to_plot, x_to_plot^2))
    plotted.pred <- glmm.predict(mod = model, newdat = preddata, se.mult = 1.96, logit_link=FALSE, log_link=FALSE, glmmTMB=FALSE)
    plot.CI.func(x.for.plot = x_to_plot, pred = plotted.pred$y, upper = plotted.pred$upper, lower = plotted.pred$lower, env.colour = "grey1", env.trans = 50, line.colour = "black", line.weight = 2, line.type = 1)
  }else{
    preddata <- with(model2, data.frame(1, x_to_plot))
    plotted.pred <- glmm.predict(mod = model2, newdat = preddata, se.mult = 1.96, logit_link=FALSE, log_link=FALSE, glmmTMB=FALSE)
    plot.CI.func(x.for.plot = x_to_plot, pred = plotted.pred$y, upper = plotted.pred$upper, lower = plotted.pred$lower, env.colour = "grey1", env.trans = 50, line.colour = "black", line.weight = 2, line.type = 1)
  }
}
dev.off()

### NCI quadratic relationship?
## All linear
for (i in 1:length(speciesabbrevlist)){
  print(speciesabbrevlist[i])
  growthquadnci <- lmer(sqrt(growth_rate) ~ std_total_nci + I(std_total_nci^2) + (1|Site/Plot/Tree), data = filter(growthnbhdata, Focal_sp == speciesabbrevlist[i]))
  print(summary(growthquadnci))
}
#Plotting if quadratic 
dev.off()
pdf("Output/growth~total_nci_if_quad.pdf", width=21, height=21)
par(mfrow=c(2,2))
par(mar=c(4,6,2,1))
par(pty="s")
for(i in 1:length(specieslist)){
  plotted.data<-as.data.frame(specieslist[i])
  plot(jitter(sqrt(plotted.data$growth_rate), amount = 0.05) ~ plotted.data$std_total_nci, pch=19, col="grey60", ylab="Growth rate (sqrt, standardised)", xlab="Total NCI (log, standardised)", cex.lab=2, cex.axis=2.00,tck=-0.01)
  mtext(paste(letters[i], ")", sep=""), side=2,line=1,adj=1.5,las=1, padj=-13, cex=1.5)
  title(main=bquote(italic(.(speciesnamelist[i]))), cex.main=2.5)
  x_to_plot<-seq.func(plotted.data$std_total_nci) 
  model<-lmer(sqrt(growth_rate) ~ std_total_nci + I(std_total_nci^2) + (1|Site/Plot/Tree), plotted.data)
  model2<-lmer(sqrt(growth_rate) ~ std_total_nci + (1|Site/Plot/Tree), plotted.data)
  if(summary(model)$coefficients[3,5]<0.05){
    preddata <- with(model, data.frame(1, x_to_plot, x_to_plot^2))
    plotted.pred <- glmm.predict(mod = model, newdat = preddata, se.mult = 1.96, logit_link=FALSE, log_link=FALSE, glmmTMB=FALSE)
    plot.CI.func(x.for.plot = x_to_plot, pred = plotted.pred$y, upper = plotted.pred$upper, lower = plotted.pred$lower, env.colour = "grey1", env.trans = 50, line.colour = "black", line.weight = 2, line.type = 1)
  }else{
    preddata <- with(model2, data.frame(1, x_to_plot))
    plotted.pred <- glmm.predict(mod = model2, newdat = preddata, se.mult = 1.96, logit_link=FALSE, log_link=FALSE, glmmTMB=FALSE)
    plot.CI.func(x.for.plot = x_to_plot, pred = plotted.pred$y, upper = plotted.pred$upper, lower = plotted.pred$lower, env.colour = "grey1", env.trans = 50, line.colour = "black", line.weight = 2, line.type = 1)
  }
}
dev.off()

### Long-term MD quadratic relationship?
## All linear
# problem with OVAT model?
for (i in 1:length(speciesabbrevlist)){
  print(speciesabbrevlist[i])
  growthquadnci <- lmer(sqrt(growth_rate) ~ std_md + I(std_md^2) + (1|Site/Plot/Tree), data = filter(growthnbhdata, Focal_sp == speciesabbrevlist[i]))
  print(summary(growthquadnci))
}
#Plotting if quadratic 
dev.off()
pdf("Output/growth~longterm_md_if_quad.pdf", width=21, height=21)
par(mfrow=c(2,2))
par(mar=c(4,6,2,1))
par(pty="s")
for(i in 1:length(specieslist)){
  plotted.data<-as.data.frame(specieslist[i])
  plot(jitter(sqrt(plotted.data$growth_rate), amount = 0.05) ~ plotted.data$std_md, pch=19, col="grey60", ylab="Growth rate (sqrt, standardised)", xlab="Long-term MD (standardised)", cex.lab=2, cex.axis=2.00,tck=-0.01)
  mtext(paste(letters[i], ")", sep=""), side=2,line=1,adj=1.5,las=1, padj=-13, cex=1.5)
  title(main=bquote(italic(.(speciesnamelist[i]))), cex.main=2.5)
  x_to_plot<-seq.func(plotted.data$std_md) 
  model<-lmer(sqrt(growth_rate) ~ std_md + I(std_md^2) + (1|Site/Plot/Tree), plotted.data)
  model2<-lmer(sqrt(growth_rate) ~ std_md + (1|Site/Plot/Tree), plotted.data)
  if(summary(model)$coefficients[3,5]<0.05){
    preddata <- with(model, data.frame(1, x_to_plot, x_to_plot^2))
    plotted.pred <- glmm.predict(mod = model, newdat = preddata, se.mult = 1.96, logit_link=FALSE, log_link=FALSE, glmmTMB=FALSE)
    plot.CI.func(x.for.plot = x_to_plot, pred = plotted.pred$y, upper = plotted.pred$upper, lower = plotted.pred$lower, env.colour = "grey1", env.trans = 50, line.colour = "black", line.weight = 2, line.type = 1)
  }else{
    preddata <- with(model2, data.frame(1, x_to_plot))
    plotted.pred <- glmm.predict(mod = model2, newdat = preddata, se.mult = 1.96, logit_link=FALSE, log_link=FALSE, glmmTMB=FALSE)
    plot.CI.func(x.for.plot = x_to_plot, pred = plotted.pred$y, upper = plotted.pred$upper, lower = plotted.pred$lower, env.colour = "grey1", env.trans = 50, line.colour = "black", line.weight = 2, line.type = 1)
  }
}
dev.off()


###### Research questions: climate vs competition main models #####
# How does sensitivity of growth to climate vary with competition?
#only looking at MD

####  Main models by species
### Total nci only model:
#growth ~ NCI + long-term climate + period climate + NCI*period climate +
#initial DBH + initial DBH*NCI + initial DBH*period climate +(1|Site/Plot/Tree)

### Intra and inter nci model:
#growth ~ NCI intra + NCI inter + long-term climate + period climate + 
#NCI intra*period climate + NCI inter*period climate +
#initial DBH + initial DBH*period climate + initial DBH*NCI intra + 
#initial DBH*NCI inter + (1|Site/Plot/Tree)

### AMYG ####

#initial model:
# amygmod1 <- lmer(sqrt(growth_rate) ~ std_total_nci + std_md + std_period_md + 
#                    std_total_nci*std_period_md + DBH_cm + DBH_cm*std_total_nci +
#                    DBH_cm*std_period_md + (1|Site/Plot/Tree), amygdata)
#removing DBH interactions from now, solves my vif problem!

#norm md: long-term 'norm' md calculated for growing months
#anomaly is period md calculated for growing months - norm_md

### Different possible climate models:
## 1) Norm + period + norm:NCI 
#std_norm_md + std_monthly_period_md + std_norm_md:std_total_nci

## 2) norm + anomalies + norm:NCI
# i. What matters is the normal climate of a site and/or the number/magnitude of anomalous events
#std_norm_md + std_anomaly + std_norm_md:std_total_nci

## 3) Norm + period + period:NCI
# i. What matters is the normal climate of a site plus what happens specifically in a given growth period layered on top
#std_norm_md + std_monthly_period_md + std_monthly_period_md:std_total_nci

## 4) Period + period:NCI
# Only growth period climate that matters
# std_monthly_period_md + std_monthly_period_md:NCI

# amygmod1 <- lmer(sqrt(growth_rate) ~ std_total_nci + std_norm_md + std_anomaly + 
#                    std_norm_md:std_total_nci + std_dbh + std_PC1 +
#                    (1|Site/Plot/Tree), amygdata)
#terrible residuals!

amygmod1 <- lmer(sqrt(growth_rate) ~ std_total_nci + std_norm_md + std_anomaly + 
                                      std_norm_md:std_total_nci + std_dbh + std_PC1 +
                                      (1|Site/Plot/Tree), amygdata)
amygmod1dharma <- simulateResiduals(amygmod1)
plot(amygmod1dharma)
#terrible
summary(amygmod1)
vif(amygmod1)
#great
#plot(amygmod1)

## Try removing period md (could replace with anomalies)
# amygmod1 <- lmer(sqrt(growth_rate) ~ std_total_nci + std_md + 
#                     + DBH_cm + DBH_cm*std_total_nci +
#                    (1|Site/Plot/Tree), amygdata)
# vif(amygmod1)

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

### intraspecific and interspecific NCIs model, mod2
amygmod2 <- lmer(sqrt(growth_rate) ~ std_intra_nci + std_inter_nci + std_norm_md + 
                   std_anomaly + std_norm_md:std_intra_nci + std_norm_md:std_inter_nci + 
                   std_dbh + PC1 + (1|Site/Plot/Tree), amygdata)
amygmod2dharma <- simulateResiduals(amygmod2)
plot(amygmod2dharma)
#terrible residuals
summary(amygmod2)
vif(amygmod1)
#great

### OBLI ####
oblimod1 <- lmer(sqrt(growth_rate) ~ std_total_nci + std_norm_md + std_anomaly + 
                   std_norm_md:std_total_nci + std_dbh + std_PC1 +
                   (1|Site/Plot/Tree), oblidata)
oblimod1dharma <- simulateResiduals(oblimod1)
plot(oblimod1dharma)
#okay
summary(oblimod1)
vif(oblimod1)
#great

oblimod2 <- lmer(sqrt(growth_rate) ~ std_intra_nci + std_inter_nci + std_norm_md + 
                   std_anomaly + std_norm_md:std_intra_nci + std_norm_md:std_inter_nci + 
                   std_dbh + PC1 + (1|Site/Plot/Tree), oblidata)
oblimod2dharma <- simulateResiduals(oblimod2)
plot(oblimod2dharma)
#okay
summary(oblimod2)
vif(oblimod1)
#great

### OVAT ####
ovatmod1 <- lmer(sqrt(growth_rate) ~ std_total_nci + std_norm_md + std_anomaly + 
                   std_norm_md:std_total_nci + std_dbh + std_PC1 +
                   (1|Site/Plot/Tree), ovatdata)
ovatmod1dharma <- simulateResiduals(ovatmod1)
plot(ovatmod1dharma)
#terrible residuals
summary(ovatmod1)
vif(ovatmod1)
#great
plot(ovatmod1)
### 
qqp(ranef(ovatmod1)$`Plot:Site`[,1])
qqp(ranef(ovatmod1)$`Plot:Site`[,1])

ovatmod2 <- lmer(sqrt(growth_rate) ~ std_intra_nci + std_inter_nci + std_norm_md + 
                   std_anomaly + std_norm_md:std_intra_nci + std_norm_md:std_inter_nci + 
                   std_dbh + PC1 + (1|Site/Plot/Tree), ovatdata)
ovatmod2dharma <- simulateResiduals(ovatmod2)
plot(ovatmod2dharma)
#terrible residuals
summary(ovatmod2)
vif(ovatmod1)
#great

### VIMI ####
vimimod1 <- lmer(sqrt(growth_rate) ~ std_total_nci + std_norm_md + std_anomaly + 
                   std_norm_md:std_total_nci + std_dbh + std_PC1 +
                   (1|Site/Plot/Tree), vimidata)
vimimod1dharma <- simulateResiduals(vimimod1)
plot(vimimod1dharma)
#great
summary(vimimod1)
vif(vimimod1)
#great

vimimod2 <- lmer(sqrt(growth_rate) ~ std_intra_nci + std_inter_nci + std_norm_md + 
                   std_anomaly + std_norm_md:std_intra_nci + std_norm_md:std_inter_nci + 
                   std_dbh + PC1 + (1|Site/Plot/Tree), vimidata)
vimimod2dharma <- simulateResiduals(vimimod2)
plot(vimimod2dharma)
#great
summary(vimimod2)
vif(vimimod1)
#great

### AIC - Is total NCI or intra and inter better model? 
#AMYG
AIC(amygmod1, amygmod2)
#mod 2 significantly better
AIC(oblimod1, oblimod2)
#mod 2 significantly better
AIC(ovatmod1, ovatmod2)
#mod 2 significantly better
AIC(vimimod1, vimimod2)
#mod 2 significantly better

#### Plotting growth rate ~ NCI from total NCI model ####
#all species
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

#### Growth rate ~ NCI in high or low long-term MD sites ####

### AMYG
# oh!! Is the reason why it looks like the lines don't fit the data because I am
#colouring it by <0 or >0 but modelling it for -1 and 1??
dev.off()
pdf("Output/AMYG_growth_rate~NCI+MD.pdf", width=21, height=21)
par(pty="s")
plot(sqrt(growth_rate) ~ std_total_nci, pch = ifelse(Period==1, 19, 17), col = alpha(ifelse(std_md>0, "forestgreen", "red"), 0.4), ylab="sqrt(Growth rate (mm/day))", xlab="Neighbourhood crowding index (standardised, sqrt)", tck=-0.01, cex= 2, cex.lab = 2, cex.axis = 2, amygdata)
model<-lmer(sqrt(growth_rate) ~ std_total_nci + std_md + std_period_md + 
              std_total_nci*std_period_md + DBH_cm + DBH_cm*std_total_nci +
              DBH_cm*std_period_md + (1|Site/Plot/Tree), amygdata)
x_to_plot<-seq.func(amygdata$std_total_nci)
#low md - red
#preddata <- with(model, data.frame(1, 0, 0, 0, 0, -1, x_to_plot, 0*x_to_plot, 0*0, 0*0, 0*0, -1*x_to_plot))
preddata <- with(model, data.frame(1, x_to_plot, -1, 0, mean(amygdata$DBH_cm), x_to_plot*0, x_to_plot*mean(amygdata$DBH_cm), 0*mean(amygdata$DBH_cm)))
plotted.pred <- glmm.predict(mod = model, newdat = preddata, se.mult = 1.96, logit_link=FALSE, log_link=FALSE, glmmTMB=FALSE)
plot.CI.func(x.for.plot = x_to_plot, pred = plotted.pred$y, upper = plotted.pred$upper, lower = plotted.pred$lower, env.colour = "grey1", env.trans = 50, line.colour = "red", line.weight = 2, line.type = 1)
#mean md - black
preddata <- with(model, data.frame(1, x_to_plot, 0, 0, mean(amygdata$DBH_cm), x_to_plot*0, x_to_plot*mean(amygdata$DBH_cm), 0*mean(amygdata$DBH_cm)))
plotted.pred <- glmm.predict(mod = model, newdat = preddata, se.mult = 1.96, logit_link=FALSE, log_link=FALSE, glmmTMB=FALSE)
plot.CI.func(x.for.plot = x_to_plot, pred = plotted.pred$y, upper = plotted.pred$upper, lower = plotted.pred$lower, env.colour = "grey1", env.trans = 50, line.colour = "black", line.weight = 2, line.type = 1)
#high md - forest green
preddata <- with(model, data.frame(1, x_to_plot, 1, 0, 0, x_to_plot*0, x_to_plot*0, 0*0))
plotted.pred <- glmm.predict(mod = model, newdat = preddata, se.mult = 1.96, logit_link=FALSE, log_link=FALSE, glmmTMB=FALSE)
plot.CI.func(x.for.plot = x_to_plot, pred = plotted.pred$y, upper = plotted.pred$upper, lower = plotted.pred$lower, env.colour = "grey1", env.trans = 50, line.colour = "forestgreen", line.weight = 2, line.type = 1)
dev.off()

### Growth rate ~ NCI given period climate ####
#Why aren't lines fitting the data?? Fix this
dev.off()
pdf("Output/AMYG_growth_rate~NCI+period_MD.pdf", width=21, height=21)
par(pty="s")
plot(sqrt(growth_rate) ~ std_total_nci, pch = ifelse(Period==1, 19, 17), col = alpha(ifelse(std_period_md>0, "forestgreen", "red"), 0.4), ylab="sqrt(Growth rate (mm/day))", xlab="Neighbourhood crowding index (standardised, sqrt)", tck=-0.01, cex= 2, cex.lab = 2, cex.axis = 2, amygdata)
model<-lmer(sqrt(growth_rate) ~ std_total_nci + std_md + std_period_md + 
              std_total_nci*std_period_md + std_dbh + std_dbh*std_total_nci +
              std_dbh*std_period_md + (1|Site/Plot/Tree), amygdata)
x_to_plot_low<-seq.func(amygdata$std_total_nci[amygdata$std_period_md<0])
x_to_plot_high<-seq.func(amygdata$std_total_nci[amygdata$std_period_md>0])
#low md - red
#fixef(model)
preddata <- with(model, data.frame(1, x_to_plot_low, 0, -1, 0, x_to_plot_low*-1, x_to_plot_low*0, -1*0))
plotted.pred <- glmm.predict(mod = model, newdat = preddata, se.mult = 1.96, logit_link=FALSE, log_link=FALSE, glmmTMB=FALSE)
plot.CI.func(x.for.plot = x_to_plot_low, pred = plotted.pred$y, upper = plotted.pred$upper, lower = plotted.pred$lower, env.colour = "grey1", env.trans = 50, line.colour = "red", line.weight = 2, line.type = 1)
#mean md - black
# preddata <- with(model, data.frame(1, x_to_plot_high, 0, 0, 0, x_to_plot_high*0, x_to_plot_high*0, 0*0))
# plotted.pred <- glmm.predict(mod = model, newdat = preddata, se.mult = 1.96, logit_link=FALSE, log_link=FALSE, glmmTMB=FALSE)
# plot.CI.func(x.for.plot = x_to_plot_high, pred = plotted.pred$y, upper = plotted.pred$upper, lower = plotted.pred$lower, env.colour = "grey1", env.trans = 50, line.colour = "black", line.weight = 2, line.type = 1)
#high md - forest green
preddata <- with(model, data.frame(1, x_to_plot_high, 0, 1, 0, x_to_plot_high*1, x_to_plot_high*0, 1*0))
plotted.pred <- glmm.predict(mod = model, newdat = preddata, se.mult = 1.96, logit_link=FALSE, log_link=FALSE, glmmTMB=FALSE)
plot.CI.func(x.for.plot = x_to_plot_high, pred = plotted.pred$y, upper = plotted.pred$upper, lower = plotted.pred$lower, env.colour = "grey1", env.trans = 50, line.colour = "forestgreen", line.weight = 2, line.type = 1)
dev.off()

### Do this in a loop for all species
## with mean is misleading because extrapolating mean values for all x values

dev.off()
pdf("Output/panel_growth~NCI+period_md_two_levels.pdf", width=21, height=21)
par(mfrow=c(2,2))
par(mar=c(4,6,2,1))
par(pty="s")
for(i in 1:length(specieslist)){
  plotted.data<-as.data.frame(specieslist[i])
  plot(sqrt(growth_rate) ~ std_total_nci, pch = ifelse(Period==1, 19, 17), col = alpha(ifelse(std_period_md>0, "red", "forestgreen"), 0.4), ylab="sqrt(Growth rate (mm/day))", xlab="Neighbourhood crowding index (standardised, sqrt)", tck=-0.01, cex= 2, cex.lab = 2, cex.axis = 2, plotted.data)
  mtext(paste(letters[i], ")", sep=""), side=2,line=1,adj=1.5,las=1, padj=-13, cex=1.5)
  title(main=bquote(italic(.(speciesnamelist[i]))), cex.main=2.5)
  model<-lmer(sqrt(growth_rate) ~ std_total_nci + std_md + std_period_md + 
                std_total_nci*std_period_md + std_dbh + (1|Site/Plot/Tree), plotted.data)
  x_to_plot_low<-seq.func(plotted.data$std_total_nci[plotted.data$std_period_md<0])
  #x_to_plot_mean<-seq.func(plotted.data$std_total_nci)
  x_to_plot_high<-seq.func(plotted.data$std_total_nci[plotted.data$std_period_md>0])
  #low md - wet - green
  preddata <- with(model, data.frame(1, x_to_plot_low, 0, -1, 0, x_to_plot_low*-1))
  plotted.pred <- glmm.predict(mod = model, newdat = preddata, se.mult = 1.96, logit_link=FALSE, log_link=FALSE, glmmTMB=FALSE)
  plot.CI.func(x.for.plot = x_to_plot_low, pred = plotted.pred$y, upper = plotted.pred$upper, lower = plotted.pred$lower, env.colour = "grey1", env.trans = 50, line.colour = "forestgreen", line.weight = 2, line.type = 1)
  #mean md - black
  # preddata <- with(model, data.frame(1, x_to_plot_low, 0, 0, mean(plotted.data$DBH_cm), x_to_plot_low*0, x_to_plot_low*mean(plotted.data$DBH_cm), 0*mean(plotted.data$DBH_cm)))
  # plotted.pred <- glmm.predict(mod = model, newdat = preddata, se.mult = 1.96, logit_link=FALSE, log_link=FALSE, glmmTMB=FALSE)
  # plot.CI.func(x.for.plot = x_to_plot_mean, pred = plotted.pred$y, upper = plotted.pred$upper, lower = plotted.pred$lower, env.colour = "grey1", env.trans = 50, line.colour = "black", line.weight = 2, line.type = 1)
  #high md - dry - red
  preddata <- with(model, data.frame(1, x_to_plot_high, 0, 1, 0, x_to_plot_high*1))
  plotted.pred <- glmm.predict(mod = model, newdat = preddata, se.mult = 1.96, logit_link=FALSE, log_link=FALSE, glmmTMB=FALSE)
  plot.CI.func(x.for.plot = x_to_plot_high, pred = plotted.pred$y, upper = plotted.pred$upper, lower = plotted.pred$lower, env.colour = "grey1", env.trans = 50, line.colour = "red", line.weight = 2, line.type = 1)
  }
dev.off()

## Not sure why OVAT trendlines don't fit the data, all other sp seem fine
#OVAT
dev.off()
pdf("Output/OVAT_growth_rate~NCI+period_MD.pdf", width=21, height=21)
par(pty="s")
plot(sqrt(growth_rate) ~ std_total_nci, pch = ifelse(Period==1, 19, 17), col = alpha(ifelse(std_period_md>0, "forestgreen", "red"), 0.4), ylab="sqrt(Growth rate (mm/day))", xlab="Neighbourhood crowding index (standardised, sqrt)", tck=-0.01, cex= 2, cex.lab = 2, cex.axis = 2, ovatdata)
model<-lmer(sqrt(growth_rate) ~ std_total_nci + std_md + std_period_md + 
              std_total_nci*std_period_md + DBH_cm + DBH_cm*std_total_nci +
              DBH_cm*std_period_md + (1|Site/Plot/Tree), ovatdata)
x_to_plot_low<-seq.func(ovatdata$std_total_nci[ovatdata$std_period_md<0])
x_to_plot_high<-seq.func(ovatdata$std_total_nci[ovatdata$std_period_md>0])
#low md - red
#fixef(model)
preddata <- with(model, data.frame(1, x_to_plot_low, 0, -1, mean(ovatdata$DBH_cm), x_to_plot_low*-1, x_to_plot_low*mean(ovatdata$DBH_cm), -1*mean(ovatdata$DBH_cm)))
plotted.pred <- glmm.predict(mod = model, newdat = preddata, se.mult = 1.96, logit_link=FALSE, log_link=FALSE, glmmTMB=FALSE)
plot.CI.func(x.for.plot = x_to_plot_low, pred = plotted.pred$y, upper = plotted.pred$upper, lower = plotted.pred$lower, env.colour = "grey1", env.trans = 50, line.colour = "red", line.weight = 2, line.type = 1)
#high md - forest green
preddata <- with(model, data.frame(1, x_to_plot_high, 0, 1, mean(ovatdata$DBH_cm), x_to_plot_high*1, x_to_plot_high*mean(ovatdata$DBH_cm), 1*mean(ovatdata$DBH_cm)))
plotted.pred <- glmm.predict(mod = model, newdat = preddata, se.mult = 1.96, logit_link=FALSE, log_link=FALSE, glmmTMB=FALSE)
plot.CI.func(x.for.plot = x_to_plot_high, pred = plotted.pred$y, upper = plotted.pred$upper, lower = plotted.pred$lower, env.colour = "grey1", env.trans = 50, line.colour = "forestgreen", line.weight = 2, line.type = 1)
dev.off()

#VIMI
#Vimi has NAs for DBH, need to remove these first
dev.off()
pdf("Output/VIMI_growth_rate~NCI+period_MD.pdf", width=21, height=21)
par(pty="s")
plot(sqrt(growth_rate) ~ std_total_nci, pch = ifelse(Period==1, 19, 17), col = alpha(ifelse(std_period_md>0, "forestgreen", "red"), 0.4), ylab="sqrt(Growth rate (mm/day))", xlab="Neighbourhood crowding index (standardised, sqrt)", tck=-0.01, cex= 2, cex.lab = 2, cex.axis = 2, vimidata)
model<-lmer(sqrt(growth_rate) ~ std_total_nci + std_md + std_period_md + 
              std_total_nci*std_period_md + DBH_cm + DBH_cm*std_total_nci +
              DBH_cm*std_period_md + (1|Site/Plot/Tree), vimidata)
x_to_plot_low<-seq.func(vimidata$std_total_nci[vimidata$std_period_md<0])
x_to_plot_high<-seq.func(vimidata$std_total_nci[vimidata$std_period_md>0])
#low md - red
#fixef(model)
preddata <- with(model, data.frame(1, x_to_plot_low, 0, -1, mean(vimidata$DBH_cm), x_to_plot_low*-1, x_to_plot_low*mean(vimidata$DBH_cm), -1*mean(vimidata$DBH_cm)))
plotted.pred <- glmm.predict(mod = model, newdat = preddata, se.mult = 1.96, logit_link=FALSE, log_link=FALSE, glmmTMB=FALSE)
plot.CI.func(x.for.plot = x_to_plot_low, pred = plotted.pred$y, upper = plotted.pred$upper, lower = plotted.pred$lower, env.colour = "grey1", env.trans = 50, line.colour = "red", line.weight = 2, line.type = 1)
#high md - forest green
preddata <- with(model, data.frame(1, x_to_plot_low, 0, 1, mean(vimidata$DBH_cm), x_to_plot_low*1, x_to_plot_low*mean(vimidata$DBH_cm), 1*mean(vimidata$DBH_cm)))
plotted.pred <- glmm.predict(mod = model, newdat = preddata, se.mult = 1.96, logit_link=FALSE, log_link=FALSE, glmmTMB=FALSE)
plot.CI.func(x.for.plot = x_to_plot_high, pred = plotted.pred$y, upper = plotted.pred$upper, lower = plotted.pred$lower, env.colour = "grey1", env.trans = 50, line.colour = "forestgreen", line.weight = 2, line.type = 1)
dev.off()

### ggplot
ggplot(growthnbhdata, aes(x = std_total_nci, y = sqrt(growth_rate), colour = site_climate))+
  geom_point(alpha = 0.3)+
  geom_smooth(method="lm")+
  theme_classic()+
  facet_wrap(~Focal_sp)

## boxplot?
ggplot(growthnbhdata, aes(x = site_climate, y = sqrt(growth_rate)))+
  geom_boxplot()+
  geom_jitter(alpha = 0.3)+
  geom_smooth(method="lm")+
  theme_classic()+
  facet_wrap(~Focal_sp)

ggplot(amygdata, aes(x = std_total_nci, y = sqrt(growth_rate), colour = Site))+
  geom_point()+
  theme_classic()

## grouped by growth period
growthnbhdata %>% filter(Period == 2) %>% 
ggplot(aes(x = std_total_nci, y = sqrt(growth_rate), colour = site_climate))+
  geom_point(alpha = 0.3)+
  geom_smooth(method="lm")+
  theme_classic()+
  facet_wrap(~Focal_sp)

## Growth rate ~ anomaly
growthnbhdata %>% 
  ggplot(aes(x = anomaly, y = sqrt(growth_rate), colour = site_climate))+
  geom_point(alpha = 0.3)+
  geom_smooth(method="lm")+
  theme_classic()+
  facet_wrap(~Focal_sp)

growthnbhdata %>% 
  ggplot(aes(x = anomaly, y = sqrt(growth_rate)))+
  geom_point(alpha = 0.3)+
  geom_smooth(method='lm')+
  theme_classic()+
  facet_wrap(~Focal_sp)

## Anomaly ~ norm MD
growthnbhdata %>% 
  ggplot(aes(x = norm_md, y = anomaly, colour = site_climate))+
  geom_point(alpha = 0.3)+
  theme_classic()+
  facet_wrap(~Focal_sp)

### Growth rate ~ NCI given high or low norm_md UPDATED ####
growthnbhdata %>% 
  ggplot(aes(x = std_total_nci, y = sqrt(growth_rate), colour = site_climate))+
  geom_point(alpha = 0.3)+
  geom_smooth(method="lm")+
  theme_classic()+
  facet_wrap(~Focal_sp)

###
dev.off()
pdf("Output/panel_growth~NCI+norm_md.pdf", width=21, height=21)
par(mfrow=c(2,2))
par(mar=c(4,6,2,1))
par(pty="s")
for(i in 1:length(specieslist)){
  plotted.data<-as.data.frame(specieslist[i])
  plot(sqrt(growth_rate) ~ std_total_nci, pch = ifelse(Period==1, 19, 17), col = alpha(ifelse(std_norm_md>0, "red", "forestgreen"), 0.4), ylab="sqrt(Growth rate (mm/day))", xlab="Neighbourhood crowding index (standardised, sqrt)", tck=-0.01, cex= 2, cex.lab = 2, cex.axis = 2, plotted.data)
  mtext(paste(letters[i], ")", sep=""), side=2,line=1,adj=1.5,las=1, padj=-13, cex=1.5)
  title(main=bquote(italic(.(speciesnamelist[i]))), cex.main=2.5)
  model<-lmer(sqrt(growth_rate) ~ std_total_nci + std_norm_md + std_anomaly + 
                std_norm_md:std_total_nci + std_dbh + std_PC1 + (1|Site/Plot/Tree), plotted.data)
  x_to_plot_low<-seq.func(plotted.data$std_total_nci[plotted.data$std_norm_md<0])
  x_to_plot_high<-seq.func(plotted.data$std_total_nci[plotted.data$std_norm_md>0])
  #low md - wet - green
  preddata <- with(model, data.frame(1, x_to_plot_low, -1, 0, 0, 0, -1*x_to_plot_low))
  plotted.pred <- glmm.predict(mod = model, newdat = preddata, se.mult = 1.96, logit_link=FALSE, log_link=FALSE, glmmTMB=FALSE)
  plot.CI.func(x.for.plot = x_to_plot_low, pred = plotted.pred$y, upper = plotted.pred$upper, lower = plotted.pred$lower, env.colour = "grey1", env.trans = 50, line.colour = "forestgreen", line.weight = 2, line.type = 1)
  #high md - dry - red
  preddata <- with(model, data.frame(1, x_to_plot_high, 1, 0, 0, 0, 1*x_to_plot_high))
  plotted.pred <- glmm.predict(mod = model, newdat = preddata, se.mult = 1.96, logit_link=FALSE, log_link=FALSE, glmmTMB=FALSE)
  plot.CI.func(x.for.plot = x_to_plot_high, pred = plotted.pred$y, upper = plotted.pred$upper, lower = plotted.pred$lower, env.colour = "grey1", env.trans = 50, line.colour = "red", line.weight = 2, line.type = 1)
}
dev.off()


#### Growth rate ~ period MD in high or low neighbourhood crowding ####
dev.off()
pdf("Output/panel_growth~period MD+NCI.pdf", width=21, height=21)
par(mfrow=c(2,2))
par(mar=c(4,6,2,1))
par(pty="s")
for(i in 1:length(specieslist)){
  plotted.data<-as.data.frame(specieslist[i])
  plot(jitter(sqrt(growth_rate), amount = 0.05) ~ std_period_md, pch = ifelse(Period==1, 19, 17), col = alpha(ifelse(std_total_nci>0, "forestgreen", "red"), 0.4), ylab="sqrt(Growth rate (mm/day))", xlab="Period MD (standardised)", tck=-0.01, cex= 2, cex.lab = 2, cex.axis = 2, plotted.data)
  mtext(paste(letters[i], ")", sep=""), side=2,line=1,adj=1.5,las=1, padj=-13, cex=1.5)
  title(main=bquote(italic(.(speciesnamelist[i]))), cex.main=2.5)
  model<-lmer(sqrt(growth_rate) ~ std_total_nci + std_md + std_period_md + 
                std_total_nci*std_period_md + DBH_cm + DBH_cm*std_total_nci +
                DBH_cm*std_period_md + (1|Site/Plot/Tree), plotted.data)
  x_to_plot_low<-seq.func(plotted.data$std_period_md[plotted.data$std_total_nci<0])
  x_to_plot_high<-seq.func(plotted.data$std_period_md[plotted.data$std_total_nci>0])
  #low NCI - red
  preddata <- with(model, data.frame(1, -1, 0, x_to_plot_low, mean(plotted.data$DBH_cm), -1*x_to_plot_low, -1*mean(plotted.data$DBH_cm), x_to_plot_low*mean(plotted.data$DBH_cm)))
  plotted.pred <- glmm.predict(mod = model, newdat = preddata, se.mult = 1.96, logit_link=FALSE, log_link=FALSE, glmmTMB=FALSE)
  plot.CI.func(x.for.plot = x_to_plot_low, pred = plotted.pred$y, upper = plotted.pred$upper, lower = plotted.pred$lower, env.colour = "grey1", env.trans = 50, line.colour = "red", line.weight = 2, line.type = 1)
  #high NCI - forest green
  preddata <- with(model, data.frame(1, 1, 0, x_to_plot_high, mean(plotted.data$DBH_cm), 1*x_to_plot_high, 1*mean(plotted.data$DBH_cm), x_to_plot_high*mean(plotted.data$DBH_cm)))
  plotted.pred <- glmm.predict(mod = model, newdat = preddata, se.mult = 1.96, logit_link=FALSE, log_link=FALSE, glmmTMB=FALSE)
  plot.CI.func(x.for.plot = x_to_plot_high, pred = plotted.pred$y, upper = plotted.pred$upper, lower = plotted.pred$lower, env.colour = "grey1", env.trans = 50, line.colour = "forestgreen", line.weight = 2, line.type = 1)
}
dev.off()

#### Growth rate ~ MD boxplot ####
ggplot(growthnbhdata, aes(x = std_norm_md, y = sqrt(growth_rate), color=site_climate)) +
  geom_boxplot()+
  geom_point(position = (position_jitter(width = .2)))+
  theme_bw()+
  facet_wrap(~Focal_sp, scales="fixed")

ggplot(growthnbhdata, aes(x = site_climate, y = sqrt(growth_rate))) +
  geom_boxplot()+
  geom_point(position = (position_jitter(width = .2)), alpha=0.4)+
  theme_bw()+
  facet_wrap(~Focal_sp, scales="fixed")

growthmd <- lmer(sqrt(growth_rate) ~ site_climate + std_PC1 + (1|Site/Plot/Tree), amygdata)
summary(growthmd)
growthmd <- lmer(sqrt(growth_rate) ~ site_climate + std_PC1 + (1|Site/Plot/Tree), oblidata)
summary(growthmd)
growthmd <- lmer(sqrt(growth_rate) ~ site_climate + std_PC1 + (1|Site/Plot/Tree), ovatdata)
summary(growthmd)
growthmd <- lmer(sqrt(growth_rate) ~ site_climate + std_PC1 + (1|Site/Plot/Tree), vimidata)
summary(growthmd)


#### All species - plotting interaction between climate and NCI ####

# #### Example I worked through with John
# #No significant interaction between PPT and NCI
# # PPT is x
# # NCI is set at -1, 0 or 1 (1 sd low, mean, or 1 sd high)
# model1<-lmer(sqrt(growth_rate) ~ std_dbh + Focal_sp + std_ppt + 
#               std_total_nci + std_dbh*std_total_nci + Focal_sp:std_dbh + std_ppt:std_total_nci +
#               (1|Site/Plot/Tree), growthnbhdata)
# 
# with(growthnbhdata, plot(jitter(sqrt(growth_rate), amount = 0.05) ~ std_ppt, col = ifelse(std_total_nci>0, "red", "blue")))
# #One sd low NCI
# curve(cbind(1, 0, 0, 0, 0, x, -1, 0*0, 0*0, 0*0, 0*0, x*-1)%*%fixef(model1), add = TRUE, col = "red")
# #One sd high NCI
# curve(cbind(1, 0, 0, 0, 0, x, 1, 0*0, 0*0, 0*0, 0*0, x*1)%*%fixef(model1), add = TRUE, col = "blue")
# #Mean NCI
# curve(cbind(1, 0, 0, 0, 0, x, 0, 0*0, 0*0, 0*0, 0*0, x*0)%*%fixef(model1), add = TRUE, col = "black")
# ###
# 
# dev.off()
# pdf("Output/growth_rate~PPT+NCI.pdf", width=21, height=21)
# par(pty="s")
# plot(sqrt(growth_rate) ~ jitter(std_ppt, 1), pch=19, col=alpha("grey60", 0.2), ylab="sqrt(Growth rate (mm/day))", xlab="Mean annual precipitation (standardised)", tck=-0.01, cex= 2, cex.lab = 2, cex.axis = 2, growthnbhdata)
# model<-lmer(sqrt(growth_rate) ~ std_dbh + Focal_sp + std_ppt + 
#               std_total_nci + std_dbh*std_total_nci + Focal_sp:std_dbh + std_ppt:std_total_nci +
#               (1|Site/Plot/Tree), growthnbhdata)
# x_to_plot<-seq.func(growthnbhdata$std_ppt)
# #mean NCI - black
# preddata <- with(model, data.frame(1, 0, 0, 0, 0, x_to_plot, 0, 0*0, 0*0, 0*0, 0*0, x_to_plot*0))
# plotted.pred <- glmm.predict(mod = model, newdat = preddata, se.mult = 1.96, logit_link=FALSE, log_link=FALSE, glmmTMB=FALSE)
# plot.CI.func(x.for.plot = x_to_plot, pred = plotted.pred$y, upper = plotted.pred$upper, lower = plotted.pred$lower, env.colour = "grey1", env.trans = 50, line.colour = "black", line.weight = 2, line.type = 1)
# #low NCI - blue
# preddata <- with(model, data.frame(1, 0, 0, 0, 0, x_to_plot, -1, 0*0, 0*0, 0*0, 0*0, x_to_plot*-1))
# plotted.pred <- glmm.predict(mod = model, newdat = preddata, se.mult = 1.96, logit_link=FALSE, log_link=FALSE, glmmTMB=FALSE)
# plot.CI.func(x.for.plot = x_to_plot, pred = plotted.pred$y, upper = plotted.pred$upper, lower = plotted.pred$lower, env.colour = "grey1", env.trans = 50, line.colour = "blue", line.weight = 2, line.type = 1)
# #high NCI - forest green
# preddata <- with(model, data.frame(1, 0, 0, 0, 0, x_to_plot, 1, 0*0, 0*0, 0*0, 0*0, x_to_plot*1))
# plotted.pred <- glmm.predict(mod = model, newdat = preddata, se.mult = 1.96, logit_link=FALSE, log_link=FALSE, glmmTMB=FALSE)
# plot.CI.func(x.for.plot = x_to_plot, pred = plotted.pred$y, upper = plotted.pred$upper, lower = plotted.pred$lower, env.colour = "grey1", env.trans = 50, line.colour = "forestgreen", line.weight = 2, line.type = 1)
# dev.off()
# 
# ## Repeating the above but for period MD
# dev.off()
# pdf("Output/growth_rate~MD+NCI.pdf", width=21, height=21)
# par(pty="s")
# plot(sqrt(growth_rate) ~ jitter(std_ppt, 1), pch=19, col=alpha("grey60", 0.2), ylab="sqrt(Growth rate (mm/day))", xlab="Mean annual precipitation (standardised)", tck=-0.01, cex= 2, cex.lab = 2, cex.axis = 2, growthnbhdata)
# model<-lmer(sqrt(growth_rate) ~ std_dbh + Focal_sp + std_ppt + 
#               std_total_nci + std_dbh*std_total_nci + Focal_sp:std_dbh + std_ppt:std_total_nci +
#               (1|Site/Plot/Tree), growthnbhdata)
# x_to_plot<-seq.func(growthnbhdata$std_ppt)
# #mean NCI - black
# preddata <- with(model, data.frame(1, 0, 0, 0, 0, x_to_plot, 0, 0*0, 0*0, 0*0, 0*0, x_to_plot*0))
# plotted.pred <- glmm.predict(mod = model, newdat = preddata, se.mult = 1.96, logit_link=FALSE, log_link=FALSE, glmmTMB=FALSE)
# plot.CI.func(x.for.plot = x_to_plot, pred = plotted.pred$y, upper = plotted.pred$upper, lower = plotted.pred$lower, env.colour = "grey1", env.trans = 50, line.colour = "black", line.weight = 2, line.type = 1)
# #low NCI - blue
# preddata <- with(model, data.frame(1, 0, 0, 0, 0, x_to_plot, -1, 0*0, 0*0, 0*0, 0*0, x_to_plot*-1))
# plotted.pred <- glmm.predict(mod = model, newdat = preddata, se.mult = 1.96, logit_link=FALSE, log_link=FALSE, glmmTMB=FALSE)
# plot.CI.func(x.for.plot = x_to_plot, pred = plotted.pred$y, upper = plotted.pred$upper, lower = plotted.pred$lower, env.colour = "grey1", env.trans = 50, line.colour = "blue", line.weight = 2, line.type = 1)
# #high NCI - forest green
# preddata <- with(model, data.frame(1, 0, 0, 0, 0, x_to_plot, 1, 0*0, 0*0, 0*0, 0*0, x_to_plot*1))
# plotted.pred <- glmm.predict(mod = model, newdat = preddata, se.mult = 1.96, logit_link=FALSE, log_link=FALSE, glmmTMB=FALSE)
# plot.CI.func(x.for.plot = x_to_plot, pred = plotted.pred$y, upper = plotted.pred$upper, lower = plotted.pred$lower, env.colour = "grey1", env.trans = 50, line.colour = "forestgreen", line.weight = 2, line.type = 1)
# dev.off()

### Intra- and inter-specific neighbour effects on growth ####
ggplot(growthnbhdata)+
  geom_jitter(aes(x = log_p1_inter_nci, y = sqrt(growth_rate), colour = "Heterospecific"), alpha = 0.4, width = 0.05)+
  geom_jitter(aes(x = log_p1_intra_nci, y = sqrt(growth_rate), colour = "Conspecific"), alpha = 0.4, width = 0.05)+
  geom_smooth(aes(x = log_p1_inter_nci, y = sqrt(growth_rate), colour = "Heterospecific"), method = "lm")+
  geom_smooth(aes(x = log_p1_intra_nci, y = sqrt(growth_rate), colour = "Conspecific"), method = "lm")+
  ylab("sqrt(Growth rate (mm/day))")+
  xlab("log(neighbourhood crowding+1)")+
  theme_classic()+
  scale_colour_manual(values = c("Heterospecific"="forestgreen", "Conspecific"="orchid"), name = NULL)+
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text = element_text(size = 12),
        strip.text.x = element_text(size = 12, face = "italic"),
        legend.text = element_text(size = 10),
        legend.title = element_blank())+
  facet_wrap(vars(Focal_sp), scales="fixed")

ggplot(onerowdata, aes(x = log_p1_intra_nci, y = log_p1_inter_nci, colour = Site))+
  geom_point(alpha = 0.3)+
  theme_classic()+
  facet_wrap(~Focal_sp)

#### 31/08/22

#not working because different # of points
ggplot(growthnbhdata, aes(x = Growth[Period==1], y = Growth[Period==2]))+
  geom_point(alpha = 0.3)+
  theme_classic()


###



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
col_order <- c('Site', 'Norm PPT', 'Norm MD', 'Actual MD 1', 'Anomaly 1', 'Norm MD 2', 'Actual MD 2', 'Anomaly 2')
climatetable2 <- climatetable2[, col_order]
#Can't figure out how to do names like this, problem with duplicated col names
#colnames(climatetable2) <- c('Site', 'Norm PPT', 'Norm MD', 'Actual MD', 'Anomaly', 'Norm MD', 'Actual MD', 'Anomaly')

  climatetable2 %>% select(-(`Norm PPT`)) %>%
  kbl(caption = "<b>Supplementary 1</b>. Long-term average ('norm') moisture deficit (MD, precipitation - evapotranspiration), MD over the growth period ('actual'), and 
  their difference ('anomaly') at each site by growth period (1 or 2). Long-term values downloaded 
  from the Global Aridity and PET Database (Zomer et al. 2006) based on 1960-1990 WorldClim climate averages (Hijmans et al. 2005). Actual MD values calculated from 
  BOM (ref*). MER is Mersey River Conservation Area, DOG is Dogs Head Regional Reserve, TMP is Tasman National Park, BOF is Doctors Peak Regional Reserve, 
      EPF is Tom Gibson Nature Reserve, FREY is Apslawn Forest Reserve and GRA is Gravelly Ridge Conservation Area.", digits = 0) %>%
  kable_classic(full_width = F, html_font = "Times") %>%
  add_header_above(c(" " = 1, "Period 1" = 3, "Period 2" = 3))

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

site_table %>%
  kbl(caption = "<b>Supplementary 1</b>. Number of focal trees and climate at each site ordered from driest to wettest based on mean annual moisture deficit (MD). Mean annual precipitation (PPT) and MD downloaded 
  from the Global Aridity and PET Database (Zomer et al. 2006) based on 1960-1990 WorldClim climate averages (Hijmans et al. 2005). PPT and MD over the study period 
  downloaded from BOM (ref*). MER is Mersey River Conservation Area, DOG is Dogs Head Regional Reserve, TMP is Tasman National Park, BOF is Doctors Peak Regional Reserve, 
      EPF is Tom Gibson Nature Reserve, FREY is Apslawn Forest Reserve and GRA is Gravelly Ridge Conservation Area.", digits = 0) %>%
  kable_classic(full_width = F, html_font = "Times") %>%
  row_spec(0, italic = T) %>%
  add_header_above(c(" " = 5, "Number of focal trees" = 4))

####  Table with norm PPT and sample sizes
site_table2 <- left_join(site_ppt, samplesizes_long) %>% replace(is.na(.), 0) %>% 
  group_by(Site) %>% filter(row_number() == 1)
site_table2 <- site_table2 %>% arrange(`Norm PPT`)
colnames(site_table2) <- c('Site', 'Norm PPT', "E. amygdalina", "E. ovata", "E. viminalis", "E. obliqua")

site_table2 %>%
  kbl(caption = "<b>Supplementary 1</b>. Number of focal trees and long-term average precipitation (Norm PPT) at each site ordered from driest to wettest based. PPT values downloaded 
  from the Global Aridity and PET Database (Zomer et al. 2006) based on 1960-1990 WorldClim climate averages (Hijmans et al. 2005). MER is Mersey River Conservation Area, DOG is Dogs Head Regional Reserve, TMP is Tasman National Park, BOF is Doctors Peak Regional Reserve, 
      EPF is Tom Gibson Nature Reserve, FREY is Apslawn Forest Reserve and GRA is Gravelly Ridge Conservation Area.", digits = 0) %>%
  kable_classic(full_width = F, html_font = "Times") %>%
  row_spec(0, italic = T) %>%
  add_header_above(c(" " = 2, "Number of focal trees" = 4))


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
effects_table$Effect[startsWith(effects_table$Effect, 'std_norm_md')] <- 'std_norm_md'
effects_table$Effect[startsWith(effects_table$Effect, 'std_anomaly')] <- 'std_anomaly'
effects_table$Effect[startsWith(effects_table$Effect, 'std_dbh')] <- 'std_dbh'
effects_table$Effect[startsWith(effects_table$Effect, 'std_total_nci:std_norm_md')] <- 'total_nci:std_norm_md'
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

germ_effects_kbl <- effects_table %>% select(Species, Effect, collated)

germ_effects_kbl <- germ_effects_kbl %>% group_by(Species) %>% mutate(row = row_number()) %>%
  pivot_wider(names_from = Species, values_from = collated) %>% select(-row)

#Plotting with kableR
germ_effects_kbl %>% mutate(Effect = c("Intercept", "Total NCI", "Norm MD", "MD anomaly", "Initial DBH", "PC1", "Total NCI:Norm MD")) %>%
  kbl(align = 'lcccc', caption = "<b>Supplementary X</b>. Model output table with Estimate ± SE for each species modelled separately. Asterisks denote significance: * p<0.05, ** p<0.01, *** p<0.001") %>%
  kable_classic(full_width = T, html_font = "Times", font_size = 12) %>%
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

rsquaredtable %>% kbl(caption = "<b>Supplementary X</b>. Model marginal R squared values for each species modelled separately.", digits = 2) %>%
  kable_classic(full_width = F, html_font = "Times", font_size = 12)

### R squared of models
#14-27%
r.squaredGLMM(amygmod1)
r.squaredGLMM(amygmod2)
r.squaredGLMM(oblimod1)
r.squaredGLMM(oblimod2)
r.squaredGLMM(ovatmod1)
r.squaredGLMM(ovatmod2)
r.squaredGLMM(vimimod1)
r.squaredGLMM(vimimod2)

########################
#### BELOW HERE NEEDS TO BE UPDATED ####
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

###################
### Visualising more basic trends ####
#Plot growth by species by site
ggplot(growthnbhdata, aes(x = Focal_sp, y = growth_rate, color=Site)) +
  geom_boxplot()+
  #geom_point(position = (position_jitter(width = .2)))+
  theme_bw()+
facet_wrap(~ Focal_sp, ncol = 2, nrow = 2, scales="free")
#Why is there so little growth for AMYG at BOF?
growthAMYGBOF <- growthalldata %>% filter(Site == "BOF", Focal_sp == 'AMYG')

#Plot growth by site
#First line of code here reorders Sites according to MD values (so from driest to wettest!)
growthnbhdata %>% mutate(Site = fct_reorder(Site, MD)) %>%
  ggplot(aes(x = Site, y = growth_rate)) +
  geom_boxplot()+
  geom_jitter(alpha = 0.4, aes(colour = Focal_sp))+
  theme_bw()+
  facet_wrap(~ Focal_sp, scales="free")

#Plotting as above but by MD values instead - remember this is WET to DRY
  ggplot(aes(x = MD, y = growth_rate), data = growthnbhdata) +
    geom_jitter(alpha = 0.3, width = 10)+
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


### Growth as a function of rainfall (PPT), PET or MD ####

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