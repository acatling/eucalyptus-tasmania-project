### Eucalyptus Growth Tasmania Project 
# Alexandra Catling PhD Research
# Started 26/02/2021

###### Importing data and packages ####
source("data_preparations.R")

library(kableExtra)
library(sf)
library(ggrepel)
library(ggfortify)

#### Plots of soil ####
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
  xlab("std_PC2")+
  my_theme+
  facet_wrap(~Focal_sp)

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
meandbh <- growthnbhdata %>%
  group_by(Focal_sp) %>%
  summarise(min_dbh = min(preceding_dbh, na.rm=T),
            max_dbh = max(preceding_dbh, na.rm=T),
            mean_dbh = mean(preceding_dbh, na.rm=T),
            sd_dbh = sd(preceding_dbh, na.rm=T))

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
ggplot(growthnbhdata, aes(x = CMD, y = total_nbh_ba))+
  geom_jitter(alpha = 0.2, width = 4)+
  geom_smooth(method="lm")+
  geom_errorbar(stat="summary", fun.data="mean_se",width=1.5,size=1.2, colour = 'red')+
  theme_classic()

#Ideally this would have fixed effects for soil properties too
modelmd1 <- lm(total_nbh_ba ~ MD, growthnbhdata)
summary(modelmd1)
#Yes, certainly more basal area of neighbours at wetter sites

## NUMBER OF NEIGHBOURS
growthnbhdata %>% mutate(Site = fct_reorder(Site, desc(CMD))) %>%
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

#MD and NCI somewhat correlated
dev.off()
pdf("Output/nci~md.pdf", width=21, height=21)
par(pty="s")
growthnbhdata %>% filter(Period == 2) %>%
  ggplot(aes(x = std_cmd, y = log_total_nci))+
  geom_jitter(alpha = 0.4, width = 0.05)+
  geom_smooth(method="lm")+
  xlab("Moisture deficit (mean annual precipitation - mean annual evapotranspiration, standardised)")+
  ylab("log(Neighbourhood crowding index)")+
  theme_classic()+
  facet_wrap(~Focal_sp)
dev.off()

modelpptnci <- lmer(log_total_nci ~ std_norm_md + (1|Site/Plot/Tree), onerowdata)
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
  growthquadnci <- lmer(sqrt(growth_rate) ~ std_norm_md + I(std_norm_md^2) + (1|Site/Plot/Tree), data = filter(growthnbhdata, Focal_sp == speciesabbrevlist[i]))
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
  plot(jitter(sqrt(plotted.data$growth_rate), amount = 0.05) ~ plotted.data$std_norm_md, pch=19, col="grey60", ylab="Growth rate (sqrt, standardised)", xlab="Long-term MD (standardised)", cex.lab=2, cex.axis=2.00,tck=-0.01)
  mtext(paste(letters[i], ")", sep=""), side=2,line=1,adj=1.5,las=1, padj=-13, cex=1.5)
  title(main=bquote(italic(.(speciesnamelist[i]))), cex.main=2.5)
  x_to_plot<-seq.func(plotted.data$std_norm_md) 
  model<-lmer(sqrt(growth_rate) ~ std_norm_md + I(std_norm_md^2) + (1|Site/Plot/Tree), plotted.data)
  model2<-lmer(sqrt(growth_rate) ~ std_norm_md + (1|Site/Plot/Tree), plotted.data)
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


#### Quick test relationships dry vs wet datasets ####
#AMYG
amygdata_wet <- amygdata %>% filter(std_norm_md<0)
amygdata_dry <- amygdata %>% filter(std_norm_md>0)

amygmod_wet <- lmer(sqrt(growth_rate) ~ std_total_nci + std_anomaly + 
                      std_preceding_dbh + std_preceding_dbh:std_total_nci + 
                      std_PC1 + (1|Site/Plot/Tree), amygdata_wet)
amygmodwetdharma <- simulateResiduals(amygmod_wet)
plot(amygmodwetdharma)
#great
summary(amygmod_wet)
vif(amygmod_wet)
#dry - not converging
amygmod_dry <- lmer(sqrt(growth_rate) ~ std_total_nci + std_anomaly + 
                      std_preceding_dbh + 
                      std_PC1 + (1|Site/Plot/Tree), amygdata_dry)
amygmoddrydharma <- simulateResiduals(amygmod_dry)
plot(amygmoddrydharma)
#great
summary(amygmod_dry)
vif(amygmod_dry)


oblidata_wet <- oblidata %>% filter(std_norm_md<0)
oblidata_dry <- oblidata %>% filter(std_norm_md>0)

oblimod_wet <- lmer(sqrt(growth_rate) ~ std_total_nci + std_anomaly + 
                      std_preceding_dbh + std_preceding_dbh:std_total_nci + 
                      std_PC1 + (1|Site/Plot/Tree), oblidata_wet)
oblimodwetdharma <- simulateResiduals(oblimod_wet)
plot(oblimodwetdharma)
#great
summary(oblimod_wet)
vif(oblimod_wet)

#dry
oblimod_dry <- lmer(sqrt(growth_rate) ~ std_total_nci + std_anomaly + 
                      std_preceding_dbh + std_preceding_dbh:std_total_nci + 
                      std_PC1 + (1|Site/Plot/Tree), oblidata_dry)
oblimoddrydharma <- simulateResiduals(oblimod_dry)
plot(oblimoddrydharma)
#great
summary(oblimod_dry)
vif(oblimod_dry)


ovatdata_wet <- ovatdata %>% filter(std_norm_md<0)
ovatdata_dry <- ovatdata %>% filter(std_norm_md>0)

#grouping problem - only 1 site
ovatmod_wet <- lmer(sqrt(growth_rate) ~ std_total_nci + std_anomaly + 
                      std_preceding_dbh + std_preceding_dbh:std_total_nci + 
                      std_PC1 + (1|Plot/Tree), ovatdata_wet)
ovatmodwetdharma <- simulateResiduals(ovatmod_wet)
plot(ovatmodwetdharma)
#great
summary(ovatmod_wet)
vif(ovatmod_wet)
#dry
ovatmod_dry <- lmer(sqrt(growth_rate) ~ std_total_nci + std_anomaly + 
                      std_preceding_dbh + std_preceding_dbh:std_total_nci + 
                      std_PC1 + (1|Site/Plot/Tree), ovatdata_dry)
ovatmoddrydharma <- simulateResiduals(ovatmod_dry)
plot(ovatmoddrydharma)
#great
summary(ovatmod_dry)
vif(ovatmod_dry)


vimidata_wet <- vimidata %>% filter(std_norm_md<0)
vimidata_dry <- vimidata %>% filter(std_norm_md>0)


vimimod_wet <- lmer(sqrt(growth_rate) ~ std_total_nci + std_anomaly + 
                      std_preceding_dbh + std_preceding_dbh:std_total_nci + 
                      std_PC1 + (1|Site/Plot/Tree), vimidata_wet)
vimimodwetdharma <- simulateResiduals(vimimod_wet)
plot(vimimodwetdharma)
#great
summary(vimimod_wet)
vif(vimimod_wet)
#dry - not converging
vimimod_dry <- lmer(sqrt(growth_rate) ~ std_total_nci + std_anomaly + 
                      std_preceding_dbh + 
                      std_PC1 + (1|Site/Plot/Tree), vimidata_dry)
vimimoddrydharma <- simulateResiduals(vimimod_dry)
plot(vimimoddrydharma)
#great
summary(vimimod_dry)
vif(vimimod_dry)

#### Quick test relationships big vs small trees ####
oblidata_small <- oblidata %>% filter(std_preceding_dbh<0)
oblidata_big <- oblidata %>% filter(std_preceding_dbh>0)

oblimod_small <- lmer(sqrt(growth_rate) ~ std_total_nci + std_norm_md + std_anomaly + 
                        std_norm_md:std_total_nci + std_PC1 + (1|Site/Plot/Tree), oblidata_small)
oblimodsmalldharma <- simulateResiduals(oblimod_small)
plot(oblimodsmalldharma)
#great
summary(oblimod_small)
vif(oblimod_small)

#big
oblimod_big <- lmer(sqrt(growth_rate) ~ std_total_nci + std_norm_md + std_anomaly + 
                      std_norm_md:std_total_nci + std_PC1 + (1|Site/Plot/Tree), oblidata_big)
oblimodbigdharma <- simulateResiduals(oblimod_big)
plot(oblimodbigdharma)
#great
summary(oblimod_big)
vif(oblimod_big)

vimidata_small <- vimidata %>% filter(std_preceding_dbh<0)
vimidata_big <- vimidata %>% filter(std_preceding_dbh>0)

vimimod_small <- lmer(sqrt(growth_rate) ~ std_total_nci + std_norm_md + std_anomaly + 
                        std_norm_md:std_total_nci + std_PC1 + (1|Site/Plot/Tree), vimidata_small)
vimimodsmalldharma <- simulateResiduals(vimimod_small)
plot(vimimodsmalldharma)
#great
summary(vimimod_small)
vif(vimimod_small)

#big
vimimod_big <- lmer(sqrt(growth_rate) ~ std_total_nci + std_norm_md + std_anomaly + 
                      std_norm_md:std_total_nci + std_PC1 + (1|Site/Plot/Tree), vimidata_big)
vimimodbigdharma <- simulateResiduals(vimimod_big)
plot(vimimodbigdharma)
#great
summary(vimimod_big)
vif(vimimod_big)

### What about growth ~ size given high or low NCI?
#Size matters only when std_total_nci < 0.
oblidata_bign <- oblidata %>% filter(std_total_nci<0)

oblimod_bign <- lmer(sqrt(growth_rate) ~ std_preceding_dbh + std_norm_md + std_anomaly + 
                        std_PC1 + std_preceding_dbh:std_norm_md + (1|Site/Plot/Tree), oblidata_bign)
oblimodbigdharma <- simulateResiduals(oblimod_bign)
plot(oblimodbigdharma)
#great
summary(oblimod_bign)
vif(oblimod_small)

model<-lmer(sqrt(growth_rate) ~ std_preceding_dbh + std_norm_md + std_anomaly + 
              std_preceding_dbh + std_PC1 + std_preceding_dbh:std_norm_md + std_PC1 + (1|Site/Plot/Tree), plotted.data)


###############
### Below plots aren't updated for final model ####
#### Plotting growth rate ~ NCI from total NCI model ####
#all species
dev.off()
pdf("Output/growth_rate~NCI.pdf", width=21, height=21)
par(pty="s")
plot(sqrt(growth_rate) ~ std_total_nci, pch=19, col=alpha("grey60", 0.3), ylab="sqrt(Growth rate (mm/day))", xlab="Neighbourhood crowding index (standardised, sqrt)", tck=-0.01, cex= 2, cex.lab = 2, cex.axis = 2, growthnbhdata)
model<-lmer(sqrt(growth_rate) ~ std_preceding_dbh + Focal_sp + std_ppt + 
              std_total_nci + std_preceding_dbh*std_total_nci + Focal_sp:std_preceding_dbh + std_ppt:std_total_nci +
              (1|Site/Plot/Tree), growthnbhdata)
x_to_plot<-seq.func(growthnbhdata$std_total_nci)
#mean NCI - black
preddata <- with(model, data.frame(1, 0, 0, 0, 0, 0, x_to_plot, 0*x_to_plot, 0*0, 0*0, 0*0, 0*x_to_plot))
plotted.pred <- glmm.predict(mod = model, newdat = preddata, se.mult = 1.96, logit_link=FALSE, log_link=FALSE, glmmTMB=FALSE)
plot.CI.func(x.for.plot = x_to_plot, pred = plotted.pred$y, upper = plotted.pred$upper, lower = plotted.pred$lower, env.colour = "grey1", env.trans = 50, line.colour = "black", line.weight = 2, line.type = 1)
dev.off()

#### Growth rate ~ NCI in high or low long-term MD sites ####

# amygmod1 <- lmer(sqrt(growth_rate) ~ std_total_nci + std_norm_md + std_anomaly + 
#                    std_norm_md:std_total_nci + std_preceding_dbh + std_preceding_dbh:std_total_nci +
#                    std_preceding_dbh:std_norm_md + std_PC1 + (1|Site/Plot/Tree), amygdata)

### AMYG
## oh!! Is the reason why it looks like the lines don't fit the data because I am
##colouring it by <0 or >0 but modelling it for -1 and 1??
# dev.off()
# pdf("Output/AMYG_growth_rate~NCI+MD.pdf", width=21, height=21)
# par(pty="s")

#THIS WORKS
plot(sqrt(growth_rate) ~ std_total_nci, pch = ifelse(Period==1, 19, 17), col = alpha(ifelse(std_norm_md>0, "forestgreen", "red"), 0.4), ylab="sqrt(Growth rate (mm/day))", xlab="Neighbourhood crowding index (standardised, sqrt)", tck=-0.01, cex= 2, cex.lab = 2, cex.axis = 2, amygdata)
# lmer(sqrt(growth_rate) ~ std_total_nci + std_norm_md + std_anomaly + 
#        std_norm_md:std_total_nci + std_preceding_dbh + std_preceding_dbh:std_total_nci +
#        std_preceding_dbh:std_norm_md + std_PC1 + (1|Site/Plot/Tree)
model<-lmer(sqrt(growth_rate) ~ std_total_nci + std_norm_md + std_anomaly + 
                     std_norm_md:std_total_nci + std_preceding_dbh + std_preceding_dbh:std_total_nci +
                     std_preceding_dbh:std_norm_md + std_PC1 + (1|Site/Plot/Tree), amygdata)
x_to_plot<-seq.func(amygdata$std_total_nci)
#mean md - black - works
preddata <- with(model, data.frame(1, x_to_plot, 0, 0, 0, 0, x_to_plot*0, x_to_plot*0, 0*0))
plotted.pred <- glmm.predict(mod = model, newdat = preddata, se.mult = 1.96, logit_link=FALSE, log_link=FALSE, glmmTMB=FALSE)
plot.CI.func(x.for.plot = x_to_plot, pred = plotted.pred$y, upper = plotted.pred$upper, lower = plotted.pred$lower, env.colour = "grey1", env.trans = 50, line.colour = "black", line.weight = 2, line.type = 1)
#low md - red - works but x extrapolates
# preddata <- with(model, data.frame(1, x_to_plot, -1, 0, 0, 0, x_to_plot*-1, x_to_plot*0, -1*0))
# plotted.pred <- glmm.predict(mod = model, newdat = preddata, se.mult = 1.96, logit_link=FALSE, log_link=FALSE, glmmTMB=FALSE)
# plot.CI.func(x.for.plot = x_to_plot, pred = plotted.pred$y, upper = plotted.pred$upper, lower = plotted.pred$lower, env.colour = "grey1", env.trans = 50, line.colour = "red", line.weight = 2, line.type = 1)
#low md (wet) - pink - without extrapolating x
x_to_plot_low<-seq.func(amygdata$std_total_nci[amygdata$norm_md<0])
preddata <- with(model, data.frame(1, x_to_plot_low, -1, 0, 0, 0, x_to_plot_low*-1, x_to_plot_low*0, -1*0))
plotted.pred <- glmm.predict(mod = model, newdat = preddata, se.mult = 1.96, logit_link=FALSE, log_link=FALSE, glmmTMB=FALSE)
plot.CI.func(x.for.plot = x_to_plot_low, pred = plotted.pred$y, upper = plotted.pred$upper, lower = plotted.pred$lower, env.colour = "pink", env.trans = 50, line.colour = "pink", line.weight = 2, line.type = 1)
#high md (dry) - orange- without extrapolating
x_to_plot_high<-seq.func(amygdata$std_total_nci[amygdata$norm_md>0])
preddata <- with(model, data.frame(1, x_to_plot_high, 1, 0, 0, 0, x_to_plot_high*1, x_to_plot_high*0, 1*0))
plotted.pred <- glmm.predict(mod = model, newdat = preddata, se.mult = 1.96, logit_link=FALSE, log_link=FALSE, glmmTMB=FALSE)
plot.CI.func(x.for.plot = x_to_plot_high, pred = plotted.pred$y, upper = plotted.pred$upper, lower = plotted.pred$lower, env.colour = "orange", env.trans = 50, line.colour = "orange", line.weight = 2, line.type = 1)
#dev.off()

### Growth rate ~ NCI given period climate ####
#Why aren't lines fitting the data?? Fix this
dev.off()
pdf("Output/AMYG_growth_rate~NCI+period_MD.pdf", width=21, height=21)
par(pty="s")
plot(sqrt(growth_rate) ~ std_total_nci, pch = ifelse(Period==1, 19, 17), col = alpha(ifelse(std_period_md>0, "forestgreen", "red"), 0.4), ylab="sqrt(Growth rate (mm/day))", xlab="Neighbourhood crowding index (standardised, sqrt)", tck=-0.01, cex= 2, cex.lab = 2, cex.axis = 2, amygdata)
model<-lmer(sqrt(growth_rate) ~ std_total_nci + std_norm_md + std_period_md + 
              std_total_nci*std_period_md + std_preceding_dbh + std_preceding_dbh*std_total_nci +
              std_preceding_dbh*std_period_md + (1|Site/Plot/Tree), amygdata)
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

## Not sure why OVAT trendlines don't fit the data, all other sp seem fine
#OVAT
dev.off()
pdf("Output/OVAT_growth_rate~NCI+period_MD.pdf", width=21, height=21)
par(pty="s")
plot(sqrt(growth_rate) ~ std_total_nci, pch = ifelse(Period==1, 19, 17), col = alpha(ifelse(std_period_md>0, "forestgreen", "red"), 0.4), ylab="sqrt(Growth rate (mm/day))", xlab="Neighbourhood crowding index (standardised, sqrt)", tck=-0.01, cex= 2, cex.lab = 2, cex.axis = 2, ovatdata)
model<-lmer(sqrt(growth_rate) ~ std_total_nci + std_norm_md + std_period_md + 
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
model<-lmer(sqrt(growth_rate) ~ std_total_nci + std_norm_md + std_period_md + 
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

#### Plot of growth rate ~ anomaly ####

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
                std_norm_md:std_total_nci + std_preceding_dbh + std_PC1 + (1|Site/Plot/Tree), plotted.data)
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
  model<-lmer(sqrt(growth_rate) ~ std_total_nci + std_norm_md + std_period_md + 
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
# model1<-lmer(sqrt(growth_rate) ~ std_preceding_dbh + Focal_sp + std_ppt + 
#               std_total_nci + std_preceding_dbh*std_total_nci + Focal_sp:std_preceding_dbh + std_ppt:std_total_nci +
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
# model<-lmer(sqrt(growth_rate) ~ std_preceding_dbh + Focal_sp + std_ppt + 
#               std_total_nci + std_preceding_dbh*std_total_nci + Focal_sp:std_preceding_dbh + std_ppt:std_total_nci +
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
# model<-lmer(sqrt(growth_rate) ~ std_preceding_dbh + Focal_sp + std_ppt + 
#               std_total_nci + std_preceding_dbh*std_total_nci + Focal_sp:std_preceding_dbh + std_ppt:std_total_nci +
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
growthnbhdata %>% mutate(Site = fct_reorder(Site, CMD)) %>%
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