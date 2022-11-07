#### Alexandra Catling Tasmania Growth Research Project ####
## Test script for predicting growth

## Load in packages, data and functions
library(tidyverse)
library(ggplot2)

growthnbhdata <-read_csv("Output/tas_data.csv")

##Filter to E. amygdalina data only
amygdata <- growthnbhdata %>% filter(Focal_sp == 'AMYG')

##John's glmm.predict function:
glmm.predict<-function(mod, newdat, se.mult, logit_link=NULL, log_link=NULL, glmmTMB=NULL){
  if(glmmTMB==T){
    pvar1 <- diag(as.matrix(newdat) %*% tcrossprod(vcov(mod)[[1]],as.matrix(newdat)))
    newdat$y<- as.matrix(newdat) %*% fixef(mod)[[1]]}
  else{
    pvar1 <- diag(as.matrix(newdat) %*% tcrossprod(vcov(mod),as.matrix(newdat)))
    newdat$y<- as.matrix(newdat) %*% fixef(mod)}
  
  newdat <- data.frame(newdat, lower = newdat$y-(se.mult*sqrt(pvar1)), upper = newdat$y+(se.mult*sqrt(pvar1)))
  
  ## if you have used binomial errors then this will back transform logits to the probability scale
  if(logit_link==T) {
    newdat$y<-plogis(newdat$y); newdat$lower<-plogis(newdat$lower); newdat$upper<-plogis(newdat$upper)
  } else
    
    ## if you have used poisson errors or have log-transformed your response, then this will back transform to the original scale (e.g. abundance)
    if(log_link==T) {
      newdat$y<-exp(newdat$y); newdat$lower<-exp(newdat$lower); newdat$upper<-exp(newdat$upper)
    }
  return(with(newdat, data.frame(y, upper, lower)))
}

### Predicting growth rate for given moisture deficit, mean size, neighbourhood crowding, PC1
#Continuous predictors standardised to mean of 0 and sd of 1
#Predicting for low and high long-term moisture deficit - one sd below and above mean
model<-lmer(sqrt(growth_rate) ~ std_total_nci + std_norm_md + std_anomaly + 
              std_norm_md:std_total_nci + std_preceding_dbh + std_preceding_dbh:std_total_nci +
              std_preceding_dbh:std_norm_md + std_PC1 + (1|Site/Plot/Tree), amygdata)
md.low<-glmm.predict(mod=model, newdat=data.frame(1, 0, 1, 0, 0, 0, 0*1, 0*0, 1*0), se.mult=1.96, logit_link=FALSE, log_link=FALSE, glmmTMB=FALSE)
md.high<-glmm.predict(mod=model, newdat=data.frame(1, 0, -1, 0, 0, 0, 0*-1, 0*0, -1*0), se.mult=1.96, logit_link=FALSE, log_link=FALSE, glmmTMB=FALSE)

## Plotting
md.low$MD <- 'low'
md.high$MD <- 'high'
md.low$x <- 'Dry'
md.high$x <- 'Wet'
#Merge low and high MD
md.merged <- rbind(md.low, md.high)

#values are much higher than I would expect? Maybe not..
#max(amygdata$growth_rate, na.rm=T) is 0.214
#max(amygdata$std_norm_md) is 1.18, and predicting for 1
ggplot(md.merged, aes(x = x, y = y, group = MD))+
  geom_point()+
  geom_errorbar(aes(ymin = lower, ymax = upper, width = 0.1))+
  ylab("Growth rate (mm/day, sqrt)")+
  xlab("Moisture deficit")+
  theme_classic()+
  theme(axis.ticks.x = element_blank())

#From John:
model<-lmer(sqrt(growth_rate) ~ std_total_nci + std_norm_md + std_anomaly + 
              std_norm_md:std_total_nci + std_preceding_dbh + std_preceding_dbh:std_total_nci +
              std_preceding_dbh:std_norm_md + std_PC1 + (1|Site/Plot/Tree), amygdata)
## you can make multiple predictions at once - here I made the predictions for md =1 and md=-1
md_pred_original_scale<-glmm.predict(mod=model, newdat=data.frame(1, 0, c(1,-1), 0, 0, 0, 0*c(1,-1), 0*0, c(1,-1)*0), 
                                     se.mult=1.96, logit_link=FALSE, log_link=FALSE, glmmTMB=FALSE)^2
## add the md category (like you did)
md_pred_original_scale$md_category<-c("dry", "wet")

## not sure what you were asking when comparing the observed growth data to the model predictions
## your model is on the scale of sqrt(growth)
## you need to back-transform the predictions (like I did above) if you want to compare to "raw" growth
ggplot(md_pred_original_scale, aes(x = md_category, y = y, group = md_category))+
  geom_point()+
  geom_errorbar(aes(ymin = lower, ymax = upper, width = 0.1))+
  ylab("Growth rate (mm/day)")+
  xlab("Moisture deficit")+
  theme_classic()+
  theme(axis.ticks.x = element_blank())
