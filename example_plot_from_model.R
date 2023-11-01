### Alexandra Catling
## Tasmania Data 
## September 2023

### How to plot regression and confidence intervals from linear models
# using John Dwyer's functions in base R

#### Load in data and packages and functions ####
source("data_preparations.R")

library(kableExtra)
library(sf)
library(ggrepel)
library(ggfortify)
library(emmeans)

## Need these functions from John Dwyer:
##creates a sequence of values from min to max by 100
seq.func<-function(x)(seq(min(x, na.rm=T), max(x, na.rm=T), length.out=100))

####function to plot the curve and associated condfidence interval
plot.CI.func<- function(x.for.plot, pred, upper, lower, env.colour, env.trans=NA, line.colour, line.weight){
  colour.rgb<-col2rgb(col=env.colour)  
  polygon.coords<-data.frame(rbind(cbind(x.for.plot[1], lower[1]), 
                                   cbind(x.for.plot, upper), 
                                   cbind(x.for.plot, lower)[rev(order(x.for.plot)),]))
  names(polygon.coords)<-c("x", "y")							
  polygon(polygon.coords$x, polygon.coords$y, col=rgb(red=colour.rgb["red",],blue=colour.rgb["blue",], green=colour.rgb["green",] , alpha=env.trans, maxColorValue = 255), border=NA)
  lines(x.for.plot, pred, col=line.colour, lwd=line.weight)         
} 
#To predict regression:
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
plot.CI.func(x.for.plot = x_to_plot, pred = plotted.pred$y, upper = plotted.pred$upper, lower = plotted.pred$lower, env.colour = "grey1", env.trans = 50, line.colour = "black", line.weight = 2)
dev.off()

#### Breaking it down ####
#Holding values at their means (0) except for NCI which is varying (x axis variable)
coef(model)

#Species as a RE
plot(sqrt(growth_rate) ~ std_total_nci, pch=19, col=alpha("grey60", 0.3), ylab="sqrt(Growth rate (mm/day))", xlab="Neighbourhood crowding index (standardised, sqrt)", tck=-0.01, cex= 2, cex.lab = 2, cex.axis = 2, growthnbhdata)
model2<-lmer(sqrt(growth_rate) ~ std_preceding_dbh + std_ppt + 
              std_total_nci + std_preceding_dbh*std_total_nci + std_ppt:std_total_nci +
              (1|Site/Plot/Tree) + (1|Focal_sp), growthnbhdata)
x_to_plot<-seq.func(growthnbhdata$std_total_nci)
coef(model2)
summary(model2)
preddata <- with(model2, data.frame(1, 0, 0, x_to_plot, 0*x_to_plot, 0*x_to_plot))
plotted.pred <- glmm.predict(mod = model2, newdat = preddata, se.mult = 1.96, logit_link=FALSE, log_link=FALSE, glmmTMB=FALSE)
plot.CI.func(x.for.plot = x_to_plot, pred = plotted.pred$y, upper = plotted.pred$upper, lower = plotted.pred$lower, env.colour = "grey1", env.trans = 50, line.colour = "black", line.weight = 2)

#### Just one predictor variable ####
#plot our data
plot(sqrt(growth_rate) ~ std_total_nci, pch=19, col=alpha("grey60", 0.3), ylab="sqrt(Growth rate (mm/day))", xlab="Neighbourhood crowding index (standardised, sqrt)", tck=-0.01, cex= 2, cex.lab = 1, cex.axis = 1, growthnbhdata)
#run our model
model3 <- lmer(sqrt(growth_rate) ~ std_total_nci + (1|Site/Plot/Tree) + (1|Focal_sp), growthnbhdata)
#check what our response variables are
summary(model3)
#create a range of 100 x axis values from min to max
x_to_plot<-seq.func(growthnbhdata$std_total_nci)
#predict data for the regression holding everything constant except variable of interest (x axis)
#hold intercept at 1
preddata <- with(model3, data.frame(1, x_to_plot))
#predict 95% confidence intervals
plotted.pred <- glmm.predict(mod = model3, newdat = preddata, se.mult = 1.96, logit_link=FALSE, log_link=FALSE, glmmTMB=FALSE)
#plot regression and CIs
plot.CI.func(x.for.plot = x_to_plot, pred = plotted.pred$y, upper = plotted.pred$upper, lower = plotted.pred$lower, env.colour = "grey1", env.trans = 50, line.colour = "black", line.weight = 2)

#compare to ggplot regression
library(ggplot2)
ggplot(aes(x=sqrt(std_total_nci), y = sqrt(growth_rate)), data = growthnbhdata)+
  geom_point()+
  geom_smooth(method="lm")+
theme_classic()
