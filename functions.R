#### Functions for WA Watering Perenjori Experiment Analysis
# Alexandra Catling
# 2021

####Histogram function for using with and pairs functions
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE, breaks=seq(min(x, na.rm=T), max(x, na.rm=T), length.out=7))
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y, na.rm=T)
  rect(breaks[-nB], 0, breaks[-1], y,...)
}

#### Theme for plotting
my_theme <- theme(axis.title.x = element_text(size = 16),
                  axis.title.y = element_text(size = 16),
                  axis.text = element_text(size = 16),
                  strip.text.x = element_text(size = 16),
                  legend.text = element_text(size = 14),
                  legend.title = element_blank())

### For plotting legends in multi-panel base R loop plots
reset <- function() {
  par(mfrow=c(1, 1), oma=rep(0, 4), mar=rep(0, 4), new=TRUE)
  plot(0:1, 0:1, type="n", xlab="", ylab="", axes=FALSE)
}

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

###function to inverse logits
inv.logit<-function(x)(exp(x)/(1+exp(x)))

##creates a sequence of values from min to max by 100
seq.func<-function(x)(seq(min(x, na.rm=T), max(x, na.rm=T), length.out=100))

## Function 12/10/21
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

### John's code to calculate variance components in models ####
## variance components from lmer model
vca_func<-function(mod) {
  var_comps<-as.data.frame(summary(mod)$varcor)$vcov / sum(as.data.frame(summary(mod)$varcor)$vcov) * 100
  groups<-gsub(":", "_", as.data.frame(summary(mod)$varcor)$grp)
  return(data.frame(groups, var_comps))
}

#From: http://www.sthda.com/english/wiki/wiki.php?id_contents=7930
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

### Need this reset function before legend:
reset <- function() {
  par(mfrow=c(1, 1), oma=rep(0, 4), mar=rep(0, 4), new=TRUE)
  plot(0:1, 0:1, type="n", xlab="", ylab="", axes=FALSE)
}

#Making a function for plot_CI (just to tidy up code)
plot_CI <- function(){plot.CI.func(x.for.plot = x_to_plot, pred = plotted.pred$y, upper = plotted.pred$upper, lower = plotted.pred$lower, env.colour = "grey1", env.trans = 50, line.colour = "black", line.weight = 2, line.type = 1)}
