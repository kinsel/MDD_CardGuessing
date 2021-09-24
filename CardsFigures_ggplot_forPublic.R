# first define some functions

summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

normDataWithin <- function(data=NULL, idvar, measurevar, betweenvars=NULL,
                           na.rm=FALSE, .drop=TRUE) {
  library(plyr)
  
  # Measure var on left, idvar + between vars on right of formula.
  data.subjMean <- ddply(data, c(idvar, betweenvars), .drop=.drop,
                         .fun = function(xx, col, na.rm) {
                           c(subjMean = mean(xx[,col], na.rm=na.rm))
                         },
                         measurevar,
                         na.rm
  )
  
  # Put the subject means with original data
  data <- merge(data, data.subjMean)
  
  # Get the normalized data in a new column
  measureNormedVar <- paste(measurevar, "_norm", sep="")
  data[,measureNormedVar] <- data[,measurevar] - data[,"subjMean"] +
    mean(data[,measurevar], na.rm=na.rm)
  
  # Remove this subject mean column
  data$subjMean <- NULL
  
  return(data)
}

summarySEwithin <- function(data=NULL, measurevar, betweenvars=NULL, withinvars=NULL,
                            idvar=NULL, na.rm=FALSE, conf.interval=.95, .drop=TRUE) {
  
  # Ensure that the betweenvars and withinvars are factors
  factorvars <- vapply(data[, c(betweenvars, withinvars), drop=FALSE],
                       FUN=is.factor, FUN.VALUE=logical(1))
  
  if (!all(factorvars)) {
    nonfactorvars <- names(factorvars)[!factorvars]
    message("Automatically converting the following non-factors to factors: ",
            paste(nonfactorvars, collapse = ", "))
    data[nonfactorvars] <- lapply(data[nonfactorvars], factor)
  }
  
  # Get the means from the un-normed data
  datac <- summarySE(data, measurevar, groupvars=c(betweenvars, withinvars),
                     na.rm=na.rm, conf.interval=conf.interval, .drop=.drop)
  
  # Drop all the unused columns (these will be calculated with normed data)
  datac$sd <- NULL
  datac$se <- NULL
  datac$ci <- NULL
  
  # Norm each subject's data
  ndata <- normDataWithin(data, idvar, measurevar, betweenvars, na.rm, .drop=.drop)
  
  # This is the name of the new column
  measurevar_n <- paste(measurevar, "_norm", sep="")
  
  # Collapse the normed data - now we can treat between and within vars the same
  ndatac <- summarySE(ndata, measurevar_n, groupvars=c(betweenvars, withinvars),
                      na.rm=na.rm, conf.interval=conf.interval, .drop=.drop)
  
  # Apply correction from Morey (2008) to the standard error and confidence interval
  #  Get the product of the number of conditions of within-S variables
  nWithinGroups    <- prod(vapply(ndatac[,withinvars, drop=FALSE], FUN=nlevels,
                                  FUN.VALUE=numeric(1)))
  correctionFactor <- sqrt( nWithinGroups / (nWithinGroups-1) )
  
  # Apply the correction factor
  ndatac$sd <- ndatac$sd * correctionFactor
  ndatac$se <- ndatac$se * correctionFactor
  ndatac$ci <- ndatac$ci * correctionFactor
  
  # Combine the un-normed means with the normed results
  merge(datac, ndatac)
}

###----now deal with the data###

RTdata<-read.csv("~/Downloads/CardsPaper/Data_For_Brainyac_Long.csv") # CHANGE DATA PATH HERE
head(RTdata)

HC_RT<-RTdata[which(RTdata$GroupLong=="HC"),]
HC_RTmeans<-summarySEwithin(HC_RT, measurevar="z.scored_RT", withinvars="Stakes", idvar="Subject", na.rm=FALSE, conf.interval=.95)

MDD_RT<-RTdata[which(RTdata$GroupLong=="DP"),]
MDD_RTmeans<-summarySEwithin(MDD_RT, measurevar="z.scored_RT", withinvars="Stakes", idvar="Subject", na.rm=FALSE, conf.interval=.95)

Combine_means<-rbind(MDD_RTmeans, HC_RTmeans)
Combine_means$Group<-c("Major Depressive Disorder", "Major Depressive Disorder", "Healthy Controls", "Healthy Controls")
Combine_means$Upper<-Combine_means$z.scored_RT+Combine_means$se
Combine_means$Lower<-Combine_means$z.scored_RT-Combine_means$se

require(ggplot2)
p1<-ggplot(Combine_means, aes(x = Group, y = z.scored_RT, fill = Stakes)) +
  coord_cartesian(ylim=c(-0.08,0.09)) +
  geom_bar(stat = "identity", position =position_dodge(), width=0.7) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2, position = position_dodge(.7)) +
  scale_fill_grey() +
  theme_classic() +
  ylab("Standardized Reaction Time (z-score)")+
  theme(axis.title.x = element_text(face="bold", size=0),axis.text.x  = element_text(vjust=0.5, face="bold", size=16)) +
  theme(axis.title.y = element_text(face="bold", size=16),axis.text.y  = element_text(vjust=0.5, size=14)) +
  theme(legend.title=element_blank()) + #theme(legend.position="top") + 
  theme(legend.direction = "horizontal") +
  theme(legend.position = c(.5, .98)) +
  theme(legend.text=element_text(size=16))+
  geom_hline(yintercept = 0)
p1

ggsave("CardsRT.pdf", dpi=500)


