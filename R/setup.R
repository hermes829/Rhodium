# Clearing workspace
rm(list=ls())

# Setting working directory
if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m")
{pathMain="~/Research/Rhodium/R";
	pathData='~/Dropbox/Research/Rhodium/Data/';
	pathGraphics="~/Dropbox/Research/Rhodium/Graphics"}

# Setting working directory
if(Sys.info()["user"]=="Ben")
{pathMain="/Users/Ben/Github/Rhodium/R";
 pathGraphics="/Users/Ben/Dropbox/Rhodium/Graphics";
 pathData="/Users/Ben/Dropbox/Rhodium/Data"
}

# Load in panel dataframe
setwd(paste(pathMain,'/BuildingPanelData/',sep=''))
load('panel.rda')

# Loading libraries and functions
require(ggplot2)
theme_set(theme_bw())
require(grid)
require(tikzDevice)
require(RColorBrewer)

require(sbgcop)
require(reshape)
require(foreign)
require(doBy)
require(lme4)

require(shapefiles)
require(cshapes)
require(WDI)
require(countrycode)

# Helpful functions
numSM=function(x){as.numeric(as.character(x))}
charSM=function(x){as.character(x)}
contToCat=function(x,by=0.1){ cut(x, breaks=quantile(x,seq(0,1,by),na.rm=T)) }

# Lagging vars
lagTS <- function(x,l){
  cuts <- (length(x)-(l-1)):length(x)
  c(rep(NA,l), x[ -cuts ] )
}

lagDataSM <- function(data, country_year, country, varsTOlag, lag=1)
{
  data[,country_year] = numSM(data[,country_year])
  data <- data[order(data[,country_year]),]
  lagData <- apply(data[,varsTOlag], 2,
    function(x){
      unlist(by(x, data[,country], function(y) lagTS(y,lag) ) )
    } )
  colnames(lagData) <- paste('lag', lag, '_', varsTOlag, sep='')
  cbind(data, lagData)
}

# http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/#Helper functions
## Summarizes data.
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
    require(plyr)

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