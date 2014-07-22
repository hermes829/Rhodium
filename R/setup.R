# Clearing workspace
rm(list=ls())

# Setting working directory
if(Sys.info()["user"]=="janus829")
{pathMain="~/Desktop/Research/Rhodium/R";
	pathData='~/Dropbox/Research/Rhodium/Data/';
	pathGraphics="~/Dropbox/Research/Rhodium/Graphics";
	pathTex="~/Desktop/Research/Rhodium/LaTeX"}
# Setting working directory
if(Sys.info()["user"]=="Ben")
{pathMain="/Users/Ben/Github/Rhodium/R";
pathTex="/Users/Ben/Github/Rhodium/LaTeX";
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
rmse=function(x){sqrt( mean( (residuals(x)^2) ) )}

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