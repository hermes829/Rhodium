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