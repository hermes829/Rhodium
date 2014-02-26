# Clearing workspace
rm(list=ls())

# Setting working directory
if(Sys.info()["user"]=="janus829")
{pathMain="~/Desktop/Research/Rhodium/R";
	pathData='~/Dropbox/Research/Rhodium/Data/';
	pathGraphics="~/Dropbox/Research/Rhodium/Graphics";
	pathFunctions="~/Desktop/Prog Notes/R Functions"}
# Setting working directory
if(Sys.info()["user"]=="Ben")
{pathMain="/Users/Ben/Github/Rhodium/R";
 pathGraphics="/Users/Ben/Dropbox/Rhodium/Graphics";
 pathData="/Users/Ben/Dropbox/Rhodium/Data"
 #pathFunctions="~/Desktop/Prog Notes/R Functions"
}

# Load in panel dataframe
setwd(paste(pathMain,'/BuildingPanelData/',sep=''))
load('panel.rda')

# Loading libraries and functions
require(ggplot2)
theme_set(theme_bw())
require(reshape)
require(foreign)

require(WDI)
require(countrycode)

# Helpful functions
numSM=function(x){as.numeric(as.character(x))}
charSM=function(x){as.character(x)}