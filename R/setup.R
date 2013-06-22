# Clearing workspace
rm(list=ls())

# Setting working directory
if(Sys.info()["user"]=="janus829")
{pathMain="~/Desktop/Research/Rhodium/R";
	pathGraphics="~/Dropbox/Research/Rhodium/Graphics";
	pathFunctions="~/Desktop/Prog Notes/R Functions"}
# Setting working directory
if(Sys.info()["user"]=="Ben")
{pathMain="C:/Users/Ben/Dropbox/Rhodium/R";
 pathGraphics="C:/Users/Ben/Dropbox/Rhodium/Graphics";
 #pathFunctions="~/Desktop/Prog Notes/R Functions"
}

# Loading libraries and functions
library(ggplot2)
theme_set(theme_bw())
library(reshape)
library(foreign)