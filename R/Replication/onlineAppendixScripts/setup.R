# Clearing workspace
rm(list=ls())

# Setting working directory
pathData='Data/'
pathGraphics="Graphics/"

# Load in panel dataframe
load(paste0(pathData, 'panel.rda'))

# Loading libraries and functions
loadPkg=function(toLoad){
  for(lib in toLoad){
  if(! lib %in% installed.packages()[,1])
    { install.packages(lib, repos='http://cran.rstudio.com/') }
  suppressMessages( library(lib, character.only=TRUE) )
  }
}

pks = c('ggplot2', 'grid', 'cshapes','tikzDevice',
  'RColorBrewer', 'stargazer', 'reshape', 'countrycode',
  'foreign', 'doBy', 'lme4', 'plm', 'MASS', 'latex2exp')
loadPkg(pks)

# gg theme
theme_set(theme_bw())

# Helpful functions
numSM=function(x){as.numeric(as.character(x))}
charSM=function(x){as.character(x)}
contToCat=function(x,by=0.1){ cut(x, breaks=quantile(x,seq(0,1,by),na.rm=T)) }
logTrans=function(x){ log( x + abs(min(x, na.rm=T)) + 1) }
starOut = function(fname,starOutput){
  output = capture.output(starOutput)
  cat(paste(output, collapse = "\n"), "\n", file=fname, append=TRUE) }

# Create model formula
modForm = function(dv='gdpGr_l0', ivs, id='ccode', type='random'){
  base = paste(dv, paste(ivs, collapse=' + '), sep=' ~ ')
  if(type=='random'){
    eff = paste0('(1 |', id, ')')
    if(length(eff)>1){ eff = paste(eff, collapse='+') }
    base = paste(base, eff, sep=' + ')
  }

  if(type=='fixed'){
    eff = paste0('factor(', id, ')')
    if(length(eff)>1){
      eff = paste0( paste(eff, collapse='+'), '- 1') } else {
        eff = paste0(eff, '- 1')
      }    
    base = paste(base, eff, sep=' + ')
  }
  return(formula(base))
}