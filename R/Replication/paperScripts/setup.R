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
  'foreign', 'doBy', 'lme4', 'plm', 'MASS')
loadPkg(pks)

# gg theme
theme_set(theme_bw())

# Helpful functions
numSM=function(x){as.numeric(as.character(x))}
charSM=function(x){as.character(x)}
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
    loadPkg('plyr')

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