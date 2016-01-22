# Workspace
source('paperScripts/setup.R')

######################################################################
# Aggregate GDP Growth and Distance
load(paste0(pathData,'combinedData.rda'))

# Create bucketed version of mindist and find averag GDP gr
yData$cityDistCat=contToCat(yData$minDist.min, .25)
yData$capDistCat=contToCat(yData$capDist.min, .25)

# Conf intervals using summarySE:
gdpByCityDist=na.omit( summarySE(data=yData, measurevar='gdpGr_l0', 
	groupvars='cityDistCat', na.rm=TRUE) )
gdpByCapDist=na.omit( summarySE(data=yData, measurevar='gdpGr_l0', 
	groupvars='capDistCat', na.rm=TRUE) )

names(gdpByCityDist)[1]='distCat'; names(gdpByCapDist)[1]='distCat'
ggData=cbind(
	rbind(gdpByCityDist, gdpByCapDist),
	type=c(rep('Min. City Dist.$_{t-1}$',nrow(gdpByCityDist)),
		rep('Min. Capital Dist.$_{t-1}$',nrow(gdpByCapDist)) ),
	cut=rep( c('0-25$^{th}$','26-50$^{th}$','51-75$^{th}$','76-100$^{th}$'), 2 )
	)

## FIGURE 2 ##
tmp=ggplot(ggData, aes(x=cut, y=gdpGr_l0))
tmp=tmp + geom_bar(stat='identity', fill='grey')
tmp=tmp + geom_errorbar(aes(ymin=gdpGr_l0-se, ymax=gdpGr_l0+se),width=.2)
tmp=tmp + ylab("\\% $\\Delta$ GDP$_{t}$")+xlab('')
tmp=tmp + facet_wrap(~type)
tmp=tmp + theme( axis.title.y=element_text(vjust=1),
  axis.ticks=element_blank(),legend.position='none',
  panel.grid.major=element_blank(), panel.grid.minor=element_blank() )
ggsave(file=paste0(pathGraphics, 'Figure2.pdf'), plot=tmp, width=7, height=4)
######################################################################