# Run to replicate figures and tables in paper
# To correctly print math in pictures, save output using tikz from tikzDevice pkg instead of ggsave
print('Creating Figure 1......'); source('paperScripts/Figure1.R')
print('Creating Figure 2......'); source('paperScripts/Figure2.R')
print('Creating Figure 3 & Table 1......'); source('paperScripts/Figure3_Table1.R')
print('Creating Figures 4 & 5......'); source('paperScripts/Figures4-5.R')
print('Creating Tables 2 & 3......'); source('paperScripts/Tables2-3.R')