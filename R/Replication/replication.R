# Run to replicate figures and tables in paper
# To correctly print math in pictures, save output using tikz from tikzDevice pkg instead of ggsave
print('Creating Figure 1......'); source('paperScripts/Figure1.R')
print('Creating Figure 2......'); source('paperScripts/Figure2.R')
print('Creating Figure 3 & Table 1......'); source('paperScripts/Figure3_Table1.R')
print('Creating Figures 4 & 5......'); source('paperScripts/Figures4-5.R')
print('Creating Tables 2 & 3......'); source('paperScripts/Tables2-3.R')

# Run to replicate figures and tables in appendix
print('Creating Online Appendix Figures 1 & 2......'); source('onlineAppendixScripts/Figures1-2.R')
print('Creating Online Appendix Table 1......'); source('onlineAppendixScripts/Table1.R')
print('Creating Online Appendix Table 2......'); source('onlineAppendixScripts/Table2.R')
print('Creating Online Appendix Table 3......'); source('onlineAppendixScripts/Table3.R')