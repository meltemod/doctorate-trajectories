#--------------------------------------------------               #
#                                                                 #
#  Combine datasets from all three years                          #
#                                                                 #
#  Meltem Odabas                                                  #
#  2021-06-03                                                     #
#--------------------------------------------------               #


#data file location
bucket = 'E:/data/doctorate-trajectories'
loc_import =loc_export = 'regression_dataset'  

#load libraries
library(tidyverse)
library(data.table)

years = c('08','10','13')

datalist = list()
a=1
for (i in years){
  datalist[[a]] = fread(file.path(bucket,loc_export,paste0('regression_data_',i,'.csv')))
  a = a+1
}

df = rbindlist(datalist)

#some observations are in the dataset for more than once!
length(unique(df$ID))
nrow(unique(df))

write_csv(df, file.path(bucket,loc_export,'regression_data_08_13.csv'))
rm(list=ls())
