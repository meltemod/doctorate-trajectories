#--------------------------------#
#  Explore Data                  #
#                                #
#  Meltem Odabas                 #
#  2021-04-22                    #
#--------------------------------#

#data file location
bucket = 'E:/data/doctorate-trajectories'
loc_import = 'psd19Public'   #original data location
loc_export = 'regression_dataset'   #original data location

#create export folder
if(dir.exists(file.path(bucket,loc_export)) == FALSE){dir.create(file.path(bucket,loc_export))}

#libraries
library(haven)       # to load SAS data file
library(tidyverse)   # for data conversions and visualizations
library(data.table)  # for data conversions 
library(sjlabelled)  #for data labels and looking through entry types

#load data
fname = 'epsd19.sas7bdat' # SAS data file
df = read_sas(file.path(bucket,loc_import,fname))


#load data description file
fname_desc = 'Dpsd19.csv' # I converted the excel file to csv first.
df_desc = read_csv(file.path(bucket,loc_import,fname_desc))
view(df_desc) #view descriptions as a new tab on R Studio


#see the index.html and click on questionnaire for details.

#identifiers
names(df)[161]; df_desc[161,1:3]   #observation number
names(df)[184]; df_desc[184,1:3]   #survey year

#degree variables
names(df)[c(134:139,142:143)]; df_desc[c(134:139,142:143),1:3] #major field for 1st, 2nd & 3rd highest degrees
names(df)[c(102:104,55:62)]; df_desc[c(102:104,55:62),1:3]     #awarded year for 1st, 2nd & 3rd highest degrees
names(df)[c(119,123:124)]; df_desc[c(119,123:124),1:3]     #awarded year for most recent degree type

#work/employer variables
names(df)[c(230,112,155:160)]; df_desc[c(230,112,155:160),1:3] #working or not; looking for work; reasons for not working
names(df)[c(1:7,70,69)]; df_desc[c(1:7,70,69),1:3]             #academic institution(yes/no); academic institution type; position type (academic varieties vs. non-academic),
names(df)[c(72,73)]; df_desc[c(72,73),1:3]                     #employer sector

#demographics
names(df)[50:53]; df_desc[50:53,1:3]             #citizenship status; 
names(df)[26]; df_desc[26,1:3]                   #age group (5-year)
names(df)[99]; df_desc[99,1:3]                   #gender
names(df)[c(118,183)]; df_desc[c(118,183),1:3]   #minority and race
names(df)[c(67,68)]; df_desc[c(67,68),1:3]       #mom and dad educ level
names(df)[c(114,200)]; df_desc[c(114,200),1:3]   #married; spouse work (full time[1], part time[2], no[3]) 
names(df)[c(41,46)]; df_desc[c(41,46),1:3]       #children & N children

#extract the list of all variable names
var_n = c(161,184,
          134:139,142:143,
          102:104,55:62,
          119,123:124,
          230,112,155:160,
          1:7,70,69,
          72,73,
          50:53,26,99,118,183,67,68,114,200,41,46)

newdf = df[,var_n]
vars = names(newdf)

#save variable names
df_varnames = df_desc[df_desc$SAS_NAME %in% vars,]
write_csv(df_varnames, file.path(bucket,loc_export,'regression_variables.csv'))

