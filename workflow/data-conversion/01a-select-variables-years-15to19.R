#--------------------------------#
#  Explore Data                  #
#                                #
#  Meltem Odabas                 #
#  2021-04-22                    #
#--------------------------------#

external='yes'

#data file location
bucket = 'E:/data/doctorate-trajectories'
loc_import = 'psd19Public'   #original data location
loc_export = 'regression_dataset'   #original data location

if(external == 'yes'){
  bucket = 'D:/data/survey-of-doctorate-recipients'
  loc_import = 'psd19Public'   #original data location
  loc_export = 'regression_dataset'   #original data location
}


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

new_df = df_desc %>%
  mutate(NEW_NAME = rep("",nrow(.)))%>% #create a new empty character column
  mutate(NEW_NAME = case_when(          #assign new elements to the new column
    
    SAS_NAME == "OBSNUM"       ~     "ID",
    SAS_NAME == "REFYR"        ~     "SURVEY_YEAR",
    SAS_NAME == "COHORT"          ~     "COHORT",

    SAS_NAME == "ACADADJF"       ~     "WORK_AC_ADJUNCT",
    SAS_NAME == "ACADADMN"       ~     "WORK_AC_DEAN_PRES",
    SAS_NAME == "ACADNA"         ~     "WORK_AC_NA",
    SAS_NAME == "ACADOTHP"       ~     "WORK_AC_ASSISTANT",
    SAS_NAME == "ACADPDOC"       ~     "WORK_AC_POSTDOC",
    SAS_NAME == "ACADRCHF"       ~     "WORK_AC_FAC_RESEARCH",
    SAS_NAME == "ACADTCHF"       ~     "WORK_AC_FAC_TEACHING",
    SAS_NAME == "FACRANK"        ~     "WORK_AC_FAC_RANK",
    SAS_NAME == "LOOKWK"         ~     "WORK_UNEMP_LOOKING",
    SAS_NAME == "WRKG"           ~     "WORK_PAY_VS_PROFIT",
    
    SAS_NAME == "EMED"           ~     "INS_EDUC",
    SAS_NAME == "EDTP"           ~     "INS_EDUC_TYPE",
    SAS_NAME == "EMSECSM"        ~     "INS_SECTOR",
    SAS_NAME == "EMSECDT"        ~     "INS_SECTOR_DETAIL",
    
    SAS_NAME == "AGEGRP"         ~     "DMG_AGE_GROUP",
    SAS_NAME == "GENDER"         ~     "DMG_GENDER",
    SAS_NAME == "RACETHMP"       ~     "DMG_RACE",
    SAS_NAME == "MINRTY"         ~     "DMG_MINORITY",
    
    SAS_NAME == "CHLVIN"         ~     "DMG_CHILD",
    SAS_NAME == "CHTOTPB"        ~     "DMG_N_CHILD",
    SAS_NAME == "MARSTA"         ~     "DMG_MARITAL_ST",
    SAS_NAME == "SPOWK"          ~     "DMG_SPOUSE_WORK_ST",
    SAS_NAME == "EDDAD"          ~     "DMG_EDUC_FATHER",
    SAS_NAME == "EDMOM"          ~     "DMG_EDUC_MOTHER",
    SAS_NAME == "FNINUS"         ~     "DMG_LIVE_IN_US",
    
    SAS_NAME == "CTZN"           ~     "DMG_CITIZEN",
    SAS_NAME == "CTZN_DRF"       ~     "DMG_CITIZEN_DOCTORATE",
    
    SAS_NAME == "D25YRP"         ~     "DEG_YEAR_AWARD_2ND_HIGH",
    SAS_NAME == "D2DG"           ~     "DEG_TYPE_2ND_HIGH",
    SAS_NAME == "D2DGRUS"        ~     "DEG_LOC_US_2ND_HIGH",
    SAS_NAME == "D2RGNP"         ~     "DEG_LOC_CODE_2ND_HIGH",
    SAS_NAME == "ND2MEMG"        ~     "DEG_FIELD_2ND_HIGH",
    
    SAS_NAME == "D35YRP"         ~     "DEG_YEAR_AWARD_3RD_HIGH",
    SAS_NAME == "D3DG"           ~     "DEG_TYPE_3RD_HIGH",
    SAS_NAME == "D3DGRUS"        ~     "DEG_LOC_US_3RD_HIGH",
    SAS_NAME == "D3RGNP"         ~     "DEG_LOC_CODE_3RD_HIGH",
    SAS_NAME == "ND3MEMG"        ~     "DEG_FIELD_3RD_HIGH",
    
    SAS_NAME == "HDAY5P"         ~     "DEG_YEAR_AWARD_1ST_HIGH",
    SAS_NAME == "HDDGRUS"        ~     "DEG_LOC_US_1ST_HIGH",
    SAS_NAME == "HDRGNP"         ~     "DEG_LOC_CODE_1ST_HIGH",
    SAS_NAME == "NDGMEMG"        ~     "DEG_FIELD_1ST_HIGH",
    SAS_NAME == "NDGMENGP"       ~     "DEG_FIELD_MINOR_1ST_HIGH",
    
    SAS_NAME == "MR5YRP"         ~     "DEG_YEAR_AWARD_MOST_RECENT",
    SAS_NAME == "MRDG"           ~     "DEG_TYPE_MOST_RECENT",
    SAS_NAME == "MRDGRUS"        ~     "DEG_LOC_US_MOST_RECENT",
    SAS_NAME == "NMRMEMG"        ~     "DEG_FIELD_MOST_RECENT", 
    SAS_NAME == "NMRMENGP"       ~     "DEG_FIELD_MINOR_MOST_RECENT",
    
  )) %>%
  filter(NEW_NAME != "") #filter new column if no new name is assigned (we will not use those variables in analysis)

write_csv(new_df, file.path(bucket,loc_export,'regression_variables_15to19.csv'))

