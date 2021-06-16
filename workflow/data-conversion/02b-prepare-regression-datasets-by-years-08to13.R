#--------------------------------------------------               #
#                                                                 #
#  Reformat and Rename Variables to prepare regression data       #
#                                                                 #
#  Meltem Odabas                                                  #
#  2021-06-03                                                     #
#--------------------------------------------------               #

#SELECT DATA COLLECTION YEAR (08, 10 or 13)
select_year_for_data = function(year){
  
  print(paste('year selected:',year))
  
  #data file location
  bucket = 'E:/data/doctorate-trajectories'
  loc_import = paste0('sdr',year,'_Public')   #original data location
  loc_export = 'regression_dataset'   #original data location
  
  #create export folder
  if(dir.exists(file.path(bucket,loc_export)) == FALSE){dir.create(file.path(bucket,loc_export))}
  
  #libraries
  library(haven)       # to load SAS data file
  library(tidyverse)   # for data conversions and visualizations
  library(data.table)  # for data conversions 
  library(sjlabelled)  #for data labels and looking through entry types
  
  #load data
  print('loading dataset...')
  fname = paste0('epsd',year,'.sas7bdat') # SAS data file
  df = read_sas(file.path(bucket,loc_import,fname))
  df = as_tibble(df)
  
  #take the subset of the variables of interest
  print('subsetting variables...')
  df_varnames = read_csv(file.path(bucket,loc_export,'regression_variables_08to13.csv'))
  df = df[,names(df) %in% df_varnames$SAS_NAME]
  
  print('reformatting variables...')
  #reformat AGEGRP variable
  df = df %>%
    mutate(AGEGRP = rep(NA_character_,nrow(df))) %>%
    mutate(AGEGRP = case_when(
      AGEP < 30                 ~    '29minus',
      AGEP >= 30 &  AGEP < 35   ~    '30-34',
      AGEP >= 35 &  AGEP < 40   ~    '35-39',
      AGEP >= 40 &  AGEP < 45   ~    '40-44',
      AGEP >= 45 &  AGEP < 50   ~    '45-49',
      AGEP >= 50 &  AGEP < 55   ~    '50-54',
      AGEP >= 55 &  AGEP < 60   ~    '55-59',
      AGEP >= 60 &  AGEP < 65   ~    '60-64',
      AGEP >= 65 &  AGEP < 70   ~    '65-69',
      AGEP >= 70                ~    '70plus'
    )) %>%
    select(-AGEP) %>%
    rename(AGEP = AGEGRP)
  
  
  #reformat CHLVIN and CHTOTPB variable (number of children)

  df = df %>%
    mutate(CHTOTPB = case_when(
      CH6IN == 'N'     ~    '0',     #if no children living in the HH, 0
      CH611IN == 'N'     ~    '0',     #if no children living in the HH, 0
      CH1218IN == 'N'     ~    '0',     #if no children living in the HH, 0
      CHTOTPB == '1'    ~    '1',     #one child
      CHTOTPB == '2'    ~    '2plus',  #2 or more
      TRUE              ~   NA_character_
    ))
  
  #reformat degree award year period variables.
  for (i in c('HD03Y5P','MR03Y5P')){
    df = df %>%
      mutate(tmp = as.numeric(!!as.symbol(i))) %>%
      mutate(tmp = case_when(
        tmp < max(df$REFYR)     ~    tmp,
        TRUE                    ~    NA_real_
      )) %>%
      mutate_at(i, funs (case_when(
        is.na(tmp) == FALSE    ~  paste0(tmp,'-',tmp+4),
        TRUE                    ~ 'SKIP'
      ))) %>%
      select(-tmp)
  }
  
  
  #reformat degree type variables.
  for (i in c('DGRDG','MRDG')){
    df = df %>%
      mutate_at(i, funs (case_when(
        . == '0'    ~  'NODEG',
        . == '1'    ~  'BA',
        . == '2'    ~  'MA',
        . == '3'    ~  'PHD',
        . == '4'    ~  'PROF',
        . == '5'    ~  'OTH',
        TRUE        ~  'SKIP'
        
      ))) 
  }
  
  
  #reformat sector vars

  df = df %>%
    mutate(EMSECPB   = case_when(
      EMSECPB == '2'   ~  'EDUC-4YR',
      EMSECPB == '1'   ~  'EDUC-2YR',
      EMSECPB == '4'   ~  'IND',
      EMSECPB == '3'   ~  'GOV',
      TRUE          ~  'SKIP'
    ))
  
  #reformat race vars
  df = df %>%
    mutate(RACETHMP   = case_when(
      RACETHMP == '1'   ~  'A',
      RACETHMP == '5'   ~  'W',
      RACETHMP == '7'   ~  'MNRTY',
      RACETHMP == 'M'   ~  NA_character_,
      TRUE              ~  'OTH'
    ))
  

  #reformat citizenship variables
  for (i in c('CTZUSIN')){
    df = df %>%
      mutate_at(i, funs (case_when(
        . == 'Y'    ~  'US',
        . == 'N'    ~  'NONUS',
        . == 'M'    ~  'MISSING'
        
      ))) 
  }
  
  #reformat degree field variables
  for (i in c('NDGMEMG', 'NMRMEMG')){
    df = df %>%
      mutate_at(i, funs (case_when(
        . == '1'    ~  'COMP-MATH',
        . == '2'    ~  'BIO',
        . == '3'    ~  'PHY',
        . == '4'    ~  'SOC',
        . == '5'    ~  'EGN',
        . == '6'    ~  'SE',
        . == '7'    ~  'NONSE',
        . == '8'    ~  'SKIP',
        . == '9'    ~  'OTH',
        
      ))) 
  }
  
  #reorder variable names
  
  df = df[, names(df)[order(names(df))]]
  
  print('renaming variables...')
  #rename variables
  names(df) = df_varnames$NEW_NAME
  
  #reorder variables
  `%notin%` = Negate(`%in%`)
  vv = names(df)[names(df) %notin% c('ID','SURVEY_YEAR')]
  new_order = c('ID','SURVEY_YEAR',vv)
  df = df %>%
    select(eval(new_order))
  #order goes id, survey year, and others in alphabetical order
  
  print('saving dataset...')
  write_csv(df, file.path(bucket,loc_export,paste0('regression_data_',year,'.csv')))
  
  print(paste('data conversion for year', year, 'is complete.'))
  flush.console()
  
}

select_year_for_data(13)
list=setdiff(ls(), "select_year_for_data")
select_year_for_data(10)
list=setdiff(ls(), "select_year_for_data")
select_year_for_data('08')
rm(list=ls())

