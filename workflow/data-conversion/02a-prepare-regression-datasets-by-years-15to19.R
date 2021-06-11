#--------------------------------------------------               #
#                                                                 #
#  Reformat and Rename Variables to prepare regression data       #
#                                                                 #
#  Meltem Odabas                                                  #
#  2021-06-03                                                     #
#--------------------------------------------------               #

#SELECT DATA COLLECTION YEAR (19, 17 or 15)
select_year_for_data = function(year){
  
  print(paste('year selected:',year))
  
  #data file location
  bucket = 'E:/data/doctorate-trajectories'
  loc_import = paste0('psd',year,'Public')   #original data location
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
  
  #if id variable id named as REFID, rename as OBSNUM, and reorder variables by alphabetical order
  if('REFID' %in% names(df) == TRUE){ names(df)[which(names(df) == 'REFID')] = 'OBSNUM' ;  df = df[, order(names(df))]}
  
  #take the subset of the variables of interest
  print('subsetting variables...')
  df_varnames = read_csv(file.path(bucket,loc_export,'regression_variables_15to19.csv'))
  df = df[,names(df) %in% df_varnames$SAS_NAME]
  
  print('reformatting variables...')
  #reformat AGEGRP variable
  df = df %>%
    mutate(AGEGRP = as.character(AGEGRP)) %>%
    mutate(AGEGRP = case_when(
      AGEGRP == '25'    ~    '29minus',
      AGEGRP == '30'    ~    '30-34',
      AGEGRP == '35'    ~    '35-39',
      AGEGRP == '40'    ~    '40-44',
      AGEGRP == '45'    ~    '45-49',
      AGEGRP == '50'    ~    '50-54',
      AGEGRP == '55'    ~    '55-59',
      AGEGRP == '60'    ~    '60-64',
      AGEGRP == '65'    ~    '65-69',
      AGEGRP == '70'    ~    '70plus'
    ))
  
  
  #reformat CHTOTPB variable (number of children)
  df = df %>%
    mutate(CHTOTPB = case_when(
      CHLVIN == 'N'     ~    '0',     #if no children living in the HH, 0
      CHTOTPB == '1'    ~    '1',     #one child
      CHTOTPB == '2'    ~    '2plus'  #2 or more
    ))
  
  #reformat degree award year period variables.
  for (i in c('D25YRP','D35YRP','HDAY5P','MR5YRP')){
    df = df %>%
      mutate(tmp = as.numeric(!!as.symbol(i))) %>%
      mutate(tmp = case_when(
        tmp < max(df$REFYR)     ~    tmp,
        TRUE                    ~ NA_real_
      )) %>%
      mutate_at(i, funs (case_when(
        is.na(tmp) == FALSE    ~  paste0(tmp,'-',tmp+4),
        TRUE                    ~ 'SKIP'
      ))) %>%
      select(-tmp)
  }
  
  
  #reformat degree type variables.
  for (i in c('D2DG','D3DG','MRDG')){
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
  
  #reformat education institution type variable
  df = df %>%
    mutate(EDTP = case_when(
      EDTP == '1'   ~  'PRECOLLEGE',
      EDTP == '2'   ~  '2YEARCOLLEGE',
      EDTP == '3'   ~  '4YEARCOLLEGE',
      EDTP == '4'   ~  'MEDSCHOOL',
      EDTP == '5'   ~  'RESEARCHINST',
      EDTP == '6'   ~  'OTH',
      TRUE          ~  'SKIP'
    ))
  
  #reformat family education variables
  for (i in c('EDDAD','EDMOM')){
    df = df %>%
      mutate_at(i, funs (case_when(
        . == '1'    ~  'BELOWHS',
        . == '2'    ~  'HS',
        . == '3'    ~  'COLLEGE',
        . == '4'    ~  'BA',
        . == '5'    ~  'MA',
        . == '6'    ~  'PROF',
        . == '7'    ~  'PHD',
        . == '8'    ~  'SKIP',
        . == '9'    ~  NA_character_,
        
      ))) 
  }
  
  #reformat sector vars
  df = df %>%
    mutate(EMSECSM  = case_when(
      EMSECSM == '1'   ~  'EDUC',
      EMSECSM == '2'   ~  'GOV',
      EMSECSM == '3'   ~  'IND',
      TRUE          ~  'SKIP'
    ))
  
  df = df %>%
    mutate(EMSECDT   = case_when(
      EMSECDT == '11'   ~  'EDUC-4YR',
      EMSECDT == '12'   ~  'EDUC-2YR',
      EMSECDT == '21'   ~  'IND-FORPROFIT',
      EMSECDT == '22'   ~  'IND-SELFEMP',
      EMSECDT == '23'   ~  'IND-NONPROFIT',
      EMSECDT == '31'   ~  'GOV-FED',
      EMSECDT == '32'   ~  'GOV-LOCAL',
      EMSECDT == '33'   ~  'GOV-NONUS',
      TRUE          ~  'SKIP'
    ))
  
  #reformat race vars
  df = df %>%
    mutate(RACETHMP   = case_when(
      RACETHMP == '1'   ~  'A',
      RACETHMP == '3'   ~  'B',
      RACETHMP == '4'   ~  'H',
      RACETHMP == '5'   ~  'W',
      TRUE              ~  'OTH'
    ))
  
  #reformat faculty rank variable
  df = df %>%
    mutate(FACRANK    = case_when(
      FACRANK  == '1'   ~  'NONE',
      FACRANK  == '2'   ~  'NONE',
      FACRANK  == '3'   ~  'PROF',
      FACRANK  == '4'   ~  'ASSOC',
      FACRANK  == '5'   ~  'ASST',
      FACRANK  == '6'   ~  'INSTR',
      FACRANK  == '7'   ~  'LEC',
      FACRANK  == '8'   ~  'OTH',
      TRUE              ~  'SKIP'
    ))
  
  #reformat citizenship variables
  for (i in c('CTZN', 'CTZN_DRF')){
    df = df %>%
      mutate_at(i, funs (case_when(
        . == '1'    ~  'US-NATIVE',
        . == '2'    ~  'US_NATURAL',
        . == '3'    ~  'NONUS-PERRES',
        . == '4'    ~  'NONUS-TEMRES',
        . == '5'    ~  'NONUS-OUTUS',
        . == '6'    ~  'NONUS-UNKNW',
        . == 'M'    ~  'MISSING'
        
      ))) 
  }
  
  #reformat degree field variables
  for (i in c('ND2MEMG', 'ND3MEMG', 'NDGMEMG', 'NMRMEMG')){
    df = df %>%
      mutate_at(i, funs (case_when(
        . == '1'    ~  'COMP-MATH',
        . == '2'    ~  'BIO',
        . == '3'    ~  'PHY',
        . == '4'    ~  'SOC',
        . == '5'    ~  'EGN',
        . == '6'    ~  'SE',
        . == '7'    ~  'NONSE',
        . == '8'    ~  'SKIP'
        
      ))) 
  }
  
  #reformat degree field (minor) variables
  for (i in c('NDGMENGP', 'NMRMENGP')){
    df = df %>%
      mutate_at(i, funs (case_when(
        . == '11'    ~  'COMP',
        . == '12'    ~  'MATH',
        
        . == '21'    ~  'AGRI',
        . == '22'    ~  'BIO',
        . == '23'    ~  'ENV',
        
        . == '31'    ~  'CHEM',
        . == '32'    ~  'EARTH',
        . == '33'    ~  'PHY',
        . == '34'    ~  'PHY-OTH',
        
        . == '41'    ~  'ECON',
        . == '42'    ~  'POLI',
        . == '43'    ~  'PSYCH',
        . == '44'    ~  'SOCANTH',
        . == '45'    ~  'SOC-OTH',
        
        . == '51'    ~  'EGN-AERO',
        . == '52'    ~  'EGN-CHEM',
        . == '53'    ~  'EGN-CIVIL',
        . == '54'    ~  'EGN-COMP',
        . == '55'    ~  'EGN-IND',
        . == '56'    ~  'EGN-MECH',
        . == '57'    ~  'EGN-OTH',
        
        . == '60'    ~  'SE',
        . == '70'    ~  'NONSE',
        
        . == '98'    ~  'SKIP',
        
      ))) 
  }
  
  #reformat marriage status variables
  df = df %>%
    mutate(MARSTA     = case_when(
      MARSTA   == '1'   ~  'MARRIED',
      MARSTA   == '2'   ~  'UNION',
      MARSTA   == '3'   ~  'WIDOWED',
      MARSTA   == '4'   ~  'SEPARATED',
      MARSTA   == '5'   ~  'DIVORCED',
      MARSTA   == '6'   ~  'NEVER MARRIED'
    ))
  
  print('renaming variables...')
  #rename variables
  names(df) = df_varnames$NEW_NAME
  
  #reorder variables
  iid = which(names(df) == 'ID')                                 #id index
  iyear = which(names(df) == 'SURVEY_YEAR')                      #survey year index
  neworder = c(iid, iyear, order(names(df)[-c(iid,iyear)]) )     #order all except id and survey year index
  df = df[ ,names(df)[neworder]]                                 #order goes id, survey year, and others in alphabetical order
  
  print('saving dataset...')
  write_csv(df, file.path(bucket,loc_export,paste0('regression_data_',year,'.csv')))
  
  print(paste('data conversion for year', year, 'is complete.'))
  flush.console()
  
}

select_year_for_data(19)
list=setdiff(ls(), "select_year_for_data")
select_year_for_data(17)
list=setdiff(ls(), "select_year_for_data")
select_year_for_data(15)
rm(list=ls())

