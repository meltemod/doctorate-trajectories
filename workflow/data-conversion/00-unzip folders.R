########################
#  00 - unzip
########################

external = 'yes'

if(external == 'yes'){
  loc_folder = 'D:/data/survey-of-doctorate-recipients'
  loc_export = file.path(loc_folder,'sestat_data')
}

zipfiles = dir(loc_folder)

for (z in zipfiles){
  z2 = gsub('.zip','',z)
  loc_tmp = file.path(loc_export,z2)
  dir.create(loc_tmp)
  unzip(zipfile = file.path(loc_folder,z), exdir = loc_tmp)
  file.remove(file.path(loc_folder,z))
}


