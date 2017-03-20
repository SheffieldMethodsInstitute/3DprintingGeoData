#OA to LSOA summing
geolibs <- c("spdep","ggmap","rgdal","rgeos","maptools","dplyr","tidyr","tmap","raster", "dplyr", "tidyr","assertthat","data.table",'readr','pryr','combinat')
lapply(geolibs, require, character.only = TRUE)

#~~~~~~~~~~~~~~
#Ethnicity----
#~~~~~~~~~~~~~~

lookup <- read_csv('data/OA to LSOA 2011.csv')

#reduce lookup to Y&H
#table(lookup$OA11CD %in% oas$code)
#lookup <- lookup[lookup$OA11CD %in% oas$code,]

#Merge in lookup.
eth <- read_csv('data/ethnicity_Y&H.csv')

eth_LSOA <- merge(eth,lookup[,c(1,2)], by.x = 'geography', by.y = 'OA11CD')

eth_LSOA_summary <- eth_LSOA[,c(5:29)] %>% group_by(LSOA11CD) %>% 
  summarise_each(funs(sum = sum(.)))

#sanity check
table(apply(eth_LSOA[,c(5:28)],2,sum) == apply(eth_LSOA_summary[,c(2:25)],2,sum))

#save
write_csv(eth_LSOA_summary,'data/ethnicity_Y&H_LSOA.csv')

#~~~~~~~~~~~~~~
#Country of Birth----
#~~~~~~~~~~~~~~

lookup <- read_csv('data/OA to LSOA 2011.csv')

#reduce lookup to Y&H
#table(lookup$OA11CD %in% oas$code)
#lookup <- lookup[lookup$OA11CD %in% oas$code,]

#Merge in lookup.
cob <- read_csv('data/countryOfBirth_Yorkshire.csv')

cob_LSOA <- merge(cob,lookup[,c(1,2)], by.x = 'geography', by.y = 'OA11CD')

cob_LSOA_summary <- cob_LSOA[,c(5:ncol(cob_LSOA))] %>% group_by(LSOA11CD) %>% 
  summarise_each(funs(sum = sum(.)))

#sanity check
table(apply(cob_LSOA[,c(5:(ncol(cob_LSOA)-1))],2,sum) == apply(cob_LSOA_summary[,c(2:ncol(cob_LSOA_summary))],2,sum))

#save
write_csv(cob_LSOA_summary,'data/countryOfBirth_Y&H_LSOA.csv')




