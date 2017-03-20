#OA to LSOA summing
geolibs <- c("spdep","ggmap","rgdal","rgeos","maptools","dplyr","tidyr","tmap","raster", "dplyr", "tidyr","assertthat","data.table",'readr','pryr','combinat')
lapply(geolibs, require, character.only = TRUE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Subset Sheffield LSOA from OA----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

oas <- readOGR('data/boundarydata','sheffield_oa_2011_MinuswestEnd')
plot(oas, col = 'red')

lookup <- read_csv('data/OA to LSOA 2011.csv')

oas$code %in% lookup$OA11CD %>% table
keepTheseLSOAs <- lookup[lookup$OA11CD %in% oas$code,]

#On Dan's local machine - don't want to sync for all England
lsoas <- readOGR('C:/Data/MapPolygons/England/2011/England_lsoa_2011_gen_clipped','england_lsoa_2011_gen_clipped')

(lsoas$code %in% keepTheseLSOAs$LSOA11CD) %>% table
lsoasSheff <- lsoas[lsoas$code %in% keepTheseLSOAs$LSOA11CD,]
plot(lsoasSheff,col='red')

#So we gained some large ones again. Might drop em again. they are (via qGIS):
#E01008128, E01008129, E01007956
(lsoas$code %in% c('E01008128', 'E01008129', 'E01007956')) %>% table
lsoasSheff <- lsoasSheff[!(lsoasSheff$code %in% c('E01008128', 'E01008129', 'E01007956')),]
plot(lsoasSheff,col='red')

#Save...
writeOGR(lsoasSheff,'data/boundarydata','sheffield_oa_2011_MinuswestEnd',driver="ESRI Shapefile", overwrite_layer=T)

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

