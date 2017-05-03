#Crime data prep. Crime data is local, but from:
#https://data.police.uk
geolibs <- c("spdep","ggmap","rgdal","rgeos","maptools","dplyr","tidyr","tmap","raster", "dplyr", "tidyr","assertthat","data.table",'readr','pryr','combinat')
lapply(geolibs, require, character.only = TRUE)

#Look at data...
crm <- read_csv('C:/Data/Crime/SouthYorkshireForceCrimeStats/2011-01/2011-01-south-yorkshire-street.csv')

#Combine all months for 2011 (same year as census data)
crmlist <- lapply(list.files('C:/Data/Crime/SouthYorkshireForceCrimeStats/', pattern='2011', recursive = T, full.names = T), read_csv)

#http://stackoverflow.com/a/29932320
crm_onedf <- as.data.frame(bind_rows(crmlist))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Count number of crimes in each LSOA
#lsoas <- readShapeSpatial('C:/Data/MapPolygons/England/2011/England_lsoa_2011_clipped/england_lsoa_2011_clipped.shp')
#need the projection!
lsoas <- readOGR('C:/Data/MapPolygons/England/2011/England_lsoa_2011_clipped','england_lsoa_2011_clipped')

crmgeo <- crm_onedf
#drop missing coords. 206K crimes left for 2011
table(0 + is.na(crmgeo$Longitude))
crmgeo <- crmgeo[!is.na(crmgeo$Longitude),]

coordinates(crmgeo) <- ~Longitude+Latitude
proj4string(crmgeo) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"

crmgeo <- spTransform(crmgeo,proj4string(lsoas))

#check
plot(crmgeo[sample(1000),])
plot(lsoas, add=T)

#Do we have some Sheffield LSOAs to check it's in the right place?
shefLSOA <- readOGR('data/boundarydata','sheffield_lsoa_2011_MinuswestEnd')

plot(shefLSOA)
points(crmgeo)

#Correct number of matches, will be same order
crmgeo_lsoa <- crmgeo %over% lsoas

crmgeo$LSOAname <- crmgeo_lsoa$name
crmgeo$LSOAcode <- crmgeo_lsoa$code

#save a copy...
#saveRDS(crmgeo,'local/databackups/crmgeo_w_lsoas.rds')
saveRDS(crmgeo,'data/CrimeSouthYorkshire_w_LSOAs.rds')

#~~~~~~~~~~~~~~~~~~~~~~~~~~
#Quick look at what crimes we've got
table(crmgeo$`Crime type`)

crm_df <- data.frame(crmgeo)

ggplot(crm_df, aes(x = factor(Crime.type))) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 270, hjust = 0, vjust = 0.3))



















