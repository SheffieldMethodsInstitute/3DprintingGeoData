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

#~~~~~~~~~~~~~~~~~~~~~~~~~~
#Crimes per head of population or per 1000 pop if a more sensible number----

cob <- read_csv('data/countryOfBirth_Y&H_LSOA.csv')

#887 ... other way round should be...?
table(cob$LSOA11CD %in% crm_df$LSOAcode)
#Well, we don't want any not in Y&H, do we?
table(unique(crm_df$LSOAcode) %in% unique(cob$LSOA11CD))

plot(lsoas[lsoas$code %in% cob$LSOA11CD,])
points(crmgeo,col='GREEN')

crmgeo2 <- merge(crmgeo,cob[,c(1:2)], by.x = 'LSOAcode', by.y = 'LSOA11CD')

names(crmgeo2@data)[names(crmgeo2@data)=='Country of Birth: All categories: Country of birth; measures: Value'] <- 'totalZonePop'

saveRDS(crmgeo2,'local/databackups/CrimeSouthYorkshire_w_LSOAs_nTotalPop.rds')

#~~~~~~~~~~~
crm_df <- data.frame(crmgeo2)

#Count of crime per zone. Want to look at three versions:
#All crimes
#Breakdown by type
#Remove antisocial behaviour, sum all others

allCrimesPerZonePerPop <- crm_df %>% 
  group_by(LSOAcode) %>% 
  summarise(count = n(), totalCrimesPer1000People = (n()/max(totalZonePop))*1000)#max totalzonepop just provides single value. Same for each zone.

crimesByTypePerZonePerPop <- crm_df %>% 
  group_by(LSOAcode,Crime.type) %>% 
  summarise(crimesPer1000People = (n()/max(totalZonePop))*1000)

#remove NAs
crimesByTypePerZonePerPop <- crimesByTypePerZonePerPop[!is.na(crimesByTypePerZonePerPop$crimesPer1000People),]

crimesByTypeLong <- spread(crimesByTypePerZonePerPop,Crime.type,crimesPer1000People)

pairs(crimesByTypeLong[,c(2:4)])
pairs(crimesByTypeLong[,c(2,6:7)])
pairs(crimesByTypeLong[,c(2,8:9)])
pairs(crimesByTypeLong[,c(2,10:11)])
pairs(crimesByTypeLong[,c(2,12)])

corz <- cor(crimesByTypeLong[,c(2:12)],use='complete.obs') %>% data.frame

eigen(corz)

#Do column sums
crimesByTypeLong$allCrimePer1000Pop <- rowSums(crimesByTypeLong[,c(2:12)],na.rm = T)
crimesByTypeLong$allCrimePer1000Pop_ExclASB <- rowSums(crimesByTypeLong[,c(3:12)],na.rm = T)



























