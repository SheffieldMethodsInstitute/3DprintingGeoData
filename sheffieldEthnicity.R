#Tweaked r2stl code
source('r2stl/R/r2stl_geo.r')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Sheffield 1: non-white pop, Sheffield OAs----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~
#OA level----
#~~~~~~~~~~~~

#Get output areas, reduce to Sheffield
oas <- readOGR('data/boundarydata','sheffield_oa_2011_MinuswestEnd')
plot(oas)

#Get ethnicity data
eth <- read_csv('data/ethnicity_Y&H.csv')

#check OA match. Tick. (1817 zones.)
table(oas$code %in% eth$geography)

#Subset to Sheffield
eth <- eth[eth$geography %in% oas$code,]

#tidy to make easier to read
names(eth) <- gsub('; measures: Value','', names(eth))

#non-white is just all usual residents minus white
eth$nonwhite <- eth$`Ethnic Group: All usual residents`-eth$`Ethnic Group: White`

#as % of zone pop
eth$nonwhiteZoneProp <- (eth$nonwhite/eth$`Ethnic Group: All usual residents`)*100
hist(eth$nonwhiteZoneProp)

#merge with geography - keep only the column we want for now.
eth_geo <- merge(oas[,c('code')], eth[,c('geography','nonwhiteZoneProp')], by.x = 'code', by.y = 'geography')

r2stl_geo(
  eth_geo,
  'nonwhiteZoneProp',
  gridResolution=50,
  keepXYratio = T,
  zRatio = 0.25,
  show.persp = F,
  filename= 'stl/test.stl'
)

#~~~~~~~~~~~~~~
#LSOA level----
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







