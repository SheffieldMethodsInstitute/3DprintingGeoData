#Tweaked r2stl code
source('r2stl/R/r2stl_geo.r')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Sheffield 1: non-white pop, Sheffield OAs----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Get output areas, reduce to Sheffield
oas <- readOGR('data/boundarydata','sheffield_oa_2011')
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

#Drop some of the outlying OAs to get a better look at the centre
#These are the ones to drop
dropz <- readOGR('data/boundarydata','sheffield_oa_2011_westEnd')

eth_geo <- eth_geo[!(eth_geo$code %in% dropz$code),]

r2stl_geo(
  eth_geo,
  'nonwhiteZoneProp',
  gridResolution=50,
  keepXYratio = T,
  zRatio = 0.25,
  show.persp = F,
  filename= 'stl/test.stl'
)



plot(eth_geo)






