geolibs <- c("spdep","ggmap","rgdal","rgeos","maptools","dplyr","tidyr","tmap","raster", "dplyr", "tidyr","assertthat","data.table",'readr','pryr','combinat','r2stl')
lapply(geolibs, require, character.only = TRUE)

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

#Everything's in British National Grid, so divided by number of metres per grid square we want
width <- (xmax(eth_geo)-xmin(eth_geo))/100
height <- (ymax(eth_geo)-ymin(eth_geo))/100

r <- raster(ncols = width, nrows = height)
proj4string(r) <- proj4string(eth_geo)
extent(r) <- extent(eth_geo)

ethRaster <- rasterize(eth_geo,r,eth_geo$nonwhiteZoneProp)
plot(ethRaster)

zed <- as.matrix(ethRaster)
zed[is.na(zed)] <- 0

r2stl(
  x = 1:nrow(ethRaster), 
  y = 1:ncol(ethRaster), 
  #z = ifelse(is.na(getValues(ethRaster)),0,getValues(ethRaster)),
  z = zed/2,
  z.expand = F,
  show.persp = F,
  filename= 'stl/shefNonWhite_centre2.stl'
)










