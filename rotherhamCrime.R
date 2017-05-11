##  Rotherham: Crime pop 3D plots----
##  >list of relevant file paths. All map data are contained in data/boundarydata so just need their file names. Change to suit
source('r2stl/R/r2stl_geo.r')
oas.path<-'rotherham_oa_2011_noproj'
lsoas.path<-'rotherham_lsoa_2011_noproj'
roads.path<-'rotherham_mainRoadsBuffer25m'
data.tab <- read_csv('data/Crimes_SouthYorkshire_byLSOA.csv') #table with the statistics of interest
summary(data.tab); head(data.tab)

##  >load in library and data: maps; roads and tables----
##  Saving projection system and reprojecting the r.ham shp file
osgb36<-("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +datum=OSGB36 +units=m +no_defs +ellps=airy +towgs84=446.448,-125.157,542.060,0.1502,0.2470,0.8421,-20.4894")

##  Read in oas and lsoa map files
oas<-readOGR(dsn='data/boundarydata',layer=oas.path,p4s=osgb36)
lsoas<-readOGR(dsn='data/boundarydata',layer=lsoas.path,p4s=osgb36)

##  Output: data.tab: table of stats and lsoa/oa; oas: polygon file for oas; lsoas; polygon file for lsoas; 


##  >Merge map files with datatables====
unique.lsoa<-match(lsoas$lsoa11cd,data.tab$LSOAcode)
data_geo_lsoa <- merge(lsoas[,c('lsoa11cd')], data.tab[unique.lsoa,], by.x = 'lsoa11cd', by.y = 'LSOAcode')
##  Building a relief layer (which is basically roads and etc)====
##  Basically a relief layer is the layer upon which we indent onto the rest of the data 

#Start with a rasterised version of our main file
r <- rasterToFitShapefileExtent(data_geo_lsoa,50)

##  Load in the roads
roads.path<-'rotherham_mainRoadsBuffer25m'
mway.path<-'rotherham_MotorwayBuffer60m'
##  load in the roads file
roads <- readOGR('data/boundarydata',roads.path)
mway <- readOGR('data/boundarydata',mway.path)

##  Rasterise the roads, the relief variable in roads, and r (the rasterised geo file)
roads$relief<--1
roadsreliefRaster <- rasterize(roads,r,roads$relief)
roadsreliefRaster[is.na(roadsreliefRaster)] <- 0#

##  Rasterise the motorways 
#Currently a single polygon with no attributes. Add one with the value we want to use
mway$relief <- -2
mwayreliefRaster <- rasterize(mway,r,mway$relief)
mwayreliefRaster[is.na(mwayreliefRaster)] <- 0#

##  >Note: North arrow rasterising routine----
northArrow <- raster('images/northArrow1.tif')
values(northArrow) <- ifelse(values(northArrow) == 0,-1,0)

sm <- aggregate(northArrow, fact=10)
proj4string(sm) <- proj4string(roadsreliefRaster)

#Chosen coordinates for corner of image
newx <- 439903.630
newy <- 379484.665
#Multiplication of image size
xmax <- extent(sm)[2] * 6
ymax <- extent(sm)[4] * 6

extent(sm) <- c(xmin <- newx, xmax <- newx + xmax, ymin <- newy, ymax <- newy + ymax)
extent(sm)

#Getting the north arrow onto a blank layer
sm2 <- projectRaster(sm,roadsreliefRaster)
sm2 <- 1-sm2

values(sm2)[is.na(values(sm2))]<-min(values(sm2),na.rm=T)
values(sm2)<-ifelse(values(sm2)>1.9,-2,0)

reliefRaster <- sm2 + min(roadsreliefRaster,mwayreliefRaster, na.rm = T)
reliefRaster ##need positve values
values(reliefRaster)<-values(reliefRaster)+3
plot(reliefRaster)
reliefRaster
##  >test plots----
summary(data_geo_lsoa)
plot(reliefRaster)
data_geo_lsoa$crim.fixed<-data_geo_lsoa$allCrimePer1000Pop/100

r2stl_geo(
  data_geo_lsoa,
  'crim.fixed',
  gridResolution=50,
  keepXYratio = T,
  zRatio = 0.25,
  show.persp = F,
  filename= 'stl/roth_crime.1000 noint.stl',
  reliefLayer = reliefRaster,
  interpolate = 0
)

