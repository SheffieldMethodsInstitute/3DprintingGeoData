##  Rotherham: Non-UK pop and roads----


##  >load in library and data: maps; roads and tables----
source('r2stl/R/r2stl_geo.r')
##  Loading in the data
##  Saving projection system and reprojecting the r.ham shp file
osgb36<-("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +datum=OSGB36 +units=m +no_defs +ellps=airy +towgs84=446.448,-125.157,542.060,0.1502,0.2470,0.8421,-20.4894")

##  Read in oas and lsoa map files
oas<-readOGR(dsn='data/boundarydata',layer='rotherham_oa_2011_noproj',p4s=osgb36)
lsoas<-readOGR(dsn='data/boundarydata',layer='rotherham_lsoa_2011_noproj',p4s=osgb36)

#Get ethnicity data
cob <- read_csv('data/cob lsoa msoa.csv')
# note: read_csv is a tidy function to make things easier to read (still read.csv)

#Subset to Rotherham
roth.id<-grep('Rotherham',cob$LSOA11NM)
cob<-cob[roth.id,]

##  load in the roads file
roads <- readOGR('data/boundarydata','rotherham_mainRoadsBuffer25m')
roads$relief <- -1 #the relief variable determines identation in the r2stl function

##  Output: cob: table of c.o.b and lsoa/oa; oas: polygon file for oas; lsoas; polygon file for lsoas; roads: a polygon file of major roads in Rotherham and a relief variable for directon of indentation


##  Graphing data: oa and lsoa----
##  >Note: C.o.b table with map files: keep only the column we want for now (cob.sub).====
cob.sub<-c('OA11CD','nonUKZoneProp.oa','nonUKZoneProp.lsoa','nonUKZoneProp.msoa')
cob_geo <- merge(oas[,c('oa11cd')], cob[,cob.sub], by.x = 'oa11cd', by.y = 'OA11CD')

##  >Saving test .stl do not run----
##  Right now we use the r2stl_geo file to output eth plots by oa and lsoa
r2stl_geo(
  cob_geo,
  'nonUKZoneProp.oa',
  gridResolution=50,
  keepXYratio = T,
  zRatio = 0.25,
  show.persp = F,
  filename= 'stl/nonUKRotherham.stl'
)

r2stl_geo(
  cob_geo,
  'nonUKZoneProp.lsoa',
  gridResolution=50,
  keepXYratio = T,
  zRatio = 0.25,
  show.persp = F,
  filename= 'stl/nonUKRotherham_lsoa.stl'
)

r2stl_geo(
  cob_geo,
  'nonUKZoneProp.msoa',
  gridResolution=50,
  keepXYratio = T,
  zRatio = 0.25,
  show.persp = F,
  filename= 'stl/nonUKRotherham_msoa.stl'
)

##  output: 

##  Graphing data: interpolated plots with roads----
##  We want an lsoa map for the interpolated plots

##  The lsoas file is map file for plotting; we need to match the right c.o.b info to each lsoa
sum(!lsoas$lsoa11cd%in%cob$LSOA11CD) #check for missing lsoas
unique.lsoa<-match(lsoas$lsoa11cd,cob$LSOA11CD) #we cannot have more than 1 match from lsoa to cov so we just take the first matching lsoa

##  subset to the c.o.b info we need (cob.sub). output: merged mapfile with c.o.b 'cob_geo_lsoa'
cob_geo_lsoa <- merge(lsoas[,c('lsoa11cd')], cob[unique.lsoa,c('LSOA11CD',cob.sub)], by.x = 'lsoa11cd', by.y = 'LSOA11CD')

##  Now to get the stl; the interpolate option has been included as part of the function itself 
r2stl_geo(
  cob_geo_lsoa,
  'nonUKZoneProp.lsoa',
  gridResolution=50,
  keepXYratio = T,
  zRatio = 0.25,
  show.persp = F,
  filename= 'stl/rothtest no inter.stl',
  reliefLayer = roads,
  interpolate = 0
)

##  Graphing data: Test using relief rasters for roads and north arrow----

##  Basically a relief layer is the layer upon which we indent onto the rest of the data 
#Create own relief raster for adding extra features like north arrow to
#Start with the roads shapefile
r <- rasterToFitShapefileExtent(cob_geo_lsoa,50)

##  Rasterise the roads, the relief variable in roads, and r (the rasterised geo file)
reliefRaster <- rasterize(roads,r,roads$relief)
reliefRaster[is.na(reliefRaster)] <- 0#

##  Next add in the motorways----
#Motorway
mway <- readOGR('data/boundarydata','rotherham_MotorwayBuffer60m')
#Currently a single polygon with no attributes. Add one with the value we want to use
mway$relief <- -2
mwayReliefRaster <- rasterize(mway,r,mway$relief)
mwayReliefRaster[is.na(mwayReliefRaster)] <- 0#

#combine the 2 raster files
reliefRaster <- min(reliefRaster,mwayReliefRaster)
plot(reliefRaster)

#North arrow rasterised
northArrow <- raster('images/northArrow1.tif')
values(northArrow) <- ifelse(values(northArrow) == 0,1,0)

sm <- aggregate(northArrow, fact=10)
plot(sm)
dim(reliefRaster)
dim(sm)
proj4string(sm) <- proj4string(reliefRaster)
extent(sm)
plot(sm)

#Chosen coordinates for corner of image
newx <- 439903.630
newy <- 379484.665
#Multiplication of image size
xmax <- extent(sm)[2] * 6
ymax <- extent(sm)[4] * 6

extent(sm) <- c(xmin <- newx, xmax <- newx + xmax, ymin <- newy, ymax <- newy + ymax)
extent(sm)

sm2 <- projectRaster(sm,reliefRaster)
plot(sm2)
#writeRaster(sm2,'local/qgis/northarrowCheck.tif', overwrite=T)

#So in theory...
sm2 <- 1-sm2
values(sm2) <- ifelse(values(sm2) < 0.75,0,-2)

plot(reliefRaster)
plot(sm2,add=T)

reliefRaster2 <- min(reliefRaster,sm2, na.rm = T)
plot(reliefRaster2)
values(reliefRaster2)<-values(reliefRaster2)+10
#reliefRaster2 <- reliefRaster - (sm2 * 2)
#reliefRaster2 <- overlay(reliefRaster,sm2,fun=function(x,y){return(x-(y*2))})

#Good lord. Now let's see if it actually works in the thing
r2stl_geo(
  cob_geo_lsoa,
  'nonUKZoneProp.lsoa',
  gridResolution=50,
  keepXYratio = T,
  zRatio = 0.25,
  show.persp = F,
  filename= 'stl/roth_arrowtest hi expo.stl',
  reliefLayer = reliefRaster2,
  interpolate = 20
)

### >Try non-interpolated version----

r2stl_geo(
  cob_geo_lsoa,
  'nonUKZoneProp.lsoa',
  gridResolution=50,
  keepXYratio = T,
  zRatio = 0.25,
  show.persp = F,
  filename= 'stl/roth_test arrow.stl',
  reliefLayer = reliefRaster2
)

plot(reliefRaster)
values(reliefRaster)