#Tweaked r2stl code
source('r2stl/R/r2stl_geo.r')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Rotherham 1: non-UK pop 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##  Saving projection system and reprojecting the r.ham shp file
osgb36<-("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +datum=OSGB36 +units=m +no_defs +ellps=airy +towgs84=446.448,-125.157,542.060,0.1502,0.2470,0.8421,-20.4894")

oas<-readOGR(dsn='data/boundarydata',layer='rotherham_oa_2011_noproj',p4s=osgb36)
lsoas<-readOGR(dsn='data/boundarydata',layer='rotherham_lsoa_2011_noproj',p4s=osgb36)

#Get eth data
cob <- read_csv('data/cob lsoa msoa.csv')
#tidy to make easier to read

#Subset to Rotherham
roth.id<-grep('Rotherham',cob$LSOA11NM)
cob<-cob[roth.id,]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  Graphing data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# oa-----------
#merge with geography - keep only the column we want for now.
names(cob)
cob_geo <- merge(oas[,c('oa11cd')], cob[,c('OA11CD','nonUKZoneProp.oa','nonUKZoneProp.lsoa','nonUKZoneProp.msoa')], by.x = 'oa11cd', by.y = 'OA11CD')
cob_geo

### Right so now to sort of use the r2stl stuff
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

# lsoa-----------
#merge with geography - keep only the column we want for now.
names(cob)
##  We want an lsoa map for the interpolated plots
sum(!lsoas$lsoa11cd%in%cob$LSOA11CD)
unique.lsoa<-match(lsoas$lsoa11cd,cob$LSOA11CD) #we cannot have more than 1 match from lsoa to cov so we just take the first matching lsoa

cob_geo_lsoa <- merge(lsoas[,c('lsoa11cd')], cob[unique.lsoa,c('LSOA11CD','nonUKZoneProp.oa','nonUKZoneProp.lsoa','nonUKZoneProp.msoa')], by.x = 'lsoa11cd', by.y = 'LSOA11CD')

##  Get the centroids
lsoaCentroids <- gCentroid(cob_geo_lsoa,byid=T)
plot(lsoaCentroids)

#Using cob_geo calculated above
df <- data.frame(cob_geo)

#So let's see if I can just calculate...
#Actually, let's just stick to code we know works
#http://gis.stackexchange.com/questions/158021/plotting-map-resulted-from-kriging-in-r

#Make grid as per link above. Start fairly crude.
#Actually, use extent of original LSOA shapefile

 min_x = min(coordinates(lsoas)[,1]) #minimun x coordinate
 min_y = min(coordinates(lsoas)[,2]) #minimun y coordinate
 
 x_length = max(coordinates(lsoas)[,1] - min_x) #easting amplitude
 y_length = max(coordinates(lsoas)[,2] - min_y) #northing amplitude
 min_x = extent(lsoas)[1]
 min_y = extent(lsoas)[3]
 
 x_length = extent(lsoas)[2] - extent(lsoas)[1]
 y_length = extent(lsoas)[4] - extent(lsoas)[3]
 
cellsize = 50 #pixel size
ncol = round(x_length/cellsize,0) #number of columns in grid
nrow = round(y_length/cellsize,0) #number of rows in grid
 
 grid = GridTopology(cellcentre.offset=c(min_x,min_y),cellsize=c(cellsize,cellsize),cells.dim=c(ncol,nrow))
# 
# #Convert GridTopolgy object to SpatialPixelsDataFrame object.
 grid = SpatialPixelsDataFrame(grid,
                               data=data.frame(id=1:prod(ncol,nrow)),
                               proj4string=CRS(proj4string(lsoas)))
 
# #Same length? Tick. So one spatial point for each data point.
# #Though I may need to double-check they're actually in the correct order.
# lsoaCentroids %>% length == cob_geo %>% nrow
# 
 interp <- idw(cob_geo_lsoa@data$nonUKZoneProp.lsoa~1, lsoaCentroids, grid, idp = 6)
 spplot(interp)
# 
 r = raster(interp)
 plot(r)
# writeRaster(r,'local/qgis/shefRasterCheck2.tif', overwrite=T)

 
#~~ Still WIP
 r2stl_geo(
   cob_geo_lsoa,
   'nonUKZoneProp.lsoa',
   gridResolution=50,
   keepXYratio = T,
   zRatio = 0.25,
   show.persp = F,
   filename= 'stl/testroth.stl',
#   reliefLayer = roads,
   interpolate = 6
 )

 
 
 