source('r2stl/R/r2stl_geo.r')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Sheffield 1: non-white pop, Sheffield OAs----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Get output areas, reduce to Sheffield
# oas <- readOGR('data/boundarydata','sheffield_oa_2011')
# 
# #Drop rural west end OAs
# dropz <- readOGR('data/boundarydata','sheffield_oa_2011_westEnd')
# 
# plot(dropz, col='red')
# plot(oas, col = 'red')
# 
# oas <- oas[!(oas$code %in% dropz$code),]
# plot(oas, col = 'red')
# 
# #save that for later
# writeOGR(oas,'data/boundarydata','sheffield_oa_2011_MinuswestEnd', driver="ESRI Shapefile", overwrite_layer=T)

oas <- readOGR('data/boundarydata','sheffield_oa_2011_MinuswestEnd')
plot(oas, col = 'red')

#Get cob data
cob <- read_csv('data/countryOfBirth_Yorkshire.csv')

#check OA match. Tick.
table(oas$code %in% cob$geography)

#Subset to Sheffield
cob <- cob[cob$geography %in% oas$code,]

#tidy to make easier to read
names(cob) <- gsub('; measures: Value','', names(cob))

#non-white is just 'all categories' minus UK (include all UK)
cob$nonUK <- cob$`Country of Birth: All categories: Country of birth` - 
  (cob$`Country of Birth: Europe: United Kingdom: Total` +
     cob$`Country of Birth: Europe: Great Britain not otherwise specified` +
     cob$`Country of Birth: Europe: United Kingdom not otherwise specified`)

#as % of zone pop
cob$nonUKZoneProp <- (cob$nonUK/cob$`Country of Birth: All categories: Country of birth`)*100
hist(cob$nonUKZoneProp)

#merge with geography - keep only the column we want for now.
cob_geo <- merge(oas[,c('code')], cob[,c('geography','nonUKZoneProp')], by.x = 'code', by.y = 'geography')

r2stl_geo(
  cob_geo,
  'nonUKZoneProp',
  gridResolution=50,
  keepXYratio = T,
  zRatio = 0.25,
  show.persp = F,
  filename= 'stl/nonUKbornSheffield.stl'
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#COB LSOA level----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

cob <- read_csv('data/countryOfBirth_Y&H_LSOA.csv')

lsoas <- readOGR('data/boundarydata','sheffield_lsoa_2011_MinuswestEnd')
plot(lsoas, col='red')

roads <- readOGR('data/boundarydata','sheffield_mainRoadsBuffer25m')
plot(roads, add=T)

#Currently a single polygon with no attributes. Add one with the value we want to use
roads$relief <- -1

#check OA match. Tick.
table(lsoas$code %in% cob$LSOA11CD)

#Subset to Sheffield
cob <- cob[cob$LSOA11CD %in% lsoas$code,]

#tidy to make easier to read
names(cob) <- gsub('; measures: Value','', names(cob))

#non-white is just 'all categories' minus UK (include all UK)
cob$nonUK <- cob$`Country of Birth: All categories: Country of birth` - 
  (cob$`Country of Birth: Europe: United Kingdom: Total` +
     cob$`Country of Birth: Europe: Great Britain not otherwise specified` +
     cob$`Country of Birth: Europe: United Kingdom not otherwise specified`)

#as % of zone pop
cob$nonUKZoneProp <- (cob$nonUK/cob$`Country of Birth: All categories: Country of birth`)*100
hist(cob$nonUKZoneProp)

#merge with geography - keep only the column we want for now.
cob_geo <- merge(lsoas[,c('code')], cob[,c('LSOA11CD','nonUKZoneProp')], by.x = 'code', by.y = 'LSOA11CD')

r2stl_geo(
  cob_geo,
  'nonUKZoneProp',
  gridResolution=10,
  keepXYratio = T,
  zRatio = 0.25,
  show.persp = F,
  filename= 'stl/shefCoBNonUKLSOA_10m_roadneg25m.stl',
  reliefLayer = roads
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Interpolation: LSOA----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Will need to pass raster data in directly to the r2stl function. But let's create it first.
#So the data itself will be point data based on LSOA centroids.
lsoas <- readOGR('data/boundarydata','sheffield_lsoa_2011_MinuswestEnd')
plot(lsoas, col='red')

#Which assumes they're all dissolved to single IDs
lsoaCentroids <- gCentroid(lsoas,byid=T)
plot(lsoaCentroids, add=T)

#Using cob_geo calculated above
df <- data.frame(cob_geo)

#So let's see if I can just calculate...
#Actually, let's just stick to code we know works
#http://gis.stackexchange.com/questions/158021/plotting-map-resulted-from-kriging-in-r

#Make grid as per link above. Start fairly crude.
#Actually, use extent of original LSOA shapefile

# min_x = min(coordinates(lsoas)[,1]) #minimun x coordinate
# min_y = min(coordinates(lsoas)[,2]) #minimun y coordinate
# 
# x_length = max(coordinates(lsoas)[,1] - min_x) #easting amplitude
# y_length = max(coordinates(lsoas)[,2] - min_y) #northing amplitude
# min_x = extent(lsoas)[1]
# min_y = extent(lsoas)[3]
# 
# x_length = extent(lsoas)[2] - extent(lsoas)[1]
# y_length = extent(lsoas)[4] - extent(lsoas)[3]
# 
# cellsize = 50 #pixel size
# ncol = round(x_length/cellsize,0) #number of columns in grid
# nrow = round(y_length/cellsize,0) #number of rows in grid
# 
# grid = GridTopology(cellcentre.offset=c(min_x,min_y),cellsize=c(cellsize,cellsize),cells.dim=c(ncol,nrow))
# 
# #Convert GridTopolgy object to SpatialPixelsDataFrame object.
# grid = SpatialPixelsDataFrame(grid,
#                               data=data.frame(id=1:prod(ncol,nrow)),
#                               proj4string=CRS(proj4string(lsoas)))
# 
# #Same length? Tick. So one spatial point for each data point.
# #Though I may need to double-check they're actually in the correct order.
# lsoaCentroids %>% length == cob_geo %>% nrow
# 
# interp <- idw(cob_geo@data$nonUKZoneProp~1, lsoaCentroids, grid, idp = 6)
# spplot(interp)
# 
# r = raster(interp)
# plot(r)
# writeRaster(r,'local/qgis/shefRasterCheck2.tif', overwrite=T)

r2stl_geo(
  cob_geo,
  'nonUKZoneProp',
  gridResolution=50,
  keepXYratio = T,
  zRatio = 0.25,
  show.persp = F,
  filename= 'stl/test.stl',
  reliefLayer = roads,
  interpolate = 6
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Interpolation: LSOA, plus add combined raster as relief layer----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Create own relief raster for adding extra features like north arrow to
#Start with the roads shapefile
r <- rasterToFitShapefileExtent(cob_geo,50)

#Roads: buffered in QGIS to provide some width in the raster.
#Motorways wider than smaller roads...

#Primary and A-roads
roads <- readOGR('data/boundarydata','sheffield_mainRoadsBuffer25m')

#Currently a single polygon with no attributes. Add one with the value we want to use
roads$relief <- -1

reliefRaster <- rasterize(roads,r,roads$relief)
reliefRaster[is.na(reliefRaster)] <- 0#

#Motorway
mway <- readOGR('data/boundarydata','sheffield_MotorwayBuffer60m')

#Currently a single polygon with no attributes. Add one with the value we want to use
mway$relief <- -2

mwayReliefRaster <- rasterize(mway,r,mway$relief)
mwayReliefRaster[is.na(mwayReliefRaster)] <- 0#

#combine
reliefRaster <- min(reliefRaster,mwayReliefRaster)
plot(reliefRaster)

#North arrow
northArrow <- raster('images/northArrow1.tif')
values(northArrow) <- ifelse(values(northArrow) < 175,0,1)

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

#reliefRaster2 <- reliefRaster - (sm2 * 2)
#reliefRaster2 <- overlay(reliefRaster,sm2,fun=function(x,y){return(x-(y*2))})

#Good lord. Now let's see if it actually works in the thing
r2stl_geo(
  cob_geo,
  'nonUKZoneProp',
  gridResolution=50,
  keepXYratio = T,
  zRatio = 0.25,
  show.persp = F,
  filename= 'stl/arrowtest.stl',
  reliefLayer = reliefRaster2,
  interpolate = 6
)