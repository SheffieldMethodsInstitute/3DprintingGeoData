#Tweaked r2stl code
source('r2stl/R/r2stl_geo.r')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Rotherham 1: non-UK pop 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##  Saving projection system and reprojecting the r.ham shp file
osgb36<-("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +datum=OSGB36 +units=m +no_defs +ellps=airy +towgs84=446.448,-125.157,542.060,0.1502,0.2470,0.8421,-20.4894")

oas<-readOGR(dsn='data/boundarydata',layer='rotherham_oa_2011_noproj',p4s=osgb36)

#Get eth data
cob <- read_csv('data/cob lsoa msoa.csv')
#tidy to make easier to read

#Subset to Rotherham
roth.id<-grep('Rotherham',cob$LSOA11NM)
cob<-cob[roth.id,]

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

