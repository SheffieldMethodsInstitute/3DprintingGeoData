##  Carbon copy of Dan's script transplanted to Rotherham

#Tweaked r2stl code
source('r2stl/R/r2stl_geo.r')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Rotherham 1: non-white pop, OAs----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##  Saving projection system and reprojecting the r.ham shp file
osgb36<-("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +datum=OSGB36 +units=m +no_defs +ellps=airy +towgs84=446.448,-125.157,542.060,0.1502,0.2470,0.8421,-20.4894")

rotherham<-readOGR(dsn='data/boundarydata',layer='rotherham_oa_2011_noproj',p4s=osgb36)


##  Saving projection system and reprojecting the r.ham shp file
osgb36<-CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +datum=OSGB36 +units=m +no_defs +ellps=airy +towgs84=446.448,-125.157,542.060,0.1502,0.2470,0.8421,-20.4894")

#Get cob data
eth <- read_csv('data/ethnicity_Y&H.csv')


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







