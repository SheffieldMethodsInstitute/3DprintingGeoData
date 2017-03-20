#Tweaked r2stl code
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
# oas <- oas[!(oas$code %in% dropz$code),]
# plot(oas)
# 
# #save that for later
# writeOGR(oas,'data/boundarydata','sheffield_oa_2011_MinuswestEnd', driver="ESRI Shapefile")
oas <- readOGR('data/boundarydata','sheffield_oa_2011_MinuswestEnd')

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







