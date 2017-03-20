##  Script for country of birth

##  Load the OA2LSOA tables and cob; get subsets for 
cob <- read_csv('data/countryOfBirth_Yorkshire.csv')
oa2lsoa<-read.csv('data/OA to LSOA 2011.csv')
sub.id<-c(grep('Sheffield',oa2lsoa$LSOA11NM),grep('Rotherham',oa2lsoa$LSOA11NM))
max(table(sub.id))
oa<-oa2lsoa[sub.id,]

#Subset to Sheffield and Rotherham
cob <- cob[cob$geography %in% oa$OA11CD,]

#tidy to make easier to read
names(cob) <- gsub('; measures: Value','', names(cob))

#non-white is just 'all categories' minus UK (include all UK)
cob$nonUK <- cob$`Country of Birth: All categories: Country of birth` - 
  (cob$`Country of Birth: Europe: United Kingdom: Total` +
     cob$`Country of Birth: Europe: Great Britain not otherwise specified` +
     cob$`Country of Birth: Europe: United Kingdom not otherwise specified`)

#as % of zone pop
cob$nonUKZoneProp.oa <- (cob$nonUK/cob$`Country of Birth: All categories: Country of birth`)*100

##  merge the data
cob.lsoa<-merge(x=oa,y=cob[,c('geography','nonUK','nonUKZoneProp.oa','Country of Birth: All categories: Country of birth')],by.x='OA11CD',by.y='geography')

## Use agrgegate to create lsoa stats
names(cob.lsoa)
lsoa.stats<-aggregate(cob.lsoa[,c('nonUK','Country of Birth: All categories: Country of birth')],
          list(cob.lsoa$LSOA11CD),sum)
names(lsoa.stats)<-paste(names(lsoa.stats),'lsoa',sep='.')
names(lsoa.stats)

## Use agrgegate to create msoa stats
msoa.stats<-aggregate(cob.lsoa[,c('nonUK','Country of Birth: All categories: Country of birth')],
                      list(cob.lsoa$MSOA11CD),sum)
names(msoa.stats)<-paste(names(msoa.stats),'msoa',sep='.')
names(msoa.stats)


##  Append the lsoa and msoa stats to cob file 
cob.lsoa<-merge(x=cob.lsoa,y=lsoa.stats,by.x='LSOA11CD',by.y='Group.1.lsoa')
cob.lsoa<-merge(x=cob.lsoa,y=msoa.stats,by.x='MSOA11CD',by.y='Group.1.msoa')

cob.lsoa$nonUKZoneProp.lsoa<-(cob.lsoa$nonUK.lsoa/cob.lsoa$`Country of Birth: All categories: Country of birth.lsoa`)*100
cob.lsoa$nonUKZoneProp.msoa<-(cob.lsoa$nonUK.msoa/cob.lsoa$`Country of Birth: All categories: Country of birth.msoa`)*100


##  Save it 
write.csv(cob.lsoa,'data/cob lsoa msoa.csv')
