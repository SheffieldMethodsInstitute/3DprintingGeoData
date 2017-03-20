##  Script for ethnicity

##  Load the OA2LSOA tables and eth; get subsets for 
eth <- read_csv('data/ethnicity_Y&H.csv')
oa2lsoa<-read.csv('data/OA to LSOA 2011.csv')
sub.id<-c(grep('Sheffield',oa2lsoa$LSOA11NM),grep('Rotherham',oa2lsoa$LSOA11NM))
max(table(sub.id)) #only 1 entry for each oa code
oa<-oa2lsoa[sub.id,]

#Subset to Sheffield and Rotherham
eth <- eth[eth$geography %in% oa$OA11CD,]

#tidy to make easier to read
names(eth) <- gsub('; measures: Value','', names(eth))

#non-white is just 'all categories' minus UK (include all UK)
eth$nonwhite <- eth$`Ethnic Group: All usual residents`-eth$`Ethnic Group: White`
eth$pstani<-eth$`Ethnic Group: Asian/Asian British: Pakistani` ##we just wanted a shorter name

##  merge the data
eth.lsoa<-merge(x=oa,y=eth[,c('geography','nonwhite','pstani','Ethnic Group: All usual residents')],by.x='OA11CD',by.y='geography')

## Use agrgegate to create lsoa stats
names(eth.lsoa)
lsoa.stats<-aggregate(eth.lsoa[,c('nonwhite','Ethnic Group: All usual residents','pstani')],
                      list(eth.lsoa$LSOA11CD),sum)
names(lsoa.stats)<-paste(names(lsoa.stats),'lsoa',sep='.')
names(lsoa.stats)

## Use agrgegate to create msoa stats
msoa.stats<-aggregate(eth.lsoa[,c('nonwhite','Ethnic Group: All usual residents','pstani')],
                      list(eth.lsoa$MSOA11CD),sum)
names(msoa.stats)<-paste(names(msoa.stats),'msoa',sep='.')
names(msoa.stats)


##  Append the lsoa and msoa stats to eth file 
eth.lsoa<-merge(x=eth.lsoa,y=lsoa.stats,by.x='LSOA11CD',by.y='Group.1.lsoa')
eth.lsoa<-merge(x=eth.lsoa,y=msoa.stats,by.x='MSOA11CD',by.y='Group.1.msoa')


#as % of zone pop
eth.lsoa$nonwhiteZoneProp.oa <- (eth.lsoa$nonwhite/eth.lsoa$`Ethnic Group: All usual residents`)*100
eth.lsoa$nonwhiteZoneProp.lsoa <- (eth.lsoa$nonwhite.lsoa/eth.lsoa$`Ethnic Group: All usual residents.lsoa`)*100
eth.lsoa$nonwhiteZoneProp.msoa <- (eth.lsoa$nonwhite.msoa/eth.lsoa$`Ethnic Group: All usual residents.msoa`)*100

eth.lsoa$pstaniZoneProp.oa <- (eth.lsoa$pstani/eth.lsoa$`Ethnic Group: All usual residents`)*100
eth.lsoa$pstaniZoneProp.lsoa <- (eth.lsoa$pstani.lsoa/eth.lsoa$`Ethnic Group: All usual residents.lsoa`)*100
eth.lsoa$pstaniZoneProp.msoa <- (eth.lsoa$pstani.msoa/eth.lsoa$`Ethnic Group: All usual residents.msoa`)*100

##  Save it 
write.csv(eth.lsoa,'data/eth lsoa msoa.csv')
