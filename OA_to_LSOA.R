#OA to LSOA summing
library(dplyr)

#~~~~~~~~~~~~~~
#Ethnicity----
#~~~~~~~~~~~~~~

lookup <- read_csv('data/OA to LSOA 2011.csv')

#reduce lookup to Y&H
#table(lookup$OA11CD %in% oas$code)
#lookup <- lookup[lookup$OA11CD %in% oas$code,]

#Merge in lookup.
eth <- read_csv('data/ethnicity_Y&H.csv')

eth_LSOA <- merge(eth,lookup[,c(1,2)], by.x = 'geography', by.y = 'OA11CD')

eth_LSOA_summary <- eth_LSOA[,c(5:29)] %>% group_by(LSOA11CD) %>% 
  summarise_each(funs(sum = sum(.)))

#sanity check
table(apply(eth_LSOA[,c(5:28)],2,sum) == apply(eth_LSOA_summary[,c(2:25)],2,sum))

#save
write_csv(eth_LSOA_summary,'data/ethnicity_Y&H_LSOA.csv')

