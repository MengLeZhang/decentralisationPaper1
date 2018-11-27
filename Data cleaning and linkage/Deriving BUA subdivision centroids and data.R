##  Deriving statistics and population weighted centroids for BUASD
##  Work using pop 2011

##  Pre: Load in all the RCI functions we need
source.file <- 'RCI functions.R' #path to source
source(source.file)

##  1) Load in the data for BUA subdivision lookups
lsoa11tobua11.lkp <- google.drive.spatial %>% 
  paste('/geoportal lkps for UK/LSOA2011 to BUA and BUASD 2011.csv', sep ='') %>%
  read.csv

##  2) Load in lsoa11 data with centroids and merge with pop data
lsoa11.sf <- google.drive.spatial %>% 
  paste('/LSOA 2011', sep ='') %>%
  st_read('Lower_Layer_Super_Output_Areas_December_2011_Population_Weighted_Centroids') %>%
  st_transform(crs = st_crs(ukgrid))

lsoa11.cent <- data.frame(lsoa11cd = lsoa11.sf$lsoa11cd,
                          lsoa11.sf %>% st_coordinates)

pop.2011 <- google.drive.spatial %>% 
  paste('/LSOA 2011/LSOA pop census 2011.csv', sep ='') %>%
  read.csv

pop.2011 <- pop.2011 %>% 
  mutate(total = Age..All.usual.residents..measures..Value,
         lsoa11cd = geography.code)

pop.2011 <- pop.2011 %>% merge(lsoa11.cent)


##  3) Now to just easily calulcate the pop weighted centroids
lsoa11tobua11.lkp %>% head
pop.weighted %>% head
pop.weighted <- lsoa11tobua11.lkp %>% merge(pop.2011, by.x = 'LSOA11CD', by.y = 'lsoa11cd')
pop.weighted <- pop.weighted %>%
  group_by(BUASD11NM) %>%
  summarise(pweight_x = X %>% weighted.mean(total),
            pweight_y = Y %>% weighted.mean(total),
            pop11 = sum(total))

##  Big error in the data!
(lsoa11tobua11.lkp$BUASD11NM == '') %>% table # some have no buasd11nm
lsoa11tobua11.lkp %>% filter(BUASD11NM == '') #ggr some have subdivisions but others do not!
lsoa11tobua11.lkp %>% filter(BUASD11NM == '' & BUA11NM == '')
### STOPPED HERE
pop.weighted %>%
  write.csv('Working analysis files/buasd11 pop weighted centroids and other data.csv', row.names = F)
