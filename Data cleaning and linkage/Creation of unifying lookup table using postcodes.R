########################################################
##  Census output area to lsoa, la and ttwa 2001      ##
##  For 2001 we cannot find which census oa maps to   ##
##  which lsoa. We can create this table ourselves    ##
##  Start: 7/3/2017                                   ##
########################################################

##  Pre: Load in all the RCI functions we need
source.file <- 'RCI functions.R' #path to source
source(source.file)

##  First: we load in the map and variables datasets-----
pc.lkp <- google.drive.spatial %>%
  paste('/geoportal lkps for UK/ONS_Postcode_Directory_Latest_Centroids.csv' , sep = '') %>% 
  fread(header = T, select = c('pcds', 'lsoa01', 'oa01', 
                               'oa11', 'lsoa11',
                               'ttwa'))

##  Need to load in ttwa names
ttwa11.nms <- google.drive.spatial %>%
  paste('/geoportal lkps for UK/Travel_to_Work_Areas_December_2011_Names_and_Codes_in_the_United_Kingdom.csv', sep = '') %>%
  read.csv
ttwa11.nms <- ttwa11.nms %>% rename(ttwa = ï..TTWA11CD)
ttwa11.nms$ttwa %>% substr(1, 1) %>% table # check how many in wales

##  merge the two datasets
pc.lkp <- pc.lkp %>% merge(ttwa11.nms)



##  Second: Using the group_by and summarise feature we can create lookups ----
oa01tolsoa01.lkp <- pc.lkp %>%
  group_by(lsoa01, oa01) %>%
  summarise(count = length(oa01)) %>% 
  group_by(oa01) %>% ## group by oa to create the weights
  mutate(weight = (count / sum(count)) %>% round(2)) 
##  Remember weights tell us how much of the oa belongs in a lsoa so it is 
##  count / total counts of oa
##  Confusion will screw us over
oa01tolsoa01.lkp$weight %>% table #1 = perfect

oa01tolsoa01.lkp %>% 
  dplyr::select(-count) %>% 
  write.csv('Working analysis files/oa01 to lsoa01 lkp.csv', row.names = F)

##  oa11 to lsoa11 lkp
oa11tolsoa01.lkp <- pc.lkp %>%
  group_by(lsoa01, oa11) %>%
  summarise(count = length(oa11)) %>% 
  group_by(oa11) %>%
  mutate(weight = (count / sum(count)) %>% round(2))

oa11tolsoa01.lkp %>% 
  dplyr::select(-count) %>% 
  write.csv('Working analysis files/oa11 to lsoa01 lkp.csv', row.names = F)

##  lsoa11 to lsoa01
lsoa11tolsoa01.lkp <- pc.lkp %>%
  group_by(lsoa01, lsoa11) %>%
  summarise(count = length(lsoa11)) %>% 
  group_by(lsoa11) %>%
  mutate(weight = (count / sum(count)) %>% round(2))

lsoa11tolsoa01.lkp %>% 
  dplyr::select(-count) %>% 
  write.csv('Working analysis files/lsoa11 to lsoa01 lkp.csv', row.names = F)

##  lsoa01 to ttwa11
lsoa01tottwa11.lkp <- pc.lkp %>%
  group_by(ttwa, lsoa01) %>%
  summarise(count = length(lsoa11)) %>% 
  group_by(lsoa01) %>%
  mutate(weight = (count / sum(count)) %>% round(2))
lsoa01tottwa11.lkp <- lsoa01tottwa11.lkp %>% left_join(ttwa11.nms)


lsoa01tottwa11.lkp$weight %>% table ## its almost perfectly all in to be fair


lsoa01tottwa11.lkp %>% 
  dplyr::select(TTWA11NM, lsoa01, weight) %>% 
  write.csv('Working analysis files/lsoa01 to ttwa11 lkp.csv', row.names = F)

##End
rm(list = ls()) #finish by removing all
