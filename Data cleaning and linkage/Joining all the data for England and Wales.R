##  Creating the unified field of data to be used for the analysis.
##  So this entails getting a file of LSOA01 and then working with it

##  Pre: Load in all the RCI functions we need
source.file <- 'RCI functions.R' #path to source
source(source.file)

##  First: we load in the variables datasets / lookup files-----
base.df <- google.drive.spatial %>% 
  paste('/geoportal lkps for UK/Lower_Layer_Super_Output_Areas_December_2001_Names_and_Codes_in_Northern_Ireland.csv', sep = '') %>%
  read.csv %>%
  rename(LSOA01CD = ï..LSOAN05CD)

oa01tolsoa01.lkp <-
  read.csv('Working analysis files/oa01 to lsoa01 lkp.csv')

oa11tolsoa01.lkp <-
  read.csv('Working analysis files/oa11 to lsoa01 lkp.csv')

lsoa11tolsoa01.lkp <-
  read.csv('Working analysis files/lsoa11 to lsoa01 lkp.csv')

lsoa01tottwa11.lkp <-
  read.csv('Working analysis files/lsoa01 to ttwa11 lkp.csv')

##  Second: We need to get the data from various sources into lsoa01cd -----
##  Population ----
##  2001 data
pop.2001 <- google.drive.spatial %>% 
  paste('/LSOA 2001/LSOA pop census 2001.csv', sep ='') %>%
  read.csv
pop.2001 %>% names
pop.2001 <- pop.2001 %>% 
  mutate(adult.pop01 = Age..All.usual.residents..measures..Value - sum(c(5:10,17:20)),
         LSOA01CD = geography.code) %>% # minus sum of some rows
  dplyr::select(LSOA01CD, adult.pop01)

## 2011 data
pop.2011 <- google.drive.spatial %>% 
  paste('/LSOA 2011/LSOA pop census 2011.csv', sep ='') %>%
  read.csv

pop.2011 <- pop.2011 %>% 
  mutate(adult.pop11 = Age..All.usual.residents..measures..Value - sum(c(6:11,18:21)),
         lsoa11 = geography.code) %>% # minus sum of some rows
  dplyr::select(lsoa11, adult.pop11)

pop.2011 <- pop.2011 %>% merge(lsoa11tolsoa01.lkp)
pop.2011 <- pop.2011 %>%
  group_by(lsoa01) %>%
  summarise(adult.pop11 = sum(adult.pop11 * weight))


##  JSA----
##  2001
jsa2001 <- google.drive.spatial %>% 
          paste('/DWP data/jsa200105.csv', sep = '') %>%
  read.csv

jsa2001 <- jsa2001 %>% 
  mutate(LSOA01CD = Lower.Layer.SOA...Data.Zone.Code,
         jsa01 = Total) %>%
  dplyr::select(LSOA01CD, jsa01)

##  2011
jsa2011 <- google.drive.spatial %>% 
  paste('/DWP data/jsa201105.csv', sep = '') %>%
  read.csv

jsa2011 <- jsa2011 %>% 
  mutate(LSOA01CD = Lower.Layer.SOA...Data.Zone.Code,
         jsa11 = Total) %>%
  dplyr::select(LSOA01CD, jsa11)

##  Workplace pop ----
## 2001 - this we have to use oa data and convert to lsoa
oa.wplace01 <- google.drive.spatial %>%
  paste('/OA 2001/workplace oa 2001.csv', sep = '') %>%
  read.csv

oa.wplace01 <- oa.wplace01 %>%
  mutate(oa01 = substr(Area, 8 , 17),
         wplace01 = X2001 %>% as.numeric) %>%
  dplyr::select(oa01, wplace01)

wplace2001 <- oa.wplace01 %>% merge(oa01tolsoa01.lkp)
wplace2001 <- wplace2001 %>%
  group_by(lsoa01) %>%
  summarise(wplace01 = sum(wplace01))

## 2011
wplace2011 <- google.drive.spatial %>%
  paste('/LSOA 2011/workplace pop census 2011.csv', sep = '') %>%
  read.csv
wplace2011 %>% head
wplace2011 <- wplace2011 %>%
  mutate(lsoa11 = X2011.super.output.area...lower.layer %>% substr(1, 9),
         wplace11 = X2011 %>% as.numeric)

wplace2011 <- wplace2011 %>% merge(lsoa11tolsoa01.lkp)
wplace2011 <- wplace2011 %>%
  group_by(lsoa01) %>%
  summarise(wplace11 = sum(wplace11 * weight))


##  Adding coordinates for the population weighted centroids. ----
lsoa01cent <- st_read(dsn = google.drive.spatial %>% 
                        paste('/LSOA 2001', sep = ''), 
                      layer='Lower_Layer_Super_Output_Areas_December_2001_Population_Weighted_Centroids')

lsoa01cent <- data.frame(LSOA01CD = lsoa01cent$lsoa01cd,
                         lsoa01cent %>% st_coordinates)
lsoa01cent %>% str ## in easting northings

##  3) Merging all the data in section 2

##  >Now we save tge ew.2001 data set for the future
save(file='../Data/Analysis data/England and Wales 0111 temp.Rdata',ew.2001)
