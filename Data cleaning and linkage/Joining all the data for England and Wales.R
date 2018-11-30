##  Creating the unified field of data to be used for the analysis.
##  So this entails getting a file of Lsoa01 codes and then working with it
##  The linking variables is always LSOA01CD

##  Pre: Load in all the RCI functions we need
source.file <- 'RCI functions.R' #path to source
source(source.file)

##  First: we load in the variables datasets / lookup files-----
base.df <- google.drive.spatial %>% 
  paste('/geoportal lkps for UK/Lower_Layer_Super_Output_Areas_December_2001_Names_and_Codes_in_England_and_Wales.csv', sep = '') %>%
  read.csv #%>%
base.df <- base.df %>%  rename(LSOA01CD = ï..LSOA04CD)

##  lkps
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
  summarise(adult.pop11 = sum(adult.pop11 * weight)) %>%
  rename(LSOA01CD = lsoa01)


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

##  IMD variables -----

##  Geo scores (note: scores not ranks) -----
geo04 <- google.drive.spatial %>% 
  paste('/English IMDs/2004/eimd04 geo.csv', sep = '') %>% read.csv

geo04 <- geo04 %>%
  mutate(LSOA01CD = SOA.CODE,
         geo04 = Geographical.Barriers.Sub.Domain.Score) %>%
  dplyr::select(LSOA01CD, geo04)

###
geo_inc10 <- google.drive.spatial %>% 
  paste('/English IMDs/2010/EIMD 2010.csv', sep = '') %>% read.csv

geo_inc10 <- geo_inc10 %>%
  mutate(LSOA01CD = LSOA.CODE,
         geo10 = Geographical.Barriers.Sub.domain.Score,
         inc_prop10 = INCOME.SCORE) %>%
  dplyr::select(LSOA01CD, geo10, inc_prop10)

##  Now for the income and pop variables (pop var is already in one file for 10)
###
inc04 <- google.drive.spatial %>% 
  paste('/English IMDs/2004/SOA levelid2004 income.csv', sep = '') %>% 
  read.csv
inc04 <- inc04 %>% 
  mutate(LSOA01CD = SOA,
         inc_prop04 = INCOME.SCORE) %>%
  dplyr::select(LSOA01CD, inc_prop04)

###
pop04 <- google.drive.spatial %>% 
  paste('/English IMDs/2004/soalevel2001 pop.csv', sep = '') %>% 
  read.csv
pop04 <- pop04 %>% 
  mutate(LSOA01CD = Lower.SOA.code,
         pop04 = Total.population) %>%
  dplyr::select(LSOA01CD, pop04)

##  We keep to imd 10 due to consistent codes
pop10 <- google.drive.spatial %>% 
  paste('/English IMDs/2010/EIMD 2010 pop.csv', sep = '') %>% 
  read.csv
pop10 <- pop10 %>% 
  mutate(LSOA01CD = LSOA.CODE,
         pop10 = Total.population..mid.2008..excluding.prisoners.) %>%
  dplyr::select(LSOA01CD, pop10)

##  Right time to merge into one imd file
imd.lkp <- geo04 %>% 
  merge(geo_inc10) %>%
  merge(inc04) %>%
  merge(pop04) %>%
  merge(pop10)
  
imd.lkp <- imd.lkp %>%
  mutate(inc_n04 = inc_prop04 * pop04,
         inc_n10 = inc_prop10 * pop10)


# Distance to centres -----------------------------------------------------
nearest30k.lkp <- read.csv('Working analysis files/Distance from nearest centre for LSOA01 (30k).csv')
nearest30k.lkp <- nearest30k.lkp %>% rename(LSOA01CD = lsoa01cd)


# Access to work ----------------------------------------------------------
emp.access <-
  read.csv('Working analysis files/Access to employment computed lkp.csv')
emp.access <- emp.access %>% 
  rename(LSOA01CD = lsoa01)

##  3) Merging all the data in section and then to save it all -------------
##  we want left joins to preserve the data

master.df <- base.df %>% 
  left_join(emp.access) %>%
  left_join(nearest30k.lkp) %>%
  left_join(imd.lkp) %>%
  left_join(pop.2001) %>%
  left_join(pop.2011) %>%
  left_join(jsa2001) %>%
  left_join(jsa2011)
  
master.df %>% summary ## minimal issues # missing pop 04 etc due to no welsh data


# Final step: Saving a master data table for analysis -------------------
master.df %>% 
  write.csv('Working analysis files/Master data tables of variables for LSOA01.csv',
            row.names = F)

