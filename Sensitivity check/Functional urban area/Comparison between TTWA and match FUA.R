##  Comparing FUA and TTWA11
##  Completely p[topm


inequal.tab_ttwa <-
  read.csv('Working analysis files/Duncan index results table.csv') %>%
  filter(type == 'inc') %>%
  mutate(zone = 'ttwa',
         name = TTWA11NM) 

inequal.tab_fua <-
  read.csv('Working analysis files/Duncan index results table for FUA (optional).csv') %>%
  filter(type == 'inc') %>%
  mutate(zone = 'fua',
         name = fua) 

##  Let's see how things look like for FUA if we restrict to just those TTWA within
##  the FUA
##  Let's get the overlaps 
ukgrid = "+init=epsg:27700" ## always remember the CRS


ttwa11.sf <- st_read(dsn = google.drive.spatial %>% 
                       paste('/TTWA 2011', sep = ''),
                     layer = 'Travel_to_Work_Areas_December_2011_Full_Extent_Boundaries_in_United_Kingdom') %>%
  st_transform(crs = st_crs(ukgrid))

fua_sf <- 
  google.drive.spatial %>%
  file.path('Functional urban area') %>%
  st_read('Urban_Audit_Functional_Urban_Areas_December_2016_Full_Clipped_Boundaries_in_the_United_Kingdom') %>%
  st_transform(crs = st_crs(ukgrid)) %>%
  st_buffer(-100)

intersect.sf <-
  ttwa11.sf %>%
  st_join(fua_sf)

intersect.sf <-
  intersect.sf %>%
  filter( !(is.na(uafua16nm)) )

intersect.nms <- intersect.sf$ttwa11nm %>% unique
## 129 left overall -- so basically we are seeing lots of smaller groups dropping out

inequal.tab_ttwa %>% summary
inequal.tab_ttwa %>% filter(TTWA11NM %in% intersect.nms) %>% summary

##  Still pretty similar results -- worse