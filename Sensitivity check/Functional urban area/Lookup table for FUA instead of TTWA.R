##  Optional file for Functional Urban areas
##  We need a number of file in order to create a supplementary analysis using
##  Functional Urban Area instead of TTWA


source.file <- 'RCI functions.R' #path to source
source(source.file)

##  First: we load in the variables datasets / lookup files-----
##  RULE: If LSOA centroid is inside FUA then it's in
##  fua files
fua_sf <- 
  google.drive.spatial %>%
  file.path('Functional urban area') %>%
  st_read('Urban_Audit_Functional_Urban_Areas_December_2016_Full_Clipped_Boundaries_in_the_United_Kingdom')

fua_sf$uafua16nm #Only some are in this functional urban area ; seems a lot larger
##  LSOA (centroids)
lsoa_sf <- 
  google.drive.spatial %>%
  file.path('LSOA 2001') %>%
  st_read('Lower_Layer_Super_Output_Areas_December_2001_Population_Weighted_Centroids')

##  Check same projection
st_crs(fua_sf) == st_crs(lsoa_sf)

##  1b) doing the merge and saving --------
lsoa2fua.lkp <- 
  lsoa_sf %>%
  st_join(fua_sf)

st_geometry(lsoa2fua.lkp) <- NULL

lsoa2fua.lkp <-
  lsoa2fua.lkp %>%
  rename(fua = uafua16nm) %>%
  dplyr::select(lsoa01cd, fua) 

lsoa2fua.lkp %>% 
  write.csv('Working analysis files/lsoa01 to fua16 lookup (optional).csv')



