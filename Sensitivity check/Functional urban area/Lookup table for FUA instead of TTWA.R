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
  write.csv('Working analysis files/lsoa01 to fua16 lookup (optional).csv',
            row.names = F)

##  Second: We need a measure of distance of each lsoa to their nearest centre -----
##  This is largely taken from the same routine to do LSOA to ttwa11

##  Load in additiona pkgs / definitions
library(RANN)
ukgrid = "+init=epsg:27700" ## always remember the CRS


# 2. 1) Load in pop weighted centroids ---------------------------------------
lsoa01.sf <- st_read(
  dsn = google.drive.spatial %>%
    paste('/LSOA 2001', sep = ''),
  layer = 'Lower_Layer_Super_Output_Areas_December_2001_Population_Weighted_Centroids') %>%
  st_transform(crs = st_crs(ukgrid))

lsoa01.coords <- data.frame(lsoa01 = lsoa01.sf$lsoa01cd,
                            lsoa01.sf %>% st_coordinates)
##  merge with fua16 info
lsoa2fua.lkp <-
  read.csv('Working analysis files/lsoa01 to fua16 lookup (optional).csv')

##  Not perfect lsoa01 to ttwa11
lsoa01.coords <- 
  lsoa01.coords %>% 
  left_join(lsoa2fua.lkp, by = c('lsoa01' = 'lsoa01cd'))

##  Only keep those that have FUAs
lsoa01.coords <-
  lsoa01.coords %>%
  filter( !(fua %>% is.na) )

# 2.2) Load in the centres data ---------------------------------------------
centres.imputed <-
  read.csv('Working analysis files/Imputed centres based on osmdata.csv')

##  Find which fua they belong to
centre.sf <- centres.imputed %>% 
  st_as_sf(coords = c('imputed_easting', 'imputed_northing'),
           crs = st_crs(ukgrid))

fua_sf <- 
  google.drive.spatial %>%
  file.path('Functional urban area') %>%
  st_read('Urban_Audit_Functional_Urban_Areas_December_2016_Full_Clipped_Boundaries_in_the_United_Kingdom') %>%
  st_transform(crs = st_crs(ukgrid))


##  Find which centres belong inside with TTWA
centre.sf <- centre.sf %>% st_join(fua_sf)
centre.sf %>% summary
centre.sf$uafua16nm %>% is.na %>% table # okay 

##  Add to the centre.imputed data
centres.imputed <- 
  centres.imputed %>%
  mutate(uafua16nm = centre.sf$uafua16nm)

##  Any over 10k and 30 k respectively
centres10k.imputed <- centres.imputed %>% filter(pop11 > 10000)
centres30k.imputed <- centres.imputed %>% filter(pop11 > 30000)


# 2.3) Finding the nearest significant centre -------------------------------
##  Critera is to include an centre over 10k but also half the the largest centre
##  for the TTWA. Also deffo include over 60k
##  Note: For reasons our ttwa lkp and distances files is combined here

fua.nms <- 
  lsoa01.coords$fua %>% unique

nearest_paper.list <- list(NULL) 
centres10k.imputed$uafua16nm

##  Big for loop to determine nearest centre -----
for (i in 1:length(fua.nms)){
  temp.ttwa <- fua.nms[i] #keep the other labels the same
  print(temp.ttwa)
  
  ##  Now select the centre in the ttwa and find the largest -- this creates a 
  ##  variable called cutoff which we will use to calculat distance
  large.cent <- centres10k.imputed %>% 
    filter(uafua16nm == temp.ttwa) %>%
    arrange(- pop11) #important to arrnge by pop size
  
  temp.cut <- 10000 # set default cut off
  
  ## set cut off if there is a centre to half largest bua
  if(large.cent %>% nrow >= 1){
    temp.cut <- large.cent$pop11[1] / 2
  }
  
  ##  Include cities over 60k
  if(temp.cut > 60000){temp.cut <- 60000}
  
  ##  Now to take the nearest distance
  temp.lsoa <- lsoa01.coords %>% 
    filter(fua == temp.ttwa)
  

  temp.centre <- centres10k.imputed %>% filter(pop11 >= temp.cut)
  temp.main <- large.cent[1, ]
  
  ##  Now to run the nearest neightbour search separately ----
  ##  For the main distance -- we need an if else statement in case zeroes
  if(large.cent %>% nrow >= 1){
    nn_main <- nn2(query = temp.lsoa %>% dplyr::select(X, Y) %>% as.matrix,
                   data = temp.main %>% dplyr::select(imputed_easting, imputed_northing) %>% as.matrix,
                   k = 1)
    temp.lsoa <- temp.lsoa %>% 
      mutate(main_dist = nn_main$nn.dists %>% c,
             main_bua = temp.main$name[nn_main$nn.idx])
  }else(
    temp.lsoa <- temp.lsoa %>% 
      mutate(main_dist = NA,
             main_bua = NA)
    
  )
  
  ##  nearest dist
  nn_nearest <- nn2(query = temp.lsoa %>% dplyr::select(X, Y) %>% as.matrix,
                    data = temp.centre %>% dplyr::select(imputed_easting, imputed_northing) %>% as.matrix,
                    k = 1)
  
  temp.lsoa <- temp.lsoa %>% 
    mutate(nearest_dist = nn_nearest$nn.dists %>% c,
           nearest_bua = temp.centre$name[nn_nearest$nn.idx])
  ##  Save to list
  nearest_paper.list[[i]] <- temp.lsoa
}

##  2.4 Saving the nearest distance data from section 2.3 ----
nearest_paper.lkp <-
  do.call(rbind, nearest_paper.list)


nearest_paper.lkp %>% 
  dplyr::select(-X, -Y) %>%
  write.csv('Working analysis files/Distance from nearest centre for LSOA01 and FUA lkp (optional).csv',
            row.names = F)

##  This is all done!