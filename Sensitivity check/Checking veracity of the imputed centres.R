##  Checking imputed centres visuallly...
##  Start: 29/11/2018

pkgs <- c('osmdata', 'tmap', 'sf', 'tidyverse')
lapply(pkgs, require, character.only = T)
ukgrid = "+init=epsg:27700" ## always remember the CRS

# 1) Loading in the centres as sf files -----------------------------------
centres.imputed <-
  read.csv('Working analysis files/Imputed centres based on osmdata.csv')

centres.sf <- centres.imputed %>%
  st_as_sf(coords = c('imputed_easting', 'imputed_northing'),
           crs = st_crs(ukgrid))

# 2) Load in the bua and buasd files ------------------------------------------

bua.sf <- st_read(
  dsn = google.drive.spatial %>%
    paste('/BUA 2011', sep = ''),
  layer = 'Builtup_Areas_December_2011_Boundaries_V2') %>%
  st_transform(crs = st_crs(ukgrid))

bua.sf <- bua.sf %>% 
  filter(has_sd == 'N' & urban_bua == 'Yes') %>%
  mutate(bua_name = bua11nm, 
         bua_type = 'bua') %>%
  dplyr::select(bua_name, bua_type, geometry) #keep those without subdivision and is urban
###
buasd.sf <- st_read(
  dsn = google.drive.spatial %>%
    paste('/BUA subdivision 2011', sep = ''),
  layer = 'Builtup_Area_Sub_Divisions_December_2011_Boundaries') %>%
  st_transform(crs = st_crs(ukgrid))

buasd.sf <- buasd.sf %>% 
  mutate(bua_name = buasd11nm, 
         bua_type = 'buasd') %>%
  dplyr::select(bua_name, bua_type, geometry) #keep those without subdivision and is urban

all_bua.sf <- bua.sf %>% rbind(buasd.sf)

# 3) Creating a map for checking (over 30k pop) ------------------------------
##  so we have centres.sf and all_bua.sf
centres.sf <- centres.sf %>% 
  arrange(- pop11) %>% 
  mutate(cumulative = (pop11 %>% cumsum) / (pop11 %>% sum)) 

centres.sf %>% filter(cumulative > 0.75) # around 30k more or less


### Sensitivity for 30k
centres30k.sf <- centres.sf %>%
  filter(pop11 > 30000)
centres30k.sf %>% summary # still got 4k imputed error eh
centres30k.sf$imputed_type %>% table #oml 14 are origins #okay almost all are using city etc
centres30k.sf %>% filter(imputed_type == 'Origin') #huh buasds not a big deal i suppose
centres30k.sf %>% filter(imputed_error > 1500) #lots are cities hmm worth inspecting visually

check <- centres30k.sf %>% filter(imputed_error > 1500) %>% dplyr::select(name, imputed_osmm, imputed_error) %>% View
##  lots are in london and because their borough are slightyl diff but seem on point!

##  Let's seet where these things are
tmap_mode('plot')
bua.sf %>% head
qtm(all_bua.sf %>% filter(bua_name %in% centres30k.sf$name)) # still loads


## Setting to tmap mode creates the leaflet to view
tmap_mode('view')
centres30k.sf %>% filter(imputed_error > 1500) %>% qtm

##  4) Plotting the landmarks 
landmarks_sf <- readRDS('Working analysis files/Large files to ignore/Saved Centre landmarks from osmdata.RDS')
landmarks_sf %>% filter(type == 'town' & !is.na(name)) %>% qtm
'