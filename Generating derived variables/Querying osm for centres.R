##  Querying openstreet map to find city and town centres as well as landmarks
##  Implementation requires the osmdata package to query osm
#options(repos = c(CRAN = "https://cran.revolutionanalytics.com")) # code to use different repository than default

pkgs <- c('osmdata', 'tmap', 'sf', 'tidyverse', 'RANN')
lapply(pkgs, require, character.only = T)
ukgrid = "+init=epsg:27700" ## always remember the CRS


# Section 1: Querying osm and saving the data (can skip if query done) -----------------------------
eng_bb <- getbb('england, uk') # Extent of England
wales_bb <- getbb('wales') # Extent of Wales
eng_bb; wales_bb ## actually england has the max and min
bb <- eng_bb

city_sf <- opq(bbox = bb) %>% 
  add_osm_feature(key = 'place', value = 'city') %>%
  osmdata_sf
city_sf$osm_points$type <- 'city'

town_sf <- opq(bbox = bb) %>% 
  add_osm_feature(key = 'place', value = 'town') %>%
  osmdata_sf
town_sf$osm_points$type <- 'town'

townhall_sf <- opq(bbox = bb) %>% 
  add_osm_feature(key = 'amenity', value = 'townhall') %>%
  osmdata_sf
townhall_sf$osm_points$type <- 'townhall'

# station_sf <- opq(bbox = bb) %>% 
#   add_osm_feature(key = 'public_transport', value = 'station') %>%
#   osmdata_sf
# station_sf$osm_points$type <- 'station'

landmarks_sf <- c(city_sf, 
                  town_sf, 
                  #station_sf, 
                  townhall_sf)
##  Okay.. I reckon we only need to get  points

landmarks_sf <- landmarks_sf$osm_points %>% st_transform(ukgrid)
landmarks_sf %>% object.size ##almost 250mb

landmarks_sf %>% saveRDS('Working analysis files/Large files to ignore/Saved Centre landmarks from osmdata.RDS')

# Section 2: Filtering and finding locality centre points -----------------
landmarks_sf <- readRDS('Working analysis files/Large files to ignore/Saved Centre landmarks from osmdata.RDS')
landmarks.coords <- data.frame(type = landmarks_sf$type, 
                               st_coordinates(landmarks_sf)) ##  we only need points

##  We have to basically get all the bua and bua subdivisions going
all.weighted <-
  read.csv('Working analysis files/bua and buasd11 pop weighted centroids and other data.csv')
pop_centroids.sf <- all.weighted %>% 
  st_as_sf(coords = c('pweight_x', 'pweight_y'),
           crs = st_crs(ukgrid))


# 3) Loading in the bua and buasd bondary files ---------------------------

bua.sf <- st_read(
  dsn = google.drive.spatial %>%
    paste('/BUA 2011', sep = ''),
  layer = 'Builtup_Areas_December_2011_Boundaries_V2') %>%
  st_transform(crs = st_crs(ukgrid))

bua.sf <- bua.sf %>% 
  filter(has_sd == 'N' & urban_bua == 'Yes') %>%
  mutate(name = bua11nm, 
         type = 'bua') %>%
  dplyr::select(name, type, geometry) #keep those without subdivision and is urban
###
buasd.sf <- st_read(
  dsn = google.drive.spatial %>%
    paste('/BUA subdivision 2011', sep = ''),
  layer = 'Builtup_Area_Sub_Divisions_December_2011_Boundaries') %>%
  st_transform(crs = st_crs(ukgrid))

buasd.sf <- buasd.sf %>% 
  mutate(name = buasd11nm, 
         type = 'buasd') %>%
  dplyr::select(name, type, geometry) #keep those without subdivision and is urban

all_bua.sf <- bua.sf %>% rbind(buasd.sf)


# 4) Running the for loop to get the centres ------------------------------


