##  Querying openstreet map to find city and town centres as well as landmarks
##  Implementation requires the osmdata package to query osm
#options(repos = c(CRAN = "https://cran.revolutionanalytics.com")) # code to use different repository than default

pkgs <- c('osmdata', 'tmap', 'sf', 'tidyverse')
lapply(pkgs, require, character.only = T)
ukgrid = "+init=epsg:27700" ## always remember the CRS


# Section 1: Querying osm and saving the data -----------------------------
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

##  Let's go and find the localities which is inclusive of non divied up settlements 
##
##  Load in the geo files
##  localities
loc.sf <- st_read(dsn = google.drive %>% 
                    paste('/Spatial lookup files/UK/Scottish settlements', sep = ''), 
                  layer = 'Localities2012') %>%
  st_transform(crs = st_crs(ukgrid))

loc.pop <- google.drive %>% 
  paste('/Spatial lookup files/UK/Scottish settlements/loc2012 pop.csv', sep = '') %>%
  read.csv

loc.pop <- loc.pop %>% 
  mutate(L12CODE = Locality.Code, locality.pop = All.Ages) %>% 
  select(L12CODE, locality.pop, Locality.Name) 

loc.sf <- loc.sf %>% merge(loc.pop)

##  We can use st_join to literally do a left join that adds the variables of
##  the polygons to any point that intersects (i.e. falls within) that polygon
landmarks_sf <- landmarks_sf %>% 
  st_join(loc.sf) %>%
  dplyr::select(geometry, L12CODE, L12NAME, name, osm_id, locality.pop, type)

##  save it as a csv
landmarks_csv <- data.frame(landmarks_sf %>% st_coordinates,
                            L12CODE = landmarks_sf$L12CODE,
                            landmarknm = landmarks_sf$name,
                            type = landmarks_sf$type)
landmarks_csv %>% write.csv('Saved files for linkage/centre landmarks for localities coords.csv')


##  Viewing statistics
landmarks_sf
loc.pop %>% 
  filter(!(L12CODE %in% landmarks_sf$L12CODE)) %>% 
  filter(locality.pop > 10000) %>%
  arrange(locality.pop)

loc.pop %>% 
  filter(!(L12CODE %in% loc.sf$L12CODE)) %>% 
  arrange(locality.pop)
##  okay missing is due to non entries
##  Not every locality has every local feature! however... the smallest locality
##  is 43k -- we can just omit???

##  There are only 86 or so localities over 10k anyway
loc.pop %>% 
  filter(locality.pop > 10000) %>%
  arrange(-locality.pop) %>%
  mutate(cumulative = cumsum(locality.pop) / sum(locality.pop))

##  okay so if we do it this way then the cut off is over 20k and leads to 38 places
##  It is perfectly possile to cope with 38 places!
loc.pop %>% 
  filter(!(L12CODE %in% landmarks_sf$L12CODE)) %>% 
  filter(locality.pop > 30000) %>%
  arrange(locality.pop)

##  Filtering
sel.loc <- loc.pop %>% filter(locality.pop > 30000) %>% select(L12CODE)

selected <- landmarks_sf %>% 
  filter(L12CODE %in% sel.loc$L12CODE) %>%
  group_by(L12NAME) %>%
  summarise(n = n()) 
## 23 
loc.pop %>% 
  filter(!(L12CODE %in% landmarks_sf2$L12CODE)) %>% 
  filter(locality.pop > 30000) %>%
  arrange(locality.pop)
