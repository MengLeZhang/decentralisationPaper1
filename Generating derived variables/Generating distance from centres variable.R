##  Creating distances from urban centres based on LSOA11
##  Start: 30/11

source.file <- 'RCI functions.R' #path to source
source(source.file)
ukgrid = "+init=epsg:27700" ## always remember the CRS
library(RANN)


# 1) Load in pop weighted centroids ---------------------------------------
lsoa01.sf <- st_read(
  dsn = google.drive.spatial %>%
    paste('/LSOA 2001', sep = ''),
  layer = 'Lower_Layer_Super_Output_Areas_December_2001_Population_Weighted_Centroids') %>%
  st_transform(crs = st_crs(ukgrid))

lsoa01.coords <- data.frame(lsoa01cd = lsoa01.sf$lsoa01cd,
                          lsoa01.sf %>% st_coordinates)


# 2) Load in the centres data ---------------------------------------------
##  Any over 30k
centres.imputed <-
  read.csv('Working analysis files/Imputed centres based on osmdata.csv')
centres30k.imputed <- centres.imputed %>% filter(pop11 > 30000)
centres30k.imputed %>% head

# 3) Time to find the nearest neighbour -----------------------------------
nn.list <- nn2(query = lsoa01.coords %>% dplyr::select(X, Y) %>% as.matrix,
    data = centres30k.imputed %>% dplyr::select(imputed_easting, imputed_northing) %>% as.matrix,
    k = 1)

nearest30k.lkp <- data.frame(lsoa01cd = lsoa01.sf$lsoa01cd,
                             nearest_dist = nn.list$nn.dists %>% c,
                             nearest_bua = centres30k.imputed$name[nn.list$nn.idx])

# 3) Save the data --------------------------------------------------------
nearest30k.lkp %>% 
  write.csv('Working analysis files/Distance from nearest centre for LSOA01 (30k).csv',
            row.names = F)

