##  Generating access to employment variable
##  We can do this by either using jobs within 15km or with 15km but weighted by RCI
##  Both is simply fine but we first need to narrow down to those with 15km

source.file <- 'RCI functions.R' #path to source
source(source.file)
ukgrid = "+init=epsg:27700" ## always remember the CRS
library(RANN)

##  1) Load in the oa files and convert to LSOA for workplace
##  Relevant lkp files for conversion
oa01tolsoa01.lkp <-
  read.csv('Working analysis files/oa01 to lsoa01 lkp.csv')

lsoa11tolsoa01.lkp <-
  read.csv('Working analysis files/lsoa11 to lsoa01 lkp.csv')

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


# 2) Getting the pop weighted centroids of lsoas --------------------------
lsoa01.sf <- st_read(
  dsn = google.drive.spatial %>%
    paste('/LSOA 2001', sep = ''),
  layer = 'Lower_Layer_Super_Output_Areas_December_2001_Population_Weighted_Centroids') %>%
  st_transform(crs = st_crs(ukgrid))

lsoa01.coords <- data.frame(lsoa01 = lsoa01.sf$lsoa01cd,
                            lsoa01.sf %>% st_coordinates)

lsoa01.coords <- lsoa01.coords %>% merge(wplace2001) %>% merge(wplace2011)
##  3) Let's now run the routine for access to employment
##  a) Employment within
##  b) divided by distance + 0.1 kmthned  squared 

##  The coordinates
search.coords <- lsoa01.coords %>% dplyr::select(X, Y) %>% as.matrix

##  We can use search a little bit at a time?; nearest 4k neighbours ought to be enough
nn.list <- nn2(query = search.coords,
               data = search.coords,
               k = 4000,
               searchtype = 'radius',
               radius = 15000)
#huge file
apply(nn.list$nn.idx, 1, min) %>% summary # okay so 4k is enough to each it

##  So now we have a huge matrix of nearest neighbours and indicies; each row contains the nearest 4k data for
##  each lsoa
emp.access <- lsoa01.coords %>% 
  mutate(access01 = NA, access11 = NA, access01_sq = NA, access11_sq = NA)
##  Add sum of 15km and inverse distance weighting (0.1 + dist)

##  For loop for calcualte the stats
for (i in 1:nrow(emp.access)){
  dist_vec <- nn.list$nn.dists[i, ] / 1000
  dist_vec <- dist_vec[dist_vec <= 15] 
  count01 <- emp.access$wplace01[nn.list$nn.idx[i, ]]
  count11 <- emp.access$wplace11[nn.list$nn.idx[i, ]]
  
  emp.access$access01[i] <- count01 %>% sum
  emp.access$access11[i] <- count11 %>% sum
  
  emp.access$access01_sq[i] <- t(1 / (0.1 + dist_vec) ^ 2) %*% count01
  emp.access$access11_sq[i] <- t(1 / (0.1 + dist_vec) ^ 2) %*% count11
}
##  Not the fastest but otherwise it would be very hard to understand


# Final step: Saving data -------------------------------------------------
emp.access %>% 
  dplyr::select(lsoa01, access01, access11, access01_sq, access11_sq) %>%
  write.csv('Working analysis files/Access to employment computed lkp.csv', row.names = F)

rm(list = ls())
