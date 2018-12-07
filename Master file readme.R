##  Master file: This is the masterfile outlining the steps used to create the 
##  data
## The idea is to simply create the data and link it all in one go to a common
##  geography (LSOA2001) for later analysis.

##  The R packages required and user functions are in this file
source('RCI functions.R')


# Step 1: Creating lookup tables for different geographies to LSOA01 --------
'Data cleaning and linkage/Creation of unifying lookup table using postcodes.R' %>% source # works but must run normally due to bizzare character
# creating lkp mainly using the ONS postcode directory
##  TTWA is using 2011; names and codes available from geoportal

##  Lots of the geo data that we used like LSOA2001, 2011, OA TTWA are 
##  simply lacking a proper lkp table so we need to create some for later stuff

# Step 2: Data need to created to link at LSOA2001 ----------------
##  Data for distance to city centres:
##  We use osm data to get city / town centres. Then we check for the closest
##  centre landmark to the population weighted centroid for each bua or bua sd.
##  Finally we calculate the distance from each LSOA centre to their closest bua 
##  centre

'Generating derived variables/Deriving BUA subdivision centroids and data.R' %>% source # we first need to use the bua and lsoa data to get bua pop and weighted centroids
'Generating derived variables/Querying osm for centres.R' %>% source  # Querying osm data and combining with bua data; we then run the find city centres routine
'Generating derived variables/Generating access to employment variable.R' %>% source # using centres data file to create distances of every pop weighted lsoa centre to nearest centre; fast using RANN

##  The other variable is access to employment which can use census data and lsoa centroids to do.
##  So based on workplace pop in census for 2001 / 2010 and distance

'Generating derived variables/Generating access to employment variable.R' %>% source


# Step 3: Linking the different sources at LSOA2001 -----------------------

'Data cleaning and linkage/Joining all the data for England and Wales.R' %>% source # Links the various data (created and from other sources) to the same dataset
##  master linked file save in : Working analysis files/Master data tables of variables for LSOA01.csv

# Step 4: Creating the results in a master table and outputs --------------

'Analysis and paper outputs/Inequality points estimates.R' %>% source # creates table of inequalities data for further analysis and stuff
##  file saved in Working analysis files/Duncan index results table.csv

