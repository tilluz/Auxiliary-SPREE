library(rgdal)
library(tidyverse)
library(rgeos)
library(sp)

setwd("your_directory_path")

# Census maps (2003 and 2013) ---------------------------------------------------------

'
We had to shift the 2002 shapefile a bit using QGIS in order to align shape with 2013 map
(GRASS algorithm -> v.transform -> X shift = 890, Y shift = 540)
'

map02 <-  readOGR(dsn = './data/census_2002', layer="census_2002", verbose = F)

map13 <-  readOGR(dsn = './data/census_2013', layer="census_2013", verbose = F)

# To planar CRS -----------------------------------------------------------

to_utm <- "+proj=utm +zone=28 +a=6378137 +rf=298.257222932867 +units=m +no_defs"
map02 <- spTransform(map02, to_utm)
map13 <- spTransform(map13, to_utm)

# Buffer to remove bad polygon parts --------------------------------------

map02 <- gBuffer(map02, byid=TRUE, width=0)
map13 <- gBuffer(map13, byid=TRUE, width=0)


# Clean up after shape shifting -------------------------------------------

poly_map02 <- gUnaryUnion(map02, map02$CODE)

map02_id <- sapply(slot(poly_map02, "polygons"), function(x) slot(x, "ID"))

data_map02 <- data.frame(map02_id, row.names = map02_id) %>% 
  rename(CODE = map02_id)

map02 <- SpatialPolygonsDataFrame(poly_map02, data_map02) %>% 
  spTransform(., to_utm)

# Create intersection of 2002 and 2013 map --------------------------------

poly_intersect <- gIntersection(map13, map02, byid=TRUE)

poly_id <- sapply(slot(poly_intersect, "polygons"), function(x) slot(x, "ID"))

data_intersect <- data.frame(str_split(poly_id, " ", simplify = TRUE), row.names = poly_id) %>% 
  rename(MAP_ID = X1, CODE = X2)

map_intersect <- SpatialPolygonsDataFrame(poly_intersect, data_intersect) %>% 
  spTransform(., "+proj=longlat +datum=WGS84 +no_defs")

map_intersect@data <- map_intersect@data %>% 
  mutate(x_id = seq.int(nrow(map_intersect))) %>% 
  left_join(map13@data %>% select(MAP_ID, COD_ENTITE))

# Allocate 2013 communes to 2002 arrondissements via commune centroids ----

map13 <- map13 %>% 
  merge(gCentroid(map13, byid = TRUE) %>% 
          over(., map02, fn = NULL) %>% 
          select(CODE) %>% 
          rownames_to_column(., var = 'MAP_ID') %>% 
          mutate(CODE=replace(CODE, MAP_ID == 17, "0111")) %>%  # Allocate Goree to Dakar Plateau
          mutate(CODE=replace(CODE, MAP_ID == 9, "0112")) %>% # Allocate Hann Bel-Air to Grand Dakar
          mutate(dept02 = substr(CODE %>% as.character(), start = 1, stop = 3))
        , by = 'MAP_ID') %>% 
  spTransform(., "+proj=longlat +datum=WGS84 +no_defs")

map13@data <- map13@data %>% 
  mutate(dept13 = substr(COD_ENTITE %>% as.character(), start = 1, stop = 3))

# Union shapes for harmonised arrondissements -----------------------------

map13_arr <- gUnaryUnion(map13, map13$CODE)

map13_arr <- map02@data %>% 
  SpatialPolygonsDataFrame(map13_arr, .) %>% 
  spTransform(., "+proj=longlat +datum=WGS84 +no_defs")

# Union shapes for harmonised departments ---------------------------------

map13_dept <- gUnaryUnion(map13, map13$dept02)

map13_dept <- map13@data %>%
  select(dept02) %>% 
  distinct()

row.names(map13_dept) <- map13_dept$dept02

map13_dept <- map13_dept %>% 
  SpatialPolygonsDataFrame(gUnaryUnion(map13, map13$dept02), .) %>% 
  spTransform(., "+proj=longlat +datum=WGS84 +no_defs")

# Union shapes for harmonised 2013 departments ----------------------------

map13_dept13 <- gUnaryUnion(map13, map13$dept13)

map13_dept13 <- map13@data %>%
  select(dept13) %>% 
  distinct()

row.names(map13_dept13) <- map13_dept13$dept13

map13_dept13 <- map13_dept13 %>% 
  SpatialPolygonsDataFrame(gUnaryUnion(map13, map13$dept13), .) %>% 
  spTransform(., "+proj=longlat +datum=WGS84 +no_defs")

# Get centroids for pixel matching ----------------------------------------

map13_arr$x <- gCentroid(map13_arr, byid = TRUE)@coords[,'x']
map13_arr$y <- gCentroid(map13_arr, byid = TRUE)@coords[,'y']

map13_dept$x <- gCentroid(map13_dept, byid = TRUE)@coords[,'x']
map13_dept$y <- gCentroid(map13_dept, byid = TRUE)@coords[,'y']

map13_dept13$x <- gCentroid(map13_dept13, byid = TRUE)@coords[,'x']
map13_dept13$y <- gCentroid(map13_dept13, byid = TRUE)@coords[,'y']

# Check visually ----------------------------------------------------------

plot(map02)
plot(map13_arr, add = TRUE, border = "red", lwd = 1)


# Calculate distance matrix -----------------------------------------------

centroids13 <- gCentroid(map13, byid=TRUE)
dist13 <- spDists(centroids13, longlat=TRUE) %>% 
  as.data.frame()
colnames(dist13) <- map13$COD_ENTITE
dist13$COD_ENTITE <- map13$COD_ENTITE

# Save shapefile ----------------------------------------------------------

writeOGR(map_intersect, "./data/midsave/", "map_x", driver="ESRI Shapefile", overwrite_layer=TRUE)
writeOGR(map13, "./data/midsave/", "map_com_harmonised", driver="ESRI Shapefile", overwrite_layer=TRUE)
writeOGR(map13_arr, "./data/midsave/", "map_arr_harmonised", driver="ESRI Shapefile", overwrite_layer=TRUE)
writeOGR(map13_dept, "./data/midsave/", "map_dept_harmonised", driver="ESRI Shapefile", overwrite_layer=TRUE)
writeOGR(map13_dept13, "./data/midsave/", "map_dept13_harmonised", driver="ESRI Shapefile", overwrite_layer=TRUE)

saveRDS(dist13, file = "./data/midsave/dist13.rds")
