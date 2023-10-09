library(tidyverse)
library(osmdata)
library(sf)

rm(list=ls())

bbox_Rome <- c(12.36, 41.78, 12.63, 42)
names(bbox_Rome) <- c("xmin", "ymin", "xmax","ymax")

pol_list <- multipol_list <- list()
v.key <- c('landcover','landuse','leisure','natural','tourism')
v.value <- list(landcover = c('grass','trees'),
                landuse = c('allotments', 'cemetery', 'construction','farmland','forest','grass','landfill', 'meadow', 'orchard', 'plant_nursery', 'residential', 'village_green', 'vineyard'),
                leisure = c('dog_park', 'garden','golf_course', 'park', 'playground', 'sports_centre'),
                natural = c('heath','grassland','scrub','wood'),
                tourism = c('caravan_site'))
i.key <- 1
for (i.key in 1 : 5) {
  cat(v.key[i.key],"\n")
  # Retrieve data from osm
  overpass_query <- opq(bbox = bbox_Rome) %>%
    add_osm_feature(key = v.key[i.key],
                    value = v.value[[i.key]])
  overpass_query %>% osmdata_sf() -> osmdata_object
  
  pol <- osmdata_object$osm_polygons
  pol %>% mutate( key= v.key[i.key], value = !!sym(v.key[i.key])) %>% select(osm_id, name, key, value) -> pol_list[[i.key]]
  if (is.null(osmdata_object$osm_multipolygons)) next
  multipol <- osmdata_object$osm_multipolygons
  multipol %>% mutate( key= v.key[i.key], value = !!sym(v.key[i.key])) %>% select(osm_id, name, key, value) -> multipol_list[[i.key]]
}

green_areas_pol <- bind_rows(pol_list)
green_areas_pol %>% pull(osm_id) %>% duplicated %>% which -> pol_to_remove
green_areas_pol <- green_areas_pol[-pol_to_remove,]

green_areas_multipol <- bind_rows(multipol_list)
green_areas_multipol %>% pull(osm_id) %>% duplicated %>% which -> multipol_to_remove
green_areas_multipol <- green_areas_multipol[-multipol_to_remove,]

green_areas <- rbind(green_areas_pol, green_areas_multipol)
green_areas %>% pull(osm_id) %>% duplicated %>% which

save(green_areas, file="Green_areas_OSM_polygons.RData")
st_write(green_areas, dsn="Green_areas_OSM_polygons.shp", append = F)
load("Green_areas_OSM_polygons.RData")


# Togliamo
# St_difference per togliere buildings, parcheggi etc
# st_difference su x (minuendo) e y (sottraendo): crea un elemento per ogni sottrazione
# quindi unendo (st_union) i sottraendi, ci sarà una sola sottrazione e sarà conservato il numero dei minuendi
load("OSM_features_noBuildings.RData")

# Seleziona elementi da sottrarre 
OSM_features %>%
  filter(value %in% c("pitch","water","industrial","service")) -> fts #%>%
  # filter(!(osm_id %in% c(142603104, 203083476, 1929164))) %>% # sono sotteranei e cancellano i prati sopra

# riscarica i parcheggi tenendo anche il layer
opq(bbox = bbox_Rome) %>%
  add_osm_feature(key = "amenity",
                  value = c("parking","parking_space")) %>%
  osmdata_sf() -> amenity_parking_space
## Evito anche landuse perché ce n'è solo 1
# opq(bbox = bbox_Rome) %>%
#   add_osm_feature(key = "landuse",
#                   value = "parking") %>%
#   osmdata_sf() -> landuse_parking
amenity_parking_space$osm_polygons %>%
  mutate(key = "amenity", value = !!sym("amenity")) %>%
  select(osm_id, key, value, layer) -> 
  amenity_parking_space_pol
amenity_parking_space$osm_multipolygons %>%
  mutate(key = "amenity", value = !!sym("amenity")) %>%
  select(osm_id, key, value, layer) -> 
  amenity_parking_space_multipol
amenity_parking_space <- rbind(amenity_parking_space_pol, amenity_parking_space_multipol)
amenity_parking_space %>% pull(osm_id) %>% duplicated %>% which
amenity_parking_space %>% pull(layer) %>% table
amenity_parking_space %>% filter(!(layer %in% c("-1","-3"))) %>%
  select(osm_id, key, value) -> amenity_parking_space_noSubt

# Unisce parkings alle altre features da rimuovere
fts <- rbind(fts, amenity_parking_space_noSubt)
fts %>% pull(osm_id) %>% duplicated %>% which
rm(amenity_parking_space, amenity_parking_space_pol, amenity_parking_space_multipol)
# e li unisce in un solo multipolygon
fts %>% st_union -> fts # features to substract

# Sottrae questi elementi alle green areas
# iv <- st_is_valid(green_areas)
green_areas_minus_1 <- st_difference(st_make_valid(green_areas),fts)
green_areas_minus_1 %>% st_geometry_type %>% table
# Extract the "geometrycollections", remove them from the object (together with multilinestrings), put them back in
green_areas_minus_1 %>%
  filter(st_is(.,"GEOMETRYCOLLECTION")) %>%
  st_collection_extract -> green_areas_minus_1_gcce
green_areas_minus_1 %>%
  filter(!st_is(.,"GEOMETRYCOLLECTION")) %>%
  filter(!st_is(.,"MULTILINESTRING")) %>%
  rbind(green_areas_minus_1_gcce) -> green_areas_minus_1
save(green_areas_minus_1,file="green_areas_minus_1.RData")
st_write(green_areas_minus_1,dsn="green_areas_minus_1.shp", append=F)

# Then remove buildings
buildings_pol <- st_read("G:/.shortcut-targets-by-id/12FFHFZn1Knejtg8k-BIRnvHkvQWdvDfw/Prin - UrBis/GIS/building_polygons.gpkg")
buildings_multipol <- st_read("G:/.shortcut-targets-by-id/12FFHFZn1Knejtg8k-BIRnvHkvQWdvDfw/Prin - UrBis/GIS/building_multipolygons.gpkg")
buildings <- rbind(buildings_pol, buildings_multipol)
buildings %>% pull(osm_id) %>% duplicated %>% which
buildings %>% pull(building) %>% table
buildings %>% st_make_valid %>% st_union -> buildings_union # takes 20 minutes
save(buildings_union,file="buildings_union.RData")
green_areas_minus_1_valid <- st_make_valid(green_areas_minus_1)
buildings_union_valid <- st_make_valid(buildings_union)
save(buildings_union_valid,file="buildings_union_valid.RData")

green_areas_minus_2 <- st_difference(green_areas_minus_1_valid,buildings_union_valid)
save(green_areas_minus_2, file="green_areas_minus_2.RData")
st_write(green_areas_minus_2,dsn="green_areas_minus_2.shp", append=F)

# Tolgo le strade dai residential
green_areas_minus_2_residential <- st_read("green_areas_minus_2_residential.shp")
load("highway_railway.RData")
highway_railway_valid <- st_make_valid(highway_railway)
highway_railway_union <- st_union(highway_railway_valid)
highway_railway_union_valid <- st_make_valid(highway_railway_union)
save(highway_railway_union_valid,file="highway_railway_union_valid.RData")
st_write(highway_railway_union_valid,dsn="highway_railway_union_valid.shp")
green_areas_minus_3_residential <- st_difference(green_areas_minus_2_residential, highway_railway_union_valid)

# QUI IL PROBLEMA E' CHE HIGHWAY RAILWAY UNION VALID SONO LINEE E NON PIù POLIGONI
green_areas_minus_3_residential %>% st_geometry_type %>% table
green_areas_minus_3_residential %>%
  filter(st_is(.,"GEOMETRYCOLLECTION")) %>%
  st_collection_extract -> green_areas_minus_3_residential_gcce
green_areas_minus_3_residential %>%
  filter(!st_is(.,"GEOMETRYCOLLECTION")) %>%
  rbind(green_areas_minus_3_residential_gcce) -> green_areas_minus_3_residential
save(green_areas_minus_3_residential, file="green_areas_minus_3_residential.RData")
st_write(green_areas_minus_3_residential,dsn="green_areas_minus_3_residential.shp", append=F)
