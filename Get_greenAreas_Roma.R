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

# Seleziona elementi da sottrarre e li unisce in un solo multipolygon
OSM_features %>%
  filter(value %in% c("pitch","water","industrial","parking","parking_space","service")) %>%
  st_union -> fts # features to substract

# Sottrae questi elementi alle green areas
iv <- st_is_valid(green_areas)
green_areas_minus_1 <- st_difference(st_make_valid(green_areas),fts)
st_write(green_areas_minus_1,dsn="green_areas_minus_1.shp")
a <- green_areas_minus_1
table(st_geometry_type(a))
a %>% filter(st_is(.,"POLYGON")) %>% st_write(dsn="green_areas_minus_1_pol.shp")
a %>% filter(st_is(.,"MULTILINESTRING")) %>% st_write(dsn="green_areas_minus_1_mls.shp")

sc <- green_areas %>% filter(osm_id ==  1192608701)
sc <- st_read("ValcoSanPaolo.shp")
sc %>% filter(value != "pitch") -> sc
plot(st_geometry(sc))
a <- st_difference(sc,fts)
plot(st_geometry(a))

parcheggio <- OSM_features %>% filter(osm_id == 203083476)
prato <- green_areas %>% filter(osm_id == 1163669812)
plot(st_geometry(parcheggio))
plot(st_geometry(prato),add=T)
st_contains(parcheggio, prato)
#   other_landuse = c('recreation_ground', )
#   other_leisure = c('bird_hide',  'nature_reserve')
#   other_natural = c('wetland', 'shrubbery', 'tree', 'tree_row')
#   other_boundary = c('forest','forest_compartment','national_park', 'protected_area')
#   
# pitch <- green_areas %>% filter(osm_id %in% c(23646556,23646557, 220649698))
# plot(st_geometry(pitch))
# pitch_union <- st_union(pitch)
# sc <- green_areas %>% filter(osm_id ==  1192608701)
# plot(st_geometry(sc))
# plot(st_geometry(pitch),add=T)
# st_area(sc)
# a <- st_difference(sc,pitch_union)
# plot(st_geometry(a))
st_area(st_geometry(st_difference(sc,pitch)))
