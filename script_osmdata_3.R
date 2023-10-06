library(tidyverse)
library(osmdata)
library(sf)

rm(list=ls())

bbox_Rome <- c(12.36, 41.78, 12.63, 42)

osmdata_object <- list()
v.key <- c("aerialway",
           "aeroway",
           "amenity",
           "barrier",
           "craft",
           "emergency",
           "geological",
           "healthcare",
           "highway",
           "historic",
           "landuse",
           "leisure",
           "man made",
           "military",
           "natural",
           "office",
           "place",
           "power",
           "public transport",
           "railway",
           # "route",
           "shop",
           "sport",
           "telecom",
           "tourism",
           "water",
           "waterway")

for (i.key in 1 : length(v.key)) {
  cat(v.key[i.key],"\n")
  overpass_query <- opq(bbox = bbox_Rome) %>%
    add_osm_feature(key = v.key[i.key])
  if (v.key[i.key] == "place") overpass_query <- opq(bbox = bbox_Rome) %>% 
    add_osm_feature(key = "place",value=c("city_block","plot","farm","square"))
  overpass_query %>% osmdata_sf() -> osmdata_object[[i.key]]
}

save(osmdata_object,file="OSM_data_Roma.RData")

# Ora check di che extent ha questa roba
for (i.key in 1 : length(v.key)) {
is.null(osmdata_object[[i.key]]$osm_polygons) | nrow(osmdata_object[[i.key]]$osm_polygons)==0
osmdata_object[[i.key]]$osm_multipolygons
}


pol_list <- multipol_list <- list()
v.key <- c('landcover','landuse','leisure','natural','tourism')
v.value <- list(landcover = c('grass','trees'),
                landuse = c('allotments', 'cemetery', 'construction','farmland','forest','grass','landfill', 'meadow', 'residential', 'vineyard'),
                leisure = c('dog_park', 'garden','park','pitch', 'playground', 'sports_centre'),
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
  # pol <- st_union(pol)
  # pol_unique <- st_cast(pol1,"POLYGON")
  # pol_unique <- st_sf(GEOMETRY=pol_unique)
  if (is.null(osmdata_object$osm_multipolygons)) next
  multipol <- osmdata_object$osm_multipolygons
  multipol %>% mutate( key= v.key[i.key], value = !!sym(v.key[i.key])) %>% select(osm_id, name, key, value) -> multipol_list[[i.key]]
  # multipol <- st_sf(GEOMETRY=st_geometry(multipol))
  # pol_multipol <- rbind(pol_unique,multipol)
}

green_areas_pol <- bind_rows(pol_list)
green_areas_pol %>% pull(osm_id) %>% duplicated %>% which -> pol_to_remove
green_areas_pol <- green_areas_pol[-pol_to_remove,]

green_areas_multipol <- bind_rows(multipol_list)
green_areas_multipol %>% pull(osm_id) %>% duplicated %>% which -> multipol_to_remove
green_areas_multipol <- green_areas_multipol[-multipol_to_remove,]

green_areas <- rbind(green_areas_pol, green_areas_multipol)
green_areas %>% pull(osm_id) %>% duplicated %>% which

st_write(green_areas, dsn="Green_areas_OSM_polygons.shp")

other_landuse = c('forest','recreation_ground', 'orchard', 'plant_nursery', 'village_green')
other_leisure = c('bird_hide',  'nature_reserve')
other_natural = c('wetland', 'shrubbery', 'tree', 'tree_row')
other_boundary = c('forest','forest_compartment','national_park', 'protected_area')