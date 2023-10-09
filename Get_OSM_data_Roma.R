library(tidyverse)
library(osmdata)
library(sf)

rm(list=ls())

bbox_Rome <- c(12.36, 41.78, 12.63, 42)
names(bbox_Rome) <- c("xmin", "ymin", "xmax","ymax")

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

# save(osmdata_object,file="OSM_data_Roma.RData")
load("OSM_data_Roma.RData")
## SPOSTATO IN GOOGLE DRIVE ###

# Ora check di che extent ha questa roba
i.key <- 1
bboxes_pol <- bboxes_multipol <- list()
for (i.key in 1 : length(v.key)) {
  if(!is.null(osmdata_object[[i.key]]$osm_polygons)){
    if(nrow(osmdata_object[[i.key]]$osm_polygons) > 0) {
      bboxes_pol[[i.key]] <- st_bbox(osmdata_object[[i.key]]$osm_polygons)
      #plot(st_geometry(osmdata_object[[i.key]]$osm_polygons))
    }
  }
  if(!is.null(osmdata_object[[i.key]]$osm_multipolygons)) {
    if (nrow(osmdata_object[[i.key]]$osm_multipolygons) > 0) {
      bboxes_multipol[[i.key]] <- st_bbox(osmdata_object[[i.key]]$osm_multipolygons)
      #plot(st_geometry(osmdata_object[[i.key]]$osm_multipolygons))
    }
  }
}

# Retrieve polygons, multipolygons and store them
i.key <- 1
pol_list <- multipol_list <- list()
for (i.key in 1 : length(v.key)) {
  if(!is.null(osmdata_object[[i.key]]$osm_polygons)){
    if(nrow(osmdata_object[[i.key]]$osm_polygons) > 0) {
      osmdata_object[[i.key]]$osm_polygons %>%
        mutate(key = v.key[i.key], value = !!sym(v.key[i.key])) %>%
        select(osm_id, key, value) ->
        pol_list[[i.key]] 
    }
  }
  if(!is.null(osmdata_object[[i.key]]$osm_multipolygons)) {
    if (nrow(osmdata_object[[i.key]]$osm_multipolygons) > 0) {
      osmdata_object[[i.key]]$osm_multipolygons %>%
        mutate(key = v.key[i.key], value = !!sym(v.key[i.key])) %>%
        select(osm_id, key, value) ->
        multipol_list[[i.key]]
    }
  }
}
rm(osmdata_object)
pol <- bind_rows(pol_list)
multipol <- bind_rows(multipol_list)

# Remove duplicates
pol %>% pull(osm_id) %>% duplicated %>% which -> pol_to_remove
pol_u <- pol[-pol_to_remove,]
multipol %>% pull(osm_id) %>% duplicated %>% which -> multipol_to_remove
multipol_u <- multipol[-multipol_to_remove,]

# Merge polygons and multipolygons in a dataset containing all but buildings
OSM_features <- rbind(pol_u, multipol_u)
OSM_features %>% pull(osm_id) %>% duplicated %>% which

save(OSM_features,file="OSM_features_noBuildings.RData")
st_write(OSM_features,dsn="OSM_features_noBuildings.shp")

# # Croppiamo tutto
# pol <- multipol <- list()
# for (i.key in 1 : length(v.key)) {
#   if(v.key[i.key] == "highway") next
#   cat(v.key[i.key],"\n")
#   if(!is.null(osmdata_object[[i.key]]$osm_polygons)){
#     if(nrow(osmdata_object[[i.key]]$osm_polygons) > 0) {
#       pol[[i.key]] <- st_crop(osmdata_object[[i.key]]$osm_polygons, bbox_Rome)
#       
#     }
#   }
#   if(!is.null(osmdata_object[[i.key]]$osm_multipolygons)) {
#     if (nrow(osmdata_object[[i.key]]$osm_multipolygons) > 0) {
#       multipol[[i.key]] <- st_crop(osmdata_object[[i.key]]$osm_multipolygons, bbox_Rome)
#     }
#   }
# }