# Ottiene le strade e le railways, poi da bufferizzare in QGis
library(tidyverse)
library(osmdata)
library(sf)


rm(list=ls())

bbox_Rome <- c(12.36, 41.78, 12.63, 42)
names(bbox_Rome) <- c("xmin", "ymin", "xmax","ymax")

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

load("C:/Users/marco/OneDrive/Green-areas-Rome large files/OSM_data_Roma.RData")


# Get highways
osmdata_object[[9]]$osm_lines %>%
  filter(!(layer %in% c("-1","-2","-3","-4","-5"))) %>%
  filter(!(tunnel %in% c("yes","building_passage","covered"))) %>% # remove subterranean and tunnel
  filter(highway %in% c('motorway','trunk','primary','secondary','tertiary',
    'residential', 'unclassified','motorway_link','trunk_link','primary_link',
    'seconday_link','tertiary_link','living_street','pedestrian','service')) %>%
  select(osm_id, highway, name) -> highway_lines
st_write(highway_lines, dsn="highway_lines.shp")

osmdata_object[[9]]$osm_polygons %>%
  filter(highway %in% c('bus_stop', 'construction', 'crossing', 'living_street',
                        'pedestrian', 'platform', 'primary', 'raceway', 'residential',
                        'rest_area', 'secondary', 'service', 'services', 'tertiary',
                        'track', 'unclassified')) %>%
  filter(!(layer %in% "-1")) %>%
  select(osm_id, highway, name) -> highway_polygons

osmdata_object[[9]]$osm_multipolygons %>% 
  filter(highway %in% c('footway', 'pedestrian','service','services')) %>%
  filter(!(layer %in% "-1")) %>%
  select(osm_id, highway, name) -> highway_multipolygons
st_write(rbind(highway_polygons, highway_multipolygons),
         dsn="highway_polygons_multipolygons.shp")

# Get railways
osmdata_object[[20]]$osm_lines %>% pull(layer) %>% table
osmdata_object[[20]]$osm_lines %>% pull(tunnel) %>% table
osmdata_object[[20]]$osm_lines %>% pull(railway) %>% table
osmdata_object[[20]]$osm_lines %>%
  filter(!(layer %in% c("-1","-2","-3","-4"))) %>%
  filter(!(tunnel %in% c("yes","building_passage"))) %>% # remove subterranean and tunnel
  filter(railway %in% c('construction','disused','light_rail','narrow_gauge','preserved',
                        'rail','razed','subway','tram')) %>%
  select(osm_id, railway, name) -> railway_lines
st_write(railway_lines, dsn="railway_lines.shp")

osmdata_object[[20]]$osm_polygons %>% pull(layer) %>% table
osmdata_object[[20]]$osm_polygons %>% pull(tunnel) %>% table
osmdata_object[[20]]$osm_polygons %>% pull(railway) %>% table
osmdata_object[[20]]$osm_polygons %>%
  filter(!(layer %in% c("-1","-2"))) %>% # remove subterranean and tunnel
  filter(railway %in% c('platform','station','turntable','water_tower','workshop',
                        'yard')) %>%
  select(osm_id, railway, name) -> railway_polygons

osmdata_object[[20]]$osm_multipolygons %>% pull(layer) %>% table
osmdata_object[[20]]$osm_multipolygons %>% pull(tunnel) %>% table
osmdata_object[[20]]$osm_multipolygons %>% pull(railway) %>% table
osmdata_object[[20]]$osm_multipolygons %>%
  select(osm_id, railway, name) -> railway_multipolygons
st_write(rbind(railway_polygons, railway_multipolygons),
         dsn="railway_polygons_multipolygons.shp")

###
# poi buffer su lines 0.00005 gradi e su railway 0.00004 gradi, dissolvendoli
# fix geometries di highway_polygons_multipolygons e railway_polygons_multipolygons
# merge di highway_lines_buffered, highway_polygons_multipolygons, railway_lines_buffered e railway_polygons_multipolygons
# buffer di quanto ottenuto sopra, che ritorna: Highway_railway_dissolto.shp
# dissolvo tutto
