# Grigi fondo carta.
# Usiamo tutti gli elementi OSM poligoni + strade, e li sottraiamo da un rettangolo equivalente all'area di studio
library(tidyverse)
library(osmdata)
library(sf)
library(units)

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

load("OSM_data_Roma.RData")


# Get highways
osmdata_object[[9]]$osm_lines %>%
  filter(!(layer %in% c("-1","-2","-3","-4","-5"))) %>%
  filter(!(tunnel %in% c("yes","building_passage","covered"))) %>% # remove subterranean and tunnel
  filter(highway %in% c('motorway','trunk','primary','secondary','tertiary',
    'residential', 'unclassified','motorway_link','trunk_link','primary_link',
    'seconday_link','tertiary_link','living_street','pedestrian','service')) %>%
  select(osm_id, highway, name) -> highway_lines

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


# Buffer su highway_lines (23 minutes)
highway_lines %>%
  st_buffer(dist=set_units(5,"m")) ->
  highway_lines_buffered
save(highway_lines_buffered, file="highway_lines_buffered.RData")
st_write(highway_lines_buffered, dsn="highway_lines_buffered.shp")
## SPOSTATO IN GOOGLE DRIVE ###

# Buffer su railway_lines
railway_lines %>%
  st_buffer(dist=set_units(4,"m")) ->
  railway_lines_buffered
save(railway_lines_buffered, file="railway_lines_buffered.RData")
st_write(railway_lines_buffered, dsn="railway_lines_buffered.shp", append=F)
## SPOSTATO IN GOOGLE DRIVE ###


# Ora merge di:
#  - OSM_features_noBuildings (i.e. tutti i poligoni tranne i buildings),
#  - buildings (polygons and multipolygons)
#  - highway buffered
#  - railway buffered
# Quello che rimane dovrebbe essere il fondo carta
# Faccio in Qgis: fondi vettori, ripara geometrie, dissolvi
# L'ho fatto senza i nuovi highway/railways polygons/multipolygons (in cui ho tolto le features sotterranee) ma non dovrebbe essere troppo sbagliato

rm(osmdata_object)
load("OSM_features_noBuildings.RData")
load("buildings_union_valid.RData")
load("highway_lines_buffered.RData")
load("railway_lines_buffered.RData")
## Change column names
highway_lines_buffered %>% mutate(key="highway", value=highway) %>% select(osm_id, key, value) -> highway_lines_buffered
highway_polygons %>% mutate(key="highway", value=highway) %>% select(osm_id, key, value) -> highway_polygons
highway_multipolygons %>% mutate(key="highway", value=highway) %>% select(osm_id, key, value) -> highway_multipolygons
railway_lines_buffered %>% mutate(key="railway", value=railway) %>% select(osm_id, key, value) -> railway_lines_buffered
railway_polygons %>% mutate(key="railway", value=railway) %>% select(osm_id, key, value) -> railway_polygons
railway_multipolygons %>% mutate(key="railway", value=railway) %>% select(osm_id, key, value) -> railway_multipolygons

rbind(highway_lines_buffered, highway_polygons, highway_multipolygons,
      railway_lines_buffered, railway_polygons,railway_multipolygons) -> highway_railway
save(highway_railway, file="highway_railway.RData")
## SPOSTATO IN GOOGLE DRIVE ###

rbind(highway_lines_buffered, highway_polygons, highway_multipolygons,
      railway_lines_buffered, railway_polygons,railway_multipolygons,
      OSM_features) -> a
a$osm_id %>% duplicated %>% which -> to_remove
a[-to_remove,] -> a
rm(OSM_features, to_remove)
a %>% st_make_valid -> a_valid
a_valid %>% st_union -> a_union
save(a_union,file="highway_railway_features.RData")
st_write(a, dsn="highway_railway_features.shp",append=F)
## SPOSTATO IN GOOGLE DRIVE ###

a_union_valid <- st_make_valid(a_union)
save(a_union_valid, file="highway_railway_features_valid.RData")

area_studio <- st_read("Area_di_studio.shp")

greys_1 <-st_difference(area_studio, a_union_valid)
st_write(greys_1, dsn="greys_1.shp",append=F)

# ho due elementi: in Qgis seleziono quello corrispondente al fondo carta,
# faccio da multipolygon a polygons, elimino gli elementi più piccoli in funzioni dell'area e del thinnes ratio

st_write(buildings_union_valid,dsn="buildings_union_valid.shp")

# provo a fare la st_difference qui e non in QGis
# carico i greys_2
greys_2 <- st_read("greys_2.shp")

greys_3 <- st_difference(greys_2, buildings_union_valid)
greys_3 %>% st_geometry_type %>% table
greys_3 %>% filter(st_is(.,"MULTILINESTRING")) %>% st_write(dsn="greys_3_mls.shp")
greys_3 %>% filter(st_is(.,"GEOMETRYCOLLECTION")) %>% st_collection_extract %>% st_write(dsn="greys_3_gcce.shp")
greys_3 %>% filter(st_is(.,"POLYGON")) %>% st_write(dsn="greys_3_p.shp")
greys_3 %>% filter(st_is(.,"MULTIPOLYGON")) %>% st_write(dsn="greys_3_mp.shp")

greys_3 %>%
  filter(st_is(.,"GEOMETRYCOLLECTION")) %>%
  st_collection_extract -> greys_3_gcce
greys_3 %>%
  filter(!st_is(.,"GEOMETRYCOLLECTION")) %>%
  filter(!st_is(.,"MULTILINESTRING")) %>%
  filter(!st_is(.,"LINESTRING")) %>%
  rbind(greys_3_gcce) -> greys_3
save(greys_3,file="greys_3.RData")
st_write(greys_3,dsn="greys_3.shp",append=F)
# POI IN QGIS DA MULTIPART A PARTI SINGOLE, NE CANCELLO ALCUNE SULLA BASE DI AREA E THINNESS: GREYS_4, ## SPOSTATO IN GOOGLE DRIVE ###

