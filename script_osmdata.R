library(osmdata)
library(sf)
# Le varie key e value sono elencate qui:
# https://wiki.openstreetmap.org/wiki/Map_Features
nat <- opq(bbox = 'Rome, Italy') %>%
    add_osm_feature(key = 'natural',
                    value = c('wood','grassland','scrub','heath','wetland'))
nat %>% osmdata_sf() -> nat
leis <- opq(bbox = 'Rome, Italy') %>%
    add_osm_feature(key = 'leisure',
                    value = c('garden','park'))
leis %>% osmdata_sf() -> leis
lu <- opq(bbox = 'Rome, Italy') %>%
    add_osm_feature(key = 'landuse',
                    value = c('meadow','forest','recreation_ground'))
lu %>% osmdata_sf() -> lu

head(nat$osm_points)
head(leis$osm_polygons)

# For points, I only save the geometry bcs most have no info and there are many features
st_write(st_geometry(nat$osm_points),dsn="OSM_vector_natural_points.gpkg")
st_write(nat$osm_lines,dsn="OSM_vector_natural_lines.gpkg")
st_write(nat$osm_polygons,dsn="OSM_vector_natural_polygons.gpkg")
st_write(nat$osm_multipolygons,dsn="OSM_vector_natural_multipolygons.gpkg")

st_write(st_geometry(leis$osm_points),dsn="OSM_vector_leisure_points.gpkg")
st_write(leis$osm_lines,dsn="OSM_vector_leisure_lines.gpkg")
st_write(leis$osm_polygons,dsn="OSM_vector_leisure_polygons.gpkg")
st_write(leis$osm_multipolygons,dsn="OSM_vector_leisure_multipolygons.gpkg")

st_write(st_geometry(lu$osm_points),dsn="OSM_vector_landuse_points.gpkg")
st_write(lu$osm_lines,dsn="OSM_vector_landuse_lines.gpkg")
st_write(lu$osm_polygons,dsn="OSM_vector_landuse_polygons.gpkg")
st_write(lu$osm_multipolygons,dsn="OSM_vector_landuse_multipolygons.gpkg")

plot(st_geometry(a$osm_polygons))
