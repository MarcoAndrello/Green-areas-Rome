library(osmdata)
library(sf)
# Le varie key e value sono elencate qui:
# https://wiki.openstreetmap.org/wiki/Map_Features

nat <- opq(bbox = 'Rome, Italy') %>%
    add_osm_feature(key = 'natural',
                    value = c('wood','grassland','scrub','heath','wetland', 'shrubbery', 'tree', 'tree_row'))
nat %>% osmdata_sf() -> nat

leis <- opq(bbox = 'Rome, Italy') %>%
    add_osm_feature(key = 'leisure',
                    value = c('garden','park','bird_hide', 'dog_park', 'nature_reserve'))
leis %>% osmdata_sf() -> leis

lu <- opq(bbox = 'Rome, Italy') %>%
    add_osm_feature(key = 'landuse',
                    value = c('meadow','forest','recreation_ground', 'farmland', 'orchard', 'plant_nursery', 'vineyard', 'cemetery','recreation_ground','village_green'))
lu %>% osmdata_sf() -> lu

by <- opq(bbox = 'Rome, Italy') %>%
    add_osm_feature(key = 'boundary',
                    value = c('forest','forest_compartment','national_park', 'protected_area'))
by %>% osmdata_sf() -> by

#head(nat$osm_points)
#head(leis$osm_polygons)

# For points, I only save the geometry bcs most have no info and there are many features
st_write(st_geometry(nat$osm_points),dsn="/Users/lucasantini/Documents/Jobs/grants/PRIN\ 2022/GIS/OSM\ layers/OSM_vector_natural_points.shp")
st_write(nat$osm_lines,dsn="/Users/lucasantini/Documents/Jobs/grants/PRIN\ 2022/GIS/OSM\ layers/OSM_vector_natural_lines.shp")
st_write(nat$osm_polygons,dsn="/Users/lucasantini/Documents/Jobs/grants/PRIN\ 2022/GIS/OSM\ layers/OSM_vector_natural_polygons.shp")
st_write(nat$osm_multipolygons,dsn="/Users/lucasantini/Documents/Jobs/grants/PRIN\ 2022/GIS/OSM\ layers/OSM_vector_natural_multipolygons.shp")

st_write(st_geometry(leis$osm_points),dsn="/Users/lucasantini/Documents/Jobs/grants/PRIN\ 2022/GIS/OSM\ layers/OSM_vector_leisure_points.shp")
st_write(leis$osm_lines,dsn="/Users/lucasantini/Documents/Jobs/grants/PRIN\ 2022/GIS/OSM\ layers/OSM_vector_leisure_lines.shp")
st_write(leis$osm_polygons,dsn="/Users/lucasantini/Documents/Jobs/grants/PRIN\ 2022/GIS/OSM\ layers/OSM_vector_leisure_polygons.shp")
st_write(leis$osm_multipolygons,dsn="/Users/lucasantini/Documents/Jobs/grants/PRIN\ 2022/GIS/OSM\ layers/OSM_vector_leisure_multipolygons.shp")

st_write(st_geometry(lu$osm_points),dsn="/Users/lucasantini/Documents/Jobs/grants/PRIN\ 2022/GIS/OSM\ layers/OSM_vector_landuse_points.shp")
st_write(lu$osm_lines,dsn="/Users/lucasantini/Documents/Jobs/grants/PRIN\ 2022/GIS/OSM\ layers/OSM_vector_landuse_lines.shp")
st_write(lu$osm_polygons,dsn="/Users/lucasantini/Documents/Jobs/grants/PRIN\ 2022/GIS/OSM\ layers/OSM_vector_landuse_polygons.shp")
st_write(lu$osm_multipolygons,dsn="/Users/lucasantini/Documents/Jobs/grants/PRIN\ 2022/GIS/OSM\ layers/OSM_vector_landuse_multipolygons.shp")

st_write(st_geometry(by$osm_points),dsn="/Users/lucasantini/Documents/Jobs/grants/PRIN\ 2022/GIS/OSM\ layers/OSM_vector_boundary_points.shp")
st_write(by$osm_lines,dsn="/Users/lucasantini/Documents/Jobs/grants/PRIN\ 2022/GIS/OSM\ layers/OSM_vector_boundary_lines.shp")
st_write(by$osm_polygons,dsn="/Users/lucasantini/Documents/Jobs/grants/PRIN\ 2022/GIS/OSM\ layers/OSM_vector_boundary_polygons.shp")
st_write(by$osm_multipolygons,dsn="/Users/lucasantini/Documents/Jobs/grants/PRIN\ 2022/GIS/OSM\ layers/OSM_vector_boundary_multipolygons.shp")

plot(st_geometry(a$osm_polygons))
