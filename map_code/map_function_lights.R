# Load libraries ----
library(tidyverse)
library(osmdata)
library(sf)
library(sysfonts)
library(showtext)
library(grid)

make_map <- function(querybox, filename_lamp_false, filename_lamp_true) {
  
  ## Step 2: Define bounding box ----
  bbox <- c(
    xmin = querybox[1],  # min longitude (western-most extent)
    ymin = querybox[2],  # min latitude (southern-most extent)
    xmax = querybox[3],  # max longitude (eastern-most extent)
    ymax = querybox[4]   # max latitude (northern-most extent)
  )
  
  ## Step 3: Create query ----
  query <- opq(bbox = bbox)
  
  suppressWarnings({
  ## Step 4: Extract OSM data ----
  ### rivers ----
  water_a <-
    query |>
    add_osm_feature(
      key = "natural", 
      value = "water") |>
    osmdata_sf()
  
  ### Other bodies of water ----
  water_b <-
    query |>
    add_osm_feature(
      key = "water",
      value = c(
        "lake",
        "pond",
        "river",
        "reservoir",
        "canal",
        "quarry_lake",
        "stream",
        "moat",
        "basin",
        "ditch",
        "oxbow",
        "wastewater",
        "reflecting_pool",
        "shallow"
      )
    ) |>
    osmdata_sf()
  
  ### "Minor" roads
  roads_b <- query |>
    add_osm_feature(
      key = "highway",
      value = c(
        "secondary",
        "tertiary",
        "unclassified",
        "residential",
        "secondary_link",
        "tertiary_link",
        "footway", 
        "pedestrian", 
        "living_street", 
        "residential", 
        "path", 
        "cycleway"
      )
    ) |>
    osmdata_sf()
  
  ### Street lamps ----
  street_lamps <- query |>
    add_osm_feature(
      key = "highway",
      value = "street_lamp") |>
    osmdata_sf()

  ###  Natural vegetation ----
  natural <-
    query |>
    add_osm_feature(
      key = "natural",
      value = c("wood", "grassland", "scrub")) |>
    osmdata_sf()
  
  ### Recreational greenery ----
  rec <-
    query |>
    add_osm_features(list(
      "landuse" = c("grass", "forest", "recreation_ground"),
      "leisure" = c("playground", "park", "garden", "dog_park")
    )) |>
    osmdata_sf()
  
  ## Step 5: Crop sf objects ----
  bbox_sf <- st_as_sfc(st_bbox(bbox, crs = 4326))
  
  water_a_cropped <- st_crop(water_a$osm_multipolygons, bbox_sf)
  water_b_cropped <- st_crop(water_b$osm_polygons, bbox_sf)
  roads_b_cropped <- st_crop(roads_b$osm_lines, bbox_sf)
  natural_cropped <- st_crop(natural$osm_polygons, bbox_sf)
  rec_cropped <- st_crop(rec$osm_polygons, bbox_sf)
  street_lamps_cropped <- st_crop(rec$osm_points, bbox_sf)

  ## Step 6: Visualize sf objects ----
  ### Define colour palette ----
  water_colour <- "#d3e1e4"
  roads_b_colour <- "#cccccc"
  natural_colour <- "#879682"
  rec_colour <- "#c1cbba"
  bkgd_colour <- "#f0f0f0"
  street_lamp_colour <- "#ffab00"
  
  plt <- ggplot() +
    # rivers
    geom_sf(data = water_a_cropped,
            fill = water_colour,
            colour = NA) +
    # other bodies of water
    geom_sf(data = water_b_cropped,
            fill = water_colour,
            colour = NA) +
    # natural vegetation
    geom_sf(data = natural_cropped,
            fill = natural_colour,
            colour = NA) +
    # recreational spaces
    geom_sf(data = rec_cropped,
            fill = rec_colour,
            colour = NA) +
    # minor roads
    geom_sf(
      data = roads_b_cropped,
      colour = roads_b_colour,
      size = 0.5,
      alpha = 0.7
    ) +
    coord_sf(expand = FALSE) + # remove margins
    theme_void() + # remove gridlines, etc.
    theme(panel.background = element_rect(fill = bkgd_colour))
  
  ggsave(filename_lamp_false)
  
  plt <- plt +
    # street lamps
    geom_sf(
      data = street_lamps_cropped,
      colour = street_lamp_colour,
      size = 0.8,
      alpha = 0.7
    )
  
  ggsave(filename_lamp_true)
  
  return(print('Your map has been exported!'))
  
  })
}

# References

# - <https://www.openstreetmap.org/>
# - <https://wiki.openstreetmap.org/wiki/Map_features>
# - <https://thetidytrekker.com/post/making-circular-maps/making-circular-maps.html>
# - <https://blog.devgenius.io/design-a-map-art-with-r-and-openstreetmap-eac6fc7a912b>
