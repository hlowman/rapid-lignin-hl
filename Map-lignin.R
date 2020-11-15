# Figure 1 Map Creation
# Heili Lowman
# 11/15/20

# The following script will create the map to be used in the lignin manuscript.
# It will use the sf package rather than creating the map by hand in powerpoint.

# Load packages
library(tidyverse)
library(ggplot2)
library(ggmap)
library(ggrepel)
library(sf)
library(USAboundaries)
library(USAboundariesData)
library(ggspatial)

# Load data
map_data <- read_csv("rapid_sites.csv")

map_df <- map_data[1:18,1:5] %>% # Removing odd rows/columns of NAs.
  mutate(lon = Lon) %>%
  mutate(lat = Lat) %>%
  mutate(env_f = factor(Environment, levels = c("Stream", "Estuary", "Marine near Stream", "Marine far from Stream"))) %>%
  mutate(fill_color = ifelse(env_f == "Marine near Stream", 2, 1))

# Create data sf object
map_sf <- st_as_sf(map_df,
                   coords = c("lon", "lat"),
                   remove = F,
                   crs = 4326) # WGS 84 projection

# Base plot to see how things are looking...
plot(map_sf$geometry)

# ggplot + sf map

# create CA county dataset for use in map below using USAboundaries
CA_counties <- us_counties(states = "California")

SB_county <- CA_counties %>%
  filter(name == "Santa Barbara")

# create base terrain map tile

# create bounding box
lats <- c(34.368063, 34.569875)
lons <- c(-120.287304, -119.634991)
bb <- make_bbox(lon = lons, lat = lats, f = 0.05)

sb_basemap <- get_map(location = bb, 
                      maptype = 'terrain-background', 
                      source = 'stamen')

ggmap(sb_basemap)
  
fullmap <- ggmap(sb_basemap) + # base google maps tile
  geom_point(data = map_sf, aes(x = lon, y = lat, shape = env_f, fill = env_f), 
          size = 4,
          inherit.aes = FALSE) + # adds points
  scale_shape_manual(values = c(21, 24, 22, 22)) +
  scale_fill_manual(values = c("white", "lightgrey", "grey40", "black")) +
  geom_text_repel(data = map_sf, 
                     aes(x = lon, 
                         y = lat, 
                         label = LTER_code),
                  nudge_x = 0.01,
                  nudge_y = 0.01) +
  ggspatial::annotation_north_arrow(location = "tr") + # adds compass due north
  ggspatial::annotation_scale() + # adds scale
  labs(x = "Longitude (WGS84)",
       y = "Latitude") +
  theme_bw() +
  theme(legend.position = "bottom") +
  coord_sf(crs = st_crs(4326))

fullmap

