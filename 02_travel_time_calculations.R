## #######################################################################################
##
## 02) CALCULATING TRAVEL TIMES BY BLOCK
##
## #######################################################################################

library(data.table)
library(ggplot2)
library(mapview)
library(sf)

# Set Java allowed memory before loading R5R
java_memory_gb <- 2
options(java.parameters = paste0("-Xmx", java_memory_gb, "G"))
library(r5r)

# WGS 1984 unprojected (lat-long) CRS
unprojected_crs <-sf::st_crs('EPSG:4326')

## Load input data
# All input data should be:
# - 1) Set to unprojected lat-long space
# - 2) Assigned a unique "ID" field that will be used by r5r
seattle_block_centroids <- sf::st_read(
  "MaptimeSEA Tutorial 2024-02/seattle_block_centroids.gpkg"
) |> sf::st_transform(crs = unprojected_crs)
seattle_block_centroids$id <- seattle_block_centroids |> nrow() |> seq_len() |> as.character()

seattle_destinations <- sf::st_read(
  "MaptimeSEA Tutorial 2024-02/seattle_destinations.gpkg"
) |> sf::st_transform(crs = unprojected_crs)
seattle_destinations$id <- seattle_destinations |> nrow() |> seq_len() |> as.character()

# Load Seattle neighborhoods, for plotting
seattle_neighborhoods <- sf::st_read(
  "MaptimeSEA Tutorial 2024-02/seattle_neighborhoods.gpkg"
) |> sf::st_transform(crs = unprojected_crs)
seattle_no_water <- seattle_neighborhoods |> sf::st_union() |> suppressWarnings()

## Set up R5 with the OpenStreeMap data
## This may take several minutes on the first setup
r5r_core <- r5r::setup_r5(data_path = "MaptimeSEA Tutorial 2024-02")

# We will use the "viridis" color pallette to map travel times
tt_colors <- c("#440154", "#3B528B", "#21908C", "#5DC863", "#FDE725")



## CREATE WALKING ISOCHRONES FOR YOUR BLOCK --------------------------------------------->

# Pick the origin block you want to map
mapview::mapview(seattle_block_centroids)

my_block_id <- 1482
my_block_centroid <- seattle_block_centroids[seattle_block_centroids$id == my_block_id, ]
walking_times <- c(5, 10, 15, 20, 25, 30)

walk_isochrone <- r5r::isochrone(
  r5r_core = r5r_core,
  origins = my_block_centroid,
  cutoffs = walking_times,
  mode = 'WALK',
  sample_size = 1
)
walk_isochrone <- walk_isochrone[order(-walk_isochrone$isochrone), ]


# Prepare some derivative spatial objects for plotting
isochrones_drop_water <- sf::st_intersection(x = walk_isochrone, y = seattle_no_water) |>
  suppressWarnings()

biggest_isochrone <- walk_isochrone[which.max(walk_isochrone$isochrone), ]

overlapping_destinations <- sf::st_intersection(
  x = seattle_destinations,
  y = sf::st_geometry(biggest_isochrone)
) |> suppressWarnings()

plot_bbox <- sf::st_bbox(isochrones_drop_water) + c(-.001, -.001, .001, .001)


# Interactive map it!
isochrones_interactive_map <- mapview::mapview(
  isochrones_drop_water,
  zcol = 'isochrone',
  at = walking_times,
  col.regions = rev(tt_colors),
  layer.name = paste0('Walking time from block<br/>', my_block_geoid),
  alpha.regions = .75
) |> suppressWarnings() + mapview::mapview(
  my_block_centroid,
  layer.name = 'Origin',
  col.regions = 'black',
  cex = 5,
  alpha.regions = 0.9,
  label = 'Origin'
) + mapview::mapview(
  overlapping_destinations,
  zcol = 'type',
  col.regions = c(Restaurants = 'red', 'Coffee shops' = 'darkorange', Supermarkets = '#ff33ff'),
  cex = 2.5,
  layer.name = 'Nearby destinations'
)
print(isochrones_interactive_map)

# GGplot it!
isochrone_fig <- ggplot() +
  geom_sf(data = seattle_no_water, fill = 'darkgrey', color = 'darkgrey') +
  geom_sf(data = isochrones_drop_water, color = NA, aes(fill = isochrone)) +
  geom_sf(data = overlapping_destinations, size = 2, aes(color = type)) +
  geom_sf(data = my_block_centroid, pch = 18, color = 'black', size = 4) +
  scale_fill_gradientn(colors = tt_colors) +
  scale_color_manual(
    values = c(Restaurants = 'red', 'Coffee shops' = 'darkorange', Supermarkets = '#ff33ff')
  ) +
  lims(x = c(plot_bbox$xmin, plot_bbox$xmax), y = c(plot_bbox$ymin, plot_bbox$ymax)) +
  labs(
    title = paste0(max(walking_times), '-minute walkshed for block ', my_block_id),
    color = 'Destination type',
    fill = 'Walk time\nfrom origin'
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = 'lightblue', color = NA),
    axis.ticks = element_blank(), axis.text = element_blank(), panel.grid = element_blank()
  )
print(isochrone_fig)


## CALCULATE TRAVEL TIMES FROM THIS BLOCK TO ALL NEARBY DESTINATIONS -------------------->

travel_times_by_destination <- r5r::travel_time_matrix(
  r5r_core = r5r_core,
  origins = my_block_centroid,
  destinations = overlapping_destinations,
  mode = 'WALK',
  max_trip_duration = 30L
)
travel_times_by_destination$id <- travel_times_by_destination$to_id

destinations_with_times <- merge(
  x = overlapping_destinations,
  y = travel_times_by_destination[, c('travel_time_p50', 'id')],
  by = 'id'
)

# Interactive map it!
destinations_interactive_map <- mapview::mapview(
  x = destinations_with_times,
  zcol = 'travel_time_p50',
  col.regions = rev(tt_colors),
  layer.name = paste0('Walking time from block<br/>', my_block_geoid),
  alpha.regions = .75,
  cex = 3
) |> suppressWarnings() + mapview::mapview(
  x = my_block_centroid,
  layer.name = 'Origin',
  col.regions = 'black',
  cex = 5,
  alpha.regions = 0.9,
  label = 'Origin'
) + mapview::mapview(
  x = sf::st_cast(biggest_isochrone, to = 'MULTILINESTRING'),
  col.regions = 'darkgrey',
  layer.name = '30-minute walkshed',
  label = '30-minute walkshed'
)
print(destinations_interactive_map)

# GGplot it!
destination_times_fig <- ggplot() +
  geom_sf(data = seattle_no_water, fill = '#666666', color = 'darkgrey') +
  geom_sf(
    data = isochrones_drop_water[1, ], fill = NA, color = '#222222', linetype = 3,
    linewidth = 1
  ) +
  geom_sf(
    data = destinations_with_times, size = 3, aes(shape = type, color = travel_time_p50)
  ) +
  geom_sf(data = my_block_centroid, pch = 18, color = 'black', size = 4) +
  scale_color_gradientn(colors = tt_colors, breaks = walking_times, labels = walking_times) +
  lims(x = c(plot_bbox$xmin, plot_bbox$xmax), y = c(plot_bbox$ymin, plot_bbox$ymax)) +
  labs(
    title = paste0('Walk times from block ', my_block_id, ' to nearby destinations'),
    color = 'Walk time\nfrom origin',
    shape = 'Destination type'
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = 'lightblue', color = NA),
    axis.ticks = element_blank(), axis.text = element_blank(), panel.grid = element_blank()
  )
print(destination_times_fig)


## CALCULATE TRAVEL TIMES TO THE NEAREST DESTINATION TYPE BY BLOCK ---------------------->

# Neighborhood selection
mapview::mapview(seattle_neighborhoods, zcol = 'S_HOOD', legend = FALSE)
neighborhood_name <- 'Fremont'
subset_type <- 'Coffee shops'

# Subset the origin blocks to the neighborhood, and destination blocks a neighborhood
#  buffer
neighborhood_block_centroids <- seattle_block_centroids[
  seattle_block_centroids$S_HOOD == neighborhood_name,
]
neighborhood_boundaries <- seattle_neighborhoods[
  seattle_neighborhoods$S_HOOD == neighborhood_name,
]
neighborhood_buffer <- neighborhood_boundaries |>
  sf::st_buffer(dist = units::set_units(3, 'km')) |>
  sf::st_geometry()

destinations_subset <- seattle_destinations[seattle_destinations$type == subset_type, ] |>
  sf::st_intersection(y = neighborhood_buffer) |>
  suppressWarnings()

# Inspect the neighborhood buffer and all intersecting points
plot(neighborhood_buffer, col = 'lightgrey')
plot(sf::st_geometry(neighborhood_boundaries), col = 'red', add = T)
plot(
  sf::st_geometry(destinations_subset),
  size = 0.2, pch = 20, col = alpha('#663300', 0.5), add = T
)

# Run travel time analysis
travel_times_by_od_pair <- r5r::travel_time_matrix(
  r5r_core = r5r_core,
  origins = neighborhood_block_centroids,
  destinations = destinations_subset,
  mode = "WALK",
  max_trip_duration = 30L
)

# Inspect results
View(travel_times_by_od_pair)

## Summarize in three ways:
##  - `closest`: Travel time to the nearest one coffee shop
##  - `n_under_30`: Number of destinations within 30 minutes
##  - `n_under_15`: Number of destinations within 15 minutes
neighborhood_summary <- travel_times_by_od_pair[, .(
  closest = min(travel_time_p50),
  n_under_30 = sum(travel_time_p50 <= 30),
  n_under_15 = sum(travel_time_p50 <= 15)
), by = .(id = from_id)]

# Load neighborhood block polygons (not centroid points) for visualization
neighborhood_blocks <- sf::st_read(
  "MaptimeSEA Tutorial 2024-02/king_county_blocks.gpkg"
) |>
  sf::st_transform(crs = unprojected_crs) |>
  sf::st_intersection(y = sf::st_geometry(neighborhood_boundaries)) |>
  merge(y = as.data.frame(neighborhood_block_centroids)[, c('GEOID', 'id')], by = 'GEOID') |>
  merge(y = neighborhood_summary, by = 'id', all.x = TRUE) |>
  suppressWarnings()

# Interactive map it!
mapview::mapview(
  x = neighborhood_blocks,
  zcol = 'closest',
  layer.name = paste0('Walking time to closest<br/>', subset_type, ' (min.)'),
  col.regions = rev(tt_colors)
) |> suppressWarnings() + mapview::mapview(
  x = destinations_subset,
  zcol = 'name',
  col.regions = '#663300',
  alpha.regions = 0.5,
  cex = 3,
  layer = subset_type
)

# More interactive maps!
dest_density_colors <- c(
  "#D53E4F","#F46D43","#FDAE61","#FEE08B","#FFFFBF","#E6F598","#ABDDA4","#66C2A5","#3288BD"
)
dest_density_map <- mapview::mapview(
  x = neighborhood_blocks,
  zcol = 'n_under_15',
  layer.name = paste0("# ", subset_type,"<br> within 15 min."),
  col.regions = dest_density_colors
) |> suppressWarnings() + mapview::mapview(
  x = destinations_subset,
  label = 'name',
  col.regions = '#663300',
  alpha.regions = 0.5,
  cex = 3,
  layer = subset_type
)
print(dest_density_map)


## #######################################################################################
##
## NEXT STEPS:
##
## - Make an interactive map showing number of destinations within a 30-minute walk of
##     each block
##
## - Try running the previous sections with a different origin block, different
##     neighborhood, and a different destination type ("Restaurants" or "Supermarkets")
##
## - Download a different amenity type from http://overpass-turbo.eu . For more examples
##     of amenities, see these OpenStreetMap wiki pages:
##     * https://wiki.openstreetmap.org/wiki/Key:amenity
##     * https://wiki.openstreetmap.org/wiki/Key:shop
##     * https://wiki.openstreetmap.org/wiki/Key:leisure
##
## #######################################################################################
