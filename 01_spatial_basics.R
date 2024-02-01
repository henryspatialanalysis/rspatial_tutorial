## #######################################################################################
##
##
##
##
##
## #######################################################################################

# To install all the packages needed in this tutorial:
# install.packages(c('leaflet','mapview','viridisLite','RColorBrewer','sf','tidycensus'))

# Load packages using `library()`
library(leaflet)
library(mapview)
library(viridis)
library(RColorBrewer)
library(sf)
library(tidycensus)

# Set projection to be used throughout this tutorial
# USA Equidistant Conic, good for the continental US
working_crs <- sf::st_crs('ESRI:102005')

# Load Seattle neighborhoods
# Source: https://data.seattle.gov/dataset/Neighborhood-Map-Atlas-Neighborhoods/3x55-77b7/about_data
seattle_neighborhoods <- sf::st_read("http://tiny.cc/bntlwz")

# Load Seattle destinations: Supermarkets, Restaurants, and Coffee Shops
seattle_destinations_table <- read.csv(
  "https://raw.githubusercontent.com/henryspatialanalysis/rspatial_tutorial/main/input_data/seattle_destinations.csv"
)
table(seattle_destinations_table$type)


## Load census data --------------------------------------------------------------------->

# I'm using the package namespaces for each function, written as `namespace::function()`
# Look at all possible variables from the 2020 census
all_variables <- tidycensus::load_variables(year = 2020, dataset = 'pl')

# Load total population, total housing units, and occupied housing units across King
#  County from the 2020 decennial census
kc_blocks <- tidycensus::get_decennial(
  geography = 'block',
  variables = c("P1_001N"), # Census tracts
  year = 2020,
  state = '53',
  county = '033', # King County FIPS code
  geometry = TRUE,
  cache_table = TRUE
)
names(kc_blocks)[names(kc_blocks) == 'value'] <- 'total_pop'

# Inspect the blocks in RStudio

# See the current CRS for the blocks
sf::st_crs(kc_blocks)

# Reproject
kc_blocks <- sf::st_transform(kc_blocks, crs = working_crs)

# Get population density for each block
kc_blocks$area_km2 <- sf::st_area(kc_blocks) |> units::set_units('km2') |> units::drop_units()
kc_blocks$pop_density <- kc_blocks$total_pop / kc_blocks$area_km2


## Prepare and combine other spatial data ----------------------------------------------->

# Convert destinations into spatial objects
seattle_destination_points <- sf::st_as_sf(
  seattle_destinations_table,
  coords = c('lon','lat'),
  crs = sf::st_crs('EPSG:4326')
) |> sf::st_transform(crs = working_crs)
# Keep only points within city bounds
seattle_destination_points <- sf::st_intersection(
  x = seattle_destination_points,
  y = seattle_neighborhoods[, c('L_HOOD', 'S_HOOD')]
)

# Reproject Seattle neighborhoods to the working CRS
seattle_neighborhoods <- sf::st_transform(seattle_neighborhoods, crs = working_crs)

# Crop census blocks to Seattle only
seattle_blocks <- sf::st_intersection(
  x = kc_blocks,
  y = seattle_neighborhoods[, c('L_HOOD', 'S_HOOD')]
)


## Visualize census data ---------------------------------------------------------------->

# Simple static plot
plot(seattle_neighborhoods[, c("L_HOOD", "S_HOOD")])

# Static plot with the ggplot2 package
ggplot_map <- ggplot() +
  geom_sf(data = seattle_blocks, aes(fill = pop_density), color = NA) +
  geom_sf(data = seattle_neighborhoods, color = 'grey80', linewidth = 0.05, fill = NA) +
  geom_sf(data = seattle_destination_points, aes(color = type)) +
  scale_fill_viridis(
    trans = 'sqrt',
    breaks = c(0, 5000, 10000, 25000, 50000),
    labels = scales::comma,
    limits = c(0, 50000)
  ) +
  labs(
    title = 'Seattle population density by block',
    subtitle = 'Source: 2020 US Census',
    fill = 'Population\nDensity per km2\n(rescaled)'
  ) +
  theme_minimal()
# Plot it to screen
print(ggplot_map)

# Save it to file
png('~/seattle_density_map.png', height = 2100, width = 1500, res = 300)
print(ggplot_map)
dev.off()

# Create an interactive map
mapview::mapview(
  seattle_blocks,
  zcol = 'pop_density',
  layer.name = 'Population Density per km2'
)

# Improve the color scheme
color_breaks <- c(-Inf, 5000, 10000, 25000, 50000, Inf)
seattle_blocks$color_bin <- cut(
  x = seattle_blocks$pop_density,
  breaks = color_breaks
)
mapview::mapview(
  seattle_blocks,
  zcol = 'color_bin',
  layer.name = 'Population Density per km2'
)



