## #######################################################################################
##
## TUTORIAL 1: SPATIAL OPERATIONS IN R WITH THE "SF" PACKAGE
##
## #######################################################################################

# To install all the packages needed in this tutorial:
# install.packages(c('caret','ggplot2','mapview','viridis','sf','tidycensus'))

# Load packages using `library()`
library(caret)
library(ggplot2)
library(mapview)
library(viridis)
library(sf)
library(tidycensus)

# Set projection to be used throughout this tutorial
# USA Equidistant Conic, good for the continental US
working_crs <- sf::st_crs('ESRI:102005')

# Cache TIGER/LINE geometry data so it loads quickly in future runs
options(tigris_use_cache = TRUE)

# Load Seattle neighborhoods
# Source: https://data.seattle.gov/dataset/Neighborhood-Map-Atlas-Neighborhoods/3x55-77b7/about_data
seattle_neighborhoods <- sf::st_read("http://tiny.cc/bntlwz")

# Load Seattle destinations: Supermarkets, Restaurants, and Coffee Shops
seattle_destinations_table <- read.csv(
  "https://raw.githubusercontent.com/henryspatialanalysis/rspatial_tutorial/main/input_data/seattle_destinations.csv"
)
table(seattle_destinations_table$type)

# If lines 27 or 30 are too slow, you can also read these from the tutorial folder:
# seattle_neighborhoods <- sf::st_read("MaptimeSEA Tutorial 2024-02/seattle_neighborhoods.gpkg")
# seattle_destinations_table <- read.csv("MaptimeSEA Tutorial 2024-02/seattle_destinations_table.csv")


## Load census data --------------------------------------------------------------------->

# I'm using the package namespaces for each function, written as `namespace::function()`
# Look at all possible variables from the 2020 census
all_variables <- tidycensus::load_variables(year = 2020, dataset = 'pl')

# Load total population by block across King County from the 2020 decennial census
kc_blocks <- tidycensus::get_decennial(
  geography = 'block',
  variables = c("P1_001N"), # Census tracts
  year = 2020,
  state = '53',
  county = '033', # King County FIPS code
  geometry = TRUE,
  cache_table = TRUE
)
# If the line above is too slow, you can also read this object from the tutorial folder
# kc_blocks <- sf::st_read("MaptimeSEA Tutorial 2024-02/king_county_blocks.gpkg")

# Rename the 'value' column to 'total_pop'
names(kc_blocks)[names(kc_blocks) == 'value'] <- 'total_pop'

# Inspect the blocks in RStudio
View(kc_blocks)

# See the current CRS for the blocks
sf::st_crs(kc_blocks)

# Reproject
kc_blocks <- sf::st_transform(kc_blocks, crs = working_crs)

# Get population density for each block
kc_blocks$area_km2 <- sf::st_area(kc_blocks) |> units::set_units('km2') |> units::drop_units()
# The following commented line is equivalent to the one above, but harder to read:
# kc_blocks$area_km2 <- units::drop_units(units::set_units(sf::st_area(kc_blocks), 'km2'))

kc_blocks$pop_density <- kc_blocks$total_pop / kc_blocks$area_km2


## Prepare and combine other spatial data ----------------------------------------------->

# Reproject Seattle neighborhoods to the working CRS
seattle_neighborhoods <- sf::st_transform(seattle_neighborhoods, crs = working_crs)

# Convert destinations into spatial objects
seattle_destination_points <- sf::st_as_sf(
  seattle_destinations_table,
  coords = c('lon','lat'),
  crs = sf::st_crs('EPSG:4326')
) |>
  sf::st_transform(crs = working_crs) |>
  sf::st_intersection(y = seattle_neighborhoods[, c('L_HOOD', 'S_HOOD')])

# Crop census blocks to Seattle only
seattle_blocks <- sf::st_intersection(
  x = kc_blocks,
  y = seattle_neighborhoods[, c('L_HOOD', 'S_HOOD')]
)

# Inspect the Seattle blocks object
View(seattle_blocks)


## Visualize census data ---------------------------------------------------------------->

## Type 1: Simple static plot
plot(seattle_neighborhoods[, c("L_HOOD", "S_HOOD")])


## Type 2: Create an interactive map
mapview::mapview(
  seattle_blocks,
  zcol = 'pop_density',
  layer.name = 'Population Density per km2'
)

# Improve the color scheme
color_breaks <- c(-Inf, 5000, 10000, 25000, 50000, Inf)
color_labels <- c("<5k", "5-10k", "10-25k", "25-50k", ">50k")
seattle_blocks$color_bin <- cut(
  x = seattle_blocks$pop_density,
  breaks = color_breaks,
  labels = color_labels
)
mapview::mapview(
  seattle_blocks,
  zcol = 'color_bin',
  layer.name = 'Population Density per km2'
)


## Type 3: Static plot with the ggplot2 package
ggplot_map <- ggplot() +
  geom_sf(data = seattle_blocks, aes(fill = pop_density), color = NA) +
  geom_sf(data = seattle_neighborhoods, color = 'grey80', linewidth = 0.05, fill = NA) +
  scale_fill_viridis(
    trans = 'sqrt',
    breaks = c(0, 2500, 10000, 25000, 50000),
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

# Add destination locations
ggplot_map_with_destinations <- ggplot_map +
  geom_sf(data = seattle_destination_points, aes(color = type), size = 1, alpha = .75) +
  scale_color_manual(
    values = c(Restaurants = 'red', 'Coffee shops' = 'darkorange', Supermarkets = '#ff33ff')
  ) +
  labs(color = 'Destination type')
print(ggplot_map_with_destinations)

# Save it to file
png('seattle_density_map.png', height = 2100, width = 1500, res = 300)
print(ggplot_map)
dev.off()


## Let's run a simple machine learning model to predict population density -------------->

# Get geographic distances (as the crow flies) from the center of each block to nearest
#  features
block_centroids <- sf::st_centroid(seattle_blocks, of_largest_polygon = TRUE)

split_by_feature <- list(
  restaurants = seattle_destination_points[seattle_destination_points$type == 'Restaurants', ],
  coffee = seattle_destination_points[seattle_destination_points$type == 'Coffee shops', ],
  supermarkets = seattle_destination_points[seattle_destination_points$type == 'Supermarkets', ]
)

for(feature_type in c('restaurants','coffee','supermarkets')){
  destination_subset <- split_by_feature[[feature_type]]
  # Get the row number of the nearest feature to each block's center
  nearest_index <- sf::st_nearest_feature(x = block_centroids, y = destination_subset)
  # Calculate the straight-line distance between each block center and the nearest
  #  destination
  block_centroids[[feature_type]] <- sf::st_distance(
    x = block_centroids,
    y = destination_subset[nearest_index, ],
    by_element = TRUE
  )
}

# Get latitude and longitude of each block centroid
block_centroids_unprojected <- sf::st_transform(block_centroids, crs = sf::st_crs('EPSG:4326'))
block_centroids_unprojected$x <- sf::st_coordinates(block_centroids_unprojected)[, 'X']
block_centroids_unprojected$y <- sf::st_coordinates(block_centroids_unprojected)[, 'Y']

# Split into training and test datasets
# Include only populated blocks for this example
full_dataset <- as.data.frame(block_centroids_unprojected)
full_dataset <- full_dataset[full_dataset$pop_density > 0, ]

train_index <- caret::createDataPartition(full_dataset$L_HOOD, p = .8, list = F, times = 1)
training_data <- full_dataset[train_index, ]
test_data <- full_dataset[-train_index, ]


## COMPARE PREDICTIVE PERFORMANCE OF SEVERAL ML MODELS ---------------------------------->

# For more information about regression models available in caret:
# https://topepo.github.io/caret/available-models.html
model_options <- list(
  lm = list(),
  enet = list(lambda = 0.25),
  svmRadial = list(sigma = 0.1),
  xgbTree = list(nrounds = 4, max_depth = 2, eta = 0.7),
  rf = list(mtry = 5)
)
# Try also: 'enet', 'svmRadial', 'xgbTree', 'rf'
test_model_type <- 'lm'

# Run a regression model on the training data
model_fit <- do.call(
  what = caret::train,
  args = c(
    list(
      form = pop_density ~ x + y + restaurants + coffee + supermarkets + L_HOOD,
      data = training_data,
      method = test_model_type
    ),
    model_options[[test_model_type]]
  )
)

# See how well the model predicts the remaining 20% of data
test_data$predictions <- predict(model_fit, newdata = test_data)

r_squared <- cor(test_data$pop_density, test_data$predictions)
rmse <- (test_data$pop_density - test_data$predictions)**2 |> mean(na.rm = T) |> sqrt()

# Plot the results
results_plot <- ggplot(data = test_data, aes(x = predictions, y = pop_density)) +
  geom_point(aes(color = L_HOOD)) +
  geom_abline(intercept = 0, slope = 1, color = '#222222', linetype = 3, linewidth = 1) +
  geom_smooth(method = 'loess', span = .3) +
  scale_x_continuous(trans = 'sqrt', labels = scales::comma) +
  scale_y_continuous(trans = 'sqrt', labels = scales::comma) +
  labs(
    title = paste('Out-of-sample performance for model type:', toupper(test_model_type)),
    subtitle = paste('RMSE:', scales::comma(rmse), '| R-squared:', round(r_squared, 2)),
    x = 'Population density (predicted)',
    y = 'Population density (actual)',
    color = 'Neighborhood'
  ) +
  guides(color = guide_legend(ncol = 2)) +
  theme_bw()
print(results_plot) |> suppressWarnings()
