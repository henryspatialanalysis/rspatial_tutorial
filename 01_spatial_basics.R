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
  sf::st_intersection(y = seattle_neighborhoods[, c('L_HOOD', 'S_HOOD')]) |>
  suppressWarnings()

# Crop census blocks to Seattle only
seattle_blocks <- sf::st_intersection(
  x = kc_blocks,
  y = seattle_neighborhoods[, c('L_HOOD', 'S_HOOD')]
) |> suppressWarnings()

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

# Include only populated blocks for this test
block_centroids <- sf::st_centroid(seattle_blocks, of_largest_polygon = TRUE)
block_centroids <- block_centroids[block_centroids$total_pop > 0, ]
block_centroids$id <- seq_len(nrow(block_centroids))

# Get geographic distances (as the crow flies) from the center of each block to nearest
#  features
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
  ) |> units::set_units('m') |> units::drop_units()
}

# Get latitude and longitude of each block centroid
block_centroids_latlong <- block_centroids |>
  sf::st_transform(crs = sf::st_crs('EPSG:4326')) |>
  sf::st_coordinates()
block_centroids$x <- block_centroids_latlong[, 'X']
block_centroids$y <- block_centroids_latlong[, 'Y']


# Get (approximate) average population density across neighboring blocks
block_buffers <- sf::st_buffer(block_centroids[, 'id'], dist = units::set_units(100, 'm'))
nearby_blocks <- sf::st_join(
  x = block_centroids[, c('id','total_pop','area_km2')],
  y = block_buffers,
  join = st_intersects
) |> as.data.frame()
nearby_blocks$geom <- NULL
nearby_blocks <- nearby_blocks[nearby_blocks$id.x != nearby_blocks$id.y, ]
nearby_summed <- aggregate(
  x = list(total_pop = nearby_blocks$total_pop, area_km2 = nearby_blocks$area_km2),
  by = list(id = nearby_blocks$id.y),
  FUN = sum
)
nearby_summed$neighboring_density <- nearby_summed$total_pop / nearby_summed$area_km2
block_centroids <- merge(
  x = block_centroids,
  y = nearby_summed[, c('id', 'neighboring_density')],
  by = c('id')
)

# Prepare the analysis dataset
full_dataset <- as.data.frame(block_centroids)
full_dataset$geometry <- NULL

# Normalize all numeric predictors
numeric_predictors <- c('x','y','restaurants','coffee','supermarkets','neighboring_density')
for(var in numeric_predictors){
  full_dataset[[var]] <- (
    (full_dataset[[var]] - mean(full_dataset[[var]])) / sd(full_dataset[[var]])
  )
}

# Split the L_HOOD variable into dummy variables (0/1) by large neighborhood
full_dataset$LH_ <- make.names(full_dataset$L_HOOD)
neighborhood_model_matrix <- stats::model.matrix(~ 0 + LH_, data = full_dataset) |>
  as.data.frame()
full_dataset <- cbind(full_dataset, neighborhood_model_matrix)
all_predictors <- c(numeric_predictors, colnames(neighborhood_model_matrix))
predictors_formula <- paste('pop_density ~', paste(all_predictors, collapse = ' + '))

# Split into training and test datasets
train_index <- caret::createDataPartition(full_dataset$L_HOOD, p = .8, list = F, times = 1)
training_data <- full_dataset[train_index, ]
test_data <- full_dataset[-train_index, ]


## COMPARE PREDICTIVE PERFORMANCE OF SEVERAL ML MODELS ---------------------------------->

# For more information about regression models available in caret:
# https://topepo.github.io/caret/available-models.html
#
# Some model options, approximately ordered by run times:
#   - 'lm': Linear regression
#   - 'ridge', 'enet': Penalized regression
#   - 'rpart': Regression trees
#   - 'glm': Generalized linear models
#   - 'gam': Generalized additive model
#   - 'gbm': Stochastic gradient boosting
#   - 'treebag': Bagged regression trees
#   - 'svmLinear', 'svmLinear2', 'svmPoly', 'svmRadial': support vector machines
#   - 'xgbLinear', 'xgbDART', 'xgbTree': XGBoost
#   - 'rf': Random forest
#   - 'bstTree': Boosted regression trees
#
# Some models have tuning parameters where you can override the defaults. For more
#  information, see caret::modelLookup()

model_type <- 'xgbDART'

# Run a regression model on the training data
model_fit <- caret::train(
  form = as.formula(predictors_formula),
  data = training_data,
  method = model_type,
  trControl = caret::trainControl(method = 'cv', number = 5)
) |> suppressWarnings()

# See how well the model predicts the remaining 20% of data
training_data$in_sample <- predict(model_fit, newdata = training_data) |> suppressWarnings()
test_data$predictions <- predict(model_fit, newdata = test_data) |> suppressWarnings()

# Get in-sample predictive metrics
is_r_squared <- cor(training_data$pop_density, training_data$in_sample)**2
is_rmse <- (training_data$pop_density - training_data$in_sample)**2 |> mean(na.rm=T) |> sqrt()

# Get out-of-sample predictive metrics
oos_r_squared <- cor(test_data$pop_density, test_data$predictions)**2
oos_rmse <- (test_data$pop_density - test_data$predictions)**2 |> mean(na.rm = T) |> sqrt()
message(
  "Model performance for ", model_type, ": ",
  "\n  In-sample RMSE: ", round(is_rmse, 0),
  "\n  In-sample R-squared: ", round(is_r_squared, 3),
  "\n  Out-of-sample RMSE: ", round(oos_rmse, 0),
  "\n  Out-of-sample R squared: ", round(oos_r_squared, 3)
)

# Plot the results
results_plot <- ggplot(data = test_data, aes(x = pop_density, y = predictions)) +
  geom_point(aes(color = L_HOOD)) +
  geom_abline(intercept = 0, slope = 1, color = '#222222', linetype = 3, linewidth = 1) +
  geom_smooth(method = 'loess', span = .3) +
  scale_x_continuous(trans = 'sqrt', labels = scales::comma) +
  scale_y_continuous(trans = 'sqrt', labels = scales::comma) +
  labs(
    title = paste('Out-of-sample performance for model type:', toupper(model_type)),
    subtitle = paste('RMSE:', scales::comma(oos_rmse), '| R-squared:', round(oos_r_squared, 3)),
    x = 'Population density (actual)',
    y = 'Population density (OOS predictions)',
    color = 'Neighborhood'
  ) +
  guides(color = guide_legend(ncol = 2)) +
  theme_bw()
print(results_plot) |> suppressWarnings()
