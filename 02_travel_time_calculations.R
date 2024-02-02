## #######################################################################################
##
## 02) CALCULATING TRAVEL TIMES BY BLOCK
##
## #######################################################################################

library(data.table)
library(sf)

# Set Java allowed memory before loading R5R
java_memory_gb <- 2
options(java.parameters = paste0("-Xmx", java_memory_gb, "G"))
library(r5r)

## Load input data
seattle_blocks <- sf::st_read("MaptimeSEA Tutorial 2024-02/seattle_block_centroids.gpkg")

seattle_destinations <- sf::st_read("MaptimeSEA Tutorial 2024-02/seattle_destinations.gpkg")

## Set up R5 with the OpenStreeMap data
## This may take several minutes on the first setup
r5r_core <- r5r::setup_r5(data_path = "MaptimeSEA Tutorial 2024-02")


## RUN TRAVEL TIME ANALYSIS ------------------------------------------------------------->

# Calculate travel times to all origin-destination pairs within a 30 minute walk
block_centroids <- sf::st_centroid(seattle_blocks, of_largest_polygon = TRUE)

travel_times_list <- list()

tictoc::tic("Calculating all travel times")
for(o_type in c('Restaurants', 'Coffee shops', 'Supermarkets')){
  travel_times_by_od_pair <- r5r::travel_time_matrix(
    r5r_core = r5r_core,
    origins = block_centroids,
    destinations = seattle_destinations[type == o_type, ],
    mode = "WALK",
    max_trip_duration = 30L
  )
  travel_times_list[[o_type]] <- travel_times_by_od_pair[
    , .(travel_time = min(travel_time_p50, na.rm=T)),
    by = .(id = from_id)
  ][, type := o_type ]
}
tictoc::toc()

## Summarize results, including missing results
travel_times_table <- data.table::rbindlist(travel_times_list)
travel_times_long <- (
  data.table::CJ(id = origins$id, type = opportunity_types, travel_time = NA_real_ )
  [origins, GEOID := i.GEOID, on = 'id']
  [travel_times_table, travel_time := i.travel_time, on = c('id', 'type')]
  [order(type, as.numeric(id))]
)


## TO DO: VISUALIZE BY BLOCK!

