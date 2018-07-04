# Installing and loading the packages
if(!'pacman' %in% installed.packages()[,'Package']) {
  install.packages('pacman')
}

pacman::p_load(osmdata, sf, ggplot2, dplyr, data.table, viridis, tmaptools,
               ggmap, devtools)

# We need the dev version of ggsn
if('ggsn' %in% installed.packages()[,'Package']) {
  if(packageVersion('ggsn') <= 0.4) {
    devtools::install_github('oswaldosantos/ggsn')
  }
} else {
  devtools::install_github('oswaldosantos/ggsn')
}
library(ggsn)

# Uncomment to set custom overpass URL
# Otherwise it will use the standard one
# osmdata::set_overpass_url("http://40.68.166.16/api/interpreter")

# Load/Extract Milan boundaries
# Load custom function to extract conveniently city borders
source('R-scripts/get_city_boundary.R')
poly_name <- 'Milan8_poly.rds'
if(!file.exists(file.path('R-scripts/data', poly_name))) {
  poly_path <- file.path('R-scripts/data', poly_name)
  city <- get_city_boundary(name = 'Milan',
                            adm_level = '8')
  saveRDS(city, poly_path)
} else {
  city <- readRDS(file.path('R-scripts/data', poly_name))
}

# Create some kind of buffer to mitigate border effects
bbox <- sf::st_bbox(city)
bbox['xmin'] <- bbox['xmin'] - 0.1
bbox['ymin'] <- bbox['ymin'] - 0.1
bbox['xmax'] <- bbox['xmax'] + 0.1
bbox['ymax'] <- bbox['ymax'] + 0.1
bbox <- matrix(bbox, nrow=2) 

# Query for restaurants in our bounding box 
query <- bbox %>% osmdata::opq() %>%
  osmdata::add_osm_feature("amenity", "restaurant") %>%
  osmdata::osmdata_sf() %>% osmdata::unique_osmdata()

# Load custom function to better aggregate all POI's.
source('R-scripts/aggregate_points.R')
poi <- aggregate_points(query)

# Plot to check if everything is fine
plot(st_as_sfc(st_bbox(city)))
plot(st_union(city), add = T)
plot(poi['osm_id'], col = 'red', add = T)

# Load Urban Atlas
# Replace the path accordingly
ua <- sf::read_sf('R-scripts/data/Milan_Urban_Atlas/Shapefiles/IT002L2_MILANO_UA2012.shp')
sf::st_crs(ua) <- 3035

# Subset Urban Atlas to "city"
city <- sf::st_transform(city, crs = 3035)
ua_city_all <- ua[city,]

# Select the objects devoted to residential purposes
urban_indexes <- grep('urban fabric', unique(ua_city_all$ITEM2012))
urban_fabric <- unique(ua_city_all$ITEM2012)[urban_indexes]
ua_city <- ua_city_all %>% dplyr::filter(ITEM2012 %in% urban_fabric)

# Building the text file to be given to OSRM as an input
sources <- sf::st_transform(sf::st_centroid(ua_city), crs = 4326)
destinations <- poi

sources <- sf::st_coordinates(sources) %>% as.data.frame
sources <- sources %>% dplyr::mutate(type = 'S')

destinations <- sf::st_coordinates(destinations) %>% as.data.frame
destinations <- destinations %>% dplyr::mutate(type = 'D')

osrm_input <- data.table::rbindlist(list(sources, destinations))
osrm_input[, id := seq.int(nrow(osrm_input)) - 1]
data.table::setcolorder(osrm_input, c('id', 'type', 'X', 'Y'))

write.table(osrm_input, 'R-scripts/data/osrm-input.txt', sep = ' ',
            row.names = FALSE, col.names = FALSE, quote = FALSE)

####
# Before proceeding, compile and run OSRM C++ script and get output from it
####

# Load with data.table for better performance
data <- data.table::fread('osrm-application/data/osrm-output.csv')

# Compute average duration for 5 closer destinations
avg <- data[, .(avg = mean(head(sort(V2), 5))), by = V1]
setnames(avg, 'V1', 'id')
# Convert seconds to minutes
avg[, avg := avg/60]

# Add index to city polygons to merge the respective averages into the table
ua_city <- ua_city %>% dplyr::mutate(id = seq.int(nrow(ua_city)) - 1)
src <- merge(ua_city, avg, by = 'id')
src <- src %>% st_transform(crs = 4326)

# Define Milan area for the final picture
xmin <- 9.1742
ymin <- 45.4511
xmax <- 9.2427
ymax <- 45.492

milan_pol = sf::st_sf(sf::st_sfc(
  sf::st_polygon(list(cbind(c(xmin, xmax, xmax, xmin, xmin),
                            c(ymin, ymin, ymax, ymax, ymin))))), crs = 4326)
src_subset <- sf::st_intersection(src, milan_pol)
plot(src_subset['avg'])


# Define ggplot2 theme
# Credits: https://timogrossenbacher.ch/2016/12/beautiful-thematic-maps-with-ggplot2-only
theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Liberation Sans", color = "#22211d"),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#f5f5f2", color = NA), 
      panel.background = element_rect(fill = "#f5f5f2", color = NA), 
      legend.background = element_rect(fill = "#f5f5f2", color = NA),
      panel.border = element_rect(colour = 'black', fill = NA, size = 1),
      ...
    )
}


# Define pretty breaks
pretty_breaks <- c(1, 2, 5, 10, 15)
# Find the extremes
minVal <- min(src_subset$avg, na.rm = T)
maxVal <- max(src_subset$avg, na.rm = T)
# compute labels
labels <- c()
brks <- c(minVal, pretty_breaks, maxVal)
# round the labels (actually, only the extremes)
for(idx in 1:length(brks)){
  labels <- c(labels, round(brks[idx+1], 1))
}
labels <- labels[1:length(labels)-1]

# Breaks
src_subset$brks <- cut(src_subset$avg, 
                       breaks = brks, 
                       include.lowest = TRUE, 
                       labels = labels)

brks_scale <- levels(src_subset$brks)
labels_scale <- rev(brks_scale)

# Download OSM tiles for the map
milan_small <- c(xmin, ymin, xmax, ymax)
bbox_milan_small <- matrix(milan_small, ncol = 2)
if(!file.exists('R-scripts/data/milan_map.Rds')) {
  milan_map <- ggmap::get_map(location = bbox_milan_small, source = 'stamen', maptype = 'toner-hybrid', zoom = 16)
  saveRDS(milan_map, 'R-scripts/data/milan_map.Rds')
} else {
  milan_map <- readRDS('R-scripts/data/milan_map.Rds')
}

# Prepare data.frame for plotting POI's
poi_df <- data.frame(st_coordinates(poi))
poi_df <- poi_df %>% mutate(destination = 'restaurant')

# Define small rectangle coordinates
bbox_milan_detail <- matrix(c(9.223, 45.4711, 9.231, 45.4747), nrow = 2)
xmin_detail <- bbox_milan_detail[1,1]
xmax_detail <- bbox_milan_detail[1,2]
ymin_detail <- bbox_milan_detail[2,1]
ymax_detail <- bbox_milan_detail[2,2]

milan_detail_pol = sf::st_sf(sf::st_sfc(sf::st_polygon(
  list(cbind(c(xmin_detail, xmax_detail, xmax_detail, xmin_detail, xmin_detail),
             c(ymin_detail, ymin_detail, ymax_detail, ymax_detail, ymin_detail))))),
  crs = 4326)

# Draw the map
p <- ggmap::ggmap(milan_map) +
  geom_sf(data = src_subset, aes(fill = brks),
          alpha = 0.7,
          col = NA,
          # lwd = 0.1,
          inherit.aes = FALSE) +
  geom_sf(data=st_boundary(milan_detail_pol), inherit.aes = FALSE, colour = 'grey25', size = 1.5, linetype = 'solid') +
  geom_point(data = poi_df, aes(x = X, y = Y, colour = destination), size = 1, inherit.aes = FALSE) +
  scalebar(src_subset, dist = 0.5, dist_unit = 'km',
           dd2km = TRUE, model = 'WGS84', location = 'bottomright',
           st.size = 4, height = 0.01, st.dist = 0.014,
           st.bottom = FALSE, anchor =
             c(x = xmax - 0.0032, y = ymin + 0.0018),
           st.color = 'black', box.fill = c('grey25', 'white'),
           border.size = 0.5) +
  theme_map() +
  theme(legend.position = 'bottom', legend.box = 'vertical')

q <- p +
  scale_color_manual(
    values = 'red3', name = 'Restaurant', label = '',
    guide = guide_legend(direction = 'horizontal',
                         title.position = 'right',
                         override.aes = list(size = 4))) +
  scale_fill_manual(
    values = rev(viridis(6)),
    breaks = rev(brks_scale),
    name = "Average time (min)",
    drop = FALSE,
    labels = labels_scale,
    guide = guide_legend(
      direction = "horizontal",
      keyheight = unit(2, units = "mm"),
      keywidth = unit(70 / length(labels), units = "mm"),
      title.position = 'top',
      # I shift the labels around, the should be placed 
      # exactly at the right end of each legend key
      title.hjust = 0.5,
      label.hjust = 1,
      nrow = 1,
      byrow = T,
      # also the guide needs to be reversed
      reverse = T,
      label.position = "bottom"
    )
  ) +
  theme(legend.box = 'horizontal')

q
