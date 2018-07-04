# BEFORE EXECUTING THIS SCRIPT 
# YOU HAVE TO TURN ON THE HTTP API
# osrm-routed map.osrm

library(curl)
library(jsonlite)
library(googlePolylines)

# Points of departure
source <- src_subset %>% filter(id == 1877) %>%
  st_transform(crs = 3857) %>% st_centroid() %>%
  st_transform(crs = 4326) %>% st_coordinates()
destination <- poi[c(1282,2124,110,1261,646),] %>% st_coordinates()

# Retrieve route from origin to destination
GPStoRoute <- function(osrm_server, service, origin_lon, origin_lat,
                       destination_lon, destination_lat) {
  
  url <- paste0(osrm_server, '/route/v1/', service, '/', origin_lon, ",", origin_lat,
                ";", destination_lon, ",", destination_lat, "?overview=full")
  
  req <- curl_fetch_memory(url)
  content <- rawToChar(req$content) %>% fromJSON
  route <- decode(content$routes$geometry)[[1]] %>%
    st_as_sf(coords = c('lon', 'lat'))
  
  route <- do.call(c, st_geometry(route)) %>% st_cast('LINESTRING')
  st_sfc(route, crs = 4326)
}

# Retrieve duration from origin to destination (not actually used)
GPStoDuration <- function (osrm_server, service, origin_lon, origin_lat,
                           destination_lon, destination_lat) {
  
  url <- paste0(osrm_server, '/route/v1/', service, '/', origin_lon, ",", origin_lat,
                ";", destination_lon, ",", destination_lat, "?overview=full")
  
  req <- curl_fetch_memory(url)
  content <- rawToChar(req$content) %>% fromJSON
  duration <- content$routes$duration
  duration
}

# Examples
GPStoRoute('http://0.0.0.0:5000', 'walking', source[1], source[2],
           destination[4,1], destination[4,2])

GPStoDuration('http://0.0.0.0:5000', 'walking', source[1], source[2],
              destination[4,1], destination[4,2])

# Define area
bbox_milan_detail <- matrix(c(9.223, 45.4711, 9.231, 45.4747), nrow = 2)
# Download OSM tiles
milan_map_detail <- ggmap::get_map(location = bbox_milan_detail, source = 'stamen', maptype = 'watercolor', zoom = 17)

# Draw small pictures
drawRoute <- function(color = 'red3') {
  route <- lapply(1:5, function(i) {
    GPStoRoute('http://0.0.0.0:5000', 'walking', source[1], source[2],
               destination[i,1], destination[i,2])
  })
  duration <- lapply(1:5, function(i) {
    GPStoDuration('http://0.0.0.0:5000', 'walking', source[1], source[2],
                  destination[i,1], destination[i,2])
  })
  
  size_arrow <- 3.5
  alpha_arrow <- 0.6
  
  s <- ggmap::ggmap(milan_map_detail) +
    geom_point(data = data.frame(destination), aes(X,Y), shape = 22, size = 8, colour = 'black', fill = 'red3', stroke = 2) +
    geom_path(data = data.frame(st_coordinates(route[[1]])), aes(X, Y),
              inherit.aes = FALSE, colour = 'midnightblue', size = size_arrow, alpha = alpha_arrow,
              arrow = arrow(angle = 15, length = unit(0.2, 'inches'),
                            ends = "last", type = "closed")) +
    geom_path(data = data.frame(st_coordinates(route[[2]])), aes(X, Y), 
              inherit.aes = FALSE, colour = 'midnightblue', size = size_arrow, alpha = alpha_arrow,
              arrow = arrow(angle = 15, length = unit(0.2, 'inches'),
                            ends = "last", type = "closed")) +
    geom_path(data = data.frame(st_coordinates(route[[3]])), aes(X, Y),
              inherit.aes = FALSE, colour = 'midnightblue', size = size_arrow, alpha = alpha_arrow,
              arrow = arrow(angle = 15, length = unit(0.2, 'inches'),
                            ends = "last", type = "closed")) +
    geom_path(data = data.frame(st_coordinates(route[[4]])), aes(X, Y),
              inherit.aes = FALSE, colour = 'midnightblue', size = size_arrow, alpha = alpha_arrow,
              arrow = arrow(angle = 15, length = unit(0.2, 'inches'),
                            ends = "last", type = "closed")) +
    geom_path(data = data.frame(st_coordinates(route[[5]])), aes(X, Y),
              inherit.aes = FALSE, colour = 'midnightblue', size = size_arrow, alpha = alpha_arrow,
              arrow = arrow(angle = 15, length = unit(0.2, 'inches'),
                            ends = "last", type = "closed")) +
    scalebar(data=filter(src_subset, id==1877), dist = 100, dist_unit = 'm',
             dd2km = TRUE, model = 'WGS84', location = 'bottomright',
             st.size = 6, height = 0.1, st.dist = 0.14,
             st.bottom = FALSE,
             box.fill = c('grey25', 'white'),
             anchor = c(x = source[1]+0.001, y = source[2]-0.0027)) +
    theme_map(axis.line = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks = element_blank())
  
  s
}

drawRoute()