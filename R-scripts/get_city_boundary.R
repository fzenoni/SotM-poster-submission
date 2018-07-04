#' City boundary generation using osmdata nominatim query

get_city_boundary <- function(name, adm_level) {
  
  ## Extract administrative boundaries of region of interest
  ## typically 7 if county, 8 or 9 if smaller (check case by case)
  ## on http://wiki.openstreetmap.org/wiki/Tag:boundary%3Dadministrative
  bbox_roi <- osmdata::opq(bbox = name)
  
  admn_roi <- bbox_roi %>%
    osmdata::add_osm_feature(key = 'admin_level',
                             value = as.character(adm_level)) %>%
    osmdata::osmdata_sf(quiet = FALSE)
  
  ## TODO. Sometimes Nominatim does not respond as expected.
  ## Trying again solves the issue most of the time.
  ## We need a try-catch in order to repeat the query if needed.
  
  ## Extract the multipolygons
  city <- admn_roi$osm_multipolygons
  
  ## return the city boundary
  return(city)
  
}