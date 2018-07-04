#' Take an osmdata object and return a single table that aggregates all Points of Interest as single POINTS.
#' Corrupt POLYGONS and MULTIPOLYGONS must be fixed before undergoing such manipulation, but we leave the possibility to the user to do it on its own by adjusting the `fix_polygons` parameter.

aggregate_points <- function(osmdata_object, fix_polygons = T) {
  
  if(fix_polygons) {
    # Fix polygons geometry to be able to later compute the centroids
    osmdata_object$osm_polygons <- fix_polygons(osmdata_object$osm_polygons)
    osmdata_object$osm_multipolygons <- fix_polygons(osmdata_object$osm_multipolygons)
  }
  
  poi <- list(osmdata_object$osm_points,
              osmdata_object$osm_lines,
              osmdata_object$osm_polygons,
              osmdata_object$osm_multilines,
              osmdata_object$osm_multipolygons)
  
  # Clean-up list
  # Remove NULL's
  poi <- poi[!sapply(poi, is.null)]
  # Remove empty tables
  poi <- poi[as.logical(sapply(poi, nrow))]
  
  if(length(poi) > 0) {
    crs = st_crs(poi[[1]])[[1]]
  } else {
    stop('POI list is empty. Try another query.')
  }
  
  poi <- lapply(poi, create_osm_id)
  centroids <- lapply(poi, compute_centroids)
  
  # Bind
  centroids <- data.table::rbindlist(centroids)
  
  poi <- lapply(poi, sf::st_set_geometry, NULL)
  poi <- lapply(poi, data.table::as.data.table)
  
  final <- lapply(poi, merge, centroids, by = 'osm_id')
  final <- data.table::rbindlist(final, fill = TRUE)
  final <- sf::st_as_sf(final, coords = c('lon', 'lat'), crs = crs)
  
  return(final)
  
}

#' Compute centroids of a given (multi)polygon sf collection
#'
#' Given a collection of sf polygons or multipolygons, the centroid of these geometrical shapes is computed thanks to \code{\link[sf]{st_centroid}}. A data.frame containing the coordinates in longitude/latitude of these centroids is returned, after the coordinates have been projected according to a given CRS. This is to ensure that the centroids are not computed starting from lon/lat description, which is never accurate.
#'
#' @usage
#' compute_centroids(sf_df)
#' @param sf_df An sf-extended data.frame
#' @return \code{osmdata} object centroids
#' @import sf
#' @examples
#' nc <- sf::st_read(system.file("shape/nc.shp", package="sf"))
#' compute_centroids(nc, crs = 32617)

compute_centroids <- function(sf_df) {
  
  if(!all(class(sf_df) == c('sf', 'data.frame'))) {
    stop('Not a sf-data.frame')
  } else {
    # CHECK IF OSM_ID COLUMN EXISTS
    # Do nothing
  }
  
  if(!is.null(sf_df) & nrow(sf_df) > 0) {
    
    crs <- get_utm_crs(sf_df)
    sf_df <- st_transform(x = sf_df, crs = crs)
    cntr <- st_centroid(st_geometry(sf_df))
    # Back to longitude and latitude degrees values
    cntr <- st_transform(x = cntr, crs = 4326)
    
    cntr <- data.frame(osm_id = sf_df$osm_id, st_coordinates(cntr))
    data.table::setnames(cntr, old = c("X", "Y"), new = c("lon", "lat"))
    
    return(cntr)
    
  } else {
    
    return(sf_df)
    
  }
}

#' Take a sf data.frame and create an osm_id column based on the row names.
#'
#' @param df An sf data.frame of selected POI's
#' @return The same df with an additional column called osm_id corresponding to the input's rownames.
#' @import sf

create_osm_id <- function(df) {
  
  # Since it is an sf-extended df, make sure the number of columns is adequate
  # aka there must be at least 1 feature next to the geometry at any moment
  n_of_col <- ncol(df)
  if('osm_id' %in% colnames(df)) {
    n_of_col <- n_of_col - 1
  }
  if(n_of_col == 1) {
    df <- df %>% dplyr::mutate(dummy = -1)
  }
  
  ## Remove osm_id column if it already exists
  df <- df[, !names(df) %in% 'osm_id']
  ## Recreate osm_id column from rownames
  df <- tibble::rownames_to_column(tibble::as.tibble(df), var = "osm_id")
  df <- st_sf(as.data.frame(df))
  
  ## Cleanup
  if('dummy' %in% colnames(df)) {
    df$dummy <- NULL
  }
  
  return(df)
  
}

#' Fix polygons by removing or healing ill-defined sf objects
#'
#' This functions provides standard fixes to (multi)polygons directly
#' extracted from an OpenStreetMap server. The principal function used is
#' \code{\link[lwgeom]{st_make_valid}}, which solves most of the usual problems
#' (intersections, invalid shapes, etc.). Healed objects can then be used
#' for geometrical or logical operations between polygons, such as intersections,
#' substractions, etc. Empty polygons are simply removed as they don't contain any
#' vertices.
#'
#' @usage
#' fix_polygons(sf_df)
#' @param sf_df A collection of \code{osmdata} objects of a single type (see \pkg{osmdata} package)
#' @return \code{osmdata} object with healthy sf (multi)polygons
#' @import sf
#' @examples
#' kbn <- get_city_boundary(name = 'Copenhagen', adm_level = 7)
#' kbn <- fix_polygons(kbn)

fix_polygons <- function(sf_df) {
  
  if(!all(class(sf_df) == c('sf', 'data.frame'))) {
    stop('Not a sf-data.frame')
  } else if(!any((st_geometry_type(sf_df)) %in% c('POLYGON', 'MULTIPOLYGON'))) {
    if(is.null(sf_df) | nrow(sf_df) == 0) {
      warning('Object is empty. Doing nothing.')
      return(sf_df)
    } else {
      warning('Not a POLYGON or MULTIPOLYGON data.frame. Doing nothing.')
      return(sf_df)
    }
  } else if(!is.null(sf_df) & nrow(sf_df) > 0) {
    
    # Remove only the empty polygons and multipolygons for every city
    # as nothing can be extracted or computed or mapped from those
    empty_sf_df <- st_dimension(st_geometry(sf_df), NA_if_empty = TRUE)
    # To take into consideration!
    # test <- st_is_empty(sf_df)
    sf_df <- sf_df[!is.na(empty_sf_df), ]
    
    # Fix broken shapes
    # Mute warnings for st_is_valid (unsure if it's a good idea)
    oldw <- getOption("warn")
    options(warn = -1)
    is_valid <- st_is_valid(sf_df)
    options(warn = oldw)
    if(!all(is_valid)) {
      ## In rare cases is_valid returns NA
      sf_df <- sf_df[!is.na(is_valid), ]
      is_valid <- is_valid[!is.na(is_valid)]
      sf_df[!is_valid, ] <- lwgeom::st_make_valid(sf_df[!is_valid, ])
    }
    
    # Allows redetermining the geometry type
    sf_df <- st_sf(sf_df)
    
    return(sf_df)
    
  } else {
    
    warning('Object is empty. Doing nothing.')
    ## Do nothing and return same exact empty object
    return(sf_df)
    
  }
}

#' Get UTM CRS given a set of (lon, lat) coordinates.
#'
#' Get UTM CRS given a set of coordinates, regardless of their projection. It first reprojects the coordinates in (lon, lat) values, then computes the average of these coordinates, and eventually returns the corresponding UTM CRS. This method has been inspired from the OSMN Python package.
#'
#' @param dat points stored in an sf-extended data.frame
#' @return The UTM CRS for the set of points provided in input
#' @import sf
#' @importFrom magrittr %>%
#' @examples
#' nc <- sf::st_read(system.file("shape/nc.shp", package="sf"))
#' pts <- sf::st_centroid(nc)
#' get_utm_crs(pts)

get_utm_crs <- function(dat) {
  
  ## To determine UTM CRS, first compute average longitude of osm_points
  if(st_crs(dat)$epsg != 4326) {
    dat <- st_transform(dat, crs = 4326)
  }
  
  ## Extra step if dat is not only made of points
  if(!all(st_is(dat, 'POINT'))) {
    oldw <- getOption("warn")
    options(warn = -1)
    dat <- st_cast(head(st_geometry(dat), 1000), 'POINT')
    options(warn = oldw)
  }
  avg_lon <- st_coordinates(head(dat, 1000))[, 1] %>% mean
  crs <- paste0(326, as.integer(floor((avg_lon + 180) / 6.) + 1)) %>% as.integer
  
  return(crs)
  
}