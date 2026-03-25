library(httr)
library(jsonlite)
library(tidyverse)

api_key <- "6Ks1tnBrm9d1NqXCd2VzVlylUm8ktGMSbBkljXn4"
or_else <- function(x, default = NA_character_) {
  if (is.null(x) || length(x) == 0) return(default)
  x
}

fetch_park_boundaries <- function(limit = 600) {
  endpoints <- c("parkboundaries", "parks")
  last_error <- NULL
  
  for (endpoint in endpoints) {
    url <- sprintf("https://developer.nps.gov/api/v1/%s?limit=%s&api_key=%s", endpoint, limit, api_key)
    response <- tryCatch(GET(url), error = function(e) e)
    
    if (inherits(response, "error")) {
      last_error <- response$message
      next
    }
    
    if (status_code(response) == 404) {
      last_error <- sprintf("Not Found (HTTP 404) for endpoint '%s'", endpoint)
      next
    }
    
    stop_for_status(response)
    
    payload <- fromJSON(content(response, "text", encoding = "UTF-8"), simplifyVector = FALSE)
    data <- payload$data
    
    if (!is.null(data) && length(data) > 0) {
      message(sprintf("Fetched park boundary source data from endpoint: %s", endpoint))
      return(data)
    }
  }
  
  stop(sprintf("Unable to fetch park boundaries from NPS API. Last error: %s", or_else(last_error, "Unknown error")))
}

extract_coords_from_geometry <- function(geometry_raw) {
  if (is.null(geometry_raw) || is.na(geometry_raw) || !nzchar(geometry_raw)) {
    return(tibble(lng = numeric(), lat = numeric()))
  }
  
  parsed <- tryCatch(fromJSON(geometry_raw, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(parsed) || is.null(parsed$coordinates)) {
    return(tibble(lng = numeric(), lat = numeric()))
  }
  
  pull_pairs <- function(node) {
    if (is.null(node)) return(list())
    if (is.numeric(node) && length(node) >= 2) {
      return(list(c(as.numeric(node[[1]]), as.numeric(node[[2]]))))
    }
    if (is.list(node)) {
      return(unlist(lapply(node, pull_pairs), recursive = FALSE))
    }
    list()
  }
  
  pairs <- pull_pairs(parsed$coordinates)
  if (length(pairs) == 0) {
    return(tibble(lng = numeric(), lat = numeric()))
  }
  
  matrix_vals <- do.call(rbind, pairs)
  tibble(lng = as.numeric(matrix_vals[, 1]), lat = as.numeric(matrix_vals[, 2])) %>%
    filter(!is.na(lng), !is.na(lat))
}

load_point_extents <- function(path = "nps_all_coordinates.csv") {
  if (!file.exists(path)) {
    return(tibble(
      park_code = character(),
      min_lng_points = numeric(),
      min_lat_points = numeric(),
      max_lng_points = numeric(),
      max_lat_points = numeric()
    ))
  }
  
  read.csv(path, stringsAsFactors = FALSE) %>%
    transmute(
      park_code = toupper(as.character(park_code)),
      latitude = suppressWarnings(as.numeric(latitude)),
      longitude = suppressWarnings(as.numeric(longitude))
    ) %>%
    filter(!is.na(park_code), nzchar(park_code), !is.na(latitude), !is.na(longitude)) %>%
    group_by(park_code) %>%
    summarise(
      min_lng_points = min(longitude, na.rm = TRUE),
      min_lat_points = min(latitude, na.rm = TRUE),
      max_lng_points = max(longitude, na.rm = TRUE),
      max_lat_points = max(latitude, na.rm = TRUE),
      .groups = "drop"
    )
}

boundaries_raw <- fetch_park_boundaries()
point_extents <- load_point_extents()

boundary_rows <- map_dfr(boundaries_raw, function(item) {
  park_code <- or_else(item$parkCode, NA_character_)
  geometry_geojson <- or_else(item$geometryGeoJSON, "")
  centroid_lat <- suppressWarnings(as.numeric(or_else(item$latitude, NA_character_)))
  centroid_lng <- suppressWarnings(as.numeric(or_else(item$longitude, NA_character_)))
  
  coords <- extract_coords_from_geometry(geometry_geojson)
  
  if (nrow(coords) == 0 && is.finite(centroid_lng) && is.finite(centroid_lat)) {
    coords <- tibble(lng = centroid_lng, lat = centroid_lat)
  }
  
  if (nrow(coords) == 0 || is.na(park_code) || !nzchar(park_code)) {
    return(tibble())
  }
  
  tibble(
    park_code = toupper(park_code),
    min_lng = min(coords$lng, na.rm = TRUE),
    min_lat = min(coords$lat, na.rm = TRUE),
    max_lng = max(coords$lng, na.rm = TRUE),
    max_lat = max(coords$lat, na.rm = TRUE)
  )
}) %>%
  distinct(park_code, .keep_all = TRUE) %>%
  left_join(point_extents, by = "park_code") %>%
  mutate(
    has_single_point_box = (min_lng == max_lng) | (min_lat == max_lat),
    min_lng = if_else(has_single_point_box & !is.na(min_lng_points), min_lng_points, min_lng),
    min_lat = if_else(has_single_point_box & !is.na(min_lat_points), min_lat_points, min_lat),
    max_lng = if_else(has_single_point_box & !is.na(max_lng_points), max_lng_points, max_lng),
    max_lat = if_else(has_single_point_box & !is.na(max_lat_points), max_lat_points, max_lat)
  ) %>%
  select(park_code, min_lng, min_lat, max_lng, max_lat)

write.csv(boundary_rows, "parkboundaries.csv", row.names = FALSE)
message(sprintf("Saved %d park boundary extents to parkboundaries.csv", nrow(boundary_rows)))