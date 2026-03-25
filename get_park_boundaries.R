library(httr)
library(jsonlite)
library(tidyverse)

api_key <- "6Ks1tnBrm9d1NqXCd2VzVlylUm8ktGMSbBkljXn4"
or_else <- function(x, default = NA_character_) {
  if (is.null(x) || length(x) == 0) return(default)
  x
}

fetch_park_boundaries <- function(limit = 600) {
  url <- sprintf("https://developer.nps.gov/api/v1/parkboundaries?limit=%s&api_key=%s", limit, api_key)
  response <- GET(url)
  stop_for_status(response)
  payload <- fromJSON(content(response, "text", encoding = "UTF-8"), simplifyVector = FALSE)
  payload$data
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

boundaries_raw <- fetch_park_boundaries()

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
  distinct(park_code, .keep_all = TRUE)

dir.create("mapdata", showWarnings = FALSE)
write.csv(boundary_rows, "mapdata/parkboundaries.csv", row.names = FALSE)
message(sprintf("Saved %d park boundary extents to mapdata/parkboundaries.csv", nrow(boundary_rows)))