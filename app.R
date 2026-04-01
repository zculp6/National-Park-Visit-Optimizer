library(shiny)
library(shinydashboard)
library(tidyverse)
library(leaflet)
library(sf)
library(geosphere)
library(igraph)
library(httr)
library(DT)
library(shinyWidgets)
library(jsonlite)
library(rvest)
library(dplyr)
library(stringr)
library(lubridate)
library(googleway)
library(htmltools)
library(shinyjs)
library(rsconnect)

google_api_key <- Sys.getenv("GOOGLE_MAPS_KEY")

normalize_park_code <- function(x) {
  x %>%
    str_replace_all("[^A-Za-z]", "") %>%
    str_to_upper() %>%
    str_trim()
}

normalize_park_name_key <- function(x) {
  x %>%
    str_to_lower() %>%
    str_replace_all("[\u2013\u2014]", "-") %>%
    str_replace_all("\\bst\\.?\\b", "saint") %>%
    str_replace_all("\\bu\\.?\\s*s\\.?\\b", " ") %>%
    str_replace_all("&", " and ") %>%
    str_replace_all("\\bnational\\s+parks?\\s+and\\s+preserves?\\b", " ") %>%
    str_replace_all("\\bnational\\s+park\\s+and\\s+preserves?\\b", " ") %>%
    str_replace_all("\\bnational\\s+park\\s+and\\s+reserve?s?\\b", " ") %>%
    str_replace_all("\\b(national\\s+park|national\\s+parks|national\\s+and\\s+state\\s+parks|state\\s+parks|preserve|preserves|reserve|reserves|np|n\\.p\\.)\\b", " ") %>%
    str_replace_all("\\bof\\b", " ") %>%
    str_replace_all("[^a-z0-9]+", " ") %>%
    str_squish()
}

resolve_boundary_code <- function(park_code, park_name = NA_character_, known_boundary_codes = NULL) {
  code_clean <- normalize_park_code(park_code)
  name_clean <- normalize_park_name_key(park_name)
  boundary_codes_clean <- known_boundary_codes %>%
    unlist(use.names = FALSE) %>%
    as.character() %>%
    normalize_park_code() %>%
    unique()
  
  boundary_name_to_code <- c(
    "glacier bay" = "glba",
    "katmai" = "katm",
    "kobuk valley" = "kova",
    "kings canyon" = "seki",
    "sequoia" = "seki",
    "sequoia and kings canyon" = "seki"
  )
  boundary_code_aliases <- c(
    "GLAC" = "GLBA",
    "KOBU" = "KOVA"
  )
  
  if (!is.na(name_clean) && nzchar(name_clean) && name_clean %in% names(boundary_name_to_code)) {
    canonical_code <- normalize_park_code(boundary_name_to_code[[name_clean]])
    if (length(boundary_codes_clean) == 0 || canonical_code %in% boundary_codes_clean) {
      return(canonical_code)
    }
  }
  
  if (!is.na(code_clean) && nzchar(code_clean)) {
    if (length(boundary_codes_clean) > 0) {
      if (code_clean %in% boundary_codes_clean) return(code_clean)
      if (code_clean %in% names(boundary_code_aliases)) {
        alias_code <- normalize_park_code(boundary_code_aliases[[code_clean]])
        if (alias_code %in% boundary_codes_clean) return(alias_code)
      }
    } else {
      return(code_clean)
    }
  }
  
  NA_character_
}

as_scalar_numeric <- function(x, default = NA_real_) {
  vals <- suppressWarnings(as.numeric(unlist(x, use.names = FALSE)))
  if (length(vals) == 0 || is.na(vals[[1]])) return(default)
  vals[[1]]
}

as_scalar_character <- function(x, default = "") {
  vals <- unlist(x, use.names = FALSE)
  if (length(vals) == 0) return(default)
  val <- as.character(vals[[1]])
  if (is.na(val) || !nzchar(val)) return(default)
  val
}

# Zip code data from: https://simplemaps.com/data/us-zips
# Other data from NPS API

###################
### Gas Cost Data
################### 

# Inside get_gas_prices.R
get_gas_prices <- function() {
  cache_file <- "gas_prices.rds"
  
  march_2026_data <- tibble(
    State = c(state.name, "District of Columbia"),
    Regular = c(3.42, 5.21, 3.15, 3.33, 5.48, 3.55, 3.88, 3.75, 3.65, 3.58, 
                5.12, 3.62, 3.48, 3.38, 3.40, 3.25, 3.35, 3.12, 3.95, 3.82, 
                3.92, 3.30, 3.45, 3.08, 3.42, 3.52, 3.44, 5.08, 3.92, 3.78, 
                3.18, 3.85, 3.55, 3.42, 3.45, 3.22, 5.35, 3.82, 3.90, 3.62, 
                3.45, 3.20, 5.15, 3.75, 3.98, 3.60, 5.25, 3.55, 3.48, 3.58, 3.50)
  )
  
  # 1. Check if the Cache exists
  if (file.exists(cache_file)) {
    file_info <- file.info(cache_file)
    
    cached_data <- readRDS(cache_file)
    return(cached_data %>% select(State, Regular))
  }
  
  # Fallback if no file exists
  return(march_2026_data)
}

###################
### Airport Data
###################

major_airports <- read.csv("us-airports.csv")
major_airports <- major_airports %>%
  filter(type == "large_airport" | type == "medium_airport") %>%
  select(ident, name, latitude_deg, longitude_deg, region_name, local_region, municipality, local_code) %>%
  rename(lat = latitude_deg) %>%
  rename(lon = longitude_deg) %>%
  mutate(iata_code = str_to_upper(trimws(local_code)))

# Add key territory airports that are not present in us-airports.csv
territory_airports <- tibble::tribble(
  ~ident, ~name, ~lat, ~lon, ~region_name, ~local_region, ~municipality, ~local_code, ~iata_code,
  "NSTU", "Pago Pago International Airport", -14.3310, -170.7100, "American Samoa", "AS", "Pago Pago", "PPG", "PPG",
  "TISX", "Henry E. Rohlsen Airport", 17.7019, -64.7986, "U.S. Virgin Islands", "VI", "St. Croix", "STX", "STX",
  "TIST", "Cyril E. King Airport", 18.3373, -64.9734, "U.S. Virgin Islands", "VI", "St. Thomas", "STT", "STT"
)

major_airports <- major_airports %>%
  bind_rows(territory_airports) %>%
  distinct(iata_code, .keep_all = TRUE)

territory_flights <- read.csv("us_territory_flights.csv", stringsAsFactors = FALSE) %>%
  mutate(
    airport_depart = str_to_upper(trimws(airport_depart)),
    airport_arrive = str_to_upper(trimws(airport_arrive)),
    airline = trimws(airline),
    num_layovers = suppressWarnings(as.numeric(num_layovers))
  ) %>%
  filter(!is.na(airport_depart), !is.na(airport_arrive), airport_depart != "", airport_arrive != "")

park_cost_data <- read.csv("park_cost.csv", stringsAsFactors = FALSE) %>%
  mutate(
    park_code = normalize_park_code(parkCode),
    entrance_fee_vehicle = suppressWarnings(as.numeric(Entrance_Fee_Per_Vehicle)),
    entrance_fee_person = suppressWarnings(as.numeric(Entrance_Fee_Per_Person)),
    entrance_fee_motorcycle = suppressWarnings(as.numeric(Entrance_Fee_Per_Motorcycle))
  ) %>%
  select(park_code, fullName, entrance_fee_vehicle, entrance_fee_person, entrance_fee_motorcycle, Cost_Description)

zip_codes <- read.csv("uszips.csv", stringsAsFactors = FALSE) %>%
  mutate(zip = str_pad(as.character(zip), width = 5, side = "left", pad = "0"))

# Find nearest airport to a location
find_nearest_airport <- function(lat, lon, airports = major_airports) {
  distances <- distHaversine(
    cbind(lon, lat),
    cbind(airports$lon, airports$lat)
  ) / 1000
  
  nearest_idx <- which.min(distances)
  return(list(
    airport = airports[nearest_idx, ],
    distance_km = distances[nearest_idx]
  ))
}

nearest_airport_cache <- new.env(parent = emptyenv())
osrm_route_cache <- new.env(parent = emptyenv())
find_nearest_airport_cached <- function(lat, lon, airports = major_airports) {
  key <- paste0(round(as.numeric(lat), 4), "_", round(as.numeric(lon), 4))
  if (exists(key, envir = nearest_airport_cache, inherits = FALSE)) {
    return(get(key, envir = nearest_airport_cache, inherits = FALSE))
  }
  result <- find_nearest_airport(lat, lon, airports = airports)
  assign(key, result, envir = nearest_airport_cache)
  result
}

estimate_final_cost <- function(dist_miles, airline_code, iata_dest, travel_date) {
  # 1. Base Variables
  base_fare <- 85
  rate_per_mile <- 0.12
  
  # 2. Extract Month for Seasonality
  travel_month <- month(travel_date)
  season_mult <- case_when(
    travel_month %in% c(12, 1) ~ 1.40,      # Winter Peaks
    travel_month %in% c(6, 7, 8) ~ 1.30,   # Summer Peaks
    travel_month %in% c(3, 4) ~ 1.20,      # Spring Break
    travel_month %in% c(2, 11) ~ 0.85,     # Low Season
    TRUE ~ 1.00                            # Shoulder Season
  )
  
  # 3. Airline Tier Multiplier
  air_mult <- case_when(
    airline_code %in% c("NK", "F9", "G4") ~ 0.75, # Budget
    airline_code %in% c("DL", "UA", "AA") ~ 1.15, # Legacy
    TRUE ~ 1.00
  )
  
  # 4. Territory/Island Premium
  territory_mult <- if_else(iata_dest %in% c("STT", "STX", "PPG"), 1.35, 1.0)
  
  # 5. Annual Inflation (Relative to 2025)
  travel_year <- year(travel_date)
  inflation_mult <- 1 + ((travel_year - 2025) * 0.04) # Assume 4% yearly rise
  
  # FINAL CALCULATION
  final_cost <- (base_fare + (dist_miles * rate_per_mile)) * season_mult * air_mult * territory_mult * inflation_mult
  
  return(round(final_cost, 2))
}

airline_name_to_code <- c(
  "alaska airlines" = "AS",
  "allegiant air" = "G4",
  "american airlines" = "AA",
  "delta air lines" = "DL",
  "frontier airlines" = "F9",
  "jetblue" = "B6",
  "jetblue airways" = "B6",
  "southwest airlines" = "WN",
  "spirit airlines" = "NK",
  "united airlines" = "UA",
  "any" = "ANY"
)

estimate_flight_segment_cost <- function(origin_iata, dest_iata, dist_km, travel_date, preferred_airlines = NULL) {
  origin_code <- str_to_upper(trimws(origin_iata))
  dest_code <- str_to_upper(trimws(dest_iata))
  dist_miles <- dist_km * 0.621371
  flights <- territory_flights %>%
    filter(
      (airport_depart == origin_code & airport_arrive == dest_code) |
        (airport_depart == dest_code & airport_arrive == origin_code)
    )
  
  preferred_clean <- preferred_airlines %>%
    unlist(use.names = FALSE) %>%
    as.character() %>%
    trimws()
  preferred_clean <- preferred_clean[preferred_clean != "" & preferred_clean != "Any"]
  
  if (length(preferred_clean) > 0 && nrow(flights) > 0) {
    preferred_matches <- flights %>% filter(airline %in% preferred_clean)
    if (nrow(preferred_matches) > 0) flights <- preferred_matches
  }
  
  if (nrow(flights) > 0) {
    chosen <- flights %>%
      mutate(num_layovers = if_else(is.na(num_layovers), 0, num_layovers)) %>%
      arrange(num_layovers, airline) %>%
      slice(1)
    airline_name <- as_scalar_character(chosen$airline, "Any")
    airline_code <- airline_name_to_code[[str_to_lower(airline_name)]]
    if (is.null(airline_code) || !nzchar(airline_code)) airline_code <- "ANY"
    layover_mult <- 1 + (0.12 * as_scalar_numeric(chosen$num_layovers, default = 0))
    estimated_cost <- estimate_final_cost(
      dist_miles = dist_miles,
      airline_code = airline_code,
      iata_dest = dest_code,
      travel_date = travel_date
    ) * layover_mult
    return(list(
      cost = round(estimated_cost, 2),
      airline = airline_name,
      layovers = as_scalar_numeric(chosen$num_layovers, default = 0),
      matched = TRUE
    ))
  }
  
  estimated_cost <- estimate_final_cost(
    dist_miles = dist_miles,
    airline_code = "ANY",
    iata_dest = dest_code,
    travel_date = travel_date
  )
  list(cost = round(estimated_cost, 2), airline = "Estimated", layovers = 0, matched = FALSE)
}

###################
### Load Data
###################

# Data from https://www.kaggle.com/datasets/thedevastator/the-united-states-national-parks?select=df_2.csv

load_park_data <- function() {
  df <- read.csv("df_2.csv", stringsAsFactors = FALSE)
  park_details <- read.csv("nps_park_details.csv", stringsAsFactors = FALSE)
  park_images <- read.csv("national_parks_images.csv", stringsAsFactors = FALSE)
  visit_stats <- read.csv("Public Use Statistics.csv", stringsAsFactors = FALSE, check.names = FALSE)
  
  extract_states <- function(location_string) {
    state_match <- str_extract(location_string, "^[A-Za-z\\s,]+(?=\\d)")
    if (is.na(state_match)) return(NA_character_)
    str_split(str_trim(state_match), ",\\s*")[[1]]
  }
  
  extract_coordinates <- function(location_string) {
    coord_pattern <- "(\\d+\\.?\\d*)°([NS])\\s+(\\d+\\.?\\d*)°([EW])"
    matches <- str_match(location_string, coord_pattern)
    if (!is.na(matches[1])) {
      lat <- as.numeric(matches[2])
      if (matches[3] == "S") lat <- -lat
      lon <- as.numeric(matches[4])
      if (matches[5] == "W") lon <- -lon
      return(c(lat, lon))
    }
    return(c(NA, NA))
  }
  
  park_coords <- df %>%
    mutate(
      states = lapply(Location, extract_states),
      coords = t(sapply(Location, extract_coordinates)),
      latitude = coords[, 1],
      longitude = coords[, 2],
      date_established = Date.established.as.park.7..12. %>%
        str_replace_all("\\[.*?\\]", "") %>%
        str_trim() %>%
        str_replace("^[A-Za-z]+,\\s*", ""),
      area = Area..2021..13.,
      visitors = Recreation.visitors..2021..11.
    ) %>%
    select(name = Name, states, latitude, longitude,
           date_established, area, visitors) %>%
    filter(!is.na(latitude) & !is.na(longitude)) %>%
    mutate(
      name = ifelse(name == "Virgin Islands", "U.S. Virgin Islands", name),
      states = map_chr(states, ~ paste(ifelse(.x == "Virgin Islands", "U.S. Virgin Islands", .x), collapse = ", "))
    ) %>%
    unnest_longer(states, values_to = "state") %>%
    distinct(name, state, .keep_all = TRUE)
  
  latest_visit_year <- suppressWarnings(max(as.numeric(visit_stats$Year), na.rm = TRUE))
  latest_visit_totals <- visit_stats %>%
    transmute(
      park_code = normalize_park_code(UnitCode),
      visit_year = suppressWarnings(as.numeric(Year)),
      recreation_visits = as.numeric(gsub(",", "", RecreationVisits))
    ) %>%
    filter(
      !is.na(park_code), park_code != "",
      !is.na(visit_year), visit_year == latest_visit_year,
      !is.na(recreation_visits)
    ) %>%
    group_by(park_code) %>%
    summarise(
      visitors_latest_year = sum(recreation_visits, na.rm = TRUE),
      visitors_year = latest_visit_year,
      .groups = "drop"
    )
  
  phone_fmt <- function(x) {
    digits <- str_replace_all(x, "[^0-9]", "")
    ifelse(
      nchar(digits) == 10,
      sprintf("(%s)-%s-%s", substr(digits, 1, 3), substr(digits, 4, 6), substr(digits, 7, 10)),
      x
    )
  }
  
  first_non_missing <- function(x) {
    vals <- x[!is.na(x) & nzchar(trimws(as.character(x)))]
    if (length(vals) == 0) NA_character_ else as.character(vals[[1]])
  }
  
  details_clean <- park_details %>%
    mutate(
      name_key = normalize_park_name_key(Park_Name),
      park_code = normalize_park_code(Park_Code),
      phone = phone_fmt(Phone)
    ) %>%
    select(
      park_code,
      name_key,
      city = City,
      phone,
      email = Email,
      hours = Monday_Hours,
      website = Website_URL,
      description = Description
    ) %>%
    group_by(park_code, name_key) %>%
    summarise(
      city = first_non_missing(city),
      phone = first_non_missing(phone),
      email = first_non_missing(email),
      hours = first_non_missing(hours),
      website = first_non_missing(website),
      description = first_non_missing(description),
      .groups = "drop"
    )
  
  image_counts <- park_images %>%
    mutate(
      park_code = normalize_park_code(Park_Code),
      image_name_key = normalize_park_name_key(Park_Name)
    ) %>%
    group_by(park_code, image_name_key) %>%
    summarise(image_count = n(), .groups = "drop") %>%
    mutate(park_code = as.character(park_code))
  
  park_coords <- park_coords %>%
    mutate(name_key = normalize_park_name_key(name)) %>%
    left_join(details_clean, by = "name_key") %>%
    left_join(image_counts %>% select(park_code, image_count), by = "park_code") %>%
    left_join(
      image_counts %>% select(image_name_key, image_count_by_name = image_count),
      by = c("name_key" = "image_name_key")
    ) %>%
    mutate(image_count = coalesce(image_count, image_count_by_name, 0L))
  
  name_to_code_fallback <- tribble(
    ~name, ~fallback_code,
    "American Samoa", "NPSA",
    "Katmai", "KATM",
    "Glacier Bay", "GLBA",
    "Wrangell–St. Elias *", "WRST",
    "Denali", "DENA",
    "Lake Clark", "LACL",
    "Gates of the Arctic", "GAAR",
    "Redwood", "REDW",
    "Kings Canyon", "SEKI",
    "Sequoia", "SEKI",
    "Great Sand Dunes", "GRSA",
    "New River Gorge", "NERI",
    "U.S. Virgin Islands", "VIIS"
  )
  
  fallback_details <- details_clean %>%
    select(
      fallback_code = park_code,
      fallback_city = city,
      fallback_phone = phone,
      fallback_email = email,
      fallback_hours = hours,
      fallback_website = website,
      fallback_description = description
    )
  
  fallback_images <- image_counts %>%
    select(
      fallback_code = park_code,
      fallback_image_count = image_count
    ) %>%
    distinct(fallback_code, .keep_all = TRUE)
  
  park_coords <- park_coords %>%
    left_join(name_to_code_fallback, by = "name") %>%
    left_join(fallback_details, by = "fallback_code") %>%
    left_join(fallback_images, by = "fallback_code") %>%
    mutate(
      park_code = coalesce(park_code, fallback_code),
      city = coalesce(city, fallback_city),
      phone = coalesce(phone, fallback_phone),
      email = coalesce(email, fallback_email),
      hours = coalesce(hours, fallback_hours),
      website = coalesce(website, fallback_website),
      description = coalesce(description, fallback_description),
      image_count = coalesce(image_count, fallback_image_count, 0L)
    ) %>%
    left_join(latest_visit_totals, by = "park_code") %>%
    mutate(
      visitors = coalesce(visitors_latest_year, visitors),
      visitors_year = if_else(!is.na(visitors_latest_year), as.integer(visitors_year), 2021L),
      image_count = case_when(
        normalize_park_name_key(name) %in% c("sequoia", "kings canyon", "sequoia and kings canyon") ~ 5L,
        TRUE ~ as.integer(image_count)
      )
    ) %>%
    select(-starts_with("fallback_")) %>%
    select(-name_key, -image_count_by_name)
  
  return(park_coords)
}

# Load park outline/coordinate data
load_park_outline_data <- function() {
  normalize_park_code <- function(x) {
    x %>%
      str_replace_all("[^A-Za-z]", "") %>%
      str_to_upper() %>%
      str_trim()
  }
  
  if (file.exists("nps_all_coordinates.csv")) {
    coords <- read.csv("nps_all_coordinates.csv", stringsAsFactors = FALSE)
    coords %>%
      mutate(
        park_code = normalize_park_code(park_code),
        type = trimws(type)
      ) %>%
      filter(!is.na(latitude) & !is.na(longitude) & !is.na(park_code) & park_code != "")
  } else {
    # Return empty tibble if file doesn't exist
    tibble(
      park_code = character(),
      entity_name = character(),
      type = character(),
      latitude = numeric(),
      longitude = numeric()
    )
  }
}

load_park_point_data <- function() {
  park_points <- load_park_outline_data()
  if (nrow(park_points) == 0) return(park_points)
  
  if (!file.exists("nps_park_details.csv")) return(park_points)
  
  park_details <- read.csv("nps_park_details.csv", stringsAsFactors = FALSE) %>%
    transmute(
      park_code = normalize_park_code(Park_Code),
      park_name = Park_Name
    ) %>%
    distinct(park_code, .keep_all = TRUE)
  
  park_points %>%
    left_join(park_details, by = "park_code")
}

load_park_boundaries <- function(path = "parkboundaries.csv") {
  if (!file.exists(path)) {
    return(tibble(
      park_code = character(),
      min_lng = numeric(),
      min_lat = numeric(),
      max_lng = numeric(),
      max_lat = numeric()
    ))
  }
  
  read.csv(path, stringsAsFactors = FALSE) %>%
    mutate(
      park_code = normalize_park_code(park_code),
      min_lng = as.numeric(min_lng),
      min_lat = as.numeric(min_lat),
      max_lng = as.numeric(max_lng),
      max_lat = as.numeric(max_lat)
    ) %>%
    filter(
      !is.na(park_code), park_code != "",
      !is.na(min_lng), !is.na(min_lat), !is.na(max_lng), !is.na(max_lat)
    ) %>%
    distinct(park_code, .keep_all = TRUE)
}

load_park_boundary_shapes <- function(path = "all_park_boundaries.json") {
  if (!file.exists(path)) {
    return(tibble(
      park_code = character(),
      park_name = character(),
      boundary_coords = list(),
      boundary_paths = list(),
      min_lng = numeric(),
      min_lat = numeric(),
      max_lng = numeric(),
      max_lat = numeric()
    ))
  }
  
  boundary_json <- fromJSON(path, simplifyVector = FALSE)
  
  mercator_to_wgs84 <- function(lng, lat) {
    lon <- (lng / 20037508.34) * 180
    lat_deg <- (atan(exp((lat / 20037508.34) * pi)) * 360 / pi) - 90
    tibble(lng = lon, lat = lat_deg)
  }
  
  normalize_boundary_crs <- function(coord_df, park_code = "") {
    if (is.null(coord_df) || nrow(coord_df) == 0) return(coord_df)
    valid <- coord_df %>% filter(!is.na(lng), !is.na(lat))
    if (nrow(valid) == 0) return(coord_df)
    looks_projected <- any(abs(valid$lng) > 180 | abs(valid$lat) > 90)
    if (!looks_projected) return(coord_df)
    converted <- mercator_to_wgs84(valid$lng, valid$lat)
    idx <- !is.na(coord_df$lng) & !is.na(coord_df$lat)
    coord_df$lng[idx] <- converted$lng
    coord_df$lat[idx] <- converted$lat
    warning(
      paste0(
        "Boundary coordinates for park code ", park_code,
        " appeared projected; converted from Web Mercator to WGS84."
      ),
      call. = FALSE
    )
    coord_df
  }
  
  flatten_geometry_coords <- function(geometry) {
    geom_type <- as_scalar_character(geometry$type, "")
    coords <- geometry$coordinates
    if (is.null(coords)) {
      return(tibble(lng = numeric(), lat = numeric()))
    }
    
    rows <- list()
    row_id <- 1
    append_ring <- function(ring_coords) {
      if (is.null(ring_coords) || length(ring_coords) == 0) return()
      ring_matrix <- do.call(rbind, ring_coords)
      if (nrow(ring_matrix) == 0) return()
      rows[[row_id]] <<- tibble(
        lng = as.numeric(ring_matrix[, 1]),
        lat = as.numeric(ring_matrix[, 2])
      )
      row_id <<- row_id + 1
      rows[[row_id]] <<- tibble(lng = NA_real_, lat = NA_real_)
      row_id <<- row_id + 1
    }
    
    if (identical(geom_type, "Polygon")) {
      walk(coords, append_ring)
    } else if (identical(geom_type, "MultiPolygon")) {
      walk(coords, function(poly) walk(poly, append_ring))
    } else {
      return(tibble(lng = numeric(), lat = numeric()))
    }
    
    bind_rows(rows)
  }
  
  extract_boundary_paths <- function(geometry) {
    geom_type <- as_scalar_character(geometry$type, "")
    coords <- geometry$coordinates
    if (is.null(coords)) return(list())
    
    build_ring <- function(ring_coords) {
      if (is.null(ring_coords) || length(ring_coords) == 0) return(NULL)
      ring_matrix <- do.call(rbind, ring_coords)
      if (is.null(ring_matrix) || nrow(ring_matrix) == 0) return(NULL)
      tibble(
        lng = as.numeric(ring_matrix[, 1]),
        lat = as.numeric(ring_matrix[, 2])
      ) %>%
        filter(!is.na(lng), !is.na(lat))
    }
    
    if (identical(geom_type, "Polygon")) {
      outer <- build_ring(coords[[1]])
      return(compact(list(outer)))
    }
    
    if (identical(geom_type, "MultiPolygon")) {
      return(
        coords %>%
          map(~ build_ring(.x[[1]])) %>%
          compact()
      )
    }
    
    list()
  }
  
  boundary_tbl <- imap_dfr(boundary_json, function(code_entry, park_code) {
    if (is.null(code_entry$features) || length(code_entry$features) == 0) {
      return(NULL)
    }
    
    coords_df <- map_dfr(code_entry$features, function(feat) {
      flatten_geometry_coords(feat$geometry)
    })
    coords_df <- normalize_boundary_crs(coords_df, normalize_park_code(park_code))
    boundary_paths <- code_entry$features %>%
      map(~ extract_boundary_paths(.x$geometry)) %>%
      unlist(recursive = FALSE) %>%
      keep(~ is.data.frame(.x) && nrow(.x) > 2) %>%
      map(~ normalize_boundary_crs(.x, normalize_park_code(park_code)))
    valid_coords <- coords_df %>% filter(!is.na(lng), !is.na(lat))
    if (nrow(valid_coords) == 0 || length(boundary_paths) == 0) {
      return(NULL)
    }
    park_name <- as_scalar_character(code_entry$features[[1]]$properties$name, "")
    
    tibble(
      park_code = normalize_park_code(park_code),
      park_name = park_name,
      boundary_coords = list(coords_df),
      boundary_paths = list(boundary_paths),
      min_lng = min(valid_coords$lng, na.rm = TRUE),
      min_lat = min(valid_coords$lat, na.rm = TRUE),
      max_lng = max(valid_coords$lng, na.rm = TRUE),
      max_lat = max(valid_coords$lat, na.rm = TRUE)
    )
  })
  
  if (nrow(boundary_tbl) == 0) {
    return(tibble(
      park_code = character(),
      park_name = character(),
      boundary_coords = list(),
      boundary_paths = list(),
      min_lng = numeric(),
      min_lat = numeric(),
      max_lng = numeric(),
      max_lat = numeric()
    ))
  }
  
  boundary_tbl
}

categorize_park_feature <- function(type, entity_name) {
  raw_type <- ifelse(is.na(type), "", type)
  raw_name <- ifelse(is.na(entity_name), "", entity_name)
  merged <- str_to_lower(paste(raw_type, raw_name))
  
  if (str_detect(merged, "webcam")) return(NA_character_)
  if (str_detect(merged, "picnic")) return("Picnic Area")
  if (str_detect(merged, "bathroom|restroom|toilet")) return("Restroom")
  if (str_detect(merged, "visitor\\s*center")) return("Visitor Center")
  if (str_detect(merged, "campground")) return("Campground")
  if (str_detect(merged, "parking")) return("Parking")
  if (str_detect(merged, "trail")) return("Trail")
  
  "Additional Areas"
}

build_image_gallery_html <- function(park_name, image_df, park_code = NA_character_) {
  park_name_key <- normalize_park_name_key(park_name)
  park_code_key <- normalize_park_code(park_code)
  images_clean <- image_df %>%
    mutate(
      name_key = normalize_park_name_key(Park_Name),
      park_code_clean = normalize_park_code(Park_Code)
    )
  
  images <- if (!is.na(park_code_key) && nzchar(park_code_key)) {
    images_clean %>% filter(park_code_clean == park_code_key)
  } else {
    images_clean %>% filter(name_key == park_name_key)
  }
  
  images <- images %>%
    distinct(Image_URL, .keep_all = TRUE)
  
  if (nrow(images) == 0) {
    return("<em>No images available.</em>")
  }
  
  gallery_id <- paste0("gallery_", gsub("[^a-zA-Z0-9]", "_", park_name))
  slides <- paste0(sprintf(
    "<div class='gallery-slide %s' data-gallery='%s'><img src='%s' alt='%s' style='width:100%%;max-height:200px;object-fit:cover;border-radius:4px;'/><div style='font-size:12px;margin-top:4px;'><strong>%s</strong></div></div>",
    ifelse(seq_len(nrow(images)) == 1, "active", ""),
    gallery_id,
    images$Image_URL,
    ifelse(is.na(images$Image_Alt_Text), images$Image_Title, images$Image_Alt_Text),
    images$Image_Title
  ), collapse = "")
  
  paste0(
    "<div class='park-gallery' id='", gallery_id, "' style='width:100%;max-width:340px;'>",
    slides,
    "<div class='gallery-counter' data-gallery='", gallery_id, "' style='font-size:12px;text-align:center;margin-top:6px;'>1 of ", nrow(images), "</div>",
    "<div style='display:flex;justify-content:space-between;margin-top:6px;'>",
    "<button onclick=\"window.parkGalleryNav('", gallery_id, "', -1)\" style='padding:2px 8px;'>&larr;</button>",
    "<button onclick=\"window.parkGalleryNav('", gallery_id, "', 1)\" style='padding:2px 8px;'>&rarr;</button>",
    "</div></div>"
  )
}

build_park_popup <- function(park_row, image_df) {
  gallery_html <- build_image_gallery_html(park_row$name, image_df, park_row$park_code)
  modal_id <- paste0("gallery_modal_", gsub("[^a-zA-Z0-9]", "_", park_row$name))
  visitors_year <- ifelse(is.na(park_row$visitors_year), 2021, park_row$visitors_year)
  paste0(
    "<h4><b>", park_row$name, "</b></h4>",
    "<b>State/Territory:</b> ", park_row$state, "<br>",
    "<b>Date Established:</b> ", park_row$date_established, "<br>",
    "<b>Area:</b> ", park_row$area, "<br>",
    "<b>Visitors (", visitors_year, "):</b> ", format(park_row$visitors, big.mark = ",", scientific = FALSE), "<br>",
    "<b>City:</b> ", park_row$city, "<br>",
    "<b>Phone:</b> ", park_row$phone, "<br>",
    "<b>Email:</b> ", park_row$email, "<br>",
    "<b>Hours:</b> ", park_row$hours, "<br>",
    "<b>Website:</b> <a href='", park_row$website, "' target='_blank'>", park_row$website, "</a><br><br>",
    "<b>Description:</b><br>", park_row$description, "<br><br>",
    "<b>Gallery (", park_row$image_count, "):</b><br>",
    "<button class='btn btn-primary btn-sm' onclick=\"window.openParkGallery('", modal_id, "')\">View Gallery</button>",
    "<div id='", modal_id, "' class='park-gallery-modal' style='display:none;'>",
    "<div class='park-gallery-modal-content'>",
    "<button class='park-gallery-close' onclick=\"window.closeParkGallery('", modal_id, "')\">&times;</button>",
    "<h4>", park_row$name, " Gallery</h4>",
    gallery_html,
    "</div></div>"
  )
}

load_visit_time_defaults <- function() {
  visit_df <- read.csv("Public Use Statistics.csv", stringsAsFactors = FALSE, check.names = FALSE)
  visit_df %>%
    transmute(
      park_code = normalize_park_code(UnitCode),
      recreation_visits = as.numeric(gsub(",", "", RecreationVisits)),
      recreation_hours = as.numeric(gsub(",", "", RecreationHours))
    ) %>%
    filter(!is.na(park_code), park_code != "", recreation_visits > 0) %>%
    mutate(default_visit_hours = pmax(recreation_hours / recreation_visits, 0.5)) %>%
    group_by(park_code) %>%
    summarise(default_visit_hours = mean(default_visit_hours, na.rm = TRUE), .groups = "drop")
}

# Classify parks by region
classify_park_region <- function(state) {
  territories <- c("American Samoa", "U.S. Virgin Islands")
  
  if (is.null(state) || length(state) == 0 || is.na(state) || !nzchar(state)) return("mainland")
  if (state %in% territories) return("territory")
  if (state == "Hawaii" || grepl("Hawaii", state)) return("hawaii")
  if (state == "Alaska" || grepl("Alaska", state)) return("alaska")
  
  return("mainland")
}

# Geocode user location (simplified - using nominatim)
get_zip_coordinates <- function(zipcode, zip_data = zip_codes) {
  zip_clean <- str_extract(as.character(zipcode), "\\d{5}")
  if (is.na(zip_clean) || !nzchar(zip_clean)) return(NULL)
  zip_match <- zip_data %>% filter(zip == zip_clean) %>% slice(1)
  if (nrow(zip_match) == 0) return(NULL)
  list(
    latitude = as.numeric(zip_match$lat[[1]]),  # Change 'lat' to 'latitude'
    longitude = as.numeric(zip_match$lon[[1]]), # Change 'lon' to 'longitude'
    city = zip_match$city[[1]],
    state = zip_match$state_name[[1]],
    zip = zip_clean
  )
}

geocode_location <- function(zipcode) {
  tryCatch({
    zip_match <- get_zip_coordinates(zipcode)
    if (!is.null(zip_match)) {
      return(list(
        latitude = zip_match$latitude,   # Change 'lat' to 'latitude'
        longitude = zip_match$longitude, # Change 'lon' to 'longitude'
        source_city = zip_match$city,
        source_state = zip_match$state,
        source_zip = zip_match$zip,
        success = TRUE
      ))
    }
    return(list(success = FALSE, message = "Location not found."))
  }, error = function(e) {
    err_text <- conditionMessage(e)
    if (is.null(err_text) || !nzchar(err_text)) {
      err_text <- "Unexpected geocoding error."
    }
    return(list(success = FALSE, message = err_text))
  })
}

all_state_choices <- c(state.name, "U.S. Virgin Islands", "American Samoa")
penalty_value <- 1e9
scalar_has_text <- function(x) {
  is.character(x) && length(x) == 1 && !is.na(x) && nzchar(x)
}

default_speed_kmh <- 100
default_flight_speed_kmh <- 800
drive_threshold_km <- 1500

infer_route_mode <- function(dist_km, threshold_km = drive_threshold_km) {
  ifelse(dist_km < threshold_km, "Drive", "Flight")
}

infer_route_mode_for_segment <- function(lat1, lon1, state1, lat2, lon2, state2, threshold_km = drive_threshold_km) {
  territory_states <- c("U.S. Virgin Islands", "American Samoa")
  state1_clean <- trimws(as.character(state1))
  state2_clean <- trimws(as.character(state2))
  
  if (!is.na(state1_clean) && !is.na(state2_clean) &&
      ((state1_clean %in% territory_states) || (state2_clean %in% territory_states))) {
    return("Flight")
  }
  
  dist_km <- haversine_distance(lon1, lat1, lon2, lat2)
  infer_route_mode(dist_km, threshold_km = threshold_km)
}

drive_speed_kmh <- default_speed_kmh
flight_speed_kmh <- default_flight_speed_kmh

flight_fixed_time_hours <- 2

estimate_travel_time <- function(distance_km, mode, speed_drive = drive_speed_kmh, speed_flight = flight_speed_kmh) {
  travel_time <- ifelse(mode == "Drive", distance_km / speed_drive, distance_km / speed_flight + flight_fixed_time_hours)
  return(travel_time)
}

haversine_distance <- function(lon1, lat1, lon2, lat2) {
  distHaversine(c(lon1, lat1), c(lon2, lat2)) / 1000
}

build_road_dist_matrix <- function(locations, google_api_key) {
  n <- nrow(locations)
  dist_matrix <- matrix(0, nrow = n, ncol = n)
  rownames(dist_matrix) <- locations$fullName
  colnames(dist_matrix) <- locations$fullName
  
  # We use a nested loop to fill the matrix
  # For production, consider 'googleway' package for batching
  for (i in 1:n) {
    for (j in 1:n) {
      if (i == j) {
        dist_matrix[i, j] <- 0
      } else {
        origin <- paste(locations$latitude[i], locations$longitude[i], sep = ",")
        dest <- paste(locations$latitude[j], locations$longitude[j], sep = ",")
        
        url <- paste0("https://maps.googleapis.com/maps/api/distancematrix/json?origins=", 
                      origin, "&destinations=", dest, "&key=", google_api_key)
        
        res <- fromJSON(url)
        
        if (res$status == "OK") {
          # distance$value is in meters; convert to miles
          dist_matrix[i, j] <- res$rows$elements[[1]]$distance$value * 0.000621371
        } else {
          # Fallback to Haversine if Google fails
          dist_matrix[i, j] <- geosphere::distHaversine(
            c(locations$longitude[i], locations$latitude[i]),
            c(locations$longitude[j], locations$latitude[j])
          ) * 0.000621371
        }
      }
    }
  }
  return(dist_matrix)
}

compute_distance_matrix <- function(locations, mode_matrix = NULL, speed_drive = drive_speed_kmh, speed_flight = flight_speed_kmh, road_dist_miles = NULL) {
  n <- nrow(locations)
  dist_mat <- matrix(0, nrow = n, ncol = n)
  time_mat <- matrix(0, nrow = n, ncol = n)
  
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      if (i == j) next
      mode <- if (!is.null(mode_matrix)) {
        mode_matrix[i, j]
      } else {
        infer_route_mode_for_segment(
          lat1 = locations$latitude[i],
          lon1 = locations$longitude[i],
          state1 = locations$state[i],
          lat2 = locations$latitude[j],
          lon2 = locations$longitude[j],
          state2 = locations$state[j]
        )
      }
      dist_km <- if (!is.null(road_dist_miles) && identical(mode, "Drive")) {
        as.numeric(road_dist_miles[i, j]) / 0.621371
      } else {
        haversine_distance(
          locations$longitude[i], locations$latitude[i],
          locations$longitude[j], locations$latitude[j]
        )
      }
      time_hours <- estimate_travel_time(dist_km, mode, speed_drive, speed_flight)
      
      dist_mat[i, j] <- dist_km
      time_mat[i, j] <- time_hours
    }
  }
  
  list(distance = dist_mat, time = time_mat)
}

build_mode_matrix <- function(locations, distance_type = "mixed") {
  n <- nrow(locations)
  mode_matrix <- matrix("Auto", nrow = n, ncol = n)
  
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      if (i == j) next
      mode_matrix[i, j] <- if (distance_type == "driving") {
        "Drive"
      } else if (distance_type == "flying") {
        "Flight"
      } else {
        infer_route_mode_for_segment(
          lat1 = locations$latitude[i],
          lon1 = locations$longitude[i],
          state1 = locations$state[i],
          lat2 = locations$latitude[j],
          lon2 = locations$longitude[j],
          state2 = locations$state[j]
        )
      }
    }
  }
  
  mode_matrix
}

enforce_endpoint_order <- function(path, start_idx = NULL, end_idx = NULL) {
  if (is.null(path) || length(path) == 0) return(path)
  
  ordered <- path
  n <- length(ordered)
  
  # Case 1: Start and End are specified and different (One-Way Trip)
  if (!is.null(start_idx) && !is.null(end_idx) && start_idx != end_idx) {
    if (ordered[1] == end_idx && ordered[n] == start_idx) {
      return(rev(ordered))
    }
  }
  
  # Case 2: Round Trip (Start and End are the same)
  if (!is.null(start_idx) && (is.null(end_idx) || start_idx == end_idx)) {
    start_pos <- which(ordered == start_idx)[1]
    if (!is.na(start_pos)) {
      ordered <- c(ordered[start_pos:n], ordered[seq_len(start_pos - 1)])
    }
  }
  
  ordered
}

tsp_solver <- function(dist_matrix, start_idx = NULL, end_idx = NULL, num_stops = NULL, time_matrix = NULL, segment_mode = "distance", max_segment_value = Inf, overall_time_limit = Inf, overall_distance_limit = Inf, mode_matrix = NULL, node_visit_times = NULL) {
  n <- nrow(dist_matrix)
  penalty_value <- 1e9
  
  if (!is.null(start_idx)) stopifnot(start_idx >= 1 && start_idx <= n)
  if (!is.null(end_idx)) stopifnot(end_idx >= 1 && end_idx <= n)
  if (!is.null(num_stops)) stopifnot(num_stops >= 1 && num_stops <= n)
  
  dist_adj <- dist_matrix
  time_adj <- if (!is.null(time_matrix)) time_matrix else dist_matrix
  
  # 1. Apply segment constraint penalty
  constraint_matrix <- if (segment_mode == "time") time_adj else dist_adj
  if (is.finite(max_segment_value) && max_segment_value > 0) {
    dist_adj <- ifelse(constraint_matrix > max_segment_value, dist_matrix + penalty_value, dist_matrix)
  }
  
  # 2. The 2-Opt Solver with rigidly locked endpoints
  tsp_result <- tryCatch({
    tour <- lapply(1:20, function(seed) { 
      set.seed(seed)
      
      middle_nodes <- setdiff(seq_len(n), c(start_idx, end_idx))
      
      if (!is.null(start_idx) && !is.null(end_idx) && start_idx != end_idx) {
        path <- c(start_idx, sample(middle_nodes), end_idx)
        min_swap <- 2          
        max_swap <- n - 1      
      } else if (!is.null(start_idx)) {
        path <- c(start_idx, sample(middle_nodes))
        min_swap <- 2          
        max_swap <- n          
      } else if (!is.null(end_idx)) {
        path <- c(sample(middle_nodes), end_idx)
        min_swap <- 1          
        max_swap <- n - 1      
      } else {
        path <- sample(n)
        min_swap <- 1
        max_swap <- n
      }
      
      curr_dist <- sum(dist_adj[cbind(path[-n], path[-1])])
      
      improved <- TRUE
      iterations <- 0
      while (improved && iterations < 200) { 
        improved <- FALSE
        iterations <- iterations + 1
        
        if (max_swap <= min_swap) break
        
        for (i in min_swap:(max_swap - 1)) {
          for (j in (i + 1):max_swap) {
            
            new_path <- path
            new_path[i:j] <- rev(path[i:j])
            
            new_dist <- sum(dist_adj[cbind(new_path[-n], new_path[-1])])
            
            if (new_dist < curr_dist) {
              path <- new_path
              curr_dist <- new_dist
              improved <- TRUE
            }
          }
        }
      }
      list(path = path, distance = curr_dist)
    })
    
    best_tour <- tour[[which.min(sapply(tour, function(x) x$distance))]]
    best_tour$path
  }, error = function(e) {
    seq_len(n) 
  })
  
  # 3. Limit Enforcement (Now Includes Visit Time & Personal Rest Time)
  route_metrics <- function(route_indices, dmat, tmat, visit_times) {
    if (length(route_indices) < 2) return(list(distance = 0, time = 0))
    
    total_dist <- 0
    total_travel_time <- 0
    
    # Sum travel time and distance
    for (i in 1:(length(route_indices) - 1)) {
      total_dist <- total_dist + dmat[route_indices[i], route_indices[i + 1]]
      total_travel_time <- total_travel_time + tmat[route_indices[i], route_indices[i + 1]]
    }
    
    # Calculate total park visit time
    total_visit_time <- 0
    if (!is.null(visit_times)) {
      total_visit_time <- sum(visit_times[route_indices], na.rm = TRUE)
    }
    
    # Apply your Rest/Personal Time Logic
    total_active_time <- total_travel_time + total_visit_time
    num_rest_periods <- floor(total_active_time / 14)
    total_rest_hours <- num_rest_periods * 10
    
    total_time <- total_active_time + total_rest_hours
    
    list(distance = total_dist, time = total_time)
  }
  
  if (is.finite(overall_time_limit) || is.finite(overall_distance_limit)) {
    current_metrics <- route_metrics(tsp_result, dist_matrix, time_adj, node_visit_times)
    
    if ((is.finite(overall_distance_limit) && current_metrics$distance > overall_distance_limit) ||
        (is.finite(overall_time_limit) && current_metrics$time > overall_time_limit)) {
      
      must_include <- unique(c(start_idx, end_idx))
      must_include <- must_include[!is.na(must_include)]
      candidate_route <- tsp_result
      
      while (length(candidate_route) > length(must_include)) {
        candidate_metrics <- route_metrics(candidate_route, dist_matrix, time_adj, node_visit_times)
        
        within_limits <- TRUE
        if (is.finite(overall_distance_limit) && candidate_metrics$distance > overall_distance_limit) within_limits <- FALSE
        if (is.finite(overall_time_limit) && candidate_metrics$time > overall_time_limit) within_limits <- FALSE
        if (within_limits) break
        
        removable_pos <- which(!candidate_route %in% must_include)
        if (length(removable_pos) == 0) break
        
        # Because rest time is non-linear, we evaluate the full route minus the candidate node
        marginal_cost <- sapply(removable_pos, function(pos) {
          test_route <- candidate_route[-pos]
          test_metrics <- route_metrics(test_route, dist_matrix, time_adj, node_visit_times)
          
          # Calculate how much time and distance we save by dropping this node
          dist_saved <- candidate_metrics$distance - test_metrics$distance
          time_saved <- candidate_metrics$time - test_metrics$time
          
          return(dist_saved + time_saved)
        })
        
        drop_pos <- removable_pos[which.max(marginal_cost)]
        candidate_route <- candidate_route[-drop_pos]
      }
      
      tsp_result <- candidate_route
    }
  }
  
  if (is.finite(max_segment_value) && max_segment_value > 0) {
    constraint_ok <- function(route_indices) {
      if (length(route_indices) < 2) return(TRUE)
      all(sapply(1:(length(route_indices) - 1), function(i) {
        constraint_matrix[route_indices[i], route_indices[i + 1]] <= max_segment_value
      }))
    }
    
    must_include <- unique(c(start_idx, end_idx))
    must_include <- must_include[!is.na(must_include)]
    while (!constraint_ok(tsp_result) && length(tsp_result) > length(must_include)) {
      violating_positions <- which(sapply(1:(length(tsp_result) - 1), function(i) {
        constraint_matrix[tsp_result[i], tsp_result[i + 1]] > max_segment_value
      }))
      if (length(violating_positions) == 0) break
      removable <- unique(c(violating_positions, violating_positions + 1))
      removable <- removable[removable <= length(tsp_result)]
      removable <- removable[!tsp_result[removable] %in% must_include]
      if (length(removable) == 0) break
      tsp_result <- tsp_result[-removable[1]]
    }
  }
  
  # 4. Limit Stops
  if (!is.null(num_stops) && num_stops < n && (is.infinite(overall_time_limit) && is.infinite(overall_distance_limit))) {
    distances_from_path <- numeric(n)
    for (i in seq_len(n)) {
      idx <- tsp_result[i]
      prev_idx <- if (i == 1) NA else tsp_result[i - 1]
      next_idx <- if (i == n) NA else tsp_result[i + 1]
      
      d1 <- if(is.na(prev_idx)) 0 else dist_matrix[prev_idx, idx]
      d2 <- if(is.na(next_idx)) 0 else dist_matrix[idx, next_idx]
      distances_from_path[i] <- d1 + d2
    }
    
    must_include <- unique(c(start_idx, end_idx))
    must_include <- must_include[!is.na(must_include)]
    
    remaining_slots <- num_stops - length(must_include)
    if (remaining_slots > 0) {
      candidates <- setdiff(seq_len(n), must_include)
      candidate_distances <- distances_from_path[candidates]
      selected_candidates <- candidates[order(candidate_distances, decreasing = TRUE)[1:min(remaining_slots, length(candidates))]]
      indices_to_keep <- c(must_include, selected_candidates)
    } else {
      indices_to_keep <- must_include
    }
    
    tsp_result <- tsp_result[tsp_result %in% indices_to_keep]
  }
  
  tsp_result <- enforce_endpoint_order(tsp_result, start_idx = start_idx, end_idx = end_idx)
  
  return(tsp_result)
}

get_osrm_route <- function(lon1, lat1, lon2, lat2) {
  base_url <- "https://router.project-osrm.org/route/v1/driving/"
  osrm_key <- paste(
    round(as.numeric(lon1), 5),
    round(as.numeric(lat1), 5),
    round(as.numeric(lon2), 5),
    round(as.numeric(lat2), 5),
    sep = "_"
  )
  
  if (exists(osrm_key, envir = osrm_route_cache, inherits = FALSE)) {
    return(get(osrm_key, envir = osrm_route_cache, inherits = FALSE))
  }
  url <- sprintf("%s%.6f,%.6f;%.6f,%.6f?overview=full&geometries=geojson",
                 base_url, lon1, lat1, lon2, lat2)
  
  result <- tryCatch({
    response <- GET(url, timeout(8))
    if (status_code(response) == 200) {
      response_content <- content(response, as = "parsed")
      if (!is.null(response_content$routes) && length(response_content$routes) > 0) {
        route <- response_content$routes[[1]]
        geometry <- route$geometry$coordinates
        distance_m <- route$distance
        duration_s <- route$duration
        return(list(
          geometry = geometry,
          distance_km = distance_m / 1000,
          duration_hours = duration_s / 3600,
          success = TRUE
        ))
      }
    }
    list(success = FALSE)
  }, error = function(e) {
    list(success = FALSE)
  })
  
  assign(osrm_key, result, envir = osrm_route_cache)
  result
}

route_with_geometry <- function(ordered_indices, locations, mode_matrix = NULL) {
  n <- length(ordered_indices)
  route_df <- locations[ordered_indices, ]
  route_df$step <- seq_len(n)
  
  distance_to_next <- numeric(n)
  time_to_next <- numeric(n)
  travel_mode <- character(n)
  segment_description <- character(n)
  segment_drive_distance_km <- rep(NA_real_, n)
  segment_flight_distance_km <- rep(NA_real_, n)
  segment_geometries <- vector("list", n)
  
  for (i in seq_len(n - 1)) {
    idx_from <- ordered_indices[i]
    idx_to <- ordered_indices[i + 1]
    
    lon1 <- locations$longitude[idx_from]
    lat1 <- locations$latitude[idx_from]
    lon2 <- locations$longitude[idx_to]
    lat2 <- locations$latitude[idx_to]
    
    dist_km <- haversine_distance(lon1, lat1, lon2, lat2)
    mode <- if (!is.null(mode_matrix)) mode_matrix[idx_from, idx_to] else "Auto"
    if (is.na(mode) || mode == "Auto") {
      mode <- infer_route_mode_for_segment(
        lat1 = lat1,
        lon1 = lon1,
        state1 = locations$state[idx_from],
        lat2 = lat2,
        lon2 = lon2,
        state2 = locations$state[idx_to]
      )
    }
    
    if (mode == "Drive") {
      route_result <- get_osrm_route(lon1, lat1, lon2, lat2)
      if (route_result$success) {
        distance_to_next[i] <- route_result$distance_km
        time_to_next[i] <- route_result$duration_hours
        segment_geometries[[i]] <- list(geometry = route_result$geometry)
      } else {
        distance_to_next[i] <- dist_km
        time_to_next[i] <- estimate_travel_time(dist_km, mode)
        segment_geometries[[i]] <- list(geometry = list(c(lon1, lat1), c(lon2, lat2)))
      }
      segment_drive_distance_km[i] <- distance_to_next[i]
      segment_description[i] <- paste0(route_df$name[i], " -> ", route_df$name[i + 1], " (Drive)")
    } else {
      airport_from <- find_nearest_airport_cached(lat1, lon1)
      airport_to <- find_nearest_airport_cached(lat2, lon2)
      
      drive_to_airport <- get_osrm_route(lon1, lat1, airport_from$airport$lon, airport_from$airport$lat)
      drive_from_airport <- get_osrm_route(airport_to$airport$lon, airport_to$airport$lat, lon2, lat2)
      
      flight_dist <- haversine_distance(airport_from$airport$lon, airport_from$airport$lat,
                                        airport_to$airport$lon, airport_to$airport$lat)
      
      total_dist <- airport_from$distance_km + flight_dist + airport_to$distance_km
      total_time <- (airport_from$distance_km / drive_speed_kmh) +
        (flight_dist / flight_speed_kmh) +
        (airport_to$distance_km / drive_speed_kmh) +
        flight_fixed_time_hours
      
      distance_to_next[i] <- total_dist
      time_to_next[i] <- total_time
      segment_drive_distance_km[i] <- airport_from$distance_km + airport_to$distance_km
      segment_flight_distance_km[i] <- flight_dist
      
      segment_geometries[[i]] <- list(
        drive_to_airport_geometry = if (drive_to_airport$success) drive_to_airport$geometry else list(c(lon1, lat1), c(airport_from$airport$lon, airport_from$airport$lat)),
        flight_geometry = list(c(airport_from$airport$lon, airport_from$airport$lat), c(airport_to$airport$lon, airport_to$airport$lat)),
        drive_from_airport_geometry = if (drive_from_airport$success) drive_from_airport$geometry else list(c(airport_to$airport$lon, airport_to$airport$lat), c(lon2, lat2))
      )
      segment_description[i] <- paste0(
        route_df$name[i], " -> ", airport_from$airport$name, " (drive ", round(airport_from$distance_km / drive_speed_kmh, 2), "h)",
        " | ", airport_from$airport$name, " -> ", airport_to$airport$name, " (flight)",
        " | ", airport_to$airport$name, " -> ", route_df$name[i + 1], " (drive ", round(airport_to$distance_km / drive_speed_kmh, 2), "h)"
      )
    }
    
    travel_mode[i] <- mode
  }
  
  distance_to_next[n] <- NA
  time_to_next[n] <- NA
  travel_mode[n] <- NA
  segment_description[n] <- NA
  
  route_df$distance_to_next_km <- distance_to_next
  route_df$time_to_next_hours <- time_to_next
  route_df$travel_mode <- travel_mode
  route_df$segment_description <- segment_description
  route_df$segment_drive_distance_km <- segment_drive_distance_km
  route_df$segment_flight_distance_km <- segment_flight_distance_km
  
  list(
    route = route_df,
    total_distance = sum(distance_to_next, na.rm = TRUE),
    total_time = sum(time_to_next, na.rm = TRUE),
    segment_geometries = segment_geometries
  )
}

###################
### UI
###################

ui <- dashboardPage(
  dashboardHeader(title = "National Parks Route Optimizer"),
  
  dashboardSidebar(
    sidebarMenu(id = "main_tabs",
                menuItem("Home", tabName = "home", icon = icon("home")),
                menuItem("User Information", tabName = "userinfo", icon = icon("user")),
                menuItem("National Parks Map", tabName = "allparks", icon = icon("globe-americas")),
                menuItem("Park Explorer", tabName = "viewpark", icon = icon("map")),
                menuItem("Route Planner", tabName = "planner", icon = icon("route")),
                menuItem("Optimal Route", tabName = "optimalroute", icon = icon("map-marked-alt")),
                menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
  ),
  
  dashboardBody(
    shinyjs::useShinyjs(), # Required for hiding the message
    tags$div(
      id = "init-message",
      style = "position: fixed; top: 50%; left: 50%; transform: translate(-50%, -50%); z-index: 9999; background: #333;
    color: white; padding: 12px 18px; border-radius: 6px; text-align: center;",
      "Initializing Maps... Please wait."
    ),
    tags$head(
      tags$script(HTML("
        $(document).on('click', '.input-group-addon', function() {
          var input = $(this).siblings('input');
          if (input.length) {
            input.trigger('focus');  // open the datepicker
          }
        });
      ")),
      tags$style(HTML("
        .info-box { min-height: 90px; }
        .info-box-icon { height: 90px; line-height: 90px; }
        .info-box-content { padding-top: 0px; padding-bottom: 0px; }
        .gallery-slide { display: none; }
        .gallery-slide.active { display: block; }
        .park-gallery-modal {
          position: fixed;
          z-index: 3000;
          left: 0;
          top: 0;
          width: 100%;
          height: 100%;
          overflow: auto;
          background-color: rgba(0, 0, 0, 0.55);
        }
        .park-gallery-modal-content {
          position: relative;
          background-color: #fff;
          margin: 8% auto;
          padding: 16px;
          border-radius: 8px;
          width: min(340px, 97vw);
          box-shadow: 0 10px 30px rgba(0, 0, 0, 0.35);
        }
        .park-gallery-close {
          position: absolute;
          top: 8px;
          right: 10px;
          border: none;
          background: transparent;
          font-size: 24px;
          cursor: pointer;
          line-height: 1;
        }
        .park-table-row {
          display: flex;
          flex-wrap: wrap;
        }
        .park-table-row > [class*='col-sm-'] {
          display: flex;
        }
        .park-table-box {
          width: 100%;
          display: flex;
          flex-direction: column;
        }
        .park-table-box .box-body {
          min-height: 450px;
          display: flex;
          flex-direction: column;
        }
        .park-table-box .dataTables_wrapper {
          display: flex;
          flex-direction: column;
          flex: 1 1 auto;
          height: 100%
        }
        .park-table-box .dataTables_wrapper .dataTables_info,
        .park-table-box .dataTables_wrapper .dataTables_paginate {
          margin-top: auto;
        }
        .park-table-box .dt-footer {
          display: flex;
          justify-content: space-between;
          align-items: center;
          width: 100%;
          margin-top: auto;
          margin-top: 10px;
        }
        .park-table-box .dt-footer .dataTables_info {
          padding-top: 0;
          margin-right: 12px;
        }
        .park-table-box .dt-footer .dataTables_paginate {
          float: none;
          margin-left: auto;
          text-align: right;
        }
        .park-table-box .dataTables_wrapper .dataTable td,
        .park-table-box .dataTables_wrapper .dataTable th {
          white-space: normal !important;
          word-break: break-word;
          vertical-align: top;
        }
        .park-table-box .dataTables_wrapper .dataTable td.dt-nowrap,
        .park-table-box .dataTables_wrapper .dataTable th.dt-nowrap {
          white-space: nowrap !important;
          word-break: normal;
        }
        .viewpark-controls {
          position: relative;
          z-index: 1200;
          margin-bottom: 0px;
          padding: 0 10px;
        }
        .viewpark-controls .form-group {
          margin-bottom: 0;
        }
        .viewpark-map-wrap {
          position: relative;
          z-index: 1100;
          padding-top: 56px;
        }
        .viewpark-controls .selectize-dropdown,
        .viewpark-controls .dropdown-menu,
        .viewpark-controls .bootstrap-select .dropdown-menu {
          z-index: 2200 !important;
        }
        .park-table-box .dataTables_wrapper .dataTable td,
        .park-table-box .dataTables_wrapper .dataTable th {
          white-space: normal !important;
          word-break: break-word;
          vertical-align: top;
        }
        .park-table-box .dataTables_wrapper .dataTable td.dt-nowrap,
        .park-table-box .dataTables_wrapper .dataTable th.dt-nowrap {
          white-space: nowrap !important;
          word-break: normal;
        }
        table.dataTable {
          width: 100% !important;
          table-layout: fixed;
        }
        table.dataTable td {
          white-space: normal !important;
          word-wrap: break-word;
          font-size: 12px;
        }
        table.dataTable th {
          white-space: normal !important;
          font-size: 13px;
        }
      "))
    ),
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/html2pdf.js/0.10.1/html2pdf.bundle.min.js"),
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/html2canvas/1.4.1/html2canvas.min.js"),
    tags$script(HTML("
        Shiny.addCustomMessageHandler('download_route_pdf', function(message) {
          var source = document.getElementById('route-pdf-content');
          if (!source || typeof html2pdf === 'undefined') return;
      
          var mapElement = document.getElementById('route_map');
          var leafletWidget = (window.HTMLWidgets && typeof HTMLWidgets.find === 'function') ? HTMLWidgets.find('#route_map') : null;
          var leafletMap = (leafletWidget && typeof leafletWidget.getMap === 'function') ? leafletWidget.getMap() : null;
      
          var filename = (message && message.filename) ? message.filename : 'optimal_route.pdf';
      
          async function generatePDF() {
              const scrollX = window.scrollX;
              const scrollY = window.scrollY;
              window.scrollTo(0,0);
              
              // 1. Instantly hide the UI controls
              var hideElements = source.querySelectorAll('.pdf-hide-control');
              hideElements.forEach(function(el) { el.style.display = 'none'; });
      
              try {
                  // 2. THE SPEED FIX: Capture map at low scale first to prevent hang
                  // We use a shorter timeout and lower scale (1.0)
                  const canvas = await html2canvas(mapElement, {
                      useCORS: true,
                      scale: 1, 
                      logging: false,
                      allowTaint: false,
                      scrollY: -window.scrollY, // CRITICAL: Negate any scroll offset
                      scrollX: -window.scrollX,
                      timeout: 15000, // Kill the process if it takes > 15 seconds
                      ignoreElements: (el) => el.classList.contains('leaflet-control-container')
                  });
      
                  // 3. Create the replacement image
                  var mapImage = new Image();
                  mapImage.src = canvas.toDataURL('image/png');
                  mapImage.style.width = '100%';
                  mapImage.style.height = '400px'; // Force a clean height for the PDF
                  mapImage.style.objectFit = 'contain';
                  mapImage.id = 'temp-map-img';
      
                  // 4. Swap Map for Image
                  mapElement.style.display = 'none';
                  mapElement.parentNode.insertBefore(mapImage, mapElement.nextSibling);
      
                  // 5. Generate PDF
                  var options = {
                      margin: [0.5, 0.5],
                      filename: filename,
                      image: { type: 'jpeg', quality: 0.98 },
                      html2canvas: { scale: 2, useCORS: true },
                      jsPDF: { unit: 'in', format: 'letter', orientation: 'portrait' },
                      pagebreak: { mode: ['css', 'legacy'], before: '.page-break' }
                  };
      
                  await html2pdf().set(options).from(source).save();
      
              } catch (e) {
                  console.error('PDF Fail:', e);
              } finally {
                  // 6. ALWAYS RESTORE
                  mapElement.style.display = 'block';
                  var tempImg = document.getElementById('temp-map-img');
                  if (tempImg) tempImg.remove();
                  hideElements.forEach(function(el) { el.style.display = ''; });
                  Shiny.setInputValue('pdf_done', Math.random());
              }
          }
      
          // Ensure the map is 'ready' before snapping
          if (leafletMap) {
              leafletMap.invalidateSize();
              // Snap the map immediately without animation
              if (message.fit_bounds) {
                  leafletMap.fitBounds(message.fit_bounds, {animate: false, padding: [30, 30]});
              }
              // Give it 500ms for the tiles to 'settle'
              setTimeout(generatePDF, 500);
          } else {
              generatePDF();
          }
      });
        window.parkGalleryNav = function(galleryId, delta) {
          var gallery = document.getElementById(galleryId);
          if (!gallery) return;
          var slides = gallery.querySelectorAll('.gallery-slide');
          if (!slides || slides.length === 0) return;
          var counter = gallery.querySelector('.gallery-counter');
          var active = 0;
          slides.forEach(function(slide, idx) { if (slide.classList.contains('active')) active = idx; });
          slides[active].classList.remove('active');
          var nextIdx = (active + delta + slides.length) % slides.length;
          slides[nextIdx].classList.add('active');
          if (counter) counter.textContent = (nextIdx + 1) + ' of ' + slides.length;
        };
        window.openParkGallery = function(modalId) {
          var modal = document.getElementById(modalId);
          if (!modal) return;
          modal.style.display = 'block';
        };
        window.closeParkGallery = function(modalId) {
          var modal = document.getElementById(modalId);
          if (!modal) return;
          modal.style.display = 'none';
        };
      ")),
    
    tabItems(
      # Home Tab
      tabItem(tabName = "home",
              fluidRow(
                box(
                  title = "Welcome",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  p("Use the sidebar or buttons below to move through the app quickly. Start with User Info (optional), then plan and calculate your route."),
                  tags$ul(
                    tags$li(strong("User Information:"), "Optionally set traveler count, MPG, airline preferences, and travel dates."),
                    tags$li(strong("National Parks Map:"), "Browse every U.S. national park with contact details and image galleries."),
                    tags$li(strong("Park Explorer:"), "Explore one park at a time with map zoom, activities, things to do, and park outline details."),
                    tags$li(strong("Route Planner:"), "Select parks, set distance/time limits, and choose default transportation behavior."),
                    tags$li(strong("Optimal Route:"), "Review route summaries, estimated costs, turn-by-turn directions, and optional per-segment overrides."),
                    tags$li(strong("About:"), "Review feature notes, assumptions, and route logic details.")
                  ),
                  br(),
                  actionButton("go_user", "Enter User Info", class = "btn-user"),
                  tags$span("  "),
                  actionButton("go_allparks", "See Map", class = "btn-primary"),
                  tags$span("  "),
                  actionButton("go_viewpark", "Individual Parks Info", class = "btn-info"),
                  tags$span("  "),
                  actionButton("go_planner", "Plan Route", class = "btn-success")
                )
              )
      ),
      
      # User Information Tab
      tabItem(tabName = "userinfo",
              fluidRow(
                box(
                  title = "User Information (Optional)",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  p("All fields are optional. Defaults are pre-filled and can be adjusted."),
                  fluidRow(
                    column(
                      3,
                      numericInput("num_travelers", "Number of Travelers:", value = 1, min = 1, step = 1)
                    ),
                    column(
                      3,
                      numericInput("car_mpg", "Vehicle MPG:", value = 29, min = 1, step = 1)
                    ),
                    column(
                      3,
                      selectInput(
                        "vehicle_type",
                        "Vehicle Type:",
                        choices = c("Car" = "car", "Motorcycle" = "motorcycle"),
                        selected = "car"
                      )
                    ),
                    column(
                      3,
                      pickerInput(
                        "airline_preference",
                        "Airline Preference:",
                        choices = c("Any", "Alaska Airlines", "Allegiant Air", "American Airlines", "Delta Air Lines", "Frontier Airlines", "JetBlue", "Southwest Airlines", "Spirit Airlines", "United Airlines"),
                        selected = "Any",
                        multiple = TRUE,
                        options = list(`actions-box` = TRUE, `live-search` = TRUE)
                      )
                    )
                  ),
                  fluidRow(
                    column(
                      3,
                      airDatepickerInput(
                        "travel_dates",
                        "Potential Travel Dates:",
                        value = c(
                          seq(Sys.Date(), by = "1 month", length.out = 2)[2],
                          seq(Sys.Date(), by = "1 month", length.out = 2)[2] + 7
                        ),
                        range = TRUE,
                        autoClose = TRUE,
                        dateFormat = "yyyy-MM-dd",
                        minDate = Sys.Date()
                      )
                    )
                  ),
                  p(em("Default dates are one month from today for a 7-day trip window. The dates are used for flight price calculation only."))
                )
              )
      ),
      
      # National Parks Map Tab
      tabItem(tabName = "allparks",
              fluidRow(
                box(
                  title = "National Parks Map",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  p("All national parks with contact details and images from NPS"),
                  leafletOutput("all_parks_map", height = 700)
                )
              )
      ),
      
      tabItem(tabName = "viewpark",
              fluidRow(
                box(
                  title = "Park Explorer",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  div(
                    class = "viewpark-controls",
                    fluidRow(
                      column(6, selectInput("view_park_choice", "Select a Park:", choices = NULL)),
                      column(3, pickerInput("view_feature_type", "Feature Type(s):", choices = NULL, selected = NULL, multiple = TRUE, options = list(`actions-box` = TRUE, `live-search` = FALSE)))
                    )
                  ),
                  uiOutput("view_feature_message"),
                  div(
                    class = "viewpark-map-wrap",
                    leafletOutput("view_park_map", height = 500)
                  )
                )
              ),
              fluidRow(
                box(
                  title = "Park Overview",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  div(style = "width: 100%; max-width: 1400px;", htmlOutput("view_park_summary"))
                )
              ),
              fluidRow(
                class = "park-table-row",
                box(title = "Activities", status = "success", solidHeader = TRUE, width = 6, class = "park-table-box", DTOutput("view_park_activities")),
                box(title = "Things To Do", status = "warning", solidHeader = TRUE, width = 6, class = "park-table-box", DTOutput("view_park_things_to_do"))
              )
      ),
      
      # Route Planner Tab
      tabItem(tabName = "planner",
              fluidRow(
                box(
                  title = "Start/End Locations (Optional)",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  collapsible = TRUE,
                  collapsed = TRUE,
                  
                  fluidRow(
                    column(6,
                           selectizeInput("start_zip_choice", "Start City:", choices = NULL, multiple = FALSE,
                                          options = list(placeholder = "Choose a city (by zip code)"))
                    ),
                    column(6,
                           selectizeInput("end_zip_choice", "End City:", choices = NULL, multiple = FALSE,
                                          options = list(placeholder = "Optional final destination city"))
                    )
                  ),
                  fluidRow(
                    column(
                      12,
                      checkboxInput(
                        "same_as_start",
                        "End location is the same as start location",
                        value = FALSE
                      )
                    )
                  ),
                  p(em("Leave blank to start at the first park and/or end at the last park."))
                )
              ),
              
              fluidRow(
                box(
                  title = "Route Configuration",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  
                  fluidRow(
                    column(4,
                           selectInput("limit_metric",
                                       "Between Park Limits:",
                                       choices = c("No Limit" = "none",
                                                   "Maximum Travel Time Between Park Locations" = "time",
                                                   "Maximum Travel Distance Between Park Locations" = "distance"),
                                       selected = "none")
                    ),
                    column(4,
                           radioButtons("distance_type",
                                        "Default Segment:",
                                        choices = c("Driving Only" = "driving",
                                                    "Auto (Quickest per segment)" = "mixed",
                                                    "Flying Only" = "flying"),
                                        selected = "mixed")
                    ),
                    column(4,
                           selectInput("distance_unit",
                                       "Distance Units:",
                                       choices = c("Miles" = "miles", "Kilometers" = "km"),
                                       selected = "miles")
                    )
                  ),
                  fluidRow(
                    column(
                      4,
                      conditionalPanel(
                        condition = "input.limit_metric !== 'none'",
                        uiOutput("limit_value_input")
                      )
                    ),
                    column(
                      4,
                      numericInput("overall_time_limit",
                                   "Total Trip Time Limit (hours, optional):",
                                   value = NA, min = 1, step = 1)
                    ),
                    column(
                      4,
                      numericInput("overall_distance_limit",
                                   "Total Trip Distance Limit (miles, optional):",
                                   value = NA, min = 1, step = 10)
                    )
                  ),
                  br(),
                  fluidRow(
                    column(12,
                           p("Click park markers to select them for your route. Red = not selected, Green = selected."),
                           leafletOutput("park_selector_map", height = 450)
                    )
                  ),
                  fluidRow(
                    column(6,
                           actionButton("select_all_parks", "Select All Parks", icon = icon("check-square"),
                                        class = "btn-success btn-block")
                    ),
                    column(6,
                           actionButton("deselect_all_parks", "Deselect All Parks", icon = icon("square"),
                                        class = "btn-warning btn-block")
                    )
                  ),
                  br(),
                  fluidRow(
                    column(12,
                           uiOutput("route_message"),
                           br(),
                           tags$div(
                             style = "display:flex; align-items:center; gap:12px; flex-wrap:wrap;",
                             actionButton("calculate", "Calculate Optimal Route",
                                          icon = icon("calculator"),
                                          class = "btn-success btn-lg"),
                             tags$span("⚠️ May take a few minutes to load.",
                                       style = "color:#856404; font-weight:600;")
                           ),
                           br(), br(),
                           textOutput("status_text")
                    )
                  )
                )
              ),
              
              
              fluidRow(
                box(
                  title = "Next Step",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  p("When calculation finishes, you will be redirected to Optimal Route automatically.")
                )
              )
      ),
      
      # Optimal Route Tab
      tabItem(tabName = "optimalroute",
              div(
                id = "route-pdf-content",
                div(
                  style = "text-align:center; margin-bottom: 25px;",
                  h1("Optimal Route Summary", style = "margin-bottom: 5px;"),
                  p("Your optimized travel plan across selected parks", 
                    style = "color: #7f8c8d; font-size: 16px;")
                ),
                br(),
                fluidRow(
                  infoBoxOutput("total_time_box", width = 4),
                  infoBoxOutput("total_distance_box", width = 4),
                  infoBoxOutput("travel_time_box", width = 4)
                ),
                fluidRow(
                  infoBoxOutput("park_visit_time_box", width = 4),
                  infoBoxOutput("num_stops_box", width = 4),
                  infoBoxOutput("avg_distance_box", width = 4)
                ),
                fluidRow(
                  infoBoxOutput("estimated_cost_box", width = 4),
                  infoBoxOutput("personal_time_box", width = 4),
                  infoBoxOutput("overnight_stays_box", width = 4)
                ),
                fluidRow(
                  infoBoxOutput("states_visited_box", width = 12)
                ),
                fluidRow(
                  class = "pdf-hide-control",
                  box(
                    title = "Interactive Route Map with GPS-Style Paths",
                    status = "primary",
                    solidHeader = TRUE,
                    width = 12,
                    actionButton("back_to_planner", "← Back to Route Planner", class = "btn-default pdf-hide-control"),
                    tags$span("  "),
                    actionButton("download_route_pdf", "Download PDF", icon = icon("file-pdf"), class = "btn-primary pdf-hide-control"),
                    br(), br(),
                    p("Blue lines = driving routes (actual roads). Red lines = flight paths (includes airport transfers).
                 Click markers to see park details."),
                    p(em("Map initializes on app startup. If no route has been calculated yet, a starter map is shown.")),
                    leafletOutput("route_map", height = 700)
                  )
                ),
                fluidRow(
                  class = "pdf-hide-control",
                  box(
                    title = "Per-Segment Travel Mode Overrides",
                    status = "warning",
                    solidHeader = TRUE,
                    width = 12,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    p("Override each segment to Auto/Drive/Flight and recalculate route details instantly."),
                    uiOutput("segment_mode_controls")
                  )
                ),
                fluidRow(
                  class = "pdf-hide-control",
                  box(
                    title = "Park Visit Time Overrides (hours)",
                    status = "info",
                    solidHeader = TRUE,
                    width = 12,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    uiOutput("park_visit_time_controls")
                  )
                ),
                div(
                  fluidRow(
                    br(),
                    box(
                      title = "Optimal Route Details",
                      status = "primary",
                      solidHeader = TRUE,
                      width = 12,
                      style = "width: 100%;",
                      DTOutput("route_table")
                    )
                  )
                ),
                div(
                  class = "page-break",
                  style = "page-break-before: always;",
                  fluidRow(
                    box(
                      title = "Turn-by-Turn Driving Directions",
                      status = "success",
                      solidHeader = TRUE,
                      width = 12,
                      style = "width: 100%;",
                      uiOutput("driving_directions_ui")
                    )
                  )
                )
              ),
      ),
      
      # About Tab
      tabItem(tabName = "about",
              fluidRow(
                box(
                  title = "About This Application",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  
                  h3("National Parks Route Optimizer"),
                  
                  h4("Key Features:"),
                  tags$ul(
                    tags$li(strong("Custom Starting Location:"), "Enter your city to start the route from home"),
                    tags$li(strong("Smart Park Selection:"), "Can visit multiple parks in same state (e.g., all 5 Utah parks)"),
                    tags$li(strong("Realistic Flight Routing:"), "Includes drives to/from major airports and flight time"),
                    tags$li(strong("Mixed Transportation:"), "Automatically uses driving for mainland, flights for Alaska/Hawaii"),
                    tags$li(strong("GPS-Accurate Routes:"), "Shows actual roads for drive segments and straight-line flight paths"),
                    tags$li(strong("Complete Park Info:"), "Dates, areas, visitor counts, and descriptions"),
                    tags$li(strong("Route Overrides:"), "Optionally override travel mode by segment and park visit hours")
                  ),
                  
                  h4("How Flights Work:"),
                  tags$ol(
                    tags$li("Calculates drive to nearest major airport"),
                    tags$li("Direct flight to destination region's airport"),
                    tags$li("Drive from airport to park"),
                    tags$li("Flight time includes 2.5 hours for boarding, taxi, etc.")
                  ),
                  
                  h4("TSP Algorithm:"),
                  p("The route optimizer uses the Traveling Salesman Problem algorithm to find
               the quickest overall travel-time path. It can select multiple parks from the same state if that
               creates a better overall route.")
                )
              )
      )
    )
  )
)

###################
### Server
###################
server <- function(input, output, session) {
  
  # Map Initialization Message (Shows for 5 seconds)
  shinyjs::delay(5000, shinyjs::hide("init-message"))
  
  map_debug_targets <- c("GLBA", "KATM", "KOVA")
  
  log_map_debug <- function(..., .park_code = NULL, .park_name = NULL) {
    code_clean <- normalize_park_code(.park_code)
    name_clean <- normalize_park_name_key(.park_name)
    target_names <- normalize_park_name_key(c("Glacier Bay", "Katmai", "Kobuk Valley"))
    should_log <- (!is.na(code_clean) && nzchar(code_clean) && code_clean %in% map_debug_targets) ||
      (!is.na(name_clean) && nzchar(name_clean) && name_clean %in% target_names)
    if (isTRUE(should_log)) {
      message(sprintf("[MAP DEBUG] %s", paste0(..., collapse = "")))
    }
  }
  
  # Load gas data
  gas_data <- reactive({
    get_gas_prices()
  })
  
  # Load park data
  park_data <- load_park_data()
  park_images <- read.csv("national_parks_images.csv", stringsAsFactors = FALSE)
  park_activities <- read.csv("nps_activities_by_park.csv", stringsAsFactors = FALSE)
  park_things_to_do <- read.csv("nps_things_to_do.csv", stringsAsFactors = FALSE)
  park_outline <- load_park_point_data()
  park_boundary_shapes <- load_park_boundary_shapes()
  park_boundary_shapes_named <- park_boundary_shapes %>%
    mutate(name_key = normalize_park_name_key(park_name))
  visit_time_defaults <- load_visit_time_defaults()
  park_catalog <- park_data %>% distinct(name, .keep_all = TRUE)
  park_data$region <- sapply(park_data$state, classify_park_region)
  park_catalog <- park_catalog %>%
    left_join(visit_time_defaults, by = c("park_code" = "park_code")) %>%
    arrange(name)
  
  selected_park_names <- reactiveVal(character(0))
  initial_view_park <- park_catalog %>% pull(name) %>% .[1]
  selected_view_park <- reactiveVal(initial_view_park)
  segment_mode_overrides <- reactiveVal(character(0))
  park_visit_time_overrides <- reactiveVal(numeric(0))
  computed_route <- reactiveVal(NULL)
  pending_route <- reactiveVal(NULL)
  route_map_bounds <- reactiveVal(NULL)
  
  get_named_value <- function(x, key) {
    if (length(x) == 0 || is.null(names(x))) return(NULL)
    value <- x[key]
    if (length(value) == 0) return(NULL)
    value[[1]]
  }
  
  first_or <- function(x, default) {
    if (is.null(x) || length(x) == 0) return(default)
    x
  }
  
  selected_feature_types <- function(x) {
    if (is.null(x)) return(character())
    values <- as.character(unlist(x, use.names = FALSE))
    values <- values[!is.na(values) & nzchar(str_trim(values))]
    if (length(values) == 0) return(character(0))
    unique(values)
  }
  
  get_boundary_shape_for_park <- function(selected_park_row) {
    raw_code <- selected_park_row$park_code[[1]]
    raw_name <- selected_park_row$name[[1]]
    selected_code <- resolve_boundary_code(
      raw_code,
      raw_name,
      known_boundary_codes = park_boundary_shapes$park_code
    )
    log_map_debug(
      "Resolving boundary shape for park='", raw_name,
      "' raw_code='", raw_code,
      "' resolved_code='", selected_code,
      "'",
      .park_code = selected_code,
      .park_name = raw_name
    )
    shape <- park_boundary_shapes %>%
      filter(park_code == selected_code)
    if (nrow(shape) > 0) {
      shape_one <- shape %>% slice(1)
      shape_points <- nrow(shape_one$boundary_coords[[1]] %>% filter(!is.na(lng), !is.na(lat)))
      log_map_debug(
        "Boundary shape found by code. bounds=[",
        round(shape_one$min_lng[[1]], 4), ", ",
        round(shape_one$min_lat[[1]], 4), "] to [",
        round(shape_one$max_lng[[1]], 4), ", ",
        round(shape_one$max_lat[[1]], 4), "], points=",
        shape_points,
        .park_code = selected_code,
        .park_name = raw_name
      )
      return(shape_one)
    }
    if (nrow(shape) > 0) return(shape %>% slice(1))
    
    selected_name_key <- normalize_park_name_key(raw_name)
    name_shape <- park_boundary_shapes_named %>%
      filter(name_key == selected_name_key)
    if (nrow(name_shape) > 0) {
      shape_one <- name_shape %>% slice(1) %>% select(-name_key)
      shape_points <- nrow(shape_one$boundary_coords[[1]] %>% filter(!is.na(lng), !is.na(lat)))
      log_map_debug(
        "Boundary shape found by normalized name. park_name='",
        shape_one$park_name[[1]],
        "', points=",
        shape_points,
        .park_code = selected_code,
        .park_name = raw_name
      )
      return(shape_one)
    }
    log_map_debug(
      "No boundary shape found for park='", raw_name,
      "' resolved_code='", selected_code, "'",
      .park_code = selected_code,
      .park_name = raw_name
    )
    
    tibble(
      park_code = character(),
      park_name = character(),
      boundary_coords = list(),
      min_lng = numeric(),
      min_lat = numeric(),
      max_lng = numeric(),
      max_lat = numeric()
    )
  }
  
  boundary_check_codes <- c("GLBA", "KATM", "KOVA")
  missing_boundary_codes <- setdiff(boundary_check_codes, park_boundary_shapes$park_code)
  malformed_boundary_codes <- park_boundary_shapes %>%
    filter(
      park_code %in% boundary_check_codes,
      is.na(min_lng) | is.na(min_lat) | is.na(max_lng) | is.na(max_lat)
    ) %>%
    pull(park_code)
  if (length(missing_boundary_codes) > 0 || length(malformed_boundary_codes) > 0) {
    warning(
      paste0(
        "Boundary JSON diagnostic: missing codes [",
        paste(missing_boundary_codes, collapse = ", "),
        "], malformed bounds [",
        paste(malformed_boundary_codes, collapse = ", "),
        "]."
      ),
      call. = FALSE
    )
  }
  
  filter_outline_for_park <- function(outline_df, selected_park_row) {
    selected_name <- selected_park_row$name[[1]]
    selected_code <- normalize_park_code(selected_park_row$park_code[[1]])
    if (is.na(selected_code) || !nzchar(selected_code)) {
      log_map_debug(
        "Outline filter skipped due to missing selected code for park='",
        selected_name, "'",
        .park_name = selected_name
      )
      return(outline_df %>% slice(0))
    }
    
    filtered <- outline_df %>%
      filter(normalize_park_code(.data$park_code) == .env$selected_code)
    log_map_debug(
      "Outline filter park='", selected_name,
      "' code='", selected_code,
      "' matched_points=", nrow(filtered),
      .park_code = selected_code,
      .park_name = selected_name
    )
    filtered
  }
  
  park_outline <- park_outline %>%
    mutate(
      feature_type = map2_chr(type, entity_name, categorize_park_feature),
      is_park_name_point = coalesce(normalize_park_name_key(entity_name) == normalize_park_name_key(park_name), FALSE),
      feature_type = if_else(is_park_name_point, "__PARK_NAME_POINT__", feature_type)
    ) %>%
    filter(!is.na(feature_type))
  
  park_type_levels <- c("Visitor Center", "Campground", "Trail", "Parking", "Picnic Area", "Restroom", "Additional Areas")
  park_type_palette <- colorFactor(
    palette = c(
      "Visitor Center" = "#1f78b4",
      "Campground" = "#33a02c",
      "Trail" = "#b2df8a",
      "Parking" = "#6a3d9a",
      "Picnic Area" = "#ff7f00",
      "Restroom" = "#e31a1c",
      "Additional Areas" = "#7f7f7f"
    ),
    domain = park_type_levels,
    na.color = "#7f7f7f"
  )
  
  get_default_visit_hours <- function(park_name) {
    val <- park_catalog %>%
      filter(name == park_name) %>%
      pull(default_visit_hours) %>%
      .[1]
    if (is.null(val) || is.na(val) || !is.finite(val) || val <= 0) 2 else round(as.numeric(val), 2)
  }
  
  add_popup_html <- function(data) {
    data %>%
      mutate(
        popup_html = map_chr(name, function(park_name_value) {
          park_row <- data %>% filter(name == park_name_value) %>% slice(1)
          build_park_popup(park_row, park_images)
        })
      )
  }
  
  city_zip_choices <- zip_codes %>%
    mutate(label = paste0(city, ", ", state_id, " (", zip, ")")) %>%
    distinct(label, .keep_all = TRUE) %>%
    arrange(city, state_id, zip)
  
  # Update city choices
  observe({
    city_choices <- setNames(city_zip_choices$zip, city_zip_choices$label)
    updateSelectizeInput(
      session,
      "start_zip_choice",
      choices = city_choices,
      selected = "",
      server = TRUE
    )
    updateSelectizeInput(
      session,
      "end_zip_choice",
      choices = city_choices,
      selected = "",
      server = TRUE
    )
    park_choices <- sort(unique(park_catalog$name))
    updateSelectInput(session, "view_park_choice", choices = park_choices, selected = initial_view_park)
    updatePickerInput(
      session,
      "view_feature_type",
      choices = setNames(park_type_levels, park_type_levels),
      selected = park_type_levels
    )
  })
  
  observeEvent(input$view_park_choice, {
    if (!is.null(input$view_park_choice)) {
      selected_view_park(input$view_park_choice)
    }
  })
  
  observe({
    if (isTRUE(input$same_as_start)) {
      start_value <- if (!is.null(input$start_zip_choice) && nzchar(input$start_zip_choice)) input$start_zip_choice else ""
      updateSelectizeInput(session, "end_zip_choice", selected = start_value)
    }
  })
  
  output$limit_value_input <- renderUI({
    label_text <- if (identical(input$limit_metric, "time")) "Hour Limit:" else "Mile Limit:"
    default_value <- if (identical(input$limit_metric, "time")) 12 else 500
    numericInput(
      "limit_value",
      label_text,
      min = 1,
      value = default_value,
      step = 1
    )
  })
  
  output$all_parks_map <- renderLeaflet({
    map_data <- add_popup_html(park_catalog)
    
    leaflet(map_data) %>%
      addTiles(options = tileOptions(noWrap = TRUE)) %>%
      setView(lng = -98.5, lat = 39.8, zoom = 4) %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        radius = 6,
        color = "#2c7fb8",
        fillColor = "#2c7fb8",
        fillOpacity = 0.8,
        stroke = TRUE,
        weight = 1.5,
        label = ~name,
        popup = ~popup_html
      )
  })
  
  output$view_park_map <- renderLeaflet({
    selected_name <- selected_view_park()
    map_data <- add_popup_html(park_catalog)
    
    selected_points <- tibble(
      entity_name = character(),
      feature_type = character(),
      latitude = numeric(),
      longitude = numeric()
    )
    if (!is.null(selected_name) && nzchar(selected_name)) {
      selected_code <- park_catalog %>%
        filter(name == selected_name) %>% slice(1)
      if (nrow(selected_code) == 1) {
        selected_points <- filter_outline_for_park(park_outline, selected_code)
        selected_boundary_shape <- get_boundary_shape_for_park(selected_code)
        if (nrow(selected_boundary_shape) > 0) {
          boundary_pad_lng <- pmax((selected_boundary_shape$max_lng[[1]] - selected_boundary_shape$min_lng[[1]]) * 0.2, 0.02)
          boundary_pad_lat <- pmax((selected_boundary_shape$max_lat[[1]] - selected_boundary_shape$min_lat[[1]]) * 0.2, 0.02)
          pre_bbox_points <- selected_points
          pre_filter_n <- nrow(pre_bbox_points)
          filtered_points <- pre_bbox_points %>%
            filter(
              longitude >= selected_boundary_shape$min_lng[[1]] - boundary_pad_lng,
              longitude <= selected_boundary_shape$max_lng[[1]] + boundary_pad_lng,
              latitude >= selected_boundary_shape$min_lat[[1]] - boundary_pad_lat,
              latitude <= selected_boundary_shape$max_lat[[1]] + boundary_pad_lat
            )
          if (nrow(filtered_points) == 0 && pre_filter_n > 0) {
            selected_points <- pre_bbox_points
            log_map_debug(
              "Boundary bbox removed all points for park='", selected_name,
              "'. Keeping unfiltered points for visibility/debugging.",
              .park_code = selected_code$park_code[[1]],
              .park_name = selected_name
            )
          } else {
            selected_points <- filtered_points
          }
          log_map_debug(
            "Boundary bbox filter park='", selected_name,
            "' pre_points=", pre_filter_n,
            ", post_points=", nrow(selected_points),
            ", pad_lng=", round(boundary_pad_lng, 4),
            ", pad_lat=", round(boundary_pad_lat, 4),
            .park_code = selected_code$park_code[[1]],
            .park_name = selected_name
          )
        }
        selected_types <- selected_feature_types(input$view_feature_type)
        park_name_points <- selected_points %>% filter(is_park_name_point)
        selected_points <- selected_points %>% filter(!is_park_name_point)
        if (length(selected_types) > 0) {
          selected_points <- selected_points %>% filter(feature_type %in% selected_types)
        } else {
          selected_points <- selected_points %>% slice(0)
        }
        selected_points <- bind_rows(selected_points, park_name_points)
      }
    }
    
    selected_points <- selected_points %>%
      mutate(
        latitude = suppressWarnings(as.numeric(unlist(latitude, use.names = FALSE))),
        longitude = suppressWarnings(as.numeric(unlist(longitude, use.names = FALSE))),
        entity_name = as.character(unlist(entity_name, use.names = FALSE)),
        feature_type = as.character(unlist(feature_type, use.names = FALSE))
      ) %>%
      filter(
        is.finite(latitude),
        is.finite(longitude),
        !is.na(entity_name),
        nzchar(entity_name),
        !is.na(feature_type),
        nzchar(feature_type)
      )
    
    base_map <- if (!is.null(selected_name) && nzchar(selected_name)) {
      map_data %>% filter(name == selected_name)
    } else {
      map_data
    }
    
    selected_boundary_shape <- tibble(
      park_code = character(),
      park_name = character(),
      boundary_coords = list(),
      min_lng = numeric(),
      min_lat = numeric(),
      max_lng = numeric(),
      max_lat = numeric()
    )
    if (!is.null(selected_name) && nzchar(selected_name)) {
      selected_row <- park_catalog %>% filter(name == selected_name) %>% slice(1)
      if (nrow(selected_row) > 0) {
        selected_boundary_shape <- get_boundary_shape_for_park(selected_row)
        if (nrow(selected_boundary_shape) == 0) {
          log_map_debug(
            "No boundary available for map render park='", selected_name, "'",
            .park_code = selected_row$park_code[[1]],
            .park_name = selected_name
          )
        }
      }
    }
    
    selected_center <- if (nrow(base_map) > 0) {
      list(
        lng = base_map$longitude[[1]],
        lat = base_map$latitude[[1]]
      )
    } else {
      list(lng = -98.5, lat = 39.8)
    }
    if (!is.finite(selected_center$lng) || !is.finite(selected_center$lat)) {
      selected_center <- list(lng = -98.5, lat = 39.8)
    }
    
    map_obj <- leaflet(base_map) %>%
      addTiles(options = tileOptions(noWrap = TRUE)) %>%
      setView(lng = selected_center$lng, lat = selected_center$lat, zoom = 8) %>%
      {
        if (nrow(selected_boundary_shape) > 0) {
          shape_paths <- selected_boundary_shape$boundary_paths[[1]]
          if (is.null(shape_paths) || length(shape_paths) == 0) {
            shape_coords <- selected_boundary_shape$boundary_coords[[1]]
            shape_paths <- list(shape_coords %>% filter(!is.na(lng), !is.na(lat)))
          }
          reduce(shape_paths, function(map_acc, path) {
            addPolygons(
              map_acc,
              lng = path$lng,
              lat = path$lat,
              stroke = TRUE,
              color = "#006d2c",
              weight = 2,
              opacity = 0.9,
              fillColor = "#31a354",
              fillOpacity = 0.25,
              group = "park_boundary",
              popup = selected_boundary_shape$park_name[[1]]
            )
          }, .init = .)
        } else .
      } %>%
      addCircleMarkers(
        data = base_map,
        layerId = ~name,
        lng = ~longitude,
        lat = ~latitude,
        radius = 7,
        color = "#1f78b4",
        fillColor = "#1f78b4",
        fillOpacity = 0.85,
        popup = ~popup_html,
        label = ~name
      ) %>%
      addCircleMarkers(
        data = selected_points %>% filter(!is_park_name_point),
        lng = ~longitude,
        lat = ~latitude,
        radius = 5,
        stroke = FALSE,
        fillOpacity = 0.9,
        fillColor = ~park_type_palette(feature_type),
        color = ~park_type_palette(feature_type),
        popup = ~paste0("<b>", entity_name, "</b><br>", feature_type),
        group = "park_features"
      ) %>%
      {
        if (nrow(selected_points) > 0) {
          addLabelOnlyMarkers(
            .,
            data = selected_points,
            lng = ~longitude,
            lat = ~latitude,
            label = ~entity_name,
            labelOptions = labelOptions(
              noHide = TRUE,
              direction = "top",
              textOnly = TRUE,
              style = list(
                "font-size" = "11px",
                "font-weight" = "600",
                "color" = "#333333",
                "background-color" = "rgba(255,255,255,0.75)",
                "padding" = "2px 4px",
                "border-radius" = "3px"
              )
            ),
            group = "park_feature_labels"
          )
        } else .
      } %>%
      {
        legend_values <- selected_points %>%
          filter(!is_park_name_point) %>%
          pull(feature_type)
        if (length(legend_values) > 0) {
          addLegend(
            .,
            "bottomright",
            pal = park_type_palette,
            values = legend_values,
            title = "Feature Type",
            opacity = 1
          )
        } else .
      }
    if (nrow(selected_boundary_shape) > 0) {
      lng_pad <- pmax((selected_boundary_shape$max_lng[[1]] - selected_boundary_shape$min_lng[[1]]) * 0.06, 0.005)
      lat_pad <- pmax((selected_boundary_shape$max_lat[[1]] - selected_boundary_shape$min_lat[[1]]) * 0.06, 0.005)
      
      map_obj %>%
        fitBounds(
          lng1 = selected_boundary_shape$min_lng[[1]] - lng_pad,
          lat1 = selected_boundary_shape$min_lat[[1]] - lat_pad,
          lng2 = selected_boundary_shape$max_lng[[1]] + lng_pad,
          lat2 = selected_boundary_shape$max_lat[[1]] + lat_pad
        )
    } else {
      map_obj
    }
  })
  
  observeEvent(input$view_park_map_marker_click, {
    clicked <- input$view_park_map_marker_click$id
    if (!is.null(clicked) && nzchar(clicked)) {
      selected_view_park(clicked)
      updateSelectInput(session, "view_park_choice", selected = clicked)
    }
  })
  
  output$view_park_summary <- renderUI({
    park_name <- selected_view_park()
    req(!is.null(park_name))
    if (!nzchar(park_name)) {
      return(HTML("<em>Select a park from the dropdown to view details.</em>"))
    }
    selected_row <- park_catalog %>% filter(name == park_name) %>% slice(1)
    req(nrow(selected_row) == 1)
    HTML(build_park_popup(selected_row, park_images))
  })
  
  output$view_park_activities <- renderDT({
    park_name <- selected_view_park()
    req(!is.null(park_name))
    if (!nzchar(park_name)) {
      return(datatable(data.frame(Message = "Select a park to view activities."), options = list(dom = "t"), rownames = FALSE))
    }
    park_code <- park_catalog %>% filter(name == park_name) %>% pull(park_code) %>% .[1]
    dat <- park_activities %>%
      filter(tolower(Park_Code) == tolower(park_code)) %>%
      distinct(Activity_Name, .keep_all = TRUE) %>%
      select(Activity_Name)
    datatable(
      dat,
      options = list(
        pageLength = 8,
        dom = "t<'dt-footer'ip>",
        scrollY = "350px",
        scrollCollapse = FALSE,
        columnDefs = list(list(targets = "_all", className = "dt-wrap"))
      ),
      rownames = FALSE
    )
  })
  
  output$view_park_things_to_do <- renderDT({
    park_name <- selected_view_park()
    req(!is.null(park_name))
    if (!nzchar(park_name)) {
      return(datatable(data.frame(Message = "Select a park to view things to do."), options = list(dom = "t"), rownames = FALSE))
    }
    park_code <- park_catalog %>% filter(name == park_name) %>% pull(park_code) %>% .[1]
    dat <- park_things_to_do %>%
      filter(tolower(Park_Code) == tolower(park_code)) %>%
      select(any_of(c("Title", "Duration", "Short_Description"))) %>%
      mutate(
        Duration = ifelse(
          !is.na(Duration) & grepl("^\\s*\\d+(?:\\.\\d+)?\\s*$", Duration, perl = TRUE),
          paste0(trimws(Duration), " hours"),
          Duration
        )
      )
    title_target <- which(names(dat) == "Title") - 1
    duration_target <- which(names(dat) == "Duration") - 1
    short_desc_target <- which(names(dat) == "Short_Description") - 1
    things_col_defs <- list(list(targets = "_all", className = "dt-wrap"))
    if (length(title_target) == 1 && title_target >= 0) {
      things_col_defs <- append(
        things_col_defs,
        list(list(targets = title_target, width = "25%"))
      )
    }
    if (length(duration_target) == 1 && duration_target >= 0) {
      things_col_defs <- append(
        things_col_defs,
        list(list(targets = duration_target, className = "dt-nowrap", width = "15%"))
      )
    }
    if (length(short_desc_target) == 1 && short_desc_target >= 0) {
      things_col_defs <- append(
        things_col_defs,
        list(list(targets = short_desc_target, width = "60%"))
      )
    }
    datatable(
      dat,
      options = list(
        pageLength = 2,
        dom = "t<'dt-footer'ip>",
        scrollY = "350px",
        scrollCollapse = FALSE,
        columnDefs = things_col_defs
      ),
      rownames = FALSE
    )
  })
  
  output$view_feature_message <- renderUI({
    park_name <- selected_view_park()
    req(!is.null(park_name))
    if (!nzchar(park_name)) return(NULL)
    
    selected_types <- selected_feature_types(input$view_feature_type)
    if (length(selected_types) == 0) {
      return(tags$div(
        style = "padding: 8px 10px; margin: 4px 0 8px 0; background-color: #fff3cd; border: 1px solid #ffe69c; border-radius: 4px; color: #664d03;",
        "No feature types selected. Check one or more feature types to display park features."
      ))
    }
    
    park_row <- park_catalog %>% filter(name == park_name) %>% slice(1)
    available_types <- filter_outline_for_park(park_outline, park_row) %>%
      pull(feature_type) %>%
      unique()
    
    missing_types <- setdiff(selected_types, available_types)
    if (length(missing_types) == 0) return(NULL)
    
    tags$div(
      style = "padding: 8px 10px; margin: 4px 0 8px 0; background-color: #fff3cd; border: 1px solid #ffe69c; border-radius: 4px; color: #664d03;",
      paste0("Not available on this park location: ", paste(missing_types, collapse = ", "), ".")
    )
  })
  
  # Park selector map - show all parks initially
  output$park_selector_map <- renderLeaflet({
    selected <- selected_park_names()
    map_data <- park_catalog %>%
      mutate(marker_color = if_else(name %in% selected, "green", "red")) %>%
      add_popup_html()
    
    leaflet() %>%
      addTiles(options = tileOptions(noWrap = TRUE)) %>%
      setView(lng = -98.5, lat = 39.8, zoom = 4) %>%
      addCircleMarkers(
        data = map_data,
        layerId = ~name,
        lng = ~longitude,
        lat = ~latitude,
        radius = 7,
        color = ~marker_color,
        fillColor = ~marker_color,
        fillOpacity = 0.85,
        weight = 2,
        label = ~name
      ) %>%
      addLegend(
        position = "bottomright",
        colors = c("green", "red"),
        labels = c("Selected", "Not selected"),
        title = "Park Selection"
      )
  })
  
  # Update map when selection changes
  observe({
    selected <- selected_park_names()
    map_data <- park_catalog %>%
      mutate(marker_color = if_else(name %in% selected, "green", "red")) %>%
      add_popup_html()
    
    leafletProxy("park_selector_map", data = map_data) %>%
      clearMarkers() %>%
      clearControls() %>%
      addCircleMarkers(
        layerId = ~name,
        lng = ~longitude,
        lat = ~latitude,
        radius = 7,
        color = ~marker_color,
        fillColor = ~marker_color,
        fillOpacity = 0.85,
        weight = 2,
        label = ~name
      ) %>%
      addLegend(
        position = "bottomright",
        colors = c("green", "red"),
        labels = c("Selected", "Not selected"),
        title = "Park Selection"
      )
  })
  
  # Handle park click
  observeEvent(input$park_selector_map_marker_click, {
    clicked_park <- input$park_selector_map_marker_click$id
    current <- selected_park_names()
    if (clicked_park %in% current) {
      selected_park_names(setdiff(current, clicked_park))
    } else {
      selected_park_names(c(current, clicked_park))
    }
  })
  
  # Select all parks
  observeEvent(input$select_all_parks, {
    selected_park_names(park_catalog$name)
  })
  
  # Deselect all parks
  observeEvent(input$deselect_all_parks, {
    selected_park_names(character(0))
  })
  
  # Route message
  output$route_message <- renderUI({
    num_selected <- length(selected_park_names())
    
    if (num_selected == 0) {
      return(tags$div(
        style = "padding: 10px; background-color: #f8d7da; border: 1px solid #f5c6cb; border-radius: 4px; color: #721c24;",
        tags$strong("No parks selected."),
        tags$br(),
        "Please select parks from the map above."
      ))
    }
    
    start_text <- if (!is.null(input$start_zip_choice) && nzchar(input$start_zip_choice)) {
      zip_info <- get_zip_coordinates(input$start_zip_choice)
      if (!is.null(zip_info)) {
        paste0(zip_info$city, ", ", zip_info$state)
      } else {
        "first park"
      }
    } else {
      "first park"
    }
    
    effective_end_zip <- if (isTRUE(input$same_as_start) &&
                             !is.null(input$start_zip_choice) &&
                             nzchar(input$start_zip_choice)) {
      input$start_zip_choice
    } else {
      input$end_zip_choice
    }
    
    end_text <- if (!is.null(effective_end_zip) && nzchar(effective_end_zip)) {
      zip_info <- get_zip_coordinates(effective_end_zip)
      if (!is.null(zip_info)) {
        paste0(zip_info$city, ", ", zip_info$state)
      } else {
        "last park"
      }
    } else {
      "last park"
    }
    
    tags$div(
      style = "padding: 10px; background-color: #d1ecf1; border: 1px solid #bee5eb; border-radius: 4px; color: #0c5460;",
      tags$strong(sprintf("Ready to calculate route for %d selected parks", num_selected)),
      tags$br(),
      sprintf("Starting at: %s", start_text),
      tags$br(),
      sprintf("Ending at: %s", end_text)
    )
  })
  
  # Navigation buttons
  observeEvent(input$go_user, {
    updateTabItems(session, "main_tabs", "userinfo")
  })
  
  observeEvent(input$go_allparks, {
    updateTabItems(session, "main_tabs", "allparks")
  })
  
  observeEvent(input$go_viewpark, {
    updateTabItems(session, "main_tabs", "viewpark")
  })
  
  observeEvent(input$go_planner, {
    updateTabItems(session, "main_tabs", "planner")
  })
  
  observeEvent(input$back_to_planner, {
    updateTabItems(session, "main_tabs", "planner")
  })
  
  observeEvent(input$download_route_pdf, {
    # Show the notification in the bottom right
    showNotification("Preparing your PDF... please wait.", 
                     id = "pdf_loading", 
                     duration = NULL, # Keep it visible until we manually remove it
                     type = "message")
    Sys.sleep(2)
    
    # Get current map bounds
    bounds <- input$route_map_bounds
    
    session$sendCustomMessage("download_route_pdf", list(
      filename = "optimal_route.pdf",
      fit_bounds = list(
        list(bounds$south, bounds$west),
        list(bounds$north, bounds$east)
      )
    ))
  })
  
  
  # Distance conversion
  selected_distance_multiplier <- reactive({
    if (input$distance_unit == "miles") 0.621371 else 1
  })
  
  selected_distance_label <- reactive({
    if (input$distance_unit == "miles") "mi" else "km"
  })
  
  # Calculate route
  observeEvent(input$calculate, {
    selected <- selected_park_names()
    computed_route(NULL)
    segment_mode_overrides(character(0))
    park_visit_time_overrides(numeric(0))
    
    if (length(selected) < 2) {
      showNotification("Please select at least 2 parks.", type = "error", duration = 5)
      return()
    }
    
    # Get selected parks data
    withProgress(message = "Calculating optimal route...", value = 0, {
      incProgress(0.15, detail = "Loading selected parks")
      
      # Get selected parks data
      parks_subset <- park_data %>%
        filter(name %in% selected) %>%
        distinct(name, .keep_all = TRUE)
      
      # Parse start location
      start_loc <- NULL
      start_idx <- NULL
      if (!is.null(input$start_zip_choice) && nzchar(input$start_zip_choice)) {
        geo_result <- geocode_location(input$start_zip_choice)
        if (isTRUE(geo_result$success)) {
          start_loc <- list(
            name = "START",
            state = geo_result$source_state,
            latitude = geo_result$latitude,
            longitude = geo_result$longitude,
            description = paste0(geo_result$source_city, ", ", geo_result$source_state),
            date_established = NA,
            area = NA,
            visitors = NA
          )
        } else {
          showNotification(paste("Could not geocode start:", geo_result$message), type = "warning")
        }
      }
      
      # Parse end location
      end_loc <- NULL
      end_idx <- NULL
      effective_end_zip <- if (isTRUE(input$same_as_start) &&
                               !is.null(input$start_zip_choice) &&
                               nzchar(input$start_zip_choice)) {
        input$start_zip_choice
      } else {
        input$end_zip_choice
      }
      
      if (!is.null(effective_end_zip) && nzchar(effective_end_zip)) {
        geo_result <- geocode_location(effective_end_zip)
        if (isTRUE(geo_result$success)) {
          end_loc <- list(
            name = "END",
            state = geo_result$source_state,
            latitude = geo_result$latitude,
            longitude = geo_result$longitude,
            description = paste0(geo_result$source_city, ", ", geo_result$source_state),
            date_established = NA,
            area = NA,
            visitors = NA
          )
        } else {
          showNotification(paste("Could not geocode end:", geo_result$message), type = "warning")
        }
      }
      
      # Build locations dataframe
      locations_list <- list()
      idx <- 1
      
      if (!is.null(start_loc)) {
        locations_list[[idx]] <- as.data.frame(start_loc, stringsAsFactors = FALSE)
        start_idx <- idx
        idx <- idx + 1
      }
      
      for (i in seq_len(nrow(parks_subset))) {
        locations_list[[idx]] <- parks_subset[i, ]
        idx <- idx + 1
      }
      
      if (!is.null(end_loc)) {
        locations_list[[idx]] <- as.data.frame(end_loc, stringsAsFactors = FALSE)
        end_idx <- idx
      }
      
      locations <- bind_rows(locations_list)
      
      incProgress(0.35, detail = "Building travel matrices")
      # Compute distance and time matrices
      base_mode_matrix <- build_mode_matrix(locations, input$distance_type)
      road_dist_miles <- NULL
      if (nzchar(google_api_key) && input$distance_type != "flying") {
        incProgress(0.15, detail = "Using Google Distance Matrix for driving segments")
        road_dist_miles <- tryCatch(
          build_road_dist_matrix(locations, google_api_key),
          error = function(e) {
            showNotification("Google Distance Matrix failed; using great-circle fallback.", type = "warning", duration = 5)
            NULL
          }
        )
      }
      matrices <- compute_distance_matrix(locations, mode_matrix = base_mode_matrix, road_dist_miles = road_dist_miles)
      dist_mat <- matrices$distance
      time_mat <- matrices$time
      
      # Determine segment constraint
      segment_mode <- input$limit_metric
      max_segment_value <- Inf
      
      if (segment_mode == "time" && !is.null(input$limit_value) && is.numeric(input$limit_value)) {
        max_segment_value <- input$limit_value
      } else if (segment_mode == "distance" && !is.null(input$limit_value) && is.numeric(input$limit_value)) {
        max_segment_value <- input$limit_value
        if (input$distance_unit == "miles") {
          max_segment_value <- max_segment_value / 0.621371  # Convert to km
        }
      } else if (segment_mode == "none") {
        segment_mode <- "distance"
        max_segment_value <- Inf
      }
      
      # Overall limits
      overall_time_limit <- if (!is.na(input$overall_time_limit)) input$overall_time_limit else Inf
      overall_distance_limit <- if (!is.na(input$overall_distance_limit)) {
        if (input$distance_unit == "miles") {
          input$overall_distance_limit / 0.621371  # Convert to km
        } else {
          input$overall_distance_limit
        }
      } else {
        Inf
      }
      
      # Build an array of visit times matching the locations dataframe
      visit_times_array <- sapply(locations$name, function(loc_name) {
        if (loc_name %in% c("START", "END")) return(0)
        
        # If the user has already entered manual overrides before hitting calculate
        override_val <- get_named_value(park_visit_time_overrides(), loc_name)
        if (!is.null(override_val) && is.finite(override_val) && override_val >= 0) {
          return(as.numeric(override_val))
        }
        
        # Otherwise use the default
        return(get_default_visit_hours(loc_name))
      })
      
      incProgress(0.25, detail = "Optimizing route order")
      # Solve TSP
      ordered_indices <- tsp_solver(
        dist_mat,
        start_idx = start_idx,
        end_idx = end_idx,
        num_stops = NULL,  # Use all selected parks
        time_matrix = time_mat,
        segment_mode = segment_mode,
        max_segment_value = max_segment_value,
        overall_time_limit = overall_time_limit,
        overall_distance_limit = overall_distance_limit,
        node_visit_times = visit_times_array
      )
      
      incProgress(0.2, detail = "Resolving segment geometry")
      # Generate route with geometry
      result <- route_with_geometry(ordered_indices, locations, mode_matrix = base_mode_matrix)
      
      # Enforce overall limits using computed segment times/distances (includes start/end travel when provided)
      has_time_limit <- is.finite(overall_time_limit)
      has_distance_limit <- is.finite(overall_distance_limit)
      must_keep <- unique(na.omit(c(start_idx, end_idx)))
      
      while ((has_time_limit && result$total_time > overall_time_limit) ||
             (has_distance_limit && result$total_distance > overall_distance_limit)) {
        removable_positions <- which(!ordered_indices %in% must_keep)
        if (length(removable_positions) == 0) break
        if (length(ordered_indices) <= length(must_keep) + 1) break
        
        marginal_cost <- sapply(removable_positions, function(pos) {
          left <- if (pos > 1) ordered_indices[pos - 1] else NA_integer_
          center <- ordered_indices[pos]
          right <- if (pos < length(ordered_indices)) ordered_indices[pos + 1] else NA_integer_
          remove_dist <- 0
          remove_time <- 0
          add_dist <- 0
          add_time <- 0
          
          if (!is.na(left)) {
            remove_dist <- remove_dist + dist_mat[left, center]
            remove_time <- remove_time + time_mat[left, center]
          }
          if (!is.na(right)) {
            remove_dist <- remove_dist + dist_mat[center, right]
            remove_time <- remove_time + time_mat[center, right]
          }
          if (!is.na(left) && !is.na(right)) {
            add_dist <- dist_mat[left, right]
            add_time <- time_mat[left, right]
          }
          (remove_dist - add_dist) + (remove_time - add_time)
        })
        
        drop_pos <- removable_positions[which.max(marginal_cost)]
        ordered_indices <- ordered_indices[-drop_pos]
        result <- route_with_geometry(ordered_indices, locations, mode_matrix = base_mode_matrix)
      }
      
      result$ordered_indices <- ordered_indices
      result$locations <- locations
      result$default_distance_type <- input$distance_type
      result$base_mode_matrix <- base_mode_matrix
      result$has_start <- !is.null(start_loc)
      result$has_end <- !is.null(end_loc)
      
      parks_in_route <- result$route %>% filter(!name %in% c("START", "END"))
      num_route_parks <- nrow(parks_in_route)
      
      if (num_route_parks <= 2) {
        pending_route(result)
        if (num_route_parks == 0) {
          showModal(modalDialog(
            title = "No parks fit your current limits",
            "The current constraints remove all selected parks from the route. Please change your limits and recalculate.",
            footer = tagList(
              modalButton("Close"),
              actionButton("change_limits_route", "Change Limits", class = "btn-warning")
            ),
            easyClose = TRUE
          ))
        } else {
          showModal(modalDialog(
            title = sprintf("Only %d park%s fit the current limits", num_route_parks, ifelse(num_route_parks == 1, "", "s")),
            "You can continue with this shorter route or go back and adjust your limits.",
            footer = tagList(
              actionButton("continue_limited_route", "Continue", class = "btn-success"),
              actionButton("change_limits_route", "Change Limits", class = "btn-warning")
            ),
            easyClose = TRUE
          ))
        }
        return()
      }
      
      segment_mode_overrides(character(0))
      park_visit_time_overrides(numeric(0))
      
      incProgress(0.05, detail = "Finalizing")
      computed_route(result)
      
      showNotification("Route calculated successfully!", type = "message", duration = 3)
      updateTabItems(session, "main_tabs", "optimalroute")
    })
  })
  
  observeEvent(input$continue_limited_route, {
    pending <- pending_route()
    req(!is.null(pending))
    removeModal()
    pending_route(NULL)
    segment_mode_overrides(character(0))
    park_visit_time_overrides(numeric(0))
    computed_route(pending)
    showNotification("Route calculated successfully!", type = "message", duration = 3)
    updateTabItems(session, "main_tabs", "optimalroute")
  })
  
  observeEvent(input$change_limits_route, {
    removeModal()
    pending_route(NULL)
    updateTabItems(session, "main_tabs", "planner")
  })
  
  # Info boxes
  output$total_time_box <- renderInfoBox({
    result <- computed_route()
    if (is.null(result)) {
      infoBox("Total Time", "--", "Travel + park visits + personal time",
              icon = icon("hourglass-half"), color = "aqua")
    } else {
      travel_hours <- result$total_time
      parks_only <- result$route %>% filter(!name %in% c("START", "END"))
      visit_overrides <- park_visit_time_overrides()
      park_hours <- if (nrow(parks_only) > 0) {
        sum(sapply(parks_only$name, function(park_name) {
          value <- get_named_value(visit_overrides, park_name)
          if (is.null(value) || !is.finite(value) || value < 0) get_default_visit_hours(park_name) else as.numeric(value)
        }))
      } else { 0 }
      total_active_time <- travel_hours + park_hours
      total_rest_hours <- floor(total_active_time / 14) * 10
      total_hours <- total_active_time + total_rest_hours
      
      days <- floor(total_hours / 24)
      hours <- round(total_hours %% 24, 1)
      total_str <- if (days > 0) {
        sprintf("%d day%s, %.1fh", days, if(days > 1) "s" else "", hours)
      } else {
        sprintf("%.1f hours", hours)
      }
      
      infoBox("Total Time", total_str, "Travel + park visits + personal time",
              icon = icon("hourglass-half"), color = "aqua")
    }
  })
  
  output$total_distance_box <- renderInfoBox({
    result <- computed_route()
    if (is.null(result)) {
      infoBox("Total Distance", "--", "Click Calculate",
              icon = icon("road"), color = "blue")
    } else {
      dist_value <- result$total_distance * selected_distance_multiplier()
      infoBox("Total Distance",
              sprintf("%.0f %s", dist_value, selected_distance_label()),
              "Complete route", icon = icon("road"), color = "blue")
    }
  })
  
  output$travel_time_box <- renderInfoBox({
    result <- computed_route()
    if (is.null(result)) {
      infoBox("Travel Time", "--", "Route travel and overnight stays only",
              icon = icon("clock"), color = "green")
    } else {
      days <- floor(result$total_time / 24)
      hours <- round(result$total_time %% 24, 1)
      time_str <- if (days > 0) {
        sprintf("%d day%s, %.1fh", days, if(days > 1) "s" else "", hours)
      } else {
        sprintf("%.1f hours", hours)
      }
      infoBox("Travel Time", time_str, "Travel segments only",
              icon = icon("clock"), color = "green")
    }
  })
  
  output$park_visit_time_box <- renderInfoBox({
    result <- computed_route()
    if (is.null(result)) {
      infoBox("Park Visit Time", "--", "Estimated park time",
              icon = icon("hiking"), color = "teal")
    } else {
      parks_only <- result$route %>% filter(!name %in% c("START", "END"))
      visit_overrides <- park_visit_time_overrides()
      visit_total <- if (nrow(parks_only) > 0) {
        sum(sapply(parks_only$name, function(park_name) {
          value <- get_named_value(visit_overrides, park_name)
          if (is.null(value) || !is.finite(value) || value < 0) get_default_visit_hours(park_name) else as.numeric(value)
        }))
      } else {
        0
      }
      days <- floor(visit_total / 24)
      hours <- round(visit_total %% 24, 1)
      visit_str <- if (days > 0) {
        sprintf("%d day%s, %.1fh", days, if(days > 1) "s" else "", hours)
      } else {
        sprintf("%.1f hours", hours)
      }
      infoBox("Park Visit Time", visit_str, "Estimated time at parks",
              icon = icon("hiking"), color = "teal")
    }
  })
  
  output$num_stops_box <- renderInfoBox({
    result <- computed_route()
    if (is.null(result)) {
      infoBox("Parks", "--", "Click Calculate",
              icon = icon("map-marker-alt"), color = "yellow")
    } else {
      n_parks <- nrow(result$route) - ifelse(isTRUE(result$has_start), 1, 0) - ifelse(isTRUE(result$has_end), 1, 0)
      infoBox("Parks to Visit", n_parks, sprintf("%d stop%s", n_parks, if(n_parks > 1) "s" else ""),
              icon = icon("map-marker-alt"), color = "yellow")
    }
  })
  
  output$avg_distance_box <- renderInfoBox({
    result <- computed_route()
    if (is.null(result)) {
      infoBox("Avg Between Stops", "--", "Click Calculate",
              icon = icon("arrows-alt-h"), color = "red")
    } else {
      avg_dist <- (result$total_distance / (nrow(result$route) - 1)) * selected_distance_multiplier()
      avg_time <- result$total_time / (nrow(result$route) - 1)
      infoBox("Avg Between Stops",
              sprintf("%.0f %s\n%.1fh", avg_dist, selected_distance_label(), avg_time),
              "Per segment", icon = icon("arrows-alt-h"), color = "red")
    }
  })
  
  trip_cost_breakdown <- reactive({
    result <- computed_route()
    if (is.null(result)) return(NULL)
    current_gas_data <- gas_data()
    if (is.null(current_gas_data) || nrow(current_gas_data) == 0) return(NULL)
    route <- result$route
    if (is.null(route) || nrow(route) < 2) return(NULL)
    
    traveler_count <- if (is.null(input$num_travelers) || !is.finite(input$num_travelers)) 1 else max(1, as.numeric(input$num_travelers))
    mpg <- if (is.null(input$car_mpg) || !is.finite(input$car_mpg)) 29 else max(1, as.numeric(input$car_mpg))
    vehicle_type <- ifelse(is.null(input$vehicle_type) || !nzchar(input$vehicle_type), "car", input$vehicle_type)
    travel_date <- {
      td <- input$travel_dates
      if (is.null(td) || length(td) == 0) Sys.Date() else as.Date(td[[1]])
    }
    preferred_airlines <- input$airline_preference
    
    drive_rows <- route %>% filter(travel_mode == "Drive", !is.na(distance_to_next_km))
    drive_miles <- sum(drive_rows$distance_to_next_km, na.rm = TRUE) * 0.621371
    
    segment_states <- drive_rows$state
    if (length(segment_states) == 0) {
      gas_price_per_gallon <- mean(current_gas_data$Regular, na.rm = TRUE)
    } else {
      state_prices <- current_gas_data %>%
        filter(State %in% segment_states) %>%
        pull(Regular)
      gas_price_per_gallon <- if (length(state_prices) == 0) {
        mean(current_gas_data$Regular, na.rm = TRUE)
      } else {
        mean(state_prices, na.rm = TRUE)
      }
    }
    gas_cost <- (drive_miles / mpg) * gas_price_per_gallon
    
    flight_rows <- route %>%
      mutate(row_idx = row_number()) %>%
      filter(travel_mode == "Flight", !is.na(segment_flight_distance_km))
    flight_total <- 0
    if (nrow(flight_rows) > 0) {
      for (i in seq_len(nrow(flight_rows))) {
        origin_idx <- as.integer(flight_rows$row_idx[i])
        dest_idx <- origin_idx + 1
        if (!is.finite(origin_idx) || dest_idx > nrow(route)) next
        origin <- route[origin_idx, ]
        dest <- route[dest_idx, ]
        airport_from <- find_nearest_airport_cached(origin$latitude, origin$longitude)
        airport_to <- find_nearest_airport_cached(dest$latitude, dest$longitude)
        flight_cost_info <- estimate_flight_segment_cost(
          origin_iata = airport_from$airport$iata_code,
          dest_iata = airport_to$airport$iata_code,
          dist_km = as.numeric(flight_rows$segment_flight_distance_km[i]),
          travel_date = travel_date,
          preferred_airlines = preferred_airlines
        )
        flight_total <- flight_total + flight_cost_info$cost
      }
    }
    flight_total <- flight_total * traveler_count
    
    park_stops <- route %>%
      filter(!name %in% c("START", "END")) %>%
      mutate(park_code = normalize_park_code(park_code)) %>%
      left_join(park_cost_data, by = "park_code")
    
    park_cost_total <- 0
    if (nrow(park_stops) > 0) {
      vehicle_col <- if (vehicle_type == "motorcycle") "entrance_fee_motorcycle" else "entrance_fee_vehicle"
      park_cost_total <- park_stops %>%
        mutate(
          vehicle_fee = as.numeric(.data[[vehicle_col]]),
          person_fee = as.numeric(entrance_fee_person),
          stop_fee = case_when(
            !is.na(vehicle_fee) & vehicle_fee > 0 ~ vehicle_fee,
            !is.na(person_fee) & person_fee > 0 ~ person_fee * traveler_count,
            TRUE ~ 0
          )
        ) %>%
        summarise(total = sum(stop_fee, na.rm = TRUE)) %>%
        pull(total)
    }
    
    total_cost <- gas_cost + flight_total + park_cost_total
    list(
      total_cost = total_cost,
      gas_cost = gas_cost,
      flight_cost = flight_total,
      park_cost = park_cost_total
    )
  })
  
  output$estimated_cost_box <- renderInfoBox({
    cost <- trip_cost_breakdown()
    if (is.null(cost)) {
      infoBox("Estimated Cost", "--", "Click Calculate",
              icon = icon("dollar-sign"), color = "purple")
    } else {
      infoBox(
        "Estimated Cost",
        paste0("$", format(round(cost$total_cost, 0), big.mark = ",")),
        paste0(
          "Gas: $", format(round(cost$gas_cost, 0), big.mark = ","),
          " | Flights: $", format(round(cost$flight_cost, 0), big.mark = ","),
          " | Park Fees: $", format(round(cost$park_cost, 0), big.mark = ","),
          " | Note: Excludes hotels, food, and tolls"
        ),
        icon = icon("dollar-sign"),
        color = "purple"
      )
    }
  })
  
  # Estimated Personal Time (Rest & Meals)
  output$personal_time_box <- renderInfoBox({
    result <- computed_route()
    if (is.null(result)) {
      infoBox("Personal Time", "--", "Rest, sleep, and meals",
              icon = icon("bed"), color = "purple")
    } else {
      # 1. Get total travel hours
      travel_hours <- result$total_time
      
      # 2. Get total park hours (using your existing logic)
      parks_only <- result$route %>% filter(!name %in% c("START", "END"))
      visit_overrides <- park_visit_time_overrides()
      park_hours <- if (nrow(parks_only) > 0) {
        sum(sapply(parks_only$name, function(park_name) {
          value <- get_named_value(visit_overrides, park_name)
          if (is.null(value) || !is.finite(value) || value < 0) 4 else as.numeric(value)
        }))
      } else { 0 }
      
      # 3. Calculate total active time and the resulting rest time
      total_active_time <- travel_hours + park_hours
      # 10 hours of rest for every 14 hours of activity
      num_rest_periods <- floor(total_active_time / 14)
      total_rest_hours <- num_rest_periods * 10
      
      days <- floor(total_rest_hours / 24)
      hours <- round(total_rest_hours %% 24, 1)
      
      rest_str <- if (days > 0) {
        sprintf("%d day%s, %.1fh", days, if(days > 1) "s" else "", hours)
      } else {
        sprintf("%.1f hours", hours)
      }
      
      infoBox("Personal Time", rest_str, "10h rest per 14h active time",
              icon = icon("bed"), color = "purple")
    }
  })
  
  # Estimated Overnight Stays
  output$overnight_stays_box <- renderInfoBox({
    result <- computed_route()
    if (is.null(result)) {
      infoBox("Overnight Stays", "--", "Total nights on trip",
              icon = icon("moon"), color = "navy")
    } else {
      # Get total active hours (travel + parks)
      travel_hours <- result$total_time
      
      parks_only <- result$route %>% filter(!name %in% c("START", "END"))
      visit_overrides <- park_visit_time_overrides()
      park_hours <- if (nrow(parks_only) > 0) {
        sum(sapply(parks_only$name, function(park_name) {
          value <- get_named_value(visit_overrides, park_name)
          if (is.null(value) || !is.finite(value) || value < 0) 4 else as.numeric(value)
        }))
      } else { 0 }
      
      total_active_time <- travel_hours + park_hours
      
      # Calculate nights based on the 14-hour activity threshold
      num_nights <- floor(total_active_time / 14)
      
      infoBox("Overnight Stays", 
              paste(num_nights, if(num_nights == 1) "Night" else "Nights"), 
              "Based on 14h activity limit",
              icon = icon("moon"), color = "navy")
    }
  })
  
  output$states_visited_box <- renderInfoBox({
    result <- computed_route()
    
    if (is.null(result) || is.null(result$route)) {
      infoBox("States Visited", "--", "States along your route",
              icon = icon("map-location-dot"), color = "maroon")
    } else {
      # 1. Get the list of locations in the actual optimized order
      # Filter out 'START' and 'END' if they are generic coordinates without state data
      route_locations <- result$route %>% 
        filter(!name %in% c("START", "END"))
      
      # 2. Extract states. 
      # We use result$locations to ensure we match the correct state metadata
      all_states_raw <- route_locations$state
      
      # 3. Clean and Split (Handles "WY, MT, ID" format)
      unique_states <- all_states_raw %>%
        str_split(",") %>%            # Split by comma
        unlist() %>%                  # Flatten list
        str_trim() %>%                # Remove whitespace
        unique() %>%                  # Keep unique values
        subset(. != "" & !is.na(.))   # Remove empty/NA values
      
      num_states <- length(unique_states)
      state_list <- paste(sort(unique_states), collapse = ", ")
      
      infoBox(
        "States Visited", 
        paste(num_states, if(num_states == 1) "State" else "States"),
        subtitle = if(num_states > 0) state_list else "No states identified",
        icon = icon("map-location-dot"), 
        color = "maroon"
      )
    }
  })
  
  # Route table
  output$route_table <- renderDT({
    result <- computed_route()
    if (is.null(result)) {
      return(data.frame(Message = "Click 'Calculate Optimal Route' to begin"))
    }
    
    route <- result$route
    visit_overrides <- park_visit_time_overrides()
    rows <- list()
    row_id <- 1
    
    for (i in seq_len(nrow(route))) {
      current_name <- route$name[i]
      
      current_label <- if (current_name == "START") {
        ifelse(!is.na(route$description[i]) && nzchar(route$description[i]), route$description[i], "Starting Location")
      } else if (current_name == "END") {
        ifelse(!is.na(route$description[i]) && nzchar(route$description[i]), route$description[i], "Ending Location")
      } else {
        paste0(current_name, " (", route$state[i], ")")
      }
      
      # --- Park Visit Row ---
      if (!current_name %in% c("START", "END")) {
        visit_hours <- get_named_value(visit_overrides, current_name)
        if (is.null(visit_hours) || !is.finite(visit_hours) || visit_hours < 0) {
          visit_hours <- get_default_visit_hours(current_name)
        }
        
        rows[[row_id]] <- data.frame(
          Type = "Park Visit",
          `Route Step` = current_label,
          `Travel Mode` = NA_character_,
          Distance = NA_real_,
          `Travel/Visit Time (hrs)` = round(as.numeric(visit_hours), 2),
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
        row_id <- row_id + 1
      }
      
      # --- Travel Segment Row ---
      if (i < nrow(route)) {
        next_name <- route$name[i + 1]
        next_label <- if (next_name %in% c("START", "END")) {
          next_name
        } else {
          paste0(next_name, " (", route$state[i + 1], ")")
        }
        
        rows[[row_id]] <- data.frame(
          Type = "Travel",
          `Route Step` = paste0(current_label, " -> ", next_label),
          `Travel Mode` = route$travel_mode[i],
          Distance = round(route$distance_to_next_km[i] * selected_distance_multiplier(), 1),
          `Travel/Visit Time (hrs)` = round(route$time_to_next_hours[i], 2),
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
        row_id <- row_id + 1
      }
    }
    
    display_df <- dplyr::bind_rows(rows)
    
    # Rename distance column with units
    names(display_df)[names(display_df) == "Distance"] <- 
      paste0("Distance (", selected_distance_label(), ")")
    
    datatable(
      display_df,
      options = list(
        paging = FALSE,
        searching = FALSE,
        info = FALSE,
        ordering = FALSE,
        autoWidth = FALSE,
        dom = 't',
        scrollX = FALSE
      ),
      rownames = FALSE,
      escape = FALSE,
      class = "compact"
    )
  })
  
  # Driving Directions UI
  output$driving_directions_ui <- renderUI({
    result <- computed_route()
    if (is.null(result) || nrow(result$route) < 2) {
      return(HTML("<p>Calculate a route to see turn-by-turn directions.</p>"))
    }
    
    route <- result$route
    directions_html <- c()
    
    api_key <- google_api_key
    if (!nzchar(api_key)) {
      return(HTML("<p><em>A valid Google Maps API key is required to fetch detailed turn-by-turn driving directions.</em></p>"))
    }
    
    for (i in seq_len(nrow(route) - 1)) {
      from_loc <- route[i, ]
      to_loc <- route[i + 1, ]
      mode <- route$travel_mode[i]
      
      directions_html <- c(directions_html, paste0("<h4 style='margin-top:20px; color:#2c3e50;'>Segment ", i, ": ", from_loc$name, " to ", to_loc$name, " (", mode, ")</h4>"))
      
      if (mode == "Drive") {
        origin_str <- paste(from_loc$latitude, from_loc$longitude, sep = ",")
        dest_str <- paste(to_loc$latitude, to_loc$longitude, sep = ",")
        
        res <- tryCatch({
          google_directions(origin = origin_str, destination = dest_str, key = api_key)
        }, error = function(e) NULL)
        
        if (!is.null(res) && res$status == "OK") {
          steps_df <- res$routes$legs[[1]]$steps[[1]]
          if (is.data.frame(steps_df) && "html_instructions" %in% names(steps_df)) {
            instr <- steps_df$html_instructions
            dist <- if ("distance" %in% names(steps_df)) steps_df$distance$text else ""
            step_items <- paste0("<li style='margin-bottom:8px;'>", instr, " - <strong>", dist, "</strong></li>")
            directions_html <- c(directions_html, "<ol style='padding-left:20px;'>", step_items, "</ol>")
          } else {
            directions_html <- c(directions_html, "<p>No detailed steps found.</p>")
          }
        } else {
          directions_html <- c(directions_html, "<p>Directions unavailable for this segment.</p>")
        }
      } else {
        directions_html <- c(directions_html, "<p><em>Flight segment. Driving directions not applicable.</em></p>")
      }
    }
    
    HTML(paste(directions_html, collapse = "\n"))
  })
  
  # Route map
  output$route_map <- renderLeaflet({
    leaflet() %>%
      addTiles(options = tileOptions(noWrap = TRUE)) %>%
      setView(lng = -95, lat = 39, zoom = 4) %>%
      addControl(
        html = "<div style='background: rgba(255,255,255,0.92); padding: 8px 10px; border-radius: 4px;'>
                <strong>Route map ready.</strong><br/>Configure your trip and click <em>Calculate Optimal Route</em>.
                </div>",
        position = "topright"
      )
  })
  
  observe({
    result <- computed_route()
    map_proxy <- leafletProxy("route_map")
    map_proxy %>% clearShapes() %>% clearMarkers() %>% clearControls()
    
    if (is.null(result)) {
      route_map_bounds(NULL)
      map_proxy %>%
        addControl(
          html = "<div style='background: rgba(255,255,255,0.92); padding: 8px 10px; border-radius: 4px;'>
                    <strong>Route map ready.</strong><br/>Configure your trip and click <em>Calculate Optimal Route</em>.
                    </div>",
          position = "topright"
        )
      return()
    }
    
    route <- result$route
    valid_route_coords <- route %>%
      filter(is.finite(longitude), is.finite(latitude))
    if (nrow(valid_route_coords) > 0) {
      lng_pad <- pmax((max(valid_route_coords$longitude) - min(valid_route_coords$longitude)) * 0.08, 0.05)
      lat_pad <- pmax((max(valid_route_coords$latitude) - min(valid_route_coords$latitude)) * 0.08, 0.05)
      bounds <- list(
        c(min(valid_route_coords$latitude) - lat_pad, min(valid_route_coords$longitude) - lng_pad),
        c(max(valid_route_coords$latitude) + lat_pad, max(valid_route_coords$longitude) + lng_pad)
      )
      route_map_bounds(bounds)
      map_proxy <- map_proxy %>%
        fitBounds(
          lng1 = bounds[[1]][2],
          lat1 = bounds[[1]][1],
          lng2 = bounds[[2]][2],
          lat2 = bounds[[2]][1]
        )
    } else {
      route_map_bounds(NULL)
    }
    
    # Only add legend if we have valid route data
    if (nrow(route) > 0) {
      to_matrix <- function(coords_list) {
        if (is.null(coords_list) || length(coords_list) < 2) return(NULL)
        m <- do.call(rbind, lapply(coords_list, function(coord) {
          if (is.list(coord)) c(as.numeric(coord[[1]]), as.numeric(coord[[2]])) else as.numeric(coord)
        }))
        if (!is.matrix(m) || ncol(m) != 2 || nrow(m) < 2 || !all(is.finite(m))) return(NULL)
        m
      }
      
      for (i in seq_len(nrow(route) - 1)) {
        segment_route <- result$segment_geometries[[i]]
        if (is.null(segment_route)) next
        label_text <- sprintf("%s → %s: %.0f %s, %.1fh",
                              route$name[i], route$name[i + 1],
                              route$distance_to_next_km[i] * selected_distance_multiplier(),
                              selected_distance_label(),
                              route$time_to_next_hours[i])
        
        if (route$travel_mode[i] == "Flight" &&
            !is.null(segment_route$drive_to_airport_geometry) &&
            !is.null(segment_route$flight_geometry) &&
            !is.null(segment_route$drive_from_airport_geometry)) {
          for (part in list(
            list(geometry = segment_route$drive_to_airport_geometry, color = "blue"),
            list(geometry = segment_route$flight_geometry, color = "red"),
            list(geometry = segment_route$drive_from_airport_geometry, color = "blue")
          )) {
            coords_matrix <- to_matrix(part$geometry)
            if (is.null(coords_matrix)) next
            map_proxy <- map_proxy %>%
              addPolylines(
                lng = coords_matrix[, 1],
                lat = coords_matrix[, 2],
                color = part$color,
                weight = 3,
                opacity = 0.7,
                label = label_text
              )
          }
        } else {
          coords_matrix <- to_matrix(segment_route$geometry)
          if (is.null(coords_matrix)) next
          map_proxy <- map_proxy %>%
            addPolylines(
              lng = coords_matrix[, 1],
              lat = coords_matrix[, 2],
              color = "blue",
              weight = 3,
              opacity = 0.7,
              label = label_text
            )
        }
        
      }
      
      map_proxy %>%
        addCircleMarkers(
          data = route,
          lng = ~longitude,
          lat = ~latitude,
          radius = 10,
          color = "red",
          fillOpacity = 0.9,
          label = ~paste0("Stop ", step, ": ", name),
          popup = ~paste0(
            "<h4><b>Stop ", step, ": ", name, "</b></h4>",
            ifelse(name != "START" & name != "END",
                   paste0("<b>State:</b> ", state, "<br>",
                          "<b>Date Established:</b> ", date_established, "<br>",
                          "<b>Visitors (2021):</b> ", format(visitors, big.mark = ",", scientific = FALSE), "<br><br>"),
                   ""
            ),
            ifelse(!is.na(distance_to_next_km),
                   paste0("<b>Next Stop:</b> ",
                          round(distance_to_next_km * selected_distance_multiplier(), 1), " ",
                          selected_distance_label(), " via ", travel_mode, "<br>",
                          "<b>Travel Time:</b> ", round(time_to_next_hours, 1), " hours"),
                   "<b>Final Destination</b>"
            )
          )
        ) %>%
        addLegend(
          position = "bottomright",
          colors = c("blue", "red"),
          labels = c("Driving Route", "Flight Path"),
          title = "Route Type"
        )
    }
  })
  
  # Segment mode controls
  output$segment_mode_controls <- renderUI({
    result <- computed_route()
    if (is.null(result) || nrow(result$route) < 2) {
      return(p("No route calculated yet."))
    }
    
    route_df <- result$route
    n_segments <- nrow(route_df) - 1
    
    lapply(1:n_segments, function(i) {
      segment_id <- paste0("segment_", i)
      from_name <- route_df$name[i]
      to_name <- route_df$name[i + 1]
      current_mode <- route_df$travel_mode[i]
      
      fluidRow(
        column(6, p(strong(sprintf("%s → %s", from_name, to_name)))),
        column(6, selectInput(
          segment_id,
          NULL,
          choices = c("Auto" = "auto", "Drive" = "drive", "Flight" = "flight"),
          selected = tolower(current_mode)
        ))
      )
    })
  })
  
  output$park_visit_time_controls <- renderUI({
    result <- computed_route()
    if (is.null(result) || nrow(result$route) == 0) {
      return(p("No route calculated yet."))
    }
    
    parks_only <- result$route %>% filter(!name %in% c("START", "END"))
    if (nrow(parks_only) == 0) return(p("No parks in the current route."))
    existing <- park_visit_time_overrides()
    
    lapply(seq_len(nrow(parks_only)), function(i) {
      park_name <- parks_only$name[i]
      input_id <- paste0("visit_hours_", i)
      default_value <- if (!is.na(existing[park_name])) as.numeric(existing[park_name]) else get_default_visit_hours(park_name)
      numericInput(
        input_id,
        label = park_name,
        value = default_value,
        min = 0,
        step = 0.5
      )
    })
  })
  
  # Handle segment mode input changes
  observe({
    result <- computed_route()
    if (is.null(result)) return()
    
    current_overrides <- segment_mode_overrides()
    
    for (i in 1:(nrow(result$route) - 1)) {
      segment_id <- paste0("segment_", i)
      selected_value <- input[[segment_id]]
      if (!is.null(selected_value)) {
        current_overrides[[as.character(i)]] <- selected_value
      }
    }
    segment_mode_overrides(current_overrides)
  })
  
  # Handle park visit time input changes
  observe({
    result <- computed_route()
    if (is.null(result) || nrow(result$route) == 0) return()
    
    parks_only <- result$route %>% filter(!name %in% c("START", "END"))
    if (nrow(parks_only) == 0) return()
    visit_overrides <- park_visit_time_overrides()
    
    for (i in seq_len(nrow(parks_only))) {
      park_name <- parks_only$name[i]
      input_id <- paste0("visit_hours_", i)
      visit_value <- input[[input_id]]
      if (!is.null(visit_value) && is.finite(visit_value) && visit_value >= 0) {
        visit_overrides[[park_name]] <- visit_value
      }
    }
    park_visit_time_overrides(visit_overrides)
  })
  
  # Recompute route details when segment overrides change
  observeEvent(segment_mode_overrides(), {
    result <- computed_route()
    if (is.null(result) || is.null(result$ordered_indices) || is.null(result$locations)) return()
    
    withProgress(message = "Recalculating path...", value = 0.5, {
      
      mode_matrix <- build_mode_matrix(result$locations, result$default_distance_type)
      overrides <- segment_mode_overrides()
      
      route_indices <- result$ordered_indices
      for (i in seq_len(length(route_indices) - 1)) {
        override_val <- get_named_value(overrides, as.character(i))
        if (is.null(override_val) || identical(override_val, "auto")) next
        from_idx <- route_indices[i]
        to_idx <- route_indices[i + 1]
        mode_matrix[from_idx, to_idx] <- ifelse(override_val == "drive", "Drive", "Flight")
      }
      
      refreshed <- route_with_geometry(route_indices, result$locations, mode_matrix = mode_matrix)
      refreshed$ordered_indices <- route_indices
      refreshed$locations <- result$locations
      refreshed$default_distance_type <- result$default_distance_type
      refreshed$base_mode_matrix <- result$base_mode_matrix
      refreshed$has_start <- result$has_start
      refreshed$has_end <- result$has_end
      
      computed_route(refreshed)
    })
  }, ignoreInit = TRUE)
  
  outputOptions(output, "route_map", suspendWhenHidden = FALSE)
  output$status_text <- renderText({ "" })
}

###################
### Run App
###################
shinyApp(ui = ui, server = server)