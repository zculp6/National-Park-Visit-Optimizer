library(shiny)
library(shinydashboard)
library(tidyverse)
library(leaflet)
library(sf)
library(geosphere)
library(igraph)
library(httr)
library(DT)

# Zip code data from: https://simplemaps.com/data/us-zips

###################
### Airport Data
###################

major_airports <- read.csv("us-airports.csv")
major_airports <- major_airports %>%
  filter(type == "large_airport" | type == "medium_airport") %>%
  select(ident, name, latitude_deg, longitude_deg, region_name, local_region, municipality, local_code) %>%
  rename(lat = latitude_deg) %>%
  rename(lon = longitude_deg)

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

###################
### Load Data
###################

# Data from https://www.kaggle.com/datasets/thedevastator/the-united-states-national-parks?select=df_2.csv

load_park_data <- function() {
  df <- read.csv("df_2.csv", stringsAsFactors = FALSE)
  
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
      date_established = Date.established.as.park.7..12.,
      area = Area..2021..13.,
      visitors = Recreation.visitors..2021..11.,
      description = Description
    ) %>%
    select(name = Name, states, latitude, longitude,
           date_established, area, visitors, description) %>%
    filter(!is.na(latitude) & !is.na(longitude)) %>%
    mutate(
      name = ifelse(name == "Virgin Islands", "U.S. Virgin Islands", name),
      states = lapply(states, function(x) {
        x <- ifelse(x == "Virgin Islands", "U.S. Virgin Islands", x)
        x
      })
    ) %>%
    unnest_longer(states, values_to = "state") %>%
    distinct(name, state, .keep_all = TRUE)
  
  return(park_coords)
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
    lat = as.numeric(zip_match$lat[[1]]),
    lon = as.numeric(zip_match$lon[[1]]),
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
        lat = zip_match$lat,
        lon = zip_match$lon,
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

compute_distance_matrix <- function(locations, mode_matrix = NULL, speed_drive = drive_speed_kmh, speed_flight = flight_speed_kmh) {
  n <- nrow(locations)
  dist_mat <- matrix(0, nrow = n, ncol = n)
  time_mat <- matrix(0, nrow = n, ncol = n)
  
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      if (i == j) next
      dist_km <- haversine_distance(
        locations$longitude[i], locations$latitude[i],
        locations$longitude[j], locations$latitude[j]
      )
      mode <- if (!is.null(mode_matrix)) mode_matrix[i, j] else infer_route_mode(dist_km)
      time_hours <- estimate_travel_time(dist_km, mode, speed_drive, speed_flight)
      
      dist_mat[i, j] <- dist_km
      time_mat[i, j] <- time_hours
    }
  }
  
  list(distance = dist_mat, time = time_mat)
}

tsp_solver <- function(dist_matrix, start_idx = NULL, end_idx = NULL, num_stops = NULL, time_matrix = NULL, segment_mode = "distance", max_segment_value = Inf, overall_time_limit = Inf, overall_distance_limit = Inf, mode_matrix = NULL) {
  n <- nrow(dist_matrix)
  
  if (!is.null(start_idx)) {
    stopifnot(start_idx >= 1 && start_idx <= n)
  }
  if (!is.null(end_idx)) {
    stopifnot(end_idx >= 1 && end_idx <= n)
  }
  if (!is.null(num_stops)) {
    stopifnot(num_stops >= 1 && num_stops <= n)
  }
  
  dist_adj <- dist_matrix
  time_adj <- if (!is.null(time_matrix)) time_matrix else dist_matrix
  
  # Determine constraint matrix based on segment_mode
  constraint_matrix <- if (segment_mode == "time") time_adj else dist_adj
  
  # Apply segment constraint penalty
  if (is.finite(max_segment_value) && max_segment_value > 0) {
    dist_adj <- ifelse(constraint_matrix > max_segment_value, dist_matrix + penalty_value, dist_matrix)
  }
  
  if (!is.null(start_idx) && !is.null(end_idx)) {
    for (i in seq_len(n)) {
      if (i != start_idx) dist_adj[i, start_idx] <- dist_adj[i, start_idx] + penalty_value
      if (i != end_idx) dist_adj[end_idx, i] <- dist_adj[end_idx, i] + penalty_value
    }
    for (i in seq_len(n)) {
      if (i == start_idx) next
      dist_adj[start_idx, i] <- dist_adj[start_idx, i] - penalty_value / 2
    }
    dist_adj[start_idx, end_idx] <- dist_adj[start_idx, end_idx] + penalty_value
  } else if (!is.null(start_idx)) {
    for (i in seq_len(n)) {
      if (i != start_idx) {
        dist_adj[i, start_idx] <- dist_adj[i, start_idx] + penalty_value
      }
    }
    for (i in seq_len(n)) {
      if (i == start_idx) next
      dist_adj[start_idx, i] <- dist_adj[start_idx, i] - penalty_value / 2
    }
  } else if (!is.null(end_idx)) {
    for (i in seq_len(n)) {
      if (i != end_idx) {
        dist_adj[end_idx, i] <- dist_adj[end_idx, i] + penalty_value
      }
    }
  }
  
  g <- graph_from_adjacency_matrix(dist_adj, mode = "directed", weighted = TRUE)
  
  tsp_result <- tryCatch({
    V(g)$name <- as.character(seq_len(n))
    tour <- sapply(1:10, function(seed) {
      set.seed(seed)
      initial <- sample(n)
      path <- initial
      improved <- TRUE
      iterations <- 0
      while (improved && iterations < 100) {
        improved <- FALSE
        iterations <- iterations + 1
        for (i in 1:(length(path) - 1)) {
          for (j in (i + 1):length(path)) {
            new_path <- path
            new_path[i:j] <- rev(path[i:j])
            if (sum(dist_adj[cbind(new_path[-length(new_path)], new_path[-1])]) <
                sum(dist_adj[cbind(path[-length(path)], path[-1])])) {
              path <- new_path
              improved <- TRUE
            }
          }
        }
      }
      list(path = path, distance = sum(dist_adj[cbind(path[-length(path)], path[-1])]))
    })
    
    best_tour <- tour[[which.min(sapply(tour, function(x) x$distance))]]
    best_tour$path
  }, error = function(e) {
    seq_len(n)
  })
  
  # If overall limits are specified and would be exceeded, try to fit max parks within limits
  if (is.finite(overall_time_limit) || is.finite(overall_distance_limit)) {
    # Calculate current route metrics
    current_dist <- 0
    current_time <- 0
    for (i in 1:(length(tsp_result) - 1)) {
      current_dist <- current_dist + dist_matrix[tsp_result[i], tsp_result[i + 1]]
      current_time <- current_time + time_adj[tsp_result[i], tsp_result[i + 1]]
    }
    
    # If we exceed limits, reduce the number of parks
    if ((is.finite(overall_distance_limit) && current_dist > overall_distance_limit) ||
        (is.finite(overall_time_limit) && current_time > overall_time_limit)) {
      
      # Start with must-include locations
      must_include <- unique(c(start_idx, end_idx))
      must_include <- must_include[!is.na(must_include)]
      
      # Try progressively smaller subsets
      all_indices <- seq_len(n)
      candidates <- setdiff(all_indices, must_include)
      
      # Binary search for max parks that fit
      min_parks <- length(must_include)
      max_parks <- n
      best_valid_route <- NULL
      
      while (min_parks <= max_parks) {
        mid_parks <- floor((min_parks + max_parks) / 2)
        test_num <- mid_parks
        
        if (test_num <= length(must_include)) {
          indices_to_test <- must_include
        } else {
          num_candidates <- test_num - length(must_include)
          selected_candidates <- candidates[1:min(num_candidates, length(candidates))]
          indices_to_test <- c(must_include, selected_candidates)
        }
        
        # Build sub-matrix
        sub_dist <- dist_matrix[indices_to_test, indices_to_test, drop = FALSE]
        sub_time <- time_adj[indices_to_test, indices_to_test, drop = FALSE]
        
        # Map indices
        new_start_idx <- if (!is.null(start_idx) && start_idx %in% indices_to_test) which(indices_to_test == start_idx) else NULL
        new_end_idx <- if (!is.null(end_idx) && end_idx %in% indices_to_test) which(indices_to_test == end_idx) else NULL
        
        # Solve for this subset
        test_route <- tsp_solver(sub_dist, start_idx = new_start_idx, end_idx = new_end_idx, 
                                 num_stops = NULL, time_matrix = sub_time, 
                                 segment_mode = segment_mode, max_segment_value = max_segment_value,
                                 overall_time_limit = Inf, overall_distance_limit = Inf)
        
        # Calculate metrics
        test_dist <- 0
        test_time <- 0
        for (i in 1:(length(test_route) - 1)) {
          test_dist <- test_dist + sub_dist[test_route[i], test_route[i + 1]]
          test_time <- test_time + sub_time[test_route[i], test_route[i + 1]]
        }
        
        # Check if within limits
        within_limits <- TRUE
        if (is.finite(overall_distance_limit) && test_dist > overall_distance_limit) within_limits <- FALSE
        if (is.finite(overall_time_limit) && test_time > overall_time_limit) within_limits <- FALSE
        
        if (within_limits) {
          best_valid_route <- indices_to_test[test_route]
          min_parks <- mid_parks + 1
        } else {
          max_parks <- mid_parks - 1
        }
      }
      
      if (!is.null(best_valid_route)) {
        tsp_result <- best_valid_route
      }
    }
  }
  
  if (!is.null(num_stops) && num_stops < n && (is.infinite(overall_time_limit) && is.infinite(overall_distance_limit))) {
    distances_from_path <- numeric(n)
    for (i in seq_len(n)) {
      idx <- tsp_result[i]
      prev_idx <- if (i == 1) tsp_result[n] else tsp_result[i - 1]
      next_idx <- if (i == n) tsp_result[1] else tsp_result[i + 1]
      distances_from_path[i] <- dist_matrix[prev_idx, idx] + dist_matrix[idx, next_idx]
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
    
    if (!is.null(start_idx)) {
      start_pos <- which(tsp_result == start_idx)
      if (length(start_pos) > 0) {
        tsp_result <- c(tsp_result[start_pos:length(tsp_result)], tsp_result[seq_len(start_pos - 1)])
      }
    }
  } else {
    if (!is.null(start_idx)) {
      start_pos <- which(tsp_result == start_idx)
      if (length(start_pos) > 0) {
        tsp_result <- c(tsp_result[start_pos:length(tsp_result)], tsp_result[seq_len(start_pos - 1)])
      }
    }
  }
  
  tsp_result
}

get_osrm_route <- function(lon1, lat1, lon2, lat2) {
  base_url <- "http://router.project-osrm.org/route/v1/driving/"
  url <- sprintf("%s%.6f,%.6f;%.6f,%.6f?overview=full&geometries=geojson",
                 base_url, lon1, lat1, lon2, lat2)
  
  tryCatch({
    response <- GET(url)
    if (status_code(response) == 200) {
      result <- content(response, as = "parsed")
      if (!is.null(result$routes) && length(result$routes) > 0) {
        route <- result$routes[[1]]
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
    return(list(success = FALSE))
  }, error = function(e) {
    return(list(success = FALSE))
  })
}

route_with_geometry <- function(ordered_indices, locations, mode_matrix = NULL) {
  n <- length(ordered_indices)
  route_df <- locations[ordered_indices, ]
  route_df$step <- seq_len(n)
  
  distance_to_next <- numeric(n)
  time_to_next <- numeric(n)
  travel_mode <- character(n)
  segment_geometries <- vector("list", n)
  
  for (i in seq_len(n - 1)) {
    idx_from <- ordered_indices[i]
    idx_to <- ordered_indices[i + 1]
    
    lon1 <- locations$longitude[idx_from]
    lat1 <- locations$latitude[idx_from]
    lon2 <- locations$longitude[idx_to]
    lat2 <- locations$latitude[idx_to]
    
    dist_km <- haversine_distance(lon1, lat1, lon2, lat2)
    mode <- if (!is.null(mode_matrix)) mode_matrix[idx_from, idx_to] else infer_route_mode(dist_km)
    
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
    } else {
      airport_from <- find_nearest_airport(lat1, lon1)
      airport_to <- find_nearest_airport(lat2, lon2)
      
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
      
      segment_geometries[[i]] <- list(
        drive_to_airport_geometry = if (drive_to_airport$success) drive_to_airport$geometry else list(c(lon1, lat1), c(airport_from$airport$lon, airport_from$airport$lat)),
        flight_geometry = list(c(airport_from$airport$lon, airport_from$airport$lat), c(airport_to$airport$lon, airport_to$airport$lat)),
        drive_from_airport_geometry = if (drive_from_airport$success) drive_from_airport$geometry else list(c(airport_to$airport$lon, airport_to$airport$lat), c(lon2, lat2))
      )
    }
    
    travel_mode[i] <- mode
  }
  
  distance_to_next[n] <- NA
  time_to_next[n] <- NA
  travel_mode[n] <- NA
  
  route_df$distance_to_next_km <- distance_to_next
  route_df$time_to_next_hours <- time_to_next
  route_df$travel_mode <- travel_mode
  
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
                menuItem("National Parks Map", tabName = "allparks", icon = icon("globe-americas")),
                menuItem("Route Planner", tabName = "planner", icon = icon("route")),
                menuItem("Optimal Route", tabName = "optimalroute", icon = icon("map-marked-alt")),
                menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .info-box { min-height: 90px; }
        .info-box-icon { height: 90px; line-height: 90px; }
        .info-box-content { padding-top: 0px; padding-bottom: 0px; }
      "))
    ),
    
    tabItems(
      # Home Tab
      tabItem(tabName = "home",
              fluidRow(
                box(
                  title = "Welcome",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  p("Use the sidebar or buttons below to move through the app quickly."),
                  tags$ul(
                    tags$li(strong("National Parks Map:"), "Browse every U.S. national park with area and description details."),
                    tags$li(strong("Route Planner:"), "Select parks and configure your trip."),
                    tags$li(strong("Optimal Route:"), "See the calculated route drawn on the map with segment details."),
                    tags$li(strong("About:"), "Review features and route logic details.")
                  ),
                  br(),
                  actionButton("go_allparks", "See Map", class = "btn-primary"),
                  tags$span("  "),
                  actionButton("go_planner", "Plan Route", class = "btn-success")
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
                  p("All national parks from df_2.csv. Click a marker to see area, visitors, and description."),
                  leafletOutput("all_parks_map", height = 700)
                )
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
                  p(em("Leave blank to start at the first selected park and/or end at the last selected park."))
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
                           actionButton("calculate", "Calculate Optimal Route",
                                        icon = icon("calculator"),
                                        class = "btn-success btn-lg"),
                           br(), br(),
                           textOutput("status_text")
                    )
                  )
                )
              ),
              
              fluidRow(
                infoBoxOutput("total_distance_box", width = 3),
                infoBoxOutput("total_time_box", width = 3),
                infoBoxOutput("num_stops_box", width = 3),
                infoBoxOutput("avg_distance_box", width = 3)
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
              fluidRow(
                box(
                  title = "Interactive Route Map with GPS-Style Paths",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  actionButton("back_to_planner", "← Back to Route Planner", class = "btn-default"),
                  br(), br(),
                  p("Blue lines = driving routes (actual roads). Red lines = flight paths (includes airport transfers).
               Click markers to see park details."),
                  p(em("Map initializes on app startup. If no route has been calculated yet, a starter map is shown.")),
                  leafletOutput("route_map", height = 700)
                )
              ),
              fluidRow(
                box(
                  title = "Per-Segment Travel Mode Overrides",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 12,
                  p("Override each segment to Auto/Drive/Flight and recalculate route details instantly."),
                  uiOutput("segment_mode_controls")
                )
              ),
              fluidRow(
                box(
                  title = "Optimal Route Details",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  DTOutput("route_table")
                )
              )
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
                    tags$li(strong("GPS-Accurate Routes:"), "Shows actual roads you would drive on"),
                    tags$li(strong("Complete Park Info:"), "Dates, areas, visitor counts, and descriptions")
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
  
  # Load park data
  park_data <- load_park_data()
  park_catalog <- park_data %>% distinct(name, .keep_all = TRUE)
  park_data$region <- sapply(park_data$state, classify_park_region)
  
  selected_park_names <- reactiveVal(character(0))
  segment_mode_overrides <- reactiveVal(character(0))
  computed_route <- reactiveVal(NULL)
  
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
    leaflet(park_catalog) %>%
      addTiles() %>%
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
        popup = ~paste0(
          "<h4><b>", name, "</b></h4>",
          "<b>State/Territory:</b> ", state, "<br>",
          "<b>Area:</b> ", area, "<br>",
          "<b>Visitors (2021):</b> ", format(visitors, big.mark = ",", scientific = FALSE), "<br><br>",
          "<b>Description:</b><br>", description
        )
      )
  })
  
  # Park selector map - show all parks initially
  output$park_selector_map <- renderLeaflet({
    selected <- selected_park_names()
    map_data <- park_catalog %>%
      mutate(marker_color = if_else(name %in% selected, "green", "red"))
    
    leaflet() %>%
      addTiles() %>%
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
        popup = ~paste0(
          "<h4><b>", name, "</b></h4>",
          "<b>State/Territory:</b> ", state, "<br>",
          "<b>Date Established:</b> ", date_established, "<br>",
          "<b>Area:</b> ", area, "<br>",
          "<b>Visitors (2021):</b> ", format(visitors, big.mark = ",", scientific = FALSE), "<br><br>",
          "<b>Description:</b><br>", description
        ),
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
      mutate(marker_color = if_else(name %in% selected, "green", "red"))
    
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
        popup = ~paste0(
          "<h4><b>", name, "</b></h4>",
          "<b>State/Territory:</b> ", state, "<br>",
          "<b>Date Established:</b> ", date_established, "<br>",
          "<b>Area:</b> ", area, "<br>",
          "<b>Visitors (2021):</b> ", format(visitors, big.mark = ",", scientific = FALSE), "<br><br>",
          "<b>Description:</b><br>", description
        ),
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
    
    end_text <- if (!is.null(input$end_zip_choice) && nzchar(input$end_zip_choice)) {
      zip_info <- get_zip_coordinates(input$end_zip_choice)
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
  observeEvent(input$go_allparks, {
    updateTabItems(session, "main_tabs", "allparks")
  })
  
  observeEvent(input$go_planner, {
    updateTabItems(session, "main_tabs", "planner")
  })
  
  observeEvent(input$back_to_planner, {
    updateTabItems(session, "main_tabs", "planner")
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
    
    if (length(selected) < 2) {
      showNotification("Please select at least 2 parks.", type = "error", duration = 5)
      return()
    }
    
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
          latitude = geo_result$lat,
          longitude = geo_result$lon,
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
    if (!is.null(input$end_zip_choice) && nzchar(input$end_zip_choice)) {
      geo_result <- geocode_location(input$end_zip_choice)
      if (isTRUE(geo_result$success)) {
        end_loc <- list(
          name = "END",
          state = geo_result$source_state,
          latitude = geo_result$lat,
          longitude = geo_result$lon,
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
    
    # Compute distance and time matrices
    matrices <- compute_distance_matrix(locations)
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
      overall_distance_limit = overall_distance_limit
    )
    
    # Generate route with geometry
    result <- route_with_geometry(ordered_indices, locations)
    result$has_start <- !is.null(start_loc)
    result$has_end <- !is.null(end_loc)
    
    computed_route(result)
    
    showNotification("Route calculated successfully!", type = "message", duration = 3)
    updateTabItems(session, "main_tabs", "optimalroute")
  })
  
  # Info boxes
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
  
  output$total_time_box <- renderInfoBox({
    result <- computed_route()
    if (is.null(result)) {
      infoBox("Total Time", "--", "Click Calculate",
              icon = icon("clock"), color = "green")
    } else {
      days <- floor(result$total_time / 24)
      hours <- round(result$total_time %% 24, 1)
      time_str <- if (days > 0) {
        sprintf("%d day%s, %.1fh", days, if(days > 1) "s" else "", hours)
      } else {
        sprintf("%.1f hours", hours)
      }
      infoBox("Total Time", time_str, "Travel time",
              icon = icon("clock"), color = "green")
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
  
  # Route table
  output$route_table <- renderDT({
    result <- computed_route()
    if (is.null(result)) {
      return(data.frame(Message = "Click 'Calculate Optimal Route' to begin"))
    }
    
    display_df <- result$route %>%
      mutate(
        Location = ifelse(name == "START",
                          paste0(
                            ifelse(!is.na(description) & nzchar(description), description, "Starting Location"),
                            ifelse(!is.na(state) & nzchar(state), paste0(" (", state, ")"), "")
                          ),
                          ifelse(name == "END",
                                 paste0(
                                   ifelse(!is.na(description) & nzchar(description), description, "Ending Location"),
                                   ifelse(!is.na(state) & nzchar(state), paste0(" (", state, ")"), "")
                                 ),
                                 paste0(name, " (", state, ")"))),
        `Travel Mode` = travel_mode,
        Distance = round(distance_to_next_km * selected_distance_multiplier(), 1),
        `Time (hours)` = round(time_to_next_hours, 1)
      ) %>%
      select(Step = step, Location, `Travel Mode`,
             Distance, `Time (hours)`)
    
    names(display_df)[names(display_df) == "Distance"] <- paste0("Distance (", selected_distance_label(), ")")
    
    datatable(display_df,
              options = list(pageLength = 25, dom = 'tip'),
              rownames = FALSE)
  })
  
  # Route map
  output$route_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
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
            ifelse(name != "START" && name != "END",
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
  
  # Handle segment mode changes
  observe({
    result <- computed_route()
    if (is.null(result)) return()
    
    current_overrides <- segment_mode_overrides()
    for (i in 1:(nrow(result$route) - 1)) {
      segment_id <- paste0("segment_", i)
      selected_value <- input[[segment_id]]
      if (!is.null(selected_value)) {
        current_overrides[as.character(i)] <- selected_value
      }
    }
    segment_mode_overrides(current_overrides)
  })
  
  outputOptions(output, "route_map", suspendWhenHidden = FALSE)
  output$status_text <- renderText({ "" })
}

###################
### Run App
###################
shinyApp(ui = ui, server = server)