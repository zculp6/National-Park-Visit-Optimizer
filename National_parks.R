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
geocode_location <- function(city, state, zipcode) {
  tryCatch({
    query_parts <- c(city, state, zipcode, "USA")
    query_parts <- query_parts[nzchar(query_parts)]
    query <- paste(query_parts, collapse = ", ")
    url <- sprintf("https://nominatim.openstreetmap.org/search?q=%s&format=json&limit=1",
                   URLencode(query))
    
    response <- GET(url, user_agent("NationalParksApp/1.0"))
    
    if (status_code(response) == 200) {
      result <- content(response, as = "parsed")
      if (length(result) > 0) {
        return(list(
          lat = as.numeric(result[[1]]$lat),
          lon = as.numeric(result[[1]]$lon),
          success = TRUE
        ))
      }
    }
    return(list(success = FALSE, message = "Location not found. Try city + state and/or ZIP."))
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

build_state_polygons <- function() {
  normalize_geometry_column <- function(sf_object, target_name = "geometry") {
    current_geometry_name <- attr(sf_object, "sf_column")
    if (!identical(current_geometry_name, target_name)) {
      names(sf_object)[names(sf_object) == current_geometry_name] <- target_name
      sf::st_geometry(sf_object) <- target_name
    }
    sf_object
  }
  previous_s2 <- sf_use_s2()
  on.exit(sf_use_s2(previous_s2), add = TRUE)
  sf_use_s2(FALSE)
  us_state_map <- maps::map("state", fill = TRUE, plot = FALSE)
  state_polygons <- st_as_sf(us_state_map) %>%
    st_transform(4326) %>%
    st_make_valid() %>%
    mutate(
      state = str_to_title(str_replace(ID, ":.*$", ""))
    ) %>%
    group_by(state) %>%
    summarize(.groups = "drop", do_union = TRUE) %>%
    normalize_geometry_column()
  
  territory_centers <- tibble(
    state = c("U.S. Virgin Islands", "American Samoa"),
    lat = c(18.3358, -14.2710),
    lon = c(-64.8963, -170.1322)
  )
  
  territory_polygons <- st_sf(
    state = territory_centers$state,
    geometry = st_sfc(
      lapply(seq_len(nrow(territory_centers)), function(i) {
        lon <- territory_centers$lon[i]
        lat <- territory_centers$lat[i]
        st_polygon(list(matrix(
          c(
            lon - 1.25, lat - 1.25,
            lon + 1.25, lat - 1.25,
            lon + 1.25, lat + 1.25,
            lon - 1.25, lat + 1.25,
            lon - 1.25, lat - 1.25
          ),
          ncol = 2,
          byrow = TRUE
        )))
      }),
      crs = 4326
    )) %>%
    normalize_geometry_column()
  
  polygon_data <- bind_rows(
    state_polygons %>% select(state, geometry),
    territory_polygons %>% select(state, geometry)
  )
  
  centers <- st_centroid(polygon_data) %>%
    mutate(
      lon = st_coordinates(geometry)[, 1],
      lat = st_coordinates(geometry)[, 2]
    ) %>%
    st_drop_geometry() %>%
    select(state, lat, lon)
  
  list(polygons = polygon_data, centers = centers)
}

select_best_park_subset <- function(parks, n) {
  if (n >= nrow(parks)) return(parks)
  
  if (n <= 6) {
    combo_idx <- combn(nrow(parks), n)
    combo_idx <- combn(nrow(parks), 3)
    best_total <- Inf
    best_combo <- combo_idx[, 1]
    
    for (k in seq_len(ncol(combo_idx))) {
      idx <- combo_idx[, k]
      combo <- parks[idx, ]
      dmat <- distm(cbind(combo$longitude, combo$latitude), fun = distHaversine) / 1000
      diag(dmat) <- 0
      tsp <- solve_tsp(dmat)
      if (tsp$distance < best_total) {
        best_total <- tsp$distance
        best_combo <- idx
      }
    }
    
    return(parks[best_combo, ])
  }
  
  # Greedy expansion for larger n to avoid combinatorial explosion.
  chosen <- 1
  while (length(chosen) < n) {
    remaining <- setdiff(seq_len(nrow(parks)), chosen)
    candidate_scores <- sapply(remaining, function(idx) {
      combo <- parks[c(chosen, idx), ]
      dmat <- distm(cbind(combo$longitude, combo$latitude), fun = distHaversine) / 1000
      diag(dmat) <- 0
      solve_tsp(dmat)$distance
    })
    chosen <- c(chosen, remaining[which.min(candidate_scores)])
  }
  
  parks[chosen, ]
}

###################
### Getting Routes
###################
get_route_info <- function(lon1, lat1, lon2, lat2) {
  tryCatch({
    url <- sprintf("http://router.project-osrm.org/route/v1/driving/%f,%f;%f,%f?overview=full&geometries=geojson",
                   lon1, lat1, lon2, lat2)
    
    response <- GET(url, timeout(10))
    
    if (status_code(response) == 200) {
      result <- content(response, as = "parsed")
      route <- result$routes[[1]]
      
      # Extract route geometry
      geometry <- route$geometry$coordinates
      
      return(list(
        distance_km = route$distance / 1000,
        duration_hours = route$duration / 3600,
        geometry = geometry,
        success = TRUE
      ))
    } else {
      dist <- distHaversine(c(lon1, lat1), c(lon2, lat2)) / 1000
      return(list(
        distance_km = dist,
        duration_hours = dist / 80,
        geometry = list(list(lon1, lat1), list(lon2, lat2)),
        success = FALSE
      ))
    }
  }, error = function(e) {
    dist <- distHaversine(c(lon1, lat1), c(lon2, lat2)) / 1000
    return(list(
      distance_km = dist,
      duration_hours = dist / 80,
      geometry = list(list(lon1, lat1), list(lon2, lat2)),
      success = FALSE
    ))
  })
}

# Calculate flight with airport transfers
calculate_flight_route <- function(lon1, lat1, lon2, lat2) {
  # Find nearest airports
  airport1 <- find_nearest_airport(lat1, lon1)
  airport2 <- find_nearest_airport(lat2, lon2)
  
  # Drive to origin airport
  drive_to_airport <- get_route_info(lon1, lat1, airport1$airport$lon, airport1$airport$lat)
  
  # Flight distance
  flight_dist <- distHaversine(
    c(airport1$airport$lon, airport1$airport$lat),
    c(airport2$airport$lon, airport2$airport$lat)
  ) / 1000
  
  # Flight time: distance/600 + 2.5 hours (boarding, taxi, etc)
  flight_time <- (flight_dist / 600) + 2.5
  
  # Drive from destination airport
  drive_from_airport <- get_route_info(airport2$airport$lon, airport2$airport$lat, lon2, lat2)
  
  total_distance <- drive_to_airport$distance_km + flight_dist + drive_from_airport$distance_km
  total_time <- drive_to_airport$duration_hours + flight_time + drive_from_airport$duration_hours
  
  # Build complete geometry
  geometry <- c(
    drive_to_airport$geometry,
    list(list(airport1$airport$lon, airport1$airport$lat)),
    list(list(airport2$airport$lon, airport2$airport$lat)),
    drive_from_airport$geometry
  )
  
  return(list(
    distance_km = total_distance,
    duration_hours = total_time,
    geometry = geometry,
    drive_to_airport_geometry = drive_to_airport$geometry,
    flight_geometry = list(
      list(airport1$airport$lon, airport1$airport$lat),
      list(airport2$airport$lon, airport2$airport$lat)
    ),
    drive_from_airport_geometry = drive_from_airport$geometry,
    airport_origin = airport1$airport$ident,
    airport_dest = airport2$airport$ident,
    success = TRUE
  ))
}

###################
### Distance Calculation
###################
calculate_distance_matrix <- function(parks_df, transport_mode = "mixed", start_location = NULL, progress = NULL,
                                      limit_metric = "none", limit_value = NULL) {
  parks_df$region <- sapply(parks_df$state, classify_park_region)
  
  # Add starting location if provided
  if (!is.null(start_location)) {
    start_row <- data.frame(
      name = "START",
      state = start_location$state,
      latitude = start_location$lat,
      longitude = start_location$lon,
      date_established = NA,
      area = NA,
      visitors = NA,
      description = "Starting Location",
      region = "mainland",
      stringsAsFactors = FALSE
    )
    parks_df <- rbind(start_row, parks_df)
  }
  
  n <- nrow(parks_df)
  dist_matrix <- matrix(0, n, n)
  time_matrix <- matrix(0, n, n)
  feasible_matrix <- matrix(TRUE, n, n)
  
  total_pairs <- n * (n - 1) / 2
  pair_count <- 0
  
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      pair_count <- pair_count + 1
      
      if (!is.null(progress)) {
        progress$inc(1/total_pairs,
                     detail = sprintf("Calculating %d of %d", pair_count, total_pairs))
      }
      
      region1 <- parks_df$region[i]
      region2 <- parks_df$region[j]
      
      drive_info <- get_route_info(
        parks_df$longitude[i], parks_df$latitude[i],
        parks_df$longitude[j], parks_df$latitude[j]
      )
      flight_info <- calculate_flight_route(
        parks_df$longitude[i], parks_df$latitude[i],
        parks_df$longitude[j], parks_df$latitude[j]
      )
      
      mode_choice <- dplyr::case_when(
        transport_mode == "driving" ~ "drive",
        transport_mode == "flying" ~ "flight",
        TRUE ~ "auto"
      )
      constrained <- route_with_constraints(
        drive_info, flight_info, mode_choice = mode_choice,
        limit_metric = limit_metric, limit_value = limit_value
      )
      
      if (!isTRUE(constrained$valid)) {
        dist <- penalty_value
        time <- penalty_value
        feasible_matrix[i, j] <- FALSE
        feasible_matrix[j, i] <- FALSE
      } else {
        dist <- constrained$route$distance_km
        time <- constrained$route$duration_hours
      }
      
      dist_matrix[i, j] <- dist
      dist_matrix[j, i] <- dist
      time_matrix[i, j] <- time
      time_matrix[j, i] <- time
    }
  }
  
  rownames(dist_matrix) <- parks_df$name
  colnames(dist_matrix) <- parks_df$name
  rownames(time_matrix) <- parks_df$name
  colnames(time_matrix) <- parks_df$name
  
  return(list(
    distance = dist_matrix,
    time = time_matrix,
    parks = parks_df,
    feasible = feasible_matrix
  ))
}

get_segment_route <- function(from_lon, from_lat, to_lon, to_lat, transport_mode = "mixed") {
  drive_info <- get_route_info(from_lon, from_lat, to_lon, to_lat)
  flight_info <- calculate_flight_route(from_lon, from_lat, to_lon, to_lat)
  
  if (transport_mode == "driving") {
    return(list(mode = "Drive", route = drive_info))
  }
  
  if (transport_mode == "flying") {
    return(list(mode = "Flight", route = flight_info))
  }
  
  if (drive_info$duration_hours <= flight_info$duration_hours) {
    return(list(mode = "Drive", route = drive_info))
  } else {
    return(list(mode = "Flight", route = flight_info))
  }
}

get_segment_route_by_choice <- function(from_lon, from_lat, to_lon, to_lat, mode_choice = "auto") {
  drive_info <- get_route_info(from_lon, from_lat, to_lon, to_lat)
  flight_info <- calculate_flight_route(from_lon, from_lat, to_lon, to_lat)
  
  if (mode_choice == "drive") {
    return(list(mode = "Drive", route = drive_info))
  }
  if (mode_choice == "flight") {
    return(list(mode = "Flight", route = flight_info))
  }
  
  if (drive_info$duration_hours <= flight_info$duration_hours) {
    list(mode = "Drive", route = drive_info)
  } else {
    list(mode = "Flight", route = flight_info)
  }
}

route_with_constraints <- function(drive_info, flight_info, mode_choice = "auto",
                                   limit_metric = "none", limit_value = NULL) {
  is_valid <- function(route_info) {
    if (limit_metric == "none" || is.null(limit_value) || is.na(limit_value)) return(TRUE)
    if (limit_metric == "time") return(route_info$duration_hours <= limit_value)
    if (limit_metric == "distance") return(route_info$distance_km <= limit_value)
    TRUE
  }
  
  drive_valid <- is_valid(drive_info)
  flight_valid <- is_valid(flight_info)
  
  if (mode_choice == "drive") {
    if (!drive_valid) return(list(valid = FALSE, mode = "Drive", route = NULL))
    return(list(valid = TRUE, mode = "Drive", route = drive_info))
  }
  
  if (mode_choice == "flight") {
    if (!flight_valid) return(list(valid = FALSE, mode = "Flight", route = NULL))
    return(list(valid = TRUE, mode = "Flight", route = flight_info))
  }
  
  candidates <- list()
  if (drive_valid) candidates <- append(candidates, list(list(mode = "Drive", route = drive_info)))
  if (flight_valid) candidates <- append(candidates, list(list(mode = "Flight", route = flight_info)))
  
  if (length(candidates) == 0) return(list(valid = FALSE, mode = "Auto", route = NULL))
  
  best_idx <- which.min(sapply(candidates, function(x) x$route$duration_hours))
  list(valid = TRUE, mode = candidates[[best_idx]]$mode, route = candidates[[best_idx]]$route)
}


###################
### Traveling Salesman Problem (Finding Optimal Path)
###################
solve_tsp <- function(dist_matrix, start_index = NULL) {
  n <- nrow(dist_matrix)
  
  if (n == 2) {
    return(list(tour = c(1, 2), distance = dist_matrix[1, 2]))
  }
  
  # Start from specified location or first park
  if (is.null(start_index)) {
    current <- 1
  } else {
    current <- start_index
  }
  
  tour <- current
  unvisited <- setdiff(1:n, current)
  
  # Nearest neighbor construction
  while (length(unvisited) > 0) {
    distances_to_unvisited <- dist_matrix[current, unvisited]
    nearest_idx <- which.min(distances_to_unvisited)
    nearest <- unvisited[nearest_idx]
    
    tour <- c(tour, nearest)
    current <- nearest
    unvisited <- setdiff(unvisited, nearest)
  }
  
  # Calculate initial distance
  total_dist <- 0
  for (i in 1:(length(tour)-1)) {
    total_dist <- total_dist + dist_matrix[tour[i], tour[i+1]]
  }
  
  # 2-opt improvement (more iterations for better results)
  improved <- TRUE
  max_iterations <- 2000
  iteration <- 0
  
  while (improved && iteration < max_iterations) {
    improved <- FALSE
    iteration <- iteration + 1
    
    for (i in 1:(n-2)) {
      for (j in (i+2):n) {
        if (j == n) next
        
        # Don't move the start position
        if (i == 1 && !is.null(start_index)) next
        
        current_dist <- dist_matrix[tour[i], tour[i+1]] +
          dist_matrix[tour[j], tour[j+1]]
        new_dist <- dist_matrix[tour[i], tour[j]] +
          dist_matrix[tour[i+1], tour[j+1]]
        
        if (new_dist < current_dist) {
          tour[(i+1):j] <- rev(tour[(i+1):j])
          total_dist <- total_dist - current_dist + new_dist
          improved <- TRUE
        }
      }
    }
  }
  
  return(list(tour = tour, distance = total_dist))
}


###################
### UI
###################

ui <- dashboardPage(
  dashboardHeader(title = "National Parks Route Optimizer"),
  
  dashboardSidebar(
    sidebarMenu(id = "main_tabs",
                menuItem("Home", tabName = "home", icon = icon("home")),
                menuItem("All Parks Map", tabName = "allparks", icon = icon("map")),
                menuItem("Route Planner", tabName = "planner", icon = icon("route")),
                menuItem("Route Map", tabName = "routemap", icon = icon("map-marked-alt")),
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
                    tags$li(strong("Route Planner:"), "Select states/territories and configure your trip."),
                    tags$li(strong("Route Map:"), "See the calculated route drawn on the map."),
                    tags$li(strong("About:"), "Review features and route logic details.")
                  ),
                  br(),
                  tags$span("  "),
                  actionButton("go_planner", "Go to Route Planner", class = "btn-success"),
                  tags$span("  "),
                  actionButton("go_routemap", "Go to Route Map", class = "btn-info")
                )
              )
      ),
      
      
      # Route Planner Tab
      tabItem(tabName = "planner",
              fluidRow(
                box(
                  title = "Starting Location (Optional)",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  collapsible = TRUE,
                  collapsed = TRUE,
                  
                  fluidRow(
                    column(4,
                           textInput("start_city", "City:", placeholder = "Columbus")
                    ),
                    column(4,
                           textInput("start_state", "State/Territory:", placeholder = "Ohio")
                    ),
                    column(4,
                           textInput("start_zip", "Zip Code:", placeholder = "43215")
                    )
                  ),
                  p(em("Leave blank to start from the first park in the route."))
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
                           selectInput("states",
                                       "Select States/Territories:",
                                       choices = NULL,
                                       multiple = TRUE,
                                       selected = character(0))
                    ),
                    column(4,
                           selectInput("limit_metric",
                                       "Maximum Segment Constraint:",
                                       choices = c("No limit" = "none",
                                                   "Maximum Segment Travel Time (hours)" = "time",
                                                   "Maximum Segment Distance (km)" = "distance"),
                                       selected = "none")
                    ),
                    column(4,
                           radioButtons("distance_type",
                                        "Default Segment:",
                                        choices = c("Driving Only" = "driving",
                                                    "Auto (Quickest per segment)" = "mixed",
                                                    "Flying Only" = "flying"),
                                        selected = "mixed")
                    )
                  ),
                  fluidRow(
                    column(
                      4,
                      numericInput("limit_value",
                                   "Limit Value:",
                                   min = 1,
                                   value = 8,
                                   step = 1)
                    )
                  ),
                  fluidRow(
                    column(
                      12,
                      actionButton("reset_states", "Reset State Selection", icon = icon("undo"))
                    )
                  ),
                  br(),
                  fluidRow(
                    column(12,
                           ("Select states/territories and then click park markers to include them in your route. Red = not selected, Green = selected."),
                           leafletOutput("park_selector_map", height = 450)
                    )
                  ),
                  
                  fluidRow(
                    column(12,
                           p(strong("Note:"), "The route optimizer can include all parks (including Alaska, Hawaii, and territories)
                   and supports multi-state parks in each matching state."),
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
                  title = "Per-Segment Travel Mode Overrides",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 12,
                  p("After calculating a route, you can override each segment to Auto/Drive/Flight and compare time."),
                  uiOutput("segment_mode_controls")
                )
              ),
              
              fluidRow(
                box(
                  title = "Route Details",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  actionButton("go_route_map_from_details", "Go to Route Map", class = "btn-info"),
                  br(), br(),
                  DTOutput("route_table")
                )
              )
      ),
      
      # Route Map Tab
      tabItem(tabName = "routemap",
              fluidRow(
                box(
                  title = "Interactive Route Map with GPS-Style Paths",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  p("Blue lines = driving routes (actual roads). Red lines = flight paths (includes airport transfers).
               Click markers to see park details."),
                  p(em("Map initializes on app startup. If no route has been calculated yet, a starter map is shown.")),
                  leafletOutput("route_map", height = 700)
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
  available_states <- sort(unique(park_data$state))
  
  selected_states <- reactiveVal(character(0))
  selected_park_names <- reactiveVal(character(0))
  segment_mode_overrides <- reactiveVal(character(0))
  
  # Update state choices
  observe({
    updateSelectInput(session, "states",
                      choices = c("None" = "NONE", "Select All States/Territories" = "ALL", available_states),
                      selected = character(0))
  })
  
  observeEvent(input$states, {
    if (is.null(input$states) || length(input$states) == 0 || "NONE" %in% input$states) {
      selected_states(character(0))
      updateSelectInput(session, "states", selected = character(0))
    } else if ("ALL" %in% input$states) {
      selected_states(available_states)
      updateSelectInput(session, "states", selected = available_states)
    } else {
      selected_states(input$states)
    }
  }, ignoreNULL = FALSE)
  
  observeEvent(input$reset_states, {
    selected_states(character(0))
    updateSelectInput(session, "states", selected = character(0))
  })
  
  # All Parks Map
  state_filtered_parks <- reactive({
    if (length(selected_states()) == 0) {
      park_catalog
    } else {
      park_catalog %>% filter(state %in% selected_states())
    }
  })
  
  output$park_selector_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -98.5, lat = 39.8, zoom = 4)
  })
  
  observe({
    parks <- state_filtered_parks()
    selected <- selected_park_names()
    selected <- intersect(selected, parks$name)
    selected_park_names(selected)
    
    map_data <- parks %>%
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
  
  observeEvent(input$park_selector_map_marker_click, {
    clicked <- input$park_selector_map_marker_click$id
    if (is.null(clicked) || clicked == "") return()
    
    selected <- selected_park_names()
    if (clicked %in% selected) {
      selected <- setdiff(selected, clicked)
    } else {
      selected <- c(selected, clicked)
    }
    
    selected_park_names(sort(unique(selected)))
  })
  
  # Selected parks for route
  selected_parks <- reactive({
    names_selected <- selected_park_names()
    parks <- park_catalog %>% filter(name %in% names_selected)
    parks
  })
  
  # Collect starting location inputs without geocoding on every re-render
  start_location_input <- reactive({
    city <- trimws(input$start_city)
    state <- trimws(input$start_state)
    zip <- trimws(input$start_zip)
    
    if (!nzchar(city) && !nzchar(state) && !nzchar(zip)) {
      return(NULL)
    }
    list(city = city, state = state, zip = zip)
  })
  
  # Calculate route
  route_result <- eventReactive(input$calculate, {
    parks <- selected_parks()
    start_input <- start_location_input()
    start_loc <- NULL
    
    if (nrow(parks) < 2) {
      showNotification("Please select at least two parks on the Route Planner map.", type = "warning")
      return(NULL)
    }
    
    if (!is.null(start_input)) {
      if (!nzchar(start_input$city) && !nzchar(start_input$state) && nzchar(start_input$zip)) {
        showNotification(
          "Tip: ZIP-only start locations are allowed, but including city/state often improves accuracy.",
          type = "message",
          duration = 6
        )
      }
      
      geocode_result <- geocode_location(start_input$city, start_input$state, start_input$zip)
      if (isTRUE(geocode_result$success)) {
        start_loc <- list(
          lat = geocode_result$lat,
          lon = geocode_result$lon,
          city = ifelse(nzchar(start_input$city), start_input$city, "ZIP"),
          state = start_input$state,
          zip = start_input$zip
        )
      } else {
        detail_msg <- geocode_result$message
        if (is.null(detail_msg) || !is.character(detail_msg) || !length(detail_msg)) {
          detail_msg <- "Unknown geocoder response."
        }
        showNotification(
          paste("Starting location lookup failed:", detail_msg),
          type = "warning",
          duration = 8
        )
      }
    }
    
    progress <- Progress$new(session, min = 0, max = 1)
    on.exit(progress$close())
    progress$set(message = "Calculating distances...", detail = "Building travel matrix...", value = 0)
    
    limit_value <- suppressWarnings(as.numeric(input$limit_value))
    if (is.na(limit_value) || limit_value <= 0) limit_value <- NULL
    
    matrices <- calculate_distance_matrix(
      parks, input$distance_type, start_loc, progress,
      limit_metric = input$limit_metric, limit_value = limit_value
    )
    
    progress$set(message = "Optimizing route...", detail = "Solving route sequence...", value = 0.65)
    
    # Solve TSP (start from index 1 if custom start location provided)
    start_idx <- if (!is.null(start_loc)) 1 else NULL
    tsp_result <- solve_tsp(matrices$time, start_idx)
    
    # Build result
    tour_indices <- tsp_result$tour
    route_df <- matrices$parks[tour_indices, ]
    route_df$step <- 1:nrow(route_df)
    
    # Calculate segment details using the selected default mode.
    distances <- numeric(0)
    times <- numeric(0)
    travel_modes <- character(0)
    
    segment_geometries <- vector("list", max(0, nrow(route_df) - 1))
    
    for (i in 1:(nrow(route_df) - 1)) {
      progress$set(
        message = "Building route segments...",
        detail = sprintf("Preparing segment %d of %d", i, nrow(route_df) - 1),
        value = 0.7 + (0.25 * (i / max(1, (nrow(route_df) - 1))))
      )
      default_choice <- dplyr::case_when(
        input$distance_type == "driving" ~ "drive",
        input$distance_type == "flying" ~ "flight",
        TRUE ~ "auto"
      )
      drive_info <- get_route_info(
        route_df$longitude[i], route_df$latitude[i],
        route_df$longitude[i + 1], route_df$latitude[i + 1]
      )
      flight_info <- calculate_flight_route(
        route_df$longitude[i], route_df$latitude[i],
        route_df$longitude[i + 1], route_df$latitude[i + 1]
      )
      segment <- route_with_constraints(
        drive_info, flight_info, mode_choice = default_choice,
        limit_metric = input$limit_metric, limit_value = limit_value
      )
      if (!isTRUE(segment$valid)) {
        showNotification("No valid segment route fits the selected limit. Increase the limit and try again.", type = "error", duration = 8)
        return(NULL)
      }
      distances <- c(distances, segment$route$distance_km)
      times <- c(times, segment$route$duration_hours)
      travel_modes <- c(travel_modes, segment$mode)
      segment_geometries[[i]] <- segment$route
    }
    
    route_df$distance_to_next_km <- c(distances, NA)
    route_df$distance_to_next_miles <- c(distances * 0.621371, NA)
    route_df$time_to_next_hours <- c(times, NA)
    route_df$travel_mode <- c(travel_modes, NA)
    
    progress$set(message = "Finalizing route...", detail = "Loading map-ready geometry...", value = 0.97)
    Sys.sleep(0.2)
    progress$set(value = 1)
    
    # Reset segment overrides to default choices after each new calculation.
    default_choice <- dplyr::case_when(
      input$distance_type == "driving" ~ "drive",
      input$distance_type == "flying" ~ "flight",
      TRUE ~ "auto"
    )
    default_overrides <- rep(default_choice, max(0, nrow(route_df) - 1))
    names(default_overrides) <- as.character(seq_len(length(default_overrides)))
    segment_mode_overrides(default_overrides)
    
    return(list(
      route = route_df,
      total_distance = sum(distances),
      total_time = sum(times),
      tour_indices = tour_indices,
      has_start = !is.null(start_loc),
      segment_geometries = segment_geometries,
      limit_metric = input$limit_metric,
      limit_value = limit_value
    ))
  })
  
  computed_route <- reactive({
    base <- route_result()
    if (is.null(base)) return(NULL)
    
    route <- base$route
    if (nrow(route) < 2) return(base)
    
    overrides <- segment_mode_overrides()
    distances <- numeric(0)
    times <- numeric(0)
    travel_modes <- character(0)
    segment_geometries <- vector("list", max(0, nrow(route) - 1))
    
    for (i in 1:(nrow(route) - 1)) {
      choice <- overrides[as.character(i)]
      if (is.na(choice) || !nzchar(choice)) {
        choice <- "auto"
      }
      drive_info <- get_route_info(
        route$longitude[i], route$latitude[i],
        route$longitude[i + 1], route$latitude[i + 1]
      )
      flight_info <- calculate_flight_route(
        route$longitude[i], route$latitude[i],
        route$longitude[i + 1], route$latitude[i + 1]
      )
      segment <- route_with_constraints(
        drive_info, flight_info, mode_choice = choice,
        limit_metric = base$limit_metric, limit_value = base$limit_value
      )
      if (!isTRUE(segment$valid)) {
        showNotification(sprintf("Segment %d exceeds your limit for the selected mode.", i), type = "warning", duration = 6)
        return(base)
      }
      distances <- c(distances, segment$route$distance_km)
      times <- c(times, segment$route$duration_hours)
      travel_modes <- c(travel_modes, segment$mode)
      segment_geometries[[i]] <- segment$route
    }
    
    route$distance_to_next_km <- c(distances, NA)
    route$distance_to_next_miles <- c(distances * 0.621371, NA)
    route$time_to_next_hours <- c(times, NA)
    route$travel_mode <- c(travel_modes, NA)
    
    base$route <- route
    base$total_distance <- sum(distances)
    base$total_time <- sum(times)
    base$segment_geometries <- segment_geometries
    base
  })
  
  # Status text
  output$status_text <- renderText({
    parks <- selected_parks()
    start_input <- start_location_input()
    
    selected_count <- length(selected_states())
    
    if (!is.null(start_input)) {
      sprintf("Ready to calculate route for %d selected parks (%d states/territories selected) starting from %s, %s",
              nrow(parks), selected_count,
              ifelse(nzchar(start_input$city), start_input$city, "entered location"),
              ifelse(nzchar(start_input$state), start_input$state, "USA"))
    } else {
      sprintf("Ready to calculate route for %d selected parks (%d states/territories selected)", nrow(parks), selected_count)
    }
  })
  
  observeEvent(input$go_allparks, {
    updateTabItems(session, "main_tabs", "allparks")
  })
  
  observeEvent(input$go_planner, {
    updateTabItems(session, "main_tabs", "planner")
  })
  
  observeEvent(input$go_routemap, {
    updateTabItems(session, "main_tabs", "routemap")
  })
  
  observeEvent(input$go_route_map_from_details, {
    updateTabItems(session, "main_tabs", "routemap")
  })
  
  output$segment_mode_controls <- renderUI({
    result <- route_result()
    if (is.null(result) || nrow(result$route) < 2) {
      return(tags$em("Calculate a route to enable per-segment travel mode controls."))
    }
    
    route <- result$route
    overrides <- segment_mode_overrides()
    controls <- lapply(seq_len(nrow(route) - 1), function(i) {
      from_label <- paste0(route$name[i], " (", route$state[i], ")")
      to_label <- paste0(route$name[i + 1], " (", route$state[i + 1], ")")
      selected_value <- overrides[as.character(i)]
      if (is.na(selected_value) || !nzchar(selected_value)) selected_value <- "auto"
      selectInput(
        inputId = paste0("segment_mode_", i),
        label = sprintf("Segment %d: %s → %s", i, from_label, to_label),
        choices = c("Auto (Quickest)" = "auto", "Drive" = "drive", "Flight" = "flight"),
        selected = selected_value,
        width = "100%"
      )
    })
    
    tagList(controls)
  })
  
  observe({
    result <- computed_route()
    if (is.null(result) || nrow(result$route) < 2) return()
    
    current_overrides <- segment_mode_overrides()
    for (i in seq_len(nrow(result$route) - 1)) {
      selected_value <- input[[paste0("segment_mode_", i)]]
      if (!is.null(selected_value) && nzchar(selected_value)) {
        current_overrides[as.character(i)] <- selected_value
      }
    }
    segment_mode_overrides(current_overrides)
  })
  
  # Info boxes
  output$total_distance_box <- renderInfoBox({
    result <- computed_route()
    if (is.null(result)) {
      infoBox("Total Distance", "--", "Click Calculate",
              icon = icon("road"), color = "blue")
    } else {
      infoBox("Total Distance",
              sprintf("%.0f km\n(%.0f mi)", result$total_distance,
                      result$total_distance * 0.621371),
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
      n_parks <- if (result$has_start) nrow(result$route) - 1 else nrow(result$route)
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
      avg_dist <- result$total_distance / (nrow(result$route) - 1)
      avg_time <- result$total_time / (nrow(result$route) - 1)
      infoBox("Avg Between Stops",
              sprintf("%.0f km\n%.1fh", avg_dist, avg_time),
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
                          paste0(city, ", ", state),
                          paste0(name, " (", state, ")")),
        `Travel Mode` = travel_mode,
        `Distance (km)` = round(distance_to_next_km, 1),
        `Distance (mi)` = round(distance_to_next_miles, 1),
        `Time (hours)` = round(time_to_next_hours, 1)
      ) %>%
      select(Step = step, Location, `Travel Mode`,
             `Distance (km)`, `Distance (mi)`, `Time (hours)`)
    
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
    label_text <- sprintf("%s \u2192 %s: %.0f km, %.1fh",
                          route$name[i], route$name[i + 1],
                          route$distance_to_next_km[i],
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
        ifelse(name != "START",
               paste0("<b>State:</b> ", state, "<br>",
                      "<b>Date Established:</b> ", date_established, "<br>",
                      "<b>Visitors (2021):</b> ", format(visitors, big.mark = ",", scientific = FALSE), "<br><br>"),
               ""
        ),
        ifelse(!is.na(distance_to_next_km),
               paste0("<b>Next Stop:</b> ", round(distance_to_next_km, 1), " km (",
                      round(distance_to_next_miles, 1), " mi) via ", travel_mode, "<br>",
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
  })
  
  outputOptions(output, "route_map", suspendWhenHidden = FALSE)
}

###################
### Run App
###################
shinyApp(ui = ui, server = server)
