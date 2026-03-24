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
  park_details <- read.csv("nps_park_details.csv", stringsAsFactors = FALSE)
  park_images <- read.csv("national_parks_images.csv", stringsAsFactors = FALSE)
  
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
      visitors = Recreation.visitors..2021..11.
    ) %>%
    select(name = Name, states, latitude, longitude,
           date_established, area, visitors) %>%
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
  
  normalize_park_name <- function(x) {
    x %>%
      str_to_lower() %>%
      str_replace_all("[^a-z0-9]+", " ") %>%
      str_squish()
  }
  
  phone_fmt <- function(x) {
    digits <- str_replace_all(x, "[^0-9]", "")
    ifelse(
      nchar(digits) == 10,
      sprintf("(%s)-%s-%s", substr(digits, 1, 3), substr(digits, 4, 6), substr(digits, 7, 10)),
      x
    )
  }
  
  details_clean <- park_details %>%
    mutate(
      name_key = normalize_park_name(Park_Name),
      phone = phone_fmt(Phone)
    ) %>%
    select(
      park_code = Park_Code,
      name_key,
      city = City,
      phone,
      email = Email,
      hours = Monday_Hours,
      website = Website_URL,
      description = Description
    ) %>%
    distinct(name_key, .keep_all = TRUE)
  
  image_counts <- park_images %>%
    group_by(Park_Code) %>%
    summarise(image_count = n(), .groups = "drop") %>%
    rename(park_code = Park_Code)
  
  park_coords <- park_coords %>%
    mutate(name_key = normalize_park_name(name)) %>%
    left_join(details_clean, by = "name_key") %>%
    left_join(image_counts, by = "park_code") %>%
    mutate(image_count = replace_na(image_count, 0L)) %>%
    select(-name_key)
  
  return(park_coords)
}

# Load park outline/coordinate data
load_park_outline_data <- function() {
  if (file.exists("nps_all_coordinates.csv")) {
    coords <- read.csv("nps_all_coordinates.csv", stringsAsFactors = FALSE)
    coords %>%
      mutate(
        park_code = toupper(trimws(park_code)),
        type = trimws(type)
      ) %>%
      filter(!is.na(latitude) & !is.na(longitude) & !is.na(park_code))
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

build_image_gallery_html <- function(park_name, image_df) {
  images <- image_df %>% filter(Park_Name == park_name)
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
    "<div class='park-gallery' id='", gallery_id, "' style='max-width:340px;'>",
    slides,
    "<div style='display:flex;justify-content:space-between;margin-top:6px;'>",
    "<button onclick=\"window.parkGalleryNav('", gallery_id, "', -1)\" style='padding:2px 8px;'>&larr;</button>",
    "<button onclick=\"window.parkGalleryNav('", gallery_id, "', 1)\" style='padding:2px 8px;'>&rarr;</button>",
    "</div></div>"
  )
}

build_park_popup <- function(park_row, image_df) {
  gallery_html <- build_image_gallery_html(park_row$name, image_df)
  paste0(
    "<h4><b>", park_row$name, "</b></h4>",
    "<b>State/Territory:</b> ", park_row$state, "<br>",
    "<b>Date Established:</b> ", park_row$date_established, "<br>",
    "<b>Area:</b> ", park_row$area, "<br>",
    "<b>Visitors (2021):</b> ", format(park_row$visitors, big.mark = ",", scientific = FALSE), "<br>",
    "<b>City:</b> ", park_row$city, "<br>",
    "<b>Phone:</b> ", park_row$phone, "<br>",
    "<b>Email:</b> ", park_row$email, "<br>",
    "<b>Hours:</b> ", park_row$hours, "<br>",
    "<b>Website:</b> <a href='", park_row$website, "' target='_blank'>", park_row$website, "</a><br><br>",
    "<b>Description:</b><br>", park_row$description, "<br><br>",
    "<b>Gallery (", park_row$image_count, "):</b><br>",
    gallery_html
  )
}

load_visit_time_defaults <- function() {
  visit_df <- read.csv("Public Use Statistics.csv", stringsAsFactors = FALSE, check.names = FALSE)
  visit_df %>%
    transmute(
      park_code = str_to_lower(trimws(UnitCodeTotal)),
      recreation_visits_total = as.numeric(gsub(",", "", RecreationVisitsTotal)),
      recreation_hours_total = as.numeric(gsub(",", "", RecreationHoursTotal))
    ) %>%
    filter(!is.na(park_code), park_code != "", recreation_visits_total > 0) %>%
    mutate(default_visit_hours = pmax(recreation_hours_total / recreation_visits_total, 0.5)) %>%
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
    lat = zip_match$lat,
    lng = zip_match$lng,
    city = zip_match$city,
    state = zip_match$state_name
  )
}

find_driving_route_geometry <- function(start_lat, start_lon, end_lat, end_lon) {
  url <- sprintf(
    "https://router.project-osrm.org/route/v1/driving/%f,%f;%f,%f?overview=full&geometries=geojson",
    start_lon, start_lat, end_lon, end_lat
  )
  
  tryCatch({
    response <- GET(url)
    if (status_code(response) == 200) {
      result <- fromJSON(content(response, "text", encoding = "UTF-8"))
      if (!is.null(result$routes) && length(result$routes) > 0) {
        coords_list <- result$routes[[1]]$geometry$coordinates
        distance_m <- result$routes[[1]]$distance
        duration_s <- result$routes[[1]]$duration
        
        return(list(
          geometry = coords_list,
          distance_km = distance_m / 1000,
          duration_hours = duration_s / 3600
        ))
      }
    }
    NULL
  }, error = function(e) NULL)
}

segment_route_geometry <- function(from_lat, from_lon, to_lat, to_lon, mode = "Drive") {
  if (mode == "Flight") {
    start_airport_info <- find_nearest_airport(from_lat, from_lon)
    end_airport_info <- find_nearest_airport(to_lat, to_lon)
    
    start_airport <- start_airport_info$airport
    end_airport <- end_airport_info$airport
    
    drive_to_airport <- find_driving_route_geometry(from_lat, from_lon, start_airport$lat, start_airport$lon)
    drive_from_airport <- find_driving_route_geometry(end_airport$lat, end_airport$lon, to_lat, to_lon)
    
    flight_coords <- list(
      c(start_airport$lon, start_airport$lat),
      c(end_airport$lon, end_airport$lat)
    )
    
    flight_distance_km <- distHaversine(
      c(start_airport$lon, start_airport$lat),
      c(end_airport$lon, end_airport$lat)
    ) / 1000
    
    flight_time_hours <- flight_distance_km / 800
    
    return(list(
      drive_to_airport_geometry = if (!is.null(drive_to_airport)) drive_to_airport$geometry else NULL,
      flight_geometry = flight_coords,
      drive_from_airport_geometry = if (!is.null(drive_from_airport)) drive_from_airport$geometry else NULL,
      total_distance_km = (if (!is.null(drive_to_airport)) drive_to_airport$distance_km else 0) +
        flight_distance_km +
        (if (!is.null(drive_from_airport)) drive_from_airport$distance_km else 0),
      total_duration_hours = (if (!is.null(drive_to_airport)) drive_to_airport$duration_hours else 0) +
        flight_time_hours +
        (if (!is.null(drive_from_airport)) drive_from_airport$duration_hours else 0)
    ))
  } else {
    drive <- find_driving_route_geometry(from_lat, from_lon, to_lat, to_lon)
    if (!is.null(drive)) {
      return(list(
        geometry = drive$geometry,
        total_distance_km = drive$distance_km,
        total_duration_hours = drive$duration_hours
      ))
    }
    return(NULL)
  }
}

build_mode_matrix <- function(locations, default_distance_type = "drive") {
  n <- nrow(locations)
  mode_matrix <- matrix("Drive", nrow = n, ncol = n)
  
  for (i in 1:n) {
    for (j in 1:n) {
      if (i == j) next
      
      from_region <- classify_park_region(locations$state[i])
      to_region <- classify_park_region(locations$state[j])
      
      distance_km <- distHaversine(
        c(locations$longitude[i], locations$latitude[i]),
        c(locations$longitude[j], locations$latitude[j])
      ) / 1000
      
      should_fly <- FALSE
      if (from_region != to_region) {
        should_fly <- TRUE
      } else if (from_region == "mainland" && to_region == "mainland") {
        if (default_distance_type == "flight") {
          should_fly <- (distance_km > 500)
        } else {
          should_fly <- (distance_km > 1000)
        }
      }
      
      mode_matrix[i, j] <- ifelse(should_fly, "Flight", "Drive")
    }
  }
  
  return(mode_matrix)
}

route_with_geometry <- function(ordered_indices, locations_df, mode_matrix = NULL) {
  if (is.null(mode_matrix)) {
    mode_matrix <- build_mode_matrix(locations_df)
  }
  
  segment_geometries <- list()
  
  ordered_locs <- locations_df[ordered_indices, ]
  n_stops <- nrow(ordered_locs)
  
  if (n_stops < 2) {
    return(list(
      route = ordered_locs %>%
        mutate(
          step = row_number(),
          distance_to_next_km = NA_real_,
          time_to_next_hours = NA_real_,
          travel_mode = NA_character_
        ),
      segment_geometries = list()
    ))
  }
  
  distance_to_next <- numeric(n_stops)
  time_to_next <- numeric(n_stops)
  travel_modes <- character(n_stops)
  
  for (i in 1:(n_stops - 1)) {
    from_idx <- ordered_indices[i]
    to_idx <- ordered_indices[i + 1]
    mode <- mode_matrix[from_idx, to_idx]
    
    segment <- segment_route_geometry(
      ordered_locs$latitude[i],
      ordered_locs$longitude[i],
      ordered_locs$latitude[i + 1],
      ordered_locs$longitude[i + 1],
      mode
    )
    
    if (!is.null(segment)) {
      distance_to_next[i] <- segment$total_distance_km
      time_to_next[i] <- segment$total_duration_hours
      travel_modes[i] <- mode
      segment_geometries[[i]] <- segment
    } else {
      fallback_dist <- distHaversine(
        c(ordered_locs$longitude[i], ordered_locs$latitude[i]),
        c(ordered_locs$longitude[i + 1], ordered_locs$latitude[i + 1])
      ) / 1000
      distance_to_next[i] <- fallback_dist
      time_to_next[i] <- fallback_dist / 80
      travel_modes[i] <- mode
      segment_geometries[[i]] <- NULL
    }
  }
  
  distance_to_next[n_stops] <- NA_real_
  time_to_next[n_stops] <- NA_real_
  travel_modes[n_stops] <- NA_character_
  
  route_df <- ordered_locs %>%
    mutate(
      step = row_number(),
      distance_to_next_km = distance_to_next,
      time_to_next_hours = time_to_next,
      travel_mode = travel_modes
    )
  
  return(list(
    route = route_df,
    segment_geometries = segment_geometries
  ))
}

haversine_distance <- function(lat1, lon1, lat2, lon2) {
  distHaversine(c(lon1, lat1), c(lon2, lat2)) / 1000
}

tsp_greedy_route <- function(locations_df, start_index = NULL, end_index = NULL, mode_matrix = NULL) {
  n <- nrow(locations_df)
  if (n < 2) return(1:n)
  
  if (is.null(mode_matrix)) {
    mode_matrix <- build_mode_matrix(locations_df)
  }
  
  dist_matrix <- matrix(0, nrow = n, ncol = n)
  for (i in 1:n) {
    for (j in 1:n) {
      if (i != j) {
        dist_matrix[i, j] <- haversine_distance(
          locations_df$latitude[i],
          locations_df$longitude[i],
          locations_df$latitude[j],
          locations_df$longitude[j]
        )
        
        if (!is.null(mode_matrix) && mode_matrix[i, j] == "Flight") {
          dist_matrix[i, j] <- dist_matrix[i, j] * 0.3
        }
      }
    }
  }
  
  if (!is.null(start_index) && !is.null(end_index)) {
    visited <- logical(n)
    route <- integer(n)
    route[1] <- start_index
    visited[start_index] <- TRUE
    current <- start_index
    
    for (step in 2:(n - 1)) {
      candidates <- which(!visited & (1:n) != end_index)
      if (length(candidates) == 0) break
      
      dists <- dist_matrix[current, candidates]
      next_idx <- candidates[which.min(dists)]
      
      route[step] <- next_idx
      visited[next_idx] <- TRUE
      current <- next_idx
    }
    
    route[n] <- end_index
    return(route)
    
  } else if (!is.null(start_index)) {
    visited <- logical(n)
    route <- integer(n)
    route[1] <- start_index
    visited[start_index] <- TRUE
    current <- start_index
    
    for (step in 2:n) {
      candidates <- which(!visited)
      if (length(candidates) == 0) break
      
      dists <- dist_matrix[current, candidates]
      next_idx <- candidates[which.min(dists)]
      
      route[step] <- next_idx
      visited[next_idx] <- TRUE
      current <- next_idx
    }
    
    return(route)
    
  } else {
    visited <- logical(n)
    route <- integer(n)
    route[1] <- 1
    visited[1] <- TRUE
    current <- 1
    
    for (step in 2:n) {
      candidates <- which(!visited)
      if (length(candidates) == 0) break
      
      dists <- dist_matrix[current, candidates]
      next_idx <- candidates[which.min(dists)]
      
      route[step] <- next_idx
      visited[next_idx] <- TRUE
      current <- next_idx
    }
    
    return(route)
  }
}

###################
### UI
###################

ui <- dashboardPage(
  dashboardHeader(title = "National Parks Road Trip Planner"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Plan Your Trip", tabName = "planner", icon = icon("route")),
      menuItem("Individual Park View", tabName = "park_detail", icon = icon("mountain"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .gallery-slide { display: none; }
        .gallery-slide.active { display: block; }
        .content-wrapper, .right-side { background-color: #ecf0f5; }
      ")),
      tags$script(HTML("
        window.parkGalleryNav = function(galleryId, direction) {
          var slides = document.querySelectorAll('[data-gallery=\"' + galleryId + '\"]');
          var currentIdx = Array.from(slides).findIndex(s => s.classList.contains('active'));
          if (currentIdx === -1) return;
          slides[currentIdx].classList.remove('active');
          var newIdx = (currentIdx + direction + slides.length) % slides.length;
          slides[newIdx].classList.add('active');
        };
      "))
    ),
    
    tabItems(
      # Trip Planner Tab
      tabItem(
        tabName = "planner",
        fluidRow(
          box(
            title = "Settings", width = 4, solidHeader = TRUE, status = "primary",
            textInput("user_zip", "Your ZIP Code:", value = "43215"),
            selectInput("distance_unit", "Distance Unit:", choices = c("Miles" = "mi", "Kilometers" = "km"), selected = "mi"),
            selectInput("default_distance_type", "Default Travel Mode:", 
                        choices = c("Prefer Driving" = "drive", "Prefer Flying" = "flight"),
                        selected = "drive"),
            checkboxInput("include_start", "Include my location as starting point", value = TRUE),
            checkboxInput("include_end", "Return to my location at the end", value = FALSE),
            actionButton("compute_route", "Compute Route", class = "btn-success")
          ),
          
          box(
            title = "Select Parks", width = 8, solidHeader = TRUE, status = "info",
            fluidRow(
              column(6, selectInput("filter_state", "Filter by State:", 
                                    choices = c("All States" = ""), selected = "")),
              column(6, selectInput("sort_by", "Sort by:", 
                                    choices = c("Name" = "name", "Visitors" = "visitors", "Date Established" = "date_established"),
                                    selected = "name"))
            ),
            DTOutput("parks_table")
          )
        ),
        
        fluidRow(
          box(
            title = "Route Details", width = 4, solidHeader = TRUE, status = "warning",
            verbatimTextOutput("route_summary"),
            hr(),
            h4("Segment Travel Modes"),
            uiOutput("segment_mode_controls"),
            hr(),
            h4("Park Visit Times (hours)"),
            uiOutput("park_visit_time_controls")
          ),
          
          box(
            title = "Map", width = 8, solidHeader = TRUE, status = "success",
            leafletOutput("route_map", height = 600)
          )
        )
      ),
      
      # Individual Park Detail Tab
      tabItem(
        tabName = "park_detail",
        fluidRow(
          box(
            title = "Select a Park", width = 12, solidHeader = TRUE, status = "primary",
            selectInput("selected_park", "Choose a park to view details:", choices = NULL),
            checkboxGroupInput("poi_types", "Show Points of Interest:",
                               choices = c("Visitor Center", "Campground", "Point of Interest", "Parking", "Webcam"),
                               selected = c("Visitor Center", "Campground"),
                               inline = TRUE)
          )
        ),
        fluidRow(
          box(
            title = "Park Map with Points of Interest", width = 12, solidHeader = TRUE, status = "success",
            leafletOutput("park_detail_map", height = 600)
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
  
  # Load data
  park_data <- reactive({
    load_park_data()
  })
  
  park_outline_data <- reactive({
    load_park_outline_data()
  })
  
  park_images <- reactive({
    if (file.exists("national_parks_images.csv")) {
      read.csv("national_parks_images.csv", stringsAsFactors = FALSE)
    } else {
      tibble(Park_Code = character(), Park_Name = character(), Image_URL = character())
    }
  })
  
  visit_defaults <- reactive({
    if (file.exists("Public Use Statistics.csv")) {
      load_visit_time_defaults()
    } else {
      tibble(park_code = character(), default_visit_hours = numeric())
    }
  })
  
  get_default_visit_hours <- function(park_name) {
    park_match <- park_data() %>% filter(name == park_name) %>% slice(1)
    if (nrow(park_match) == 0 || is.na(park_match$park_code)) return(4)
    
    default_row <- visit_defaults() %>% filter(park_code == str_to_lower(park_match$park_code))
    if (nrow(default_row) > 0) {
      return(round(default_row$default_visit_hours[1], 1))
    }
    return(4)
  }
  
  # Update state filter choices
  observe({
    states <- sort(unique(park_data()$state))
    updateSelectInput(session, "filter_state", choices = c("All States" = "", states))
  })
  
  # Update park selection for detail view
  observe({
    parks <- park_data()
    park_names <- sort(unique(parks$name))
    updateSelectInput(session, "selected_park", choices = park_names, selected = park_names[1])
  })
  
  # Filtered and sorted parks
  filtered_parks <- reactive({
    parks <- park_data()
    
    if (input$filter_state != "") {
      parks <- parks %>% filter(state == input$filter_state)
    }
    
    if (input$sort_by == "name") {
      parks <- parks %>% arrange(name)
    } else if (input$sort_by == "visitors") {
      parks <- parks %>% arrange(desc(visitors))
    } else if (input$sort_by == "date_established") {
      parks <- parks %>% arrange(date_established)
    }
    
    parks
  })
  
  # Parks table
  output$parks_table <- renderDT({
    datatable(
      filtered_parks() %>%
        select(name, state, date_established, area, visitors),
      selection = "multiple",
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })
  
  # Track selected parks
  selected_parks <- reactiveVal(NULL)
  
  observeEvent(input$parks_table_rows_selected, {
    selected_parks(input$parks_table_rows_selected)
  })
  
  # Reactive values for route computation
  computed_route <- reactiveVal(NULL)
  segment_mode_overrides <- reactiveVal(list())
  park_visit_time_overrides <- reactiveVal(list())
  
  selected_distance_multiplier <- reactive({
    if (input$distance_unit == "mi") 0.621371 else 1
  })
  
  selected_distance_label <- reactive({
    if (input$distance_unit == "mi") "miles" else "km"
  })
  
  # Compute route
  observeEvent(input$compute_route, {
    selected_indices <- selected_parks()
    
    if (is.null(selected_indices) || length(selected_indices) == 0) {
      showNotification("Please select at least one park.", type = "warning")
      return()
    }
    
    selected_data <- filtered_parks()[selected_indices, ]
    
    # Get user location
    user_coords <- get_zip_coordinates(input$user_zip)
    if (is.null(user_coords)) {
      showNotification("Invalid ZIP code.", type = "error")
      return()
    }
    
    # Build locations dataframe
    locations <- selected_data
    start_index <- NULL
    end_index <- NULL
    
    if (input$include_start) {
      start_row <- tibble(
        name = "START",
        state = user_coords$state,
        latitude = user_coords$lat,
        longitude = user_coords$lng,
        date_established = NA,
        area = NA,
        visitors = NA,
        park_code = NA,
        city = user_coords$city,
        phone = NA,
        email = NA,
        hours = NA,
        website = NA,
        description = "Your starting location",
        image_count = 0
      )
      locations <- bind_rows(start_row, locations)
      start_index <- 1
    }
    
    if (input$include_end) {
      end_row <- tibble(
        name = "END",
        state = user_coords$state,
        latitude = user_coords$lat,
        longitude = user_coords$lng,
        date_established = NA,
        area = NA,
        visitors = NA,
        park_code = NA,
        city = user_coords$city,
        phone = NA,
        email = NA,
        hours = NA,
        website = NA,
        description = "Your ending location",
        image_count = 0
      )
      locations <- bind_rows(locations, end_row)
      end_index <- nrow(locations)
    }
    
    mode_matrix <- build_mode_matrix(locations, input$default_distance_type)
    ordered_indices <- tsp_greedy_route(locations, start_index, end_index, mode_matrix)
    result <- route_with_geometry(ordered_indices, locations, mode_matrix)
    
    result$ordered_indices <- ordered_indices
    result$locations <- locations
    result$default_distance_type <- input$default_distance_type
    result$base_mode_matrix <- mode_matrix
    result$has_start <- input$include_start
    result$has_end <- input$include_end
    
    computed_route(result)
    segment_mode_overrides(list())
    park_visit_time_overrides(list())
  })
  
  # Route summary
  output$route_summary <- renderText({
    result <- computed_route()
    if (is.null(result)) return("No route computed yet.")
    
    route <- result$route
    total_distance <- sum(route$distance_to_next_km, na.rm = TRUE) * selected_distance_multiplier()
    total_travel_time <- sum(route$time_to_next_hours, na.rm = TRUE)
    
    parks_only <- route %>% filter(!name %in% c("START", "END"))
    visit_overrides <- park_visit_time_overrides()
    
    total_visit_time <- 0
    for (park_name in parks_only$name) {
      if (!is.na(visit_overrides[[park_name]])) {
        total_visit_time <- total_visit_time + as.numeric(visit_overrides[[park_name]])
      } else {
        total_visit_time <- total_visit_time + get_default_visit_hours(park_name)
      }
    }
    
    total_time <- total_travel_time + total_visit_time
    
    paste0(
      "Total Distance: ", round(total_distance, 1), " ", selected_distance_label(), "\n",
      "Travel Time: ", round(total_travel_time, 1), " hours\n",
      "Visit Time: ", round(total_visit_time, 1), " hours\n",
      "Total Trip Time: ", round(total_time, 1), " hours (", round(total_time / 24, 1), " days)\n",
      "Number of Parks: ", nrow(parks_only)
    )
  })
  
  # Render route map
  observe({
    result <- computed_route()
    if (is.null(result)) {
      output$route_map <- renderLeaflet({
        leaflet() %>%
          addTiles() %>%
          setView(lng = -98.5795, lat = 39.8283, zoom = 4)
      })
      return()
    }
    
    route <- result$route
    
    output$route_map <- renderLeaflet({
      map <- leaflet() %>%
        addTiles() %>%
        fitBounds(
          lng1 = min(route$longitude),
          lat1 = min(route$latitude),
          lng2 = max(route$longitude),
          lat2 = max(route$latitude)
        )
      
      map
    })
  })
  
  # Update route on map
  observe({
    result <- computed_route()
    if (is.null(result)) return()
    
    route <- result$route
    map_proxy <- leafletProxy("route_map")
    
    map_proxy %>%
      clearMarkers() %>%
      clearShapes() %>%
      clearControls()
    
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
        current_overrides[as.character(i)] <- selected_value
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
        visit_overrides[park_name] <- visit_value
      }
    }
    park_visit_time_overrides(visit_overrides)
  })
  
  # Recompute route details when segment overrides change
  observeEvent(segment_mode_overrides(), {
    result <- computed_route()
    if (is.null(result) || is.null(result$ordered_indices) || is.null(result$locations)) return()
    
    mode_matrix <- build_mode_matrix(result$locations, result$default_distance_type)
    overrides <- segment_mode_overrides()
    
    route_indices <- result$ordered_indices
    for (i in seq_len(length(route_indices) - 1)) {
      override_val <- overrides[[as.character(i)]]
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
  }, ignoreInit = TRUE)
  
  outputOptions(output, "route_map", suspendWhenHidden = FALSE)
  output$status_text <- renderText({ "" })
  
  ###################
  ### Individual Park Detail View
  ###################
  
  # Get icon for POI type
  get_poi_icon <- function(type) {
    icon_map <- list(
      "Visitor Center" = makeAwesomeIcon(icon = "info-circle", markerColor = "blue", library = "fa"),
      "Campground" = makeAwesomeIcon(icon = "campground", markerColor = "green", library = "fa"),
      "Point of Interest" = makeAwesomeIcon(icon = "star", markerColor = "orange", library = "fa"),
      "Parking" = makeAwesomeIcon(icon = "parking", markerColor = "gray", library = "fa"),
      "Webcam" = makeAwesomeIcon(icon = "camera", markerColor = "red", library = "fa")
    )
    icon_map[[type]]
  }
  
  # Render individual park detail map
  output$park_detail_map <- renderLeaflet({
    req(input$selected_park)
    
    # Get selected park data
    park <- park_data() %>% filter(name == input$selected_park) %>% slice(1)
    
    if (nrow(park) == 0) {
      return(leaflet() %>% addTiles() %>% setView(lng = -98.5795, lat = 39.8283, zoom = 4))
    }
    
    # Get park code
    park_code_val <- park$park_code
    
    # Create base map centered on park
    map <- leaflet() %>%
      addTiles() %>%
      setView(lng = park$longitude, lat = park$latitude, zoom = 10)
    
    # Add main park marker
    map <- map %>%
      addAwesomeMarkers(
        lng = park$longitude,
        lat = park$latitude,
        icon = makeAwesomeIcon(icon = "mountain", markerColor = "darkred", library = "fa"),
        popup = build_park_popup(park, park_images()),
        label = park$name
      )
    
    # Add POI markers if park code exists and data available
    if (!is.na(park_code_val) && nrow(park_outline_data()) > 0) {
      poi_data <- park_outline_data() %>%
        filter(park_code == toupper(park_code_val)) %>%
        filter(type %in% input$poi_types)
      
      if (nrow(poi_data) > 0) {
        # Group by type for layer control
        for (poi_type in unique(poi_data$type)) {
          type_data <- poi_data %>% filter(type == poi_type)
          
          map <- map %>%
            addAwesomeMarkers(
              data = type_data,
              lng = ~longitude,
              lat = ~latitude,
              icon = get_poi_icon(poi_type),
              popup = ~paste0("<b>", entity_name, "</b><br>Type: ", type),
              label = ~entity_name,
              group = poi_type
            )
        }
        
        # Add layers control
        map <- map %>%
          addLayersControl(
            overlayGroups = unique(poi_data$type),
            options = layersControlOptions(collapsed = FALSE)
          )
      }
    }
    
    map
  })
}

###################
### Run App
###################
shinyApp(ui = ui, server = server)