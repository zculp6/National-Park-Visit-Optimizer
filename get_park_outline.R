library(httr)
library(jsonlite)
library(tidyverse)

api_key <- "6Ks1tnBrm9d1NqXCd2VzVlylUm8ktGMSbBkljXn4"

# 1. Helper function to fetch data
fetch_endpoint <- function(endpoint, limit = 1000) {
  url <- sprintf("https://developer.nps.gov/api/v1/%s?limit=%s&api_key=%s", endpoint, limit, api_key)
  response <- GET(url)
  if (status_code(response) != 200) return(tibble())
  fromJSON(content(response, "text", encoding = "UTF-8"))$data %>% as_tibble()
}

# 2. Helper function to standardize point data
# Updated Helper: Robust Coordinate Extraction
extract_coords <- function(df, type_name, name_col = "name") {
  if (nrow(df) == 0) return(tibble())
  
  # 1. HANDLE PARK CODES (Robust check)
  if (!"parkCode" %in% colnames(df)) {
    if ("relatedParks" %in% colnames(df)) {
      df$parkCode <- sapply(df$relatedParks, function(x) {
        if(is.data.frame(x) && "parkCode" %in% colnames(x)) x$parkCode[1] else NA_character_
      })
    } else {
      df$parkCode <- NA_character_
    }
  }
  
  # 2. HANDLE MISSING NAME COLUMNS
  # Use 'title' if 'name' doesn't exist (common in 'places' and 'webcams')
  actual_name_col <- if (name_col %in% colnames(df)) name_col else if ("title" %in% colnames(df)) "title" else NA
  
  df %>%
    mutate(
      # Convert lat/long to numeric safely
      latitude = as.numeric(latitude),
      longitude = as.numeric(longitude),
      # Create the 'type' column with the actual string value
      type = type_name,
      # Normalize park code to uppercase
      park_code = toupper(as.character(parkCode)),
      # Assign the entity name based on the found column
      entity_name = if(!is.na(actual_name_col)) as.character(.data[[actual_name_col]]) else "Unnamed Site"
    ) %>%
    # Filter out rows that are missing coordinates
    filter(!is.na(latitude) & !is.na(longitude)) %>%
    # Now we select the columns we just created/processed
    select(park_code, entity_name, type, latitude, longitude)
}

message("Fetching expanded coordinate data...")

# Fetching all available spatial endpoints
visitor_centers <- fetch_endpoint("visitorcenters")
campgrounds     <- fetch_endpoint("campgrounds")
places          <- fetch_endpoint("places")
parking_lots    <- fetch_endpoint("parkinglots")
webcams         <- fetch_endpoint("webcams")

# Combine all point data into one master CSV
all_points <- bind_rows(
  extract_coords(visitor_centers, "Visitor Center"),
  extract_coords(campgrounds, "Campground"),
  extract_coords(places, "Point of Interest"),
  extract_coords(parking_lots, "Parking"),
  extract_coords(webcams, "Webcam")
)

write.csv(all_points, "nps_all_coordinates.csv", row.names = FALSE)
message(sprintf("Successfully saved %d coordinates to nps_all_coordinates.csv", nrow(all_points)))