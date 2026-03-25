library(httr)
library(jsonlite)
library(tidyverse)

api_key <- "6Ks1tnBrm9d1NqXCd2VzVlylUm8ktGMSbBkljXn4"

# --- HELPER FUNCTIONS ---
# Extracts park codes from the 'relatedParks' list-column (Things To Do)
extract_park_codes_ttd <- function(related_parks_obj) {
  if (is.null(related_parks_obj) || length(related_parks_obj) == 0) return(NA_character_)
  
  if (is.data.frame(related_parks_obj)) {
    if ("parkCode" %in% names(related_parks_obj)) return(as.character(related_parks_obj$parkCode))
  }
  
  if (is.list(related_parks_obj)) {
    extracted <- sapply(related_parks_obj, function(x) {
      if (is.list(x) && !is.null(x$parkCode)) return(as.character(x$parkCode))
      return(NA_character_)
    })
    return(as.character(unlist(extracted)))
  }
  return(NA_character_)
}

# Ensures columns exist. Does NOT force conversion here to avoid size mismatch errors.
standardize_columns <- function(df, target_cols) {
  for (col in target_cols) {
    if (!(col %in% names(df))) {
      df[[col]] <- as.character(NA)
    }
  }
  return(df)
}

# --- 1. GET ACTIVITIES DATA ---
print("Fetching Activities data...")
act_url <- paste0("https://developer.nps.gov/api/v1/activities/parks?api_key=", api_key)
act_response <- GET(act_url)

if (status_code(act_response) == 200) {
  act_data <- fromJSON(content(act_response, "text", encoding = "UTF-8"))$data
  activities_df <- act_data %>%
    unnest(parks, names_sep = "_") %>%
    select(Activity_ID = id, Activity_Name = name, Park_Code = parks_parkCode, 
           Park_Full_Name = parks_fullName, Park_URL = parks_url)
  
  write.csv(activities_df, "nps_activities_by_park.csv", row.names = FALSE)
  print("Activities dataframe ready in environment.")
}

# --- 2. GET THINGS TO DO DATA ---
print("Fetching Things To Do...")
ttd_url <- paste0("https://developer.nps.gov/api/v1/thingstodo?limit=500&api_key=", api_key)
ttd_response <- GET(ttd_url)
ttd_df <- tibble()

if (status_code(ttd_response) == 200) {
  ttd_raw <- fromJSON(content(ttd_response, "text", encoding = "UTF-8"))$data
  ttd_df <- as_tibble(ttd_raw) %>%
    standardize_columns(c("relatedParks", "images", "title", "duration", "shortDescription")) %>%
    mutate(
      Park_Codes = map(relatedParks, extract_park_codes),
      Image_URL = sapply(images, function(x) if(!is.null(x) && is.data.frame(x) && nrow(x) > 0) x$url[1] else NA_character_),
      Record_Type = "Thing To Do"
    ) %>%
    unnest_longer(Park_Codes, values_to = "Park_Code") %>%
    select(Park_Code, Title = title, Duration = duration, Short_Description = shortDescription, Image_URL, Record_Type) %>%
    # Final cleanup previously done after combine
    mutate(Park_Code = tolower(as.character(Park_Code))) %>%
    filter(!is.na(Park_Code) & Park_Code != "na") %>%
    distinct(Park_Code, Title, .keep_all = TRUE)
  
  print(paste("Things To Do dataframe ready with", nrow(ttd_df), "entries."))
}

# --- 3. GET TOURS DATA ---
print("Fetching Tours...")
tours_url <- paste0("https://developer.nps.gov/api/v1/tours?limit=500&api_key=", api_key)
tours_response <- GET(tours_url)
tours_df <- tibble()

if (status_code(tours_response) == 200) {
  tours_raw <- fromJSON(content(tours_response, "text", encoding = "UTF-8"))$data
  
  if (nrow(tours_raw) > 0) {
    tours_df <- as_tibble(tours_raw) %>%
      # Tours uses 'park', 'durationMin', and 'description'
      standardize_columns(c("park", "images", "title", "durationMin", "description")) %>%
      mutate(
        Park_Code = if (is.data.frame(park)) {
          park$parkCode
        } else {
          sapply(park, function(x) {
            if (is.list(x) && !is.null(x$parkCode)) x$parkCode else NA_character_
          })
        },
        Image_URL = sapply(images, function(x) {
          if(!is.null(x) && is.data.frame(x) && nrow(x) > 0) x$url[1] else NA_character_
        })
      ) %>%
      transmute(
        Park_Code = tolower(as.character(Park_Code)),
        Title = as.character(title),
        Duration = as.character(durationMin),
        Short_Description = as.character(description),
        Image_URL = as.character(Image_URL),
        Record_Type = "Tour"
      ) %>%
      filter(!is.na(Park_Code) & Park_Code != "na") %>%
      distinct(Park_Code, Title, .keep_all = TRUE)
    
    print(paste("Tours dataframe (tours_df) ready with", nrow(tours_df), "entries."))
  }
}

# Combine the two dataframes into one master list
combined_ttd_tours <- bind_rows(ttd_df, tours_df) %>%
  # Sort by Park Code and then Title so it's organized
  arrange(Park_Code, Title)

# Print a summary to confirm it worked
print(paste("Total 'Things To Do' records:", nrow(ttd_df)))
print(paste("Total 'Tours' records:", nrow(tours_df)))
print(paste("Combined master list total:", nrow(combined_ttd_tours)))

# View the top of the new combined dataframe
head(combined_ttd_tours)

write.csv(combined_ttd_tours, "nps_things_to_do.csv", row.names = FALSE)
