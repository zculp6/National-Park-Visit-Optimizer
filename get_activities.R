library(httr)
library(jsonlite)
library(tidyverse)

api_key <- "6Ks1tnBrm9d1NqXCd2VzVlylUm8ktGMSbBkljXn4"

# --- 1. GET ACTIVITIES DATA ---
# This hits the 'activities/parks' endpoint to see which parks offer which activities
print("Fetching Activities data...")
act_url <- paste0("https://developer.nps.gov/api/v1/activities/parks?api_key=", api_key)
act_response <- GET(act_url)

if (status_code(act_response) == 200) {
  act_data <- fromJSON(content(act_response, "text", encoding = "UTF-8"))$data
  
  activities_df <- act_data %>%
    # Use names_sep to distinguish Activity 'name' from Park 'name'
    unnest(parks, names_sep = "_") %>%
    select(
      Activity_ID = id,
      Activity_Name = name,         # This is the activity (e.g., Hiking)
      Park_Code = parks_parkCode,   # These now have the 'parks_' prefix
      Park_Full_Name = parks_fullName,
      Park_URL = parks_url
    )
  
  write.csv(activities_df, "nps_activities_by_park.csv", row.names = FALSE)
  print("Saved: nps_activities_by_park.csv")
}

# --- 2. GET THINGS TO DO DATA ---
# This endpoint has a lot of data, so we set a high limit
print("Fetching Things To Do data...")
ttd_url <- paste0("https://developer.nps.gov/api/v1/thingstodo?limit=500&api_key=", api_key)
ttd_response <- GET(ttd_url)


extract_park_codes <- function(related_parks_obj) {
  if (is.null(related_parks_obj) || length(related_parks_obj) == 0) return(character(0))
  
  if (is.data.frame(related_parks_obj) && "parkCode" %in% names(related_parks_obj)) {
    return(as.character(related_parks_obj$parkCode))
  }
  
  if (is.list(related_parks_obj)) {
    extracted <- map_chr(related_parks_obj, function(x) {
      if (is.null(x)) return(NA_character_)
      if (is.list(x) && !is.null(x$parkCode)) return(as.character(x$parkCode[[1]]))
      NA_character_
    })
    return(extracted[!is.na(extracted)])
  }
  
  character(0)
}

if (status_code(ttd_response) == 200) {
  ttd_data <- fromJSON(content(ttd_response, "text", encoding = "UTF-8"))$data
  
  ttd_df <- ttd_data %>%
  
    mutate(
      Park_Codes = map(relatedParks, extract_park_codes),
      Image_URL = sapply(images, function(x) if(!is.null(x) && nrow(x) > 0) x$url[1] else NA)
    ) %>%
   
    unnest_longer(Park_Codes, values_to = "Park_Code") %>%
    select(
      Park_Code,
      Title = title,
      Duration = duration,
      Short_Description = shortDescription,
      Image_URL,
      Record_Type = "Thing To Do"
    ) %>%
    filter(!is.na(Park_Code) & nzchar(Park_Code)) %>%
    distinct(Park_Code, Title, .keep_all = TRUE)
  
  print("Fetching Tours data...")
  tours_url <- paste0("https://developer.nps.gov/api/v1/tours?limit=500&api_key=", api_key)
  tours_response <- GET(tours_url)
  tours_df <- tibble()
  
  if (status_code(tours_response) == 200) {
    tours_data <- fromJSON(content(tours_response, "text", encoding = "UTF-8"))$data
    tours_df <- tours_data %>%
      mutate(
        Park_Codes = map(relatedParks, extract_park_codes),
        Image_URL = sapply(images, function(x) if(!is.null(x) && nrow(x) > 0) x$url[1] else NA)
      ) %>%
      unnest_longer(Park_Codes, values_to = "Park_Code") %>%
      select(
        Park_Code,
        Title = title,
        Duration = duration,
        Short_Description = shortDescription,
        Image_URL,
        Record_Type = "Tour"
      ) %>%
      filter(!is.na(Park_Code) & nzchar(Park_Code)) %>%
      distinct(Park_Code, Title, .keep_all = TRUE)
  }
  
  combined_ttd <- bind_rows(ttd_df, tours_df) %>%
    mutate(Park_Code = tolower(Park_Code))
  
  write.csv(combined_ttd, "nps_things_to_do.csv", row.names = FALSE)
  print(paste("Saved: nps_things_to_do.csv with", nrow(combined_ttd), "entries."))
}