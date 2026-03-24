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

if (status_code(ttd_response) == 200) {
  ttd_data <- fromJSON(content(ttd_response, "text", encoding = "UTF-8"))$data
  
  ttd_df <- ttd_data %>%
    # 1. Extract the parkCode from the nested 'relatedParks' list
    # 2. Extract the first image URL from the nested 'images' list
    mutate(
      Park_Code = sapply(relatedParks, function(x) if(!is.null(x) && nrow(x) > 0) x$parkCode[1] else NA),
      Image_URL = sapply(images, function(x) if(!is.null(x) && nrow(x) > 0) x$url[1] else NA)
    ) %>%
    # Now 'Park_Code' exists because we created it above!
    select(
      Park_Code,
      Title = title,
      Duration = duration,
      Short_Description = shortDescription,
      Image_URL
    ) %>%
    # Optional: Remove rows where Park_Code is missing
    filter(!is.na(Park_Code))
  
  write.csv(ttd_df, "nps_things_to_do.csv", row.names = FALSE)
  print(paste("Saved: nps_things_to_do.csv with", nrow(ttd_df), "entries."))
}