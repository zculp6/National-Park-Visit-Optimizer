library(httr)
library(jsonlite)
library(tidyverse)

api_key <- "6Ks1tnBrm9d1NqXCd2VzVlylUm8ktGMSbBkljXn4"

# 1. Fetch the main Parks data
print("Fetching Park details...")
url <- paste0("https://developer.nps.gov/api/v1/parks?limit=500&api_key=", api_key)
response <- GET(url)

if (status_code(response) == 200) {
  raw_data <- fromJSON(content(response, "text", encoding = "UTF-8"))$data
  
  # 2. Filter for National Parks and extract Info
  parks_info_df <- raw_data %>%
    filter(
      grepl("National Park", designation, ignore.case = TRUE) |
        grepl("National and State Parks", designation, ignore.case = TRUE) |
        parkCode %in% c("npsa", "seki", "redw")
    ) %>%
    # Remove Roosevelt Campobello (International Park)
    filter(fullName != "Roosevelt Campobello") %>%
    mutate(
      # Extract primary Email
      Email = sapply(contacts$emailAddresses, function(x) if(length(x) > 0) x$emailAddress[1] else NA),
      # Extract primary Phone
      Phone = sapply(contacts$phoneNumbers, function(x) if(length(x) > 0) x$phoneNumber[1] else NA),
      # Extract the "Standard Hours" for Monday as a representative sample
      # (Accessing: operatingHours -> standardHours -> monday)
      Monday_Hours = sapply(operatingHours, function(x) {
        if(length(x) > 0 && !is.null(x$standardHours)) x$standardHours$monday[1] else "Check Website"
      }),
      # Extract City/State from the first address entry
      City = sapply(addresses, function(x) if(length(x) > 0) x$city[1] else NA),
      State = sapply(addresses, function(x) if(length(x) > 0) x$stateCode[1] else NA)
    ) %>%
    select(
      Park_Name = fullName,
      Park_Code = parkCode,
      State,
      City,
      Phone,
      Email,
      Monday_Hours,
      Website_URL = url,
      Description = description
    )
  
  # 3. Save to CSV
  write.csv(parks_info_df, "nps_park_details.csv", row.names = FALSE)
  print(paste("Success! Saved details for", nrow(parks_info_df), "National Parks."))
}

### Getting park cost

# 1. Fetch the main Parks data
print("Fetching Park cost...")
url <- paste0("https://developer.nps.gov/api/v1/feespasses?limit=500&api_key=", api_key)
response <- GET(url)

if (status_code(response) == 200) {
  # Parse the main fees data
  raw_data <- fromJSON(content(response, "text", encoding = "UTF-8"))$data
  
  # Ensure parks_df exists (assuming you fetched general park info elsewhere)
  # parks_df <- fromJSON(content(parks_response, "text", encoding = "UTF-8"), flatten = TRUE)$data %>%
  #   select(parkCode, fullName, designation)
  
  # 2. Filter and Process
  park_cost_df <- raw_data %>%
    left_join(parks_df, by = "parkCode") %>%
    filter(
      grepl("National Park", designation, ignore.case = TRUE) |
        grepl("National and State Parks", designation, ignore.case = TRUE) |
        parkCode %in% c("npsa", "seki", "redw")
    ) %>%
    # No need for hoist()! unnest() already brought these to the top level:
    unnest(cols = c(fees), keep_empty = TRUE) %>% 
    
    # 1. Process the data using the ACTUAL column names we found
    mutate(
      temp_cost = as.numeric(cost),
      # Use entranceFeeType instead of 'name' or 'title'
      Entrance_Fee_Per_Vehicle = ifelse(str_detect(tolower(entranceFeeType), "vehicle"), temp_cost, NA),
      Entrance_Fee_Per_Person = ifelse(str_detect(tolower(entranceFeeType), "person|individual"), temp_cost, NA),
      Entrance_Fee_Per_Motorcycle = ifelse(str_detect(tolower(entranceFeeType), "motorcycle"), temp_cost, NA)
    ) %>%
    
    group_by(fullName, parkCode) %>%
    summarise(
      Entrance_Fee_Per_Vehicle = max(Entrance_Fee_Per_Vehicle, na.rm = TRUE),
      Entrance_Fee_Per_Person = max(Entrance_Fee_Per_Person, na.rm = TRUE),
      Entrance_Fee_Per_Motorcycle = max(Entrance_Fee_Per_Motorcycle, na.rm = TRUE),
      
      # Check for timed entry in the columns we see in your names() list
      Timed_Entry_Required = any(
        str_detect(tolower(as.character(timedEntryDescription)), "reservation|timed entry") | 
          str_detect(tolower(as.character(timedEntryHeading)), "reservation|timed entry"), 
        na.rm = TRUE
      ),
      
      # Use the 'description' column we found
      Cost_Description = paste(unique(na.omit(description)), collapse = " | "),
      .groups = "drop"
    ) %>%
    
    # Clean up -Inf values from max()
    mutate(across(starts_with("Entrance_Fee"), ~ifelse(is.infinite(.), NA, .)))
  
  # Save to CSV
  write.csv(park_cost_df, "park_cost.csv", row.names = FALSE)
  print(paste("Success! Saved details for", nrow(park_cost_df), "National Parks."))
}