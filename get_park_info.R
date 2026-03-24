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