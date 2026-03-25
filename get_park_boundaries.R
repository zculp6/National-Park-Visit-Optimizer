# Install packages if you haven't already:
# install.packages(c("httr", "jsonlite", "readr"))

library(httr)
library(jsonlite)
library(readr)

# 1. Configuration
api_key <- "6Ks1tnBrm9d1NqXCd2VzVlylUm8ktGMSbBkljXn4"
csv_file <- "nps_park_details.csv"
base_url <- "https://developer.nps.gov/api/v1/mapdata/parkboundaries/"

# 2. Load Park Codes
# Using read_csv to handle the file and tolower() to ensure API compatibility
park_df <- read_csv(csv_file)
park_codes <- tolower(park_df$Park_Code)

# 3. Prepare to collect data
all_park_boundaries <- list()

message(paste("Starting requests for", length(park_codes), "parks..."))

# 4. Iterative API Requests
for (code in park_codes) {
  full_url <- paste0(base_url, code)
  
  # Make the GET request with the API key in the header
  response <- GET(
    url = full_url,
    add_headers(`X-Api-Key` = api_key)
  )
  
  # Check if the request was successful
  if (status_code(response) == 200) {
    # Parse the JSON response
    boundary_data <- content(response, as = "parsed", type = "application/json")
    all_park_boundaries[[code]] <- boundary_data
    message(paste("Successfully fetched:", code))
  } else {
    message(paste("Failed to fetch", code, "- Status:", status_code(response)))
  }
  
  # Pause for 100ms to stay within rate limits
  Sys.sleep(0.1)
}

# 5. Export results to a JSON file
write_json(all_park_boundaries, "all_park_boundaries.json", auto_unbox = TRUE, pretty = TRUE)

message("Processing complete. Data saved to all_park_boundaries.json")