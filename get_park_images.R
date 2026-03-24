library(httr)
library(jsonlite)
library(tidyverse)

# 1. Insert your free NPS API key here
# Get one at: https://www.nps.gov/subjects/developer/api-documentation.htm
api_key <- "6Ks1tnBrm9d1NqXCd2VzVlylUm8ktGMSbBkljXn4"

# 2. Define the API endpoint (limit=500 ensures we grab all park units at once)
url <- paste0("https://developer.nps.gov/api/v1/parks?limit=500&api_key=", api_key)

print("Fetching data from the NPS API...")
response <- GET(url)

# 3. Check if the request was successful
if (status_code(response) == 200) {
  
  # Parse the JSON response into an R list/dataframe
  json_data <- fromJSON(content(response, "text", encoding = "UTF-8"))
  parks_df <- json_data$data
  
  # 4. Filter, format, and unnest the data
  np_images_df <- parks_df %>%
    # Keep only sites designated as a "National Park"
    filter(grepl("National Park", designation, ignore.case = TRUE)) %>%
    # Select just the columns we care about
    select(fullName, parkCode, images) %>%
    # Unnest the embedded list of image dataframes so each image gets its own row
    unnest(images, names_sep = "_") %>% 
    # Rename columns to be clean and readable
    select(
      Park_Name = fullName,
      Park_Code = parkCode,
      Image_URL = images_url,
      Image_Title = images_title,
      Image_Credit = images_credit,
      Image_Alt_Text = images_altText
    )
  
  # 5. Export to CSV
  file_name <- "national_parks_images.csv"
  write.csv(np_images_df, file_name, row.names = FALSE)
  
  print(paste("Success! Found", nrow(np_images_df), "image links."))
  print(paste("Data saved to your working directory as:", file_name))
  
} else {
  print(paste("Error fetching data. HTTP Status code:", status_code(response)))
}