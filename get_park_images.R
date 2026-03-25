library(httr)
library(jsonlite)
library(tidyverse)

api_key <- "6Ks1tnBrm9d1NqXCd2VzVlylUm8ktGMSbBkljXn4"

normalize_park_name_key <- function(x) {
  x %>%
    str_to_lower() %>%
    str_replace_all("[\u2013\u2014]", "-") %>%
    str_replace_all("\\bst\\.?\\b", "saint") %>%
    str_replace_all("\\bu\\.?\\s*s\\.?\\b", " ") %>%
    str_replace_all("&", " and ") %>%
    str_replace_all("\\b(national\\s+park|national\\s+parks|national\\s+and\\s+state\\s+parks|state\\s+parks|preserve|np|n\\.p\\.)\\b", " ") %>%
    str_replace_all("\\bof\\b", " ") %>%
    str_replace_all("[^a-z0-9]+", " ") %>%
    str_squish()
}

url <- paste0("https://developer.nps.gov/api/v1/parks?limit=500&api_key=", api_key)

message("Fetching data from the NPS API...")
response <- GET(url)

if (status_code(response) == 200) {
  
  json_data <- fromJSON(content(response, "text", encoding = "UTF-8"), simplifyVector = FALSE)
  parks_raw <- json_data$data
  
  parks_tbl <- map_dfr(parks_raw, function(item) {
    tibble(
      fullName = item$fullName %||% NA_character_,
      parkCode = item$parkCode %||% NA_character_,
      designation = item$designation %||% NA_character_,
      images = list(item$images)
    )
  })
  
  np_images_df <- parks_tbl %>%
    filter(
      grepl("National Park", designation, ignore.case = TRUE) |
        grepl("National and State Parks", designation, ignore.case = TRUE) |
        parkCode %in% c("npsa", "seki", "redw")
    ) %>%
    filter(fullName != "Roosevelt Campobello") %>%
    mutate(
      park_name_key = normalize_park_name_key(fullName),
      Park_Code = toupper(parkCode)
    ) %>%
    unnest_longer(images, keep_empty = FALSE) %>%
    mutate(
      Image_URL = map_chr(images, ~ .x$url %||% NA_character_),
      Image_Title = map_chr(images, ~ .x$title %||% NA_character_),
      Image_Credit = map_chr(images, ~ .x$credit %||% NA_character_),
      Image_Alt_Text = map_chr(images, ~ .x$altText %||% NA_character_)
    ) %>%
    transmute(
      Park_Name = fullName,
      park_name_key,
      Park_Code,
      Image_URL,
      Image_Title,
      Image_Credit,
      Image_Alt_Text
    )
  
  file_name <- "national_parks_images.csv"
  write.csv(np_images_df, file_name, row.names = FALSE)
  
  message(sprintf("Success! Found %d image links.", nrow(np_images_df)))
  message(sprintf("Data saved to your working directory as: %s", file_name))
  
} else {
  message(sprintf("Error fetching data. HTTP Status code: %s", status_code(response)))
}