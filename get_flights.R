# Load necessary libraries
library(dplyr)
library(readr)

# 1. URLs
airports_url <- "https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat"
routes_url   <- "https://raw.githubusercontent.com/jpatokal/openflights/master/data/routes.dat"
airlines_url <- "https://raw.githubusercontent.com/jpatokal/openflights/master/data/airlines.dat"

# 2. Load the data
airports <- read_csv(airports_url, col_names = c("id", "name", "city", "country", "iata", "icao", "lat", "long", "alt", "tz", "dst", "tz_type", "source", "type"), show_col_types = FALSE)
routes   <- read_csv(routes_url, col_names = c("airline", "airline_id", "source_airport", "source_id", "dest_airport", "dest_id", "codeshare", "stops", "equipment"), show_col_types = FALSE)
airlines <- read_csv(airlines_url, col_names = c("airline_id", "name", "alias", "iata", "icao", "callsign", "country", "active"), show_col_types = FALSE)

# 3. Filter and convert ID to character for the join
target_territories <- c("United States", "American Samoa", "Virgin Islands")

us_related_airports <- airports %>%
  filter(country %in% target_territories) %>%
  mutate(id = as.character(id)) %>%  # Convert to character here
  select(id, iata, country)

# 4. Filter routes and ensure IDs match types
filtered_flights <- routes %>%
  mutate(
    source_id = as.character(source_id),
    dest_id = as.character(dest_id),
    airline_id = as.character(airline_id)
  ) %>%
  inner_join(us_related_airports, by = c("source_id" = "id")) %>%
  inner_join(us_related_airports, by = c("dest_id" = "id"), suffix = c("_dept", "_arr")) %>%
  left_join(airlines %>% mutate(airline_id = as.character(airline_id)) %>% select(airline_id, name), by = "airline_id") %>%
  select(
    airport_depart = source_airport,
    airport_arrive = dest_airport,
    airline = name,
    num_layovers = stops
  )

# 5. Remove duplicates
unique_flights <- filtered_flights %>%
  distinct(airport_depart, airport_arrive, .keep_all = TRUE)

# 6. Export
write_csv(unique_flights, "us_territory_flights.csv")

print(head(unique_flights))