# Install packages if you haven't already:
# install.packages(c("httr", "jsonlite", "readr", "sf", "dplyr"))

library(httr)
library(jsonlite)
library(readr)
library(sf)
library(dplyr)

# 1. Configuration
api_key <- "6Ks1tnBrm9d1NqXCd2VzVlylUm8ktGMSbBkljXn4"
csv_file <- "nps_park_details.csv"
base_url <- "https://developer.nps.gov/api/v1/mapdata/parkboundaries/"

# Path to the shapefile - update this to your folder path
shapefile_folder <- "Administrative_Boundaries_of_National_Park_System_Units"
shapefile_name <- "Administrative_Boundaries_of_National_Park_System_Units.shp"
shapefile_path <- file.path(shapefile_folder, shapefile_name)

# 2. First, let's check what files exist
message("=== CHECKING SHAPEFILE COMPONENTS ===")
message(paste("Looking in folder:", shapefile_folder))

required_extensions <- c(".shp", ".shx", ".dbf", ".prj")
found_files <- list()

for (ext in required_extensions) {
  filename <- file.path(shapefile_folder, paste0("Administrative_Boundaries_of_National_Park_System_Units", ext))
  if (file.exists(filename)) {
    message(paste("✓ Found:", basename(filename)))
    found_files[[ext]] <- filename
  } else {
    message(paste("✗ Missing:", basename(filename)))
  }
}

if (length(found_files) < 4) {
  stop("Error: Missing required shapefile components. Need .shp, .shx, .dbf, and .prj files")
}

# 3. Load Park Codes
park_df <- read_csv(csv_file)
park_codes <- tolower(park_df$Park_Code)

# 4. Prepare to collect data
all_park_boundaries <- list()

message("\n=== READING SHAPEFILE ===")
message(paste("Reading from:", shapefile_path))

# Try to read shapefile
tryCatch({
  boundary_sf <- st_read(shapefile_path, quiet = FALSE)
  message(paste("\n✓ Successfully read shapefile with", nrow(boundary_sf), "features"))
  
  # Ensure coordinates are in lon/lat (WGS84) so downstream map bounds are valid.
  current_crs <- st_crs(boundary_sf)
  if (is.null(current_crs) || is.na(current_crs)) {
    warning("Shapefile CRS is missing; geometry will be used as-is and may not align with lon/lat maps.")
  } else {
    message(paste("Detected CRS:", current_crs$input))
    if (!st_is_longlat(boundary_sf)) {
      message("Transforming shapefile geometry to EPSG:4326 (lon/lat)")
      boundary_sf <- st_transform(boundary_sf, 4326)
    }
  }
  
  # Show column names
  message("\n=== SHAPEFILE COLUMNS ===")
  print(names(boundary_sf))
  
  # Show first few rows of key columns to understand the data
  message("\n=== SAMPLE DATA (first 5 rows) ===")
  if (nrow(boundary_sf) > 0) {
    sample_data <- boundary_sf[1:min(5, nrow(boundary_sf)), ]
    
    # Print key columns (excluding geometry)
    for (col in names(sample_data)) {
      if (col != "geometry") {
        message(paste("\n", col, ":"))
        print(sample_data[[col]])
      }
    }
  }
  
  # Look for Glacier Bay and Katmai
  message("\n=== SEARCHING FOR GLACIER BAY AND KATMAI ===")
  
  # Try different column names that might contain park codes or names
  possible_code_cols <- c("UNIT_CODE", "PARKCODE", "CODE", "UNITCODE")
  possible_name_cols <- c("UNIT_NAME", "PARKNAME", "NAME", "UNITNAME")
  
  code_col <- NULL
  name_col <- NULL
  
  for (col in possible_code_cols) {
    if (col %in% names(boundary_sf)) {
      code_col <- col
      message(paste("Using column", col, "for park codes"))
      break
    }
  }
  
  for (col in possible_name_cols) {
    if (col %in% names(boundary_sf)) {
      name_col <- col
      message(paste("Using column", col, "for park names"))
      break
    }
  }
  
  # Search for GLBA and KATM
  glba_features <- NULL
  katm_features <- NULL
  
  if (!is.null(code_col)) {
    glba_features <- boundary_sf %>% filter(toupper(!!sym(code_col)) == "GLBA")
    katm_features <- boundary_sf %>% filter(toupper(!!sym(code_col)) == "KATM")
    
    message(paste("\nFound", nrow(glba_features), "features for GLBA (Glacier Bay)"))
    message(paste("Found", nrow(katm_features), "features for KATM (Katmai)"))
  }
  
  if ((is.null(glba_features) || nrow(glba_features) == 0) && !is.null(name_col)) {
    glba_features <- boundary_sf %>% filter(grepl("Glacier Bay", !!sym(name_col), ignore.case = TRUE))
    message(paste("Searching by name: Found", nrow(glba_features), "Glacier Bay features"))
  }
  
  if ((is.null(katm_features) || nrow(katm_features) == 0) && !is.null(name_col)) {
    katm_features <- boundary_sf %>% filter(grepl("Katmai", !!sym(name_col), ignore.case = TRUE))
    message(paste("Searching by name: Found", nrow(katm_features), "Katmai features"))
  }
  
  # Show details of found features
  if (!is.null(glba_features) && nrow(glba_features) > 0) {
    message("\n=== GLACIER BAY FEATURES DETAILS ===")
    for (i in 1:nrow(glba_features)) {
      message(paste("\nGlacier Bay Feature", i, ":"))
      for (col in names(glba_features)) {
        if (col != "geometry") {
          val <- glba_features[[col]][i]
          message(paste("  ", col, ":", val))
        }
      }
    }
  }
  
  if (!is.null(katm_features) && nrow(katm_features) > 0) {
    message("\n=== KATMAI FEATURES DETAILS ===")
    for (i in 1:nrow(katm_features)) {
      message(paste("\nKatmai Feature", i, ":"))
      for (col in names(katm_features)) {
        if (col != "geometry") {
          val <- katm_features[[col]][i]
          message(paste("  ", col, ":", val))
        }
      }
    }
  }
  
  # Keep fields compatible with NPS mapdata format so National_parks.R can parse it.
  sf_to_geojson_feature <- function(sf_row, feature_index, park_code, park_display_name) {
    geom <- st_geometry(sf_row)[[1]]
    coords <- st_coordinates(geom)
    geom_type <- st_geometry_type(geom)
    
    if (geom_type == "MULTIPOLYGON") {
      coord_list <- list()
      unique_parts <- unique(coords[, "L2"])
      
      for (part in unique_parts) {
        part_coords <- coords[coords[, "L2"] == part, , drop = FALSE]
        unique_rings <- unique(part_coords[, "L1"])
        
        rings <- list()
        for (ring in unique_rings) {
          ring_coords <- part_coords[part_coords[, "L1"] == ring, c("X", "Y"), drop = FALSE]
          ring_list <- lapply(1:nrow(ring_coords), function(i) {
            c(ring_coords[i, "X"], ring_coords[i, "Y"])
          })
          rings[[length(rings) + 1]] <- ring_list
        }
        coord_list[[length(coord_list) + 1]] <- rings
      }
      
      geom_json <- list(
        type = "MultiPolygon",
        coordinates = coord_list
      )
    } else if (geom_type == "POLYGON") {
      unique_rings <- unique(coords[, "L1"])
      rings <- list()
      
      for (ring in unique_rings) {
        ring_coords <- coords[coords[, "L1"] == ring, c("X", "Y"), drop = FALSE]
        ring_list <- lapply(1:nrow(ring_coords), function(i) {
          c(ring_coords[i, "X"], ring_coords[i, "Y"])
        })
        rings[[length(rings) + 1]] <- ring_list
      }
      
      geom_json <- list(
        type = "Polygon",
        coordinates = rings
      )
    }
    
    # Extract properties and add API-compatible keys used downstream.
    raw_props <- as.list(sf_row)
    raw_props$geometry <- NULL
    unit_name <- as.character(raw_props$UNIT_NAME[[1]])
    if (is.na(unit_name) || !nzchar(unit_name)) unit_name <- park_display_name
    props <- list(
      name = ifelse(nzchar(unit_name), unit_name, park_display_name),
      parkCode = toupper(park_code),
      source = "nps_admin_boundaries_shapefile",
      sourceProperties = raw_props
    )
    
    list(
      type = "Feature",
      id = paste0("shapefile-", tolower(park_code), "-", feature_index),
      geometry = geom_json,
      properties = props
    )
  }
  
  # Convert features to GeoJSON format
  shapefile_parks <- list()
  
  if (!is.null(glba_features) && nrow(glba_features) > 0) {
    message("\n=== CONVERTING GLACIER BAY TO GEOJSON ===")
    glba_geojson_features <- list()
    for (i in 1:nrow(glba_features)) {
      message(paste("Converting GLBA feature", i))
      glba_geojson_features[[i]] <- sf_to_geojson_feature(
        glba_features[i, ],
        i,
        park_code = "GLBA",
        park_display_name = "Glacier Bay"
      )
    }
    shapefile_parks[["glba"]] <- list(
      type = "FeatureCollection",
      features = glba_geojson_features
    )
    message(paste("✓ Converted", length(glba_geojson_features), "Glacier Bay features"))
  }
  
  if (!is.null(katm_features) && nrow(katm_features) > 0) {
    message("\n=== CONVERTING KATMAI TO GEOJSON ===")
    katm_geojson_features <- list()
    for (i in 1:nrow(katm_features)) {
      message(paste("Converting KATM feature", i))
      katm_geojson_features[[i]] <- sf_to_geojson_feature(
        katm_features[i, ],
        i,
        park_code = "KATM",
        park_display_name = "Katmai"
      )
    }
    shapefile_parks[["katm"]] <- list(
      type = "FeatureCollection",
      features = katm_geojson_features
    )
    message(paste("✓ Converted", length(katm_geojson_features), "Katmai features"))
  }
  
  # Now fetch API data for other parks
  message("\n\n=== FETCHING FROM NPS API ===\n")
  
  for (code in park_codes) {
    # Skip if we have shapefile data
    if (code %in% names(shapefile_parks)) {
      message(paste("*** Using SHAPEFILE data for", toupper(code), "***"))
      all_park_boundaries[[code]] <- shapefile_parks[[code]]
      next
    }
    
    full_url <- paste0(base_url, code)
    response <- GET(url = full_url, add_headers(`X-Api-Key` = api_key))
    
    if (status_code(response) == 200) {
      boundary_data <- content(response, as = "parsed", type = "application/json")
      all_park_boundaries[[code]] <- boundary_data
      message(paste("✓ Fetched:", code))
    } else {
      message(paste("✗ Failed:", code, "- Status:", status_code(response)))
    }
    
    Sys.sleep(0.1)
  }
  
  # Export results
  write_json(all_park_boundaries, "all_park_boundaries.json", auto_unbox = TRUE, pretty = TRUE)
  
  message("\n\n=== PROCESSING COMPLETE ===")
  message("Data saved to all_park_boundaries.json")
  message("\nSummary:")
  message(paste("- Total parks processed:", length(all_park_boundaries)))
  message(paste("- Parks from shapefile:", length(shapefile_parks)))
  if ("glba" %in% names(shapefile_parks)) {
    message(paste("  - Glacier Bay features:", length(shapefile_parks$glba$features)))
  }
  if ("katm" %in% names(shapefile_parks)) {
    message(paste("  - Katmai features:", length(shapefile_parks$katm$features)))
  }
  message(paste("- Parks from API:", length(all_park_boundaries) - length(shapefile_parks)))
  
}, error = function(e) {
  message("\n✗ ERROR reading shapefile:")
  message(conditionMessage(e))
  message("\nPlease check that all required files are in the folder:")
  message("  - .shp (shapes)")
  message("  - .shx (index)")
  message("  - .dbf (attributes)")
  message("  - .prj (projection)")
})