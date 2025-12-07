# R Script for Spatial Joining Rat Sightings to NYC Census Tracts

# --- 1. Install and Load Required Packages ---
# The 'sf' package is the standard for modern spatial data in R.
# The 'dplyr' package is useful for data manipulation.
if (!requireNamespace("sf", quietly = TRUE)) {
  install.packages("sf")
}
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}

library(sf)
library(dplyr)
library(tibble) # Adding tibble package for explicit conversion

# --- 2. Define File Paths and Parameters ---
# IMPORTANT: Ensure your 'nyct2020.shp' and 'rat_sightings.csv' are in your
# RStudio working directory, or update the paths below.

# File path for the census tracts shapefile
tracts_file <- "nyct2020_25d/nyct2020.shp"
# File path for the large rat sightings CSV
sightings_file <- "Rat_Sightings_20251201.csv"

# Column names for coordinates in the CSV file (as confirmed in the prompt)
lat_col <- "Latitude"
lon_col <- "Longitude"

# CRS (Coordinate Reference System)
# NYC is typically in EPSG 4326 (WGS 84, standard GPS coordinates) or
# a local state plane (like EPSG 2263). We'll assume the raw lat/lon data
# is WGS 84 (EPSG:4326), which is standard for most mapping data.
WGS84_CRS <- 4326

# --- 3. Load and Prepare Data ---

# Load Census Tracts
# The st_read function reads all components of the shapefile (.shp, .shx, .dbf, etc.)
message("Loading Census Tracts...")
nyc_tracts <- st_read(tracts_file)

# Load Rat Sightings CSV
message("Loading Rat Sightings CSV...")
rat_data <- read.csv(sightings_file) %>%
  filter(!is.na(Latitude)) %>%
  filter(!is.na(Longitude))

# Convert Rat Sightings to a Simple Features (sf) object
# We use the Latitude and Longitude columns to create geometry.
message("Converting Rat Sightings to Spatial Data...")
rat_sf <- st_as_sf(
  rat_data,
  coords = c(lon_col, lat_col),
  crs = WGS84_CRS,
  remove = FALSE # Keep the original lat/lon columns
)

# --- 4. Standardize Coordinate Systems (Projection Check) ---
# Ensure both spatial datasets use the same CRS.
# If the CRS of the tracts file is different (e.g., NAD83 / New York Long Island - EPSG:2263),
# we transform the rat points to match the tracts' CRS.

target_crs <- st_crs(nyc_tracts)

if (target_crs != st_crs(rat_sf)) {
  message(paste("Transforming rat sightings to match Tracts CRS:", target_crs$input))
  rat_sf <- st_transform(rat_sf, target_crs)
} else {
  message("CRSs already match.")
}

# --- 5. Perform the Spatial Join (Point-in-Polygon) ---
# This is the core operation that binds each point (rat sighting) to the polygon
# (census tract) it falls within.
# The 'st_join' performs a left join, keeping all rat sightings.

message("Performing Spatial Join...")
rat_sightings_by_tract <- st_join(
  rat_sf,
  nyc_tracts,
  join = st_intersects,
  left = TRUE # Keep all rat sightings, even those outside any tract boundary (e.g., in water)
)

# --- 6. Clean and Export Results ---

# Convert the result back to a standard data frame for easier statistical analysis.
# The 'st_drop_geometry()' function removes the spatial column, leaving the data.
final_data_df <- rat_sightings_by_tract %>%
  st_drop_geometry() %>%
  # Convert to a tibble for smooth dplyr operation
  as_tibble()

# Optional: Print structure to confirm it is a tibble (tbl_df)
message(paste("Structure check before select:", class(final_data_df)[1]))

# Now, perform the selection using explicit namespace call for robustness
final_data_df <- final_data_df %>%
  # *** FIX: Use dplyr::select for explicit method call to resolve 'tbl_df' error ***
  dplyr::select(
    all_of(lat_col),
    all_of(lon_col),
    GEOID, # This is the unique Census Tract Identifier
    everything()
  )

# --- 7. Save the Final Data ---
output_file <- "rat_sightings_with_tract_id.csv"
message(paste("Saving final data to:", output_file))

write.csv(final_data_df, output_file, row.names = FALSE)

message("--- Process Complete ---")
message(paste("Data with Census Tract IDs saved to:", output_file))
message("The GEOID column now contains the unique tract ID for each sighting.")
# You can now proceed to aggregate and run statistical models on the 'final_data_df' object.
# Example: final_data_df %>% group_by(GEOID) %>% summarise(rat_count = n())


