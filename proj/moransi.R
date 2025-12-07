
library(readr)
rat_sightings_with_tract_id <- read_csv("rat_sightings_with_tract_id.csv")

library(sf)
library(dplyr)
library(tidyr)
library(units)
library(spdep)

# file paths
tracts_file <- "nyct2020_25d/nyct2020.shp"
sightings_file_input <- "rat_sightings_with_tract_id.csv"
output_file_spatial <- "nyc_rat_density.geojson"

# Load the Census Tracts geometry
nyc_tracts <- st_read(tracts_file)

# Load the output CSV from the previous script
sightings_df <- read.csv(sightings_file_input)

# NEW FIX: Ensure GEOID in the sightings data is explicitly a character string
# Census IDs should always be treated as text to preserve leading zeros.
sightings_df <- sightings_df %>%
  dplyr::mutate(GEOID = as.character(GEOID))

# --- 4. Aggregate Rat Counts per Census Tract ---
rat_counts <- sightings_df %>%
  # Filter out points that fell outside all tracts (e.g., in water, or outside NYC)
  dplyr::filter(!is.na(GEOID)) %>%
  dplyr::group_by(GEOID) %>%
  dplyr::summarise(Rat_Count = n(), .groups = 'drop')

# --- 5. Join Counts to Spatial Data and Clean ---
# FIX: Ensure GEOID in the census tracts is also a character before joining
# This addresses the original 'double' vs 'character' error.
nyc_tracts <- nyc_tracts %>%
  dplyr::mutate(GEOID = as.character(GEOID))

nyc_tracts_with_counts <- nyc_tracts %>%
  dplyr::left_join(rat_counts, by = "GEOID") %>%
  dplyr::mutate(Rat_Count = tidyr::replace_na(Rat_Count, 0))

# --- 6. Calculate Density (The Rate for Statistical Analysis) ---
# Normalizing by area is a standard density measure. Ideally, normalize by population (if available).
# Ensure geometry is valid and calculate area
nyc_tracts_with_counts <- st_make_valid(nyc_tracts_with_counts)
nyc_tracts_with_counts$Area_SqM <- st_area(nyc_tracts_with_counts)

# Convert area to square kilometers for easier interpretation
nyc_tracts_with_counts$Area_SqKM <- units::set_units(nyc_tracts_with_counts$Area_SqM, "km^2")

# Calculate Rat Sighting Density (Sightings per square kilometer)
nyc_tracts_with_counts <- nyc_tracts_with_counts %>%
  dplyr::mutate(Rat_Density = Rat_Count / as.numeric(Area_SqKM))

write_csv(nyc_tracts_with_counts, "nyc_tracts_with_counts.csv")

# --- 7. Prepare Spatial Weights for Moran's I / LISA ---
# Spatial weights define who is a "neighbor." Here we use Queens-Contiguity (shared border).

# Ensure the GeoDataFrame is a simple polygon object for spdep compatibility
tracts_sp <- as(nyc_tracts_with_counts, "Spatial")

# 7a. Create list of neighbors
tracts_nb <- poly2nb(tracts_sp, queen = TRUE)

# 7b. Create list of weights (Row-standardized for comparability)
tracts_lw <- nb2listw(tracts_nb, style = "W", zero.policy = TRUE)

# --- 8. Run Global Moran's I (The Clustering Test) ---
# We test the hypothesis that the Rat_Density is randomly distributed.
moran_test <- moran.test(nyc_tracts_with_counts$Rat_Density, tracts_lw, zero.policy = TRUE)

print(moran_test)

if (moran_test$p.value < 0.05 && moran_test$estimate[1] > 0) {
  message("\nCONCLUSION: Rat sightings are significantly CLUSTERED (Hotspots and Coldspots exist).")
} else if (moran_test$p.value < 0.05 && moran_test$estimate[1] < 0) {
  message("\nCONCLUSION: Rat sightings are significantly DISPERSED (Uniformly Spread Out).")
} else {
  message("\nCONCLUSION: Rat sightings appear to be SPATIALLY RANDOM.")
}

st_write(nyc_tracts_with_counts, output_file_spatial, delete_layer = TRUE)
