# R Script for Aggregating Rat Sightings and Preparing for Spatial Analysis (Moran's I / LISA)

# --- 1. Load Required Packages ---
# 'sf' for spatial data, 'dplyr' and 'tidyr' for data manipulation, 'units' for area calculation.
if (!requireNamespace("sf", quietly = TRUE)) { install.packages("sf") }
if (!requireNamespace("dplyr", quietly = TRUE)) { install.packages("dplyr") }
if (!requireNamespace("tidyr", quietly = TRUE)) { install.packages("tidyr") }
if (!requireNamespace("units", quietly = TRUE)) { install.packages("units") }
if (!requireNamespace("spdep", quietly = TRUE)) { install.packages("spdep") }
if (!requireNamespace("tmap", quietly = TRUE)) { install.packages("tmap") } # New package for mapping

library(sf)
library(dplyr)
library(tidyr)
library(units)
library(spdep)
library(tmap) # Load tmap for plotting

# --- 2. Define File Paths ---
# IMPORTANT: This assumes the input data from the previous script exists and is named as below.
tracts_file <- "nyct2020_25d/nyct2020.shp"
sightings_file_input <- "rat_sightings_with_tract_id.csv"
output_file_spatial <- "nyc_rat_density.geojson"
output_file_map <- "nyc_rat_density_choropleth.png" # New output file for the map

# --- 3. Load Data ---
message("Loading Census Tracts...")
# Load the Census Tracts geometry
nyc_tracts <- st_read(tracts_file)

message("Loading Spatially Joined Rat Sightings...")
# Load the output CSV from the previous script
sightings_df <- read.csv(sightings_file_input)

# FIX: Ensure GEOID in the sightings data is explicitly a character string
# Census IDs should always be treated as text to preserve leading zeros.
sightings_df <- sightings_df %>%
  dplyr::mutate(GEOID = as.character(GEOID))

# --- 4. Aggregate Rat Counts per Census Tract ---
message("Aggregating Rat Counts...")
rat_counts <- sightings_df %>%
  # Filter out points that fell outside all tracts (e.g., in water, or outside NYC)
  dplyr::filter(!is.na(GEOID)) %>%
  dplyr::group_by(GEOID) %>%
  dplyr::summarise(Rat_Count = n(), .groups = 'drop')

# --- 5. Join Counts to Spatial Data and Clean ---
message("Joining counts back to spatial data...")

# FIX: Ensure GEOID in the census tracts is also a character before joining
nyc_tracts <- nyc_tracts %>%
  dplyr::mutate(GEOID = as.character(GEOID))

nyc_tracts_with_counts <- nyc_tracts %>%
  dplyr::left_join(rat_counts, by = "GEOID") %>%
  # Replace NA counts with 0 (for tracts where no sightings occurred)
  dplyr::mutate(Rat_Count = tidyr::replace_na(Rat_Count, 0)) %>%
  # CRITICAL FIX: Explicitly ensure the object is still an sf object after the join
  st_as_sf()


# --- 6. Calculate Density (The Rate for Statistical Analysis) ---
# Normalizing by area is a standard density measure. Ideally, normalize by population (if available).
message("Calculating Rat Density per square kilometer...")

# Ensure geometry is valid and calculate area
nyc_tracts_with_counts <- st_make_valid(nyc_tracts_with_counts)
nyc_tracts_with_counts$Area_SqM <- st_area(nyc_tracts_with_counts)

# Convert area to square kilometers for easier interpretation
nyc_tracts_with_counts$Area_SqKM <- units::set_units(nyc_tracts_with_counts$Area_SqM, "km^2")

# Calculate Rat Sighting Density (Sightings per square kilometer)
nyc_tracts_with_counts <- nyc_tracts_with_counts %>%
  dplyr::mutate(Rat_Density = Rat_Count / as.numeric(Area_SqKM))

# --- 7. Prepare Spatial Weights for Moran's I / LISA ---
# Spatial weights define who is a "neighbor." Here we use Queens-Contiguity (shared border).

message("Creating Queen Contiguity Spatial Weights Matrix...")
# Ensure the GeoDataFrame is a simple polygon object for spdep compatibility
tracts_sp <- as(nyc_tracts_with_counts, "Spatial")

# 7a. Create list of neighbors
tracts_nb <- poly2nb(tracts_sp, queen = TRUE)

# 7b. Create list of weights (Row-standardized for comparability)
tracts_lw <- nb2listw(tracts_nb, style = "W", zero.policy = TRUE)

# --- 8. Run Global Moran's I (The Clustering Test) ---
# We test the hypothesis that the Rat_Density is randomly distributed.
message("Running Global Moran's I Test...")
moran_test <- moran.test(nyc_tracts_with_counts$Rat_Density, tracts_lw, zero.policy = TRUE)

print("--- Global Moran's I Results ---")
print(moran_test)

if (moran_test$p.value < 0.05 && moran_test$estimate[1] > 0) {
  message("\nCONCLUSION: Rat sightings are significantly CLUSTERED (Hotspots and Coldspots exist).")
} else if (moran_test$p.value < 0.05 && moran_test$estimate[1] < 0) {
  message("\nCONCLUSION: Rat sightings are significantly DISPERSED (Uniformly Spread Out).")
} else {
  message("\nCONCLUSION: Rat sightings appear to be SPATIALLY RANDOM.")
}

# --- 9. Save the Final Spatial Data for Mapping and Further Analysis ---
message(paste("\nSaving analysis-ready spatial data to:", output_file_spatial))
st_write(nyc_tracts_with_counts, output_file_spatial, delete_layer = TRUE)


# --- 10. Generate Choropleth Map ---
message(paste("\nGenerating Choropleth Map and saving to:", output_file_map))
# Use tmap to create a professional choropleth map
# CRITICAL FIX: Explicitly cast to sf one last time to satisfy tm_shape()
rat_map <- tm_shape(st_as_sf(nyc_tracts_with_counts)) +
  # Fill the polygons based on the Rat_Density variable
  tm_fill(
    col = "Rat_Density",
    title = "Rat Sightings per Sq. Km",
    style = "quantile", # Classify the data into five equal groups for visual contrast
    palette = "Reds", # Use a single-hue color scale
    n = 5,
    legend.hist = TRUE
  ) +
  # Draw the borders of the tracts
  tm_borders(col = "grey", lwd = 0.5) +
  # Add map elements (compass, scale bar, title)
  tm_compass(position = c("right", "top"), type = "4star", size = 2) +
  tm_scale_bar(position = c("left", "bottom"), breaks = c(0, 5, 10)) +
  tm_layout(
    title = "NYC Rat Sighting Density by Census Tract",
    title.position = c("right", "bottom"),
    title.size = 1.2,
    legend.position = c("left", "top"),
    frame = FALSE
  )

rat_map

# Save the map to a file
tmap_save(rat_map, filename = output_file_map, width = 8, height = 8, units = "in", dpi = 300)

message("--- Process Complete ---")
message(paste("The final data file '", output_file_spatial, "' contains the Rat_Count and Rat_Density fields."))
message(paste("The Choropleth Map is saved as '", output_file_map, "'."))
message("It is now ready for Local LISA analysis (Local Moran's I).")

