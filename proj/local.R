
lisa <- read.csv("nyc_tracts_with_counts.csv")
output_file_lisa_map <- "nyc_rat_lisa_map.png"

# --- 11. Run Local Moran's I (LISA) ---
lisa_results <- localmoran(nyc_tracts_with_counts$Rat_Density, tracts_lw, zero.policy = TRUE)

# Combine LISA results with the spatial data
lisa_df <- as.data.frame(lisa_results)
names(lisa_df) <- c("Local_I", "E_Local_I", "Var_Local_I", "Z_Local_I", "P_Local_I")
nyc_tracts_with_counts <- cbind(nyc_tracts_with_counts, lisa_df)

# 1. Standardize the Rat Density variable (Z-score)
# Suppress warning about R-squared
suppressWarnings({
  nyc_tracts_with_counts$Z_Density <- as.vector(scale(nyc_tracts_with_counts$Rat_Density))
})

# 2. Determine the LISA Quadrant Classification
# The lag variable is the average of neighbors' density
nyc_tracts_with_counts$Lag_Density <- lag.listw(tracts_lw, nyc_tracts_with_counts$Rat_Density)

# Calculate Z-score for the lag variable (neighbors' average)
suppressWarnings({
  nyc_tracts_with_counts$Z_Lag_Density <- as.vector(scale(nyc_tracts_with_counts$Lag_Density))
})


# Create the classification variable, focusing only on significant results (P < 0.05)
nyc_tracts_with_counts <- nyc_tracts_with_counts %>%
  dplyr::mutate(
    LISA_Cluster = case_when(
      P_Local_I >= 0.05 ~ "Insignificant",
      Z_Density >= 0 & Z_Lag_Density >= 0 ~ "High-High (Hotspot)",
      Z_Density < 0 & Z_Lag_Density < 0 ~ "Low-Low (Coldspot)",
      Z_Density >= 0 & Z_Lag_Density < 0 ~ "High-Low (Outlier)",
      Z_Density < 0 & Z_Lag_Density >= 0 ~ "Low-High (Outlier)",
      TRUE ~ "Insignificant"
    ),
    LISA_Cluster = factor(LISA_Cluster, levels = c(
      "High-High (Hotspot)",
      "Low-Low (Coldspot)",
      "High-Low (Outlier)",
      "Low-High (Outlier)",
      "Insignificant"
    ))
  )

# --- 12. Generate LISA Map ---
# Define colors for the clusters (standard colors for LISA maps)
cluster_colors <- c(
  "High-High (Hotspot)" = "#D7191C", # Red
  "Low-Low (Coldspot)" = "#2C7BB6",  # Blue
  "High-Low (Outlier)" = "#FDAE61",  # Orange
  "Low-High (Outlier)" = "#ABDDA4",  # Light Green
  "Insignificant" = "lightgrey"
)

lisa_map <- tm_shape(st_as_sf(nyc_tracts_with_counts)) +
  tm_fill(
    col = "LISA_Cluster",
    palette = cluster_colors,
    title = "Local Autocorrelation",
    legend.hist = FALSE,
    na.value = "white" # tracts with no neighbors will be white
  ) +
  tm_borders(col = "black", lwd = 0.1) +
  tm_layout(
    title = "Hotspots and Coldspots of Rat Density",
    title.position = c("right", "bottom"),
    title.size = 1.2,
    legend.position = c("left", "top"),
    frame = FALSE
  )
lisa_map

# Save the map to a file
tmap_save(lisa_map, filename = output_file_lisa_map, width = 8, height = 8, units = "in", dpi = 300)
