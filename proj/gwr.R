library(GWmodel)

# Example: Calculate optimal bandwidth using AICc
bw_aicc <- bw.gwr(formula = dependent_variable ~ explanatory_variable1 + explanatory_variable2,
                  data = your_spatial_data,
                  kernel = "gaussian", # or "bisquare", "exponential", etc.
                  adaptive = FALSE, # or TRUE for adaptive bandwidth
                  longlat = TRUE, # Set to TRUE if using lat/lon coordinates
                  criterion = "AICc")
gwr_model <- gwr.basic(formula = dependent_variable ~ explanatory_variable1 + explanatory_variable2,
                       data = your_spatial_data,
                       bw = bw_aicc, # Use the calculated bandwidth
                       kernel = "gaussian",
                       adaptive = FALSE,
                       longlat = TRUE)

library(readxl)
rats <- read_excel("Rat_Sightings_20251201.xlsx")
str(rats)

# Variables to consider: Location Type ~ Incident Zip

pacman::p_load(tidyverse,   # For data workflow
               sf,          # For managing spatial data class sf
               sp,          # For managing spatial data class sp
               GWmodel,     # For geographically-weighted statistics
               raster,      # For converting spatial grid to raster in final section
               tmap)        # For mapping results

# Create a copy of data in the 'sp' format for use in some functions
rats.sp <- rats %>%
  as('Spatial')

# Create distance matrix from centroids
atl.DM <- gw.dist(dp.locat = coordinates(rats))
