library(sf)
library(spdep)
library(tmap)
library(readr)
library(tidyverse)

library(readxl)
sightings <- read_excel("Rat_Sightings_20251201.xlsx")

str(sightings)
census <- st_read("nyct2020_25d/nyct2020.shp")

# Is a spatial pattern random or non-random?

sightings_clean <- sightings %>%
  filter(!is.na(Longitude), !is.na(Latitude))

rats_sf <- st_as_sf(
  sightings_clean,
  coords = c("Longitude", "Latitude"),
  crs = 4326
)

rats_sf <- st_transform(rats_sf, 2263)

library(sf)
library(ggplot2)

ggplot() +
  geom_sf(data = census, color = "grey", size = 0.5) +
  geom_sf(data = rats_sf, color = "red", size = 0.5) +
  theme_minimal() +
  labs(title = "NYC Rat Sightings")


nyc <- census %>% st_transform(2263)

#---------------------------------------------------------------------------------------------------------------

install.packages("RANN")

library(RANN)

# read your data
df <- sightings  # adjust your file name
coords <- df[, c("Latitude", "Longitude")]  # or AJ/AK columns

# Convert to numeric (important)
df$lat <- as.numeric(df$Latitude)
df$lon <- as.numeric(df$Longitude)

# Remove rows with NA or non-finite values
df <- df[is.finite(df$Latitude) & is.finite(df$Longitude), ]

coords <- as.matrix(df[, c("Latitude", "Longitude")])

# find nearest neighbor (k = 2 because k=1 is the point itself)
nn <- nn2(coords, k = 2)

# nearest neighbor distance for each point
nearest_dist <- nn$nn.dists[,2]

# add to dataframe if you want
df$nearest_dist <- nearest_dist

# get minimum distance across whole dataset
min(nearest_dist)

#----------------------------------------------------------------------------------------------------------------
library(ggplot2)

df$nearest_dist <- nearest_dist  # make sure it's in your dataframe

ggplot(df, aes(x = Longitude, y = Latitude, color = nearest_dist)) +
  geom_point(alpha = 0.6) +
  scale_color_viridis_c(option = "plasma") +
  theme_minimal() +
  labs(title = "Rat Sightings Colored by Nearest-Neighbor Distance",
       x = "Longitude", y = "Latitude", color = "Distance (m)")



