
# This script uses the Glasgow GeoTIFF and the sanitary district (SD) and ward shapefiles
# to generate a measure of pixel density in each polygon in the intersection between
# SDs and wards.

rm(list = ls())
getwd() # set to project root

library(sf) # easy reading in of shapefiles for plotting
library(tidyverse) # for dplyr pipes and SQL-like joins
library(raster)
library(ggplot2)

# Read in shp files for Wards and SDs
wards <- sf::st_read("shp/Wards_Shp/Wards_1912.shp"); dim(wards)
SD <- sf::st_read("shp/SD_Shp/Sanitary_Districts.shp"); dim(SD)
SD %>% as_tibble() %>% print(n = 35)
# Rename some data columns for clarity
SD$SD_Num <- SD$Number; SD$Number <- NULL
SD$SD_Name <- SD$Name; SD$Name <- NULL
SD$SD_ID <- SD$id
wards$Ward_ID <- wards$id

# Plot to check that SD are labelled correctly, etc.
ggplot(SD) + geom_sf() + ggtitle("Sanitary district names") +
  geom_sf_label(aes(label = paste0(Num_1912, " ", Name_1912)))

ggplot(wards) + geom_sf() + ggtitle("Ward names") +
  geom_sf_label(aes(label = paste0(Ward_Num, " ", Ward_Name)))

# Plot SD overlaid on wards to check alignment
ggplot() + 
  geom_sf(data = wards, fill = NA, colour = "orange", linetype = 2, alpha = 0.5) + 
  geom_sf(data = SD, fill = NA, colour = "blue", linetype = 4, alpha = 0.5) + 
  ggtitle("Overlaid SD and wards") 
  
# --------- Compute polygons that make up SDs and wards -------------

# Intersect (note that sf is intelligent with attribute data)
pi <- st_intersection(SD, wards)
# Plot to check - wards in black, SD in grey; intersection in red
ggplot() + geom_sf(data = wards, colour = "black", fill = NA, linetype = 3) + 
  geom_sf(data=SD, colour = "grey", fill = NA, linetype = 2)
ggplot(pi) + geom_sf(colour = "red", fill = NA)
# Rename data object
Joint_SD_Wards <- pi
dim(Joint_SD_Wards)

# Add information area of each polygon to Joint_SD_Wards 
Joint_SD_Wards <- Joint_SD_Wards %>% 
  mutate(area = st_area(.) %>% as.numeric())

# Check threshold for considering areas as negligible, assuming anything < 1e4 is fine
#    (these are all just overlaps/lines), and filter
ggplot(subset(Joint_SD_Wards, area < 1.1e4)) + geom_sf(aes(fill = area))
ggplot(subset(Joint_SD_Wards, area > 1.1e4)) + geom_sf(aes(fill = area))
Joint_SD_Wards <- Joint_SD_Wards %>% filter (area > 1.1e4)
dim(Joint_SD_Wards)

# Add in columns for polygon number and name
Joint_SD_Wards <- mutate(Joint_SD_Wards, Poly_Num = 1:70) # Poly_Num needs to run from 1:num polygons
Joint_SD_Wards$Poly_Name <- paste("Poly", Joint_SD_Wards$Poly_Num,sep = "_")
# View polygons
ggplot(Joint_SD_Wards) + geom_sf() + ggtitle("Polygons") +
  geom_sf_label(aes(label = paste0(Poly_Num)))

# --------- Add population density information -------------

# Load up raster of Glasgow, covering area of interest
glasgow_raster <- raster("tif/processed_tif/glasgow_raster.tif") 
# The file glasgow_raster.tif can be reconstructed by following the required steps
#   in 1_Generate_Glasgow_Raster.R
plot(glasgow_raster)

# Calculate population density for each polygon (slow to run)
Joint_SD_Wards$Poly_Dens = apply(Joint_SD_Wards, 1, function(x){ #This loops through the sf dataframe
  cat(paste0("\r Polygon: ", x[["Poly_Num"]], " of ",nrow(Joint_SD_Wards)))
  raster::extract(glasgow_raster,
                  Joint_SD_Wards %>% filter(Poly_Name==x[["Poly_Name"]])) %>%
    # This just takes the output of extract() which is a "list" object and turns it into what is in the list, in this case a vector
    unlist() %>%
    # Sum of dots
    sum()
})

# Joint_SD_Wards contains the intersecting polygons and a Poly_Dens (polygon density)
#   attribute for each. The polygon density is a proxy of building density in this area, 
#   to be used as a weighting.

# Save Joint_SD_Wards (which is an sf object)
plot(Joint_SD_Wards)
Joint_SD_Wards
saveRDS(object = Joint_SD_Wards, file = "Output_data/Joint_SD_Wards_Densities_Full_CityV3.rda")

