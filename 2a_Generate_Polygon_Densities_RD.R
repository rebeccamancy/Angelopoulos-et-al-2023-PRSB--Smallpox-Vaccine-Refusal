
# This script uses the Glasgow GeoTIFF and the ward and registration district (RD) shapefiles
# to generate a measure of pixel density in each polygon in the intersection between
# wards and RDs.

rm(list = ls())
getwd()

library(sf) # easy reading in of shapefiles for plotting
library(tidyverse) # for dplyr pipes and SQL-like joins
library(raster)
library(ggplot2)

# Read in shp files for Wards and registration districts (RDs)
wards <- sf::st_read("shp/Wards_Shp/Wards_1912.shp"); dim(wards); nrow(wards)
RD <- sf::st_read("shp/Registration_Districts_Shp/Registration_Districts.shp")
RD %>% as_tibble() %>% print(n = nrow(wards))
# Rename some data columns for clarity
RD$RD_Name <- RD$Name; RD$Name <- NULL
RD$RD_ID <- RD$id
wards$Ward_ID <- wards$id

# Plot to check that RD are labelled correctly, etc.
ggplot(RD) + geom_sf() + ggtitle("Registration district names") +
  geom_sf_label(aes(label = paste0(RD_Name)))

# plot wards for comparison
ggplot(wards) + geom_sf() + ggtitle("Ward names") +
  geom_sf_label(aes(label = paste0(Ward_Name)))

# Plot RD overlaid on wards to check alignment
ggplot() + 
  geom_sf(data = wards, fill = NA, colour = "orange", linetype = 2, alpha = 0.5) + 
  geom_sf(data = RD, fill = NA, colour = "blue", linetype = 4, alpha = 0.5) + 
  ggtitle("Overlaid RD and wards") 

# --------- Compute polygons that make up RDs and wards -------------

# Intersect (note that sf is intelligent with attribute data)
pi <- st_intersection(RD, wards)
# Plot to check - wards in black, RD in grey; intersection in red
ggplot() + geom_sf(data = wards, colour = "black", fill = NA, linetype = 3) + 
  geom_sf(data=RD, colour = "red", fill = NA, linetype = 2) + theme_classic()
ggplot(pi) + geom_sf(colour = "red", fill = NA)
# Rename data object
Joint_RD_Wards <- pi

# Add information area of each polygon to Joint_RD_Wards 
Joint_RD_Wards <- Joint_RD_Wards %>% 
  mutate(area = st_area(.) %>% as.numeric())

# Check threshold for considering areas as negligible, assuming anything < 1e4 is fine
#    (these are all just overlaps/lines), and filter
ggplot(subset(Joint_RD_Wards, area < 1e4)) + geom_sf(aes(fill = area))
ggplot(subset(Joint_RD_Wards, area > 1e4)) + geom_sf(aes(fill = area))
Joint_RD_Wards <- Joint_RD_Wards %>% filter (area > 1e4)
dim(Joint_RD_Wards)

# Add in columns for polygon number and name
Joint_RD_Wards <- mutate(Joint_RD_Wards, Poly_Num = 1:65)
Joint_RD_Wards$Poly_Name <- paste("Poly", Joint_RD_Wards$Poly_Num,sep = "_")
# View polygons
ggplot(Joint_RD_Wards) + geom_sf() + ggtitle("Polygons") +
  geom_sf_label(aes(label = paste0(Poly_Num)))


# --------- Add population density information -------------

# Load up raster of Glasgow, covering area of interest
glasgow_raster <- raster("tif/processed_tif/glasgow_raster.tif")
plot(glasgow_raster)

# Calculate population density for each polygon (slow to run)
Joint_RD_Wards$Poly_Dens = apply(Joint_RD_Wards, 1, function(x){ #This loops through the sf dataframe
  cat(paste0("\r Polygon: ", x[["Poly_Num"]], " of ",nrow(Joint_RD_Wards)))
  raster::extract(glasgow_raster,
                  Joint_RD_Wards %>% filter(Poly_Name==x[["Poly_Name"]])) %>%
    # This takes the output of extract() which is a "list" object and turns it into what is in the list, in this case a vector
    unlist() %>%
    # Sum of dots
    sum()
})
#warnings()
# Save Joint_RD_Wards (which is an sf object)
plot(Joint_RD_Wards)
saveRDS(object = Joint_RD_Wards, file = "Output_data/Joint_RD_Wards_Densities_Full_City_extraPoliesV3.rda")

