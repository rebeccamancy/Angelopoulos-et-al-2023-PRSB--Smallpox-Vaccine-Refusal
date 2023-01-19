rm(list = ls())
library(raster)
library(sf) # Included to read in shapefiles as simple features (useful for plotting)
library(rgdal) # Includes functionality for alternative way to read in shapefiles

# Note that this script will not run directly from the public repository because
#   the geotiff files are downloaded from Digimap and are therefore not shared.
# To run the code here, see paper for references to geotiffs from Digimap. These
#   need to be added to a folder called "tif"; see filepath information at L28-29.

# Algorithm
# ---------
# Geotiff tiles from Digimap are "manually" combined by summing pixel values, file-by-file.
# Note that there is a slight overlap between tiles and this can generate values > 1 when summing.
#   We only want to include these once so we reset to 1 where this has occurred
# Overlaps arise because Digimap tiles are drawn from maps of Renfrewshire (renf), 
#   Lanarkshire (lana) and Dumbarton (dumb), which do not align exactly.
# Note that some manipulations in this script are slow to run.

# Read in ward and SD shapefiles for visual checking and to set map projection information
wards_sf <- sf::st_read("shp/Wards_Shp/Wards_1912.shp"); dim(wards_sf)
wards <- readOGR("shp/Wards_Shp/Wards_1912.shp")
proj4string(wards)
SD_sf <- sf::st_read("shp/SD_Shp/Sanitary_Districts.shp")
SD <- readOGR("shp/SD_Shp/Sanitary Districts.shp")
(p4s <- proj4string(wards))
proj4string(SD) # Check the projections match between SD and ward shapefiles

# Filepath for geotiffs 
filepath <- "tif/Download_1st+Edition_1869563/cs_10560_1_natgrid_4277101/" # for reading in
filepath_geotiffs <- "tif/processed_tif/" # for output

# Get projection information from one of the map tiles
(my_crs <- crs(raster(paste0(filepath, "renf/renf-ns56nw-1.tif"))))

# Construct cell ns56 (upper half, left of map)
# ns56nw - part in renf, part in lana, part in dumb
renf_ns56nw <- raster::rectify(raster(paste0(filepath, "renf/renf-ns56nw-1.tif")))
lana_ns56nw <- raster::rectify(raster(paste0(filepath, "lana/lana-ns56nw-1.tif")))
dumb_ns56nw <- raster::rectify(raster(paste0(filepath, "dumb/dumb-ns56nw-1.tif")))
ns56nw <- sum(renf_ns56nw, lana_ns56nw, dumb_ns56nw, na.rm=TRUE)
values(ns56nw)[values(ns56nw) > 1] = 1
# ns56ne - some in lana, some in dumb
lana_ns56ne <- raster::rectify(raster(paste0(filepath, "lana/lana-ns56ne-1.tif")))
dumb_ns56ne <- raster::rectify(raster(paste0(filepath, "dumb/dumb-ns56ne-1.tif")))
ns56ne <- sum(lana_ns56ne, dumb_ns56ne, na.rm=TRUE)
values(ns56ne)[values(ns56ne) > 1] = 1
plot(ns56ne)
# ns56sw - some in lana, some in renf
renf_ns56sw <- raster::rectify(raster(paste0(filepath, "renf/renf-ns56sw-1.tif")))
lana_ns56sw <- raster::rectify(raster(paste0(filepath, "lana/lana-ns56sw-1.tif")))
ns56sw <- sum(renf_ns56sw, lana_ns56sw, na.rm=TRUE)
values(ns56sw)[values(ns56sw) > 1] = 1
# ns56se - some in lana, some in renf
renf_ns56se <- raster::rectify(raster(paste0(filepath, "renf/renf-ns56se-1.tif")))
lana_ns56se <- raster::rectify(raster(paste0(filepath, "lana/lana-ns56se-1.tif")))
ns56se <- sum(renf_ns56se, lana_ns56se, na.rm=TRUE)
values(ns56se)[values(ns56se) > 1] = 1
# Compile grid cell ns56
crs(ns56nw) <- my_crs
crs(ns56sw) <- my_crs
crs(ns56sw) <- my_crs
crs(ns56se) <- my_crs
ns56 <- raster::merge(ns56nw, ns56ne, ns56sw, ns56se)
plot(ns56)

# Construct cell ns57 (very top left of map)
# Only one tif from this grid cell, which is the SE cell in lana
ns57 <- raster::rectify(raster(paste0(filepath, "lana/lana-ns57se-1.tif")))
crs(ns57) <- my_crs

# Construct cell ns55 (bottom left of map)
# Only one tif from this grid cell, which is the NE cell in renf
ns55ne <- raster(paste0(filepath, "renf/renf-ns55ne-1.tif"))
ns55 <- raster::rectify(ns55ne)
crs(ns55) <- my_crs

# Construct cell ns66 (all of right-hand side of map)
# ns66nw - part in renf, part in lana
renf_ns66nw <- raster::rectify(raster(paste0(filepath, "renf/renf-ns66nw-1.tif")))
lana_ns66nw <- raster::rectify(raster(paste0(filepath, "lana/lana-ns66nw-1.tif")))
ns66nw <- sum(renf_ns66nw, lana_ns66nw, na.rm=TRUE)
values(ns66nw)[values(ns66nw) > 1] = 1
# ns66sw - part in renf (small area), part in lana
renf_ns66sw <- raster::rectify(raster(paste0(filepath, "renf/renf-ns66sw-1.tif")))
lana_ns66sw <- raster::rectify(raster(paste0(filepath, "lana/lana-ns66sw-1.tif")))
ns66sw <- sum(renf_ns66sw, lana_ns66sw, na.rm=TRUE)
values(ns66sw)[values(ns66sw) > 1] = 1
# ns66se - all in lana se
ns66se <- raster::rectify(raster(paste0(filepath, "lana/lana-ns66se-1.tif")))
# Compile cell ns66
crs(ns66nw) <- my_crs
crs(ns66sw) <- my_crs
crs(ns66se) <- my_crs
ns66 <- raster::merge(ns66nw, ns66sw, ns66se)

# Check projections are all OK
crs(ns56); crs(ns57); crs(ns55); crs(ns66)

# Save off all tiles to filepath
saveRDS(object = ns56, file = paste0(filepath_geotiffs, "Glasgow_ns56.rda"))
saveRDS(object = ns57, file = paste0(filepath_geotiffs, "Glasgow_ns57.rda"))
saveRDS(object = ns55, file = paste0(filepath_geotiffs, "Glasgow_ns55.rda"))
saveRDS(object = ns66, file = paste0(filepath_geotiffs, "Glasgow_ns66.rda"))
# Write out the rasters as tifs to filepath, now that we have merged the different layers
writeRaster(ns56nw, paste0(filepath_geotiffs, 'ns56nw.tif'), overwrite=T)
writeRaster(ns56ne, paste0(filepath_geotiffs, 'ns56ne.tif'), overwrite=T)
writeRaster(ns56sw, paste0(filepath_geotiffs, 'ns56sw.tif'), overwrite=T)
writeRaster(ns56se, paste0(filepath_geotiffs, 'ns56se.tif'), overwrite=T)
writeRaster(ns57, paste0(filepath_geotiffs, 'ns57.tif'), overwrite=T)
writeRaster(ns55, paste0(filepath_geotiffs, 'ns55.tif'), overwrite=T)
writeRaster(ns66nw, paste0(filepath_geotiffs, 'ns66nw.tif'), overwrite=T)
writeRaster(ns66sw, paste0(filepath_geotiffs, 'ns66sw.tif'), overwrite=T)
writeRaster(ns66se, paste0(filepath_geotiffs, 'ns66se.tif'), overwrite=T)

# Compose full Glasgow raster
glasgow_raster <- raster::merge(ns56nw, ns56ne, ns56sw, ns56se, 
                                ns57, ns55,
                                ns66nw, ns66sw, ns66se)
crs(glasgow_raster)
res(glasgow_raster)

# Plot to check
plot(glasgow_raster)
plot(wards_sf$geometry, add = TRUE, axes = TRUE, lwd = 1, lty = 3)
plot(SD_sf$geometry, add = TRUE, axes = TRUE, lwd = 1, lty = 2, border = "red")

# Write out full raster of Glasgow and save as tif
writeRaster(glasgow_raster, paste0(filepath_geotiffs, 'glasgow_raster.tif'), overwrite=T)
saveRDS(object = glasgow_raster, file = paste0(filepath_geotiffs, "glasgow_raster.rda"))

