# This script makes Figure S1, with maps of smallpox cases, deaths, etc.

# Set working directory to Project Directory before attempting to run this code
getwd()
rm(list = ls())

library(readxl) # For reading in data from Excel
library(dplyr) # for left_join
library(writexl)
library(ggplot2) 
library(cowplot)
library(sf) # For maps
library(rgeos) # for readWKT
library(rgdal) # to read in using OGR for projection info

figure.path <- "figs"

# ---- Select Load up dataset ----
COV <- read_excel(path = paste0("Output_data/COV_Main_Dataset.xlsx"), 
                         sheet = "Ward_All")
dim(COV)
str(COV)

# ------------------------- Plot maps ----------------------------
wards <- sf::st_read("shp/Wards_Shp/Wards_1912.shp")
rivers <- sf::st_read("shp/Rivers_Shp/Rivers.shp")
subway <- sf::st_read("shp/Subway_Shp/Subway.shp")
SDs <- sf::st_read("shp/SD_Shp/Sanitary_Districts.shp")
RD <- sf::st_read("shp/Registration_Districts_Shp/Registration_Districts.shp")
RD_bbox <- st_bbox(RD)

# ------------------------ Figure S1 (SDs, Wards, RDs) -------------------------
fill_colour <- "gold3"
boundary_colour <- "lightgrey"

## ----------------- SDs ----------------- ##

# Map of SDs
g_SDs <- ggplot(data=SDs) + 
  geom_sf(fill = fill_colour, colour = boundary_colour, size = 0.2) +
  geom_sf(data=subway, colour = "black", linetype = 6, size = 0.6) + 
  geom_sf(data=rivers, fill = "dodgerblue3", size = 0.2, colour="dodgerblue3") + 
  ggtitle("Sanitary districts") +
  scale_x_continuous(limits = c(RD_bbox["xmin"], RD_bbox["xmax"])) + 
  scale_y_continuous(limits = c(RD_bbox["ymin"], RD_bbox["ymax"])) + 
  theme_light() +
  theme(plot.margin = unit(c(0, 0, 0, 0.2), "cm")) # top, right, bottom, left
g_SDs

## ----------------- Wards ----------------- ##

wards$Num <- as.numeric(wards$Ward_Num)
wards_plus <- left_join(x=wards["Num"], y=COV, by=c("Num"="Municipal_Ward_Number"))

# Map of wards
wards_plus <- left_join(x=wards["Num"], y=COV, by=c("Num"="Municipal_Ward_Number"))
# labels
ggplot(wards_plus) + geom_sf() + ggtitle("wards") +
  geom_sf_label(aes(label = Num))

# Exclude wards after Maryhill (25)
wards_plus$in_analysis <- ifelse(wards_plus$Num <= 25, "Y", "N") # Exclude wards not included in the dataset 
g_wards <- ggplot(data=wards_plus) + 
  geom_sf(aes(fill = in_analysis), colour = boundary_colour, size = 0.2) +
  geom_sf(data=subway, colour = "black", linetype = 6, size = 0.6) + 
  geom_sf(data=rivers, fill = "dodgerblue3", size = 0.2, colour="dodgerblue3") + 
  scale_fill_manual(values = c("Y" = fill_colour, "N" = "white")) +
  ggtitle("Municipal wards") +
  scale_x_continuous(limits = c(RD_bbox["xmin"], RD_bbox["xmax"])) + 
  scale_y_continuous(limits = c(RD_bbox["ymin"], RD_bbox["ymax"])) + 
  theme_light() +
  theme(legend.position = "none") +
  theme(plot.margin = unit(c(0, 0, 0, 0.2), "cm")) # top, right, bottom, left
g_wards


## ----------------- Registration Districts ----------------- ##

# # Registration districts
RD$RD_ID <- RD$id
ggplot(RD) + geom_sf() + ggtitle("RD") +
  geom_sf_label(aes(label = RD_ID))

g_RD <- ggplot(data=RD) + 
  geom_sf(fill = fill_colour, colour = boundary_colour, size = 0.2) +
  geom_sf(data=subway, colour = "black", linetype = 6, size = 0.6) + 
  geom_sf(data=rivers, fill = "dodgerblue3", size = 0.2, colour="dodgerblue3") + 
  ggtitle("Registration districts") +
  scale_x_continuous(limits = c(RD_bbox["xmin"], RD_bbox["xmax"])) + 
  scale_y_continuous(limits = c(RD_bbox["ymin"], RD_bbox["ymax"])) + 
  theme_light() +
  theme(plot.margin = unit(c(0, 0, 0, 0.2), "cm"))  # top, right, bottom, left
g_RD

fig_S1 <- plot_grid(g_SDs, g_wards, g_RD,  labels = c('A','B','C'), label_size = 12, ncol = 3)
fig_S1 

ggsave(plot = fig_S1, filename = "fig_S1.pdf", path = figure.path, width = 20, height = 7, units = "cm")
ggsave(plot = fig_S1, filename = "fig_S1.png", path = figure.path, width = 20, height = 7, units = "cm")



# ------------------ Figure S2 -------------------


# Smallpox cases
g_cases <- ggplot(data=wards_plus) + 
  geom_sf(aes(fill = Smallpox_cases), colour = "lightgrey", size = 0.2) +
  geom_sf(data=subway, colour = "black", linetype = 6, size = 0.6) + 
  geom_sf(data=rivers, fill = "dodgerblue3", size = 0.2, colour="dodgerblue3") + 
  scale_fill_gradient("Cases      ",low = "gold", high = "red3", na.value="white") +
  theme_light(); g_cases

# Smallpox case rate
g_case_rate <- ggplot(data=wards_plus) + 
  geom_sf(aes(fill = Smallpox_case_rate), colour = "lightgrey", size = 0.2) +
  geom_sf(data=subway, colour = "black", linetype = 6, size = 0.6) + 
  geom_sf(data=rivers, fill = "dodgerblue3", size = 0.2, colour="dodgerblue3") + 
  scale_fill_gradient("Case rate  ",low = "gold", high = "red3", na.value="white") +
  theme_light(); g_case_rate

# Smallpox deaths
g_deaths <- ggplot(data=wards_plus) + 
  geom_sf(aes(fill = Smallpox_deaths), colour = "lightgrey", size = 0.2) +
  geom_sf(data=subway, colour = "black", linetype = 6, size = 0.6) + 
  geom_sf(data=rivers, fill = "dodgerblue3", size = 0.2, colour="dodgerblue3") + 
  scale_fill_gradient("Deaths      ",low = "gold", high = "red3", na.value="white") +
  theme_light(); g_deaths

# Smallpox death rate
g_death_rate <- ggplot(data=wards_plus) + 
  geom_sf(aes(fill = Smallpox_death_rate), colour = "lightgrey", size = 0.2) +
  geom_sf(data=subway, colour = "black", linetype = 6, size = 0.6) + 
  geom_sf(data=rivers, fill = "dodgerblue3", size = 0.2, colour="dodgerblue3") + 
  scale_fill_gradient("Death rate",low = "gold", high = "red3", na.value="white") +
  theme_light(); g_death_rate

fig_S2 <- plot_grid(g_cases, g_case_rate, g_deaths, g_death_rate, labels = c('A','B','C','D'), label_size = 12, ncol = 2)
fig_S2 

ggsave(plot = fig_S2, filename = "fig_S2.pdf", path = figure.path, width = 20, height = 12, units = "cm")
ggsave(plot = fig_S2, filename = "fig_S2.png", path = figure.path, width = 20, height = 12, units = "cm")
