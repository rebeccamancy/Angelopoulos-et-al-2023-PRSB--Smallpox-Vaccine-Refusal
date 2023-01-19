# This script makes Figure 1 (maps and time series) 

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

### FOR WARDS ###
# ---- Select Load up dataset ----
COV <- read_excel(path = paste0("Output_data/COV_Main_Dataset.xlsx"), 
                  sheet = "Ward_All")
#str(COV)

# ------------------------- Plot maps ----------------------------
wards <- sf::st_read("shp/Wards_Shp/Wards_1912.shp")
rivers <- sf::st_read("shp/Rivers_Shp/Rivers.shp")
subway <- sf::st_read("shp/Subway_Shp/Subway.shp")

wards$Num <- as.numeric(wards$Ward_Num)

# -------------------- Combined figures using cowplot ------------------
wards_proj4string <- readOGR(dsn = path.expand("../COV-Files/shp/Wards_Shp"), layer = "Wards_1912")
belvidere_smallpox <- readWKT("POINT(262400 663601)", p4s = proj4string(wards_proj4string)) # in Dalmarnock, British National Grid coordinates, pulled from Digimap directly
belvidere_smallpox <- st_as_sf(belvidere_smallpox)

# Time series
COV$Ward <- COV$Municipal_Ward_Name
n_wards <- length(unique(COV$Municipal_Ward_Name))
COV$linetype <- factor(COV$Municipal_Ward_Number %/% 2) # Force the linetypes because too many wards to have one each
g_time_series <- ggplot(data = COV, aes(x=Year, y=COV_Proportion_Births)) + 
  geom_line(aes(group=Ward, alpha = Municipal_Ward_Name), colour="grey20") +
  geom_point(aes(shape = Municipal_Ward_Name)) +
  scale_alpha_discrete("Ward", guide = guide_legend(direction = "horizontal", title.position = "top")) +
  scale_shape_manual(name="Ward", values = c(1:(length(unique(COV$Municipal_Ward_Name)))-1), 
                     guide = guide_legend(direction = "horizontal", title.position = "top")) + 
  scale_y_continuous("Vaccine refusal rate") +
  scale_x_continuous(breaks = seq(1907, 1913)) + 
  theme_classic() + theme(legend.position="bottom"); 
g_time_series

# Map of average vaccine refusal rate 1907-13
# Calculate averages
COV_averages <- aggregate(x= COV$COV_Proportion_Births ,
                          by= list(COV$Municipal_Ward_Number),
                          FUN=mean)
COV_averages <- rename(COV_averages, "Municipal_Ward_Number" = "Group.1")
wards_plus <- left_join(x=wards["Num"], y=COV_averages, by=c("Num"="Municipal_Ward_Number"))
g_cov <- ggplot(data=wards_plus) + geom_sf(aes(fill = x), colour = "lightgrey", size = 0.2) +
  geom_sf(data=subway, colour = "black", linetype = 6, size = 0.6) + 
  geom_sf(data=rivers, fill = "dodgerblue3", size = 0.2, colour="dodgerblue3") + 
  geom_sf(data=belvidere_smallpox, shape = 3, colour = "black", size = 2.5) +
  scale_fill_gradient("Vaccine \nrefusal rate",low = "gold", high = "darkolivegreen", 
                      limits = c(0,0.255), na.value="white") +
  theme_light()+
  theme(plot.margin = unit(c(0, 0, 0, 0.2), "cm")); # top, right, bottom, left
g_cov

# Map of smallpox death rates
COV_Yr <- subset(COV, Year == 1908)
wards_plus <- left_join(x=wards["Num"], y=COV_Yr, by=c("Num"="Municipal_Ward_Number"))
g_death_rate <- ggplot(data=wards_plus) + geom_sf(aes(fill = Smallpox_death_rate), colour = "lightgrey", size = 0.2) +
  geom_sf(data=subway, colour = "black", linetype = 6, size = 0.6) + 
  geom_sf(data=rivers, fill = "dodgerblue3", size = 0.2, colour="dodgerblue3") + 
  geom_sf(data=belvidere_smallpox, shape = 3, colour = "black", size = 2.5) +
  scale_fill_gradient("Death rate ",low = "gold", high = "red3", na.value="white") +
  theme_light() +
  theme(plot.margin = unit(c(0, 0, 0, 0.2), "cm")); # top, right, bottom, left
g_death_rate

left_panel <- plot_grid(g_death_rate, g_cov, labels = c('A', 'B'), label_size = 12, ncol = 1)
right_panel <- g_time_series
fig_1 <- plot_grid(left_panel, right_panel, labels = c('', 'C'), label_size = 12, ncol = 2)
fig_1

ggsave(filename = paste0("fig_1_wards_death_rate.png"), plot = fig_1, path = "figs", width = 31, height = 21, units = "cm")
ggsave(filename = paste0("fig_1_wards_death_rate.pdf"), plot = fig_1, path = "figs", width = 31, height = 20, units = "cm")
