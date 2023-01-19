# This script makes Figure S4, maps of vaccination refusal by year
# Also shows correlations between smallpox DR and COV by ward

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
belvidere_colour <- "black"

legend_position_x <- 0.9
legend_position_y <- 0.26
annotate_hjust <- -0.15
annotate_vjust <- -0.6
legend_key_size <- 0.42

# Map of smallpox death rate
COV_Yr <- subset(COV, Year == 1908)
wards_plus <- left_join(x=wards["Num"], y=COV_Yr, by=c("Num"="Municipal_Ward_Number"))
g_death_rate <- ggplot(data=wards_plus) + 
  geom_sf(aes(fill = Smallpox_death_rate), colour = "lightgrey", size = 0.2) +
  geom_sf(data=subway, colour = "black", linetype = 6, size = 0.6) + 
  geom_sf(data=rivers, fill = "dodgerblue3", size = 0.2, colour="dodgerblue3") + 
  geom_sf(data=belvidere_smallpox, shape = 3, colour = belvidere_colour, size = 2.5) +
  scale_fill_gradient( low = "gold", high = "red3", na.value="white") +
  theme_light() +
  ggtitle("Smallpox death rate") +
  theme(plot.margin = unit(c(0.1, 0, 0.5, 0.2), "cm")) + # top, right, bottom, left
  theme(legend.position = c(legend_position_x, legend_position_y), 
        legend.key.size = unit(legend_key_size, 'cm'),
        legend.title = element_blank())
#g_death_rate

# Map of vaccine refusal rate 1907
COV_1907 <- subset(COV, Year == 1907)
# Add in correlation smallpox DR and COV
my_cor <- sprintf("%0.3f", cor(COV_1907$Smallpox_death_rate, COV_1907$COV_Proportion_Births))
wards_plus <- left_join(x=wards["Num"], y=COV_1907, by=c("Num"="Municipal_Ward_Number"))
#str(wards_plus)

g_cov_1907 <- ggplot(data=wards_plus) + 
  geom_sf(aes(fill = COV_Proportion_Births), colour = "lightgrey", size = 0.2) +
  geom_sf(data=subway, colour = "black", linetype = 6, size = 0.6) + 
  geom_sf(data=rivers, fill = "dodgerblue3", size = 0.2, colour="dodgerblue3") + 
  geom_sf(data=belvidere_smallpox, shape = 3, colour = belvidere_colour, size = 2.5) +
  scale_fill_gradient(low = "gold", high = "darkolivegreen", 
                      limits = c(0,0.4), na.value="white") +
  theme_light() +
  ggtitle("1907") +
  annotate(geom = "text", x=-Inf,y=-Inf, hjust=annotate_hjust, vjust=annotate_vjust, 
           label = paste0("r = ", my_cor)) +
  theme(legend.position = c(legend_position_x, legend_position_y),
        legend.title = element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(),
        legend.key.size = unit(legend_key_size, 'cm'),
        plot.margin = unit(c(0.1, 0, 0.5, 0.2), "cm")) # top, right, bottom, left
#g_cov_1907

COV_1908 <- subset(COV, Year == 1908)
my_cor <- sprintf("%0.3f", cor(COV_1908$Smallpox_death_rate, COV_1908$COV_Proportion_Births))
wards_plus_1908 <- left_join(x=wards["Num"], y=COV_1908, by=c("Num"="Municipal_Ward_Number"))

g_cov_1908 <- ggplot(data=wards_plus_1908) + geom_sf(aes(fill = COV_Proportion_Births), colour = "lightgrey", size = 0.2) +
  geom_sf(data=subway, colour = "black", linetype = 6, size = 0.6) + 
  geom_sf(data=rivers, fill = "dodgerblue3", size = 0.2, colour="dodgerblue3") + 
  geom_sf(data=belvidere_smallpox, shape = 3, colour = belvidere_colour, size = 2.5) +
  scale_fill_gradient("Vaccine \nrefusal rate",low = "gold", high = "darkolivegreen", 
                      limits = c(0,0.4), na.value="white") +
  theme_light()+
  ggtitle("1908") +
  annotate(geom = "text", x=-Inf,y=-Inf, hjust=annotate_hjust, vjust=annotate_vjust, 
           label = paste0("r = ", my_cor)) +
  theme(legend.position = c(legend_position_x, legend_position_y),
        legend.title = element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(),
        legend.key.size = unit(legend_key_size, 'cm'),
        plot.margin = unit(c(0.1, 0, 0.5, 0.2), "cm")) # top, right, bottom, left
#g_cov_1908

COV_1909 <- subset(COV, Year == 1909)
my_cor <- sprintf("%0.3f", cor(COV_1909$Smallpox_death_rate, COV_1909$COV_Proportion_Births))
wards_plus_1909 <- left_join(x=wards["Num"], y=COV_1909, by=c("Num"="Municipal_Ward_Number"))

g_cov_1909 <- ggplot(data=wards_plus_1909) + geom_sf(aes(fill = COV_Proportion_Births), colour = "lightgrey", size = 0.2) +
  geom_sf(data=subway, colour = "black", linetype = 6, size = 0.6) + 
  geom_sf(data=rivers, fill = "dodgerblue3", size = 0.2, colour="dodgerblue3") + 
  geom_sf(data=belvidere_smallpox, shape = 3, colour = belvidere_colour, size = 2.5) +
  scale_fill_gradient("Vaccine \nrefusal rate",low = "gold", high = "darkolivegreen", 
                      limits = c(0,0.4), na.value="white") +
  theme_light()+
  ggtitle("1909") +
  annotate(geom = "text", x=-Inf,y=-Inf, hjust=annotate_hjust, vjust=annotate_vjust, 
           label = paste0("r = ", my_cor)) +
  theme(legend.position = c(legend_position_x, legend_position_y),
        legend.title = element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(),
        legend.key.size = unit(legend_key_size, 'cm'),
        plot.margin = unit(c(0.1, 0, 0.5, 0.2), "cm")) # top, right, bottom, left
#g_cov_1909

COV_1910 <- subset(COV, Year == 1910)
my_cor <- sprintf("%0.3f", cor(COV_1910$Smallpox_death_rate, COV_1910$COV_Proportion_Births))
wards_plus_1910 <- left_join(x=wards["Num"], y=COV_1910, by=c("Num"="Municipal_Ward_Number"))

g_cov_1910 <- ggplot(data=wards_plus_1910) + geom_sf(aes(fill = COV_Proportion_Births), colour = "lightgrey", size = 0.2) +
  geom_sf(data=subway, colour = "black", linetype = 6, size = 0.6) + 
  geom_sf(data=rivers, fill = "dodgerblue3", size = 0.2, colour="dodgerblue3") + 
  geom_sf(data=belvidere_smallpox, shape = 3, colour = belvidere_colour, size = 2.5) +
  scale_fill_gradient("Vaccine \nrefusal rate",low = "gold", high = "darkolivegreen", 
                      limits = c(0,0.4), na.value="white") +
  theme_light()+
  ggtitle("1910") +
  annotate(geom = "text", x=-Inf,y=-Inf, hjust=annotate_hjust, vjust=annotate_vjust, 
           label = paste0("r = ", my_cor)) +
  theme(legend.position = c(legend_position_x, legend_position_y),
        legend.title = element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(),
        legend.key.size = unit(legend_key_size, 'cm'),
        plot.margin = unit(c(0.1, 0, 0.5, 0.2), "cm")) # top, right, bottom, left
#  g_cov_1910

COV_1911 <- subset(COV, Year == 1911)
my_cor <- sprintf("%0.3f", cor(COV_1911$Smallpox_death_rate, COV_1911$COV_Proportion_Births))
wards_plus_1911 <- left_join(x=wards["Num"], y=COV_1911, by=c("Num"="Municipal_Ward_Number"))

g_cov_1911 <- ggplot(data=wards_plus_1911) + geom_sf(aes(fill = COV_Proportion_Births), colour = "lightgrey", size = 0.2) +
  geom_sf(data=subway, colour = "black", linetype = 6, size = 0.6) + 
  geom_sf(data=rivers, fill = "dodgerblue3", size = 0.2, colour="dodgerblue3") + 
  geom_sf(data=belvidere_smallpox, shape = 3, colour = belvidere_colour, size = 2.5) +
  scale_fill_gradient("Vaccine \nrefusal rate",low = "gold", high = "darkolivegreen", 
                      limits = c(0,0.4), na.value="white") +
  theme_light()+
  ggtitle("1911") +
  annotate(geom = "text", x=-Inf,y=-Inf, hjust=annotate_hjust, vjust=annotate_vjust, 
           label = paste0("r = ", my_cor)) +
  theme(legend.position = c(legend_position_x, legend_position_y),
        legend.title = element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(),
        legend.key.size = unit(legend_key_size, 'cm'),
        plot.margin = unit(c(0.1, 0, 0.5, 0.2), "cm")) # top, right, bottom, left
#  g_cov_1911

COV_1912 <- subset(COV, Year == 1912)
my_cor <- sprintf("%0.3f", cor(COV_1912$Smallpox_death_rate, COV_1912$COV_Proportion_Births))
wards_plus_1912 <- left_join(x=wards["Num"], y=COV_1912, by=c("Num"="Municipal_Ward_Number"))

g_cov_1912 <- ggplot(data=wards_plus_1912) + geom_sf(aes(fill = COV_Proportion_Births), colour = "lightgrey", size = 0.2) +
  geom_sf(data=subway, colour = "black", linetype = 6, size = 0.6) + 
  geom_sf(data=rivers, fill = "dodgerblue3", size = 0.2, colour="dodgerblue3") + 
  geom_sf(data=belvidere_smallpox, shape = 3, colour = belvidere_colour, size = 2.5) +
  scale_fill_gradient("Vaccine \nrefusal rate",low = "gold", high = "darkolivegreen", 
                      limits = c(0,0.4), na.value="white") +
  theme_light()+
  ggtitle("1912") +
  annotate(geom = "text", x=-Inf,y=-Inf, hjust=annotate_hjust, vjust=annotate_vjust, 
           label = paste0("r = ", my_cor)) +
  theme(legend.position = c(legend_position_x, legend_position_y),
        legend.title = element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(),
        legend.key.size = unit(legend_key_size, 'cm'),
        plot.margin = unit(c(0.1, 0, 0.5, 0.2), "cm")) # top, right, bottom, left
#  g_cov_1912

COV_1913 <- subset(COV, Year == 1913)
my_cor <- sprintf("%0.3f", cor(COV_1913$Smallpox_death_rate, COV_1913$COV_Proportion_Births))
wards_plus_1913 <- left_join(x=wards["Num"], y=COV_1913, by=c("Num"="Municipal_Ward_Number"))

g_cov_1913 <- ggplot(data=wards_plus_1913) + geom_sf(aes(fill = COV_Proportion_Births), colour = "lightgrey", size = 0.2) +
  geom_sf(data=subway, colour = "black", linetype = 6, size = 0.6) + 
  geom_sf(data=rivers, fill = "dodgerblue3", size = 0.2, colour="dodgerblue3") + 
  geom_sf(data=belvidere_smallpox, shape = 3, colour = belvidere_colour, size = 2.5) +
  scale_fill_gradient("Vaccine \nrefusal rate",low = "gold", high = "darkolivegreen", 
                      limits = c(0,0.4), na.value="white") +
  theme_light()+
  ggtitle("1913") +
  annotate(geom = "text", x=-Inf,y=-Inf, hjust=annotate_hjust, vjust=annotate_vjust, 
           label = paste0("r = ", my_cor)) +
  theme(legend.position = c(legend_position_x, legend_position_y), axis.title.y=element_blank(),
        legend.key.size = unit(legend_key_size, 'cm'),
        legend.title = element_blank(), axis.title.x=element_blank(),
        plot.margin = unit(c(0.1, 0, 0.5, 0.2), "cm")) # top, right, bottom, left
#  g_cov_1913

first_row <- plot_grid(g_death_rate, g_cov_1907,  label_size = 12, nrow = 1)
second_row <- plot_grid(g_cov_1908, g_cov_1909,  label_size = 12, nrow = 1)
third_row <- plot_grid(g_cov_1910, g_cov_1911,  label_size = 12, nrow = 1)
fourth_row <- plot_grid(g_cov_1912, g_cov_1913,  label_size = 12, nrow = 1)

fig_S4_Vac_refusal <- plot_grid(first_row, second_row, third_row,
                                fourth_row, label_size = 12, nrow = 4)
fig_S4_Vac_refusal

ggsave(filename = paste0("fig_S4_Vac_refusal.png"), plot = fig_S4_Vac_refusal, path = "figs", width = 20, height = 30, units = "cm")
ggsave(filename = paste0("fig_S4_Vac_refusal.pdf"), plot = fig_S4_Vac_refusal, path = "figs", width = 20, height = 30, units = "cm")






# 
# 
# ### FOR REGISTRATION DISTRICTS ###
# # ---- Select Load up dataset ----
# COV <- read_excel(path = paste0("Output_data/COV_from_RDs_rev_Aug22_v4.xlsx"))
# 
# # ------------------------- Plot maps ----------------------------
# RDs <- sf::st_read("shp/Registration_Districts_Shp/Registration_Districts.shp")
# rivers <- sf::st_read("shp/Rivers_Shp/Rivers.shp")
# subway <- sf::st_read("shp/Subway_Shp/Subway.shp")
# 
# #ggplot(data=wards) + geom_sf(aes(fill = Group)) # Need to check that the groups are actually correct!!
# #wards$Num <- as.numeric(wards$Ward_Num)
# 
# # -------------------- Combined figures using cowplot ------------------
# RDs_proj4string <- readOGR(dsn = path.expand("../COV-Files/shp/Registration_Districts_Shp"), layer = "Registration_Districts")
# belvidere_smallpox <- readWKT("POINT(262400 663601)", p4s = proj4string(wards_proj4string)) # in Dalmarnock, British National Grid coordinates, pulled from Digimap directly
# belvidere_smallpox <- st_as_sf(belvidere_smallpox)
# 
# # Time series
# COV$RD <- COV$Registration_district_name
# n_RDs <- length(unique(COV$Registration_district_name))
# COV$linetype <- factor(COV$Reg_district_number %/% 2) # Force the linetypes because too many wards to have one each
# g_time_series <- ggplot(data = COV, aes(x=Year, y=COV_surv_perc)) + 
#   geom_line(aes(group=RD, alpha = Registration_district_name), colour="grey20") +
#   geom_point(aes(shape = Registration_district_name)) +
#   scale_alpha_discrete("RD", guide = guide_legend(direction = "horizontal", title.position = "top")) +
#   scale_shape_manual(name="RD", values = c(1:(length(unique(COV$Registration_district_name)))-1), guide = guide_legend(direction = "horizontal", title.position = "top")) + 
#   scale_y_continuous("Vaccine refusal rate") +
#   theme_classic() + theme(legend.position="bottom"); 
# g_time_series
# 
# # Vaccine refusal rate in 1908
# COV_Yr <- subset(COV, Year == 1908)
# RDs_plus <- left_join(x=RDs["Name"], y=COV_Yr, by=c("Name"="Registration_district_name"))
# g_cov <- ggplot(data=RDs_plus) + geom_sf(aes(fill = COV_surv_perc), colour = "lightgrey", size = 0.2) +
#   geom_sf(data=subway, colour = "black", linetype = 6, size = 0.6) + 
#   geom_sf(data=rivers, fill = "dodgerblue3", size = 0.2, colour="dodgerblue3") + 
#   geom_sf(data=belvidere_smallpox, shape = 3, colour = belvidere_colour, size = 2.5) +
#   scale_fill_gradient("Proportion",low = "gold", high = "darkolivegreen", limits = c(0,0.2), na.value="white") +
#   theme_light(); #g_cov
# g_cov
# 
# # Smallpox cases
# #wards_plus <- left_join(x=wards["Num"], y=COV_Yr, by=c("Num"="Municipal_Ward_Number"))
# g_cases <- ggplot(data=RDs_plus) + geom_sf(aes(fill = Smallpox_cases), colour = "lightgrey", size = 0.2) +
#   geom_sf(data=subway, colour = "black", linetype = 6, size = 0.6) + 
#   geom_sf(data=rivers, fill = "dodgerblue3", size = 0.2, colour="dodgerblue3") + 
#   scale_fill_gradient("Cases      ",low = "gold", high = "red3", na.value="white") +
#   theme_light(); #g_cases
# 
# g_cases
# 
# left_panel <- plot_grid(g_cases, g_cov, labels = c('A', 'B'), label_size = 12, ncol = 1)
# right_panel <- g_time_series
# fig_1 <- plot_grid(left_panel, right_panel, labels = c('', 'C'), label_size = 12, ncol = 2)
# fig_1
# 
# #ggsave(plot = fig_1, filename = "fig_1_wards_", Sys.Date() ,".pdf", path = figure.path, width = 31, height = 20, units = "cm")
# #ggsave(plot = fig_1, filename = "fig_1__wards", Sys.Date() ,".png", path = figure.path, width = 31, height = 20, units = "cm")
# ggsave(filename = paste0("fig_1_RDs_", Sys.Date(), ".png"), plot = fig_1, path = "figs", width = 31, height = 20, units = "cm")
# ggsave(filename = paste0("fig_1_RDs_", Sys.Date(), ".pdf"), plot = fig_1, path = "figs", width = 31, height = 20, units = "cm")
