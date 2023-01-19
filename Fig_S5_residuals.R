# Plots of residuals for lm models, to include in SM. 
# Shows both plots in one figure

rm(list = ls())
library(readxl) # For reading in data from Excel
library(cowplot)
library(ggplot2)

#######################################################################################
#                                 Set up and estimate models using lm
#######################################################################################

yrs <- 1:7
yrs_text <- yrs + 1906
figure.path <- "figs"

# OLS
# Read in data and code to compile model outputs
COV_ward <- read_excel(path = paste0("Output_data/COV_Main_Dataset.xlsx"), 
                       sheet = "Ward_All")

# # NB: Models are estimated using lm here for convenience (the fact that we do not include e.g. 
# robust se, is not important because here we are only interested in the residuals)

# Deaths
deaths_wards <- lm(COV_Proportion_Births ~ Smallpox_deaths * Year_Numeric +
                     Population_density_1911 + 
                     Average_rooms_1911 +
                     Perc_Irish_Born_1901 , 
                   data=COV_ward)

# Death rate
death_rate_wards <- lm(COV_Proportion_Births ~ Smallpox_death_rate * Year_Numeric +
                      Population_density_1911 + 
                      Average_rooms_1911 +
                      Perc_Irish_Born_1901 , 
                    data=COV_ward)

# Plots of residuals
deaths_resid_data_wards <- data.frame(Fitted_values = deaths_wards$fitted.values, Residuals = deaths_wards$residuals)
p_resid_deaths_wards <- ggplot(deaths_resid_data_wards) + geom_point(aes(x=Fitted_values, y=Residuals), shape = 1, alpha = 0.7) +
  geom_smooth(aes(x=Fitted_values, y=Residuals), se = F, colour = "#3E8DC5", size = 0.5) + 
  scale_x_continuous("Fitted values") +
  ggtitle("Smallpox deaths") +
  theme_classic(); p_resid_deaths_wards
p_hist_resid_deaths_wards <- ggplot(deaths_resid_data_wards) + 
  geom_histogram(aes(x=Residuals, y = ..density..), fill = "#3E8DC5", colour = "white", bins = 14) +
  scale_y_continuous(limits = c(0,15)) +
  #ggtitle("Smallpox deaths") +
  stat_function(fun = dnorm, args = list(mean = mean(deaths_resid_data_wards$Residuals), sd = sd(deaths_resid_data_wards$Residuals))) +
  theme_classic();p_hist_resid_deaths_wards

death_rate_resid_data_wards <- data.frame(Fitted_values = death_rate_wards$fitted.values, Residuals = death_rate_wards$residuals)
p_resid_death_rate_wards <- ggplot(death_rate_resid_data_wards) + geom_point(aes(x=Fitted_values, y=Residuals), shape = 1, alpha = 0.7) +
  geom_smooth(aes(x=Fitted_values, y=Residuals), se = F, colour = "#3E8DC5", size = 0.5) + 
  scale_x_continuous("Fitted values") +
  ggtitle("Smallpox death rate") +
  theme_classic(); p_resid_death_rate_wards
p_hist_resid_death_rate_wards <- ggplot(death_rate_resid_data_wards) + 
  geom_histogram(aes(x=Residuals, y = ..density..), fill = "#3E8DC5", colour = "white", bins = 14) +
  scale_y_continuous(limits = c(0,15)) +
  #ggtitle("Smallpox death rate") +
  stat_function(fun = dnorm, args = list(mean = mean(death_rate_resid_data_wards$Residuals), sd = sd(death_rate_resid_data_wards$Residuals))) +
  theme_classic();p_hist_resid_death_rate_wards

# Put figures together into panels
fig_scatter <- plot_grid(p_resid_deaths_wards,  p_resid_death_rate_wards,
                     labels = c('A', 'B'), label_size = 12, ncol = 2)
fig_scatter

fig_hist <- plot_grid(p_hist_resid_deaths_wards,  p_hist_resid_death_rate_wards,
                      labels = c('C', 'D'), label_size = 12, ncol = 2)
fig_hist

# Join both together
fig_all <- plot_grid(fig_scatter,  fig_hist,label_size = 12, nrow = 2)
fig_all

ggsave(plot = fig_all, filename = "fig_resid_joint_wards.pdf", path = figure.path, width = 15, height = 13, units = "cm")
ggsave(plot = fig_all, filename = "fig_resid_joint_wards.png", path = figure.path, width = 15, height = 13, units = "cm")

