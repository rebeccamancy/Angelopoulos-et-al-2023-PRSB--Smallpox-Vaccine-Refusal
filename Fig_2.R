# Figure 2 - linear and non-linear marginal effects, wards only, death rate only
# Then separately for deaths

rm(list = ls())
library(readxl) # For reading in data from Excel
library(ggplot2)
library(dplyr)
library(cowplot)

#  Proceedings journals Dark red #662B3D, turquoise blue #3E8DC5, main red of Proceedings journals #AA2734

yrs <- 1:7
yrs_text <- yrs + 1906

OLS_df <- read_excel(path = "Output_data/Results_Ward_OLS_Baseline.xlsx")
fracreg_df <- read_excel(path = "Output_data/Results_Wards_fracreg_Baseline.xlsx")

# Combine all files
all_results <- rbind(OLS_df, fracreg_df)

#### Death rates only
all_results <- subset(all_results, Experience_variable == "Smallpox_death_rate"  )

# Use factor levels and labels to set up experience variable ordering and display labels
Exp_var <- unique(all_results$Experience_variable)
all_results$Experience_variable <- factor(all_results$Experience_variable, 
                                          levels = Exp_var,
                                          labels = gsub(pattern = "_", replacement = " ", x = Exp_var))
all_results$Model <- factor(all_results$Model, levels = c("OLS","fracreg"),
                            labels = c("Linear","Non-linear"))
# 
# Construct Fig 2
fig_2 <- ggplot(filter(all_results, Time_varying == T)) + 
  geom_hline(yintercept = 0, linetype = 2, colour = "grey70") +
  geom_line(aes(x = Year, y = Marginal_effect, colour = Model)) + 
  geom_pointrange(aes(x = Year, y = Marginal_effect, 
                      ymin = LCI_marginal_effect, ymax = UCI_marginal_effect,
                      colour = Model), 
                  size = 0.3,
                  position = position_dodge(width = 0.3)) + 
  scale_x_continuous(name = "", breaks = yrs_text) + 
  scale_y_continuous(name = "Marginal effect of the smallpox death rate \non the vaccine refusal rate") +
  scale_colour_manual(name = NULL, values = c("Linear" = "#3E8DC5", "Non-linear" = "#AA2734")) + 
  theme_classic() + 
  theme(strip.background = element_blank(), 
        axis.title.y = element_text(size = 9) ,
        strip.text = element_text(face = "bold"),
        legend.position = c(0.33,0.95),
        legend.direction = "horizontal",
        legend.text = element_text(size = 8),
        legend.background = element_rect(fill = "transparent", colour = "transparent"),
        legend.box.margin = unit(c(0,0,0,0), "cm")) # top, right, bottom, left
fig_2

ggsave(filename = paste0("fig_2_DR_only.pdf"), plot = fig_2, path = "figs", width = 8.5, height = 7, units = "cm")
ggsave(filename = paste0("fig_2_DR_only.png"), plot = fig_2, path = "figs", width = 8.5, height = 7, units = "cm")

#### Deaths only
all_results <- rbind(OLS_df, fracreg_df)
all_results <- subset(all_results, Experience_variable == "Smallpox_deaths"  )

# Use factor levels and labels to set up experience variable ordering and display labels
Exp_var <- unique(all_results$Experience_variable)
all_results$Experience_variable <- factor(all_results$Experience_variable, 
                                          levels = Exp_var,
                                          labels = gsub(pattern = "_", replacement = " ", x = Exp_var))
all_results$Model <- factor(all_results$Model, levels = c("OLS","fracreg"),
                            labels = c("Linear","Non-linear"))
# 
# Construct Fig 2 copy for deaths
fig_2_deaths <- ggplot(filter(all_results, Time_varying == T)) + 
  geom_hline(yintercept = 0, linetype = 2, colour = "grey70") +
  geom_line(aes(x = Year, y = Marginal_effect, colour = Model)) + 
  geom_pointrange(aes(x = Year, y = Marginal_effect, 
                      ymin = LCI_marginal_effect, ymax = UCI_marginal_effect,
                      colour = Model), 
                  size = 0.3,
                  position = position_dodge(width = 0.3)) + 
  scale_x_continuous(name = "", breaks = yrs_text) + 
  scale_y_continuous(name = "Marginal effect of smallpox deaths \non the vaccine refusal rate") +
  scale_colour_manual(name = NULL, values = c("Linear" = "#3E8DC5", "Non-linear" = "#AA2734")) + 
  theme_classic() + 
  theme(strip.background = element_blank(), 
        axis.title.y = element_text(size = 9) ,
        strip.text = element_text(face = "bold"),
        legend.position = c(0.33,0.95),
        legend.direction = "horizontal",
        legend.text = element_text(size = 8),
        legend.background = element_rect(fill = "transparent", colour = "transparent"),
        legend.box.margin = unit(c(0,0,0,0), "cm")) # top, right, bottom, left
fig_2_deaths

ggsave(filename = paste0("fig_2_deaths_only.pdf"), plot = fig_2_deaths, path = "figs", width = 8.5, height = 7, units = "cm")
ggsave(filename = paste0("fig_2_deaths_only.png"), plot = fig_2_deaths, path = "figs", width = 8.5, height = 7, units = "cm")

