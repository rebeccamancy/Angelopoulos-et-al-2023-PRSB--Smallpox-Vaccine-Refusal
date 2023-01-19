# Creating Fig 3, which is sd change effect over years for wards, DR only,
# with and without socioeconomic variables
# Second row is static variables, sd change effect

getwd()
rm(list = ls())
library(readxl) # For reading in data from Excel
library(ggplot2)
library(cowplot)
library('ggthemes')
library(dplyr) # for SQL-like joins
library(margins) # for margins
library(stringr) # for the function str_wrap()

yrs <- 1:7
yrs_text <- yrs + 1906

OLS_df <- read_excel(path = "Output_data/Results_Ward_OLS_Baseline.xlsx")
fracreg_df <- read_excel(path = "Output_data/Results_Wards_fracreg_Baseline.xlsx")
OLS_no_controls_df <- read_excel(path = paste0("Output_data/Results_Ward_OLS_No_Controls.xlsx"))
fracreg_no_controls_df <- read_excel(path = paste0("Output_data/Results_Ward_fracreg_No_Controls.xlsx"))

## Create line plot for upper panel Fig 3 ##

all_results <- rbind(OLS_df, OLS_no_controls_df, fracreg_df, fracreg_no_controls_df)

# Use factor levels and labels to set up experience variable ordering and display labels
Exp_var <- unique(all_results$Experience_variable)
all_results$Experience_variable <- factor(all_results$Experience_variable,
                                          levels = Exp_var,
                                          labels = gsub(pattern = "_", replacement = " ", x = Exp_var))

# Remove deaths, cases and case rates (not in main text but in Supplementary Materials) and re-label rates
all_results <- subset(all_results, Experience_variable == "Smallpox death rate")
all_results$Controls <- factor(all_results$With_controls, levels = c("With", "Without"),
                               labels = c("With SC", "Without SC"))
all_results$Model <- factor(all_results$Model, levels = c("OLS","fracreg"),
                            labels = c("Linear","Non-linear"))
all_results <- subset(all_results,Time_varying == T)

# Construct Fig 3a
fig_3a <- ggplot(all_results) + 
  geom_line(aes(x = Year, y = - SD_change_effect, linetype = Controls, colour = Model)) +
  scale_x_continuous(name = NULL, breaks = yrs_text) + 
  scale_y_continuous(name = " ", labels = scales::percent) +
  scale_colour_manual(values = c("Linear" = "#3E8DC5", "Non-linear" = "#AA2734")) + 
  scale_linetype_manual(values = c("With SC" = "solid","Without SC" = "dashed" )) +
  coord_cartesian(ylim = c(0, .35), expand = F) + # Get axes to cross at zero on y-axis
  theme_classic() + 
  theme(plot.margin = unit(c(0.4,0.5,0.5,0.2), "cm"), # top, right, bottom, left
        legend.title = element_blank(),
        legend.position = c(0.6,0.93),
        legend.direction = "horizontal",
        legend.text = element_text(size = 8),
        legend.margin = margin(c(0,0,0,0)),
        legend.box.margin = unit(c(0.0,0,0,0), "cm"),
        legend.spacing.y = unit(0, "mm"),
        legend.background = element_rect(fill = "transparent", colour = "transparent")) 
fig_3a

## Create bar chart for lower panel Fig 3 ###

#  Proceedings journals Dark red #662B3D, turquoise blue #3E8DC5, main red of Proceedings journals #AA2734

all_results_bar <- rbind(OLS_df,  fracreg_df)
static_variables <- c("Average_rooms_1911","Perc_Irish_Born_1901","Population_density_1911")
# Subset for death rate
all_results_bar <- subset(all_results_bar, Experience_variable == "Smallpox_death_rate")

# Use factor levels and labels to set up experience variable ordering and display labels
Exp_var <- unique(all_results_bar$Experience_variable)
all_results_bar$Experience_variable <- factor(all_results_bar$Experience_variable,
                                              levels = Exp_var,
                                              labels = gsub(pattern = "_", replacement = " ", x = Exp_var))
all_results_bar$Model <- factor(all_results_bar$Model, levels = c("OLS","fracreg"),
                                labels = c("Linear","Non-linear"))
# Construct Fig 3b
# Marginal effects on the y-axis
static_variables_results <- filter(all_results_bar, Variable_name %in% static_variables)
static_variables_results$Variable_name <- factor(static_variables_results$Variable_name,
                                                 levels = static_variables,
                                                 labels = c("Population density","Rooms per dwelling","Percent Irish born"))
# SD_change_effect on the y-axis
fig_3b <- ggplot(static_variables_results) + 
  geom_col(aes(x = Variable_name, 
               y = - SD_change_effect,
               fill = Model),
           position = position_dodge(width = 0.5),
           width = 0.4,
           colour = "white") +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey70") +
  scale_y_continuous(name = "",
                     labels = scales::percent_format(accuracy = 5L), limits = c(0,0.4),
                     expand = expansion(mult = c(0, 0))) +
  scale_fill_manual(name = NULL, values = c("Linear" = "#3E8DC5", "Non-linear" = "#AA2734")) + 
  scale_x_discrete("", labels = function(x) str_wrap(x, width = 10)) +
  theme_classic() +
  theme(plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm"), # top, right, bottom, left
        legend.position = c(0.75,0.90),
        legend.direction = "vertical",
        legend.text = element_text(size = 8),
        legend.box.margin = unit(c(0,0,0,0), "cm"),
        legend.background = element_rect(fill = "transparent", colour = "transparent")) 
fig_3b

# Create full-height y-axis label
title <- ggdraw() + 
  draw_label(
    "Percent reduction in vaccine refusal rate for one sd increase in\n (A) death rate and (B) variable shown on x-axis",
    size = 10,
    x = 0,
    vjust = 0,
    angle = 90
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 25)
  )

right_panel <- plot_grid(fig_3a, fig_3b, labels = c('A', 'B'), ncol = 1)
fig_3 <- plot_grid(title, right_panel, ncol = 2, rel_widths = c(0.13,0.87))
fig_3

ggsave(filename = paste0("fig_3_wards_DR_only.pdf"), plot = fig_3, path = "figs", width = 8.5, height = 12, units = "cm")
ggsave(filename = paste0("fig_3_wards_DR_only.png"), plot = fig_3, path = "figs", width = 8.5, height = 12, units = "cm")

# Compute additional statistics relating to the ranges in 1907 and 1910 for main text
all_results %>% filter(Time_varying == T & Year == 1907 & With_controls == "With" & Count_rate == "Count") %>%
  pull(SD_change_effect) %>%
  summary() * -100

all_results %>% filter(Time_varying == T & Year == 1907 & With_controls == "With" & Count_rate == "Rate") %>%
  pull(SD_change_effect) %>%
  summary() * -100

all_results %>% filter(Time_varying == T & Year == 1910 & With_controls == "With" & Count_rate == "Count") %>%
  pull(SD_change_effect) %>%
  summary() * -100 

all_results %>% filter(Time_varying == T & Year == 1910 & With_controls == "With" & Count_rate == "Rate") %>%
  pull(SD_change_effect) %>%
  summary() * -100

