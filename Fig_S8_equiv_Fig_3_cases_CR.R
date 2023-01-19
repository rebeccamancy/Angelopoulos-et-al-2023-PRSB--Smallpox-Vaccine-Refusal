# Creating top panel of Fig S8 for SM, which is sd change effect over years for wards, cases and CR,
# with and without controls
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

## Creating line plot for upper panel Fig 3 ###

all_results <- rbind(OLS_df, OLS_no_controls_df, fracreg_df, fracreg_no_controls_df)

# Use factor levels and labels to set up experience variable ordering and display labels
Exp_var <- unique(all_results$Experience_variable)
all_results$Experience_variable <- factor(all_results$Experience_variable,
                                          levels = Exp_var,
                                          labels = gsub(pattern = "_", replacement = " ", x = Exp_var))

# Remove deaths and death rates and re-label rates
all_results <- subset(all_results, Deaths_cases == "Cases")
all_results$Controls <- factor(all_results$With_controls, levels = c("With", "Without"),
                               labels = c("With socioeconomic variables", "Without socioeconomic variables"))
all_results$Model <- factor(all_results$Model, levels = c("OLS","fracreg"),
                            labels = c("Linear","Non-linear"))
all_results$Count_rate <- factor(all_results$Count_rate, levels = c("Count","Rate"),
                                 labels = c("Smallpox cases","Smallpox case rate"))

all_results <- subset(all_results,Time_varying == T) # moved this here as wasn't running in ggplot

# Construct Fig 3a
fig_3a <- ggplot(all_results) + 
  geom_line(aes(x = Year, y = - SD_change_effect, alpha = Model,  linetype = Controls,
                colour = Model)) +
  scale_x_continuous(name = NULL, breaks = yrs_text) + 
  scale_y_continuous(name = " ", labels = scales::percent) +
  scale_colour_manual(name = "", values = c("Linear" = "#3E8DC5", "Non-linear" = "#AA2734")) + 
  scale_linetype_manual(name = "", values = c("With socioeconomic variables" = "solid","Without socioeconomic variables" = "dashed" )) +
  scale_alpha_manual(name = "", values = c("With socioeconomic variables" = 0.99,"Without socioeconomic variables" = 0.5)) +
  coord_cartesian(ylim = c(0, .4), expand = F) + # Get axes to cross at zero on y-axis
  facet_grid(~ Count_rate) + 
  theme_classic() + 
  theme(strip.background = element_blank(), 
        strip.text = element_text(face = "bold"),
#        legend.position = c(0.35, 0.85),
        legend.margin=margin(c(0,10,0,5)), # top, right, bottom, left
        plot.margin=grid::unit(c(0,5,0,5), "mm"),
        axis.title.y = element_text(size = 8),
#        plot.margin = unit(c(1,0.5,0.5,0.5), "cm"), # top, right, bottom, left
        panel.spacing = unit(2, "lines"),
        legend.position="bottom")
fig_3a

## Creating bar chart for lower panel Fig 3 ###

#  Proceedings journals Dark red #662B3D, turquoise blue #3E8DC5, main red of Proceedings journals #AA2734

all_results_bar <- rbind(OLS_df,  fracreg_df)
static_variables <- c("Average_rooms_1911","Perc_Irish_Born_1901","Population_density_1911")
# Subset for deaths/death rate
all_results_bar <- subset(all_results_bar, Deaths_cases == "Cases")

# Use factor levels and labels to set up experience variable ordering and display labels
Exp_var <- unique(all_results_bar$Experience_variable)
all_results_bar$Experience_variable <- factor(all_results_bar$Experience_variable,
                                          levels = Exp_var,
                                          labels = gsub(pattern = "_", replacement = " ", x = Exp_var))
all_results_bar$Model <- factor(all_results_bar$Model, levels = c("OLS","fracreg"),
                            labels = c("Linear","Non-linear"))
all_results_bar$Count_rate <- factor(all_results$Count_rate, 
                               levels = c("Count", "Rate"))
# Construct Fig 3b
# Marginal effects on the y-axis
#all_results_bar <- data.frame(all_results_bar)
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
  scale_y_continuous(name = "            Percent reduction in vaccine refusal rate for one sd change",
                     labels = scales::percent, limits = c(0,0.4),
                     expand = expansion(mult = c(0, 0))) +
  scale_fill_manual(name = NULL, values = c("Linear" = "#3E8DC5", "Non-linear" = "#AA2734")) + 
  scale_x_discrete("", labels = function(x) str_wrap(x, width = 10)) +
  facet_wrap(~ Experience_variable) + 
  theme_classic() + theme(axis.line=element_line(),
                          strip.background = element_blank(),
                          strip.text = element_blank(),
                          legend.margin=margin(c(0,5,0,5)), # top, right, bottom, left
                          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                          axis.title.y = element_text(hjust = -0.4), # fiddle to get axis text across both plots
                          panel.spacing = unit(2, "lines"),
                          legend.position="bottom")
fig_3b

fig_3 <- plot_grid(fig_3a, fig_3b,  label_size = 12, nrow = 2)
fig_3

ggsave(filename = paste0("fig_SM_equiv_fig3_cases.pdf"), plot = fig_3, path = "figs", width = 18, height = 20, units = "cm")
ggsave(filename = paste0("fig_SM_equiv_fig3_cases.png"), plot = fig_3, path = "figs", width = 18, height = 20, units = "cm")
