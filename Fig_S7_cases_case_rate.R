# Figure S7 - linear and non-linear marginal effects, wards only, cases and case rates
# Replicates Fig 2 in main manuscript but cases/case rates

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

# Select cases, case rates 
all_results <- subset(all_results, Experience_variable == "Smallpox_cases" |Experience_variable == "Smallpox_case_rate"  )

# Use factor levels and labels to set up experience variable ordering and display labels
Exp_var <- unique(all_results$Experience_variable)
all_results$Experience_variable <- factor(all_results$Experience_variable, 
                                          levels = Exp_var,
                                          labels = gsub(pattern = "_", replacement = " ", x = Exp_var))
all_results$Model <- factor(all_results$Model, levels = c("OLS","fracreg"),
                            labels = c("Linear","Non-linear"))
all_results$Count_rate <- factor(all_results$Count_rate, levels = c("Count","Rate"),
                                 labels = c("Smallpox cases","Smallpox case rate"))

# Construct Figure
fig_2_cases <- ggplot(filter(all_results, Time_varying == T)) + 
  geom_hline(yintercept = 0, linetype = 2, colour = "grey70") +
  geom_line(aes(x = Year, y = Marginal_effect, colour = Model)) + 
  geom_pointrange(aes(x = Year, y = Marginal_effect, 
                      ymin = LCI_marginal_effect, ymax = UCI_marginal_effect,
                      colour = Model), 
                  size = 0.3,
                  position = position_dodge(width = 0.3)) + 
  scale_x_continuous(name = "", breaks = yrs_text) + 
  scale_y_continuous(name = "Marginal effect on vaccine refusal rate for one unit change") +
  scale_colour_manual(name = NULL, values = c("Linear" = "#3E8DC5", "Non-linear" = "#AA2734")) + 
  facet_wrap( ~  Count_rate, scales = "free_y") + 
  theme_classic() + 
  theme(strip.background = element_blank(), 
        axis.title.y = element_text(size = 8) ,
        strip.text = element_text(face = "bold"),
        legend.position = "bottom",
        legend.box.margin = unit(c(0,0,0,0), "cm")) # top, right, bottom, left
fig_2_cases

ggsave(filename = paste0("fig_SM_cases_case_rate.pdf"), plot = fig_2_cases, path = "figs", width = 14, height = 10, units = "cm")
ggsave(filename = paste0("fig_SM_cases_case_rate.png"), plot = fig_2_cases, path = "figs", width = 14, height = 10, units = "cm")

