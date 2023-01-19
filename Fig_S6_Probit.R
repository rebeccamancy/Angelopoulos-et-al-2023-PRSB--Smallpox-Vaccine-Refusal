# Creating Fig S6 for fracreg probit in line with Fig 2 in main manuscript
# Compare probit with logit (both fracreg). Wards only.

rm(list = ls())
library(readxl) # For reading in data from Excel
library(ggplot2)

#  Proceedings journals Dark red #662B3D, turquoise blue #3E8DC5, main red of Proceedings journals #AA2734

yrs <- 1:7
yrs_text <- yrs + 1906

fracreg_df <- read_excel(path = "Output_data/Results_Wards_fracreg_Baseline.xlsx")
probit_df <- read_excel(path = "Output_data/Results_Ward_fracreg_probit.xlsx")

probit_df$Model <- "Probit"

all_results <- rbind(probit_df, fracreg_df)

# Select deaths and death rate
all_results <- subset(all_results, Experience_variable == "Smallpox_deaths" | Experience_variable == "Smallpox_death_rate")
all_results <- subset(all_results, Time_varying == T)

# Use factor levels and labels to set up experience variable ordering and display labels
Exp_var <- unique(all_results$Experience_variable)
all_results$Experience_variable <- factor(all_results$Experience_variable, 
                                          levels = Exp_var,
                                          labels = gsub(pattern = "_", replacement = " ", x = Exp_var))
all_results$Model <- factor(all_results$Model, levels = c("Probit","fracreg"),
                            labels = c("Probit","Logit"))

# Construct Fig probit
fig_probit <- ggplot(all_results) + 
  geom_hline(yintercept = 0, linetype = 2, colour = "grey70") +
  geom_line(aes(x = Year, y = Marginal_effect, colour = Model)) + 
  geom_pointrange(aes(x = Year, y = Marginal_effect, 
                      ymin = LCI_marginal_effect, ymax = UCI_marginal_effect,
                      colour = Model), 
                  size = 0.3,
                  position = position_dodge(width = 0.3)) + 
  scale_x_continuous(name = "", breaks = yrs_text) + 
  scale_y_continuous(name = "Marginal effect on vaccine refusal rate for one unit change") +
  scale_colour_manual(name = NULL, values = c("Probit" = "#3E8DC5", "Logit" = "#AA2734")) + 
  facet_wrap(~ Experience_variable, scales = "free_y") + 
  theme_classic() + 
  theme(axis.title.y = element_text(size = 10)) +
  theme(strip.background = element_blank(), 
        strip.text = element_text(face = "bold"),)
fig_probit

ggsave(filename = paste0("fig_Probit_wards.pdf"), plot = fig_probit, path = "figs", width = 18, height = 10, units = "cm")
ggsave(filename = paste0("fig_Probit_wards.png"), plot = fig_probit, path = "figs", width = 18, height = 10, units = "cm")

