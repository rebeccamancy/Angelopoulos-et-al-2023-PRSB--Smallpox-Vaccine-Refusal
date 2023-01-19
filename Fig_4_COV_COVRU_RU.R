# Creating Fig 4 for main text, COV, COVRU and RU for registration districts

rm(list = ls())
library(readxl) # For reading in data from Excel
library(ggplot2)
library(dplyr)
library(cowplot)

#  Proceedings journals Dark red #662B3D, turquoise blue #3E8DC5, main red of Proceedings journals #AA2734

yrs <- 1:7
yrs_text <- yrs + 1906

RD_OLS_df <- read_excel(path = "Output_data/Results_RD_OLS_Baseline.xlsx")
RD_fracreg_df <- read_excel(path = "Output_data/Results_RD_fracreg_Baseline.xlsx")
RD_OLS_df_COVRU <- read_excel(path = "Output_data/Results_RD_OLS_COVRU.xlsx")
RD_fracreg_df_COVRU <- read_excel(path = "Output_data/Results_RD_fracreg_COVRU.xlsx")
RD_OLS_df_RU <- read_excel(path = "Output_data/Results_RD_OLS_RU.xlsx")
RD_fracreg_df_RU <- read_excel(path = "Output_data/Results_RD_fracreg_RU.xlsx")

# Add COV_COVRU column
RD_OLS_df$COV_COVRU <- "COV"
RD_fracreg_df$COV_COVRU <- "COV"
RD_OLS_df_COVRU$COV_COVRU <- "COVRU"
RD_fracreg_df_COVRU$COV_COVRU <- "COVRU"
RD_OLS_df_RU$COV_COVRU <- "RU"
RD_fracreg_df_RU$COV_COVRU <- "RU"

# Combine all files
all_results <- rbind(RD_OLS_df, RD_fracreg_df, RD_OLS_df_COVRU, RD_fracreg_df_COVRU,
                     RD_OLS_df_RU, RD_fracreg_df_RU)

# Death rate only
all_results <- subset(all_results, Experience_variable == "Smallpox_death_rate")

# Use factor levels and labels to set up experience variable ordering and display labels
Exp_var <- unique(all_results$Experience_variable)
all_results$Experience_variable <- factor(all_results$Experience_variable, 
                                          levels = Exp_var,
                                          labels = gsub(pattern = "_", replacement = " ", x = Exp_var))
all_results$Model <- factor(all_results$Model, levels = c("OLS","fracreg"),
                            labels = c("Linear","Non-linear"))
# all_results$Count_rate <- factor(all_results$Count_rate, levels = c("Count","Rate"),
#                                  labels = c("Smallpox deaths","Smallpox death rate"))
all_results$COV_COVRU <- factor(all_results$COV_COVRU, levels = c("COV","COVRU","RU"),
                                labels = c("COV","CR","RU"))

# Construct Fig 4
fig_4 <- ggplot(filter(all_results, Time_varying == T)) + 
  geom_hline(yintercept = 0, linetype = 2, colour = "grey70") +
  geom_line(aes(x = Year, y = Marginal_effect, colour = Model)) + 
  geom_pointrange(aes(x = Year, y = Marginal_effect, 
                      ymin = LCI_marginal_effect, ymax = UCI_marginal_effect,
                      colour = Model), 
                  size = 0.3,
                  position = position_dodge(width = 0.3)) + 
  scale_x_continuous(name = "", breaks = yrs_text) + 
  scale_y_continuous(name = "Marginal effect of the smallpox death rate on COV, CR or RU") +
  scale_colour_manual(name = NULL, values = c("Linear" = "#3E8DC5", "Non-linear" = "#AA2734")) + 
  facet_wrap(~COV_COVRU, scales = "free_y", ncol = 1) + 
  theme_classic() + 
  theme(strip.background = element_blank(), 
        strip.text = element_text(face = "bold"),
        legend.position = c(0.31,0.375),
        legend.direction = "horizontal",
        legend.text = element_text(size = 8),
        legend.box.margin = unit(c(0,0,0,0), "cm"),
        legend.background = element_rect(fill = "transparent", colour = "transparent")) 
fig_4

ggsave(filename = paste0("fig_4_RDs_DR_only.pdf"), plot = fig_4, path = "figs", width = 8.5, height = 13, units = "cm")
ggsave(filename = paste0("fig_4_RDs_DR_only.png"), plot = fig_4, path = "figs", width = 8.5, height = 13, units = "cm")

