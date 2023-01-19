# This script makes Figure S4b, scatter plot of vaccination refusal and year
# Also shows correlations between smallpox DR and COV by ward

# Set working directory to Project Directory before attempting to run this code
getwd()
rm(list = ls())

library(readxl) # For reading in data from Excel
library(dplyr) # for left_join
library(writexl)
library(ggplot2) 
library(cowplot)
library(stargazer)

### FOR WARDS ###
# ---- Select Load up dataset ----
COV <- read_excel(path = paste0("Output_data/COV_Main_Dataset.xlsx"), 
                  sheet = "Ward_All")

# legend_position_x <- 0.9
# legend_position_y <- 0.26
# annotate_hjust <- -0.15
# annotate_vjust <- -0.6
# legend_key_size <- 0.42

#### Create scatter plots of COV by smallpox DR
# All years
COV_by_DR <- ggplot(COV,aes(x=Smallpox_death_rate,y=COV_Proportion_Births, groupName= "Municipal_Ward_Name")) +
  geom_point() +
  #geom_smooth() +
  ylab("Vaccine refusal rate")  +
  xlab("Smallpox death rate during 1900 and 1903 epidemics") +
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.01)) +
  scale_x_continuous(breaks = 1907:1913) +
  theme_classic() +
  theme(plot.title=element_text(size=10,face="bold")) +
  theme(axis.title=element_text(size=8)) 
COV_by_DR

# Scatterplot 1907
COV_1907 <- subset(COV, Year == 1907)

COV_by_DR_1907 <- ggplot(COV_1907,aes(x=Smallpox_death_rate,y=COV_Proportion_Births, groupName= "Municipal_Ward_Name")) +
  geom_point() +
  #geom_smooth() +
  ylab("Vaccine refusal rate")  +
  xlab("Smallpox death rate during 1900 and 1903 epidemics") +
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.01)) +
  scale_x_continuous(breaks = 1907:1913) +
  theme_classic() +
  theme(plot.title=element_text(size=10,face="bold")) +
  theme(axis.title=element_text(size=8)) +
  ggtitle("1907")
COV_by_DR_1907

COV_1908 <- subset(COV, Year == 1908)

COV_by_DR_1908 <- ggplot(COV_1908,aes(x=Smallpox_death_rate,y=COV_Proportion_Births, groupName= "Municipal_Ward_Name")) +
  geom_point() +
  #geom_smooth() +
  ylab("Vaccine refusal rate")  +
  xlab("Smallpox death rate during 1900 and 1903 epidemics") +
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.01)) +
  scale_x_continuous(breaks = 1907:1913) +
  theme_classic() +
  theme(plot.title=element_text(size=10,face="bold")) +
  theme(axis.title=element_text(size=8)) +
  ggtitle("1908")
COV_by_DR_1908

COV_1909 <- subset(COV, Year == 1909)

COV_by_DR_1909 <- ggplot(COV_1909,aes(x=Smallpox_death_rate,y=COV_Proportion_Births, groupName= "Municipal_Ward_Name")) +
  geom_point() +
  #geom_smooth() +
  ylab("Vaccine refusal rate")  +
  xlab("Smallpox death rate during 1900 and 1903 epidemics") +
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.01)) +
  scale_x_continuous(breaks = 1907:1913) +
  theme_classic() +
  theme(plot.title=element_text(size=10,face="bold")) +
  theme(axis.title=element_text(size=8)) +
  ggtitle("1909")
COV_by_DR_1909

COV_1910 <- subset(COV, Year == 1910)

COV_by_DR_1910 <- ggplot(COV_1910,aes(x=Smallpox_death_rate,y=COV_Proportion_Births, groupName= "Municipal_Ward_Name")) +
  geom_point() +
  #geom_smooth() +
  ylab("Vaccine refusal rate")  +
  xlab("Smallpox death rate during 1900 and 1903 epidemics") +
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.01)) +
  scale_x_continuous(breaks = 1907:1913) +
  theme_classic() +
  theme(plot.title=element_text(size=10,face="bold")) +
  theme(axis.title=element_text(size=8)) +
  ggtitle("1910")
COV_by_DR_1910

COV_1911 <- subset(COV, Year == 1911)

COV_by_DR_1911 <- ggplot(COV_1911,aes(x=Smallpox_death_rate,y=COV_Proportion_Births, groupName= "Municipal_Ward_Name")) +
  geom_point() +
  #geom_smooth() +
  ylab("Vaccine refusal rate")  +
  xlab("Smallpox death rate during 1900 and 1903 epidemics") +
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.01)) +
  scale_x_continuous(breaks = 1907:1913) +
  theme_classic() +
  theme(plot.title=element_text(size=10,face="bold")) +
  theme(axis.title=element_text(size=8)) +
  ggtitle("1911")
COV_by_DR_1911

COV_1912 <- subset(COV, Year == 1912)

COV_by_DR_1912 <- ggplot(COV_1912,aes(x=Smallpox_death_rate,y=COV_Proportion_Births, groupName= "Municipal_Ward_Name")) +
  geom_point() +
  #geom_smooth() +
  ylab("Vaccine refusal rate")  +
  xlab("Smallpox death rate during 1900 and 1903 epidemics") +
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.01)) +
  scale_x_continuous(breaks = 1907:1913) +
  theme_classic() +
  theme(plot.title=element_text(size=10,face="bold")) +
  theme(axis.title=element_text(size=8)) +
  ggtitle("1912")
COV_by_DR_1912

COV_1913 <- subset(COV, Year == 1913)

COV_by_DR_1913 <- ggplot(COV_1913,aes(x=Smallpox_death_rate,y=COV_Proportion_Births, groupName= "Municipal_Ward_Name")) +
  geom_point() +
  #geom_smooth() +
  ylab("Vaccine refusal rate")  +
  xlab("Smallpox death rate during 1900 and 1903 epidemics") +
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.01)) +
  scale_x_continuous(breaks = 1907:1913) +
  theme_classic() +
  theme(plot.title=element_text(size=10,face="bold")) +
  theme(axis.title=element_text(size=8)) +
  ggtitle("1913")
COV_by_DR_1913

first_row <- plot_grid(COV_by_DR_1907, COV_by_DR_1908,  label_size = 12, nrow = 1)
second_row <- plot_grid(COV_by_DR_1909, COV_by_DR_1910,  label_size = 12, nrow = 1)
third_row <- plot_grid(COV_by_DR_1911, COV_by_DR_1912,  label_size = 12, nrow = 1)
fourth_row <- plot_grid(COV_by_DR_1913, NULL,  label_size = 12, nrow = 1)

fig_S4b_Vac_refusal <- plot_grid(first_row, second_row, third_row,
                                 fourth_row, label_size = 12, nrow = 4)
fig_S4b_Vac_refusal

ggsave(filename = paste0("fig_S4b_Vac_refusal_scatterplots.png"), plot = fig_S4b_Vac_refusal, path = "figs", width = 19, height = 28, units = "cm")
ggsave(filename = paste0("fig_S4b_Vac_refusal_scatterplots.pdf"), plot = fig_S4b_Vac_refusal, path = "figs", width = 19, height = 28, units = "cm")

### Correlations
my_cor_1907 <- sprintf("%0.3f", cor(COV_1907$Smallpox_death_rate, COV_1907$COV_Proportion_Births))
my_cor_1908 <- sprintf("%0.3f", cor(COV_1908$Smallpox_death_rate, COV_1908$COV_Proportion_Births))
my_cor_1909 <- sprintf("%0.3f", cor(COV_1909$Smallpox_death_rate, COV_1909$COV_Proportion_Births))
my_cor_1910 <- sprintf("%0.3f", cor(COV_1910$Smallpox_death_rate, COV_1910$COV_Proportion_Births))
my_cor_1911 <- sprintf("%0.3f", cor(COV_1911$Smallpox_death_rate, COV_1911$COV_Proportion_Births))
my_cor_1912 <- sprintf("%0.3f", cor(COV_1912$Smallpox_death_rate, COV_1912$COV_Proportion_Births))
my_cor_1913 <- sprintf("%0.3f", cor(COV_1913$Smallpox_death_rate, COV_1913$COV_Proportion_Births))

Years <- 1907:1913
Correlation <- c(my_cor_1907, my_cor_1908, my_cor_1909, my_cor_1910,
                 my_cor_1911, my_cor_1912, my_cor_1913)

Correl_df <- data.frame(Years, Correlation)
write_xlsx(Correl_df, paste0("Output_data/Correlations_incl_Dal_ME.xlsx"))

# Write out Correlation Table
stargazer(Correl_df, type = "html", out = "tables/Table_correl_FigS4b.html", summary = F, rownames = F)
