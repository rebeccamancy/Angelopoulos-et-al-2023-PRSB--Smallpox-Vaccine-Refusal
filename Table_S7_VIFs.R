# Table S6 - Checking VIFs for  wards 

rm(list = ls())
library(readxl) # For reading in data from Excel
library(writexl)
library(dplyr) # for SQL-like joins
library(estimatr)
library(car) # for VIFs

# Read in data
COV_wards <- read_excel(path = paste0("Output_data/COV_Main_Dataset.xlsx"), 
                        sheet = "Ward_All")

# WARDS
# Run base model without interaction term
# VIF package doesn't run with clustered SE but this makes no difference to output
# Output is identical to Stata

smallpox_cases_OLS_lm <- lm(COV_Proportion_Births ~ Smallpox_cases + Year_Numeric +
                              Population_density_1911 + 
                              Average_rooms_1911 +
                              Perc_Irish_Born_1901 +
                              Distance_Belvidere, data=COV_wards)
summary(smallpox_cases_OLS_lm)

vif(smallpox_cases_OLS_lm)

smallpox_case_rate_OLS_lm <- lm(COV_Proportion_Births ~ Smallpox_case_rate + Year_Numeric +
                                  Population_density_1911 + 
                                  Average_rooms_1911 +
                                  Perc_Irish_Born_1901  +
                                  Distance_Belvidere, data=COV_wards)
summary(smallpox_case_rate_OLS_lm)

vif(smallpox_case_rate_OLS_lm)

smallpox_deaths_OLS_lm <- lm(COV_Proportion_Births ~ Smallpox_deaths + Year_Numeric +
                               Population_density_1911 + 
                               Average_rooms_1911 +
                               Perc_Irish_Born_1901   +
                               Distance_Belvidere, data=COV_wards)
summary(smallpox_deaths_OLS_lm)

vif(smallpox_deaths_OLS_lm)

smallpox_death_rate_OLS_lm <- lm(COV_Proportion_Births ~ Smallpox_death_rate + Year_Numeric +
                                   Population_density_1911 + 
                                   Average_rooms_1911 +
                                   Perc_Irish_Born_1901   +
                                   Distance_Belvidere, data=COV_wards)
summary(smallpox_death_rate_OLS_lm)

vif(smallpox_death_rate_OLS_lm)
# Output table, deaths and DR wards 
VIF_wards_deaths <- as.data.frame(vif(smallpox_deaths_OLS_lm))
VIF_wards_DR <- as.data.frame(vif(smallpox_death_rate_OLS_lm))
VIF_all <- cbind(VIF_wards_deaths, VIF_wards_DR)

# Rename columns
colnames(VIF_all)[1] <- "Smallpox deaths" 
colnames(VIF_all)[2] <-"Smallpox death rate" 

# Rename rows
rownames(VIF_all) <- gsub(pattern = "_", replacement = " ", rownames(VIF_all))
rownames(VIF_all)[1]<-"Smallpox"
rownames(VIF_all)[2]<-"Year"
rownames(VIF_all)[3]<-"Population density"
rownames(VIF_all)[4]<-"Rooms per dwelling"
rownames(VIF_all)[5]<-"Percent Irish born"
rownames(VIF_all)[6]<-"Distance from Belvidere"

## Write out VIF matrix 
stargazer(VIF_all, type = "html", out = "tables/Table_VIF_Wards.html", summary = F, rownames = T)

