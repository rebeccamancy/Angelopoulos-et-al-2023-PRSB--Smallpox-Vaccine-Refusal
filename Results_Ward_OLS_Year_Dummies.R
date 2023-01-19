# Running clustered SE regressions but with year dummies, 
# omitting Yr 7 to prevent collinearity
# Used to construct table in SM, deaths and death rate only

rm(list = ls())
library(readxl) # For reading in data from Excel
library(writexl)
library(dplyr) # for SQL-like joins
library(tidyr) # for gather

# Load libraries for robust standard errors and small-sample adjustment
library("lmtest")
library(clubSandwich)
library(estimatr)
library(margins)

# Read in data
COV <- read_excel(path = paste0("Output_data/COV_Main_Dataset.xlsx"), 
                  sheet = "Ward_All")
source("R/compile_OLS_outputs.R")

yrs <- 1:7
yrs_text <- yrs + 1906

# ----------------------------------- Estimate models --------------------------------------
# DEATHS
# Create interaction variable
COV$intd <- COV$Smallpox_deaths * COV$Year_Numeric
smallpox_deaths_time_dummies <- lm_robust(COV_Proportion_Births ~ Smallpox_deaths + intd + Year_1 +
                                            Year_2 +  Year_3 +
                                            Year_4 +  Year_5 +
                                            Year_6 +
                                            Population_density_1911 + 
                                           Average_rooms_1911 +
                                           Perc_Irish_Born_1901 , 
                                         cluster = COV$Municipal_Ward_Name,
                                         se_type = 'stata', data=COV)
summary(smallpox_deaths_time_dummies)

# DEATHS PER 1000
# Create interaction variable
COV$intdr <- COV$Smallpox_death_rate * COV$Year_Numeric
smallpox_death_rate_time_dummies <- lm_robust(COV_Proportion_Births ~ Smallpox_death_rate + intdr + Year_1 +
                                                Year_2 +  Year_3 +
                                                Year_4 +  Year_5 +
                                                Year_6 +
                                              Population_density_1911 + 
                                            Average_rooms_1911 +
                                            Perc_Irish_Born_1901 , 
                                          cluster = COV$Municipal_Ward_Name,
                                          se_type = 'stata', data=COV)
summary(smallpox_death_rate_time_dummies)

# Format data for output

deaths_year_dummies_df <- data.frame(Experience_variable = "Smallpox deaths",
                                Variable_Name = names(smallpox_deaths_time_dummies$coefficients),
                                Estimate = smallpox_deaths_time_dummies$coefficients,
                                SE_Estimate = smallpox_deaths_time_dummies$std.error,
                                Pval_Estimate = smallpox_deaths_time_dummies$p.value,
                                Signif_estimate = stars.pval(smallpox_deaths_time_dummies$p.value))

death_rate_year_dummies_df <- data.frame(Experience_variable = "Smallpox death rates",
                                    Variable_Name = names(smallpox_death_rate_time_dummies$coefficients),
                                    Estimate = smallpox_death_rate_time_dummies$coefficients,
                                    SE_Estimate = smallpox_death_rate_time_dummies$std.error,
                                    Pval_Estimate = smallpox_death_rate_time_dummies$p.value,
                                    Signif_estimate = stars.pval(smallpox_death_rate_time_dummies$p.value))

year_dummies_OLS_df <- rbind(deaths_year_dummies_df, death_rate_year_dummies_df)

write_xlsx(year_dummies_OLS_df, paste0("Output_data/Results_Ward_OLS_year_dummies.xlsx"))
write.csv(x = year_dummies_OLS_df, file = paste0("Output_data/Results_Ward_OLS_year_dummie.csv"))



