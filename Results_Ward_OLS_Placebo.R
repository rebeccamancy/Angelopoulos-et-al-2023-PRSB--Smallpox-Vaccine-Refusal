# Running OLS model for measles or scarlet fever instead of smallpox - placebo
# Produces equivalent of original results, with clustered SE, but for measles/scarlet fever
# Used in Supplementary Materials

rm(list = ls())
library(readxl) # For reading in data from Excel
library(writexl)
library(dplyr) # for SQL-like joins
library(tidyr) # for gather
library(estimatr)

# Load libraries for robust standard errors and small-sample adjustment
library("lmtest")
library(clubSandwich)
library(performance)
library(margins)

require(gtools) # for stars from pvals

source("R/compile_OLS_outputs.R")

# Read in data 
COV <- read_excel(path = paste0("Output_data/COV_Main_Dataset.xlsx"), 
                  sheet = "Ward_All")

yrs <- 1:7
yrs_text <- yrs + 1906

# ----------------------------------- Estimate models --------------------------------------
## MEASLES
# DEATHS
measles_deaths_OLS <- lm_robust(COV_Proportion_Births ~ Measles_deaths * Year_Numeric +
                                  Population_density_1911 +
                                  Average_rooms_1911 +
                                  Perc_Irish_Born_1901 ,
                                cluster = COV$Municipal_Ward_Name,
                                se_type = 'stata',
                                data = COV)
summary(measles_deaths_OLS)

# Margins
deaths_OLS_margins <- margins(measles_deaths_OLS)
deaths_OLS_margins_years <- margins(measles_deaths_OLS, variables = "Measles_deaths", 
                                    at = list(Year_Numeric = yrs))
summary(deaths_OLS_margins)
summary(deaths_OLS_margins_years)[c("AME","SE","z","p","lower","upper")]

# DEATHS PER 1000
measles_death_rate_OLS <- lm_robust(COV_Proportion_Births ~ Measles_death_rate * Year_Numeric +
                                      Population_density_1911 + 
                                      Average_rooms_1911 +
                                      Perc_Irish_Born_1901 , 
                                    cluster = COV$Municipal_Ward_Name,
                                    se_type = 'stata', 
                                    data = COV)
summary(measles_death_rate_OLS) 

# Margins
death_rate_OLS_margins <- margins(measles_death_rate_OLS)
death_rate_OLS_margins_years <- margins(measles_death_rate_OLS, variables = "Measles_death_rate", 
                                        at = list(Year_Numeric = yrs))
summary(death_rate_OLS_margins)
summary(death_rate_OLS_margins_years)[c("AME","SE","z","p","lower","upper")]

#######################################################################################
#                     Construct data.frame for making figures
#######################################################################################

# Read in descriptive statistics for computing sd change effects
descriptive_stats <- read_excel(path = paste0("Output_data/Desc_Stat_Ward_All_TimeInvariant.xlsx"))
COV_descriptive_stats <- read_excel(path = "Output_data/DescStat_Ward_All_TimeVarying.xlsx")
# Compute mean COV rate over all years and wards 
mean_COV <- mean(COV_descriptive_stats$Mean_COV)

deaths_OLS_df <- compile_OLS_outputs(Experience_var_name = "Measles_deaths", 
                                     Count_rate = "Count",
                                     Deaths_cases = "Deaths",
                                     Model = "OLS", 
                                     With_controls = "With", 
                                     model_output = measles_deaths_OLS, 
                                     margins_output = deaths_OLS_margins,
                                     margins_output_years = deaths_OLS_margins_years,
                                     descriptive_stats = descriptive_stats,
                                     COV_descriptive_stats = COV_descriptive_stats, 
                                     mean_COV = mean_COV)

death_rate_OLS_df <- compile_OLS_outputs(Experience_var_name = "Measles_death_rate",
                                         Count_rate = "Rate",
                                         Deaths_cases = "Deaths",
                                         Model = "OLS", 
                                         With_controls = "With", 
                                         model_output = measles_death_rate_OLS, 
                                         margins_output = death_rate_OLS_margins,
                                         margins_output_years = death_rate_OLS_margins_years,
                                         descriptive_stats = descriptive_stats,
                                         COV_descriptive_stats = COV_descriptive_stats, 
                                         mean_COV = mean_COV)

measles_OLS_df <- rbind(deaths_OLS_df, death_rate_OLS_df)


write_xlsx(measles_OLS_df, paste0("Output_data/Results_Ward_OLS_measles.xlsx"))
write.csv(x = measles_OLS_df, file = paste0("Output_data/Results_Ward_OLS_measles.csv"))

## SCARLET FEVER
# DEATHS
Scarlet_fever_deaths_OLS <- lm_robust(COV_Proportion_Births ~ Scarlet_fever_deaths * Year_Numeric +
                                        Population_density_1911 +
                                        Average_rooms_1911 +
                                        Perc_Irish_Born_1901 ,
                                      cluster = COV$Municipal_Ward_Name,
                                      se_type = 'stata',
                                      data = COV)
summary(Scarlet_fever_deaths_OLS)

# Margins
deaths_OLS_margins <- margins(Scarlet_fever_deaths_OLS)
deaths_OLS_margins_years <- margins(Scarlet_fever_deaths_OLS, variables = "Scarlet_fever_deaths", 
                                    at = list(Year_Numeric = yrs))
summary(deaths_OLS_margins)
summary(deaths_OLS_margins_years)[c("AME","SE","z","p","lower","upper")]

# DEATHS PER 1000
Scarlet_fever_death_rate_OLS <- lm_robust(COV_Proportion_Births ~ Scarlet_fever_death_rate * Year_Numeric +
                                            Population_density_1911 + 
                                            Average_rooms_1911 +
                                            Perc_Irish_Born_1901 , 
                                          cluster = COV$Municipal_Ward_Name,
                                          se_type = 'stata', 
                                          data = COV)
summary(Scarlet_fever_death_rate_OLS) 

# Margins
death_rate_OLS_margins <- margins(Scarlet_fever_death_rate_OLS)
death_rate_OLS_margins_years <- margins(Scarlet_fever_death_rate_OLS, variables = "Scarlet_fever_death_rate", 
                                        at = list(Year_Numeric = yrs))
summary(death_rate_OLS_margins)
summary(death_rate_OLS_margins_years)[c("AME","SE","z","p","lower","upper")]

#######################################################################################
#                     Construct data.frame for making figures
#######################################################################################

# Read in descriptive statistics for computing sd change effects
descriptive_stats <- read_excel(path = paste0("Output_data/Desc_Stat_Ward_All_TimeInvariant.xlsx"))
COV_descriptive_stats <- read_excel(path = "Output_data/DescStat_Ward_All_TimeVarying.xlsx")
# Compute mean COV rate over all years and wards 
mean_COV <- mean(COV_descriptive_stats$Mean_COV)

# 
deaths_OLS_df <- compile_OLS_outputs(Experience_var_name = "Scarlet_fever_deaths", 
                                     Count_rate = "Count",
                                     Deaths_cases = "Deaths",
                                     Model = "OLS", 
                                     With_controls = "With", 
                                     model_output = Scarlet_fever_deaths_OLS, 
                                     margins_output = deaths_OLS_margins,
                                     margins_output_years = deaths_OLS_margins_years,
                                     descriptive_stats = descriptive_stats,
                                     COV_descriptive_stats = COV_descriptive_stats, 
                                     mean_COV = mean_COV)

death_rate_OLS_df <- compile_OLS_outputs(Experience_var_name = "Scarlet_fever_death_rate",
                                         Count_rate = "Rate",
                                         Deaths_cases = "Deaths",
                                         Model = "OLS", 
                                         With_controls = "With", 
                                         model_output = Scarlet_fever_death_rate_OLS, 
                                         margins_output = death_rate_OLS_margins,
                                         margins_output_years = death_rate_OLS_margins_years,
                                         descriptive_stats = descriptive_stats,
                                         COV_descriptive_stats = COV_descriptive_stats, 
                                         mean_COV = mean_COV)

Scarlet_fever_OLS_df <- rbind(deaths_OLS_df, death_rate_OLS_df)

write_xlsx(Scarlet_fever_OLS_df, paste0("Output_data/Results_Ward_OLS_Scarlet_fever.xlsx"))
write.csv(x = Scarlet_fever_OLS_df, file = paste0("Output_data/Results_Ward_OLS_Scarlet_fever.csv"))
