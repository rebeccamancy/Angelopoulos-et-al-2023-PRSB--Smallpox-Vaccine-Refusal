# Produces results using random effects regression with clustered SE
# Used to construct table in Supplementary Materials (linear model only)

rm(list = ls())
library(readxl) # For reading in data from Excel
library(writexl)
library(sandwich) # for vcovCL
library(plm) # Estimates model with random effects
library(clubSandwich) # for coef_test, for clustered standard errors
library(gtools) # for stars from pvals

# Read in data
COV <- read_excel(path = paste0("Output_data/COV_Main_Dataset.xlsx"), 
                  sheet = "Ward_All")

# ----------------------------------- Estimate models --------------------------------------
# THE FOLLOWING REPLICATES STATA OUTPUT 
# CASES
Smallpox_cases_plmre <- plm(COV_Proportion_Births ~ Smallpox_cases * Year_Numeric +
                             Population_density_1911 + 
                                   Average_rooms_1911 + 
                              Perc_Irish_Born_1901 ,
                                 data=COV, index = c("Municipal_Ward_Number"), model = "random")
cases_RE_output <- coef_test(Smallpox_cases_plmre, vcov = "CR1S", test = "z")

# CASES PER 1000
Smallpox_cases_thou_plmre <- plm(COV_Proportion_Births ~ Smallpox_case_rate * Year_Numeric +
                                  Population_density_1911 + 
                                   Average_rooms_1911 + 
                                   Perc_Irish_Born_1901 ,
                                 data=COV, index = c("Municipal_Ward_Number"), model = "random")
case_rate_RE_output <- coef_test(Smallpox_cases_thou_plmre, vcov = "CR1S", test = "z")

# DEATHS
Smallpox_deaths_plmre <- plm(COV_Proportion_Births ~ Smallpox_deaths * Year_Numeric +
                              Population_density_1911 + 
                               Average_rooms_1911 + 
                               Perc_Irish_Born_1901, 
                          data=COV, index = c("Municipal_Ward_Number"), model = "random")
deaths_RE_output <- coef_test(Smallpox_deaths_plmre, vcov = "CR1S", test = "z")

# DEATHS PER 1000
Smallpox_death_rate_plmre <- plm(COV_Proportion_Births ~ Smallpox_death_rate * Year_Numeric +
                                  Population_density_1911 + 
                                   Average_rooms_1911 + 
                                   Perc_Irish_Born_1901,
                             data=COV, index = c("Municipal_Ward_Number"), model = "random")
death_rate_RE_output <- coef_test(Smallpox_death_rate_plmre, vcov = "CR1S", test = "z")

# Create data for output
cases_RE_df <- data.frame(Experience_variable = "Smallpox cases",
                    Variable_Name = names(Smallpox_cases_plmre$coefficients),
                               Estimate = cases_RE_output$beta,
                               SE_Estimate = cases_RE_output$SE,
                               Pval_Estimate = cases_RE_output$p_z,
                               Signif_estimate = stars.pval(cases_RE_output$p_z))

case_rate_RE_df <- data.frame(Experience_variable = "Smallpox case rates",
                                   Variable_Name = names(Smallpox_cases_thou_plmre$coefficients),
                                   Estimate = case_rate_RE_output$beta,
                                   SE_Estimate = case_rate_RE_output$SE,
                                   Pval_Estimate = case_rate_RE_output$p_z,
                                   Signif_estimate = stars.pval(case_rate_RE_output$p_z))

deaths_RE_df <- data.frame(Experience_variable = "Smallpox deaths",
                           Variable_Name = names(Smallpox_deaths_plmre$coefficients),
                           Estimate = deaths_RE_output$beta,
                           SE_Estimate = deaths_RE_output$SE,
                           Pval_Estimate = deaths_RE_output$p_z,
                           Signif_estimate = stars.pval(cases_RE_output$p_z))

death_rate_RE_df <- data.frame(Experience_variable = "Smallpox death rates",
                               Variable_Name = names(Smallpox_death_rate_plmre$coefficients),
                               Estimate = death_rate_RE_output$beta,
                               SE_Estimate = death_rate_RE_output$SE,
                               Pval_Estimate = death_rate_RE_output$p_z,
                               Signif_estimate = stars.pval(death_rate_RE_output$p_z))

random_effects_OLS_df <- rbind(cases_RE_df, case_rate_RE_df,
                        deaths_RE_df, death_rate_RE_df)

write_xlsx(random_effects_OLS_df, paste0("Output_data/Results_Ward_OLS_random_effects.xlsx"))
write.csv(x = random_effects_OLS_df, file = paste0("Output_data/Results_Ward_OLS_random_effects.csv"))


