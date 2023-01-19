# Data from registration districts (RDs)
# Produces base model results (OLS with clustered SE)
# Also produces margins output for OLS model
# COV output is used to create Figure 4
# Deaths and DR output for COV, COVRU and RU is provided in tables in SM

getwd()
rm(list = ls())
library(readxl) # For reading in data from Excel
library(writexl)
library(dplyr) # for SQL-like joins
library(tidyr) # for gather

# Load libraries for robust standard errors and small-sample adjustment
library(lmtest)
library(clubSandwich)
library(margins)
library(estimatr)
library(sandwich) # for vcovCL
library(aods3)

#######################################################################################
#                                 Set up and estimate models 
#######################################################################################

yrs <- 1:7
yrs_text <- yrs + 1906

### Baseline model ###
# Read in data and code to compile model outputs
COV <- read_excel(path = paste0("Output_data/COV_Main_Dataset.xlsx"), 
                  sheet = "RD_Drop_Blyt")

source("R/compile_OLS_outputs.R")

# ----------------------------------- Estimate models --------------------------------------

#### COV ONLY ####
# CASES
cases_OLS <- lm_robust(COV_surv_perc ~ Smallpox_cases * Year_Numeric +
                         Population_density_1911 + 
                         Average_rooms_1911 +
                         Perc_Irish_Born_1901, 
                       cluster = COV$Registration_district_name,
                       se_type = 'stata', data=COV)
summary(cases_OLS) 

# Margins 
cases_OLS_margins_years <- margins(cases_OLS, variables = "Smallpox_cases", 
                                   at = list(Year_Numeric = yrs))
summary(cases_OLS_margins_years)[c("AME","SE","z","p","lower","upper")]
cases_OLS_margins <- margins(cases_OLS)

# CASES PER 1000
case_rate_OLS <- lm_robust(COV_surv_perc ~ Smallpox_case_rate * Year_Numeric +
                             Population_density_1911 + 
                             Average_rooms_1911 +
                             Perc_Irish_Born_1901 , 
                           cluster = COV$Registration_district_name,
                           se_type = 'stata', 
                           data = COV)
summary(case_rate_OLS) 

# Margins
case_rate_OLS_margins <- margins(case_rate_OLS)
case_rate_OLS_margins_years <- margins(case_rate_OLS, variables = "Smallpox_case_rate", 
                                       at = list(Year_Numeric = yrs))
summary(case_rate_OLS_margins)
summary(case_rate_OLS_margins_years)[c("AME","SE","z","p","lower","upper")]

# DEATHS
deaths_OLS <- lm_robust(COV_surv_perc ~ Smallpox_deaths * Year_Numeric +
                          Population_density_1911 + 
                          Average_rooms_1911 +
                          Perc_Irish_Born_1901 , 
                        cluster = COV$Registration_district_name,
                        se_type = 'stata', 
                        data = COV)
summary(deaths_OLS) 

# Margins
deaths_OLS_margins <- margins(deaths_OLS)
deaths_OLS_margins_years <- margins(deaths_OLS, variables = "Smallpox_deaths", 
                                    at = list(Year_Numeric = yrs))
summary(deaths_OLS_margins)
summary(deaths_OLS_margins_years)[c("AME","SE","z","p","lower","upper")]

# DEATHS PER 1000
death_rate_OLS <- lm_robust(COV_surv_perc ~ Smallpox_death_rate * Year_Numeric +
                              Population_density_1911 + 
                              Average_rooms_1911 +
                              Perc_Irish_Born_1901 , 
                            cluster = COV$Registration_district_name,
                            se_type = 'stata', 
                            data = COV)
summary(death_rate_OLS)

# Margins
death_rate_OLS_margins <- margins(death_rate_OLS)
death_rate_OLS_margins_years <- margins(death_rate_OLS, variables = "Smallpox_death_rate", 
                                        at = list(Year_Numeric = yrs))
summary(death_rate_OLS_margins)
summary(death_rate_OLS_margins_years)[c("AME","SE","z","p","lower","upper")]

#######################################################################################
#                     Construct data.frame for making figures
#######################################################################################

# Read in descriptive statistics for computing sd change effects
descriptive_stats <- read_excel(path = paste0("Output_data/Desc_Stat_RD_All_TimeInvariant.xlsx"))
COV_descriptive_stats <- read_excel(path = "Output_data/DescStat_RD_All_TimeVarying.xlsx")
# Compute mean COV rate over all years directly from COV
mean_COV <- mean(COV_descriptive_stats$Mean_COV) # Note COV in RD descriptive stats is actually COV_surv_perc

cases_OLS_df <- compile_OLS_outputs(Experience_var_name = "Smallpox_cases", 
                                    Count_rate = "Count",
                                    Deaths_cases = "Cases",
                                    Model = "OLS", 
                                    With_controls = "With", 
                                    model_output = cases_OLS, 
                                    margins_output = cases_OLS_margins,
                                    margins_output_years = cases_OLS_margins_years,
                                    descriptive_stats = descriptive_stats,
                                    COV_descriptive_stats = COV_descriptive_stats, 
                                    mean_COV = mean_COV)

cases_OLS_df %>% filter(Time_varying == F)

case_rate_OLS_df <- compile_OLS_outputs(Experience_var_name = "Smallpox_case_rate", 
                                        Count_rate = "Rate",
                                        Deaths_cases = "Cases",
                                        Model = "OLS", 
                                        With_controls = "With", 
                                        model_output = case_rate_OLS, 
                                        margins_output = case_rate_OLS_margins,
                                        margins_output_years = case_rate_OLS_margins_years,
                                        descriptive_stats = descriptive_stats,
                                        COV_descriptive_stats = COV_descriptive_stats, 
                                        mean_COV = mean_COV)

deaths_OLS_df <- compile_OLS_outputs(Experience_var_name = "Smallpox_deaths", 
                                     Count_rate = "Count",
                                     Deaths_cases = "Deaths",
                                     Model = "OLS", 
                                     With_controls = "With", 
                                     model_output = deaths_OLS, 
                                     margins_output = deaths_OLS_margins,
                                     margins_output_years = deaths_OLS_margins_years,
                                     descriptive_stats = descriptive_stats,
                                     COV_descriptive_stats = COV_descriptive_stats, 
                                     mean_COV = mean_COV)

death_rate_OLS_df <- compile_OLS_outputs(Experience_var_name = "Smallpox_death_rate",
                                         Count_rate = "Rate",
                                         Deaths_cases = "Deaths",
                                         Model = "OLS", 
                                         With_controls = "With", 
                                         model_output = death_rate_OLS, 
                                         margins_output = death_rate_OLS_margins,
                                         margins_output_years = death_rate_OLS_margins_years,
                                         descriptive_stats = descriptive_stats,
                                         COV_descriptive_stats = COV_descriptive_stats, 
                                         mean_COV = mean_COV)

OLS_df <- rbind(cases_OLS_df, case_rate_OLS_df,
                deaths_OLS_df, death_rate_OLS_df)

write_xlsx(OLS_df, paste0("Output_data/Results_RD_OLS_Baseline.xlsx"))
#write.csv(x = OLS_df, file = paste0("Output_data/Results_RD_OLS_Baseline.csv"))

#### COV and RU together
# CASES
cases_OLS <- lm_robust(COV_RU_surv_perc ~ Smallpox_cases * Year_Numeric +
                         Population_density_1911 + 
                         Average_rooms_1911 +
                         Perc_Irish_Born_1901, 
                       cluster = COV$Registration_district_name,
                       se_type = 'stata', data=COV)
summary(cases_OLS) 

# Margins 
cases_OLS_margins_years <- margins(cases_OLS, variables = "Smallpox_cases", 
                                   at = list(Year_Numeric = yrs))
summary(cases_OLS_margins_years)[c("AME","SE","z","p","lower","upper")]
cases_OLS_margins <- margins(cases_OLS)


# CASES PER 1000
case_rate_OLS <- lm_robust(COV_RU_surv_perc ~ Smallpox_case_rate * Year_Numeric +
                             Population_density_1911 + 
                             Average_rooms_1911 +
                             Perc_Irish_Born_1901, 
                           cluster = COV$Registration_district_name,
                           se_type = 'stata', 
                           data = COV)
summary(case_rate_OLS) 

# Margins
case_rate_OLS_margins <- margins(case_rate_OLS)
case_rate_OLS_margins_years <- margins(case_rate_OLS, variables = "Smallpox_case_rate", 
                                       at = list(Year_Numeric = yrs))
summary(case_rate_OLS_margins)
summary(case_rate_OLS_margins_years)[c("AME","SE","z","p","lower","upper")]

# DEATHS
deaths_OLS <- lm_robust(COV_RU_surv_perc ~ Smallpox_deaths * Year_Numeric +
                          Population_density_1911 + 
                          Average_rooms_1911 +
                          Perc_Irish_Born_1901 , 
                        cluster = COV$Registration_district_name,
                        se_type = 'stata', 
                        data = COV)
summary(deaths_OLS) 

# Margins
deaths_OLS_margins <- margins(deaths_OLS)
deaths_OLS_margins_years <- margins(deaths_OLS, variables = "Smallpox_deaths", 
                                    at = list(Year_Numeric = yrs))
summary(deaths_OLS_margins)
summary(deaths_OLS_margins_years)[c("AME","SE","z","p","lower","upper")]

# DEATHS PER 1000
death_rate_OLS <- lm_robust(COV_RU_surv_perc ~ Smallpox_death_rate * Year_Numeric +
                              Population_density_1911 + 
                              Average_rooms_1911 +
                              Perc_Irish_Born_1901 , 
                            cluster = COV$Registration_district_name,
                            se_type = 'stata', 
                            data = COV)
summary(death_rate_OLS)

# Margins
death_rate_OLS_margins <- margins(death_rate_OLS)
death_rate_OLS_margins_years <- margins(death_rate_OLS, variables = "Smallpox_death_rate", 
                                        at = list(Year_Numeric = yrs))
summary(death_rate_OLS_margins)
summary(death_rate_OLS_margins_years)[c("AME","SE","z","p","lower","upper")]

#######################################################################################
#                     Construct data.frame for making figures
#######################################################################################
# Read in for COVRU
COVRU_descriptive_stats <- read_excel(path = "Output_data/DescStat_RD_All_TimeVarying_COVRU.xlsx")
# Compute mean COV rate over all years directly from COV
mean_COV <- mean(COVRU_descriptive_stats$Mean_COV) # Note COV in RD descriptive stats is actually COV_surv_perc

cases_OLS_df <- compile_OLS_outputs(Experience_var_name = "Smallpox_cases", 
                                    Count_rate = "Count",
                                    Deaths_cases = "Cases",
                                    Model = "OLS", 
                                    With_controls = "With", 
                                    model_output = cases_OLS, 
                                    margins_output = cases_OLS_margins,
                                    margins_output_years = cases_OLS_margins_years,
                                    descriptive_stats = descriptive_stats,
                                    COV_descriptive_stats = COVRU_descriptive_stats, 
                                    mean_COV = mean_COV)

cases_OLS_df %>% filter(Time_varying == T)

case_rate_OLS_df <- compile_OLS_outputs(Experience_var_name = "Smallpox_case_rate", 
                                        Count_rate = "Rate",
                                        Deaths_cases = "Cases",
                                        Model = "OLS", 
                                        With_controls = "With", 
                                        model_output = case_rate_OLS, 
                                        margins_output = case_rate_OLS_margins,
                                        margins_output_years = case_rate_OLS_margins_years,
                                        descriptive_stats = descriptive_stats,
                                        COV_descriptive_stats = COVRU_descriptive_stats, 
                                        mean_COV = mean_COV)

deaths_OLS_df <- compile_OLS_outputs(Experience_var_name = "Smallpox_deaths", 
                                     Count_rate = "Count",
                                     Deaths_cases = "Deaths",
                                     Model = "OLS", 
                                     With_controls = "With", 
                                     model_output = deaths_OLS, 
                                     margins_output = deaths_OLS_margins,
                                     margins_output_years = deaths_OLS_margins_years,
                                     descriptive_stats = descriptive_stats,
                                     COV_descriptive_stats = COVRU_descriptive_stats, 
                                     mean_COV = mean_COV)

death_rate_OLS_df <- compile_OLS_outputs(Experience_var_name = "Smallpox_death_rate",
                                         Count_rate = "Rate",
                                         Deaths_cases = "Deaths",
                                         Model = "OLS", 
                                         With_controls = "With", 
                                         model_output = death_rate_OLS, 
                                         margins_output = death_rate_OLS_margins,
                                         margins_output_years = death_rate_OLS_margins_years,
                                         descriptive_stats = descriptive_stats,
                                         COV_descriptive_stats = COVRU_descriptive_stats, 
                                         mean_COV = mean_COV)

OLS_df <- rbind(cases_OLS_df, case_rate_OLS_df,
                deaths_OLS_df, death_rate_OLS_df)

write_xlsx(OLS_df, paste0("Output_data/Results_RD_OLS_COVRU.xlsx"))

#### RU only

# CASES
cases_OLS <- lm_robust(RU_surv_perc ~ Smallpox_cases * Year_Numeric +
                         Population_density_1911 + 
                         Average_rooms_1911 +
                         Perc_Irish_Born_1901, 
                       cluster = COV$Registration_district_name,
                       se_type = 'stata', data=COV)
summary(cases_OLS) 

# Margins 
cases_OLS_margins_years <- margins(cases_OLS, variables = "Smallpox_cases", 
                                   at = list(Year_Numeric = yrs))
summary(cases_OLS_margins_years)[c("AME","SE","z","p","lower","upper")]
cases_OLS_margins <- margins(cases_OLS)


# CASES PER 1000
case_rate_OLS <- lm_robust(RU_surv_perc ~ Smallpox_case_rate * Year_Numeric +
                             Population_density_1911 + 
                             Average_rooms_1911 +
                             Perc_Irish_Born_1901, 
                           cluster = COV$Registration_district_name,
                           se_type = 'stata', 
                           data = COV)
summary(case_rate_OLS) 

# Margins
case_rate_OLS_margins <- margins(case_rate_OLS)
case_rate_OLS_margins_years <- margins(case_rate_OLS, variables = "Smallpox_case_rate", 
                                       at = list(Year_Numeric = yrs))
summary(case_rate_OLS_margins)
summary(case_rate_OLS_margins_years)[c("AME","SE","z","p","lower","upper")]

# DEATHS
deaths_OLS <- lm_robust(RU_surv_perc ~ Smallpox_deaths * Year_Numeric +
                          Population_density_1911 + 
                          Average_rooms_1911 +
                          Perc_Irish_Born_1901 , 
                        cluster = COV$Registration_district_name,
                        se_type = 'stata', 
                        data = COV)
summary(deaths_OLS) 

# Margins
deaths_OLS_margins <- margins(deaths_OLS)
deaths_OLS_margins_years <- margins(deaths_OLS, variables = "Smallpox_deaths", 
                                    at = list(Year_Numeric = yrs))
summary(deaths_OLS_margins)
summary(deaths_OLS_margins_years)[c("AME","SE","z","p","lower","upper")]

# DEATHS PER 1000
death_rate_OLS <- lm_robust(RU_surv_perc ~ Smallpox_death_rate * Year_Numeric +
                              Population_density_1911 + 
                              Average_rooms_1911 +
                              Perc_Irish_Born_1901 , 
                            cluster = COV$Registration_district_name,
                            se_type = 'stata', 
                            data = COV)
summary(death_rate_OLS)

# Margins
death_rate_OLS_margins <- margins(death_rate_OLS)
death_rate_OLS_margins_years <- margins(death_rate_OLS, variables = "Smallpox_death_rate", 
                                        at = list(Year_Numeric = yrs))
summary(death_rate_OLS_margins)
summary(death_rate_OLS_margins_years)[c("AME","SE","z","p","lower","upper")]

#######################################################################################
#                     Construct data.frame for making figures
#######################################################################################
# Read in for RU
RU_descriptive_stats <- read_excel(path = "Output_data/DescStat_RD_All_TimeVarying_RU.xlsx")
# Compute mean COV rate over all years directly from COV
mean_COV <- mean(RU_descriptive_stats$Mean_COV) # Note COV in RD descriptive stats is actually COV_surv_perc

cases_OLS_df <- compile_OLS_outputs(Experience_var_name = "Smallpox_cases", 
                                    Count_rate = "Count",
                                    Deaths_cases = "Cases",
                                    Model = "OLS", 
                                    With_controls = "With", 
                                    model_output = cases_OLS, 
                                    margins_output = cases_OLS_margins,
                                    margins_output_years = cases_OLS_margins_years,
                                    descriptive_stats = descriptive_stats,
                                    COV_descriptive_stats = RU_descriptive_stats, 
                                    mean_COV = mean_COV)

cases_OLS_df %>% filter(Time_varying == T)

case_rate_OLS_df <- compile_OLS_outputs(Experience_var_name = "Smallpox_case_rate", 
                                        Count_rate = "Rate",
                                        Deaths_cases = "Cases",
                                        Model = "OLS", 
                                        With_controls = "With", 
                                        model_output = case_rate_OLS, 
                                        margins_output = case_rate_OLS_margins,
                                        margins_output_years = case_rate_OLS_margins_years,
                                        descriptive_stats = descriptive_stats,
                                        COV_descriptive_stats = RU_descriptive_stats, 
                                        mean_COV = mean_COV)

deaths_OLS_df <- compile_OLS_outputs(Experience_var_name = "Smallpox_deaths", 
                                     Count_rate = "Count",
                                     Deaths_cases = "Deaths",
                                     Model = "OLS", 
                                     With_controls = "With", 
                                     model_output = deaths_OLS, 
                                     margins_output = deaths_OLS_margins,
                                     margins_output_years = deaths_OLS_margins_years,
                                     descriptive_stats = descriptive_stats,
                                     COV_descriptive_stats = RU_descriptive_stats, 
                                     mean_COV = mean_COV)

death_rate_OLS_df <- compile_OLS_outputs(Experience_var_name = "Smallpox_death_rate",
                                         Count_rate = "Rate",
                                         Deaths_cases = "Deaths",
                                         Model = "OLS", 
                                         With_controls = "With", 
                                         model_output = death_rate_OLS, 
                                         margins_output = death_rate_OLS_margins,
                                         margins_output_years = death_rate_OLS_margins_years,
                                         descriptive_stats = descriptive_stats,
                                         COV_descriptive_stats = RU_descriptive_stats, 
                                         mean_COV = mean_COV)

OLS_df <- rbind(cases_OLS_df, case_rate_OLS_df,
                deaths_OLS_df, death_rate_OLS_df)

write_xlsx(OLS_df, paste0("Output_data/Results_RD_OLS_RU.xlsx"))

