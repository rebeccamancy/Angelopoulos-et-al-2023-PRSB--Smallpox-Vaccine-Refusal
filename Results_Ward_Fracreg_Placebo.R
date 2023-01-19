# Running Fracreg logit model for measles and scarlet fever instead of smallpox - placebo
# Produces equivalent of original results, with clustered SE, but for measles
# Used to construct Table S16 in Supplementary Materials

getwd()
rm(list = ls())
library(readxl) # For reading in data from Excel
library(writexl)
library(dplyr) 
library(lmtest)
library(margins)
library(sandwich)
library(clubSandwich)
require(gtools) # for stars from pvals
library(margins) # for margins

# Read in data
COV <- read_excel(path = paste0("Output_data/COV_Main_Dataset.xlsx"), 
                  sheet = "Ward_All")
source("R/compile_fracreg_outputs.R")

yrs <- 1:7
yrs_text <- yrs + 1906
# ----------------------------------- Estimate models --------------------------------------
### MEASLES
# DEATHS 
glm_frac_measles_deaths <- glm(COV_Proportion_Births ~ Measles_deaths * Year_Numeric +
                                 Population_density_1911 + 
                                 Average_rooms_1911 + Perc_Irish_Born_1901 , data = COV, family = quasibinomial('logit'))
summary(glm_frac_measles_deaths)
measles_deaths_fracreg_cl_vcov_mat <- vcovCL(glm_frac_measles_deaths, cluster = ~Municipal_Ward_Name)
measles_deaths_fracreg_clusSE <- coeftest(glm_frac_measles_deaths, vcov = measles_deaths_fracreg_cl_vcov_mat)
measles_deaths_fracreg_clusSE

# Compute margins, passing in covariance matrix for clustered standard errors
deaths_fracreg_margins_years <- margins(glm_frac_measles_deaths, 
                                        variables = "Measles_deaths", 
                                        vcov = measles_deaths_fracreg_cl_vcov_mat,
                                        at = list(Year_Numeric = yrs))
deaths_fracreg_margins <- margins(glm_frac_measles_deaths, vcov = measles_deaths_fracreg_cl_vcov_mat) 
summary(deaths_fracreg_margins)

# DEATHS PER THOU
glm_frac_measles_deaths_thou <- glm(COV_Proportion_Births ~ Measles_death_rate * Year_Numeric +
                                      Population_density_1911 + 
                                      Average_rooms_1911 + Perc_Irish_Born_1901 , data = COV, family = quasibinomial('logit'))
summary(glm_frac_measles_deaths_thou)
measles_deaths_thou_fracreg_cl_vcov_mat <- vcovCL(glm_frac_measles_deaths_thou, cluster = ~Municipal_Ward_Name)
measles_deaths_thou_fracreg_clusSE <- coeftest(glm_frac_measles_deaths_thou, vcov = measles_deaths_thou_fracreg_cl_vcov_mat)
measles_deaths_thou_fracreg_clusSE

# Compute margins, passing in covariance matrix for clustered standard errors
death_rate_fracreg_margins_years <- margins(glm_frac_measles_deaths_thou, 
                                            variables = "Measles_death_rate",
                                            vcov = measles_deaths_thou_fracreg_cl_vcov_mat,
                                            at = list(Year_Numeric = yrs))
death_rate_fracreg_margins <- margins(glm_frac_measles_deaths_thou, vcov = measles_deaths_thou_fracreg_cl_vcov_mat) 
summary(death_rate_fracreg_margins)
summary(death_rate_fracreg_margins_years)

#######################################################################################
#                     Construct data.frame for making figures
#######################################################################################

# Read in descriptive statistics for computing sd change effects
descriptive_stats <- read_excel(path = paste0("Output_data/Desc_Stat_Ward_All_TimeInvariant.xlsx"))
COV_descriptive_stats <- read_excel(path = "Output_data/DescStat_Ward_All_TimeVarying.xlsx")
# Compute mean COV rate over all years 
mean_COV <- mean(COV_descriptive_stats$Mean_COV)
# 

deaths_fracreg_df <- compile_fracreg_outputs(Experience_var_name = "Measles_deaths",
                                             Count_rate = "Count",
                                             Deaths_cases = "Deaths",
                                             Model = "fracreg", 
                                             With_controls = "With", 
                                             margins_output = deaths_fracreg_margins,
                                             margins_output_years = deaths_fracreg_margins_years,
                                             clustered_se = measles_deaths_fracreg_clusSE,
                                             descriptive_stats = descriptive_stats,
                                             COV_descriptive_stats = COV_descriptive_stats, 
                                             mean_COV = mean_COV)

death_rate_fracreg_df <- compile_fracreg_outputs(Experience_var_name = "Measles_death_rate", 
                                                 Count_rate = "Rate",
                                                 Deaths_cases = "Deaths",
                                                 Model = "fracreg", 
                                                 With_controls = "With",
                                                 margins_output = death_rate_fracreg_margins,
                                                 margins_output_years = death_rate_fracreg_margins_years,
                                                 clustered_se = measles_deaths_thou_fracreg_clusSE,
                                                 descriptive_stats = descriptive_stats,
                                                 COV_descriptive_stats = COV_descriptive_stats, 
                                                 mean_COV = mean_COV)

measles_fracreg_df <- rbind(deaths_fracreg_df, death_rate_fracreg_df)



write_xlsx(measles_fracreg_df, paste0("Output_data/Results_Ward_fracreg_measles.xlsx"))
write.csv(x = measles_fracreg_df, file = paste0("Output_data/Results_Ward_fracreg_measles.csv"))

### SCARLET FEVER
# DEATHS 
glm_frac_Scarlet_fever_deaths <- glm(COV_Proportion_Births ~ Scarlet_fever_deaths * Year_Numeric +
                                       Population_density_1911 + 
                                       Average_rooms_1911 + Perc_Irish_Born_1901 , data = COV, family = quasibinomial('logit'))
summary(glm_frac_Scarlet_fever_deaths)
Scarlet_fever_deaths_fracreg_cl_vcov_mat <- vcovCL(glm_frac_Scarlet_fever_deaths, cluster = ~Municipal_Ward_Name)
Scarlet_fever_deaths_fracreg_clusSE <- coeftest(glm_frac_Scarlet_fever_deaths, vcov = Scarlet_fever_deaths_fracreg_cl_vcov_mat)
Scarlet_fever_deaths_fracreg_clusSE

# Compute margins, passing in covariance matrix for clustered standard errors
deaths_fracreg_margins_years <- margins(glm_frac_Scarlet_fever_deaths, 
                                        variables = "Scarlet_fever_deaths", 
                                        vcov = Scarlet_fever_deaths_fracreg_cl_vcov_mat,
                                        at = list(Year_Numeric = yrs))
deaths_fracreg_margins <- margins(glm_frac_Scarlet_fever_deaths, vcov = Scarlet_fever_deaths_fracreg_cl_vcov_mat) 
summary(deaths_fracreg_margins)


# DEATHS PER THOU
glm_frac_Scarlet_fever_deaths_thou <- glm(COV_Proportion_Births ~ Scarlet_fever_death_rate * Year_Numeric +
                                            Population_density_1911 + 
                                            Average_rooms_1911 + Perc_Irish_Born_1901 , data = COV, family = quasibinomial('logit'))
summary(glm_frac_Scarlet_fever_deaths_thou)
Scarlet_fever_deaths_thou_fracreg_cl_vcov_mat <- vcovCL(glm_frac_Scarlet_fever_deaths_thou, cluster = ~Municipal_Ward_Name)
Scarlet_fever_deaths_thou_fracreg_clusSE <- coeftest(glm_frac_Scarlet_fever_deaths_thou, vcov = Scarlet_fever_deaths_thou_fracreg_cl_vcov_mat)
Scarlet_fever_deaths_thou_fracreg_clusSE

# Compute margins, passing in covariance matrix for clustered standard errors
death_rate_fracreg_margins_years <- margins(glm_frac_Scarlet_fever_deaths_thou, 
                                            variables = "Scarlet_fever_death_rate",
                                            vcov = Scarlet_fever_deaths_thou_fracreg_cl_vcov_mat,
                                            at = list(Year_Numeric = yrs))
death_rate_fracreg_margins <- margins(glm_frac_Scarlet_fever_deaths_thou, vcov = Scarlet_fever_deaths_thou_fracreg_cl_vcov_mat) 
summary(death_rate_fracreg_margins)
summary(death_rate_fracreg_margins_years)

#######################################################################################
#                     Construct data.frame for making figures
#######################################################################################

# Read in descriptive statistics for computing sd change effects
descriptive_stats <- read_excel(path = paste0("Output_data/Desc_Stat_Ward_All_TimeInvariant.xlsx"))
COV_descriptive_stats <- read_excel(path = "Output_data/DescStat_Ward_All_TimeVarying.xlsx")
# Compute mean COV rate over all years 
mean_COV <- mean(COV_descriptive_stats$Mean_COV)
# 

deaths_fracreg_df <- compile_fracreg_outputs(Experience_var_name = "Scarlet_fever_deaths",
                                             Count_rate = "Count",
                                             Deaths_cases = "Deaths",
                                             Model = "fracreg", 
                                             With_controls = "With", 
                                             margins_output = deaths_fracreg_margins,
                                             margins_output_years = deaths_fracreg_margins_years,
                                             clustered_se = Scarlet_fever_deaths_fracreg_clusSE,
                                             descriptive_stats = descriptive_stats,
                                             COV_descriptive_stats = COV_descriptive_stats, 
                                             mean_COV = mean_COV)

death_rate_fracreg_df <- compile_fracreg_outputs(Experience_var_name = "Scarlet_fever_death_rate", 
                                                 Count_rate = "Rate",
                                                 Deaths_cases = "Deaths",
                                                 Model = "fracreg", 
                                                 With_controls = "With",
                                                 margins_output = death_rate_fracreg_margins,
                                                 margins_output_years = death_rate_fracreg_margins_years,
                                                 clustered_se = Scarlet_fever_deaths_thou_fracreg_clusSE,
                                                 descriptive_stats = descriptive_stats,
                                                 COV_descriptive_stats = COV_descriptive_stats, 
                                                 mean_COV = mean_COV)

Scarlet_fever_fracreg_df <- rbind(deaths_fracreg_df, death_rate_fracreg_df)

write_xlsx(Scarlet_fever_fracreg_df, paste0("Output_data/Results_Ward_fracreg_Scarlet_fever.xlsx"))
write.csv(x = Scarlet_fever_fracreg_df, file = paste0("Output_data/Results_Ward_fracreg_Scarlet_fever.csv"))

