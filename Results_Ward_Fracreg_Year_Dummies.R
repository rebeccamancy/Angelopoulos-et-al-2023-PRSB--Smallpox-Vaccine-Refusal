# Using LOGIT fractional regression to model impact of prior experience on COV
# Calculating with year dummies, for deaths and death rate only
# For supplementary materials

getwd()
rm(list = ls())
library(readxl) # For reading in data from Excel
library(writexl)
library(dplyr) # for SQL-like joins
library(lmtest) # for coeftest
library(margins) # for margins
library(sandwich) # for vcovCL

#######################################################################################
#                                 Set up and estimate models 
#######################################################################################

# Read in data and code to compile model outputs
COV <- read_excel(path = paste0("Output_data/COV_Main_Dataset.xlsx"), 
                  sheet = "Ward_All")
source("R/compile_fracreg_outputs.R")

yrs <- 1:7
yrs_text <- yrs + 1906

# DEATHS
# Create interaction variable
COV$intd <- COV$Smallpox_deaths * COV$Year_Numeric
deaths_fracreg <- glm(COV_Proportion_Births ~ Smallpox_deaths + intd + Year_1 +
                        Year_2 +  Year_3 +
                        Year_4 +  Year_5 +
                        Year_6 + Population_density_1911 + 
                       Average_rooms_1911 + Perc_Irish_Born_1901 , 
                     data = COV, 
                     family = quasibinomial('logit')) # fracreg logit
summary(deaths_fracreg) # NB: Only use this output for coefficient estimates because se are non-clustered

# Compute clustered standard errors - see https://www.r-bloggers.com/2021/05/clustered-standard-errors-with-r/
# Compute covariance matrix
deaths_fracreg_cl_vcov_mat <- vcovCL(deaths_fracreg, cluster = ~Municipal_Ward_Name)
deaths_fracreg_clusSE <- coeftest(deaths_fracreg, vcov = deaths_fracreg_cl_vcov_mat)
# Compute margins, passing in covariance matrix for clustered standard errors
# This gives the same AME as Stata with clustered SE
deaths_fracreg_margins_years <- margins(deaths_fracreg, vcov = deaths_fracreg_cl_vcov_mat, 
                                           variables = "Smallpox_deaths", 
                                           at = list(Year_Numeric = yrs))
summary(deaths_fracreg_margins_years)
deaths_fracreg_margins <- margins(deaths_fracreg, vcov = deaths_fracreg_cl_vcov_mat) 
summary(deaths_fracreg_margins)

# DEATH RATES
# Create interaction variable
COV$intdr <- COV$Smallpox_death_rate * COV$Year_Numeric
death_rate_fracreg <- glm(COV_Proportion_Births ~ Smallpox_death_rate + intdr + Year_1 +
                            Year_2 +  Year_3 +
                            Year_4 +  Year_5 +
                            Year_6 + Population_density_1911 + 
                            Average_rooms_1911 + Perc_Irish_Born_1901, 
                         data = COV, 
                         family = quasibinomial('logit')) # fracreg logit
summary(death_rate_fracreg) # NB: Only use this output for coefficient estimates because se are non-clustered

# Compute clustered standard errors - see https://www.r-bloggers.com/2021/05/clustered-standard-errors-with-r/
# Compute covariance matrix
death_rate_fracreg_cl_vcov_mat <- vcovCL(death_rate_fracreg, cluster = ~Municipal_Ward_Name)
death_rate_fracreg_clusSE <- coeftest(death_rate_fracreg, vcov = death_rate_fracreg_cl_vcov_mat)
# Compute margins, passing in covariance matrix for clustered standard errors
# This gives the same AME as Stata with clustered SE
death_rate_fracreg_margins_years <- margins(death_rate_fracreg, vcov = death_rate_fracreg_cl_vcov_mat, 
                                           variables = "Smallpox_death_rate", 
                                           at = list(Year_Numeric = yrs))
summary(death_rate_fracreg_margins_years)
death_rate_fracreg_margins <- margins(death_rate_fracreg, vcov = death_rate_fracreg_cl_vcov_mat) 
summary(death_rate_fracreg_margins)


# Read in descriptive statistics for computing sd change effects
descriptive_stats <- read_excel(path = paste0("Output_data/Desc_Stat_Ward_All_TimeInvariant.xlsx"))
COV_descriptive_stats <- read_excel(path = "Output_data/DescStat_Ward_All_TimeVarying.xlsx")
# Compute mean COV rate over all years directly from COV
mean_COV <- mean(COV_descriptive_stats$Mean_COV)

deaths_fracreg_df <- compile_fracreg_outputs(Experience_var_name = "Smallpox_deaths",
                                             Count_rate = "Count",
                                             Deaths_cases = "Deaths",
                                             Model = "fracreg", 
                                             With_controls = "With", 
                                             margins_output = deaths_fracreg_margins,
                                             margins_output_years = deaths_fracreg_margins_years,
                                             clustered_se = deaths_fracreg_clusSE,
                                             descriptive_stats = descriptive_stats,
                                             COV_descriptive_stats = COV_descriptive_stats, 
                                             mean_COV = mean_COV)

death_rate_fracreg_df <- compile_fracreg_outputs(Experience_var_name = "Smallpox_death_rate", 
                                                 Count_rate = "Rate",
                                                 Deaths_cases = "Deaths",
                                                 Model = "fracreg", 
                                                 With_controls = "With",
                                                 margins_output = death_rate_fracreg_margins,
                                                 margins_output_years = death_rate_fracreg_margins_years,
                                                 clustered_se = death_rate_fracreg_clusSE,
                                                 descriptive_stats = descriptive_stats,
                                                 COV_descriptive_stats = COV_descriptive_stats, 
                                                 mean_COV = mean_COV)

fracreg_df <- rbind(deaths_fracreg_df, death_rate_fracreg_df)

write_xlsx(fracreg_df, paste0("Output_data/Results_Ward_fracreg_Year_Dummies.xlsx"))
write.csv(x = fracreg_df, file = paste0("Output_data/Results_Ward_fracreg_Year_Dummies.csv"))

