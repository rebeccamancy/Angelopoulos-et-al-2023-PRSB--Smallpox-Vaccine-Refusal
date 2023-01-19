# Using LOGIT fractional regression to model impact of prior experience on COV, 
# registration district data, without controls
# Used in Figure 4 of the main manuscript

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
                  sheet = "RD_Drop_Blyt")

source("R/compile_fracreg_outputs.R")

yrs <- 1:7
yrs_text <- yrs + 1906

# ----------------------------------- Estimate models --------------------------------------
# CASES
cases_fracreg <- glm(COV_surv_perc ~ Smallpox_cases * Year_Numeric, 
                     data = COV, 
                     family = quasibinomial('logit')) # fracreg logit
summary(cases_fracreg) # NB: Only use this output for coefficient estimates because se are non-clustered

# Compute clustered standard errors - see https://www.r-bloggers.com/2021/05/clustered-standard-errors-with-r/
# Compute covariance matrix
cases_fracreg_cl_vcov_mat <- vcovCL(cases_fracreg, cluster = ~Reg_district_number)
cases_fracreg_clusSE <- coeftest(cases_fracreg, vcov = cases_fracreg_cl_vcov_mat)
# Compute margins, passing in covariance matrix for clustered standard errors
# This gives the same AME as Stata with clustered SE
cases_fracreg_margins_years <- margins(cases_fracreg, vcov = cases_fracreg_cl_vcov_mat, 
                                       variables = "Smallpox_cases", 
                                       at = list(Year_Numeric = yrs))
summary(cases_fracreg_margins_years)
cases_fracreg_margins <- margins(cases_fracreg, vcov = cases_fracreg_cl_vcov_mat) 
summary(cases_fracreg_margins)

# CASES PER THOU
case_rate_fracreg <- glm(COV_surv_perc ~ Smallpox_case_rate * Year_Numeric, 
                         data = COV, family = quasibinomial('logit'))
summary(case_rate_fracreg)
# Compute covariance matrix
case_rate_fracreg_cl_vcov_mat <- vcovCL(case_rate_fracreg, cluster = ~Reg_district_number)
case_rate_fracreg_clusSE <- coeftest(case_rate_fracreg, vcov = case_rate_fracreg_cl_vcov_mat)
# Compute margins, passing in covariance matrix for clustered standard errors
case_rate_fracreg_margins_years <- margins(case_rate_fracreg, vcov = case_rate_fracreg_cl_vcov_mat,
                                           variables = "Smallpox_case_rate", 
                                           at = list(Year_Numeric = yrs))
summary(case_rate_fracreg_margins_years)
case_rate_fracreg_margins <- margins(case_rate_fracreg, vcov = case_rate_fracreg_cl_vcov_mat) 
summary(case_rate_fracreg_margins)

# DEATHS 
deaths_fracreg <- glm(COV_surv_perc ~ Smallpox_deaths * Year_Numeric, 
                      data = COV, family = quasibinomial('logit'))
summary(deaths_fracreg)
# Compute covariance matrix
deaths_fracreg_cl_vcov_mat <- vcovCL(deaths_fracreg, cluster = ~Reg_district_number)
deaths_fracreg_clusSE <- coeftest(deaths_fracreg, vcov = deaths_fracreg_cl_vcov_mat)
# Compute margins, passing in covariance matrix for clustered standard errors
deaths_fracreg_margins_years <- margins(deaths_fracreg, 
                                        variables = "Smallpox_deaths", 
                                        vcov = deaths_fracreg_cl_vcov_mat,
                                        at = list(Year_Numeric = yrs))
deaths_fracreg_margins <- margins(deaths_fracreg, vcov = deaths_fracreg_cl_vcov_mat) 
summary(deaths_fracreg_margins)

# DEATHS PER THOU
death_rate_fracreg <- glm(COV_surv_perc ~ Smallpox_death_rate * Year_Numeric, 
                          data = COV, family = quasibinomial('logit'))
summary(death_rate_fracreg)
# Compute covariance matrix
death_rate_fracreg_cl_vcov_mat <- vcovCL(death_rate_fracreg, cluster = ~Reg_district_number)
death_rate_fracreg_clusSE <- coeftest(death_rate_fracreg, vcov = death_rate_fracreg_cl_vcov_mat)
# Compute margins, passing in covariance matrix for clustered standard errors
death_rate_fracreg_margins_years <- margins(death_rate_fracreg, 
                                            variables = "Smallpox_death_rate",
                                            vcov = death_rate_fracreg_cl_vcov_mat,
                                            at = list(Year_Numeric = yrs))
death_rate_fracreg_margins <- margins(death_rate_fracreg, vcov = death_rate_fracreg_cl_vcov_mat) 
summary(death_rate_fracreg_margins)

#######################################################################################
#                     Construct data.frame for making figures
#######################################################################################

# Read in descriptive statistics for computing sd change effects
descriptive_stats <- read_excel(path = paste0("Output_data/Desc_Stat_RD_All_TimeInvariant.xlsx"))
COV_descriptive_stats <- read_excel(path = "Output_data/DescStat_RD_All_TimeVarying.xlsx")
# Compute mean COV rate over all years directly from COV
mean_COV <- mean(COV_descriptive_stats$Mean_COV) # Note COV in RD descriptive stats is actually COV_surv_perc

cases_fracreg_df <- compile_fracreg_outputs(Experience_var_name = "Smallpox_cases",
                                            Count_rate = "Count",
                                            Deaths_cases = "Cases",
                                            Model = "fracreg",
                                            With_controls = "Without",
                                            margins_output = cases_fracreg_margins,
                                            margins_output_years = cases_fracreg_margins_years,
                                            clustered_se = cases_fracreg_clusSE,
                                            descriptive_stats = descriptive_stats,
                                            COV_descriptive_stats = COV_descriptive_stats, 
                                            mean_COV = mean_COV)

cases_fracreg_df %>% filter(Time_varying == T)

case_rate_fracreg_df <- compile_fracreg_outputs(Experience_var_name = "Smallpox_case_rate",
                                                Count_rate = "Rate",
                                                Deaths_cases = "Cases",
                                                Model = "fracreg",
                                                With_controls = "Without",
                                                margins_output = case_rate_fracreg_margins,
                                                margins_output_years = case_rate_fracreg_margins_years,
                                                clustered_se = case_rate_fracreg_clusSE,
                                                descriptive_stats = descriptive_stats,
                                                COV_descriptive_stats = COV_descriptive_stats,
                                                mean_COV = mean_COV)
case_rate_fracreg_df %>% filter(Time_varying == T)

deaths_fracreg_df <- compile_fracreg_outputs(Experience_var_name = "Smallpox_deaths",
                                             Count_rate = "Count",
                                             Deaths_cases = "Deaths",
                                             Model = "fracreg", 
                                             With_controls = "Without", 
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
                                                 With_controls = "Without",
                                                 margins_output = death_rate_fracreg_margins,
                                                 margins_output_years = death_rate_fracreg_margins_years,
                                                 clustered_se = death_rate_fracreg_clusSE,
                                                 descriptive_stats = descriptive_stats,
                                                 COV_descriptive_stats = COV_descriptive_stats, 
                                                 mean_COV = mean_COV)

fracreg_df <- rbind(cases_fracreg_df, case_rate_fracreg_df,
                    deaths_fracreg_df, death_rate_fracreg_df)

write_xlsx(fracreg_df, paste0("Output_data/Results_RD_fracreg_No_Controls.xlsx"))
write.csv(x = fracreg_df, file = paste0("Output_data/Results_RD_fracreg_No_Controls.csv"))
