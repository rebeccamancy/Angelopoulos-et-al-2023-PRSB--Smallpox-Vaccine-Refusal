# This creates an empty table for Wald test output calculated in Stata
# Stata code is included to allow replication

rm(list = ls())
library(readxl) # For reading in data from Excel
library(writexl)
library(dplyr) # for SQL-like joins
library(naniar) # to empty dataframe


# Read in data
COV_wards <- read_excel(path = paste0("Output_data/Results_Ward_OLS_Baseline.xlsx"))

# Remove cases and case rates
COV_wards <- subset(COV_wards, Experience_variable != "Smallpox_cases" & Experience_variable != "Smallpox_case_rate")
COV_wards <- subset(COV_wards, Time_varying == "TRUE")

# # Stata code for replication using the dataset ("Output_data/COV_Main_Dataset.xlsx"), sheet = "Ward_All"
# generate intd= smallpox_deaths*year_numeric
# generate intdr= smallpox_death_rate*year_numeric
# reg cov_proportion_births smallpox_deaths intd year_numeric average_rooms_1911 population_density_1911 perc_irish_born, vce(cluster municipal_ward_number)
# test smallpox_deaths intd (Wald test for significance of including the smallpox variables in the model specification)
# test population_density_1911 average_rooms_1911 perc_irish_born (Wald test for significance of including the socieconomic variables in the model specification)
# fracreg logit cov_proportion_births smallpox_deaths intd year_numeric average_rooms_1911 population_density_1911 perc_irish_born, vce(cluster municipal_ward_number)
# test smallpox_deaths intd
# test population_density_1911 average_rooms_1911 perc_irish_born
# reg cov_proportion_births smallpox_death_rate intdr year_numeric average_rooms_1911 population_density_1911 perc_irish_born, vce(cluster municipal_ward_number)
# test smallpox_death_rate intdr
# test population_density_1911 average_rooms_1911 perc_irish_born
# fracreg logit cov_proportion_births smallpox_death_rate intdr year_numeric average_rooms_1911 population_density_1911 perc_irish_born, vce(cluster municipal_ward_number)
# test smallpox_death_rate intdr
# test population_density_1911 average_rooms_1911 perc_irish_born


# Output table, deaths and DR wards, linear and non-linear
Wald_table <- COV_wards$Experience_variable
Wald_table <- data.frame(Wald_table)
Wald_table$Model <- COV_wards$Model
Wald_table$Year <- COV_wards$Year
Wald_table$Year1 <- COV_wards$Year

colnames(Wald_table)[1] <- "Smallpox deaths (linear)" 
colnames(Wald_table)[2] <- "Smallpox deaths (non-linear)"
colnames(Wald_table)[3] <- "Smallpox death rate (linear)" 
colnames(Wald_table)[4] <- "Smallpox death rate (non-linear)"

rownames(Wald_table) <- gsub(pattern = "_", replacement = " ", rownames(Wald_table))
rownames(Wald_table)[1]<-"Drop smallpox"
rownames(Wald_table)[2]<-"Drop controls"
Wald_table <- Wald_table[1:2,]

# Table isn't empty but temporary output is deleted and replaced with Stata output

## Write out table
stargazer(Wald_table, type = "html", out = "tables/Table_Wald_output.html", summary = F, rownames = T)

