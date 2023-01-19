# Table_SM for placebo (measles), ward level data
# - a four column table, deaths and death rate for WARDS both linear and non-linear models

rm(list = ls())
library(readxl) # For reading in data from Excel
library(tidyr)
library(dplyr)
library(stargazer)

yrs <- 1:7
yrs_text <- yrs + 1906

OLS_df <- read_excel(path = "Output_data/Results_Ward_OLS_measles.xlsx")
fracreg_df <- read_excel(path = "Output_data/Results_Ward_fracreg_measles.xlsx")

sprintf_estimates <- "%1.2e"
sprintf_se <- "%1.2e"

# We want the data.frame organised by 
results_df <- OLS_df
results_df <- results_df[c("Experience_variable","Variable_Name","Estimate", "SE_Estimate", "Signif_estimate")]
#results_df <- subset(results_df, Experience_variable != "Smallpox_cases" & Experience_variable != "Smallpox_case_rate" )
results_df$Signif_estimate <- ifelse(results_df$Signif_estimate == "." | is.na(results_df$Signif_estimate), "", results_df$Signif_estimate)
# Format Estimate and SE_estimate
results_df$Data_formatted <- paste0(sprintf(sprintf_estimates, results_df$Estimate), "", 
                                    results_df$Signif_estimate, " ",
                                    "(", sprintf(sprintf_se, results_df$SE_Estimate), ")")
results_df <- results_df %>%
  dplyr::select(Experience_variable, Variable_Name, Data_formatted)
# Substitute display variable names
# Replace Variable_names to allow correct grouping: All those with interaction term to be named the same; then all those wiht Measles in to be named the same
results_df$Variable_Name <- ifelse(results_df$Variable_Name %in% c( "Measles_deaths","Measles_death_rate"),
                                   "Measles", results_df$Variable_Name)
results_df$Variable_Name <- ifelse(results_df$Variable_Name %in% c("Measles_deaths:Year_Numeric", "Measles_death_rate:Year_Numeric"),
                                   "Measles x year", results_df$Variable_Name)
head(results_df)

results_table <- results_df %>%
  pivot_wider(names_from = Experience_variable, values_from = c(Data_formatted))
results_table$Variable_Name <- factor(results_table$Variable_Name,
                                      levels = c("(Intercept)","Measles","Measles x year","Year_Numeric",
                                                 "Population_density_1911","Average_rooms_1911","Perc_Irish_Born_1901"),
                                      labels = c("(Intercept)","Measles","Measles x year","Year",
                                                 "Population density","Rooms per dwelling","Percent Irish born"))
results_table <- arrange(results_table, Variable_Name)
results_table$Variable_Name <- as.character(results_table$Variable_Name) # coerce back to char so don't print out factor numbers, but labels!
colnames(results_table) <- gsub(pattern = "_", replacement = " ", x = colnames(results_table))
results_table <- rbind(results_table, c("R2", "", ""))
results_table
# Rename columns
results_table <- rename(results_table,"Measles deaths (linear)" = "Measles deaths")
results_table <- rename(results_table,"Measles death rate (linear)" = "Measles death rates")

# Fracreg
# We want the data.frame organised by 
results_df <- fracreg_df 
results_df <- results_df[c("Experience_variable","Variable_Name","Estimate", "SE_estimate", "Signif_estimate")]
results_df <- subset(results_df, Experience_variable == "Measles death rate" | Experience_variable == "Measles deaths")
results_df$Signif_estimate <- ifelse(results_df$Signif_estimate == "." | is.na(results_df$Signif_estimate), "", results_df$Signif_estimate)
# Format Estimate and SE_estimate
results_df$Data_formatted <- paste0(sprintf(sprintf_estimates, results_df$Estimate), "", 
                                    results_df$Signif_estimate, " ",
                                    "(", sprintf(sprintf_se, results_df$SE_estimate), ")")
results_df <- results_df %>%
  dplyr::select(Experience_variable, Variable_Name, Data_formatted)
# Substitute display variable names
# Replace Variable_Names to allow correct grouping: All those with interaction term to be named the same; then all those wiht Measles in to be named the same
results_df$Variable_Name <- ifelse(results_df$Variable_Name %in% c("Measles_deaths","Measles_death_rate"),
                                   "Measles", results_df$Variable_Name)
results_df$Variable_Name <- ifelse(results_df$Variable_Name %in% c("Measles_deaths:Year_Numeric","Measles_death_rate:Year_Numeric"),
                                   "Measles x year", results_df$Variable_Name)

head(results_df)

results_table_frac <- results_df %>%
  pivot_wider(names_from = Experience_variable, values_from = c(Data_formatted))
results_table_frac$Variable_Name <- factor(results_table_frac$Variable_Name,
                                           levels = c("(Intercept)","Measles","Measles x year","Year_Numeric",
                                                      "Population_density_1911","Average_rooms_1911","Perc_Irish_Born_1901"),
                                           labels = c("(Intercept)","Measles","Measles x year","Year",
                                                      "Population density","Rooms per dwelling","Percent Irish born"))
results_table_frac <- arrange(results_table_frac, Variable_Name)
results_table_frac$Variable_Name <- as.character(results_table_frac$Variable_Name) # coerce back to char so don't print out factor numbers, but labels!
colnames(results_table_frac) <- gsub(pattern = "_", replacement = " ", x = colnames(results_table_frac))
colnames(results_table_frac)[1] <- " " # Remove name of first col
results_table_frac <- rbind(results_table_frac, c("R2", "", ""))
results_table_frac
# Rename columns
results_table_frac <- rename(results_table_frac,"Measles deaths (non-linear)" = "Measles deaths")
results_table_frac <- rename(results_table_frac,"Measles death rate (non-linear)" = "Measles death rate")

# Bind the table together
results_table <- cbind(results_table, results_table_frac[,2:3])

# Change order of columns so deaths first
results_table <- results_table %>% dplyr::select("Variable Name","Measles deaths (linear)","Measles deaths (non-linear)", "Measles death rate (linear)","Measles death rate (non-linear)")
colnames(results_table)[1] <- " " # Remove name of first col 

# Use stargazer to create table. This seems to introduce spaces between asterisks in html output so remove and then 
#    write out manually as text
html_code <- stargazer(results_table, type = "html", summary = F, rownames = F)
html_code <- gsub(pattern = "\\* \\* \\*", replacement = "***", x = html_code)
html_code <- gsub(pattern = "\\* \\*", replacement = "**", x = html_code)

fileConn <- file("tables/Table_SM_Placebo.html", )
writeLines(html_code, fileConn)
close(fileConn)

