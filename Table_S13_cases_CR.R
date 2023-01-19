# Table_SM_ - a four column table, cases and case rate for WARDS both linear and non-linear models

rm(list = ls())
library(readxl) # For reading in data from Excel
library(tidyr)
library(dplyr)
library(stargazer)

yrs <- 1:7
yrs_text <- yrs + 1906

OLS_df <- read_excel(path = "Output_data/Results_Ward_OLS_Baseline.xlsx")
fracreg_df <- read_excel(path = "Output_data/Results_Wards_fracreg_Baseline.xlsx")

sprintf_estimates <- "%1.2e"
sprintf_se <- "%1.2e"

# We want the data.frame organised by 
results_df <- OLS_df %>% filter(Time_varying == F)
results_df <- results_df[c("Experience_variable","Variable_name","Estimate", "SE_estimate", "Signif_estimate")]
results_df <- subset(results_df, Experience_variable == "Smallpox_cases" | Experience_variable == "Smallpox_case_rate" )
results_df$Signif_estimate <- ifelse(results_df$Signif_estimate == "." | is.na(results_df$Signif_estimate), "", results_df$Signif_estimate)
# Format Estimate and SE_estimate
results_df$Data_formatted <- paste0(sprintf(sprintf_estimates, results_df$Estimate), "", 
                                    results_df$Signif_estimate, " ",
                                    "(", sprintf(sprintf_se, results_df$SE_estimate), ")")
results_df <- results_df %>%
  dplyr::select(Experience_variable, Variable_name, Data_formatted)
# Substitute display variable names
# Replace Variable_names to allow correct grouping: All those with interaction term to be named the same; then all those wiht Smallpox in to be named the same
results_df$Variable_name <- ifelse(results_df$Variable_name %in% c( "Smallpox_cases","Smallpox_case_rate"),
                                   "Smallpox", results_df$Variable_name)
results_df$Variable_name <- ifelse(results_df$Variable_name %in% c("Smallpox_cases:Year_Numeric", "Smallpox_case_rate:Year_Numeric"),
                                   "Smallpox x year", results_df$Variable_name)
head(results_df)

results_table <- results_df %>%
  pivot_wider(names_from = Experience_variable, values_from = c(Data_formatted))
results_table$Variable_name <- factor(results_table$Variable_name,
                                      levels = c("(Intercept)","Smallpox","Smallpox x year","Year_Numeric",
                                                 "Population_density_1911","Average_rooms_1911","Perc_Irish_Born_1901"),
                                      labels = c("(Intercept)","Smallpox","Smallpox x year","Year",
                                                 "Population density","Rooms per dwelling","Percent Irish born"))
results_table <- arrange(results_table, Variable_name)
results_table$Variable_name <- as.character(results_table$Variable_name) # coerce back to char so don't print out factor numbers, but labels!
colnames(results_table) <- gsub(pattern = "_", replacement = " ", x = colnames(results_table))
results_table <- rbind(results_table, c("R2", "", ""))
results_table
# Rename columns
results_table <- rename(results_table,"Smallpox cases (linear)" = "Smallpox cases")
results_table <- rename(results_table,"Smallpox case rate (linear)" = "Smallpox case rate")

# Fracreg
# We want the data.frame organised by 
results_df <- fracreg_df %>% filter(Time_varying == F)
results_df <- results_df[c("Experience_variable","Variable_name","Estimate", "SE_estimate", "Signif_estimate")]
results_df <- subset(results_df, Experience_variable == "Smallpox_case_rate" | Experience_variable == "Smallpox_cases")
results_df$Signif_estimate <- ifelse(results_df$Signif_estimate == "." | is.na(results_df$Signif_estimate), "", results_df$Signif_estimate)
# Format Estimate and SE_estimate
results_df$Data_formatted <- paste0(sprintf(sprintf_estimates, results_df$Estimate), "", 
                                    results_df$Signif_estimate, " ",
                                    "(", sprintf(sprintf_se, results_df$SE_estimate), ")")
results_df <- results_df %>%
  dplyr::select(Experience_variable, Variable_name, Data_formatted)
# Substitute display variable names
# Replace Variable_names to allow correct grouping: All those with interaction term to be named the same; then all those wiht Smallpox in to be named the same
results_df$Variable_name <- ifelse(results_df$Variable_name %in% c("Smallpox_cases","Smallpox_case_rate"),
                                   "Smallpox", results_df$Variable_name)
results_df$Variable_name <- ifelse(results_df$Variable_name %in% c("Smallpox_cases:Year_Numeric","Smallpox_case_rate:Year_Numeric"),
                                   "Smallpox x year", results_df$Variable_name)

head(results_df)

results_table_frac <- results_df %>%
  pivot_wider(names_from = Experience_variable, values_from = c(Data_formatted))
results_table_frac$Variable_name <- factor(results_table_frac$Variable_name,
                                      levels = c("(Intercept)","Smallpox","Smallpox x year","Year_Numeric",
                                                 "Population_density_1911","Average_rooms_1911","Perc_Irish_Born_1901"),
                                      labels = c("(Intercept)","Smallpox","Smallpox x year","Year",
                                                 "Population density","Rooms per dwelling","Percent Irish born"))
results_table_frac <- arrange(results_table_frac, Variable_name)
results_table_frac$Variable_name <- as.character(results_table_frac$Variable_name) # coerce back to char so don't print out factor numbers, but labels!
colnames(results_table_frac) <- gsub(pattern = "_", replacement = " ", x = colnames(results_table_frac))
colnames(results_table_frac)[1] <- " " # Remove name of first col
results_table_frac <- rbind(results_table_frac, c("R2", "", ""))
results_table_frac
# Rename columns
results_table_frac <- rename(results_table_frac,"Smallpox cases (non-linear)" = "Smallpox cases")
results_table_frac <- rename(results_table_frac,"Smallpox case rate (non-linear)" = "Smallpox case rate")

# Bind the table together
results_table <- cbind(results_table, results_table_frac[,2:3])

# Change order of columns so cases first
results_table <- results_table %>% dplyr::select("Variable name","Smallpox cases (linear)","Smallpox cases (non-linear)", "Smallpox case rate (linear)","Smallpox case rate (non-linear)")
colnames(results_table)[1] <- " " # Remove name of first col 

# Use stargazer to create table. This seems to introduce spaces between asterisks in html output so remove and then 
#    write out manually as text
html_code <- stargazer(results_table, type = "html", summary = F, rownames = F)
html_code <- gsub(pattern = "\\* \\* \\*", replacement = "***", x = html_code)
html_code <- gsub(pattern = "\\* \\*", replacement = "**", x = html_code)

fileConn <- file("tables/Table_SM_cases_case_rate.html", )
writeLines(html_code, fileConn)
close(fileConn)

