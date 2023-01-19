# Table_SM_year dummies - a four column table including year dummies, deaths and DR for WARDS 
# linear and non-linear

rm(list = ls())
library(readxl) # For reading in data from Excel
library(tidyr)
library(dplyr)
library(stargazer)

yrs <- 1:7
yrs_text <- yrs + 1906

OLS_df <-read_excel(path = "Output_data/Results_Ward_OLS_year_dummies.xlsx")
fracreg_df <- read_excel(path = "Output_data/Results_Ward_fracreg_Year_Dummies.xlsx")
sprintf_estimates <- "%1.2e"
sprintf_se <- "%1.2e"

# We want the data.frame organised by 
results_df <- OLS_df
results_df <- results_df[c("Experience_variable","Variable_Name","Estimate", "SE_Estimate", "Signif_estimate")]
results_df <- subset(results_df, Experience_variable != "Smallpox_cases" & Experience_variable != "Smallpox_case_rate")
results_df$Signif_estimate <- ifelse(results_df$Signif_estimate == "." | is.na(results_df$Signif_estimate), "", results_df$Signif_estimate)
# Rename interaction term for rates for consistency
results_df$Variable_Name <- ifelse(results_df$Variable_Name== "intdr", "intd", results_df$Variable_Name)

# Format Estimate and SE_estimate
results_df$Data_formatted <- paste0(sprintf(sprintf_estimates, results_df$Estimate), "", 
                                    results_df$Signif_estimate, " ",
                                    "(", sprintf(sprintf_se, results_df$SE_Estimate), ")")
results_df <- results_df %>%
  dplyr::select(Experience_variable, Variable_Name, Data_formatted)
# Substitute display variable names
# Replace Variable_names to allow correct grouping: All those with interaction term to be named the same; then all those with smallpox in to be named the same
results_df$Variable_Name <- ifelse(results_df$Variable_Name %in% c("Smallpox_cases","Smallpox_case_rate","Smallpox_deaths","Smallpox_death_rate"),
                                   "Smallpox", results_df$Variable_Name)
results_df$Variable_Name <- ifelse(grepl(pattern = "Year_1",x = results_df$Variable_Name, fixed = T),
                                   "Year 1", results_df$Variable_Name)
results_df$Variable_Name <- ifelse(grepl(pattern = "Year_2",x = results_df$Variable_Name, fixed = T),
                                   "Year 2", results_df$Variable_Name)
results_df$Variable_Name <- ifelse(grepl(pattern = "Year_3",x = results_df$Variable_Name, fixed = T),
                                   "Year 3", results_df$Variable_Name)
results_df$Variable_Name <- ifelse(grepl(pattern = "Year_4",x = results_df$Variable_Name, fixed = T),
                                   "Year 4", results_df$Variable_Name)
results_df$Variable_Name <- ifelse(grepl(pattern = "Year_5",x = results_df$Variable_Name, fixed = T),
                                   "Year 5", results_df$Variable_Name)
results_df$Variable_Name <- ifelse(grepl(pattern = "Year_6",x = results_df$Variable_Name, fixed = T),
                                   "Year 6", results_df$Variable_Name)

head(results_df)
data.frame(results_df)

results_table <- results_df %>%
  pivot_wider(names_from = Experience_variable, values_from = c(Data_formatted))
results_table$Variable_Name <- factor(results_table$Variable_Name,
                                      levels = c("(Intercept)","Smallpox","intd", "Year 1","Year 2","Year 3",
                                                 "Year 4","Year 5","Year 6","Population_density_1911", "Average_rooms_1911",
                                                 "Perc_Irish_Born_1901"),
                                      labels = c("(Intercept)","Smallpox","Interaction term", "Year 1", "Year 2","Year 3",
                                                 "Year 4","Year 5","Year 6",
                                                 "Population density","Rooms per dwelling","Percent Irish born"))
results_table <- arrange(results_table, Variable_Name)
results_table$Variable_Name <- as.character(results_table$Variable_Name) # coerce back to char so don't print out factor numbers, but labels!
colnames(results_table) <- gsub(pattern = "_", replacement = " ", x = colnames(results_table))
results_table

# Rename columns
results_table <- rename(results_table,"Smallpox deaths (linear)" = "Smallpox deaths")
results_table <- rename(results_table,"Smallpox death rate (linear)" = "Smallpox death rates")

# Now do for fracreg
results_df_fracreg <- fracreg_df
results_df_fracreg <- results_df_fracreg[c("Experience_variable","Variable_name","Estimate", "SE_estimate", "Signif_estimate")]
results_df_fracreg <- subset(results_df_fracreg, Experience_variable != "Smallpox_cases" & Experience_variable != "Smallpox_case_rate")
results_df_fracreg$Signif_estimate <- ifelse(results_df_fracreg$Signif_estimate == "." | is.na(results_df_fracreg$Signif_estimate), "", results_df_fracreg$Signif_estimate)
# Rename interaction term for rates for consistency
results_df_fracreg$Variable_name <- ifelse(results_df_fracreg$Variable_name== "intdr", "intd", results_df_fracreg$Variable_name)

# Format Estimate and SE_estimate
results_df_fracreg$Data_formatted <- paste0(sprintf(sprintf_estimates, results_df_fracreg$Estimate), "", 
                                    results_df_fracreg$Signif_estimate, " ",
                                    "(", sprintf(sprintf_se, results_df_fracreg$SE_estimate), ")")
results_df_fracreg <- results_df_fracreg %>%
  dplyr::select(Experience_variable, Variable_name, Data_formatted)

# Get rid of blanks for variable = year
results_df_fracreg <- subset(results_df_fracreg, Variable_name != "Year")
# Substitute display variable names
# Replace Variable_names to allow correct grouping: All those with interaction term to be named the same; then all those with smallpox in to be named the same
results_df_fracreg$Variable_name <- ifelse(results_df_fracreg$Variable_name %in% c("Smallpox_cases","Smallpox_case_rate","Smallpox_deaths","Smallpox_death_rate"),
                                   "Smallpox", results_df_fracreg$Variable_name)
results_df_fracreg$Variable_name <- ifelse(grepl(pattern = "Year_1",x = results_df_fracreg$Variable_name, fixed = T),
                                   "Year 1", results_df_fracreg$Variable_name)
results_df_fracreg$Variable_name <- ifelse(grepl(pattern = "Year_2",x = results_df_fracreg$Variable_name, fixed = T),
                                   "Year 2", results_df_fracreg$Variable_name)
results_df_fracreg$Variable_name <- ifelse(grepl(pattern = "Year_3",x = results_df_fracreg$Variable_name, fixed = T),
                                   "Year 3", results_df_fracreg$Variable_name)
results_df_fracreg$Variable_name <- ifelse(grepl(pattern = "Year_4",x = results_df_fracreg$Variable_name, fixed = T),
                                   "Year 4", results_df_fracreg$Variable_name)
results_df_fracreg$Variable_name <- ifelse(grepl(pattern = "Year_5",x = results_df_fracreg$Variable_name, fixed = T),
                                   "Year 5", results_df_fracreg$Variable_name)
results_df_fracreg$Variable_name <- ifelse(grepl(pattern = "Year_6",x = results_df_fracreg$Variable_name, fixed = T),
                                   "Year 6", results_df_fracreg$Variable_name)

head(results_df_fracreg)
data.frame(results_df_fracreg)

results_table_fracreg <- results_df_fracreg %>%
  pivot_wider(names_from = Experience_variable, values_from = c(Data_formatted))
results_table_fracreg$Variable_name <- factor(results_table_fracreg$Variable_name,
                                      levels = c("(Intercept)","Smallpox","intd","Year 1", "Year 2",
                                                 "Year 3","Year 4","Year 5","Year 6",
                                                 "Population_density_1911","Average_rooms_1911","Perc_Irish_Born_1901"),
                                      labels = c("(Intercept)","Smallpox","Interaction term", "Year 1", "Year 2","Year 3",
                                                 "Year 4","Year 5","Year 6",
                                                 "Population density","Rooms per dwelling","Percent Irish born"))
results_table_fracreg <- arrange(results_table_fracreg, Variable_name)
results_table_fracreg$Variable_name <- as.character(results_table_fracreg$Variable_name) # coerce back to char so don't print out factor numbers, but labels!
colnames(results_table_fracreg) <- gsub(pattern = "_", replacement = " ", x = colnames(results_table_fracreg))
results_table_fracreg

# Rename columns
results_table_fracreg <- rename(results_table_fracreg,"Smallpox deaths (non-linear)" = "Smallpox deaths")
results_table_fracreg <- rename(results_table_fracreg,"Smallpox death rate (non-linear)" = "Smallpox death rate")

# Combine dfs
results_table <- cbind(results_table, results_table_fracreg[,2:3])

# Change order of columns so deaths first
results_table <- results_table %>% dplyr::select("Variable Name","Smallpox deaths (linear)","Smallpox deaths (non-linear)", "Smallpox death rate (linear)","Smallpox death rate (non-linear)")
colnames(results_table)[1] <- " " # Remove name of first col

# Use stargazer to create table. This seems to introduce spaces between asterisks in html output so remove and then 
#    write out manually as text
html_code <- stargazer(results_table, type = "html", summary = F, rownames = F)
html_code <- gsub(pattern = "\\* \\* \\*", replacement = "***", x = html_code)
html_code <- gsub(pattern = "\\* \\*", replacement = "**", x = html_code)

fileConn <- file("tables/Table_year_dummies_wards.html", )
writeLines(html_code, fileConn)
close(fileConn)

