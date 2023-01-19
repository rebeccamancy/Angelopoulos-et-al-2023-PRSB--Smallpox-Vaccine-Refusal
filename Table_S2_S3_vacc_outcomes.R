# Table_SM2 Showing vaccination outcomes by RD, 1899 - 1906 and
# Table SM3 showing same data plus COV 1907-1913

rm(list = ls())
library(readxl) # For reading in data from Excel
library(tidyr)
library(dplyr)
library(stargazer)
library(scales) # for percentages

yrs <- 1:7
yrs_text <- yrs + 1906

Vacc_outcomes <- read_excel("raw_data/COV_Glasgow_Data_Final.xlsx", sheet = "Reg_district_vacc_annual")

# 1899-1906
results_table <- Vacc_outcomes 
# Select only years and variables required
#str(results_table)
results_table <- subset(results_table, Year <= 1906)
results_table <- results_table %>%
  dplyr::select(Year, Successfully_vaccinated_perc, "Successfully_vacc_as_%_of_those_still_alive_aged_1_yr",
                Removed_otherwise_unaccounted_perc, `Removed_or_unacc_as_%_of_those_still_alive_aged_1_yr`,
                Other_perc, Other_perc_surv)

# Restrict to currently required variables
required_vars <- c("Year", "Successfully_vacc_as_%_of_those_still_alive_aged_1_yr", "Removed_or_unacc_as_%_of_those_still_alive_aged_1_yr",
                   "Other_perc_surv")

results_table <- results_table[required_vars]

# Format as percentages
results_table$`Successfully_vacc_as_%_of_those_still_alive_aged_1_yr` <- percent(results_table$`Successfully_vacc_as_%_of_those_still_alive_aged_1_yr`, accuracy = 0.1)
results_table$`Removed_or_unacc_as_%_of_those_still_alive_aged_1_yr`<- percent(results_table$`Removed_or_unacc_as_%_of_those_still_alive_aged_1_yr`, accuracy = 0.1)
results_table$Other_perc_surv <- percent(results_table$Other_perc_surv, accuracy = 0.1)

colnames(results_table)[1] <- "Year of birth"
colnames(results_table)[2] <- "Successfully vaccinated" 
colnames(results_table)[3] <- "Removed/Unaccounted for"
colnames(results_table)[4] <- "Insusceptible, etc." 

results_table

# Use stargazer to create table. This seems to introduce spaces between asterisks in html output so remove and then 
#    write out manually as text
html_code <- stargazer(results_table, type = "html", summary = F, rownames = F)
#html_code <- gsub(pattern = "\\* \\* \\*", replacement = "***", x = html_code)
#html_code <- gsub(pattern = "\\* \\*", replacement = "**", x = html_code)

fileConn <- file("tables/Table_SM2_vacc_outcomes_1898_1906.html", )
writeLines(html_code, fileConn)
close(fileConn)

# 1907-1913
results_table <- Vacc_outcomes 
# Select only years and variables required
#str(results_table)
results_table <- subset(results_table, Year >= 1907)
results_table <- results_table %>%
  dplyr::select(Year, Successfully_vaccinated_perc, "Successfully_vacc_as_%_of_those_still_alive_aged_1_yr",
                Removed_otherwise_unaccounted_perc, `Removed_or_unacc_as_%_of_those_still_alive_aged_1_yr`,
                Other_perc, Other_perc_surv, COV_as_perc_of_those_still_alive_aged_1_yr)

# Restrict to currently required variables
required_vars <- c("Year", "Successfully_vacc_as_%_of_those_still_alive_aged_1_yr", "Removed_or_unacc_as_%_of_those_still_alive_aged_1_yr",
                   "Other_perc_surv", "COV_as_perc_of_those_still_alive_aged_1_yr")

results_table <- results_table[required_vars]

# Format as percentages
results_table$`Successfully_vacc_as_%_of_those_still_alive_aged_1_yr` <- percent(results_table$`Successfully_vacc_as_%_of_those_still_alive_aged_1_yr`, accuracy = 0.1)
results_table$`Removed_or_unacc_as_%_of_those_still_alive_aged_1_yr`<- percent(results_table$`Removed_or_unacc_as_%_of_those_still_alive_aged_1_yr`, accuracy = 0.1)
results_table$Other_perc_surv <- percent(results_table$Other_perc_surv, accuracy = 0.1)
results_table$COV_as_perc_of_those_still_alive_aged_1_yr <- percent(results_table$COV_as_perc_of_those_still_alive_aged_1_yr, accuracy = 0.1)

colnames(results_table)[1] <- "Year of birth"
colnames(results_table)[2] <- "Successfully vaccinated" 
colnames(results_table)[3] <- "Removed/Unaccounted for"
colnames(results_table)[4] <- "Insusceptible, etc." 
colnames(results_table)[5] <- "Conscientious objection to vaccination" 

results_table

# Use stargazer to create table. This seems to introduce spaces between asterisks in html output so remove and then 
#    write out manually as text
html_code <- stargazer(results_table, type = "html", summary = F, rownames = F)
#html_code <- gsub(pattern = "\\* \\* \\*", replacement = "***", x = html_code)
#html_code <- gsub(pattern = "\\* \\*", replacement = "**", x = html_code)

fileConn <- file("tables/Table_SM2_vacc_outcomes_1907_13.html", )
writeLines(html_code, fileConn)
close(fileConn)
