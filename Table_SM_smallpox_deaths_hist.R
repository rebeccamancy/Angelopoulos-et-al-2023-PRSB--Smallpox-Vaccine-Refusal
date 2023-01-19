### Smallpox deaths since 1855 - a table

rm(list = ls())
library(readxl) # For reading in data from Excel
library(writexl)
library(stargazer)


COV_data_path <- "raw_data/COV_Glasgow_Data_Final.xlsx"
excel_sheets(COV_data_path)

Smallpox_deaths <- read_excel(path = COV_data_path, sheet = "Smallpox_historical")

# Write out Table
stargazer(Smallpox_deaths, type = "html", out = "tables/Smallpox_deaths_historical.html", summary = F, rownames = F)
