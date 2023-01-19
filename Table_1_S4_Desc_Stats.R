# Table 1 (descriptive stats for main m/s)
# and Table of COV descriptive stats by year for SM

rm(list = ls())
library(readxl) # For reading in data from Excel
library(tidyr)
library(dplyr)
library(stargazer)

yrs <- 1:7
yrs_text <- yrs + 1906

# WARDS
Desc_stats_df <- read_excel(path = "Output_data/Desc_Stat_Ward_All_TimeInvariant.xlsx")
Desc_stats_df <- subset(Desc_stats_df, var != "Distance_Belvidere" & var != "Measles_death_rate")

Desc_stats_df$var <- factor(Desc_stats_df$var,
                                      levels = c("Smallpox_cases","Smallpox_case_rate","Smallpox_deaths","Smallpox_death_rate",
                                                 "Population_density_1911","Average_rooms_1911","Perc_Irish_Born_1901"),
                                      labels = c("Smallpox cases","Smallpox case rate (per 1000 population)","Smallpox deaths","Smallpox death rate (per 1000 population)",
                                                 "Population density (persons per hectare)","Rooms per dwelling","Irish born (percent)"))

Desc_stats_df$var <- as.character(Desc_stats_df$var) # coerce back to char so don't print out factor numbers, but labels!
Desc_stats_df <- Desc_stats_df[c("var","min","max","mean","sd")]
colnames(Desc_stats_df) <- c("Variable","Minimum","Maximum","Mean","Standard deviation")

# Currently adding in figs for 7 year's COV manually from calculation in 7_Compute_Descriptive_Stats_Wards

# Write out Table 1
stargazer(Desc_stats_df, type = "html", out = "tables/Table_1.html", summary = F, rownames = F)

###################################################################################

# Create table for descriptive statistics for COV_Proportion_Births by year
Desc_stats_by_year_df <- read_excel(path = "Output_data/DescStat_Ward_All_TimeVarying.xlsx")

Desc_stats_by_year_df$Year <- factor(Desc_stats_by_year_df$Year,
                            levels = c("1907","1908","1909","1910",
                                       "1911","1912","1913"),
                            labels = c("COV 1907","COV 1908","COV 1909","COV 1910",
                                       "COV 1911","COV 1912","COV 1913"))

Desc_stats_by_year_df$Minimum <- Desc_stats_by_year_df$min_COV
Desc_stats_by_year_df$Maximum <- Desc_stats_by_year_df$max_COV
Desc_stats_by_year_df$Mean <- Desc_stats_by_year_df$Mean_COV
Desc_stats_by_year_df$"Standard deviation" <- Desc_stats_by_year_df$sd_COV
Desc_stats_by_year_df <- cbind(Desc_stats_by_year_df[,1], round(Desc_stats_by_year_df[,6:9], digits = 3))
Desc_stats_by_year_df$Variable <- factor(Desc_stats_by_year_df$Year,
                                          levels = c("COV 1907", "COV 1908", "COV 1909", "COV 1910",
                                                     "COV 1911", "COV 1912", "COV 1913"))
# Reorder columns
Desc_stats_by_year_df <- Desc_stats_by_year_df[,c("Variable", "Minimum", "Maximum", "Mean", "Standard deviation")]

## Write out Table 
stargazer(Desc_stats_by_year_df, type = "html", out = "tables/Table_SM_Desc_Stat_COV.html", summary = F, rownames = F)

