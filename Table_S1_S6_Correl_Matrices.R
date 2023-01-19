# Table S1 - correlation matrices of experience variables for wards 
# Table S6 - correlation matrices of control variables for wards 

rm(list = ls())
library(readxl) # For reading in data from Excel
library(writexl)
library(dplyr) # for SQL-like joins
library(estimatr)
library(stargazer)

# Read in data
COV_wards <- read_excel(path = paste0("Output_data/COV_Main_Dataset.xlsx"), 
                        sheet = "Ward_All")

### Correlation matrices

# WARDS
# Correlation of smallpox experience variables
# Select one year as all years the same
COV_wards_1907 <-subset(COV_wards, Year == 1907)
COV_wards_exp <- COV_wards_1907 %>% dplyr::select(Smallpox_cases, Smallpox_case_rate, Smallpox_deaths,
                                             Smallpox_death_rate)
cor(COV_wards_exp)
COV_wards_exp <- as.data.frame(cor(COV_wards_exp))
colnames(COV_wards_exp) <- gsub(pattern = "_", replacement = " ", colnames(COV_wards_exp))
rownames(COV_wards_exp) <- gsub(pattern = "_", replacement = " ", rownames(COV_wards_exp))
## Write out correlation matrix for experience var
stargazer(COV_wards_exp, type = "html", out = "tables/Smallpox_correl_matrix_wards.html", summary = F, rownames = T)

# Select only control variables
COV_controls_wards <- COV_wards %>% dplyr::select(Population_density_1911,
                                      Average_rooms_1911, Perc_Irish_Born_1901, Distance_Belvidere)
# Rename columns
COV_controls_wards <- rename(COV_controls_wards,"Population density" = "Population_density_1911")
COV_controls_wards <- rename(COV_controls_wards,"Rooms per dwelling" = "Average_rooms_1911")
COV_controls_wards <- rename(COV_controls_wards,"Irish born percent" = "Perc_Irish_Born_1901")
COV_controls_wards <- rename(COV_controls_wards,"Distance from Belvidere" = "Distance_Belvidere")

cor(COV_controls_wards)
COV_controls_wards <- as.data.frame(cor(COV_controls_wards))
colnames(COV_controls_wards) <- gsub(pattern = "_", replacement = " ", colnames(COV_controls_wards))
rownames(COV_controls_wards) <- gsub(pattern = "_", replacement = " ", rownames(COV_controls_wards))
## Write out correlation matrix for controls
stargazer(COV_controls_wards, type = "html", out = "tables/Controls_correl_matrix_wards.html", summary = F, rownames = T)
