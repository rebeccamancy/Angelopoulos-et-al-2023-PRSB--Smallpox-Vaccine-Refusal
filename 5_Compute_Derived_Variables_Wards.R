# Computes derived variables (including distance to Belvidere Smallpox Hospital in km) 
# and population density by hectare.
# Compiles final version of dataset for ward-based data.

# Set working directory to Project Directory before attempting to run this code
getwd()
rm(list = ls())

library(readxl) # For reading in data from Excel
library(writexl)
library(rgdal) # for readOGR
library(rgeos) # for readWKT (location of smallpox hospital)
library(tidyverse) # for SQL-like joins and %>%

# ---- Select Load up dataset ----

COV <- read_excel(path = "Output_data/COV_Compiled_GIS_PixelWtd_Final.xlsx", sheet = "Panel_Format")
dim(COV)

# Convert ward name to factor variable, and year to numeric counting from 1 = 1907 
COV$Ward <- factor(COV$Municipal_Ward_Name)
COV$Year_Numeric <- COV$Year - 1906
dim(COV); summary(COV)

# ---- Compute derived variables ----

# Derived smallpox case variables
COV$Smallpox_cases <- COV$Smallpox_Cases_1901_Epidemic + COV$Smallpox_Cases_1903_Epidemic
COV$Smallpox_deaths <- COV$Smallpox_Deaths_1901_Epidemic + COV$Smallpox_Deaths_1903_Epidemic
# Cases per thousand population across the two epidemics
COV$Smallpox_case_rate <- ((COV$Smallpox_Cases_Per_Mill_1901_Epidemic + COV$Smallpox_Cases_Per_Mill_1903_Epidemic)/2)/1000
COV$Smallpox_death_rate <- ((COV$Smallpox_Deaths_Per_Mill_1901_Epidemic + COV$Smallpox_Deaths_Per_Mill_1903_Epidemic)/2)/1000

# Compute correlations between epidemics for reporting in paper
cor.test(COV$Smallpox_Cases_1901_Epidemic, COV$Smallpox_Cases_1903_Epidemic)
cor.test(COV$Smallpox_Deaths_1901_Epidemic, COV$Smallpox_Deaths_1903_Epidemic)
cor.test(COV$Smallpox_Cases_Per_Mill_1901_Epidemic, COV$Smallpox_Cases_Per_Mill_1903_Epidemic)
cor.test(COV$Smallpox_Deaths_Per_Mill_1901_Epidemic, COV$Smallpox_Deaths_Per_Mill_1903_Epidemic)

# Derived variables for additional control variables and placebo
COV$Measles_deaths <- COV$Measles_Deaths_1900 + COV$Measles_Deaths_1901 + COV$Measles_Deaths_1903 + COV$Measles_Deaths_1904
COV$Measles_death_rate <- ((COV$Measles_Deaths_Mill_1900 + COV$Measles_Deaths_Mill_1901 + COV$Measles_Deaths_Mill_1903 + COV$Measles_Deaths_Mill_1904)/4) / 1000
COV$Scarlet_fever_deaths <- COV$Scarlet_fever_Deaths_1900 + COV$Scarlet_fever_Deaths_1901 + COV$Scarlet_fever_Deaths_1903 + COV$Scarlet_fever_Deaths_1904
COV$Scarlet_fever_death_rate <- ((COV$Scarlet_fever_Deaths_Mill_1900 + COV$Scarlet_fever_Deaths_Mill_1901 + COV$Scarlet_fever_Deaths_Mill_1903 + COV$Scarlet_fever_Deaths_Mill_1904)/4) / 1000

# Change variable names and convert population density to persons-per-hectare to use SI units
COV$Population_density_1911 <- COV$Persons_Per_Acre_Inc_Institutions_Census_1911*2.47105
COV$Average_rooms_1911 <- COV$Average_Occ_House_Size_1911
str(COV)

# Compute distance to Belvidere smallpox hospital
wards <- readOGR(dsn = path.expand("shp/Wards_Shp"), layer = "Wards_1912")
belvidere_smallpox <- readWKT("POINT(262400 663601)", p4s = proj4string(wards)) # in Dalmarnock, British National Grid coordinates, pulled from Digimap directly
ward_centroids <- gCentroid(wards, byid = T)
plot(wards); plot(ward_centroids, add=T, col="blue"); plot(belvidere_smallpox, add=T, col="red")
wards$Distance_Belvidere_metres <- as.numeric(gDistance(ward_centroids, belvidere_smallpox, byid = T))
wards$Distance_Belvidere <- wards$Distance_Belvidere_metres/1000 # convert to km
wards$Ward_Numeric <- as.numeric(as.character(wards$Ward_Num))
wards@data

# Join distances back into COV dataset
COV <- left_join(COV, wards@data, by=c("Municipal_Ward_Number"="Ward_Numeric"))
head(data.frame(COV[c("Ward","Ward_Name","Municipal_Ward_Number","Municipal_Ward_Name","Distance_Belvidere")]), 100)

# ---------------------- Compile only relevant variables -------------------
# Write out a spreadsheet containing only the relevant variables, and the associated data dictionary
final_vars <- c("Municipal_Ward_Name", "Municipal_Ward_Number",
                "Year", "Year_Numeric",
                "COV_Proportion_Births",
                "Smallpox_cases", "Smallpox_deaths",
                "Smallpox_case_rate", "Smallpox_death_rate",
                "Population_density_1911", 
                "Average_rooms_1911",
                "Perc_Irish_Born_1901",
                "Distance_Belvidere", "Measles_deaths",
                "Measles_death_rate",  "Scarlet_fever_deaths",
                "Scarlet_fever_death_rate", "Year_1", "Year_2",
                "Year_3", "Year_4", "Year_5", "Year_6", "Year_7")

data_dictionary <- data.frame(name = final_vars,
                              display_name = c("Municipal ward name", "Municipal ward number",
                                               "Year", "Year",
                                               "Vaccination refusal rate",
                                               "Smallpox cases", "Smallpox deaths",
                                               "Smallpox cases per 1000", "Smallpox deaths per 1000",
                                               "Population density 1911", 
                                               "Mean house size 1911",
                                               "Percentage Irish born",
                                               "Distance to smallpox hospital", "Measles deaths",
                                               "Measles death rate",  "Scarlet_fever_deaths",
                                               "Scarlet_fever_death_rate","Year dummy", "Year dummy",
                                               "Year dummy", "Year dummy", "Year dummy", "Year dummy",
                                               "Year dummy"),
                              description = c("Municipal ward name", "Municipal ward number",
                                              "Year","Count of years, starting in 1907",
                                              "Conscientious objections to vaccination as a proportion of births",
                                              "Total smallpox cases over the two epidemics","Total deaths over the two epidemics",
                                              "Smallpox cases per million population over the two epidemics","Smallpox deaths per million population over the two epidemics",
                                              "Population density (persons per hectare) in 1911",
                                              "Average number of rooms in occupied dwellings in 1911",
                                              "Percentage of the population born in Ireland", 
                                              "Distance to Belvidere smallpox hospital (in km)", "Measles deaths",
                                              "Measles deaths per 1000 population", "Scarlet fever deaths",
                                              "Scarlet fever deaths per 1000 population", "Year dummy Year 1", "Year dummy Year 2",
                                              "Year dummy Year 3", "Year dummy Year 4", "Year dummy Year 5", "Year dummy Year 6",
                                              "Year dummy Year 7"))

COV_Final <- COV[final_vars]

sheets <- list("Wards_All" = COV_Final, "Data_Dictionary_Wards" = data_dictionary) #assumes sheet1 and sheet2 are data frames
write_xlsx(sheets, paste0("Output_data/COV_Ward_Final.xlsx"))


