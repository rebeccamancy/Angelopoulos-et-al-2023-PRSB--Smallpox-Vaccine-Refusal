# This script compiles raw data from multiple data entry tables into a single file, in panel format.
# For "historical" variables recorded by ward or SD (previously converted to ward), the file
# carries out conversion to registration districts (RDs). 
# The variables that need to be converted are smallpox experience variables, as well as measles 
# experience variables (for the "placebo" experiment), plus Irish born percent.
# All other variables are recorded directly by RD.

# Need to run 5_Compute_Derived_Variables_Wards before running this script.

# Set working directory to Project Directory before attempting to run this code
getwd()
rm(list = ls())

library(readxl) # For reading in data from Excel
library(writexl)
library(tidyverse)
library(reshape2) # for "melt"
library(sf) # for st_area for measuring areas 
COV_data_path <- "raw_data/COV_Glasgow_Data_Final.xlsx"
excel_sheets(COV_data_path)

# There are areas covered by the RDs that are within city boundaries (thus in the shapefile), but 
#   not in the area covered by SDs, and thus for which we do not have smallpox experience information.
#   Therefore, they are not relevant for our analysis and need to be excluded.
#   These are parts of wards 34-36 (Jordanhill, Pollokshaws and Cathcart) which overlap with
#   Hillhead, Eastwood and Cathcart RDs

# Read in conversion method using GIS conversion matrices - to remove parts of Cathcart etc 
# not in our Glasgow
where_RDs_come_from_A <- as.matrix(read_excel(path = "Output_data/GIS_Conversion_RD_Pixels_Wtd_RDs_A.xlsx", sheet = "where_RDs_come_from_GIS_A"))
where_wards_go_to_A <- as.matrix(read_excel(path = "Output_data/GIS_Conversion_RD_Pixels_Wtd_RDs_A.xlsx", sheet = "where_wards_go_to_GIS_A"))
(num_RDs_A <- ncol(where_RDs_come_from_A)) # 20 
(num_wards_A <- nrow(where_RDs_come_from_A)) # 28

# Remove the portion of the RD that is outwith our area of interest. 

# Read in RD data 
COV_counts <- read_excel(path = COV_data_path, sheet = "Vars_By_RD")
# Remove unwanted columns from end of dataset
COV <- COV_counts[,1:23]
# Reduce RD numbers to account for areas outwith Wards 1-25
# Define wards to be removed. Jordanhill, Pollokshaws and Cathcart wards are lines 26-28.
Unwanted_wards <- where_RDs_come_from_A[26:28,]
# Calculate what percentage of the total these wards account for
Unwanted_wards_perc <- colSums(Unwanted_wards)
Unwanted_wards_perc <- data.frame(Unwanted_wards_perc)
# Add RD number
Unwanted_wards_perc$RD_num <- 1:20
# Calculate % of the RD that is to remain
Unwanted_wards_perc$Perc_required <- ifelse(Unwanted_wards_perc$Unwanted_wards_perc==0,1,1-Unwanted_wards_perc$Unwanted_wards_perc)
# Add % calculation to main dataframe
COV$Perc_required <- rep(c(Unwanted_wards_perc$Perc_required), times=7)
COV_replicate <- COV
# Calculate the actual number of children born in RDs within our area of interest
COV$RD_Number_of_children_born <- COV_replicate$RD_Number_of_children_born*COV_replicate$Perc_required
# Check
aggregate(x= COV$RD_Number_of_children_born ,
          by= list(COV$Year),
          FUN=sum)
# Repeat for other count variables
COV$RD_Successfully_vaccinated_count <- COV_replicate$RD_Successfully_vaccinated_count*COV_replicate$Perc_required
COV$RD_Died_before_vaccination_count <- COV_replicate$RD_Died_before_vaccination_count*COV_replicate$Perc_required
COV$RD_Removed_otherwise_unaccounted_count <- COV_replicate$RD_Removed_otherwise_unaccounted_count*COV_replicate$Perc_required
COV$RD_COV_count <- COV_replicate$RD_COV_count*COV_replicate$Perc_required
COV$Illeg_births_count <- COV_replicate$Illeg_births_count*COV_replicate$Perc_required
COV$Population_1911 <- COV_replicate$Population_1911*COV_replicate$Perc_required

### Main conversion of data from 25 wards starts here ###
# Read in conversion method using GIS conversion matrices 
where_RDs_come_from <- as.matrix(read_excel(path = "Output_data/GIS_Conversion_RD_Pixels_Wtd_RDs.xlsx", sheet = "where_RDs_come_from_GIS"))
where_wards_go_to <- as.matrix(read_excel(path = "Output_data/GIS_Conversion_RD_Pixels_Wtd_RDs.xlsx", sheet = "where_wards_go_to_GIS"))
(num_RDs <- ncol(where_RDs_come_from)) # 20 
(num_wards <- nrow(where_RDs_come_from)) # 25

# ---- Convert historical data that exist by ward to registration district ----
# Read in historical variables by ward
historical_ward <- read_excel(path = "Output_data/COV_Ward_Final.xlsx")

# Reduce historical_ward to only the variables of interest
historical_ward <- dplyr::select (historical_ward, "Year", "Municipal_Ward_Number", "Municipal_Ward_Name",
                                  "Smallpox_cases","Smallpox_deaths",
                                  "Smallpox_case_rate","Smallpox_death_rate","Perc_Irish_Born_1901",
                                  "Measles_deaths", "Measles_death_rate")

all_ward_variables <- names(historical_ward)
rate_variables <- c("Smallpox_case_rate","Smallpox_death_rate",
                    "Measles_death_rate", 
                    "Perc_Irish_Born_1901")
count_variables <- c("Smallpox_cases","Smallpox_deaths", "Measles_deaths")
# # Cross-check we have covered all variables!
setdiff(x = all_ward_variables, y = c(rate_variables, count_variables))
setdiff(y = all_ward_variables, x = c(rate_variables, count_variables))

# Carry out main conversion of historical variables from wards to RDs, into a renamed tibble COV_Compiled
# Use just one year as expereince variables don't change by year. Choosing 1911 as has full data.

historical_ward_1911 <- subset(historical_ward, Year== 1911)

COV_Compiled <- subset(COV, Year == 1907)
# Loop over variables ... Rate conversion or Count conversion of variables 
# from wards to RDs
for (cc in 3:ncol(historical_ward_1911)) { # cc = 4 is Smallpox_cases
  if (names(historical_ward_1911[cc]) %in% rate_variables) { # RATES
    print(paste0(names(historical_ward_1911[cc]), " so doing rate conversion"))
    curr_variable <- as.numeric(unlist(historical_ward_1911[,cc]))
    curr_rate_variable_matrix <- replicate(num_RDs, curr_variable)
    # Compute average of the ward rates, weighted by RD make-up
    convert_rate_matrix <- where_RDs_come_from * curr_rate_variable_matrix
    RD_rate_variable <- (colSums(convert_rate_matrix))
    COV_Compiled <- cbind(COV_Compiled, RD_rate_variable)
    colnames(COV_Compiled)[ncol(COV_Compiled)] <- colnames(historical_ward_1911)[cc]
  } else if (names(historical_ward_1911[cc]) %in% count_variables) { # COUNTS
    print(paste0(names(historical_ward_1911[cc]), " so doing count conversion"))
    curr_variable <- as.numeric(unlist(historical_ward_1911[,cc]))
    curr_count_variable_matrix <- replicate(num_RDs, curr_variable)
    # Spread out the ward counts according to where the wards went to 
    convert_count_matrix <- where_wards_go_to * curr_variable
    RD_count_variable <- (colSums(convert_count_matrix))
    print(paste0(sum(curr_variable), "=", sum(RD_count_variable)))
    COV_Compiled <- cbind(COV_Compiled, RD_count_variable)
    colnames(COV_Compiled)[ncol(COV_Compiled)] <- colnames(historical_ward_1911)[cc]
  }
}

# Add translated variables to basic COV dataframe
COV$Smallpox_cases <- rep(c(COV_Compiled$Smallpox_cases), times=7)
COV$Smallpox_case_rate <- rep(c(COV_Compiled$Smallpox_case_rate), times=7)
COV$Smallpox_deaths <- rep(c(COV_Compiled$Smallpox_deaths), times=7)
COV$Smallpox_death_rate <- rep(c(COV_Compiled$Smallpox_death_rate), times=7)
COV$Measles_deaths <- rep(c(COV_Compiled$Measles_deaths), times=7)
COV$Measles_death_rate <- rep(c(COV_Compiled$Measles_death_rate), times=7)
COV$Perc_Irish_Born_1901 <- rep(c(COV_Compiled$Perc_Irish_Born_1901), times=7)

# Add Year_Numeric
COV$Year_Numeric <- COV$Year - 1906

# Compute distance to smallpox hospital
RDs <- readOGR(dsn = path.expand("shp/Registration_Districts_Shp/Registration_Districts.shp"), layer = "Registration_Districts")
#RD <- sf::st_read("../COV-Files/shp/Registration_Districts_Shp/Registration_Districts.shp")
belvidere_smallpox <- readWKT("POINT(262400 663601)", p4s = proj4string(RDs)) # in Dalmarnock, British National Grid coordinates, pulled from Digimap directly
RD_centroids <- gCentroid(RDs, byid = T)
plot(RDs); plot(RD_centroids, add=T, col="blue"); plot(belvidere_smallpox, add=T, col="red")
RDs$Distance_Belvidere_metres <- as.numeric(gDistance(RD_centroids, belvidere_smallpox, byid = T))
RDs$Distance_Belvidere <- RDs$Distance_Belvidere_metres/1000
#RDs$RD_Numeric <- as.numeric(as.character(RDs$RD_Num))
RDs@data

# Join distances back into main dataset
COV <- left_join(COV, RDs@data, by=c("Registration_district_name"="Name"))

#str(All_COV_RDs_rev)
final_vars <- c("Year", "Reg_district_number", "Registration_district_name", "Year_Numeric",
                "RD_Number_of_children_born", "RD_Successfully_vaccinated_count", "RD_Died_before_vaccination_count",
                "RD_Removed_otherwise_unaccounted_count", "RD_COV_count", "RU_surv_perc", "COV_surv_perc",
                "COV_RU_surv_perc", "Illeg_births_count",
                "Illeg_births_perc_all_births", "Population_density_1911", "Average_rooms_1911",
                "Smallpox_cases", "Smallpox_case_rate",
                "Smallpox_deaths", "Smallpox_death_rate", "Measles_deaths","Measles_death_rate", "Perc_Irish_Born_1901", "Distance_Belvidere")
# Create data dictionary
data_dictionary <- data.frame(name = final_vars,
                              display_name = c("Year", "Reg_district_number", "Registration_district_name", "Year_Numeric",
                              "RD_Number_of_children_born", "RD_Successfully_vaccinated_count", "RD_Died_before_vaccination_count",
                              "RD_Removed_otherwise_unaccounted_count", "RD_COV_count", "RU_surv_perc", "COV_surv_perc",
                              "COV_RU_surv_perc", "Illeg_births_count",
                              "Illeg_births_perc_all_births", "Population_density_1911", "Average_rooms_1911",
                              "Smallpox_cases", "Smallpox_case_rate",
                              "Smallpox_deaths", "Smallpox_death_rate", "Measles_deaths","Measles_death_rate", 
                              "Perc_Irish_Born_1901" , "Distance_Belvidere"),
                              description = c("Year","Registration District number","Registration District name" , "Count of years, starting in 1907", "Number of children born",
                                              "Number of children successfully vaccinated", "Number of children who died before vaccination",
                                              "Number of children removed from the district of otherwise unaccounted for before vaccination",
                                              "Conscientious objections to vaccination count",
                                              "Number of children removed from the district of otherwise unaccounted for as a proportion of children surviving",
                                              "Conscientious objections to vaccination as a proportion of children surviving",
                                              "Number of children removed from the district of otherwise unaccounted for plus conscientious objections to vaccination as a proportion of children surviving",
                                              "Number of illegitimate children born", "Number of illegitimate births as a percentage of all births",
                                              "Population density (persons per acre) in 1911",
                                              "Average number of rooms in occupied dwellings in 1911",
                                              "Total smallpox cases over the two outbreak periods",
                                              "Smallpox cases per 1000 population over the two outbreak periods",
                                              "Total smallpox deaths over the two outbreak periods",
                                              "Smallpox deaths per 1000 population over the two outbreak periods",
                                              "Total measles deaths",
                                              "Measles deaths per 1000",
                                              "Percentage of the population born in Ireland in 1901",
                                              "Distance from Belvidere Smallpox Hospital"))

data_dictionary <- rbind(data_dictionary, data.frame(name = c("(Intercept)","Year_Numeric:Measles_case_rate"),
                                                     display_name = c("(Intercept)","Year x Measles cases"),
                                                     description = c("")))

sheets <- list("RD_All" = COV[final_vars], "Data_Dictionary_RD" = data_dictionary) #assumes sheet1 and sheet2 are data frames
write_xlsx(sheets, paste0("Output_data/COV_Final_RDs.xlsx"))



