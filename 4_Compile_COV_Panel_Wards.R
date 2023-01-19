# This script compiles raw data from multiple data entry tables into a single file, in panel format.
# Smallpox experience variables, as well as measles and scarlet fever experience variables (for the "placebo" experiment) 
# are recorded by sanitary district, and therefore need to be converted to wards for analysis. 
# All other variables are recorded directly by ward.

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

# Read in conversion method using GIS conversion matrices 
where_wards_come_from <- as.matrix(read_excel(path = "Output_data/SD_Ward_Matrix_Pixels_Wtd.xlsx", sheet = "where_wards_come_from"))
where_sd_go_to <- as.matrix(read_excel(path = "Output_data/SD_Ward_Matrix_Pixels_Wtd.xlsx", sheet = "where_sd_go_to"))
(num_wards <- ncol(where_wards_come_from)) # 25
(num_sd <- nrow(where_wards_come_from)) # 34

# ---- Dependent variable manipulations: Conscientious objection to vaccination data (COV) ----
# Read in counts of COV
COV_counts <- read_excel(path = COV_data_path, sheet = "COV_By_Ward"); tail(COV_counts)
COV_counts <- filter(COV_counts, Municipal_Ward_Number < 26)
# Inspect zeros
filter(COV_counts, COV_1907 == 0 | COV_1908 == 0 | COV_1909 == 0 | COV_1910 == 0 | COV_1911 == 0 | COV_1912 == 0 | COV_1913 == 0)

# Read in births by ward (births_w) and filter
births_w <- read_excel(path = COV_data_path, sheet = "Births_By_Ward"); tail(births_w)
births_w <- filter(births_w, Municipal_Ward_Number < 26)
# Remove full birth count in 1907
births_w <- births_w[c(1:2,4:ncol(births_w))]; dim(births_w); tail(births_w)

dim(COV_counts); dim(births_w)
# Compute conscientious objections as proportion of the births (cbind ward nums/names, and COV_counts/births)
COV <- cbind(COV_counts[,1:2], COV_counts[,3:ncol(COV_counts)] / births_w[,3:ncol(births_w)])

# ---- Convert historical data that exist by Sanitary District to Ward ----

# Read in historical variables by Sanitary District, and correct for the fact that Springburn and Rockvilla are combined in this dataset
historical_SD <- read_excel(path = COV_data_path, sheet = "Vars_By_Sanitary_Dist")
historical_SD <- filter(historical_SD, !is.na(Sanitary_District_Number) & Sanitary_District_Name !="Institutions and Harbour")
# Identify rate variables versus count variables
all_SD_variables <- names(historical_SD)
rate_variables <- c("Smallpox_Cases_Per_Mill_1901_Epidemic","Smallpox_Deaths_Per_Mill_1901_Epidemic",
                    "Measles_Deaths_Mill_1900","Measles_Deaths_Mill_1901",
                    "Scarlet_fever_Deaths_Mill_1900","Scarlet_fever_Deaths_Mill_1901")            
count_variables <- c("Smallpox_Cases_1901_Epidemic","Smallpox_Deaths_1901_Epidemic",
                     "Measles_Deaths_1900", "Measles_Deaths_1901",
                     "Scarlet_fever_Deaths_1900", "Scarlet_fever_Deaths_1901")
# Cross-check we have covered all variables!
setdiff(x = all_SD_variables, y = c(rate_variables, count_variables))
setdiff(y = all_SD_variables, x = c(rate_variables, count_variables))

SD <- sf::st_read("shp/SD_Shp/Sanitary Districts.shp")
# Separate out Springburn and Rockvilla that are aggregated in the data, 
# but not the conversion table.
RV_SP_Areas <- SD %>%
  filter(Num_1912 %in% c("SP","RV")) %>%
  mutate(area = st_area(.) %>% as.numeric()) %>% as_tibble() %>%
  dplyr::select(Num_1912, Name_1912, area)
# Obtain pixel density for RV and SP (so we can allocate cases and deaths according to population)
# Load up sf object containing population density information
Joint_SD_Wards <- read_excel(path = "Output_data/Conversion_Data_Pixels_Wtd.xlsx")
RV_SP_Density <- Joint_SD_Wards %>% 
  filter(Num_1912 %in% c("SP","RV")) %>% 
  dplyr::select(Num_1912, Name_1912, Wtd_Poly)

RV_SP_Density$RV_Density <- ifelse(RV_SP_Density$Name_1912=="Rockvilla", RV_SP_Density$Wtd_Poly,0)
RV_SP_Density$SP_Density <- ifelse(RV_SP_Density$Name_1912=="Springburn", RV_SP_Density$Wtd_Poly,0)
proportions <- RV_SP_Areas %>% 
  filter(Num_1912 %in% c("SP","RV")) %>% 
  dplyr::select(Num_1912, Name_1912)
proportions$Density <- ifelse(proportions$Name_1912 == "Rockvilla", sum(RV_SP_Density$RV_Density),sum(RV_SP_Density$SP_Density))
proportions$proportion_density <- proportions$Density / sum(proportions$Density) # Rockvilla smaller than Springburn

SP <- filter(historical_SD, Sanitary_District_Name=="Springburn and Rockvilla")
SP %>% dplyr::select(Sanitary_District_Number, Sanitary_District_Name, 
                     Smallpox_Cases_1901_Epidemic, Smallpox_Cases_Per_Mill_1901_Epidemic)

# Duplicate so we have a row for each of SP and RV
SP <- rbind(SP, SP)
SP$Sanitary_District_Name <- proportions$Name_1912
SP$Sanitary_District_Number <- proportions$Num_1912
# Loop over columns "cc" (i.e. variables), allocating counts to RV and SP, weighting by density
for (cc in 3:ncol(SP)) {
  if (colnames(SP)[cc] %in% c(count_variables)) {
    SP[,cc] <- round(SP[,cc] * proportions$proportion_density) # weighted average according to relative population density
  } # NB: If it is a rate column, then we assume the same rate in both RV and SP, so no adjustment required
}
SP %>% dplyr::select(Sanitary_District_Number, Sanitary_District_Name, 
                     Smallpox_Cases_1901_Epidemic, Smallpox_Cases_Per_Mill_1901_Epidemic)
# Recombine data for SD except SP with split data for SP into the same location as originally (btw 22 and 23)
historical_SD_Except_SP <- filter(historical_SD, Sanitary_District_Name!="Springburn and Rockvilla")
historical_SD_Corrected <- rbind(historical_SD_Except_SP[1:23,], SP, historical_SD_Except_SP[24:nrow(historical_SD_Except_SP),])

# Carry out conversion of variables recorded by SD into wards to create a renamed tibble COV_Compiled
COV_Compiled <- COV
# Loop over variables, conducting rate conversion or count conversion of variables from SD to wards
for (cc in 3:ncol(historical_SD_Corrected)) { 
  if (names(historical_SD_Corrected[cc]) %in% rate_variables) { # RATES
    print(paste0(names(historical_SD_Corrected[cc]), " so doing rate conversion"))
    curr_variable <- as.numeric(unlist(historical_SD_Corrected[,cc]))
    # Compute average of the SD rates, weighted by ward make-up
    ####curr_rate_variable_matrix <- replicate(num_wards, curr_variable)
    ####convert_rate_matrix <- where_wards_come_from * curr_rate_variable_matrix
    ####ward_rate_variable <- round(colSums(convert_rate_matrix))
    ward_rate_variable <- t(where_wards_come_from) %*% curr_variable
    ####print(ward_rate_variable); print(t(ward_rate_variable_test))
    # Bind new variable with existing dataset
    COV_Compiled <- cbind(COV_Compiled, ward_rate_variable)
    # Rename new column to match SD variable
    colnames(COV_Compiled)[ncol(COV_Compiled)] <- colnames(historical_SD_Corrected)[cc]
  } else if (names(historical_SD_Corrected[cc]) %in% count_variables) { # COUNTS
    print(paste0(names(historical_SD_Corrected[cc]), " so doing count conversion"))
    curr_variable <- as.numeric(unlist(historical_SD_Corrected[,cc]))
    # Spread out the SD counts according to where the SDs went to 
    ####convert_count_matrix <- where_sd_go_to * curr_variable
    ####ward_count_variable <- (colSums(convert_count_matrix))
    ward_count_variable <- as.vector(curr_variable %*% where_sd_go_to)
    print(paste0("Check that SD totals and ward totals match: ", sum(curr_variable), "=", sum(ward_count_variable)))
    # Bind new variable with existing dataset 
    COV_Compiled <- cbind(COV_Compiled, ward_count_variable)
    # Rename new column to match SD variable
    colnames(COV_Compiled)[ncol(COV_Compiled)] <- colnames(historical_SD_Corrected)[cc]
  }
}
COV_Compiled

##################

# ---- Add information already by ward ----
historical_w <- read_excel(path = COV_data_path, sheet = "Vars_By_Ward")
historical_w <- filter(historical_w, Municipal_Ward_Number < 26)
COV_Compiled <- cbind(COV_Compiled, historical_w[,3:ncol(historical_w)])

# ---- Push into panel format ----
my.cols <- colnames(COV_Compiled)[c(1:2, 10:ncol(COV_Compiled))] # ward info and historical variables
COV_panel_format <- melt(COV_Compiled, (my.cols))
# Rename variables
COV_panel_format$Year <- as.numeric(substr(COV_panel_format$variable, start = 5, stop = 9))
COV_panel_format$COV_Proportion_Births <- COV_panel_format$value 
COV_panel_format$variable <- NULL
COV_panel_format$value <- NULL

# Re-order columns
col_order <- c("Municipal_Ward_Number","Municipal_Ward_Name","Year","COV_Proportion_Births",
               "Smallpox_Cases_1901_Epidemic","Smallpox_Cases_1903_Epidemic",
               "Smallpox_Deaths_1901_Epidemic","Smallpox_Deaths_1903_Epidemic",
               "Smallpox_Cases_Per_Mill_1901_Epidemic","Smallpox_Cases_Per_Mill_1903_Epidemic",
               "Smallpox_Deaths_Per_Mill_1901_Epidemic","Smallpox_Deaths_Per_Mill_1903_Epidemic",
               "Measles_Deaths_1900", "Measles_Deaths_1901", "Measles_Deaths_1903", "Measles_Deaths_1904",
               "Measles_Deaths_Mill_1900","Measles_Deaths_Mill_1901","Measles_Deaths_Mill_1903","Measles_Deaths_Mill_1904",
               "Scarlet_fever_Deaths_1900", "Scarlet_fever_Deaths_1901", "Scarlet_fever_Deaths_1903", "Scarlet_fever_Deaths_1904",
               "Scarlet_fever_Deaths_Mill_1900","Scarlet_fever_Deaths_Mill_1901","Scarlet_fever_Deaths_Mill_1903","Scarlet_fever_Deaths_Mill_1904",
               "Persons_Per_Acre_Inc_Institutions_Census_1911","Average_Occ_House_Size_1911",
               "Perc_Irish_Born_1901")
dim(COV_panel_format[col_order])

COV_panel_format <- COV_panel_format[col_order]
# Re-order rows into proper panel format
COV_panel_format <- arrange(COV_panel_format, Municipal_Ward_Number, Municipal_Ward_Name, Year)
# Add in Year Dummies
Year_Dummies <- read_excel(path = COV_data_path, sheet = "Year_Dummies")
COV_panel_format <- cbind(COV_panel_format,Year_Dummies)

# ---- Write out to file ----
sheets <- list("Wide_Format" = COV_Compiled, "Panel_Format" = COV_panel_format) 
write_xlsx(sheets, paste0("Output_data/COV_Compiled_GIS_PixelWtd_Final.xlsx"))
