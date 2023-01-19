# This script constructs a matrix of the proportion contribution of each 
# sanitary district (SD) to each ward. It uses the polygons based on the 
# intersection between SD and ward from 2_Generate_Polygon_Densities_Wards.R
# and the pixel density values (corresponding to building density) computed therein.
# In this file, we further weight building densities by the proportion of the
# premises that are residential (inhabited houses).

rm(list = ls())
getwd()

library(sf)
library(tidyverse)
library(writexl)
library(readxl) # For reading in data from Excel

# Load up sf object containing population density information
Joint_SD_Wards <- readRDS(file = "Output_data/Joint_SD_Wards_Densities_Full_CityV3.rda")

# ----- Construct weighting information -----

# Weight the darkness of polygons (from polygon density calculations) by proportion of 
#  premises that were commercial versus residential, to account for non-domestic buildings
Wt_Data <- read_excel("raw_data/COV_Glasgow_Data_Final.xlsx", sheet = "Weightings_1903")

# Left join the polygon-based data with the weighting data to add weights
convert <- left_join(x = tibble(Joint_SD_Wards), y = Wt_Data, 
                     by = c("Ward_Num" = "Municipal_Ward_Number")) %>%
  dplyr::select(id.1, Ward_Num, Ward_Name, SD_ID, Num_1912, Name_1912, Poly_Num, Poly_Dens, 
                Workshops_1903, Inhabited_houses_1903)
head(convert)
tail(convert)

# Compute residential weighting for each polygon
convert$Weighting <- convert$Inhabited_houses_1903/(convert$Inhabited_houses_1903 + convert$Workshops_1903)
# Weight densities by proportions of building density that is residential
convert$Wtd_Poly <- convert$Poly_Dens * convert$Weighting

# Output file for use to split RV and SP data in 4_Compile_COV_Panel_Wards
write_xlsx(convert, paste0("Output_data/Conversion_Data_Pixels_Wtd.xlsx"))

# Convert into matrix form with Cols:
#    Municipal_Ward_Number	Municipal_Ward_Name, BL	1	2	3	4	5	6	7	8	9	10	11	12	13	14	15	16	17	18	19	20 21 22	RV SP 23	24	25	26	27	28	29	30	31
# Use hand-entered data from Matching_Matrix only to provide row and column names
conv_info <- read_excel(path = "raw_data/COV_Glasgow_Data_Final.xlsx", sheet = "Matching_Matrix") 
col_headers <- colnames(conv_info)[3:ncol(conv_info)]
row_names <- conv_info$Municipal_Ward_Number
rm(conv_info)

my_matrix <- matrix(0L, nrow = length(row_names), ncol = length(col_headers))
# Loop over wards and compute relative population that came in from different SDs
for(w in 1:length(row_names)) {
  ww <- row_names[w]
  print(paste0("Ward number: ", ww))
  corresp <- convert %>% filter(Ward_Num==ww) %>% dplyr::select(Ward_Num, Num_1912, Wtd_Poly)
  col_ids <- data.frame(corresp, col_ids=match(as.character(corresp$Num_1912), col_headers))
  for (i in 1:nrow(col_ids)) {
    my_matrix[w,col_ids$col_ids[i]] <- col_ids$Wtd_Poly[i]
  }
}
my_matrix <- t(my_matrix)
dim(my_matrix) # 34x26 (cols are Wards, rows are Sanitary Districts)
# So an entry tells us how many (pixels*residential weighting) are shared between the SD and Ward
# Remove the final column, because Ward 36 (Kinning Park) was not part of Glasgow until 1905
my_matrix <- my_matrix[, 1:(ncol(my_matrix)-1)]
dim(my_matrix) # 34x25

# Normalise so that for each ward (column), it sums to 1
#    i.e. so that each column shows the proportion of each ward that came from the corresponding SD
colsums_matr <- t(replicate(nrow(my_matrix), colSums(my_matrix)))
where_wards_come_from <- my_matrix / colsums_matr # element-wise division
colSums(where_wards_come_from) # sum to 1
rowSums(where_wards_come_from) # don't sum to 1

# Normalise so that for each SD (row), it sums to 1
#    i.e. so that each row shows the proportion of each SD that went into the corresponding ward
rowsums_matr <- replicate(ncol(my_matrix), rowSums(my_matrix))
where_sd_go_to <- my_matrix / rowsums_matr
rowSums(where_sd_go_to) # sum to 1
colSums(where_sd_go_to) # don't sum to 1

# Write conversion matrix to file as xlsx
sheets <- list("SD_Ward_Shared_Pixels_Wtd" = data.frame(my_matrix), # un-normalised
               "where_wards_come_from" = data.frame(where_wards_come_from), "where_sd_go_to" = data.frame(where_sd_go_to))
write_xlsx(sheets, paste0("Output_data/SD_Ward_Matrix_Pixels_Wtd.xlsx"))

# Write conversion matrix to file as csv
write.csv(my_matrix, 'Output_data/Matrix_shared_pixels_SD_wards_GIS.csv')
write.csv(where_wards_come_from, 'Output_data/Matrix_where_wards_come_from.csv')
write.csv(where_sd_go_to, 'Output_data/Matrix_where_sd_go_to.csv')
