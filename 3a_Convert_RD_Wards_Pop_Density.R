# This script constructs a matrix of the proportion contribution of each 
# ward to each registration district (RD). It uses the polygons based on the 
# intersection between RD and ward from 2a_Generate_Polygon_Densities_RD.R
# and the pixel density values (corresponding to building density) computed therein.
# In this file, we further weight building densities by the proportion of the
# premises that are residential (inhabited houses).

rm(list = ls())
getwd()

library(sf)
library(tidyverse)
library(writexl)
library(readxl) # For reading in data from Excel
#library(dplyr) # for SQL-like joins

# Load up sf object containing population density information
Joint_RD_Wards <- readRDS(file = "Output_data/Joint_RD_Wards_Densities_Full_City_extraPoliesV3.rda")

# Exclude Shettleston not in our RD data
Joint_RD_Wards <- subset(Joint_RD_Wards, RD_Name != "Shettleston")

# ------ Construct weighting information ------

# Weight the darkness of polygons (from polygon density calculations) by proportion of 
#  buildings that were commercial properties to account for non-domestic buildings
Wt_Data <- read_excel("raw_data/COV_Glasgow_Data_Final.xlsx", sheet = "Weightings_1903")

# Left join the polygon-based data with the weighting data to add weights
convert <- left_join(x = tibble(Joint_RD_Wards), y = Wt_Data, 
                     by = c("Ward_Num" = "Municipal_Ward_Number")) %>%
  dplyr::select(Ward_Num, Ward_Name, RD_ID, RD_Name, Poly_Num, Poly_Dens, 
                Workshops_1903, Inhabited_houses_1903)
# Add RD_Num as in 1907 data
RD_numbers <- read_excel(path = "../COV-Files/raw_data/COV_Glasgow_Data_Final.xlsx", sheet = "RD_MOH_Numbering")
convert <- left_join(x = convert, y = RD_numbers, 
                      by = c("RD_Name" = "RD_Name"))
head(convert)
tail(convert)

# Compute residential weighting for each polygon
convert$Weighting <- convert$Inhabited_houses_1903/(convert$Inhabited_houses_1903 + convert$Workshops_1903)
convert$Wtd_Poly <- convert$Poly_Dens * convert$Weighting

# Convert into matrix form with Cols:
#    Registration_district_number	Registration_district_name, Bridgeton	Camlachie	Calton	Dennistoun	Garngadhill	Springburn	Possilpark	
# St Rollox	Milton	Blythswood	Anderston	Hillhead	Kelvin	Maryhill	Hutchesontown	Govanhill	Gorbals	Pollokshields	Cathcart	Eastwood
# Use hand-entered data from Matching_Matrix only to provide row and column names
conv_info <- read_excel(path = "../COV-Files/raw_data/COV_Glasgow_Data_Final.xlsx", sheet = "Matching_Matrix_RD")
## NOTE Spelling of ward names has been amended to match Polygons file
col_headers <- colnames(conv_info)[3:ncol(conv_info)]
row_names <- conv_info$RD_Number
rm(conv_info)

my_matrix <- matrix(0L, nrow = length(row_names), ncol = length(col_headers))
# Loop over RDs and compute pop density that came in from different wards

for(w in 1:length(row_names)) {
  ww <- row_names[w]
  print(paste0("RD number: ", ww))
  corresp <- convert %>% filter(RD_Num==ww) %>% dplyr::select(RD_Num, Ward_Name, Wtd_Poly)
  col_ids <- data.frame(corresp, col_ids=match(as.character(corresp$Ward_Name), col_headers))
  for (i in 1:nrow(col_ids)) {
    my_matrix[w,col_ids$col_ids[i]] <- col_ids$Wtd_Poly[i]
  }
}
my_matrix <- t(my_matrix)
dim(my_matrix) # 36x20 (cols are RDs, rows are wards)
# So an entry tells us how many pixels are shared between the RD and Ward
# Remove rows 26-33, as outwith area covered by ward data, but keep in 34-36 
# as include parts of RD data
my_matrix <- my_matrix[-c(26:33),  ]

dim(my_matrix) # 28x20

# Normalise so that for each ward (column), it sums to 1
#    i.e. so that each column shows the proportion of each ward that came from the corresponding RD
# Do first for 28 columns to include extra wards so can extract births etc outwith our core area
colsums_matr <- t(replicate(nrow(my_matrix), colSums(my_matrix)))
where_RDs_come_from_A <- my_matrix / colsums_matr # element-wise division
colSums(where_RDs_come_from_A) # sum to 1
rowSums(where_RDs_come_from_A) # don't sum to 1

# Normalise so that for each RD (row), it sums to 1
#    i.e. so that each row shows the proportion of each RD that went into the corresponding ward
rowsums_matr <- replicate(ncol(my_matrix), rowSums(my_matrix))
where_wards_go_to_A <- my_matrix / rowsums_matr
rowSums(where_wards_go_to_A) # sum to 1
colSums(where_wards_go_to_A) # don't sum to 1

# Write conversion matrix to file as xlsx
sheets <- list("shared_pixels_RD_wards_GIS_A" = data.frame(my_matrix), # un-normalised
               "where_RDs_come_from_GIS_A" = data.frame(where_RDs_come_from_A), "where_wards_go_to_GIS_A" = data.frame(where_wards_go_to_A))
write_xlsx(sheets, paste0("Output_data/GIS_Conversion_RD_Pixels_Wtd_RDs_A.xlsx"))

# Now remove old rows 34-36 and normalise again, so totals sum to 1 for 25 wards
# This means we can split the ward data out to RDs that cover the same area as the wards
my_matrix <- my_matrix[-c(26:28),  ]

dim(my_matrix) # 25x20
# Normalise so that for each ward (column), it sums to 1
#    i.e. so that each column shows the proportion of each ward that came from the corresponding RD
colsums_matr <- t(replicate(nrow(my_matrix), colSums(my_matrix)))
where_RDs_come_from <- my_matrix / colsums_matr # element-wise division
colSums(where_RDs_come_from) # sum to 1
rowSums(where_RDs_come_from) # don't sum to 1

# Normalise so that for each RD (row), it sums to 1
#    i.e. so that each row shows the proportion of each RD that went into the corresponding ward
rowsums_matr <- replicate(ncol(my_matrix), rowSums(my_matrix))
where_wards_go_to <- my_matrix / rowsums_matr
rowSums(where_wards_go_to) # sum to 1
colSums(where_wards_go_to) # don't sum to 1

# Write conversion matrix to file as xlsx
sheets <- list("shared_pixels_RD_wards_GIS" = data.frame(my_matrix), # un-normalised
               "where_RDs_come_from_GIS" = data.frame(where_RDs_come_from), "where_wards_go_to_GIS" = data.frame(where_wards_go_to))
write_xlsx(sheets, paste0("Output_data/GIS_Conversion_RD_Pixels_Wtd_RDs.xlsx"))
# Write conversion matrix to file as csv
write.csv(my_matrix, 'Output_data/Matrix_shared_pixels_RD_wards_GIS.csv')
write.csv(where_RDs_come_from, 'Output_data/where_RDs_come_from_GIS.csv')
write.csv(where_wards_go_to, 'Output_data/where_wards_go_to_GIS.csv')
