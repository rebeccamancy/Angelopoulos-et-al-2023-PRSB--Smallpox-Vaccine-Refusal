# This scripts combine data at ward and RD level into one file COV_Main_Dataset.xlsx
# with various worksheets:
# a.	Data dictionary
# b.	Ward_All
# c.	W_Drop_Blyt_Exch
# d.	RD_All
# e.	RD_Drop_Blyt

rm(list = ls())
library(readxl) # For reading in data from Excel
library(writexl)
library(tidyverse)
library(dplyr)

Ward_All <- read_excel(path = paste0("Output_data/COV_Ward_Final.xlsx"),
                         sheet = "Wards_All")
Data_Dictionary_Wards <- read_excel(path = paste0("Output_data/COV_Ward_Final.xlsx"),
                                 sheet = "Data_Dictionary_Wards")
RD_All <- read_excel(path = paste0("Output_data/COV_Final_RDs.xlsx"),
                       sheet = "RD_All")
Data_Dictionary_RD <- read_excel(path = paste0("Output_data/COV_Final_RDs.xlsx"),
                       sheet = "Data_Dictionary_RD")

# Create W_Drop_Blyt_Exch
W_Drop_Blyt_Exch <- subset(Ward_All, Municipal_Ward_Number != "10" & Municipal_Ward_Number != "11")

# Create RD_Drop_Blyt
RD_Drop_Blyt <- subset(RD_All, Registration_district_name != "Blythswood")

# Combine data dictionaries
Data_dictionary <- rbind(Data_Dictionary_Wards, Data_Dictionary_RD)

# Create main data file COV_Main_Dataset.xlsx
sheets <- list("Data_Dictionary" = Data_dictionary, "Ward_All" = Ward_All,"W_Drop_Blyt_Exch" = W_Drop_Blyt_Exch,
               "RD_All" = RD_All, "RD_Drop_Blyt" = RD_Drop_Blyt ) #assumes sheets are data frames
write_xlsx(sheets, paste0("Output_data/COV_Main_Dataset.xlsx"))
write.csv(Ward_All,"Output_data/Ward_All.csv")
write.csv(RD_All,"Output_data/RD_All.csv")
write.csv(W_Drop_Blyt_Exch,"Output_data/W_Drop_Blyt_Exch.csv")
write.csv(RD_Drop_Blyt,"Output_data/RD_Drop_Blyt.csv")
write.csv(Data_dictionary,"Output_data/Data_Dictionary.csv")

