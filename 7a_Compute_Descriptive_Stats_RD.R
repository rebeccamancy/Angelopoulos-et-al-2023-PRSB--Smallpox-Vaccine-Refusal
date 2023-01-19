# Computes descriptive statistics for RD data excluding Blythswood
# Looking at COV as percentage of surviving infants (COV_surv_perc)

rm(list = ls())
library(readxl) # For reading in data from Excel
library(dplyr) # for SQL-like joins and pipe %>%
library(writexl)

# Read in data
COV <- read_excel(path = paste0("Output_data/COV_Main_Dataset.xlsx"),
                  sheet = "RD_Drop_Blyt")
# ---------------------- Descriptive statistics for main text -----------------------

# Overall COV_Prop_Births desc stats for Table 1
mean(COV$COV_surv_perc)
min(COV$COV_surv_perc)
max(COV$COV_surv_perc)
sd(COV$COV_surv_perc)

# Summary of COV rates in 1913
summary(COV %>% filter(Year == 1913) %>% dplyr::select(COV_surv_perc))
# Compute mean and sd in the number of cases of smallpox
mean(unlist(COV %>% filter(Year == 1907) %>% dplyr::select(Smallpox_cases)))
sd(unlist(COV %>% filter(Year == 1907) %>% dplyr::select(Smallpox_cases)))
sd(unlist(COV %>% filter(Year == 1907) %>% dplyr::select(Smallpox_deaths)))
summary(unlist(COV %>% filter(Year == 1907) %>% dplyr::select(Smallpox_cases)))
summary(unlist(COV %>% filter(Year == 1907) %>% dplyr::select(Smallpox_deaths)))
# Mean and sd in rate of COV in 1907
mean(unlist(COV %>% filter(Year == 1907) %>% dplyr::select(COV_surv_perc)))
sd(unlist(COV %>% filter(Year == 1907) %>% dplyr::select(COV_surv_perc)))
mean(unlist(COV %>% dplyr::select(COV_surv_perc))) # Across all RDs/times

sum(COV %>% filter(Year == 1907) %>% dplyr::select(Smallpox_deaths))
summary(COV %>% filter(Year == 1907) %>% dplyr::select(Smallpox_deaths))
# How many fold difference is there in cases per 1000 between RDs?
max(COV %>% filter(Year == 1907) %>% 
      dplyr::select(Smallpox_case_rate)) / min(COV %>% 
                                                 filter(Year == 1907) %>% 
                                                 dplyr::select(Smallpox_case_rate))

# ---------------------- Mean values of COV per year -----------------------

# Used in standard deviation change effect by year for Fig 3
# Note that this is the dependent variable only, so there is no rescaled / standardised version
  Mean_COV_By_Year <- COV %>% 
    group_by(Year) %>%
    summarise(Mean_COV = mean(COV_surv_perc),
              sd_COV = sd(COV_surv_perc),
              min_COV = min(COV_surv_perc),
              max_COV = max(COV_surv_perc))
  
  write_xlsx(Mean_COV_By_Year, paste0("Output_data/DescStat_RD_All_TimeVarying.xlsx"))
  write.csv(Mean_COV_By_Year, file = paste0("Output_data/DescStat_RD_All_TimeVarying.csv"))


# ---------- Descriptive statistics for Supplememtary Materials (table) and figure calculations --------------

# Compute descriptive statistics
COV_1907 <- COV %>% filter(Year == 1907) # So we don't get each of the variables repeated for multiple years
variables <- c("Smallpox_cases", "Smallpox_case_rate",
               "Smallpox_deaths", "Smallpox_death_rate",
               "Measles_death_rate",
               "Perc_Irish_Born_1901", "Average_rooms_1911",
               "Population_density_1911",
               "Distance_Belvidere")
output <- data.frame() # As numeric
output_formatted <- data.frame() # For writing out table (formatted character strings)
for (var in variables) {
  print(var)
  c_min <- min(COV_1907 %>% dplyr::select(one_of(var)))
  c_max <- max(COV_1907 %>% dplyr::select(one_of(var)))
  c_mean <- mean(unlist(COV_1907 %>% dplyr::select(one_of(var))))
  c_sd <- sd(unlist(COV_1907 %>% dplyr::select(one_of(var))))
  output <- rbind(output, data.frame(var = var,
                                     min = c_min,
                                     max = c_max,
                                     mean = c_mean,
                                     sd = c_sd))
  output_formatted <- rbind(output_formatted, data.frame(var = var, 
                                                         min = c_min,
                                                         max = c_max, 
                                                         mean = formatC(signif(c_mean, digits=3), digits=3, format="fg", flag="#"), 
                                                         sd = formatC(signif(c_sd, digits=3), digits=3, format="fg", flag="#")))
}
print(output)
str(output)
str(output_formatted)

# Write out summary statistics to file

write_xlsx(output, path = paste0("Output_data/Desc_Stat_RD_All_TimeInvariant.xlsx"))
write.csv(output, file = paste0("Output_data/Desc_Stat_RD_All_TimeInvariant.csv"))

##For COVRU

# Read in data
COV <- read_excel(path = paste0("Output_data/COV_Main_Dataset.xlsx"),
                  sheet = "RD_Drop_Blyt")
# ---------------------- Descriptive statistics for main text -----------------------

# Overall COV_Prop_Births desc stats for Table 1
mean(COV$COV_RU_surv_perc)
min(COV$COV_RU_surv_perc)
max(COV$COV_RU_surv_perc)
sd(COV$COV_RU_surv_perc)

# Summary of COV rates in 1913
summary(COV %>% filter(Year == 1913) %>% dplyr::select(COV_RU_surv_perc))
# Compute mean and sd in the number of cases of smallpox
mean(unlist(COV %>% filter(Year == 1907) %>% dplyr::select(Smallpox_cases)))
sd(unlist(COV %>% filter(Year == 1907) %>% dplyr::select(Smallpox_cases)))
sd(unlist(COV %>% filter(Year == 1907) %>% dplyr::select(Smallpox_deaths)))
summary(unlist(COV %>% filter(Year == 1907) %>% dplyr::select(Smallpox_cases)))
summary(unlist(COV %>% filter(Year == 1907) %>% dplyr::select(Smallpox_deaths)))
# Mean and sd in rate of COV in 1907
mean(unlist(COV %>% filter(Year == 1907) %>% dplyr::select(COV_RU_surv_perc)))
sd(unlist(COV %>% filter(Year == 1907) %>% dplyr::select(COV_RU_surv_perc)))
mean(unlist(COV %>% dplyr::select(COV_RU_surv_perc))) # Across all RDs/times

# sum(COV %>% filter(Year == 1907) %>% dplyr::select(Smallpox_Deaths_1901_Epidemic))
# sum(COV %>% filter(Year == 1907) %>% dplyr::select(Smallpox_Deaths_1903_1904))
# 
sum(COV %>% filter(Year == 1907) %>% dplyr::select(Smallpox_deaths))
summary(COV %>% filter(Year == 1907) %>% dplyr::select(Smallpox_deaths))
# How many fold difference is there in cases per 1000 between RDs?
max(COV %>% filter(Year == 1907) %>% 
      dplyr::select(Smallpox_case_rate)) / min(COV %>% 
                                                 filter(Year == 1907) %>% 
                                                 dplyr::select(Smallpox_case_rate))

# ---------------------- Mean values of COV per year -----------------------

# Used in standard deviation change effect by year for Fig 3
# Note that this is the dependent variable only, so there is no rescaled / standardised version
Mean_COV_RU_By_Year <- COV %>% 
  group_by(Year) %>%
  summarise(Mean_COV = mean(COV_RU_surv_perc),
            sd_COV = sd(COV_RU_surv_perc),
            min_COV = min(COV_RU_surv_perc),
            max_COV = max(COV_RU_surv_perc))

write_xlsx(Mean_COV_RU_By_Year, paste0("Output_data/DescStat_RD_All_TimeVarying_COVRU.xlsx"))
write.csv(Mean_COV_RU_By_Year, file = paste0("Output_data/DescStat_RD_All_TimeVarying.csv"))

# For RU only
# Used in standard deviation change effect by year for Fig 3
Mean_RU_By_Year <- COV %>% 
  group_by(Year) %>%
  summarise(Mean_COV = mean(RU_surv_perc),
            sd_COV = sd(RU_surv_perc),
            min_COV = min(RU_surv_perc),
            max_COV = max(RU_surv_perc))

write_xlsx(Mean_RU_By_Year, paste0("Output_data/DescStat_RD_All_TimeVarying_RU.xlsx"))
write.csv(Mean_RU_By_Year, file = paste0("Output_data/DescStat_RD_All_TimeVarying.csv"))


