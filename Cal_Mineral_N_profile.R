## Importing and fromatting APAL data

library(dplyr)
library(readr)
library(readxl)
library(tidyverse)


################################################################################
########################            Define the directory              ##########
################################################################################



# site_number <- "1.Walpeup_MRS125"
# site_name   <- "Walpeup_MRS125"

site_number <- "2.Crystal_Brook_Brians_House"
site_name <- "Crystal_Brook_Brians_House"


dir     <- "//fs1-cbr.nexus.csiro.au/{af-sandysoils-ii}"
headDir <- paste0(dir, "/work/Output-1/", site_number)

soils_folder <- "/6.Soil_Data"
compiled_folder <- "/Compiled_Data/"

#Walpeup_MRS125
# file           <- "MRS125_2026_ presowing tests_2026-04-14.xlsx"
#Brians_House
file           <- "CRY_BHO_pH_EC_N_2026_reformat.csv"
################################################################################

sampling_timing <- "Pre_Season" 
year_sampling <- 26
soil_test <- "Mineral N profile"


### import data ###########################################################

df <- read.csv(paste0(headDir, soils_folder, compiled_folder, file))

names(df)

## replace less than values ###########################################################
df <- df %>%
  mutate(across(contains("TMs"), ~ ifelse(str_detect(as.character(.), "^<"), 0, .)))


## add a clm for profile depth

df <- df %>%
  mutate(
    DepthUpper = as.numeric(str_extract(SampleDepth, "^\\d+")),
    DepthLower = as.numeric(str_extract(SampleDepth, "\\d+$")),
    DepthCm    = DepthLower - DepthUpper
  )
df <- df %>%
  relocate(DepthUpper, DepthLower, DepthCm, .after = SampleDepth)


## convert to kg/ha ###########################################################
names(df)
# "TMs.007NO3_Nitrate...N..2M.KCl._mg.kg"
# "TMs.007NH4_Ammonium...N..2M.KCl._mg.kg"

# mg/kg*bulk_density*(DepthCm/10) 


bulk_density <- 1.3

nitrate_col <- names(df)[str_detect(names(df), "Nitrate") & str_detect(names(df), "mg.kg")]
ammonium_col <- names(df)[str_detect(names(df), "Ammonium") & str_detect(names(df), "mg.kg")]

df <- df %>%
  mutate(
    Nitrate_kg_ha = as.numeric(.data[[nitrate_col]]) * bulk_density * (DepthCm / 10)
  )

df <- df %>%
  mutate(
    Ammonium_kg_ha = as.numeric(.data[[ammonium_col]]) * bulk_density * (DepthCm / 10)
  )


################################################################################
### Summary data #############################################################


df_summary <- df %>%
  group_by(SampleNameShort) %>%
  summarise(
    Site               = first(Site),
    SamplingDate       = first(SamplingDate),
    FilePath           = first(FilePath),
    Nitrate_kg_ha_sum  = sum(Nitrate_kg_ha, na.rm = TRUE),
    Ammonium_kg_ha_sum = sum(Ammonium_kg_ha, na.rm = TRUE),
    DepthUpper_min     = min(DepthUpper, na.rm = TRUE),
    DepthLower_max     = max(DepthLower, na.rm = TRUE)
  ) %>%
  mutate(
    MinN_kg_ha    = Nitrate_kg_ha_sum + Ammonium_kg_ha_sum,
    ProfileDepth  = paste0(DepthUpper_min, "-", DepthLower_max)
  )


df_summary <- df_summary %>%
  select(-c(DepthUpper_min, DepthLower_max, Nitrate_kg_ha_sum, Ammonium_kg_ha_sum))

###############################################################################


out_file <- paste0(headDir, soils_folder, "/Compiled_Data/", 
                   paste0(sampling_timing, soil_test, year_sampling), 
                   ".csv")

write.csv(df_summary, out_file, row.names = FALSE)
