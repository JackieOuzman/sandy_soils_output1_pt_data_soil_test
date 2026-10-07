## Importing and formatting APAL data

library(dplyr)
library(readr)
library(readxl)
library(tidyverse)


################################################################################
########################            Define the directory              ##########
################################################################################

site_number_input <- 8  # <-- change this number only (1-8)   

site_lookup <- data.frame(                                     # 
  id = 1:8,                                                    # 
  site_number = c(                                             # 
    "1.Walpeup_MRS125",                                        # 
    "2.Crystal_Brook_Brians_House",                            # 
    "3.Wynarka_Mervs_West",                                    # 
    "4.Wharminda_Woodys",                                      # 
    "5.Walpeup_Gums",                                          # 
    "6.Crystal_Brook_Randals",                                 # 
    "7.Wharminda_Bonanza",                                     # 
    "8.Wynarka_Tanks"                                          # 
  ),                                                           # 
  site_name = c(                                               # 
    "Walpeup_MRS125",                                          # 
    "Crystal_Brook_Brians_House",                              # 
    "Wynarka_Mervs_West",                                      # 
    "Wharminda_Woodys",                                        # 
    "Walpeup_Gums",                                            # 
    "Crystal_Brook_Randals",                                   # 
    "Wharminda_Bonanza",                                       # 
    "Wynarka_Tanks"                                            # 
  )                                                            # 
)                                                              # 

site_row    <- site_lookup[site_lookup$id == site_number_input, ]   # 
site_number <- site_row$site_number                                 # 
site_name   <- site_row$site_name                                   # 


dir     <- "//fs1-cbr.nexus.csiro.au/{af-sandysoils-ii}"
headDir <- paste0(dir, "/work/Output-1/", site_number)

soils_folder <- "/6.Soil_Data"
compiled_folder <- "/Compiled_Data/"


file <- case_when(
  site_number == "1.Walpeup_MRS125"             ~ "MRS125_2026_ presowing tests_2026-04-14_reformat.csv",
  site_number == "2.Crystal_Brook_Brians_House" ~ "CRY_BHO_pH_EC_N_2026_reformat.csv",
  site_number == "3.Wynarka_Mervs_West"         ~ "Batch-49912-49911-49913-49914-Grdc-Sandy-Soils-Ii-Wynarka-Mer-Sba4-Sba1-Ds1-Data-Only-Samples-In-Rows-2026-04-21_reformat.csv",
  site_number == "4.Wharminda_Woodys"           ~ "Batch-Grdc-Sandy-Soils-Ii-Client-Sba4-Sba1-Ds1-WAH_WOD-2026-04-10_reformat.csv",
  site_number == "5.Walpeup_Gums"               ~ "WHA_GUM_Pre-sow soils-2026-04-14_reformat.csv",
  site_number == "6.Crystal_Brook_Randals"      ~ "Batch-50000-49999-50001-50002-Grdc-Sandy-Soils-Ii-Crystal-Brook-Ran-Sba4-Sba1-Ds1-Data-Only-Samples-In-Rows-2026-04-27_reformat.csv",
  site_number == "7.Wharminda_Bonanza"          ~ "Grdc-Sandy-Soils-Ii-Client-Sba4-Sba1-Phec-WHA_BON_2026-04-10_reformat.csv",
  site_number == "8.Wynarka_Tanks"              ~ "Batch-49908-49912-49911-49909-Grdc-Sandy-Soils-Ii-Wynarka-Tan-Sba4-Sba1-Phec-Data-Only-Samples-In-Rows-2026-04-21_reformat.csv",
  TRUE                                          ~ NA_character_
)

if (is.na(file)) stop(paste("Unknown site_number:", site_number))

# #Walpeup_MRS125
# # file           <- "MRS125_2026_ presowing tests_2026-04-14.xlsx"
# #Brians_House
# #file           <- "CRY_BHO_pH_EC_N_2026_reformat.csv"
# #Wood
# #file           <- "Grdc-Sandy-Soils_WHA_WOD_min N_2026-04-08_reformat.csv"
# #TAN
# #file           <- "Batch-49908-49912-49911-49909-Grdc-Sandy-Soils-Ii-Wynarka-Tan-Sba4-Sba1-Phec-Data-Only-Samples-In-Rows-2026-04-21_reformat.csv"
# #RAN
# #file           <- "Batch-50000-49999-50001-50002-Grdc-Sandy-Soils-Ii-Crystal-Brook-Ran-Sba4-Sba1-Ds1-Data-Only-Samples-In-Rows-2026-04-27_reformat.csv"
# #Gum
# file           <- "WHA_GUM_Pre-sow soils-2026-04-14_reformat.csv"
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
#careful here am I reporting the depth that is recorded in the datasheet or the depth we have results for
df <- df %>%
  filter(!is.na(TMs.007NO3_Nitrate...N..2M.KCl._mg.kg))

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
names(df)
Check <- df %>% select(Site:DepthCm, Nitrate_kg_ha, Ammonium_kg_ha , 
                       TMs.007NO3_Nitrate...N..2M.KCl._mg.kg,
                       TMs.007NH4_Ammonium...N..2M.KCl._mg.kg)
##what the go with the zero values?All checkes out.. if it was coded <1 it became a zero
################################################################################
### Summary data #############################################################
# ---- NEW from here: save the layer-level N results ----
layers_N <- df %>%
  transmute(
    SampleNameShort, Site, SamplingDate, SampleDepth,
    DepthUpper, DepthLower, DepthCm,
    Nitrate_mg_kg  = as.numeric(.data[[nitrate_col]]),
    Ammonium_mg_kg = as.numeric(.data[[ammonium_col]]),
    Nitrate_kg_ha, Ammonium_kg_ha,
    MinN_kg_ha     = rowSums(across(c(Nitrate_kg_ha, Ammonium_kg_ha)), na.rm = TRUE),
    bulk_density   = bulk_density
  ) %>%
  filter(!is.na(SampleNameShort) & SampleNameShort != "")

write.csv(layers_N,
          paste0(headDir, soils_folder, "/Compiled_Data/", "Mineral N layers", year_sampling, ".csv"),
          row.names = FALSE)




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

df_summary <- df_summary %>% filter(!is.na(SampleNameShort) & SampleNameShort != "")

###############################################################################


out_file <- paste0(headDir, soils_folder, "/Compiled_Data/", 
                   paste0(sampling_timing, soil_test, year_sampling), 
                   ".csv")

write.csv(df_summary, out_file, row.names = FALSE)

