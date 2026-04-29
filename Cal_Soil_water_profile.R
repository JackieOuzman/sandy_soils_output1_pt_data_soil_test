## Importing and formatting APAL data

library(dplyr)
library(readr)
library(readxl)
library(tidyverse)


################################################################################
########################            Define the directory              ##########
################################################################################



# site_number <- "1.Walpeup_MRS125"
# site_name <- "Walpeup_MRS125"

# site_number <- "2.Crystal_Brook_Brians_House"
# site_name <- "Crystal_Brook_Brians_House"

# site_number <- "4.Wharminda_Woodys"
# site_name <- "Wharminda_Woodys"

# site_number <-  "5.Walpeup_Gums"
# site_name   <-  "Walpeup_Gums"

# site_number <-  "6.Crystal_Brook_Randals"
# site_name   <-  "Crystal_Brook_Randals"

#### NEW Sites ######

# site_number <- "7.Wharminda_Bonanza"
# site_name <- "Wharminda_Bonanza"

site_number <- "8.Wynarka_Tanks"
site_name <- "Wynarka_Tanks"








dir     <- "//fs1-cbr.nexus.csiro.au/{af-sandysoils-ii}"
headDir <- paste0(dir, "/work/Output-1/", site_number)

soils_folder <- "/6.Soil_Data"
compiled_folder <- "/Compiled_Data/"

file <- case_when(
  site_number == "1.Walpeup_MRS125"             ~ "MRS125_2026_ presowing tests_2026-04-14_reformat.csv",
  site_number == "2.Crystal_Brook_Brians_House" ~ "CRY_BHO_pH_EC_N_2026_reformat.csv",
  site_number == "3.Wynarka_Mervs_West"         ~ "",
  site_number == "4.Wharminda_Woodys"           ~ "Batch-Grdc-Sandy-Soils-Ii-Client-Sba4-Sba1-Ds1-WAH_WOD-2026-04-10_reformat.csv",
  site_number == "5.Walpeup_Gums"               ~ "WHA_GUM_Pre-sow soils-2026-04-14_reformat.csv",
  site_number == "6.Crystal_Brook_Randals"      ~ "Batch-50000-49999-50001-50002-Grdc-Sandy-Soils-Ii-Crystal-Brook-Ran-Sba4-Sba1-Ds1-Data-Only-Samples-In-Rows-2026-04-27_reformat.csv",
  site_number == "7.Wharminda_Bonanza"          ~ "Grdc-Sandy-Soils-Ii-Client-Sba4-Sba1-Phec-WHA_BON_2026-04-10_reformat.csv",
  site_number == "8.Wynarka_Tanks"              ~ "Batch-49908-49912-49911-49909-Grdc-Sandy-Soils-Ii-Wynarka-Tan-Sba4-Sba1-Phec-Data-Only-Samples-In-Rows-2026-04-21_reformat.csv",
  TRUE                                          ~ NA_character_
)

if (is.na(file)) stop(paste("Unknown site_number:", site_number))

################################################################################

sampling_timing <- "Pre_Season" 
year_sampling <- 26
soil_test <- "Soil water profile"


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
#"TMs.002B1_Gravimetric.moisture..as.received._..of.wet.wt"
#"TMs.002B1_Gravimetric.moisture..as.received._..of.dry.wt"

# mg/kg*bulk_density*(DepthCm/10) 


bulk_density <- 1.3

Gravimetric_moist_dry_col <- names(df)[str_detect(names(df), "Gravimetric.moisture") & str_detect(names(df), "dry.wt")]


df <- df %>%
  mutate(
    Soil_water_mm = as.numeric(.data[[Gravimetric_moist_dry_col]]) /100* bulk_density * DepthCm * 10)
  



################################################################################
### Summary data #############################################################


df_summary <- df %>%
  group_by(SampleNameShort) %>%
  summarise(
    Site               = first(Site),
    SamplingDate       = first(SamplingDate),
    FilePath           = first(FilePath),
    Soil_water_mm_profile  = sum(Soil_water_mm, na.rm = TRUE),
    DepthUpper_min     = min(DepthUpper, na.rm = TRUE),
    DepthLower_max     = max(DepthLower, na.rm = TRUE)
  ) %>%
  mutate(
    ProfileDepth  = paste0(DepthUpper_min, "-", DepthLower_max)
  )


df_summary <- df_summary %>%
  select(-c(DepthUpper_min, DepthLower_max))

df_summary <- df_summary %>% filter(!is.na(SampleNameShort) & SampleNameShort != "")

###############################################################################


out_file <- paste0(headDir, soils_folder, "/Compiled_Data/", 
                   paste0(sampling_timing, soil_test, year_sampling), 
                   ".csv")

write.csv(df_summary, out_file, row.names = FALSE)
