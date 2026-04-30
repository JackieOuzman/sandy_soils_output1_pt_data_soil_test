
library(dplyr)
library(readr)
library(readxl)
library(tidyverse)


################################################################################
########################            Define the directory              ##########
################################################################################





### this site has two sets of analysis results
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


### import data data set 1 ###########################################################

df1 <- read.csv(paste0(headDir, soils_folder, compiled_folder, file))

### import data data set 1 ###########################################################



df_2 <- read_excel(path = paste0(headDir, "/6.Soil_Data/1.Baseline/RawData/WYN_TAN_baseline_soils_GM.xlsx"))
str(df_2)
df2_edited <- df_2 %>%
  mutate(
    Site = paste(Site, Paddock, sep = "_"),
          SamplingDate = paste(
          sprintf("%02d", as.integer(gsub("D", "", Day))),
          sprintf("%02d", as.integer(gsub("M", "", Month))),
          paste0("20", gsub("YR", "", Year)),
          sep = "-"
        )
        ) %>%
  rename(
    SampleNameShort = `Plot/Sampling point`,
    SampleDepth = Depth,
    TMs.002B1_Gravimetric.moisture..as.received._..of.dry.wt2 = `GSM (%)`

  )
df2_edited <- df2_edited %>% select(Site,
                                SampleNameShort,
                                SamplingDate,
                                SampleDepth,
                                TMs.002B1_Gravimetric.moisture..as.received._..of.dry.wt2)
                                


####df2_edited has all the depths I want this to be the base

df2_edited <- left_join(df2_edited, df1)
df2_edited <- df2_edited %>%
  relocate(TMs.002B1_Gravimetric.moisture..as.received._..of.dry.wt2,
           .after = TMs.002B1_Gravimetric.moisture..as.received._..of.dry.wt)

df2_edited <- df2_edited %>%
  mutate(
    TMs.002B1_Gravimetric.moisture..as.received._..of.dry.wt = coalesce(
      TMs.002B1_Gravimetric.moisture..as.received._..of.dry.wt,
      TMs.002B1_Gravimetric.moisture..as.received._..of.dry.wt2
    )
  )

df2_edited <- df2_edited %>% select(- TMs.002B1_Gravimetric.moisture..as.received._..of.dry.wt2)
  
df <- df2_edited
## replace less than values ###########################################################
df <- df %>%
  mutate(across(contains("TMs"), ~ ifelse(str_detect(as.character(.), "^<"), 0, .)))


## add a clm for profile depth - 
#careful here am I reporting the depth that is recorded in the datasheet or the depth we have results for

###

df <- df %>%
  filter(!is.na(TMs.002B1_Gravimetric.moisture..as.received._..of.dry.wt))


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
