## Importing and fromatting APAL data

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

site_number <- "7.Wharminda_Bonanza"
site_name <- "Wharminda_Bonanza"


dir <- "//fs1-cbr.nexus.csiro.au/{af-sandysoils-ii}"
headDir <- paste0(dir, "/work/Output-1/", site_number)

soils_folder  <- "/6.Soil_Data"
#subfolder     <- "/4.26/RawData/"
subfolder     <- "/1.Baseline/RawData/"

#Walpeup_MRS125
# file           <- "MRS125_2026_ presowing tests_2026-04-14.xlsx"
#Brians_House
#file           <- "CRY_BHO_pH_EC_N_2026.xlsx"
#Wood 
#file           <- "Grdc-Sandy-Soils_WHA_WOD_min N_2026-04-08.xlsx"
#BON
file          <- "Batch_49475_2026_04_08.xlsx"

worksheet      <- "Data"



### import data ###########################################################

df <- read_excel(path = paste0(headDir, soils_folder, subfolder, file),
                 sheet = worksheet,
                 skip = 7, col_names = FALSE)


### get heading names ###########################################################
# Combine them into one header, ignoring NAs
header <- apply(df[1:3, ], 2, function(x) {
  paste(na.omit(x[x != ""]), collapse = "_")
})


# Set as column names and remove the header rows
colnames(df) <- header
df <- df[-(1:3), ]

# Reset row numbers
rownames(df) <- NULL


### subset data ################################################################

names(df)

# Find the position of "Transect GPS Finish"
gps_pos <- which(colnames(df) == "Transect GPS Finish")

# Get all column names after it
after_gps <- colnames(df)[(gps_pos + 1):ncol(df)]

# Subset
df_subset <- df %>%
  select("SampleName", "Barcode", "SampleDepth", all_of(after_gps))

### get the name clms sorted
df_subset <- df_subset %>%
  mutate(
    split           = str_split(SampleName, "_"),
    Site            = map_chr(split, ~ paste(.x[3], .x[4], sep = "_")),
    SampleNameShort = map_chr(split, ~ .x[5]),
    SamplingDate    = str_extract(SampleName, "D\\d+_M\\d+_YR\\d+")
  ) %>%
  select(-split)

df_subset <- df_subset %>%
  relocate(Site, SampleNameShort, SamplingDate, .before = 1)

df_subset <- df_subset %>%
  mutate(
    SamplingDate = as.Date(
      str_replace_all(SamplingDate, "D(\\d+)_M(\\d+)_YR(\\d+)", "20\\3-\\2-\\1"),
      format = "%Y-%m-%d"
    ),
    SamplingDate = format(SamplingDate, "%d-%m-%Y")
  )


df_subset <- df_subset %>%
  mutate(FilePath = paste0(headDir, soils_folder, subfolder, file))

df_subset <- df_subset %>% select(- `Min N_kg/ha`, - "Nitrate - N (2M KCl)_kg/ha" , -"Ammonium - N (2M KCl)_kg/ha" )
names(df_subset)
##############################################################################

out_folder <- paste0(headDir, soils_folder, "/Compiled_Data/")
dir.create(out_folder, showWarnings = FALSE, recursive = TRUE)

out_file <- paste0(out_folder, tools::file_path_sans_ext(file), "_reformat.csv")

write.csv(df_subset, out_file, row.names = FALSE)
