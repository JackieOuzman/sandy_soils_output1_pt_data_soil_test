## Importing and formatting APAL data

library(dplyr)
library(readr)
library(readxl)
library(tidyverse)
library(sf)

################################################################################
########################            Define the directory              ##########
################################################################################
site_number_input <- 8  # <-- change this number only

site_lookup <- data.frame(
  id = 1:8,
  site_number = c(
    "1.Walpeup_MRS125",
    "2.Crystal_Brook_Brians_House",
    "3.Wynarka_Mervs_West",
    "4.Wharminda_Woodys",
    "5.Walpeup_Gums",
    "6.Crystal_Brook_Randals",
    "7.Wharminda_Bonanza",
    "8.Wynarka_Tanks"
  ),
  site_name = c(
    "Walpeup_MRS125",
    "Crystal_Brook_Brians_House",
    "Wynarka_Mervs_West",
    "Wharminda_Woodys",
    "Walpeup_Gums",
    "Crystal_Brook_Randals",
    "Wharminda_Bonanza",
    "Wynarka_Tanks"
  )
)

site_row        <- site_lookup[site_lookup$id == site_number_input, ]
site_number     <- site_row$site_number
site_name       <- site_row$site_name


dir <- "//fs1-cbr.nexus.csiro.au/{af-sandysoils-ii}"
headDir <- paste0(dir, "/work/Output-1/", site_number)

soils_folder  <- "/6.Soil_Data"
subfolder <- if (site_number_input %in% c(7, 8)) "/1.Baseline/RawData/" else "/4.26/RawData/"


file <- case_when(
  site_number == "1.Walpeup_MRS125"             ~ "MRS125_2026_ presowing tests_2026-04-14.xlsx", #N and water
  site_number == "2.Crystal_Brook_Brians_House" ~ "CRY_BHO_pH_EC_N_2026.xlsx",
  site_number == "3.Wynarka_Mervs_West"         ~ "Batch-49912-49911-49913-49914-Grdc-Sandy-Soils-Ii-Wynarka-Mer-Sba4-Sba1-Ds1-Data-Only-Samples-In-Rows-2026-04-21.xlsx",
  #site_number == "4.Wharminda_Woodys"           ~ "Grdc-Sandy-Soils_WHA_WOD_min N_2026-04-08.xlsx",
  site_number == "4.Wharminda_Woodys"           ~ "Batch-Grdc-Sandy-Soils-Ii-Client-Sba4-Sba1-Ds1-WAH_WOD-2026-04-10.xlsx",### this seems more complete
  
  site_number == "5.Walpeup_Gums"               ~ "WHA_GUM_Pre-sow soils-2026-04-14.xlsx",
  site_number == "6.Crystal_Brook_Randals"      ~ "Batch-50000-49999-50001-50002-Grdc-Sandy-Soils-Ii-Crystal-Brook-Ran-Sba4-Sba1-Ds1-Data-Only-Samples-In-Rows-2026-04-27.xlsx",
  #site_number == "7.Wharminda_Bonanza"          ~ "Batch_49475_2026_04_08.xlsx",#this only has N
  site_number == "7.Wharminda_Bonanza"          ~ "Grdc-Sandy-Soils-Ii-Client-Sba4-Sba1-Phec-WHA_BON_2026-04-10.xlsx",#this one has more
  site_number == "8.Wynarka_Tanks"              ~ "Batch-49908-49912-49911-49909-Grdc-Sandy-Soils-Ii-Wynarka-Tan-Sba4-Sba1-Phec-Data-Only-Samples-In-Rows-2026-04-21.xlsx",
  TRUE                                          ~ NA_character_
)

if (is.na(file)) stop(paste("Unknown site_number:", site_number))

worksheet      <- "Data"


metadata_path <- paste0(dir,"/work/Output-1/0.Site-info/")

metadata_file_name <- "names of treatments per site 2025 metadata and other info.xlsx"

crs_used <- 4326 # Name: WGS 84 (World Geodetic System 1984) Type: Geographic coordinate system (latitude/longitude)
projetion_crs <- 7854 #GDA2020 / MGA Zone 54 (EPSG:7854).

#################################################################################
# --- Paths for shape files ---
#################################################################################
## Note some samples are taken as a baseline other are pre season
sampling_timing <- ifelse(site_number_input <= 6, "Pre_Season", "Baseline")
year_sampling <- 26

sampling_pts_shapefile_source <- readxl::read_excel(
  paste0(metadata_path,metadata_file_name),
  sheet = "Soil_sampling_files_location") %>%
  filter(Site == site_number)  %>%
  filter(variable == paste0(sampling_timing, "_Soil_Sampling_Location_", year_sampling )) %>% 
  pull("file path")
################################################################################
# --- Read for shapefile ---
sampling_pts   <- st_read(paste0(headDir,sampling_pts_shapefile_source))

if (site_number_input == 8) {
  sampling_pts <- sampling_pts %>%
    mutate(field_1 = as.numeric(gsub("TAN", "", Sample)))
}

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


##############################################################################
## SR wants the x and y coord added to the dataframe


#join sampling location to results
str(df_subset)
str(sampling_pts)



join_col <- case_when(
  site_number == "1.Walpeup_MRS125"            ~ "id",
  site_number == "2.Crystal_Brook_Brians_House" ~ "id",
  site_number == "3.Wynarka_Mervs_West"         ~ "id",
  site_number == "4.Wharminda_Woodys"           ~ "pt_ID_Soil",  
  site_number == "5.Walpeup_Gums"              ~ "ID_new",
  site_number == "6.Crystal_Brook_Randals"      ~ "id",
  site_number == "7.Wharminda_Bonanza"          ~ "field_1",
  site_number == "8.Wynarka_Tanks"              ~ "field_1",
  TRUE ~ NA_character_
)
if (is.na(join_col)) stop(paste("Join column not defined for site_number:", site_number))

# Keep only join column and geometry. Tidy up so that I am not using any predefined zones
sampling_pts <- sampling_pts[, c(join_col, attr(sampling_pts, "sf_column"))]

# Convert join column to character if needed
if (!is.character(sampling_pts[[join_col]])) {
  sampling_pts[[join_col]] <- as.character(sampling_pts[[join_col]])
}

### Join sampling location to soil test results
soil_results_plus_location <- left_join(
  sampling_pts,
  df_subset,
  join_by(!!sym(join_col) == SampleNameShort)
)

id_col <- case_when(
  site_number == "1.Walpeup_MRS125"            ~ "id",
  site_number == "2.Crystal_Brook_Brians_House" ~ "id",
  site_number == "3.Wynarka_Mervs_West"         ~ "id",# 
  site_number == "4.Wharminda_Woodys"           ~ "pt_ID_Soil",  
  site_number == "5.Walpeup_Gums"              ~ "ID_new",
  site_number == "6.Crystal_Brook_Randals"      ~ "id",
  site_number == "7.Wharminda_Bonanza"          ~ "field_1",
  site_number == "8.Wynarka_Tanks"              ~ "field_1",
  TRUE ~ NA_character_
)
if (is.na(id_col)) stop(paste("ID column not defined for site_number:", site_number))

## Assign a consistent name to soil test ID results
soil_results_plus_location <- soil_results_plus_location %>%
    rename(ID = !!sym(id_col))



soil_results_plus_location$CRS <- st_crs(soil_results_plus_location)$input
names(soil_results_plus_location)

coords <- st_coordinates(soil_results_plus_location)
soil_results_plus_location$X <- coords[, "X"]
soil_results_plus_location$Y <- coords[, "Y"]

out_file <- paste0(out_folder, tools::file_path_sans_ext(file), "_reformat_coods.csv")

write.csv(st_drop_geometry(soil_results_plus_location), out_file, row.names = FALSE)
