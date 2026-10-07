## Compile all sites - pre-sowing profile Min N and soil water
## Script: Step6_Compile_all_sites_profile.R

library(dplyr)
library(readr)
library(stringr)
library(purrr)

################################################################################
## Settings
################################################################################
dir        <- "//fs1-cbr.nexus.csiro.au/{af-sandysoils-ii}"
output_dir <- paste0(dir, "/work/Output-1/")

# folder for the combined file (change if you want it somewhere else)
out_folder <- paste0(output_dir, "0.Site-info/")

bulk_density <- 1.3   # g/cm3 - assumed in Steps 2 and 3 for every site and depth

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
  )
)

################################################################################
## Read the Step 5 csv for each site
################################################################################
read_site_csv <- function(site_number) {
  folder <- paste0(output_dir, site_number, "/6.Soil_Data/Compiled_Data/R_maps/")
  files  <- list.files(folder, pattern = "_N_and_H2O_\\d{2}_\\.csv$", full.names = TRUE)
  
  if (length(files) == 0) {
    warning("No Step 5 csv found for ", site_number)
    return(NULL)
  }
  
  map_dfr(files, function(f) {
    read_csv(f, col_types = cols(.default = col_character())) %>%  # read all as text, convert below
      mutate(source_file = basename(f))
  }) %>%
    mutate(site_number = site_number)
}

raw <- map_dfr(site_lookup$site_number, read_site_csv)

# sites 7 and 8 have no treatment column yet, so add it if missing
if (!"treat_desc" %in% names(raw)) raw$treat_desc <- NA_character_

names(raw)

################################################################################
## Standardise columns
################################################################################
profile_all <- raw %>%
  transmute(
    site               = str_remove(site_number, "^\\d+\\."),
    site_number,
    sampling_timing    = if_else(str_detect(site_number, "^[78]\\."), "Baseline", "Pre_Season"),
    year               = 2000L + as.integer(str_match(source_file, "_N_and_H2O_(\\d{2})_\\.csv")[, 2]),
    sample_id          = ID,
    sampling_date      = as.Date(SamplingDate, format = "%d %B %Y"),
    longitude          = as.numeric(X),
    latitude           = as.numeric(Y),
    epsg               = as.integer(str_extract(CRS, "\\d+")),
    zone,
    treatment          = treat_desc,
    profile_depth_cm   = ProfileDepth,
    min_n_kg_ha        = as.numeric(MinN_kg_ha),
    soil_water_mm      = as.numeric(Soil_water_mm_profile),
    bulk_density_g_cm3 = bulk_density
  ) %>%
  arrange(site, sample_id)

################################################################################
## Checks - look at these before trusting the file
################################################################################
# 1. rows per site and year
profile_all %>% count(site, year, sampling_timing)

# 2. coordinates / CRS (all should be 4326, so x = longitude, y = latitude)
table(profile_all$epsg, useNA = "ifany")

# 3. dates that failed to parse
profile_all %>% filter(is.na(sampling_date)) %>% count(site)

# 4. duplicated sample points (pivot_wider can split N and water into two rows)
profile_all %>%
  count(site, sample_id, sampling_date) %>%
  filter(n > 1)

# 5. missing results
profile_all %>%
  group_by(site) %>%
  summarise(n = n(),
            n_missing_N     = sum(is.na(min_n_kg_ha)),
            n_missing_water = sum(is.na(soil_water_mm)))

################################################################################
## Add zone descriptors from the metadata file                      # NEW
################################################################################
metadata_file_name <- "names of treatments per site 2025 metadata and other info.xlsx"   # 

zone_lookup <- readxl::read_excel(                                  # 
  paste0(out_folder, metadata_file_name),                           # 
  sheet = "zone_details") %>%                                       # 
  transmute(                                                        # 
    site_number      = Site,                                        # 
    zone_code        = as.integer(`zone names`),                    # 
    zone_description = str_trim(str_remove(`zone label names`, "^\\d+="))   # NEW (drops the "1=" prefix)
  )                                                                 # 

profile_all <- profile_all %>%                                      # 
  mutate(zone_code = suppressWarnings(as.integer(as.numeric(zone)))) %>%   # 
  left_join(zone_lookup, by = c("site_number", "zone_code")) %>%    # 
  select(-zone_code) %>%                                            # 
  relocate(zone_description, .after = zone)                         # 

# check: every zone should have a description, none should be NA    # 
profile_all %>%                                                     # 
  distinct(site, zone, zone_description) %>%                        # 
  arrange(site, zone)                                               # 

################################################################################
## Save
################################################################################
write_csv(profile_all, paste0(out_folder, "All_sites_pre_sowing_profile_N_and_H2O.csv"))
