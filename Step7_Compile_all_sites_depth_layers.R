## Compile all sites - depth layers and depth groups
## Script: Step7_Compile_all_sites_depth_layers.R

library(dplyr)
library(readr)
library(stringr)
library(purrr)

################################################################################
## Settings
################################################################################
dir        <- "//fs1-cbr.nexus.csiro.au/{af-sandysoils-ii}"
output_dir <- paste0(dir, "/work/Output-1/")
out_folder <- paste0(output_dir, "0.Site-info/")

site_numbers <- c(
  "1.Walpeup_MRS125", "2.Crystal_Brook_Brians_House", "3.Wynarka_Mervs_West",
  "4.Wharminda_Woodys", "5.Walpeup_Gums", "6.Crystal_Brook_Randals",
  "7.Wharminda_Bonanza", "8.Wynarka_Tanks"
)

# depth groups, based on the top of each layer (cm). Change these once we see the real depths.
depth_breaks <- c(0, 20, 60, Inf)                          # NEW
depth_labels <- c("0-20 cm", "20-60 cm", "60-100 cm")      # NEW

sum_or_na <- function(x) if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)

################################################################################
## Read the layer files
################################################################################
read_layers <- function(site_number, pattern) {
  folder <- paste0(output_dir, site_number, "/6.Soil_Data/Compiled_Data/")
  files  <- list.files(folder, pattern = pattern, full.names = TRUE)
  if (length(files) == 0) {
    warning("No file matching '", pattern, "' for ", site_number)
    return(NULL)
  }
  map_dfr(files, ~ read_csv(.x, col_types = cols(.default = col_character()))) %>%
    mutate(site_number = site_number)
}

N_layers <- map_dfr(site_numbers, read_layers, pattern = "^Mineral N layers\\d{2}\\.csv$") %>%
  transmute(
    site_number,
    sample_id       = SampleNameShort,
    sample_depth    = SampleDepth,
    depth_upper_cm  = as.numeric(DepthUpper),
    depth_lower_cm  = as.numeric(DepthLower),
    nitrate_mg_kg   = as.numeric(Nitrate_mg_kg),
    ammonium_mg_kg  = as.numeric(Ammonium_mg_kg),
    nitrate_kg_ha   = as.numeric(Nitrate_kg_ha),
    ammonium_kg_ha  = as.numeric(Ammonium_kg_ha),
    min_n_kg_ha     = as.numeric(MinN_kg_ha),
    bd_N            = as.numeric(bulk_density)
  )

H2O_layers <- map_dfr(site_numbers, read_layers, pattern = "^Soil water layers\\d{2}\\.csv$") %>%
  transmute(
    site_number,
    sample_id       = SampleNameShort,
    sample_depth    = SampleDepth,
    depth_upper_cm  = as.numeric(DepthUpper),
    depth_lower_cm  = as.numeric(DepthLower),
    gravimetric_moisture_pct = as.numeric(Gravimetric_moisture_pct),
    soil_water_mm   = as.numeric(Soil_water_mm),
    bd_H2O          = as.numeric(bulk_density)
  )

layers <- full_join(
  N_layers, H2O_layers,
  by = c("site_number", "sample_id", "sample_depth", "depth_upper_cm", "depth_lower_cm")
) %>%
  mutate(bulk_density_g_cm3 = coalesce(bd_N, bd_H2O)) %>%
  select(-bd_N, -bd_H2O)

################################################################################
## Add site info (date, coordinates, EPSG, zone, treatment) from the Step 6 file
################################################################################
profile_file <- paste0(out_folder, "All_sites_pre_sowing_profile_N_and_H2O.csv")
profile_raw  <- read_csv(profile_file, col_types = cols(.default = col_character()))

site_info <- profile_raw %>%
  select(site, site_number, sampling_timing, year, sample_id, sampling_date,
         longitude, latitude, epsg, zone, zone_description, treatment) %>%
  distinct() %>%
  mutate(year          = as.integer(year),
         sampling_date = as.Date(sampling_date),
         longitude     = as.numeric(longitude),
         latitude      = as.numeric(latitude),
         epsg          = as.integer(epsg))

layers <- layers %>%
  left_join(site_info, by = c("site_number", "sample_id")) %>%
  mutate(
    depth_group = cut(depth_upper_cm, breaks = depth_breaks, labels = depth_labels, right = FALSE)
  ) %>%
  select(site, site_number, sampling_timing, year, sample_id, sampling_date,
         longitude, latitude, epsg, zone, zone_description, treatment,
         sample_depth, depth_upper_cm, depth_lower_cm, depth_group,
         everything()) %>%
  arrange(site, sample_id, depth_upper_cm)

################################################################################
## Checks - look at these before trusting the files
################################################################################
# 1. what depths were actually sampled at each site (use this to settle the groupings)
layers %>% count(site, sample_depth, depth_upper_cm, depth_lower_cm) %>% arrange(site, depth_upper_cm)

# 2. layers that did not match a sample point in the Step 6 file (should be none)
layers %>% filter(is.na(site)) %>% count(site_number, sample_id)

# 3. layers that straddle a group boundary (should be none)
group_max <- depth_breaks[-1]
layers %>%
  filter(depth_lower_cm > group_max[as.integer(depth_group)]) %>%
  count(site, sample_depth, depth_group)

# 4. layer sums should equal the profile values in the Step 6 file (should be none)
profile_check <- profile_raw %>%
  transmute(site_number, sample_id,
            profile_n     = as.numeric(min_n_kg_ha),
            profile_water = as.numeric(soil_water_mm)) %>%
  distinct()

layers %>%
  group_by(site_number, sample_id) %>%
  summarise(layer_n = sum_or_na(min_n_kg_ha), layer_water = sum_or_na(soil_water_mm), .groups = "drop") %>%
  left_join(profile_check, by = c("site_number", "sample_id")) %>%
  filter(abs(layer_n - profile_n) > 0.01 | abs(layer_water - profile_water) > 0.01)

################################################################################
## Depth groups: one row per sample point per group
################################################################################
depth_groups <- layers %>%
  group_by(site, site_number, sampling_timing, year, sample_id, sampling_date,
           longitude, latitude, epsg, zone, zone_description, treatment,
           depth_group, bulk_density_g_cm3) %>%
  summarise(
    n_layers      = n(),
    depth_top_cm  = min(depth_upper_cm),
    depth_base_cm = max(depth_lower_cm),
    min_n_kg_ha   = sum_or_na(min_n_kg_ha),
    soil_water_mm = sum_or_na(soil_water_mm),
    .groups = "drop"
  ) %>%
  arrange(site, sample_id, depth_top_cm)

################################################################################
## Save
################################################################################
write_csv(layers,       paste0(out_folder, "All_sites_pre_sowing_depth_layers_N_and_H2O.csv"))
write_csv(depth_groups, paste0(out_folder, "All_sites_pre_sowing_depth_groups_N_and_H2O.csv"))
              