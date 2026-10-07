## Stack 2024 baselines and 2026 data into one set of files
## Script: Step8_Stack_all_years.R

library(dplyr)
library(readr)
library(purrr)

################################################################################
## Settings
################################################################################
dir        <- "//fs1-cbr.nexus.csiro.au/{af-sandysoils-ii}"
out_folder <- paste0(dir, "/work/Output-1/0.Site-info/")

files_layers <- c(
  "All_sites_pre_sowing_depth_layers_N_and_H2O.csv",                       # 2026, sites 1-8 (Step 7)
  "Baseline_2024_Walpeup_MRS125_depth_layers_N_and_H2O.csv",               # 2024, site 1
  "Baseline_2024_Crystal_Brook_Brians_House_depth_layers_N_and_H2O.csv",   # 2024, site 2
  "PrePlant_2025_Walpeup_MRS125_depth_layers_N_and_H2O.csv",             # 2025, site 1 (N only)
  "PrePlant_2025_Crystal_Brook_Brians_House_depth_layers_N_and_H2O.csv",   # 2025, site 2 (N only)
  "Baseline_2025_Wynarka_Mervs_West_depth_layers_N_and_H2O.csv" # 2025, site 3
)
files_groups <- c(
  "All_sites_pre_sowing_depth_groups_N_and_H2O.csv",
  "Baseline_2024_Walpeup_MRS125_depth_groups_N_and_H2O.csv",
  "Baseline_2024_Crystal_Brook_Brians_House_depth_groups_N_and_H2O.csv",
  "PrePlant_2025_Walpeup_MRS125_depth_groups_N_and_H2O.csv",
  "PrePlant_2025_Crystal_Brook_Brians_House_depth_groups_N_and_H2O.csv",
  "Baseline_2025_Wynarka_Mervs_West_depth_groups_N_and_H2O.csv"
)

depth_labels <- c("0-20 cm", "20-60 cm", "60-100 cm")
sum_or_na    <- function(x) if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)

# read every file as text so the different years bind cleanly, then convert below
read_stack <- function(files) {
  map_dfr(files, function(f) {
    read_csv(paste0(out_folder, f), col_types = cols(.default = col_character()),
             show_col_types = FALSE) %>%
      mutate(source_file = f)
  })
}

# zone codes should be plain whole numbers ("1", not "1.0")
tidy_zone <- function(z) {
  n <- suppressWarnings(as.numeric(z))
  if_else(is.na(n), z, as.character(as.integer(n)))
}

################################################################################
## Layers
################################################################################
layers <- read_stack(files_layers) %>%
  mutate(
    year          = as.integer(year),
    sampling_date = as.Date(sampling_date),
    epsg          = as.integer(epsg),
    zone          = tidy_zone(zone),
    depth_group   = factor(depth_group, levels = depth_labels),
    across(c(longitude, latitude, depth_upper_cm, depth_lower_cm,
             nitrate_mg_kg, ammonium_mg_kg, nitrate_kg_ha, ammonium_kg_ha,
             min_n_kg_ha, gravimetric_moisture_pct, soil_water_mm, bulk_density_g_cm3),
           as.numeric)
  ) %>%
  select(site, site_number, year, sampling_timing, sample_id, lab_label,
         sampling_date, longitude, latitude, epsg,
         zone, zone_description, treatment,
         sample_depth, depth_upper_cm, depth_lower_cm, depth_group,
         nitrate_mg_kg, ammonium_mg_kg, nitrate_kg_ha, ammonium_kg_ha, min_n_kg_ha,
         gravimetric_moisture_pct, soil_water_mm, bulk_density_g_cm3, source_file) %>%
  arrange(site_number, year, sample_id, depth_upper_cm)

################################################################################
## Depth groups
################################################################################
groups <- read_stack(files_groups) %>%
  mutate(
    year          = as.integer(year),
    sampling_date = as.Date(sampling_date),
    epsg          = as.integer(epsg),
    zone          = tidy_zone(zone),
    depth_group   = factor(depth_group, levels = depth_labels),
    across(c(n_layers, longitude, latitude, depth_top_cm, depth_base_cm,
             min_n_kg_ha, soil_water_mm, bulk_density_g_cm3),
           as.numeric)
  ) %>%
  select(site, site_number, year, sampling_timing, sample_id, lab_label,
         sampling_date, longitude, latitude, epsg,
         zone, zone_description, treatment,
         depth_group, n_layers, depth_top_cm, depth_base_cm,
         min_n_kg_ha, soil_water_mm, bulk_density_g_cm3, source_file) %>%
  arrange(site_number, year, sample_id, depth_top_cm)

################################################################################
## Profile totals, built by summing the layers
################################################################################
profile <- layers %>%
  group_by(site, site_number, year, sampling_timing, sample_id, lab_label,
           sampling_date, longitude, latitude, epsg,
           zone, zone_description, treatment, bulk_density_g_cm3) %>%
  summarise(
    n_layers      = n(),
    depth_top_cm  = min(depth_upper_cm),
    depth_base_cm = max(depth_lower_cm),
    min_n_kg_ha   = sum_or_na(min_n_kg_ha),
    soil_water_mm = sum_or_na(soil_water_mm),
    .groups = "drop"
  ) %>%
  arrange(site_number, year, sample_id)

################################################################################
## Checks - send me these
################################################################################
# 1. sample points per site, year and timing (expect 25 for each 2024 site)
layers %>% distinct(site, year, sampling_timing, sample_id) %>%
  count(site, year, sampling_timing)

# 2. missing values per site and year (all should be 0, except treatment)
profile %>%
  group_by(site, year) %>%
  summarise(points = n(),
            no_N = sum(is.na(min_n_kg_ha)), no_water = sum(is.na(soil_water_mm)),
            no_zone = sum(is.na(zone_description)), no_date = sum(is.na(sampling_date)),
            no_coords = sum(is.na(longitude) | is.na(latitude)), .groups = "drop")

# 3. duplicated layers (should be none)
layers %>% count(site, year, sample_id, sample_depth) %>% filter(n > 1)

# 4. 2026 profile totals should match the Step 6 profile file (should be none)
step6 <- read_csv(paste0(out_folder, "All_sites_pre_sowing_profile_N_and_H2O.csv"),
                  col_types = cols(.default = col_character())) %>%
  transmute(site_number, sample_id, n6 = as.numeric(min_n_kg_ha), w6 = as.numeric(soil_water_mm)) %>%
  distinct()

profile %>% filter(year == 2026) %>%
  left_join(step6, by = c("site_number", "sample_id")) %>%
  filter(abs(min_n_kg_ha - n6) > 0.01 | abs(soil_water_mm - w6) > 0.01 | is.na(n6))

# 5. look at the zones
profile %>% count(site, zone, zone_description) %>% print(n = Inf)

################################################################################
## Save
################################################################################
write_csv(layers,  paste0(out_folder, "All_sites_2024_2026_N_and_H2O_depth_layers.csv"))
write_csv(groups,  paste0(out_folder, "All_sites_2024_2026_N_and_H2O_depth_groups.csv"))
write_csv(profile, paste0(out_folder, "All_sites_2024_2026_N_and_H2O_profile.csv"))

