## Walpeup MRS125 - 2024 baseline - N and soil water by layer (Part A)
## Script: Baseline_2024_Site1_Walpeup_layers.R

library(dplyr)
library(readr)
library(readxl)
library(stringr)

################################################################################
## Settings
################################################################################
dir         <- "//fs1-cbr.nexus.csiro.au/{af-sandysoils-ii}"
site_number <- "1.Walpeup_MRS125"
site_name   <- "Walpeup_MRS125"
headDir     <- paste0(dir, "/work/Output-1/", site_number)
baseline_folder <- paste0(headDir, "/6.Soil_Data/1.Baseline/")

n_file     <- paste0(baseline_folder, "Structured/Walpeup_MRS125_Soil_Characterisation_Results_All_Lab_Data_APAL.csv")
water_file <- paste0(baseline_folder, "RawData/Walpeup baseline soil water .xlsx")

sampling_date <- as.Date("2024-03-20")  # file says 20/21 March 2024 - change if needed
bulk_density  <- 1.3                    # g/cm3 - assumed, same as the 2026 workflow
epsg          <- 4326                   # ASSUMED from lat/long values - confirm from FMP-Results.shp

# lab values like "<0.5" become 0, same as Step 2
to_num <- function(x) {
  x <- str_trim(x)
  if_else(str_detect(x, "^<"), 0, suppressWarnings(as.numeric(x)))
}

################################################################################
## Mineral N by layer (from the APAL results file)
################################################################################
n_raw <- read_csv(n_file, col_types = cols(.default = col_character()),
                  name_repair = "unique_quiet")

N_layers <- n_raw %>%
  transmute(
    point_id       = Point_ID,                             # GIS point (FMP01...)
    lab_label      = str_extract(SampleName, "^[^_]+"),    # lab label (FMP10...)
    sample_depth   = str_extract(SampleName, "\\d+-\\d+$"), # SampleDepth has "10-20" mangled to "Oct-20"
    longitude      = as.numeric(X),
    latitude       = as.numeric(Y),
    nitrate_mg_kg  = to_num(`TMs-007NO3`),
    ammonium_mg_kg = to_num(`TMs-007NH4`)                   # units assumed mg/kg, as in 2026
  ) %>%
  mutate(
    depth_upper_cm = as.numeric(str_extract(sample_depth, "^\\d+")),
    depth_lower_cm = as.numeric(str_extract(sample_depth, "\\d+$")),
    thickness_cm   = depth_lower_cm - depth_upper_cm,
    nitrate_kg_ha  = nitrate_mg_kg  * bulk_density * thickness_cm / 10,
    ammonium_kg_ha = ammonium_mg_kg * bulk_density * thickness_cm / 10,
    min_n_kg_ha    = if_else(is.na(nitrate_kg_ha), NA_real_,
                             rowSums(across(c(nitrate_kg_ha, ammonium_kg_ha)), na.rm = TRUE))
  )

################################################################################
## Soil water by layer (recomputed from the raw weights)
################################################################################
water_layers <- read_excel(water_file, sheet = "Soil weights (output 1)") %>%
  filter(SampleID == "Chem only") %>%
  transmute(
    lab_label          = Plot,
    sample_depth       = str_trim(as.character(Depth)),
    chip_wt_g          = as.numeric(`Chip weight (g)`),
    wet_wt_g           = as.numeric(`Wet weight (g)`),
    dry_wt_g           = as.numeric(`Dry weight (g)`),
    sheet_pct_moisture = as.numeric(`% moisture`)           # the sheet's own column (wet-weight basis)
  ) %>%
  mutate(
    # same basis as the 2026 data: (wet - dry) / dry soil, with the chip subtracted
    gravimetric_moisture_pct = (wet_wt_g - dry_wt_g) / (dry_wt_g - chip_wt_g) * 100
  )

################################################################################
## Join N and water, add the site details
################################################################################
baseline_layers <- N_layers %>%
  left_join(water_layers, by = c("lab_label", "sample_depth")) %>%
  mutate(
    soil_water_mm      = gravimetric_moisture_pct / 100 * bulk_density * thickness_cm * 10,
    site               = site_name,
    site_number        = site_number,
    sampling_timing    = "Baseline",
    year               = 2024L,
    sampling_date      = sampling_date,
    epsg               = epsg,
    bulk_density_g_cm3 = bulk_density
  )

################################################################################
## Checks - run these and send me the results
################################################################################
# 1. should be 125 rows (25 points x 5 depths)
nrow(baseline_layers)
baseline_layers %>% count(point_id) %>% count(n)

# 2. layers with no water result (should be none)
baseline_layers %>% filter(is.na(soil_water_mm)) %>% count(point_id, sample_depth)

# 3. recomputed moisture against the sheet's own column (recomputed should be higher)
baseline_layers %>%
  summarise(mean_recomputed = mean(gravimetric_moisture_pct, na.rm = TRUE),
            mean_sheet      = mean(sheet_pct_moisture, na.rm = TRUE))

# 4. profile totals per point, to see if they look sensible next to the 2026 values
baseline_layers %>%
  group_by(point_id) %>%
  summarise(min_n_kg_ha = sum(min_n_kg_ha, na.rm = TRUE),
            soil_water_mm = sum(soil_water_mm, na.rm = TRUE)) %>%
  summary()
################################################################################
## Part B - zone, treatment strip, depth groups, write the files
################################################################################
library(sf)

metadata_path      <- paste0(dir, "/work/Output-1/0.Site-info/")
metadata_file_name <- "names of treatments per site 2025 metadata and other info.xlsx"
out_folder         <- metadata_path

depth_breaks <- c(0, 20, 60, Inf)
depth_labels <- c("0-20 cm", "20-60 cm", "60-100 cm")
sum_or_na    <- function(x) if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)

# file paths come from the metadata sheet, same as Step 5
get_path <- function(var) {
  readxl::read_excel(paste0(metadata_path, metadata_file_name), sheet = "file location etc") %>%
    filter(Site == site_number, variable == var) %>%
    pull("file path") %>%
    unique()
}

zones <- st_read(paste0(headDir, get_path("location of zone shp")), quiet = TRUE)
trial <- st_read(paste0(headDir, get_path("trial.plan")), quiet = TRUE)

# one row per sample point, placed in a zone and a strip
pts <- baseline_layers %>%
  distinct(point_id, longitude, latitude) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = epsg, remove = FALSE) %>%
  st_transform(st_crs(zones))

pts <- st_join(pts, zones %>% select(zone = gridcode), join = st_within)
pts <- st_join(pts, st_transform(trial, st_crs(zones)) %>% select(treatment = treat_desc),
               join = st_within)

pt_info <- pts %>% st_drop_geometry() %>% select(point_id, zone, treatment)

# zone descriptions from the metadata sheet
zone_lookup <- readxl::read_excel(paste0(metadata_path, metadata_file_name), sheet = "zone_details") %>%
  filter(Site == site_number) %>%
  transmute(zone_code        = as.integer(`zone names`),
            zone_description = str_trim(str_remove(`zone label names`, "^\\d+=")))

################################################################################
## Final layer file (same layout as the 2026 layers file)
################################################################################
final_layers <- baseline_layers %>%
  left_join(pt_info, by = "point_id") %>%
  mutate(zone_code = suppressWarnings(as.integer(as.numeric(zone)))) %>%
  left_join(zone_lookup, by = "zone_code") %>%
  mutate(depth_group = cut(depth_upper_cm, breaks = depth_breaks,
                           labels = depth_labels, right = FALSE)) %>%
  transmute(
    site, site_number, sampling_timing, year,
    sample_id = point_id, lab_label,
    sampling_date, longitude, latitude, epsg,
    zone, zone_description, treatment,
    sample_depth, depth_upper_cm, depth_lower_cm, depth_group,
    nitrate_mg_kg, ammonium_mg_kg, nitrate_kg_ha, ammonium_kg_ha, min_n_kg_ha,
    gravimetric_moisture_pct, soil_water_mm, bulk_density_g_cm3
  ) %>%
  arrange(sample_id, depth_upper_cm)

################################################################################
## Depth groups: one row per sample point per group
################################################################################
final_groups <- final_layers %>%
  group_by(site, site_number, sampling_timing, year, sample_id, lab_label,
           sampling_date, longitude, latitude, epsg, zone, zone_description, treatment,
           depth_group, bulk_density_g_cm3) %>%
  summarise(
    n_layers      = n(),
    depth_top_cm  = min(depth_upper_cm),
    depth_base_cm = max(depth_lower_cm),
    min_n_kg_ha   = sum_or_na(min_n_kg_ha),
    soil_water_mm = sum_or_na(soil_water_mm),
    .groups = "drop"
  ) %>%
  arrange(sample_id, depth_top_cm)

################################################################################
## Checks - send me these
################################################################################
# 1. should be 125 rows with no duplicated point/depth
nrow(final_layers)
final_layers %>% count(sample_id, sample_depth) %>% filter(n > 1)

# 2. every point should have a zone, a description and a strip
final_layers %>% distinct(sample_id, zone, zone_description, treatment) %>%
  count(zone, zone_description, treatment)

################################################################################
## Save
################################################################################
write_csv(final_layers, paste0(out_folder, "Baseline_2024_Walpeup_MRS125_depth_layers_N_and_H2O.csv"))
write_csv(final_groups, paste0(out_folder, "Baseline_2024_Walpeup_MRS125_depth_groups_N_and_H2O.csv"))



### samples outside of treatment?
trial_t <- st_transform(trial, st_crs(zones))
na_pts  <- pts %>% filter(is.na(treatment))
d       <- st_distance(na_pts, st_union(trial_t))

na_pts %>%
  st_drop_geometry() %>%
  mutate(dist_to_strip_m = round(as.numeric(d), 1)) %>%
  select(point_id, zone, dist_to_strip_m) %>%
  arrange(dist_to_strip_m) %>%
  print(n = Inf)
