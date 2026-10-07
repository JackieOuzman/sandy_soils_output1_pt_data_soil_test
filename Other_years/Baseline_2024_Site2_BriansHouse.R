## Brians House (Crystal Brook) - 2024 baseline - N and soil water by layer
## Script: Baseline_2024_Site2_BriansHouse.R
## Copy of the Walpeup script. Lines marked "# CHANGED" differ from site 1.

library(dplyr)
library(readr)
library(readxl)
library(stringr)
library(sf)

################################################################################
## Settings
################################################################################
dir         <- "//fs1-cbr.nexus.csiro.au/{af-sandysoils-ii}"
site_number <- "2.Crystal_Brook_Brians_House"                 # CHANGED
site_name   <- "Crystal_Brook_Brians_House"                   # CHANGED
headDir     <- paste0(dir, "/work/Output-1/", site_number)
baseline_folder <- paste0(headDir, "/6.Soil_Data/1.Baseline/")

# CHANGED: find the lab results csv by pattern (the file name is long)
n_file <- list.files(paste0(baseline_folder, "Structured"),
                     pattern = "Brians_House_Soil_Characterisation_Results_All_Lab_Data.*\\.csv$",
                     full.names = TRUE)
stopifnot(length(n_file) == 1)
water_file <- paste0(baseline_folder, "RawData/Crystal Brook baseline soil water.xlsx")   # CHANGED

bulk_density  <- 1.3    # g/cm3 - assumed, same as the 2026 workflow
epsg          <- 4326   # ASSUMED from the lat/long values (as at Walpeup)

metadata_path      <- paste0(dir, "/work/Output-1/0.Site-info/")
metadata_file_name <- "names of treatments per site 2025 metadata and other info.xlsx"
out_folder         <- metadata_path

depth_breaks <- c(0, 20, 60, Inf)
depth_labels <- c("0-20 cm", "20-60 cm", "60-100 cm")
sum_or_na    <- function(x) if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)

# lab values like "<0.5" become 0, same as Step 2
to_num <- function(x) {
  x <- str_trim(x)
  if_else(str_detect(x, "^<"), 0, suppressWarnings(as.numeric(x)))
}

################################################################################
## Mineral N by layer
################################################################################
n_raw <- read_csv(n_file, col_types = cols(.default = col_character()),
                  name_repair = "unique_quiet")

N_layers <- n_raw %>%
  transmute(
    point_id       = Point_ID,
    lab_label      = str_extract(SampleName, "^[^_]+"),
    sample_depth   = str_extract(SampleName, "\\d+-\\d+$"),   # SampleDepth has "10-20" mangled to "Oct-20"
    longitude      = as.numeric(X),
    latitude       = as.numeric(Y),
    nitrate_mg_kg  = to_num(`Nitrate...N..2M.KCl.`),           # CHANGED
    ammonium_mg_kg = to_num(`Ammonium...N..2M.KCl.`)           # CHANGED (units assumed mg/kg)
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
## Soil water by layer
################################################################################
water_raw <- read_excel(water_file, sheet = "Sheet1")                  # CHANGED

# CHANGED: the sampling date is in the water sheet (one date for all rows)
sampling_date <- unique(as.Date(water_raw$Date))
stopifnot(length(sampling_date) == 1)

water_layers <- water_raw %>%
  filter(SampleID == "Chem only") %>%
  transmute(
    lab_label          = Plot,
    sample_depth       = str_trim(as.character(Depth)),
    chip_wt_g          = as.numeric(`Chip weight (g)`),
    wet_wt_g           = as.numeric(`Wet weight (g)`),
    dry_wt_g           = as.numeric(`Dry weight (g)`),
    sheet_pct_moisture = as.numeric(`% mois`)                          # CHANGED column name
  ) %>%
  mutate(
    gravimetric_moisture_pct = (wet_wt_g - dry_wt_g) / (dry_wt_g - chip_wt_g) * 100
  )

################################################################################
## Join N and water
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
## Zone and treatment strip
################################################################################
get_path <- function(var) {
  readxl::read_excel(paste0(metadata_path, metadata_file_name), sheet = "file location etc") %>%
    filter(Site == site_number, variable == var) %>%
    pull("file path") %>%
    unique()
}

zones <- st_read(paste0(headDir, get_path("location of zone shp")), quiet = TRUE)
trial <- st_read(paste0(headDir, get_path("trial.plan")), quiet = TRUE)

pts <- baseline_layers %>%
  distinct(point_id, longitude, latitude) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = epsg, remove = FALSE) %>%
  st_transform(st_crs(zones))

pts <- st_join(pts, zones %>% select(zone = cluster), join = st_within)      # CHANGED: zone column is 'cluster'
pts <- st_join(pts, st_transform(trial, st_crs(zones)) %>% select(treatment = treat_desc),
               join = st_within)

pt_info <- pts %>% st_drop_geometry() %>% select(point_id, zone, treatment)

zone_lookup <- readxl::read_excel(paste0(metadata_path, metadata_file_name), sheet = "zone_details") %>%
  filter(Site == site_number) %>%
  transmute(zone_code        = as.integer(`zone names`),
            zone_description = str_trim(str_remove(`zone label names`, "^\\d+=")))

################################################################################
## Final layer file and depth groups
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
# 1. 125 rows, 25 points x 5 depths, no missing water
nrow(final_layers)
final_layers %>% count(sample_id) %>% count(n)
final_layers %>% filter(is.na(soil_water_mm)) %>% count(sample_id, sample_depth)

# 2. profile totals per point
final_layers %>%
  group_by(sample_id) %>%
  summarise(min_n_kg_ha = sum(min_n_kg_ha, na.rm = TRUE),
            soil_water_mm = sum(soil_water_mm, na.rm = TRUE)) %>%
  summary()

# 3. zone, description and strip per point
final_layers %>% distinct(sample_id, zone, zone_description, treatment) %>%
  count(zone, zone_description, treatment)

# 4. zone from the shapefile vs the cluster4 column in the lab file
n_raw %>% distinct(Point_ID, cluster4) %>%
  left_join(pt_info, by = c("Point_ID" = "point_id")) %>%
  count(cluster4, zone)

################################################################################
## Save
################################################################################
write_csv(final_layers, paste0(out_folder, "Baseline_2024_Crystal_Brook_Brians_House_depth_layers_N_and_H2O.csv"))
write_csv(final_groups, paste0(out_folder, "Baseline_2024_Crystal_Brook_Brians_House_depth_groups_N_and_H2O.csv"))


site2 <- paste0(dir, "/work/Output-1/2.Crystal_Brook_Brians_House")

list.files(paste0(site2, "/4.Sampling"),  pattern = "\\.shp$", recursive = TRUE)
list.files(paste0(site2, "/6.Soil_Data"), pattern = "\\.shp$", recursive = TRUE)

library(sf)

shp <- st_read(paste0(site2, "/4.Sampling/1.Baseline/ACTUAL/BHO_baseline_points_4326.shp"), quiet = TRUE)
st_crs(shp)$input

shp_xy <- st_coordinates(shp) %>% as.data.frame() %>% transmute(X = round(X, 5), Y = round(Y, 5))
csv_xy <- final_layers %>% distinct(sample_id, longitude, latitude) %>%
  transmute(X = round(longitude, 5), Y = round(latitude, 5))

nrow(shp)                                  # points in the shapefile
nrow(inner_join(shp_xy, csv_xy, by = c("X", "Y")))   # should be 25
