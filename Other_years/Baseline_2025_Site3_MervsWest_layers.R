## Mervs West (Wynarka) - 2025 baseline - N and soil water by layer
## Script: Baseline_2025_Site3_MervsWest_layers.R

library(dplyr)
library(readr)
library(readxl)
library(stringr)
library(sf)

################################################################################
## Settings
################################################################################
dir         <- "//fs1-cbr.nexus.csiro.au/{af-sandysoils-ii}"
site_number <- "3.Wynarka_Mervs_West"
site_name   <- "Wynarka_Mervs_West"
headDir     <- paste0(dir, "/work/Output-1/", site_number)
soil_dir    <- paste0(headDir, "/6.Soil_Data/1.Baseline/")

lab_file    <- paste0(soil_dir, "Structured/Wynarka_Soil_Characterisation_Results_All_Lab_Data_APAL.xlsx")
water_file  <- paste0(soil_dir, "RawData/WYN_Baseline_soils.xlsx")
coords_file <- paste0(soil_dir, "Structured/Coordinates.csv")

bulk_density <- 1.3     # g/cm3 - assumed, same as the 2026 workflow
epsg         <- 4326    # lab X/Y are lat/long

metadata_path      <- paste0(dir, "/work/Output-1/0.Site-info/")
metadata_file_name <- "names of treatments per site 2025 metadata and other info.xlsx"
out_folder         <- metadata_path

depth_breaks <- c(0, 20, 60, Inf)
depth_labels <- c("0-20 cm", "20-60 cm", "60-100 cm")
sum_or_na    <- function(x) if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)

to_num <- function(x) {
  x <- str_trim(as.character(x))
  if_else(str_detect(x, "^<"), 0, suppressWarnings(as.numeric(x)))
}

# read a sheet whose header row is somewhere down (find it by a known column name)
read_with_header <- function(path, sheet, key) {
  raw <- read_excel(path, sheet = sheet, col_names = FALSE, col_types = "text", .name_repair = "minimal")
  hr  <- which(apply(raw, 1, function(r) any(r == key, na.rm = TRUE)))[1]
  stopifnot(!is.na(hr))
  hdr <- as.character(unlist(raw[hr, ]))
  hdr[is.na(hdr)] <- paste0("col", seq_along(hdr))[is.na(hdr)]
  out <- raw[-seq_len(hr), ]; names(out) <- make.unique(hdr)
  out
}

################################################################################
## Mineral N by layer
################################################################################
lab <- read_with_header(lab_file, "Data", "SampleName") %>%
  filter(!is.na(SampleName))

N_layers <- lab %>%
  transmute(
    barcode        = str_trim(Barcode),
    point_id       = Point_ID,
    lab_label      = Point_ID,
    sample_depth   = SampleDepth,
    longitude      = as.numeric(X),
    latitude       = as.numeric(Y),
    nitrate_mg_kg  = to_num(`Nitrate - N (2M KCl)`),
    ammonium_mg_kg = to_num(`Ammonium - N (2M KCl)`)
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
## Soil water by layer (linked by APAL barcode)
################################################################################
wt <- read_excel(water_file, sheet = "Full analysis", col_types = "text") %>%
  filter(!is.na(`APAL Barcode`))

# sampling date from the sample labels (D18 M03 YR25)
sampling_date <- unique(as.Date(paste0("20", str_sub(wt$Year, 3), "-", str_sub(wt$Month, 2), "-", str_sub(wt$Day, 2))))
stopifnot(length(sampling_date) == 1)

water_layers <- wt %>%
  transmute(
    barcode  = str_trim(`APAL Barcode`),
    chip_wt_g = as.numeric(`Chip wt (g)`),
    wet_wt_g  = as.numeric(`Wet wt (g)`),
    dry_wt_g  = as.numeric(`Dry wt (g)`)
  ) %>%
  mutate(gravimetric_moisture_pct = (wet_wt_g - dry_wt_g) / (dry_wt_g - chip_wt_g) * 100)

# field GPS position of each point (replaces the X/Y carried in the lab file)
coords_field <- read_csv(coords_file, col_types = cols(.default = col_character()),
                             name_repair = "unique_quiet") %>%
  transmute(point_id = Sample, longitude = as.numeric(X...1), latitude = as.numeric(Y...2))
  
baseline_layers <- N_layers %>%
  left_join(water_layers %>% select(barcode, gravimetric_moisture_pct), by = "barcode") %>%
  select(-longitude, -latitude) %>%
  left_join(coords_field, by = "point_id") %>%
  mutate(
    soil_water_mm      = gravimetric_moisture_pct / 100 * bulk_density * thickness_cm * 10,
    site               = site_name,
    site_number        = site_number,
    sampling_timing    = "Baseline",
    year               = 2025L,
    sampling_date      = sampling_date,
    epsg               = epsg,
    bulk_density_g_cm3 = bulk_density
  )

################################################################################
## Zone and treatment
################################################################################
get_path <- function(var) {
  readxl::read_excel(paste0(metadata_path, metadata_file_name), sheet = "file location etc") %>%
    filter(Site == site_number, variable == var) %>%
    pull("file path") %>%
    unique()
}

zones <- st_read(paste0(headDir, get_path("location of zone shp")), quiet = TRUE)
trial <- st_read(paste0(headDir, get_path("trial.plan")), quiet = TRUE)
names(zones); names(trial)          # look at these if the next lines fail
zone_col  <- "fcl_mdl"              # same zone column as the 2026 Mervs West data
trial_col <- intersect(c("treat_desc", "treatment"), names(trial))[1]
stopifnot(zone_col %in% names(zones), !is.na(trial_col))

pts <- baseline_layers %>%
  distinct(point_id, longitude, latitude) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = epsg, remove = FALSE) %>%
  st_transform(st_crs(zones))

pts <- st_join(pts, zones %>% select(zone = all_of(zone_col)), join = st_within)
pts <- st_join(pts, st_transform(trial, st_crs(zones)) %>% select(treatment = all_of(trial_col)),
               join = st_within)

pt_info <- pts %>% st_drop_geometry() %>% select(point_id, zone, treatment) %>% distinct(point_id, .keep_all = TRUE)

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
# 1. expect 101 layers, 25 points, deepest layer per point varies (shallow rocky points)
nrow(final_layers)
final_layers %>% group_by(sample_id) %>%
  summarise(n_layers = n(), base_cm = max(depth_lower_cm), .groups = "drop") %>%
  count(n_layers, base_cm)

# 2. missing values (expect 0 for N and water; zone/treatment may be NA)
final_layers %>%
  summarise(no_N = sum(is.na(min_n_kg_ha)), no_water = sum(is.na(soil_water_mm)),
            no_coords = sum(is.na(longitude)), no_zone = sum(is.na(zone)),
            no_zone_desc = sum(is.na(zone_description)), no_treatment = sum(is.na(treatment)))

# 3. recomputed moisture vs the sheet's GSM (should be none)
wt %>% transmute(barcode = str_trim(`APAL Barcode`), gsm_sheet = as.numeric(`GSM (%)`)) %>%
  inner_join(water_layers %>% select(barcode, gravimetric_moisture_pct), by = "barcode") %>%
  filter(abs(gsm_sheet - gravimetric_moisture_pct) > 0.01)

# 4. zone from the shapefile vs cluster4 recorded at sampling time
coords <- read_csv(coords_file, col_types = cols(.default = col_character()), name_repair = "unique_quiet")
coords %>% transmute(point_id = Sample, cluster4 = as.integer(as.numeric(cluster4))) %>%
  left_join(pt_info, by = "point_id") %>% count(cluster4, zone)

# 5. lab X/Y vs Coordinates.csv (largest difference in degrees; should be ~0)
final_layers %>% distinct(sample_id, longitude, latitude) %>%
  inner_join(coords %>% transmute(sample_id = Sample, x = as.numeric(X...1), y = as.numeric(Y...2)), by = "sample_id") %>%
  summarise(max_dx = max(abs(longitude - x)), max_dy = max(abs(latitude - y)))

# 6. points per treatment, zone
final_layers %>% distinct(sample_id, treatment, zone, zone_description) %>%
  count(treatment, zone, zone_description)

################################################################################
## Save
################################################################################
write_csv(final_layers, paste0(out_folder, "Baseline_2025_Wynarka_Mervs_West_depth_layers_N_and_H2O.csv"))
write_csv(final_groups, paste0(out_folder, "Baseline_2025_Wynarka_Mervs_West_depth_groups_N_and_H2O.csv"))



final_layers %>% distinct(sample_id, longitude, latitude) %>%
  inner_join(coords %>% transmute(sample_id = Sample, x = as.numeric(X...1), y = as.numeric(Y...2),
                                  new_point, visited), by = "sample_id") %>%
  mutate(dx_m = (longitude - x) * 91000, dy_m = (latitude - y) * 111000) %>%
  filter(abs(dx_m) > 1 | abs(dy_m) > 1) %>%
  select(sample_id, dx_m, dy_m, new_point, visited)
