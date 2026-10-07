## Brians House (Crystal Brook) - 2025 pre-plant - N by layer (no soil water measured)
## Script: PrePlant_2025_Site2_BriansHouse_layers.R

library(dplyr)
library(readr)
library(readxl)
library(stringr)
library(sf)

################################################################################
## Settings
################################################################################
dir         <- "//fs1-cbr.nexus.csiro.au/{af-sandysoils-ii}"
site_number <- "2.Crystal_Brook_Brians_House"
site_name   <- "Crystal_Brook_Brians_House"
headDir     <- paste0(dir, "/work/Output-1/", site_number)

lab_file <- paste0(headDir, "/6.Soil_Data/3.25/RawData/",
                   "Copy of Brians House 2025 pre sow soil test results-depths corrected.xlsx")
pts_file <- paste0(headDir, "/4.Sampling/2.InSeason/25/0.PrePlant/PrePlant_Sampling_v2.shp")

# SAMPLING DATE IS UNKNOWN (lab received the samples 2025-05-08).
# Leave as NA until confirmed, then change to e.g. as.Date("2025-05-xx")
sampling_date <- as.Date(NA)

bulk_density <- 1.3     # g/cm3 - assumed, same as the 2026 workflow
epsg         <- 4326    # PrePlant_Sampling_v2.shp is WGS 84

metadata_path      <- paste0(dir, "/work/Output-1/0.Site-info/")
metadata_file_name <- "names of treatments per site 2025 metadata and other info.xlsx"
out_folder         <- metadata_path

depth_breaks <- c(0, 20, 60, Inf)
depth_labels <- c("0-20 cm", "20-60 cm", "60-100 cm")
sum_or_na    <- function(x) if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)

# lab values like "<1.0" become 0, same as Step 2
to_num <- function(x) {
  x <- str_trim(x)
  if_else(str_detect(x, "^<"), 0, suppressWarnings(as.numeric(x)))
}

################################################################################
## Mineral N by layer (find the header row by looking for "SampleName")
################################################################################
raw <- read_excel(lab_file, sheet = "Data", col_names = FALSE,
                  col_types = "text", .name_repair = "minimal")
hdr_row <- which(apply(raw, 1, function(r) any(r == "SampleName", na.rm = TRUE)))[1]
stopifnot(!is.na(hdr_row))
hdr <- as.character(unlist(raw[hdr_row, ]))
hdr[is.na(hdr)] <- paste0("col", seq_along(hdr))[is.na(hdr)]
y <- raw[-seq_len(hdr_row), ]; names(y) <- make.unique(hdr)
y <- y %>% filter(str_detect(SampleName, "^BH \\d+[A-Z]$"))

N_layers <- y %>%
  transmute(
    point_no       = as.integer(str_match(SampleName, "^BH (\\d+)[A-Z]$")[, 2]),
    depth_letter   = str_match(SampleName, "^BH \\d+([A-Z])$")[, 2],
    sample_depth   = str_replace(SampleDepth, "_", "-"),
    nitrate_mg_kg  = to_num(`Nitrate - N (2M KCl)`),
    ammonium_mg_kg = to_num(`Ammonium - N (2M KCl)`)
  ) %>%
  mutate(
    point_id       = paste0("BHO-PP25-", point_no),
    lab_label      = point_id,
    depth_upper_cm = as.numeric(str_extract(sample_depth, "^\\d+")),
    depth_lower_cm = as.numeric(str_extract(sample_depth, "\\d+$")),
    thickness_cm   = depth_lower_cm - depth_upper_cm,
    nitrate_kg_ha  = nitrate_mg_kg  * bulk_density * thickness_cm / 10,
    ammonium_kg_ha = ammonium_mg_kg * bulk_density * thickness_cm / 10,
    min_n_kg_ha    = if_else(is.na(nitrate_kg_ha), NA_real_,
                             rowSums(across(c(nitrate_kg_ha, ammonium_kg_ha)), na.rm = TRUE))
  )

################################################################################
## Coordinates, treatment and zone
################################################################################
pts <- st_read(pts_file, quiet = TRUE) %>% st_transform(epsg)
xy  <- st_coordinates(pts)
pts$longitude <- xy[, 1]; pts$latitude <- xy[, 2]

get_path <- function(var) {
  readxl::read_excel(paste0(metadata_path, metadata_file_name), sheet = "file location etc") %>%
    filter(Site == site_number, variable == var) %>%
    pull("file path") %>%
    unique()
}
zones <- st_read(paste0(headDir, get_path("location of zone shp")), quiet = TRUE)

pts <- st_join(st_transform(pts, st_crs(zones)), zones %>% select(zone = cluster), join = st_within)

pt_info <- pts %>% st_drop_geometry() %>%
  select(point_id = pt_id, treatment = treat_desc, zone, longitude, latitude) %>%
  distinct()

zone_lookup <- readxl::read_excel(paste0(metadata_path, metadata_file_name), sheet = "zone_details") %>%
  filter(Site == site_number) %>%
  transmute(zone_code        = as.integer(`zone names`),
            zone_description = str_trim(str_remove(`zone label names`, "^\\d+=")))

################################################################################
## Final layer file and depth groups
################################################################################
final_layers <- N_layers %>%
  left_join(pt_info, by = "point_id") %>%
  mutate(
    zone_code = suppressWarnings(as.integer(as.numeric(zone))),
    site = site_name, site_number = site_number,
    sampling_timing = "Pre_Season", year = 2025L,
    sampling_date = sampling_date, epsg = epsg,
    gravimetric_moisture_pct = NA_real_, soil_water_mm = NA_real_,
    bulk_density_g_cm3 = bulk_density
  ) %>%
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
    gravimetric_moisture_pct, soil_water_mm, bulk_density_g_cm3,
    depth_letter
  ) %>%
  arrange(sample_id, depth_upper_cm)

# depth letter check BEFORE dropping the helper column (A=0-10 ... E=60-100)
depth_check <- final_layers %>% count(depth_letter, sample_depth)
final_layers <- final_layers %>% select(-depth_letter)

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
# 1. expect 105 rows = 21 points x 5 depths
nrow(final_layers)
final_layers %>% count(sample_id) %>% count(n)

# 2. depth letter vs depth (expect A=0-10, B=10-20, C=20-40, D=40-60, E=60-100)
depth_check

# 3. anything missing? (expect 0 for N, coords, zone, treatment; water NA by design)
final_layers %>%
  summarise(no_N = sum(is.na(min_n_kg_ha)), no_coords = sum(is.na(longitude)),
            no_zone = sum(is.na(zone)), no_zone_desc = sum(is.na(zone_description)),
            no_treatment = sum(is.na(treatment)))

# 4. points per treatment and zone
final_layers %>% distinct(sample_id, treatment, zone, zone_description) %>%
  count(treatment, zone, zone_description)

# 5. profile N per point
final_layers %>% group_by(sample_id) %>%
  summarise(min_n_kg_ha = sum(min_n_kg_ha), .groups = "drop") %>%
  summary()

################################################################################
## Save
################################################################################
write_csv(final_layers, paste0(out_folder, "PrePlant_2025_Crystal_Brook_Brians_House_depth_layers_N_and_H2O.csv"))
write_csv(final_groups, paste0(out_folder, "PrePlant_2025_Crystal_Brook_Brians_House_depth_groups_N_and_H2O.csv"))
