### combined the pre sowing soils into one file.
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
  ),
  site_name_display = c(
    "Walpeup MRS125",
    "Crystal Brook Brians House",
    "Wynarka Mervs West",
    "Wharminda Woodys",
    "Walpeup Gums",
    "Crystal Brook Randals",
    "Wharminda Bonanza",
    "Wynarka Tanks"
  )
)

site_row        <- site_lookup[site_lookup$id == site_number_input, ]
site_number     <- site_row$site_number
site_name       <- site_row$site_name
site_name_display <- site_row$site_name_display

################################################################################


dir <- "//fs1-cbr.nexus.csiro.au/{af-sandysoils-ii}"
headDir <- paste0(dir, "/work/Output-1/", site_number)
metadata_path <- paste0(dir,"/work/Output-1/0.Site-info/")

metadata_file_name <- "names of treatments per site 2025 metadata and other info.xlsx"

soils_folder <- "/6.Soil_Data"
compiled_folder <- "/Compiled_Data/"


#################################################################################
# --- Paths for shape files ---
#################################################################################
## Note some samples are taken as a baseline other are pre season
sampling_timing <- ifelse(site_number_input <= 6, "Pre_Season", "Baseline")
year_sampling <- 26

soil_test <- "Pre sowing"


N_Soil_test_results_source <- readxl::read_excel(
  paste0(metadata_path,metadata_file_name),
  sheet = "Soil_sampling_files_location") %>%
  filter(Site == site_number)  %>%
  filter(variable == paste0(sampling_timing, "_Soil_Sampling_MinN_profile_data_", year_sampling )) %>% 
  pull("file path")


H2O_Soil_test_results_source <- readxl::read_excel(
  paste0(metadata_path,metadata_file_name),
  sheet = "Soil_sampling_files_location") %>%
  filter(Site == site_number)  %>%
  filter(variable == paste0(sampling_timing, "_Soil_Sampling_soil_water_profile_data_", year_sampling )) %>% 
  pull("file path")



N_soil_results   <- read_csv(paste0(headDir,N_Soil_test_results_source))
H20_soil_results   <- read_csv(paste0(headDir,H2O_Soil_test_results_source))

#################################################################################
## match the clm headings 
N_soil_results <- N_soil_results %>%
  rename(Value = MinN_kg_ha) %>%
  mutate(Soil_test = "MinN_kg_ha")

H20_soil_results <- H20_soil_results %>%
  rename(Value = Soil_water_mm_profile) %>%
  mutate(Soil_test = "Soil_water_mm_profile")

################################################################################
Pre_sowing_soil_results <- rbind(N_soil_results, H20_soil_results)



out_file <- paste0(headDir, soils_folder, "/Compiled_Data/", 
                   paste0("Pre_sowing", "_N_and_H2O_", year_sampling), 
                   ".csv")

write.csv(Pre_sowing_soil_results, out_file, row.names = FALSE)
