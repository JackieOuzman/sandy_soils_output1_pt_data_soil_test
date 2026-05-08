## mapping soil N profile data
library(leaflet)
library(sf)
library(htmlwidgets)
library(dplyr)
library(knitr)
library(readxl)
#install.packages("mapview")
library(mapview)
library(readr)
library(kableExtra)


################################################################################
########################            Define the directory              ##########
################################################################################
# site_number <- "1.Walpeup_MRS125"
# site_name <- "Walpeup_MRS125"
# site_name_display <- "Walpeup MRS125"

# site_number <- "2.Crystal_Brook_Brians_House"
# site_name <- "Crystal_Brook_Brians_House"
# site_name_display <- "Crystal Brook Brians House"


site_number <- "3.Wynarka_Mervs_West"
site_name <- "Wynarka_Mervs_West"
site_name_display <- "Wynarka Mervs West"


# site_number        <- "4.Wharminda_Woodys"
# site_name          <- "Wharminda_Woodys"
# site_name_display  <-  "Wharminda Woodys"


# site_number <-  "5.Walpeup_Gums"
# site_name   <-  "Walpeup_Gums"
# site_name_display <- "Walpeup Gums"

# site_number <-  "6.Crystal_Brook_Randals"
# site_name   <-  "Crystal_Brook_Randals"
# site_name_display <- "Crystal Brook Randals"


### NEW SITES WITH NO TRIAL YET
# site_number <- "7.Wharminda_Bonanza"
# site_name <- "Wharminda_Bonanza"
# site_name_display  <-  "Wharminda Bonanza"

# site_number <- "8.Wynarka_Tanks"
# site_name <- "Wynarka_Tanks"
# site_name_display  <-  "Wynarka Tanks"





dir <- "//fs1-cbr.nexus.csiro.au/{af-sandysoils-ii}"
headDir <- paste0(dir, "/work/Output-1/", site_number)
metadata_path <- paste0(dir,"/work/Output-1/0.Site-info/")

metadata_file_name <- "names of treatments per site 2025 metadata and other info.xlsx"

crs_used <- 4326 # Name: WGS 84 (World Geodetic System 1984) Type: Geographic coordinate system (latitude/longitude)
projetion_crs <- 7854 #GDA2020 / MGA Zone 54 (EPSG:7854).


#################################################################################
# --- Paths for shape files ---
#################################################################################
## Note some samples are taken as a baseline other are pre season
sampling_timing <- "Pre_Season" 
#sampling_timing <- "Baseline" #BON and TAN
year_sampling <- 26
soil_test <- "Mineral N profile"


zones_shapefile_source <- readxl::read_excel(
  paste0(metadata_path,metadata_file_name),
  sheet = "file location etc") %>%
  filter(Site == site_number)  %>%
  filter(variable == "location of zone shp" ) %>% 
  pull("file path")

boundary_shapefile_source <- readxl::read_excel(
  paste0(metadata_path,metadata_file_name),
  sheet = "file location etc") %>%
  filter(Site == site_number)  %>%
  filter(variable == "boundary_shapefile" ) %>% 
  pull("file path")

trial_shapefile_source<- readxl::read_excel(
  paste0(metadata_path,metadata_file_name),
  sheet = "file location etc") %>%
  filter(Site == site_number)  %>%
  filter(variable == "trial.plan" ) %>% 
  pull("file path")

sampling_pts_shapefile_source <- readxl::read_excel(
  paste0(metadata_path,metadata_file_name),
  sheet = "Soil_sampling_files_location") %>%
  filter(Site == site_number)  %>%
  filter(variable == paste0(sampling_timing, "_Soil_Sampling_Location_", year_sampling )) %>% 
  pull("file path")

Soil_test_results_source <- readxl::read_excel(
  paste0(metadata_path,metadata_file_name),
  sheet = "Soil_sampling_files_location") %>%
  filter(Site == site_number)  %>%
  filter(variable == paste0(sampling_timing, "_Soil_Sampling_MinN_profile_data_", year_sampling )) %>% 
  pull("file path")












#################################################################################
# --- Read for shapefiles ---
zones <- st_read(paste0(headDir, zones_shapefile_source))
bounadry    <- st_read(paste0(headDir,boundary_shapefile_source))
trial    <- st_read(paste0(headDir,trial_shapefile_source)) #BON and #TAN no trial yet
sampling_pts   <- st_read(paste0(headDir,sampling_pts_shapefile_source))
soil_results   <- read_csv(paste0(headDir,Soil_test_results_source))
#################################################################################
#Fix the sampling_pts names 
# sampling_pts <- sampling_pts %>%
#   mutate(field_1 = as.numeric(gsub("TAN", "", Sample)))
# str(sampling_pts)

#################################################################################
### Join the sampling pts to the soil test results
#################################################################################
names(soil_results)
names(sampling_pts)

#join sampling location to results


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

soil_results_plus_location <- left_join(
  sampling_pts,
  soil_results,
  join_by(!!sym(join_col) == SampleNameShort)
)


## Tidy up so that I am not using any predefined zones
names(soil_results_plus_location)


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

names(soil_results_plus_location)

soil_results_plus_location <- soil_results_plus_location %>%
  dplyr::select(all_of(id_col), "MinN_kg_ha", "SamplingDate", "ProfileDepth", "geometry") %>%
  rename(ID = !!sym(id_col))


#join zone type
soil_results_plus_location_zone <-st_join(soil_results_plus_location, zones, 
                                          join = st_within)  

#join trial / treatment strip 
soil_results_plus_location_zone_strip <-st_join(soil_results_plus_location_zone, 
                                                trial, 
                                                join = st_within)  

### some site wont have treatment yet if this happens#BON has not got a trial yet
#soil_results_plus_location_zone_strip <- soil_results_plus_location_zone

names(soil_results_plus_location_zone_strip)
## rename some clms and tidy up


zone_col <- case_when(
  site_number == "1.Walpeup_MRS125"             ~ "gridcode",
  site_number == "2.Crystal_Brook_Brians_House"  ~ "cluster",
  site_number == "3.Wynarka_Mervs_West"          ~ "fcl_mdl", # update when known
  site_number == "4.Wharminda_Woodys"            ~ "fcl_mdl", 
  site_number == "5.Walpeup_Gums"               ~ "cluster3",
  site_number == "6.Crystal_Brook_Randals"       ~ "cluster",
  site_number == "7.Wharminda_Bonanza"           ~ "DN",
  site_number == "8.Wynarka_Tanks"               ~ "zone",
  TRUE ~ NA_character_
)
if (is.na(zone_col)) stop(paste("Zone column not defined for site_number:", site_number))

soil_results_plus_location_zone_strip <- soil_results_plus_location_zone_strip %>%
  rename(zone = !!sym(zone_col)) %>%
  dplyr::select("ID", "MinN_kg_ha", "SamplingDate", "ProfileDepth", "zone", 
                "treat_desc",
                "geometry")


names(soil_results_plus_location_zone_strip)

## rename the zone clm 
zones <- zones %>% dplyr::rename(zone = !!sym(zone_col))



#################################################################################
# --- Reproject to WGS84 (leaflet requires this) ---
#################################################################################

zones         <- st_transform(zones, 4326)
bounadry      <- st_transform(bounadry, 4326)
sampling_pts  <- st_transform(soil_results_plus_location_zone_strip, 4326)
trial         <- st_transform(trial, 4326)


#################################################################################
#### summary of soil N by zones
#################################################################################


summary_tbl <- sampling_pts %>%
  st_drop_geometry() %>%           # drop spatial info, just need the data
  group_by(zone) %>%
  summarise(
    n         = n(),
    mean_N    = round(mean(MinN_kg_ha, na.rm = TRUE), 2)  
  ) %>%
  arrange(zone)
#Format it as an HTML table for the map
summary_tbl_html <- kable(summary_tbl,
                  format   = "html",
                  col.names = c("Zone", "Count", "Mean Soil N kg/ha"),
                  align    = "c") %>%
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "condensed"),
    full_width        = FALSE,
    font_size         = 12
  )

summary_tbl_html

#################################################################################
#### colors for the map
#################################################################################
## zones

zone_details_details <- readxl::read_excel(
  paste0(metadata_path,metadata_file_name),
  sheet = "zone_details") %>%
  filter(Site == site_number)  
  
palette_zone <- setNames(zone_details_details$`Hex Code`,
                    zone_details_details$`zone names`  )

pal_zones <- colorFactor(
  palette = palette_zone,
  levels  = names(palette_zone)
)

## strips
strips_details_details <- readxl::read_excel(
  paste0(metadata_path,metadata_file_name),
  sheet = "treatment names") %>%
  filter(Site == site_number)  

palette_strip <- setNames(strips_details_details$Hex,
                          strips_details_details$`Treatment Name`)


pal_strip <- colorFactor(
  palette = palette_strip,
  levels  = names(palette_strip)
)


#################################################################################
# --- Build the map ---
#################################################################################
sampling_pts_filter <- sampling_pts %>% 
  dplyr::filter(!is.na(SamplingDate)) %>%
  dplyr::mutate(SamplingDate = format(as.Date(SamplingDate, "%d-%m-%Y"), "%d %B %Y"))


map <- leaflet()  %>% 
  
  addProviderTiles("CartoDB.Positron",  group = "Light basemap") %>% 
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>% 
  
  addPolygons(
    data        = bounadry,
    fillOpacity = 0,
    color       = "black",
    weight      = 2,
    group       = "Boundary"
  ) %>% 
  
  addPolygons(
    data        = zones,
    fillColor   = ~pal_zones(zone),
    fillOpacity = 0.6,
    color       = "black",
    weight      = 1,
    popup       = ~paste0("<b>Zone: ", zone, "</b>"),
    group       = "Zone"
  )  %>% 
  
  #some sites dont have strips yet
  addPolygons(
    data        = trial,
    fillColor   = ~pal_strip(treat_desc),
    fillOpacity = 0.6,
    color       = "black",
    weight      = 1,
    popup       = ~paste0("<b>Treatments: ", treat_desc, "</b>"),
    group       = "Strips"
  )%>%
  
  addCircleMarkers(
    data        = sampling_pts_filter,
    radius      = 5,
    color       = "darkred",
    fillColor   = "darkred",
    fillOpacity = 0.8,
    weight      = 1,
    popup       = ~paste0("<b>Sample ID: ", ID, "</b><br>Min N profile: ", MinN_kg_ha),   
    group       = "Sampling points"
  ) %>%
  
  addLegend(
    position = "bottomright",
    colors   = unname(palette_zone),
    labels   = zone_details_details$`zone label names`, #assumes same order
    title    = "Zone",
    opacity  = 0.6
  )  %>% 
  
 # # some sites dont have strips yet
  addLegend(
    position = "bottomright",
    colors   = unname(palette_strip),
    labels   = strips_details_details$`Treatment Name`,
    title    = "Treatment",
    opacity  = 0.6
  ) %>%
  
  addLayersControl(
    baseGroups    = c("Light basemap", "Satellite"),
    overlayGroups = c("Zone", "Strips", "Sampling points"),
    #overlayGroups = c("Zone", "Sampling points"),
    options       = layersControlOptions(collapsed = FALSE)
  )%>% 
  
  addScaleBar(position = "bottomleft")






map <- map %>%
  addControl(
    html     = paste0('<div style="background:white; padding:6px 10px; border-radius:4px; font-size:14px; font-weight:bold;">',
                      site_name_display, ' - ', sampling_timing, ' ', soil_test,
                      '</div>'),
    position = "topleft"
  ) %>%
  addControl(
    html     = summary_tbl_html,
    position = "bottomright"
  ) %>%
  addControl(
    html     = paste0('<div style="background:white; padding:4px 8px; border-radius:4px; font-size:11px; color:#555;">',
                      'Created: ', format(Sys.Date(), "%d %B %Y"),
                      '. Sampling date: ', unique(sampling_pts_filter$SamplingDate),
                      '. Sampling depth: ', unique(sampling_pts_filter$ProfileDepth),
                      '</div>'),
    position = "bottomleft"
  )

map

# --- Save as standalone HTML ---? somehow this is not working as well

name_of_map <- paste0(site_name, "_", sampling_timing, "_", soil_test, "_.html")
name_of_data <- paste0(site_name, "_", sampling_timing, "_", soil_test, "_.csv")
save_location <- paste0(headDir, "/6.Soil_Data/Compiled_Data/R_maps/")

saveWidget(map,
           file          = paste0(save_location, name_of_map),
           selfcontained = TRUE,
           libdir        = NULL)


### save the results 
sampling_pts


# st_drop_geometry(sampling_pts) %>%
#   write_csv(paste0(paste0(save_location, name_of_data)))


st_coordinates(sampling_pts) %>%
  as.data.frame() %>%
  bind_cols(st_drop_geometry(sampling_pts)) %>%
  mutate(CRS = st_crs(sampling_pts)$input) %>%
  write_csv(paste0(save_location, name_of_data))
