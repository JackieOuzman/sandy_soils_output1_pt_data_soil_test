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


#install.packages("ggnewscale")
library(ggnewscale)
library(ggplot2)
library(ggspatial)

################################################################################
########################            Define the directory              ##########
################################################################################
site_number_input <- 1  # <-- change this number only

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

crs_used <- 4326 # Name: WGS 84 (World Geodetic System 1984) Type: Geographic coordinate system (latitude/longitude)
projetion_crs <- 7854 #GDA2020 / MGA Zone 54 (EPSG:7854).


#################################################################################
# --- Paths for shape files ---
#################################################################################
## Note some samples are taken as a baseline other are pre season
sampling_timing <- ifelse(site_number_input <= 6, "Pre_Season", "Baseline")
year_sampling <- 26

soil_test <- "Pre sowing"

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


Soil_test_results_source <- "/6.Soil_Data/Compiled_Data/Pre_sowing_N_and_H2O_26.csv"









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
  dplyr::select(all_of(id_col), "Value", "SamplingDate","Soil_test", "ProfileDepth", "geometry") %>%
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

names(soil_results_plus_location_zone_strip)

soil_results_plus_location_zone_strip <- soil_results_plus_location_zone_strip %>%
  rename(zone = !!sym(zone_col)) %>%
  dplyr::select("ID", "Value", "Soil_test", "SamplingDate", "ProfileDepth", "zone", 
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
  group_by(zone, Soil_test) %>%
  summarise(
    n         = n(),
    mean_N    = round(mean(Value, na.rm = TRUE), 2)  
  ) %>%
  arrange(zone)
#pivot wider to get N and H2O in own clms
summary_tbl_wide <- summary_tbl %>%
  pivot_wider(
    names_from  = Soil_test,
    values_from = c(n, mean_N)
  ) %>%
  select(zone, n_MinN_kg_ha, mean_N_MinN_kg_ha, mean_N_Soil_water_mm_profile) %>%
  rename(
    Zone          = zone,
    Count         = n_MinN_kg_ha,
    `Mean Min N (kg/ha)`      = mean_N_MinN_kg_ha,
    `Mean Soil Water (mm)`    = mean_N_Soil_water_mm_profile
  )

summary_tbl_html <- kable(summary_tbl_wide,
                          format    = "html",
                          align     = "c") %>%
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
### Mapping helpers ############################################################

sampling_pts_filter <- sampling_pts %>% 
  dplyr::filter(!is.na(SamplingDate)) %>%
  dplyr::mutate(SamplingDate = format(as.Date(SamplingDate, "%d-%m-%Y"), "%d %B %Y"))

sampling_pts_filter_wide <- sampling_pts_filter %>%
  pivot_wider(
    names_from  = Soil_test,
    values_from = Value
  )



# Classify MinN into Low/Med/High
sampling_pts_filter_wide <- sampling_pts_filter_wide %>%
  mutate(MinN_class = case_when(
    MinN_kg_ha < 40              ~ "Low",
    MinN_kg_ha >= 40 & MinN_kg_ha <= 70 ~ "Medium",
    MinN_kg_ha > 70              ~ "High"
  ))

# Palette
pal_minN <- colorFactor(
  palette = c("Low" = "#d7191c", "Medium" = "#fdae61", "High" = "#1a9641"),
  levels  = c("Low", "Medium", "High"),
  domain  = sampling_pts_filter_wide$MinN_class
)

##############################################################################
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
    data        = sampling_pts_filter_wide,
    radius      = 5,
    color       = ~pal_minN(MinN_class),
    fillColor   = ~pal_minN(MinN_class),
    fillOpacity = 0.8,
    weight      = 1,
    popup       = ~paste0(
      "<b>Sample ID: ", ID, "</b><br>",
      "Date: ", SamplingDate, "<br>",
      "Zone: ", zone, "<br>",
      "Treatment: ", treat_desc, "<br>",
      "Min N (kg/ha): ", round(MinN_kg_ha, 1), "<br>",
      "Soil Water (mm): ", round(Soil_water_mm_profile, 1)
    ),
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
  addLegend(
    position = "bottomright",
    pal      = pal_minN,
    values   = factor(sampling_pts_filter_wide$MinN_class, levels = c("Low", "Medium", "High")),
    title    = "Min N (kg/ha)",
    opacity  = 0.8
  ) %>%
  
  addScaleBar(position = "bottomleft")






map <- map %>%
  addControl(
    html     = paste0('<div style="background:white; padding:6px 10px; border-radius:4px; font-size:14px; font-weight:bold;">',
                      site_name_display, ' - ',  soil_test,
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
                      #'. Sampling depth: ', unique(sampling_pts_filter$ProfileDepth), #removed the depth info
                      '</div>'),
    position = "bottomleft"
  )

map

# --- Save as standalone HTML ---? somehow this is not working as well

name_of_map <- paste0(site_name, "_", "Pre Season", "_N_and_H2O_", year_sampling, "_.html")
name_of_data <- paste0(site_name, "_", "Pre Season", "_N_and_H2O_", year_sampling, "_.csv")
save_location <- paste0(headDir, "/6.Soil_Data/Compiled_Data/R_maps/")

saveWidget(map,
           file          = paste0(save_location, name_of_map),
           selfcontained = TRUE,
           libdir        = NULL)


### save the results 
sampling_pts_filter_wide



st_coordinates(sampling_pts_filter_wide) %>%
  as.data.frame() %>%
  bind_cols(st_drop_geometry(sampling_pts_filter_wide)) %>%
  mutate(CRS = st_crs(sampling_pts_filter_wide)$input) %>%
  write_csv(paste0(save_location, name_of_data))


################################################################################
### static map ###

static_map <- ggplot() +
  
  # Basemap tiles
  annotation_map_tile(type = "cartolight", zoom = 15, quiet = TRUE) +
  
  # Zones
  geom_sf(data = zones, aes(fill = as.factor(zone)),
          alpha = 0.5, color = "black", linewidth = 0.3) +
  scale_fill_manual(name   = "Zone",
                    values = palette_zone,
                    labels = zone_details_details$`zone label names`) +
  
  # New scale for strips
  ggnewscale::new_scale_fill() +
  
  # Strips
  geom_sf(data = trial, aes(fill = treat_desc),
          alpha = 0.6, color = "black", linewidth = 0.3) +
  scale_fill_manual(name   = "Treatment",
                    values = palette_strip,
                    labels = strips_details_details$`Treatment Name`) +
  
  # Boundary
  geom_sf(data = bounadry,
          fill = NA, color = "black", linewidth = 0.6) +
  
  # New scale for points
  ggnewscale::new_scale_fill() +
  ggnewscale::new_scale_color() +
  
  # Sampling points
  geom_sf(data = sampling_pts_filter_wide, aes(fill = MinN_class, color = MinN_class),
          shape = 21, size = 3, stroke = 0.8) +
  scale_fill_manual(name   = "Min N (kg/ha)",
                    values = c("Low" = "#d7191c", "Medium" = "#fdae61", "High" = "#1a9641"),
                    breaks = c("Low", "Medium", "High")) +
  scale_color_manual(name  = "Min N (kg/ha)",
                     values = c("Low" = "#d7191c", "Medium" = "#fdae61", "High" = "#1a9641"),
                     breaks = c("Low", "Medium", "High")) +
  
  # Sample ID labels
  geom_sf_text(data = sampling_pts_filter_wide, aes(label = ID),
               size = 2.5, fontface = "bold",
               nudge_y = 0.00015) +
  
  # Scale bar and north arrow
  annotation_scale(location = "bl", width_hint = 0.2) +
  annotation_north_arrow(location = "bl", 
                         pad_y = unit(0.5, "cm"),
                         style = north_arrow_minimal()) +
  
  # Title and caption
  labs(
    title   = paste0(site_name_display, " - ", soil_test),
    caption = paste0("Created: ", format(Sys.Date(), "%d %B %Y"),
                     ". Sampling date: ", unique(sampling_pts_filter$SamplingDate))
  ) +
  
  theme_void() +
  theme(
    plot.title   = element_text(face = "bold", size = 13, hjust = 0),
    plot.caption = element_text(size = 8, color = "grey50", hjust = 0),
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 9),
    legend.text  = element_text(size = 8)
  )

static_map




ggsave(
  filename = file.path(save_location, paste0(site_name, "_Pre Season", "_N_and_H2O_", year_sampling, "_static_map.png")),
  plot     = static_map,
  width    = 10,
  height   = 8,
  dpi      = 300,
  units    = "in"
)
