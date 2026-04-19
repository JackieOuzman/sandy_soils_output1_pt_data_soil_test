## mapping soil N profile data
library(leaflet)
library(sf)
library(htmlwidgets)
library(dplyr)
library(knitr)


################################################################################
########################            Define the directory              ##########
################################################################################

# site_number <- "7.Wharminda_Bonanza"
# site_name <- "Wharminda_Bonanza"

site_number <- "4.Wharminda_Woodys"
site_name <- "Wharminda_Woodys"

dir <- "//fs1-cbr.nexus.csiro.au/{af-sandysoils-ii}"
headDir <- paste0(dir, "/work/Output-1/", site_number)
metadata_path <- paste0(dir,"/work/Output-1/0.Site-info/")

metadata_file_name <- "names of treatments per site 2025 metadata and other info.xlsx"

crs_used <- 4326 # Name: WGS 84 (World Geodetic System 1984) Type: Geographic coordinate system (latitude/longitude)
projetion_crs <- 7854 #GDA2020 / MGA Zone 54 (EPSG:7854).



# --- Paths for shapefiles ---

#This will be the template 
# shapefile_source <- readxl::read_excel(
#   paste0(metadata_path,metadata_file_name),
#   sheet = "file location etc") %>% 
#   filter(Site == site_number)  %>% 
#   filter(variable == paste0(analysis.type, " shp file" ))
# 
# zones_shapefile_source <- paste0(headDir, "/3.Covariates/6.Clusters_Zones/FINAL/Opt_Clusters_7854.shp")
# boundary_shapefile_source <- paste0(headDir, "/1.Paddock_Boundary/Verran_1_boundary.shp")
# sampling_pts_shapefile_source <- paste0(headDir, "/4.Sampling/1.Baseline/ACTUAL/WHA_BON_soil_profile_N_with_zones.shp")

zones_shapefile_source <- paste0(headDir, "/3.Covariates/6.Clusters_Zones/FINAL/WOD_4zones_syd_7854.shp")
boundary_shapefile_source <- paste0(headDir, "/1.Paddock_Boundary/Wharminda_Boundary_Masked_7854.shp")
sampling_pts_shapefile_source <- paste0(headDir, "/4.Sampling/2.InSeason/26/Pre_season_Soil_Sampling/ACTUAL/Centered/WHA_WOD_Pre_seas_soil_Actual_Cent_zone_SoilN.shp")


# --- Read for shapefiles ---
zones <- st_read(zones_shapefile_source)
bounadry    <- st_read(boundary_shapefile_source)
sampling_pts   <- st_read(sampling_pts_shapefile_source)

# --- Reproject to WGS84 (leaflet requires this) ---
zones <- st_transform(zones, 4326)
bounadry    <- st_transform(bounadry,    4326)
sampling_pts   <- st_transform(sampling_pts,   4326)

## check
# st_crs(zones)$epsg
# st_crs(bounadry)$epsg
# st_crs(sampling_pts)$epsg
# st_is_valid(zones)
# st_is_valid(bounadry)
# zones    <- st_make_valid(zones)
# bounadry <- st_make_valid(bounadry)


#### summary of soil N by zones

summary_tbl <- sampling_pts %>%
  st_drop_geometry() %>%           # drop spatial info, just need the data
  group_by(Zone) %>%
  summarise(
    n         = n(),
    mean_N    = round(mean(Min_N__kg_, na.rm = TRUE), 2)  
  ) %>%
  arrange(Zone)
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


#### colours for the map
## zones
pal_zones <- colorFactor(
  palette = c("#404040", "#2F4F4F", "#F5F5DC", "#696969"),
  levels  = c(1, 2, 3, 4)
)


zones


# --- Build the map ---
map <- leaflet()  %>% 
  
  addProviderTiles("CartoDB.Positron",  group = "Light basemap") %>% 
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>% 
  
  addPolygons(
    data        = zones,
    fillColor   = ~pal_zones(fcl_mdl),
    fillOpacity = 0.6,
    color       = "black",
    weight      = 1,
    popup       = ~paste0("<b>Zone: ", fcl_mdl, "</b>"),
    group       = "Polygons"
  )  %>% 
  
  addPolygons(
    data        = bounadry,
    fillOpacity = 0,
    color       = "black",
    weight      = 2,
    group       = "Boundary"
  ) %>% 
  addCircleMarkers(
    data        = sampling_pts,
    radius      = 5,
    color       = "darkred",
    fillColor   = "darkred",
    fillOpacity = 0.8,
    weight      = 1,
    popup       = ~paste0("<b>Min N profile: ", Min_N__kg_, "</b>"),  # replace YOUR_ID_FIELD
    group       = "Sampling points"
  ) %>%
  
   addLegend(
    position = "bottomright",
    colors   = c("#404040", "#696969", "#F5F5DC"),
    labels   = c("Zone 1", "Zone 2", "Zone 3"),
    title    = "Zone",
    opacity  = 0.6        # 
  )  %>% 
  
  
  addLayersControl(
    baseGroups    = c("Light basemap", "Satellite"),
    overlayGroups = c("Zone", "Boundary", "Sampling points"),
    options       = layersControlOptions(collapsed = FALSE)
  )%>% 
  
  addScaleBar(position = "bottomleft")

map <- map %>%
  addControl(
    html     = summary_tbl_html,
    position = "topright"
  )

map

# --- Save as standalone HTML ---

# saveWidget(map, file = paste0(headDir, "/4.Sampling/1.Baseline/ACTUAL/WHA_BON_soilN_profile_Base.html")
#            , selfcontained = TRUE)


saveWidget(map, file = paste0(headDir, "/4.Sampling/2.InSeason/26/Pre_season_Soil_Sampling/ACTUAL/WHA_Wood_soilN_profile_PreSeason.html")
           , selfcontained = TRUE)


