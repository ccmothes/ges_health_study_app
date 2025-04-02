#' =============================================================================
#' Project: GES Health Study (ENVIRONS)
#' Date Created: Sep 27, 2024
#' Author: Sheena Martenies
#' Contact: smarte4@illinois.edu
#' 
#' Description: Combining ACS data with environmental data for the app
#' Updated 12/12: Including health data in the "final" data set
#' =============================================================================

library(sf)
library(tidycensus)
library(tigris)
library(tidyverse)
library(readxl)
library(ggplot2)
library(ggspatial)
library(scales)
library(colorspace)
library(patchwork)

census_api_key(census_api_key)

#' CRS strings
albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
ll_nad83 <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
ll_wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

#' Load shapefiles
load(here::here("data", "shapefiles.rdata"))

#' Read in ACS data
load(file = here::here("data", "acs_tracts.rdata"))
load(file = here::here("data", "acs_neighborhoods.rdata"))

#' Combined neighborhood-level ACS, environmental data, and health data
nbhd_data <- acs_neighborhoods %>%
  left_join(st_set_geometry(nbhd_env, NULL), by = "NBHD_NAME") %>%
  left_join(st_set_geometry(nbhd_health, NULL), by = "NBHD_NAME") 

nbhd_data_long <- nbhd_data %>%
  pivot_longer(cols = -c(NBHD_NAME, geometry),
               names_to = "var", values_to = "value")

save(nbhd_data, nbhd_data_long,  
     file = here::here("data", "neighborhood_data.rdata"))

#' -----------------------------------------------------------------------------
#' Save everything to the app folder
#' -----------------------------------------------------------------------------

save(denver, den_box, den_highways, den_hw_labels, 
     ges, ges_box, ges_highways, ges_hw_labels,
     neighborhoods, tracts10, tracts20, ges_tracts10, ges_tracts20,
     nbhd_env, nbhd_health,
     acs_tracts, acs_tracts_long,
     acs_neighborhoods, acs_neighborhoods_long,
     nbhd_data, nbhd_data_long, 
     file = here::here("ges_health_study_app", "ges_app_data.rdata"))
