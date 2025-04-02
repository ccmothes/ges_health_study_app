#' =============================================================================
#' Project: GES Health Study (ENVIRONS)
#' Date Created: July 18, 2024
#' Author: Sheena Martenies
#' Contact: smarte4@illinois.edu
#' 
#' Description: Cleaning up spatial data that can be mapped in the Shiny app
#' Update 9/27: Adding environmental data (priority for CC over ACS data)
#' Update 11/26: Finalizing env data sets for the next round of CC feedback
#' Update 12/12: Adding CDC PLACES and Colorado Enviroscreen data for a new tab 
#' mapping health outcomes
#' =============================================================================

library(sf)
library(raster)
library(tidycensus)
library(tigris)
library(tidyverse)
library(readxl)

census_api_key(census_api_key)

#' CRS strings
albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
ll_nad83 <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
ll_wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

#' Denver county census tracts
#' Remove DIA airport tract (GEOID = "08031980000" or "08031980001")
tracts10 <- tracts(state = 08, county = 31, cb = T, year = 2010) %>%
  mutate(GEOID = str_remove(GEO_ID, "1400000US")) %>%
  filter(GEOID != "08031980000") %>% 
  st_transform(crs = ll_wgs84)
tracts20 <- tracts(state = 08, county = 31, cb = T, year = 2020) %>%
  filter(GEOID != "08031980001") %>%
  st_transform(crs = ll_wgs84)

#' Statistical neighborhoods
neighborhoods <- st_read(here::here("data/shapefiles", 
                                    "ADMN_NEIGHBORHOOD_A.shp")) %>%
  st_transform(ll_wgs84) %>%
  filter(NBHD_NAME != "DIA") %>%
  mutate(NBHD_NAME = ifelse(NBHD_NAME %in% c("Globeville", "Elyria Swansea"),
                            "Globeville - Elyria Swansea", NBHD_NAME)) %>%
  group_by(NBHD_NAME) %>%
  summarize() 
glimpse(neighborhoods)
plot(st_geometry(neighborhoods))
neighborhoods$NBHD_NAME

#' GES census tracts
ges_tract10_ids <- c("08031001500", "08031003500")
ges_tracts10 <- filter(tracts10, GEOID %in% ges_tract10_ids)
plot(st_geometry(ges_tracts10))

ges_tract20_ids <- c("08031001500", "08031003501", "08031003502")
ges_tracts20 <- filter(tracts20, GEOID %in% ges_tract20_ids)
plot(st_geometry(ges_tracts20))

#' City and County of Denver boundary
denver <- counties(state = "08", year = 2020, cb = T) %>%
  st_transform(ll_wgs84) %>%
  filter(NAME == "Denver")
plot(st_geometry(denver))

#' GES (all tracts combined)
ges <- ges_tracts20 %>%
  summarize()
plot(st_geometry(ges))

#' Major highways
highways <- primary_roads() %>%
  filter(FULLNAME %in% c("I- 25", "I- 70")) %>%
  st_transform(ll_wgs84) %>%
  group_by(FULLNAME) %>%
  summarize()
plot(st_geometry(highways))

# Box for Denver
den_box <- tibble(lon = c(-105.1, -105.1, -104.75, -104.75),
                  lat = c(39.61, 39.83, 39.61, 39.83)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = ll_wgs84) %>%
  st_bbox() %>% 
  st_as_sfc()
plot(st_geometry(den_box))

#' Denver highways
den_highways <- st_intersection(highways, den_box)
plot(st_geometry(den_box))
plot(st_geometry(den_highways), add = T)

den_hw_labels <- tibble(name = c("I-70", "I-25"),
                        lon = c(-104.80, -105.00),
                        lat = c(39.76, 39.81)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = ll_wgs84)

plot(st_geometry(den_box))
plot(st_geometry(den_highways), add = T)
plot(st_geometry(den_hw_labels), add = T)
plot(st_geometry(tracts10), add = T)

#' Box for GES
ges_box <- tibble(lon = c(-105.005, -105.005, -104.93, -104.93),
                  lat = c(39.76, 39.8, 39.76, 39.8)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = ll_wgs84) %>%
  st_bbox() %>% 
  st_as_sfc()
plot(st_geometry(ges_box))
plot(st_geometry(ges_tracts20), add = T)

#' GES Highways
ges_highways <- st_intersection(highways, ges_box)
plot(st_geometry(ges_box))
plot(st_geometry(ges_highways), add = T)

ges_hw_labels <- tibble(name = c("I-70", "I-25"),
                        lon = c(-104.935, -104.991),
                        lat = c(39.782, 39.795)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = ll_wgs84)

plot(st_geometry(ges_box))
plot(st_geometry(ges_highways), add = T)
plot(st_geometry(ges_hw_labels), add = T)
plot(st_geometry(ges), add = T)

#'----------------------------------------------
#'Summarize environmental data at the census 
#'tract and neighborhood level
#'
#'Starting with alcohol and cannabis, transportation
#'noise, and tree equity as a working example
#'for the shiny app
#'
#'Adding additional data sets for the next version
#'of the app
#'----------------------------------------------

#' alcohol + cannabis ratios
alc_cannabis_ratio <- st_read(here::here("data", "shapefiles", 
                                         "Alc_MJ_Ratios.shp")) %>%
  mutate(NBHD_NAME2 = ifelse(NBHD_NAME %in% c("Globeville", "Elyria Swansea"),
                             "Globeville - Elyria Swansea",
                             NBHD_NAME)) %>%
  st_set_geometry(NULL) %>%
  group_by(NBHD_NAME2) %>%
  summarize(Alc_to_foo = mean(Alc_to_foo),
            MJstores_t = mean(MJstores_t)) %>%
  dplyr::select(NBHD_NAME = NBHD_NAME2, 
                alc_to_food = Alc_to_foo, can_to_food = MJstores_t) 

alc_cannabis_ratio2 <- neighborhoods %>%
  left_join(alc_cannabis_ratio, by = "NBHD_NAME")
   
ggplot(alc_cannabis_ratio2) + geom_sf(aes(fill = alc_to_food))
ggplot(alc_cannabis_ratio2) + geom_sf(aes(fill = can_to_food))

#' Transportation noise
tran_noise <- raster(here::here("data", "shapefiles",
                                "CO_rail_road_and_aviation_noise_2020.tif"))
tran_noise
nbhd_noise <- st_transform(neighborhoods, 
                           crs = "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")
nbhd_noise$transport_noise <- raster::extract(tran_noise, nbhd_noise, 
                                              fun = mean, na.rm = T)[,1]
nbhd_noise2 <- st_transform(nbhd_noise, crs = ll_wgs84)

ggplot(nbhd_noise2) + geom_sf(aes(fill = transport_noise))

#' Tree equity
tree_equity <- st_read(here::here("data", "shapefiles",
                                  "co_tree_equity.shp")) %>%
  filter(county == "Denver County") %>%
  st_transform(crs = ll_wgs84) %>%
  st_join(neighborhoods, largest = T)
plot(st_geometry(tree_equity))

test1 <- filter(tree_equity, NBHD_NAME == "Globeville - Elyria Swansea")
test2 <- filter(neighborhoods, NBHD_NAME == "Globeville - Elyria Swansea")

ggplot() +
  geom_sf(data = test1) +
  geom_sf(data = test2, fill = NA, color = "red")

nbhd_tree_equity <- tree_equity %>%
  st_set_geometry(NULL) %>%
  dplyr::select(NBHD_NAME, tes) %>%
  group_by(NBHD_NAME) %>%
  summarize(tree_equity_score = mean(tes, na.rm = T))
nbhd_tree_equity2 <- left_join(neighborhoods, nbhd_tree_equity, by = "NBHD_NAME")

ggplot(nbhd_tree_equity2) + geom_sf(aes(fill = tree_equity_score))

#' Grocery store access by neighborhood
grocery_access <- st_read(here::here("data", "shapefiles",
                                  "limited_food_access_by_neighborhood___2021.shp")) %>%
  st_set_geometry(NULL) %>%
  mutate(NBHD_NAME = ifelse(NBRHD_NAME %in% c("Globeville", "Elyria Swansea"),
                            "Globeville - Elyria Swansea",
                            NBRHD_NAME)) %>%
  group_by(NBHD_NAME) %>%
  summarize(grocery_store_access = mean(PCTNBRHD_W))

grocery_access2 <- neighborhoods %>%
  left_join(grocery_access, by = "NBHD_NAME")

ggplot(grocery_access2) + geom_sf(aes(fill = grocery_store_access))

#' Street temperature percentiles
street_temp <- st_read(here::here("data", "shapefiles",
                                  "street_temp_percentile.shp")) %>%
  st_transform(st_crs(neighborhoods))
nbhd_street_temp <- st_join(street_temp, neighborhoods, largest = T) 
nbhd_street_temp1 <- st_set_geometry(nbhd_street_temp, NULL) %>%
  group_by(NBHD_NAME) %>%
  summarize(street_temp_pctile = mean(STREET_TEM))

nbhd_street_temp2 <- neighborhoods %>%
  left_join(nbhd_street_temp1, by = "NBHD_NAME")

ggplot(nbhd_street_temp2) + geom_sf(aes(fill = street_temp_pctile))

#' Beth's air pollution estimates
nbhd_air_pol <- read_csv(here::here("data", "IDW_nbhd_level_2017_2021.csv")) %>%
  select(NBHD_NAME, contains("_m1")) %>%
  mutate(NBHD_NAME2 = ifelse(NBHD_NAME %in% c("Globeville", "Elyria Swansea"),
                             "Globeville - Elyria Swansea",
                             NBHD_NAME)) %>%
  group_by(NBHD_NAME2) %>%
  summarize(pm25 = mean(pm25_pred_m1_ugm3),
            pm10 = mean(pm10_pred_m1_ugm3),
            o3 = mean(o3_pred_m1_ppm*1000),
            no2 = mean(no2_pred_m1_ppb),
            so2 = mean(so2_pred_m1_ppb),
            co = mean(co_pred_m1_ppm)) %>%
  dplyr::select(NBHD_NAME = NBHD_NAME2, 
                pm25:co) 

#' Combined data set with all the neighborhood-level environmental exposures
nbhd_env <- neighborhoods %>%
  left_join(st_set_geometry(alc_cannabis_ratio2, NULL), by = "NBHD_NAME") %>%
  left_join(st_set_geometry(nbhd_noise2, NULL), by = "NBHD_NAME") %>%
  left_join(st_set_geometry(nbhd_tree_equity2, NULL), by = "NBHD_NAME") %>%
  left_join(st_set_geometry(grocery_access2, NULL), by = "NBHD_NAME") %>%
  left_join(st_set_geometry(nbhd_street_temp2, NULL), by = "NBHD_NAME") %>%
  left_join(nbhd_air_pol, by = "NBHD_NAME")

#'----------------------------------------------
#'Summarize health data at the census 
#'tract and neighborhood level
#'
#'USALEEP (Life expectancy), PLACES, and CO Enviroscreen data are at the census 
#'tract level
#'
#'To get neighborhood level estimates, I'm going to calculate the 
#'population-weighted mean
#'
#'Note: USALEEP and PLACES use 2010 census tract IDs
#'NOTE: 
#'----------------------------------------------

#' Census tract populations for USALEEP (2011-2015)
#' Consistent with the USALEEP methods: https://www.cdc.gov/nchs/data/series/sr_02/sr02_181.pdf
tract_pop15 <- get_acs(geography = "tract", variables = "B01001_001",
                       year = 2015, state = 08, county = 031) %>%
  select(GEOID, pop = estimate)

#' Join tract populations
life_exp <- read_csv(here::here("data", "CO_A.csv")) %>%
  filter(CNTY2KX == "031") %>%
  rename("GEOID" = `Tract ID`,
         life_exp_years = `e(0)`,
         life_exp_years_se =`se(e(0))`) %>%
  left_join(tract_pop15, by = "GEOID")
length(unique(life_exp$GEOID))

#' Join neighborhoods
life_exp_tracts_check <- tracts10 %>%
  left_join(life_exp, by = "GEOID")

ggplot() + 
  geom_sf(data = life_exp_tracts_check, aes(fill = life_exp_years)) +
  geom_sf(data = neighborhoods, fill = NA, color = "red")

life_exp_tracts <- tracts10 %>%
  left_join(life_exp, by = "GEOID") %>%
  st_transform(st_crs(neighborhoods)) %>%
  st_join(neighborhoods, largest = T)

#' Population weighted neighborhood mean life expectancy
life_exp_neighborhoods <- life_exp_tracts %>%
  st_set_geometry(NULL) %>%
  group_by(NBHD_NAME) %>%
  summarize(life_exp = weighted.mean(life_exp_years, pop, na.rm = T)) %>%
  mutate(life_exp = ifelse(is.nan(life_exp), NA, life_exp))
life_exp_neighborhoods

#' Census tract populations for PLACES (2015-2019)
#' Consistent with CDC documentation of the 2023 release:
#' https://data.cdc.gov/500-Cities-Places/PLACES-Local-Data-for-Better-Health-Place-Data-202/krqc-563j/about_data

tract_vars_19 <- load_variables(dataset = "acs5", year = "2019")
write_csv(tract_vars_19, here::here("documentation", 
                                    "acs5_2019_variable_names.csv"))

pop_vars <- c("B01001A_001", # total population
              paste0("B01001A_", str_pad(c(7:16, 22:31), pad = "0", width = 3)))

tract_pop19 <- get_acs(geography = "tract", variables = pop_vars,
                       year = 2015, state = 08, county = 031) %>%
  pivot_wider(id_cols = GEOID, names_from = "variable", values_from = "estimate") %>%
  mutate(pop_18_64 = B01001A_007 + B01001A_008 + B01001A_009 + 
           B01001A_010 + B01001A_011 + B01001A_012 + B01001A_013 +  
           B01001A_022 + B01001A_023 + B01001A_024 + B01001A_025 + 
           B01001A_026 + B01001A_027 + B01001A_028, 
         pop_18_plus = B01001A_007 + B01001A_008 + B01001A_009 + B01001A_010 +
           B01001A_011 + B01001A_012 + B01001A_013 + B01001A_014 + B01001A_015 +
           B01001A_016 + B01001A_022 + B01001A_023 + B01001A_024 + B01001A_025 +
           B01001A_026 + B01001A_027 + B01001A_028 + B01001A_029 + B01001A_030 +
           B01001A_031,
         pop_65_plus = B01001A_014 + B01001A_015 + B01001A_016 +
           B01001A_029 + B01001A_030 + B01001A_031,
         pop_50_75 = (B01001A_012 * (5/10)) + B01001A_013 + B01001A_014 +
           (B01001A_015 * (1/10)) +
           (B01001A_027 * (5/10)) + B01001A_028 + B01001A_029 +
           (B01001A_030 * (1/10)),
         pop_women_50_74 = (B01001A_027 * (1/2)) + B01001A_028 + B01001A_029,
         pop_women_21_65 = (B01001A_023 * (4/5)) + B01001A_024 + B01001A_025 +
           B01001A_026 + B01001A_027 + B01001A_028 + (B01001A_029 * (1/10)),
         pop_men_65_plus = B01001A_014 + B01001A_015 + B01001A_016) %>%
  select(GEOID, pop_18_64:pop_men_65_plus) %>%
  pivot_longer(cols = -GEOID, names_to = "pop_group", values_to = "pop")

#' Read in data and join tract populations
places <- read_csv(here::here("data",
                              "PLACES__Local_Data_for_Better_Health__Census_Tract_Data_2023_release.csv")) %>%
  filter(CountyFIPS == "08031") %>%
  mutate(GEOID = LocationName) %>%
  mutate(pop_group = case_when(str_detect(Measure, "women aged 50-74") ~ "pop_women_50_74",
                               str_detect(Measure, "women aged 21-65") ~ "pop_women_21_65",
                               str_detect(Measure, "adults aged >=18") ~ "pop_18_plus",
                               str_detect(Measure, "adults ages >=18") ~ "pop_18_plus",
                               str_detect(Measure, "adults aged 18-64") ~ "pop_18_64",
                               str_detect(Measure, "adults aged 50-75") ~ "pop_50-75",
                               str_detect(Measure, "adults aged >=65") ~ "pop_65_plus",
                               str_detect(Measure, "men aged >=65") ~ "pop_men_65_plus",
                               .default = NA)) %>%
  left_join(tract_pop19, by = c("GEOID", "pop_group"))
glimpse(places)
sum(is.na(places$pop_group))
length(unique(places$GEOID)) #143 tract IDs-- 2010 census tracts

#' Join to tracts
places_tracts_check <- tracts10 %>%
  left_join(places, by = "GEOID")

ggplot() + 
  geom_sf(data = places_tracts_check, aes(fill = pop)) +
  geom_sf(data = neighborhoods, fill = NA, color = "red")

places_tracts <- tracts10 %>%
  left_join(places, by = "GEOID") %>%
  st_transform(st_crs(neighborhoods)) %>%
  st_join(neighborhoods, largest = T)

#' Calculate population-weighted crude prevalence measures 
places_neighborhoods <- places_tracts %>%
  st_set_geometry(NULL) %>%
  group_by(NBHD_NAME, MeasureId) %>%
  summarize(prevalence = weighted.mean(Data_Value, pop, na.rm = T)) %>%
  pivot_wider(id_cols = "NBHD_NAME", names_from = "MeasureId", 
              values_from = "prevalence") %>%
  select(-c("NA"))

places_dict <- select(places, MeasureId, Measure, Data_Value_Unit, Data_Value_Type) %>%
  distinct()
write_csv(places_dict,
          here::here("documentation", "cdc_places_2023_dictionary.csv"))

#' Colorado Enviroscreen 2.0 data: asthma hospitalizations and LBW
tract_vars_22 <- load_variables(dataset = "acs5", year = "2022")
write_csv(tract_vars_22, here::here("documentation", 
                                    "acs5_2022_variable_names.csv"))

tract_pop22 <- get_acs(geography = "tract", 
                       variables = c("B01001A_001", "B01001A_003", "B01001A_018"),
                       year = 2022, state = 08, county = 031) %>%
  pivot_wider(id_cols = GEOID, names_from = "variable", values_from = "estimate") %>%
  mutate(pop_under5 = B01001A_003 + B01001A_018,
         pop_total = B01001A_001) %>%
  select(GEOID, pop_under5, pop_total) %>%
  pivot_longer(cols = -GEOID, names_to = "pop_group", values_to = "pop")

ces <- read_csv(here::here("data", 
                           "ColoradoEnviroScreen_v2_Tract_24118.xlsx - ColoradoEnviroScreen_v2_Tract.csv")) %>%
  filter(County == "Denver County")
colnames(ces) <- tolower(str_replace_all(colnames(ces), " ", "_"))
ces2 <- ces %>%
  select(GEOID = census_tract_geoid, 
         ast_hosp = asthma_hospitalization_rate,
         lbw_pct = low_birth_weight)

ces_tracts <- tracts20 %>%
  left_join(ces2, by = "GEOID") %>%
  st_transform(st_crs(neighborhoods)) %>%
  st_join(neighborhoods, largest = T)

#' Calculate population-weighted crude prevalence measures 
ces_neighborhoods <- ces_tracts %>%
  st_set_geometry(NULL) %>%
  select(NBHD_NAME, GEOID, ast_hosp, lbw_pct) %>%
  pivot_longer(cols = -c("NBHD_NAME", "GEOID"), names_to = "outcome", values_to = "rate") %>%
  mutate(pop_group = ifelse(outcome == "ast_hosp", "pop_total", "pop_under5")) %>%
  left_join(tract_pop22, by = c("GEOID", "pop_group")) %>%
  group_by(NBHD_NAME, outcome) %>%
  summarize(rate = weighted.mean(rate, pop, na.rm = T)) %>%
  mutate(rate = ifelse(is.nan(rate), NA, rate)) %>%
  pivot_wider(id_cols = "NBHD_NAME", names_from = "outcome", 
              values_from = "rate")

nbhd_health <- neighborhoods %>%
  left_join(places_neighborhoods, by = "NBHD_NAME") %>%
  left_join(life_exp_neighborhoods, by = "NBHD_NAME") %>%
  left_join(ces_neighborhoods, by = "NBHD_NAME")

#' -----------------------------------------------------------------------------
#' Save all the spatial data to a .rdata file
#' -----------------------------------------------------------------------------

save(denver, den_box, den_highways, den_hw_labels, 
     ges, ges_box, ges_highways, ges_hw_labels,
     neighborhoods, tracts10, tracts20, ges_tracts10, ges_tracts20,
     nbhd_env, nbhd_health,
     file = here::here("data", "shapefiles.rdata"))
