#' =============================================================================
#' Project: GES Health Study (ENVIRONS)
#' Date Created: July 18, 2024
#' Author: Sheena Martenies
#' Contact: smarte4@illinois.edu
#' 
#' Description: Summary of ACS demographic and SES variables by census tract for
#' the Shiny app. Variables are assessed at the census tract level and 
#' summarized at the neighborhood level as population-weighted means 
#' 
#' Note: updating this script for the app will require tidycensus API key. This
#' script should only by updated as needed.
#' =============================================================================

library(sf)
library(tigris)
library(tidycensus)
library(tidyverse)
library(readxl)

census_api_key(census_api_key)

#' function for estimating SE of proportions based on US Census Bureau guidance
#' https://www2.census.gov/programs-surveys/acs/tech_docs/accuracy/2022_ACS_Accuracy_Document_Worked_Examples.pdf
#' https://www2.census.gov/programs-surveys/acs/tech_docs/accuracy/MultiyearACSAccuracyofData2021.pdf?#
#' function for cleaning up estimates and reporting 95% CIs

prop <- function(num, denom) {
  prop <- (num / denom) * 100
  return(prop)
}

prop_se <- function(num, denom, se_num, se_denom) {
  se_prop <- ((1 / denom) * sqrt(se_num^2 - (num^2/denom^2)*se_denom^2)) * 100
  return(se_prop)
}

#' Summarize the data using the 95% CI
#' Note: this function sets the lower limit to 0 if the value is negative
acs_tab <- function(var, var_se, crit = 1.96, rnd = 1) {
  est <- format(round(var, rnd), nsmall = rnd, trim = T)
  ll_1 <- ifelse(var - (var_se * crit) < 0, 0, var - (var_se * crit))
  
  ll <- format(round(ll_1, rnd), nsmall = rnd, trim = T)
  ul <- format(round(var + (var_se * crit), rnd), nsmall = rnd, trim = T)
  out <- paste0(est, " (", ll, " - ", ul, ")")
  return(out)
}

test <- prop(41142530, 88337406)
test_se <- prop_se(41142530, 88337406, 51284, 74563)

acs_tab(test, test_se)
acs_tab(test, test_se, crit = 1.645)
acs_tab(test, test_se, crit = 1.645, rnd = 0)

#' Function without the 95%CI so that we can map these variables
map_tab <- function(var, var_se, crit = 1.96, rnd = 1) {
  est <- format(round(var, rnd), nsmall = rnd, trim = T)
  # ll_1 <- ifelse(var - (var_se * crit) < 0, 0, var - (var_se * crit))
  # 
  # ll <- format(round(ll_1, rnd), nsmall = rnd, trim = T)
  # ul <- format(round(var + (var_se * crit), rnd), nsmall = rnd, trim = T)
  # out <- paste0(est, " (", ll, " - ", ul, ")")
  return(as.numeric(est))
}

#'==============================================================================
#'sf objects to join with ACS data
#'==============================================================================

load(here::here("data", "shapefiles.rdata"))

#'==============================================================================
#'Summarizing demographic variables for each study area using ACS data
#'==============================================================================

tract_vars_21 <- load_variables(dataset = "acs5", year = "2021")
write_csv(tract_vars_21, here::here("documentation", 
                                    "acs5_2021_variable_names.csv"))

#' -----------------------------------------------------------------------------
#' Assign ACS variables for each census tract, for Denver county, and for Colorado
#' Using the 2017-2021 ACS 5-Year Estimates 
#' 
#' For the ACS, the MOE value is provided at the 90% confidence interval
#' To calculate the 95%CI, we need to first divide by 1.645 to get the SD and then
#' multiply by 1.96
#' Info about dealing with margins of error can be found here:
#' https://www2.census.gov/programs-surveys/acs/tech_docs/accuracy/2018_ACS_Accuracy_Document_Worked_Examples.pdf?
#' 
#' ACS Data Dictionary
#' ------------------------------ 
#' Demographics:
#' ------------------------------
#' 
#' Age and Sex: % under 5, % under18, % 65plus
#' B01001_001    Total population
#' B01001_002    All males
#' B01001_003    Males under age 5
#' B01001_004    Males 5-9
#' B01001_005    Males 10-14
#' B01001_006    Males 15-17
#' B01001_020    Males 65-66
#' B01001_021    Males 67-69
#' B01001_022    Males 70-74
#' B01001_023    Males 75-79
#' B01001_024    Males 80-85
#' B01001_025    Males 85+
#' B01001_026    All females
#' B01001_027    Females under age 5
#' B01001_028    Females 5-9
#' B01001_029    Females 10-14
#' B01001_030    Females 15-17
#' B01001_044    Females 65-66
#' B01001_045    Females 67-69
#' B01001_046    Females 70-74
#' B01001_047    Females 75-79
#' B01001_048    Females 80-85
#' B01001_049    Females 85+
#' 
#' Race and ethnicity: % non-Hispanic White, % non-Hispanic Black, % Hispanic or Latino (any race)
#' B03002_001   Total population
#' B03002_003   White alone, not Hispanic or Latino
#' B03002_004   Black or African American alone, not Hispanic or Latino
#' B03002_005   American Indian or Alaska Native alone, not Hispanic or Latino
#' B03002_006   Asian alone, not Hispanic or Latino
#' B03002_007   Native Hawaiian or Other Pacific Islander alone, not Hispanic or Latino
#' B03002_008   Some other race alone, not Hispanic or Latino
#' B03002_009   Two or more races, not Hispanic or Latino 
#' B03002_012   Hispanic or Latino (any race)
#' 
#' Disability
#' B18101_003   Male under 5
#' B18101_004   Male under 5 with any disability
#' B18101_006   Male 5-17
#' B18101_007   Male 5-17 with any disability
#' B18101_009   Male 18-34
#' B18101_010   Male 18-34 with any disability
#' B18101_012   Male 35-64
#' B18101_013   Male 35-64 with any disability
#' B18101_015   Male 65-74
#' B18101_016   Male 65-74 with any disability
#' B18101_018   Male 75+
#' B18101_019   Male 75+ with any disability
#' B18101_022   Female under 5
#' B18101_023   Female under 5 with any disability
#' B18101_025   Female 5-17
#' B18101_026   Female 5-17 with any disability
#' B18101_028   Female 18-34
#' B18101_029   Female 18-34 with any disability
#' B18101_031   Female 35-64
#' B18101_032   Female 35-64 with any disability
#' B18101_034   Female 65-74
#' B18101_035   Female 65-74 with any disability
#' B18101_037   Female 75+
#' B18101_038   Female 75+ with any disability
#' 
#' Disability by difficulty
#' B18102_003   Male under 5
#' B18102_004   Male under 5 with hearing difficulty
#' B18102_006   Male 5-17
#' B18102_007   Male 5-17 with hearing difficulty
#' B18102_009   Male 18-34
#' B18102_010   Male 18-34 with hearing difficulty
#' B18102_012   Male 35-64
#' B18102_013   Male 35-64 with hearing difficulty
#' B18102_015   Male 65-74
#' B18102_016   Male 65-74 with hearing difficulty
#' B18102_018   Male 75+
#' B18102_019   Male 75+ with hearing difficulty
#' B18102_022   Female under 5
#' B18102_023   Female under 5 with hearing difficulty
#' B18102_025   Female 5-17
#' B18102_026   Female 5-17 with hearing difficulty
#' B18102_028   Female 18-34
#' B18102_029   Female 18-34 with hearing difficulty
#' B18102_031   Female 35-64
#' B18102_032   Female 35-64 with hearing difficulty
#' B18102_034   Female 65-74
#' B18102_035   Female 65-74 with hearing difficulty
#' B18102_037   Female 75+
#' B18102_038   Female 75+ with hearing difficulty
#' 
#' B18103_003   Male under 5
#' B18103_004   Male under 5 with vision difficulty
#' B18103_006   Male 5-17
#' B18103_007   Male 5-17 with vision difficulty
#' B18103_009   Male 18-34
#' B18103_010   Male 18-34 with vision difficulty
#' B18103_012   Male 35-64
#' B18103_013   Male 35-64 with vision difficulty
#' B18103_015   Male 65-74
#' B18103_016   Male 65-74 with vision difficulty
#' B18103_018   Male 75+
#' B18103_019   Male 75+ with vision difficulty
#' B18103_022   Female under 5
#' B18103_023   Female under 5 with vision difficulty
#' B18103_025   Female 5-17
#' B18103_026   Female 5-17 with vision difficulty
#' B18103_028   Female 18-34
#' B18103_029   Female 18-34 with vision difficulty
#' B18103_031   Female 35-64
#' B18103_032   Female 35-64 with vision difficulty
#' B18103_034   Female 65-74
#' B18103_035   Female 65-74 with vision difficulty
#' B18103_037   Female 75+
#' B18103_038   Female 75+ with vision difficulty
#' 
#' B18104_003   Male 5-17
#' B18104_004   Male 5-17 with cognitive difficulty
#' B18104_006   Male 18-34
#' B18104_007   Male 18-34 with cognitive difficulty
#' B18104_009   Male 35-64
#' B18104_010   Male 35-64 with cognitive difficulty
#' B18104_012   Male 65-74
#' B18104_013   Male 65-74 with cognitive difficulty
#' B18104_015   Male 75+
#' B18104_016   Male 75+ with cognitive difficulty
#' B18104_019   Female 5-17
#' B18104_020   Female 5-17 with cognitive difficulty
#' B18104_022   Female 18-34
#' B18104_023   Female 18-34 with cognitive difficulty
#' B18104_025   Female 35-64
#' B18104_026   Female 35-64 with cognitive difficulty
#' B18104_028   Female 65-74
#' B18104_029   Female 65-74 with cognitive difficulty
#' B18104_031   Female 75+
#' B18104_032   Female 75+ with cognitive difficulty

#' B18105_003   Male 5-17
#' B18105_004   Male 5-17 with ambulatory difficulty
#' B18105_006   Male 18-34
#' B18105_007   Male 18-34 with ambulatory difficulty
#' B18105_009   Male 35-64
#' B18105_010   Male 35-64 with ambulatory difficulty
#' B18105_012   Male 65-74
#' B18105_013   Male 65-74 with ambulatory difficulty
#' B18105_015   Male 75+
#' B18105_016   Male 75+ with ambulatory difficulty
#' B18105_019   Female 5-17
#' B18105_020   Female 5-17 with ambulatory difficulty
#' B18105_022   Female 18-34
#' B18105_023   Female 18-34 with ambulatory difficulty
#' B18105_025   Female 35-64
#' B18105_026   Female 35-64 with ambulatory difficulty
#' B18105_028   Female 65-74
#' B18105_029   Female 65-74 with ambulatory difficulty
#' B18105_031   Female 75+
#' B18105_032   Female 75+ with ambulatory difficulty

#' B18106_003   Male 5-17
#' B18106_004   Male 5-17 with self-care difficulty
#' B18106_006   Male 18-34
#' B18106_007   Male 18-34 with self-care difficulty
#' B18106_009   Male 35-64
#' B18106_010   Male 35-64 with self-care difficulty
#' B18106_012   Male 65-74
#' B18106_013   Male 65-74 with self-care difficulty
#' B18106_015   Male 75+
#' B18106_016   Male 75+ with self-care difficulty
#' B18106_019   Female 5-17
#' B18106_020   Female 5-17 with self-care difficulty
#' B18106_022   Female 18-34
#' B18106_023   Female 18-34 with self-care difficulty
#' B18106_025   Female 35-64
#' B18106_026   Female 35-64 with self-care difficulty
#' B18106_028   Female 65-74
#' B18106_029   Female 65-74 with self-care difficulty
#' B18106_031   Female 75+
#' B18106_032   Female 75+ with self-care difficulty

#' B18107_003   Male 18-34
#' B18107_004   Male 18-34 with independent living difficulty
#' B18107_006   Male 35-64
#' B18107_007   Male 35-64 with independent living difficulty
#' B18107_009   Male 65-74
#' B18107_010   Male 65-74 with independent living difficulty
#' B18107_012   Male 75+
#' B18107_013   Male 75+ with independent living difficulty
#' B18107_016   Female 18-34
#' B18107_017   Female 18-34 with independent living difficulty
#' B18107_019   Female 35-64
#' B18107_020   Female 35-64 with independent living difficulty
#' B18107_022   Female 65-74
#' B18107_023   Female 65-74 with independent living difficulty
#' B18107_025   Female 75+
#' B18107_026   Female 75+ with independent living difficulty

#' Ability to speak English (pop 5 years of age +)
#' B16004_001   Total population 5 years of age and older
#' B16004_007   Spanish speakers 5-17 years, speak English not well
#' B16004_008   Spanish speakers 5-17, speak English not at all
#' B16004_012   Other Indo-European speakers 5-17 years, speak English not well
#' B16004_013   Other Indo-European speakers 5-17, speak English not at all
#' B16004_017   API speakers 5-17 years, speak English not well
#' B16004_018   API speakers 5-17, speak English not at all
#' B16004_022   Other language speakers 5-17 years, speak English not well
#' B16004_023   Other language speakers 5-17, speak English not at all
#' B16004_029   Spanish speakers 18-64 years, speak English not well
#' B16004_030   Spanish speakers 18-64, speak English not at all
#' B16004_034   Other Indo-European speakers 18-64 years, speak English not well
#' B16004_035   Other Indo-European speakers 18-64, speak English not at all
#' B16004_039   API speakers 18-64 years, speak English not well
#' B16004_040   API speakers 18-64, speak English not at all
#' B16004_044   Other language speakers 18-64 years, speak English not well
#' B16004_045   Other language speakers 18-64, speak English not at all 
#' B16004_051   Spanish speakers 65+ years, speak English not well
#' B16004_052   Spanish speakers 65+, speak English not at all
#' B16004_056   Other Indo-European speakers 65+ years, speak English not well
#' B16004_057   Other Indo-European speakers 65+, speak English not at all
#' B16004_061   API speakers 65+ years, speak English not well
#' B16004_062   API speakers 65+, speak English not at all
#' B16004_066   Other language speakers 65+ years, speak English not well
#' B16004_067   Other language speakers 65+, speak English not at all 
#'
#' Household type for children under 18: % in single-head households
#' B09005_001   Children under 18
#' B09005_004   children under 18, male householder no partner present
#' B09005_005   children under 18, female householder no partner present
#' 
#' Children <18 years of age living with grandparent head of household
#' B09018_001   Children under 18
#' B09018_006   Children under 18 living with grandparent head of household
#'
#' ------------------------------ 
#' Employment, Income, Insurance Status
#' ------------------------------
#' 
#' Per capita income 
#' B19301_001   Per capita income in the past 12 months (in 2021 inflation-adjusted dollars)
#' 
#' Household income
#' B19013_001    Median household income in the past 12 months (in 2021 inflation-adjusted dollars)
#' 
#' Household poverty
#' B17017_001   Total households
#' B17017_002   Households with income in the past 12 months below poverty level 
#' 
#' Employment status
#' B23025_001   Population 16 years of age or older
#' B23025_003   Population 16 years of age or older in the civilian labor force
#' B23025_005   Population 16 years of age or older in the civilian labor force and unemployed
#' 
#' SNAP Benefits
#' B22002_001   Total households
#' B22002_002   Households receiving SNAP benefits in the past 12 months
#' 
#' Health insurance coverage
#' B27010_001   Total population
#' B27010_017   Population under 19 years with no health insurance coverage
#' B27010_033   Population 19-34 years with no health insurance coverage
#' B27010_050   Population 35-64 years with no health insurance coverage
#' B27010_066   Population 65+ years with no health insurance coverage
#' 
#' ------------------------------ 
#' Education
#' ------------------------------
#' 
#' Educational attainment: % ages 25+ without HS diploma
#' Educational attainment: % ages 25 + with a bachelor's degree or higher
#' B15002_001    Population 25 years and older
#' B15002_003    No schooling males
#' B15002_004    Nursery school - 4th grade males
#' B15002_005    5th grade - 6th grade males
#' B15002_006    7th grade - 8th grade males
#' B15002_007    9th grade males
#' B15002_008    10th grade males
#' B15002_009    11th grade males
#' B15002_010    12th grade, no diploma males
#' B15002_015    Bachelor's degree males
#' B15002_016    Master's degree males
#' B15002_017    Professional degree males
#' B15002_018    Doctorate males
#' B15002_020    No schooling females
#' B15002_021    Nursery school - 4th grade females
#' B15002_022    5th grade - 6th grade females
#' B15002_023    7th grade - 8th grade females
#' B15002_024    9th grade females
#' B15002_025    10th grade females
#' B15002_026    11th grade females
#' B15002_027    12th grade, no diploma females
#' B15002_032    Bachelor's degree females
#' B15002_033    Master's degree females
#' B15002_034    Professional degree females
#' B15002_035    Doctorate females
#' 
#' ------------------------------ 
#' Housing
#' ------------------------------
#' 
#' Owner- and Renter-occupied housing units
#' B25003_001   Total occupied housing units
#' B25003_002   Owner occupied
#' B25003_003   Renter occupied
#' 
#' Population in owner- and renter-occupied housing
#' B25008_001   Total population 
#' B25008_002   Population in owner-occupied housing
#' B25008_003   Population in renter-occupied housing
#' 
#' Pre-1980 Housing
#' B25034_001   Total housing structures (occupied and vacant)
#' B25034_007   Housing structures built 1970 - 1979
#' B25034_008   Housing structures built 1960 - 1969
#' B25034_009   Housing structures built 1950 - 1959
#' B25034_010   Housing structures built 1940 - 1949
#' B25034_011   Housing structures built 1939 or earlier
#' 
#' B25077_001   Median housing value (dollars)
#' 
#' Internet access
#' B28002_001   Total households
#' B28002_013   Households without internet access
#' 
#' Housing Facilities (lacks plumbing, kitchen, overcrowded, cost burden)
#' B25048_003   Occupied housing lacking complete plumbing facilities
#' B25052_003   Occupied housing lacking complete kitchen facilities
#' 
#' Overcrowding (more than one occupant per room)
#' B25014_005   Owner occupied units, 1.01-1.5 occupants per room
#' B25014_006   Owner occupied units, 1.51-2.0 occupants per room
#' B25014_007   Owner occupied units, >2 occupants per room
#' B25014_011   Renter occupied units, 1.01-1.5 occupants per room
#' B25014_012   Renter occupied units, 1.51-2.0 occupants per room
#' B25014_013   Renter occupied units, >2 occupants per room
#' 
#' Cost burden (housing costs >30% of annual income)
#' B25106_006   Owner occupied units, costs <20k, >30% of income
#' B25106_010   Owner occupied units, costs 20k-35k, >30% of income
#' B25106_014   Owner occupied units, costs 35k-50k, >30% of income
#' B25106_018   Owner occupied units, costs 50k-57k, >30% of income
#' B25106_022   Owner occupied units, costs 75k+, >30% of income
#' B25106_028   Renter occupied units, costs <20k, >30% of income
#' B25106_032   Renter occupied units, costs 20k-35k, >30% of income
#' B25106_036   Renter occupied units, costs 35k-50k, >30% of income
#' B25106_040   Renter occupied units, costs 50k-57k, >30% of income
#' B25106_044   Renter occupied units, costs 75k+, >30% of income
#' 
#' Housing burdened (CO Enviroscreen calculation)
#' B25070_001   All rented properties
#' B25070_007   Rent 30-34.9% of income in last 12 months
#' B25070_008   Rent 35-39.9% of income in last 12 months
#' B25070_009   Rent 40-49.9% of income in last 12 months
#' B25070_010   Rent 50%+ of income in last 12 months
#' B25091_001   Owner-occupied properties
#' B25091_008   Owner-occupied with a mortgage, 30-34.9% of income in last 12 months
#' B25091_009   Owner-occupied with a mortgage, 30-39.9% of income in last 12 months
#' B25091_010   Owner-occupied with a mortgage, 40-49.9% of income in last 12 months
#' B25091_011   Owner-occupied with a mortgage, 50%+ of income in last 12 months
#' B25091_019   Owner-occupied with a mortgage, 30-34.9% of income in last 12 months
#' B25091_020   Owner-occupied with a mortgage, 30-39.9% of income in last 12 months
#' B25091_021   Owner-occupied with a mortgage, 40-49.9% of income in last 12 months
#' B25091_022   Owner-occupied with a mortgage, 50%+ of income in last 12 months
#' 
#' Foreign-born population
#' B05002_001   Total pop
#' B05002_002   Native-born
#' B05002_013   Foreign-born
#' -----------------------------------------------------------------------------

acs_vars_21 <- c("B01001_001",
                 paste0("B01001_", str_pad(c(3:6, 20:25, 27:30, 44:49), pad = "0", width = 3, side = "left")),
                 paste0("B03002_", str_pad(c(1, 3:9, 12), pad = "0", width = 3, side = "left")),
                 paste0("B18101_", str_pad(c(1:39), pad = "0", width = 3, side = "left")),
                 paste0("B18102_", str_pad(c(1:39), pad = "0", width = 3, side = "left")),
                 paste0("B18103_", str_pad(c(1:39), pad = "0", width = 3, side = "left")),
                 paste0("B18104_", str_pad(c(1:33), pad = "0", width = 3, side = "left")),
                 paste0("B18105_", str_pad(c(1:33), pad = "0", width = 3, side = "left")),
                 paste0("B18106_", str_pad(c(1:33), pad = "0", width = 3, side = "left")),
                 paste0("B18107_", str_pad(c(1:27), pad = "0", width = 3, side = "left")),
                 paste0("B16004_", str_pad(c(1, 7, 8, 12, 13, 17, 18, 22, 23, 
                                             29, 30, 34, 35, 39, 40, 44, 45, 
                                             51, 52, 56, 57, 61, 62, 66, 67), 
                                           pad = "0", width = 3, side = "left")),
                 "B09005_001", "B09005_004", "B09005_005", 
                 "B09018_001", "B09018_006",
                 "B19301_001",
                 "B19013_001",
                 "B17017_001", "B17017_002",
                 "B23025_001", "B23025_003", "B23025_005",
                 "B22002_001", "B22002_002",
                 paste0("B27010_", str_pad(c(1, 17, 33, 50, 66), pad = "0", width = 3, side = "left")),
                 paste0("B15002_", str_pad(c(1, 3:10, 15:18, 20:27, 32:35), pad = "0", width = 3, side = "left")),
                 "B25003_001", "B25003_002", "B25003_003",
                 "B25008_001", "B25008_002", "B25008_003",
                 paste0("B25034_", str_pad(c(1, 7:11), pad = "0", width = 3, side = "left")),
                 "B25077_001",
                 "B28002_001", "B28002_013",
                 "B25048_003", "B25052_003",
                 paste0("B25014_", str_pad(c(5:7, 11:13), pad = "0", width = 3, side = "left")),
                 paste0("B25106_", str_pad(c(6, 10, 14, 18, 22, 28, 32, 36, 40, 44),
                                           pad = "0", width = 3, side = "left")),
                 paste0("B25070_", str_pad(c(1, 7:10), pad = "0", width = 3, side = "left")),
                 paste0("B25091_", str_pad(c(1, 8:11, 19:22), pad = "0", width = 3, side = "left")),
                 paste0("B05002_", str_pad(c(1, 2, 13), pad = "0", width = 3, side = "left")))

acs_test <- get_acs(geography = "tract", year = 2021,
                    variables = "B01001_001",
                    state = 08, county = 31)

acs_tracts_df <- get_acs(geography = "tract", year = 2021,
                      variables = acs_vars_21,
                      state = 08, county = 31) %>%
  mutate(se = ifelse(!is.na(moe), moe / 1.645, 0)) %>%
  rename("e" = "estimate") %>%
  pivot_wider(id_cols = GEOID, names_from = "variable",
              values_from = c("e", "se")) %>%
  mutate(e_total_pop = e_B01001_001,
         se_total_pop = se_B01001_001,
         e_nhw = e_B03002_003,
         se_nhw = se_B03002_003,
         e_nhb = e_B03002_004,
         se_nhb = se_B03002_004,
         e_hl = e_B03002_012,
         se_hl = se_B03002_012,
         e_under5 = e_B01001_003 + e_B01001_027,
         se_under5 = sqrt(se_B01001_003^2 + se_B01001_027^2),
         e_under18 = e_B01001_003 + e_B01001_004 + e_B01001_005 + e_B01001_006 +
           e_B01001_027 + e_B01001_028 + e_B01001_029 +e_B01001_030,
         se_under18 = sqrt(se_B01001_003^2 + se_B01001_004^2 + se_B01001_005^2 + se_B01001_006^2 +
                             se_B01001_027^2 + se_B01001_028^2 + se_B01001_029^2 + se_B01001_030^2),
         e_65plus = e_B01001_020 + e_B01001_021 + e_B01001_022 + e_B01001_023 + e_B01001_024 +
           e_B01001_025 + e_B01001_044 + e_B01001_045 + e_B01001_046 + e_B01001_047 +
           e_B01001_048 + e_B01001_049,
         se_65plus = sqrt(se_B01001_020^2 + se_B01001_021^2 + se_B01001_022^2 + se_B01001_023^2 +
                            se_B01001_024^2 + se_B01001_025^2 + se_B01001_044^2 + se_B01001_045^2 +
                            se_B01001_046^2 + se_B01001_047^2 + se_B01001_048^2 + se_B01001_049^2),
         e_anydis_chtot = e_B18101_003 + e_B18101_006 + e_B18101_022 + e_B18101_025,
         se_anydis_chtot = sqrt(se_B18101_003^2 + se_B18101_006^2 +
                                  se_B18101_022^2 + se_B18101_025^2),
         e_anydis_ch = e_B18101_004 + e_B18101_007 + e_B18101_023 + e_B18101_026,
         se_anydis_ch = sqrt(se_B18101_004^2 + se_B18101_007^2 +
                               se_B18101_023^2 + se_B18101_026^2),
         e_anydis_adtot = e_B18101_009 + e_B18101_012 + e_B18101_015 + e_B18101_018 +
           e_B18101_028 + e_B18101_031 + e_B18101_034 + e_B18101_037,
         se_anydis_adtot = sqrt(se_B18101_009^2 + se_B18101_012^2 + se_B18101_015^2 + se_B18101_018^2 +
                                  se_B18101_028^2 + se_B18101_031^2 + se_B18101_034^2 + se_B18101_037^2),
         e_anydis_ad = e_B18101_010 + e_B18101_013 + e_B18101_016 + e_B18101_019 +
           e_B18101_029 + e_B18101_032 + e_B18101_035 + e_B18101_038,
         se_anydis_ad = sqrt(se_B18101_010^2 + se_B18101_013^2 + se_B18101_016^2 + se_B18101_019^2 +
                               se_B18101_029^2 + se_B18101_032^2 + se_B18101_035^2 + se_B18101_038^2),
         e_hearing_chtot = e_B18102_003 + e_B18102_006 + e_B18102_022 + e_B18102_025,
         se_hearing_chtot = sqrt(se_B18102_003^2 + se_B18102_006^2 +
                                   se_B18102_022^2 + se_B18102_025^2),
         e_hearing_ch = e_B18102_004 + e_B18102_007 + e_B18102_023 + e_B18102_026,
         se_hearing_ch = sqrt(se_B18102_004^2 + se_B18102_007^2 +
                                se_B18102_023^2 + se_B18102_026^2),
         e_hearing_adtot = e_B18102_009 + e_B18102_012 + e_B18102_015 + e_B18102_018 +
           e_B18102_028 + e_B18102_031 + e_B18102_034 + e_B18102_037,
         se_hearing_adtot = sqrt(se_B18102_009^2 + se_B18102_012^2 + se_B18102_015^2 + se_B18102_018^2 +
                                   se_B18102_028^2 + se_B18102_031^2 + se_B18102_034^2 + se_B18102_037^2),
         e_hearing_ad = e_B18102_010 + e_B18102_013 + e_B18102_016 + e_B18102_019 +
           e_B18102_029 + e_B18102_032 + e_B18102_035 + e_B18102_038,
         se_hearing_ad = sqrt(se_B18102_010^2 + se_B18102_013^2 + se_B18102_016^2 + se_B18102_019^2 +
                                se_B18102_029^2 + se_B18102_032^2 + se_B18102_035^2 + se_B18102_038^2),
         e_vision_chtot = e_B18103_003 + e_B18103_006 + e_B18103_022 + e_B18103_025,
         se_vision_chtot = sqrt(se_B18103_003^2 + se_B18103_006^2 +
                                  se_B18103_022^2 + se_B18103_025^2),
         e_vision_ch = e_B18103_004 + e_B18103_007 + e_B18103_023 + e_B18103_026,
         se_vision_ch = sqrt(se_B18103_004^2 + se_B18103_007^2 +
                               se_B18103_023^2 + se_B18103_026^2),
         e_vision_adtot = e_B18103_009 + e_B18103_012 + e_B18103_015 + e_B18103_018 +
           e_B18103_028 + e_B18103_031 + e_B18103_034 + e_B18103_037,
         se_vision_adtot = sqrt(se_B18103_009^2 + se_B18103_012^2 + se_B18103_015^2 + se_B18103_018^2 +
                                  se_B18103_028^2 + se_B18103_031^2 + se_B18103_034^2 + se_B18103_037^2),
         e_vision_ad = e_B18103_010 + e_B18103_013 + e_B18103_016 + e_B18103_019 +
           e_B18103_029 + e_B18103_032 + e_B18103_035 + e_B18103_038,
         se_vision_ad = sqrt(se_B18103_010^2 + se_B18103_013^2 + se_B18103_016^2 + se_B18103_019^2 +
                               se_B18103_029^2 + se_B18103_032^2 + se_B18103_035^2 + se_B18103_038^2),
         e_cognitive_chtot = e_B18104_003 + e_B18104_019,
         se_cognitive_chtot = sqrt(se_B18104_003^2 + se_B18104_019^2),

         e_cognitive_ch = e_B18104_004 + e_B18104_020,
         se_cognitive_ch = sqrt(se_B18104_004^2 + se_B18104_020^2),
         e_cognitive_adtot = e_B18104_006 + e_B18104_009 + e_B18104_012 + e_B18104_015 +
           e_B18104_022 + e_B18104_025 + e_B18104_028 + e_B18104_031,
         se_cognitive_adtot = sqrt(se_B18104_006^2 + se_B18104_009^2 + se_B18104_012^2 + se_B18104_015^2 +
                                     se_B18104_022^2 + se_B18104_025^2 + se_B18104_028^2 + se_B18104_031^2),
         e_cognitive_ad = e_B18104_007 + e_B18104_010 + e_B18104_013 + e_B18104_016 +
           e_B18104_023 + e_B18104_026 + e_B18104_029 + e_B18104_032,
         se_cognitive_ad = sqrt(se_B18104_007^2 + se_B18104_010^2 + se_B18104_013^2 + se_B18104_016^2 +
                                  se_B18104_023^2 + se_B18104_026^2 + se_B18104_029^2 + se_B18104_032^2),
         e_ambulatory_chtot = e_B18105_003 + e_B18105_019,
         se_ambulatory_chtot = sqrt(se_B18105_003^2 + se_B18105_019^2),
         e_ambulatory_ch = e_B18105_004 + e_B18105_020,
         se_ambulatory_ch = sqrt(se_B18105_004^2 + se_B18105_020^2),
         e_ambulatory_adtot = e_B18105_006 + e_B18105_009 + e_B18105_012 + e_B18105_015 +
           e_B18105_022 + e_B18105_025 + e_B18105_028 + e_B18105_031,
         se_ambulatory_adtot = sqrt(se_B18105_006^2 + se_B18105_009^2 + se_B18105_012^2 + se_B18105_015^2 +
                                      se_B18105_022^2 + se_B18105_025^2 + se_B18105_028^2 + se_B18105_031^2),
         e_ambulatory_ad = e_B18105_007 + e_B18105_010 + e_B18105_013 + e_B18105_016 +
           e_B18105_023 + e_B18105_026 + e_B18105_029 + e_B18105_032,
         se_ambulatory_ad = sqrt(se_B18105_007^2 + se_B18105_010^2 + se_B18105_013^2 + se_B18105_016^2 +
                                   se_B18105_023^2 + se_B18105_026^2 + se_B18105_029^2 + se_B18105_032^2),
         e_selfcare_chtot = e_B18106_003 + e_B18106_019,
         se_selfcare_chtot = sqrt(se_B18106_003^2 + se_B18106_019^2),
         e_selfcare_ch = e_B18106_004 + e_B18106_020,
         se_selfcare_ch = sqrt(se_B18106_004^2 + se_B18106_020^2),
         e_selfcare_adtot = e_B18106_006 + e_B18106_009 + e_B18106_012 + e_B18106_015 +
           e_B18106_022 + e_B18106_025 + e_B18106_028 + e_B18106_031,
         se_selfcare_adtot = sqrt(se_B18106_006^2 + se_B18106_009^2 + se_B18106_012^2 + se_B18106_015^2 +
                                    se_B18106_022^2 + se_B18106_025^2 + se_B18106_028^2 + se_B18106_031^2),
         e_selfcare_ad = e_B18106_007 + e_B18106_010 + e_B18106_013 + e_B18106_016 +
           e_B18106_023 + e_B18106_026 + e_B18106_029 + e_B18106_032,
         se_selfcare_ad = sqrt(se_B18106_007^2 + se_B18106_010^2 + se_B18106_013^2 + se_B18106_016^2 +
                                 se_B18106_023^2 + se_B18106_026^2 + se_B18106_029^2 + se_B18106_032^2),
         e_indep_adtot = e_B18107_003 + e_B18107_006 + e_B18107_009 + e_B18107_012 +
           e_B18107_016 + e_B18107_019 + e_B18107_022 + e_B18107_025,
         se_indep_adtot = sqrt(se_B18107_003^2 + se_B18107_006^2 + se_B18107_009^2 + se_B18107_012^2 +
                                 se_B18107_016^2 + se_B18107_019^2 + se_B18107_022^2 + se_B18107_025^2),
         e_indep_ad = e_B18107_004 + e_B18107_007 + e_B18107_010 + e_B18107_013 +
           e_B18107_017 + e_B18107_020 + e_B18107_023 + e_B18107_026,
         se_indep_ad = sqrt(se_B18107_004^2 + se_B18107_007^2 + se_B18107_010^2 + se_B18107_013^2 +
                              se_B18107_017^2 + se_B18107_020^2 + se_B18107_023^2 + se_B18107_026^2),
         e_5plus = e_B16004_001,
         se_5plus = se_B16004_001,
         e_eng_ltw = e_B16004_007 + e_B16004_008 + e_B16004_012 + e_B16004_013 +
           e_B16004_017 + e_B16004_018 + e_B16004_022 + e_B16004_023 +
           e_B16004_029 + e_B16004_030 + e_B16004_034 + e_B16004_035 +
           e_B16004_039 + e_B16004_040 + e_B16004_044 + e_B16004_045 +
           e_B16004_051 + e_B16004_052 + e_B16004_056 + e_B16004_057 +
           e_B16004_061 + e_B16004_062 + e_B16004_066 + e_B16004_067,
         se_eng_ltw = sqrt(se_B16004_007^2 + se_B16004_008^2 + se_B16004_012^2 + se_B16004_013^2 +
                             se_B16004_017^2 + se_B16004_018^2 + se_B16004_022^2 + se_B16004_023^2 +
                             se_B16004_029^2 + se_B16004_030^2 + se_B16004_034^2 + se_B16004_035^2 +
                             se_B16004_039^2 + se_B16004_040^2 + se_B16004_044^2 + se_B16004_045^2 +
                             se_B16004_051^2 + se_B16004_052^2 + se_B16004_056^2 + se_B16004_057^2 +
                             se_B16004_061^2 + se_B16004_062^2 + se_B16004_066^2 + se_B16004_067^2),
         e_children = e_B09005_001,
         se_children = se_B09005_001,
         e_children_shh = e_B09005_004 + e_B09005_005,
         se_children_shh = sqrt(se_B09005_004^2 + se_B09005_005^2),
         e_children_ghh = e_B09018_006,
         se_children_ghh = se_B09018_006,
         e_pc_income = e_B19301_001,
         se_pc_income = se_B19301_001,
         e_hh_income = e_B19013_001,
         se_hh_income = se_B19013_001,
         e_total_hh = e_B17017_001,
         se_total_hh = se_B17017_001,
         e_hh_poverty = e_B17017_002,
         se_hh_poverty = se_B17017_002,
         e_hh_snap = e_B22002_002,
         se_hh_snap = se_B22002_002,
         e_civ_pop_lf = e_B23025_003,
         se_civ_pop_lf = se_B23025_003,
         e_civ_pop_unemp = e_B23025_005,
         se_civ_pop_unemp= se_B23025_005,
         e_ins_pop = e_B27010_001,
         se_ins_pop = se_B27010_001,
         e_no_ins = e_B27010_017 + e_B27010_033 + e_B27010_050 + e_B27010_066,
         se_no_ins = sqrt(se_B27010_017^2 + se_B27010_033^2 + se_B27010_050^2 + se_B27010_066^2),
         e_25plus_pop = e_B15002_001,
         se_25plus_pop = se_B15002_001,
         e_less_hs = e_B15002_003 + e_B15002_004 + e_B15002_005 + e_B15002_006 + e_B15002_007 +
           e_B15002_008 + e_B15002_009 + e_B15002_010 + e_B15002_020 + e_B15002_021 +
           e_B15002_022 + e_B15002_023 + e_B15002_024 + e_B15002_025 + e_B15002_026 +
           e_B15002_027,
         se_less_hs = sqrt(se_B15002_003^2 + se_B15002_004^2 + se_B15002_005^2 + se_B15002_006^2 +
                             se_B15002_007^2 + se_B15002_008^2 + se_B15002_009^2 + se_B15002_010^2 +
                             se_B15002_020^2 + se_B15002_021^2 + se_B15002_022^2 + se_B15002_023^2 +
                             se_B15002_024^2 + se_B15002_025^2 + se_B15002_026^2 + se_B15002_027^2),
         e_bs_plus = e_B15002_015 + e_B15002_016 + e_B15002_017 + e_B15002_018 +
           e_B15002_032 + e_B15002_033 + e_B15002_034 + e_B15002_035,
         se_bs_plus = sqrt(se_B15002_015^2 + se_B15002_016^2 + se_B15002_017^2 + se_B15002_018^2 +
                             se_B15002_032^2 + se_B15002_033^2 + se_B15002_034^2 + se_B15002_035^2),
         e_occ_units = e_B25003_001,
         se_occ_units = se_B25003_001,
         e_owner_units = e_B25003_002,
         se_owner_units = se_B25003_002,
         e_renter_units = e_B25003_003,
         se_renter_units = se_B25003_003,
         e_housing_pop = e_B25008_001,
         se_housing_pop = se_B25008_001,
         e_housing_pop_owner = e_B25008_002,
         se_housing_pop_owner = se_B25008_002,
         e_housing_pop_renter = e_B25008_003,
         se_housing_pop_renter = se_B25008_003,
         e_all_units = e_B25034_001,
         se_all_units = se_B25034_001,
         e_housing_pre1980 = e_B25034_007 + e_B25034_008 + e_B25034_009 + e_B25034_010 + e_B25034_011,
         se_housing_pre1980 = sqrt(se_B25034_007^2 + se_B25034_008^2 + se_B25034_009^2 +
                                     se_B25034_010^2 + se_B25034_011^2),
         e_med_housing_value = e_B25077_001,
         se_med_housing_value = se_B25077_001,
         e_hh_no_internet = e_B28002_013,
         se_hh_no_internet = se_B28002_013,
         e_occ_lack_plumbing = e_B25048_003,
         se_occ_lack_plumbing = se_B25048_003,
         e_occ_lack_kitchen = e_B25052_003,
         se_occ_lack_kitchen = se_B25052_003,
         e_occ_crowded = e_B25014_005 + e_B25014_006 + e_B25014_007 +
           e_B25014_011 + e_B25014_012 + e_B25014_013,
         se_occ_crowded = sqrt(se_B25014_005^2 + se_B25014_006^2 + se_B25014_007^2 +
                                 se_B25014_011^2 + se_B25014_012^2 + se_B25014_013^2),
         e_occ_cost_burdened = e_B25106_006 + e_B25106_010 + e_B25106_014 + e_B25106_018 +
           e_B25106_022 + e_B25106_028 + e_B25106_032 + e_B25106_036 +
           e_B25106_040 + e_B25106_044,
         se_occ_cost_burdened = sqrt(se_B25106_006^2 + se_B25106_010^2 + se_B25106_014^2 +
                                       se_B25106_018^2 + se_B25106_022^2 + se_B25106_028^2 +
                                       se_B25106_032^2 + se_B25106_036^2 + se_B25106_040^2 +
                                       se_B25106_044^2),
         e_all_housing = e_B25070_001 + e_B25091_001,
         se_all_housing = sqrt(se_B25070_001^2 + se_B25091_001^2),
         e_burdened_housing = e_B25070_007 + e_B25070_008 + e_B25070_009 + e_B25070_010 +
           e_B25091_008 + e_B25091_009 + e_B25091_010 + e_B25091_011 +
           e_B25091_019 + e_B25091_020 + e_B25091_021 + e_B25091_022,
         se_burdened_housing = sqrt(se_B25070_007^2 + se_B25070_008^2 + se_B25070_009^2 + se_B25070_010^2 +
                                      se_B25091_008^2 + se_B25091_009^2 + se_B25091_010^2 + se_B25091_011^2 +
                                      se_B25091_019^2 + se_B25091_020^2 + se_B25091_021^2 + se_B25091_022^2),
         e_nativity_pop = e_B05002_001,
         se_nativity_pop = se_B05002_001,
         e_native_pop = e_B05002_002,
         se_native_pop = se_B05002_002,
         e_foreignborn_pop = e_B05002_013,
         se_foreignborn_pop = se_B05002_013) %>%
  mutate(total_pop = map_tab(e_total_pop, se_total_pop, rnd = 0),
         e_pct_nhw = prop(e_nhw, e_total_pop),
         se_pct_nhw = prop_se(e_nhw, e_total_pop, se_nhw, se_total_pop),
         pct_nhw = map_tab(e_pct_nhw, se_pct_nhw),
         e_pct_nhb = prop(e_nhb, e_total_pop),
         se_pct_nhb = prop_se(e_nhb, e_total_pop, se_nhb, se_total_pop),
         pct_nhb = map_tab(e_pct_nhb, se_pct_nhb),
         e_pct_hl = prop(e_hl, e_total_pop),
         se_pct_hl = prop_se(e_hl, e_total_pop, se_hl, se_total_pop),
         pct_hl = map_tab(e_pct_hl, se_pct_hl),
         e_pct_under5 = prop(e_under5, e_total_pop),
         se_pct_under5 = prop_se(e_under5, e_total_pop, se_under5, se_total_pop),
         pct_under5 = map_tab(e_pct_under5, se_pct_under5),
         e_pct_under18 = prop(e_under18, e_total_pop),
         se_pct_under18 = prop_se(e_under18, e_total_pop, se_under18, se_total_pop),
         pct_under18 = map_tab(e_pct_under18, se_pct_under18),
         e_pct_65plus = prop(e_65plus, e_total_pop),
         se_pct_65plus = prop_se(e_65plus, e_total_pop, se_65plus, se_total_pop),
         pct_65plus = map_tab(e_pct_65plus, se_pct_65plus),
         e_pct_anydis_ch = prop(e_anydis_ch, e_anydis_chtot),
         se_pct_anydis_ch = prop_se(e_anydis_ch, e_anydis_chtot, se_anydis_ch, se_anydis_chtot),
         pct_anydis_ch = map_tab(e_pct_anydis_ch, se_pct_anydis_ch),
         e_pct_anydis_ad = prop(e_anydis_ad, e_anydis_adtot),
         se_pct_anydis_ad = prop_se(e_anydis_ad, e_anydis_adtot, se_anydis_ad, se_anydis_adtot),
         pct_anydis_ad = map_tab(e_pct_anydis_ad, se_pct_anydis_ad),
         e_pct_hearing_ch = prop(e_hearing_ch, e_hearing_chtot),
         se_pct_hearing_ch = prop_se(e_hearing_ch, e_hearing_chtot, se_hearing_ch, se_hearing_chtot),
         pct_hearing_ch = map_tab(e_pct_hearing_ch, se_pct_hearing_ch),
         e_pct_hearing_ad = prop(e_hearing_ad, e_hearing_adtot),
         se_pct_hearing_ad = prop_se(e_hearing_ad, e_hearing_adtot, se_hearing_ad, se_hearing_adtot),
         pct_hearing_ad = map_tab(e_pct_hearing_ad, se_pct_hearing_ad),
         e_pct_vision_ch = prop(e_vision_ch, e_vision_chtot),
         se_pct_vision_ch = prop_se(e_vision_ch, e_vision_chtot, se_vision_ch, se_vision_chtot),
         pct_vision_ch = map_tab(e_pct_vision_ch, se_pct_vision_ch),
         e_pct_vision_ad = prop(e_vision_ad, e_vision_adtot),
         se_pct_vision_ad = prop_se(e_vision_ad, e_vision_adtot, se_vision_ad, se_vision_adtot),
         pct_vision_ad = map_tab(e_pct_vision_ad, se_pct_vision_ad),
         e_pct_cognitive_ch = prop(e_cognitive_ch, e_cognitive_chtot),
         se_pct_cognitive_ch = prop_se(e_cognitive_ch, e_cognitive_chtot, se_cognitive_ch, se_cognitive_chtot),
         pct_cognitive_ch = map_tab(e_pct_cognitive_ch, se_pct_cognitive_ch),
         e_pct_cognitive_ad = prop(e_cognitive_ad, e_cognitive_adtot),
         se_pct_cognitive_ad = prop_se(e_cognitive_ad, e_cognitive_adtot, se_cognitive_ad, se_cognitive_adtot),
         pct_cognitive_ad = map_tab(e_pct_cognitive_ad, se_pct_cognitive_ad),
         e_pct_ambulatory_ch = prop(e_ambulatory_ch, e_ambulatory_chtot),
         se_pct_ambulatory_ch = prop_se(e_ambulatory_ch, e_ambulatory_chtot, se_ambulatory_ch, se_ambulatory_chtot),
         pct_ambulatory_ch = map_tab(e_pct_ambulatory_ch, se_pct_ambulatory_ch),
         e_pct_ambulatory_ad = prop(e_ambulatory_ad, e_ambulatory_adtot),
         se_pct_ambulatory_ad = prop_se(e_ambulatory_ad, e_ambulatory_adtot, se_ambulatory_ad, se_ambulatory_adtot),
         pct_ambulatory_ad = map_tab(e_pct_ambulatory_ad, se_pct_ambulatory_ad),
         e_pct_selfcare_ch = prop(e_selfcare_ch, e_selfcare_chtot),
         se_pct_selfcare_ch = prop_se(e_selfcare_ch, e_selfcare_chtot, se_selfcare_ch, se_selfcare_chtot),
         pct_selfcare_ch = map_tab(e_pct_selfcare_ch, se_pct_selfcare_ch),
         e_pct_selfcare_ad = prop(e_selfcare_ad, e_selfcare_adtot),
         se_pct_selfcare_ad = prop_se(e_selfcare_ad, e_selfcare_adtot, se_selfcare_ad, se_selfcare_adtot),
         pct_selfcare_ad = map_tab(e_pct_selfcare_ad, se_pct_selfcare_ad),
         e_pct_indep_ad = prop(e_indep_ad, e_indep_adtot),
         se_pct_indep_ad = prop_se(e_indep_ad, e_indep_adtot, se_indep_ad, se_indep_adtot),
         pct_indep_ad = map_tab(e_pct_indep_ad, se_pct_indep_ad),
         e_pct_eng_ltw = prop(e_eng_ltw, e_5plus),
         se_pct_eng_ltw = prop_se(e_eng_ltw, e_5plus, se_eng_ltw, se_5plus),
         pct_eng_ltw = map_tab(e_pct_eng_ltw, se_pct_eng_ltw),
         e_pct_children_shh = prop(e_children_shh, e_children),
         se_pct_children_shh = prop_se(e_children_shh, e_children, se_children_shh, se_children),
         pct_children_shh = map_tab(e_pct_children_shh, se_pct_children_shh),
         e_pct_children_ghh = prop(e_children_ghh, e_children),
         se_pct_children_ghh = prop_se(e_children_ghh, e_children, se_children_ghh, se_children),
         pct_children_ghh = map_tab(e_pct_children_ghh, se_pct_children_ghh),
         per_capita_income = map_tab(e_pc_income, se_pc_income, rnd = 0),
         median_hh_income = map_tab(e_hh_income, se_hh_income, rnd = 0),
         e_pct_hh_poverty = prop(e_hh_poverty, e_total_hh),
         se_pct_hh_poverty = prop_se(e_hh_poverty, e_total_hh, se_hh_poverty, se_total_hh),
         pct_hh_poverty = map_tab(e_pct_hh_poverty, se_pct_hh_poverty),
         e_pct_hh_snap = prop(e_hh_snap, e_total_hh),
         se_pct_hh_snap = prop_se(e_hh_snap, e_total_hh, se_hh_snap, se_total_hh),
         pct_hh_snap = map_tab(e_pct_hh_snap, se_pct_hh_snap),
         e_pct_unemployed = prop(e_civ_pop_unemp, e_civ_pop_lf),
         se_pct_unemployed = prop_se(e_civ_pop_unemp, e_civ_pop_lf, se_civ_pop_unemp, se_civ_pop_lf),
         pct_unemployed = map_tab(e_pct_unemployed, se_pct_unemployed),
         e_pct_no_insurance = prop(e_no_ins, e_ins_pop),
         se_pct_no_insurance = prop_se(e_no_ins, e_ins_pop, se_no_ins, se_ins_pop),
         pct_no_insurance = map_tab(e_pct_no_insurance, se_pct_no_insurance),
         e_pct_less_hs_edu = prop(e_less_hs, e_25plus_pop),
         se_pct_less_hs_edu = prop_se(e_less_hs, e_25plus_pop, se_less_hs, se_25plus_pop),
         pct_less_hs_edu = map_tab(e_pct_less_hs_edu, se_pct_less_hs_edu),
         e_pct_bs_plus_edu = prop(e_bs_plus, e_25plus_pop),
         se_pct_bs_plus_edu = prop_se(e_bs_plus, e_25plus_pop, se_bs_plus, se_25plus_pop),
         pct_bs_plus_edu = map_tab(e_pct_bs_plus_edu, se_pct_bs_plus_edu),
         e_pct_rented_housing = prop(e_renter_units, e_occ_units),
         se_pct_rented_housing = prop_se(e_renter_units, e_occ_units, se_renter_units, se_occ_units),
         pct_rented_housing = map_tab(e_pct_rented_housing, se_pct_rented_housing),
         e_pct_owner_housing = prop(e_owner_units, e_occ_units),
         se_pct_owner_housing = prop_se(e_owner_units, e_occ_units, se_owner_units, se_occ_units),
         pct_owner_housing = map_tab(e_pct_owner_housing, se_pct_owner_housing),
         e_pct_rented_pop = prop(e_housing_pop_renter, e_housing_pop),
         se_pct_rented_pop = prop_se(e_housing_pop_renter, e_housing_pop, se_housing_pop_renter, se_housing_pop),
         pct_rented_pop = map_tab(e_pct_rented_pop, se_pct_rented_pop),
         e_pct_owner_pop = prop(e_housing_pop_owner, e_housing_pop),
         se_pct_owner_pop = prop_se(e_housing_pop_owner, e_housing_pop, se_housing_pop_owner, se_housing_pop),
         pct_owner_pop = map_tab(e_pct_owner_pop, se_pct_owner_pop),
         e_pct_pre1980_housing = prop(e_housing_pre1980, e_all_units),
         se_pct_pre1980_housing = prop_se(e_housing_pre1980, e_all_units, se_housing_pre1980, se_all_units),
         pct_pre1980_housing = map_tab(e_pct_pre1980_housing, se_pct_pre1980_housing),
         median_housing_value = map_tab(e_med_housing_value, se_med_housing_value, rnd = 0),
         e_pct_hh_no_internet = prop(e_hh_no_internet, e_total_hh),
         se_pct_hh_no_internet = prop_se(e_hh_no_internet, e_total_hh, se_hh_no_internet, se_total_hh),
         pct_hh_no_internet = map_tab(e_pct_hh_no_internet, se_pct_hh_no_internet),
         e_pct_lack_plumbing = prop(e_occ_lack_plumbing, e_occ_units),
         se_pct_lack_plumbing = prop_se(e_occ_lack_plumbing, e_occ_units, se_occ_lack_plumbing, se_occ_units),
         pct_lack_plumbing = map_tab(e_pct_lack_plumbing, se_pct_lack_plumbing),
         e_pct_lack_kitchen = prop(e_occ_lack_kitchen, e_occ_units),
         se_pct_lack_kitchen = prop_se(e_occ_lack_kitchen, e_occ_units, se_occ_lack_kitchen, se_occ_units),
         pct_lack_kitchen = map_tab(e_pct_lack_kitchen, se_pct_lack_kitchen),
         e_pct_crowded = prop(e_occ_crowded, e_occ_units),
         se_pct_crowded = prop_se(e_occ_crowded, e_occ_units, se_occ_crowded, se_occ_units),
         pct_crowded = map_tab(e_pct_crowded, se_pct_crowded),
         e_pct_cost_burdened = prop(e_occ_cost_burdened, e_occ_units),
         se_pct_cost_burdened = prop_se(e_occ_cost_burdened, e_occ_units, se_occ_cost_burdened, se_occ_units),
         pct_cost_burdened = map_tab(e_pct_cost_burdened, se_pct_cost_burdened),
         e_pct_housing_burdened = prop(e_burdened_housing, e_all_housing),
         se_pct_housing_burdened = prop_se(e_burdened_housing, e_all_housing, se_burdened_housing, se_all_housing),
         pct_housing_burdened = map_tab(e_pct_housing_burdened, se_pct_housing_burdened),
         e_pct_foreignborn = prop(e_foreignborn_pop, e_nativity_pop),
         se_pct_foreignborn = prop_se(e_foreignborn_pop, e_nativity_pop, se_foreignborn_pop, se_nativity_pop),
         pct_foreignborn = map_tab(e_pct_foreignborn, se_pct_foreignborn)) %>%
  select(!starts_with(c("e_", "se_")))

write_csv(acs_tracts_df, file = here::here("data", "acs_2021_tracts.csv"))

acs_tracts <- left_join(tracts20, acs_tracts_df, by = "GEOID")
acs_tracts_long <- acs_tracts %>%
  pivot_longer(cols = total_pop:pct_foreignborn,
               names_to = "var", values_to = "value")

save(acs_tracts, acs_tracts_long, file = here::here("data", "acs_tracts.rdata"))

#' -----------------------------------------------------------------------------
#' Average ACS variables to the neighborhood level
#' -----------------------------------------------------------------------------

neighborhoods
tracts20

ggplot() +
  geom_sf(data = tracts20, aes(fill = ALAND)) +
  geom_sf(data = neighborhoods, fill = NA, color = "red")

tract_neighborhoods <- st_join(tracts20, neighborhoods, largest = T) %>%
  select(GEOID, NBHD_NAME) %>%
  mutate(NBHD_NAME = ifelse(NBHD_NAME %in% c("Globeville", "Elyria Swansea"),
                            "Globeville - Elyria Swansea", NBHD_NAME))

tract_pops <- get_acs(geography = "tract", year = 2021, variables = "B01001_001",
                      state = 08, county = 31) %>%
  select(GEOID, pop = estimate)

#' Population-weighted mean of ACS variables
#' Weight by total population

test <- acs_tracts_df %>%
  left_join(tract_neighborhoods, by = "GEOID") %>%
  select(NBHD_NAME, GEOID, starts_with("pct_")) %>%
  pivot_longer(cols = !c(NBHD_NAME, GEOID), names_to = "variable", values_to = "pct") %>%
  left_join(tract_pops, by = "GEOID") %>%
  filter(NBHD_NAME == "Globeville - Elyria Swansea" & variable == "pct_hl")
test

acs_neighborhoods_df <- acs_tracts_df %>%
  left_join(tract_neighborhoods, by = "GEOID") %>%
  select(NBHD_NAME, GEOID, starts_with("pct_")) %>%
  pivot_longer(cols = !c(NBHD_NAME, GEOID), names_to = "variable", values_to = "pct") %>%
  left_join(tract_pops, by = "GEOID") %>%
  group_by(NBHD_NAME, variable) %>%
  summarize(mean_pct = weighted.mean(pct, pop, na.rm = T)) %>%
  pivot_wider(id_cols = NBHD_NAME, 
              names_from = "variable", values_from = "mean_pct")

acs_neighborhoods <- left_join(neighborhoods, acs_neighborhoods_df,
                               by = "NBHD_NAME")

acs_neighborhoods_long <- acs_neighborhoods %>%
  pivot_longer(cols = -c(NBHD_NAME, geometry),
               names_to = "var", values_to = "value")

write_csv(acs_neighborhoods_df, file = here::here("data", "acs_2021_neighborhoods.csv"))
save(acs_neighborhoods, acs_neighborhoods_long, 
     file = here::here("data", "acs_neighborhoods.rdata"))

