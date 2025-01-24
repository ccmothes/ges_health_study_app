# GES Health Study Interactive Maps Application
The code in this repository was used to develop the GES Health Study Interactive 
Maps application. More information about the GES Health Study
is available on our [project website](http://geshealthstudy.org).

This project is a collaboration between the GES Health Study Community Council 
(Nancy Santos, Dolores Alfaro, Ana Varela, and Sandra Ruiz Parrilla) and the 
ENVIRONS team (Dr. Sheryl Magzamen, Dr. Sheena Martenies, Beth Lunsford, 
Nona Nyhart, Anna Kenyon, and Rocio Monroy-Tello). Former members of the GES 
Community Council who contributed to the interpretation of community data featured 
in this tool include Maria de Santiago and Rebecca Trujillo. Marshall Thomas 
(former ENVIRONS team member) also contributed to the development of the data sets 
used in this tool.

The GES Health Study Interactive Map Application was developed using Shiny for R.

### File Inventory

This repository contained all of the code and data files needed to launch the 
GES Health Study Interactive Maps Application using Shiny for R.

- **app.R** contains the app's UI and server functions.
- **/ges_health_study_app**: This folder includes files required to run the application:
    - dictionary_english.xlsx: The English version of the variable dictionary used to generate the maps and other figures
    - dictionary_spanish.xlsx: The Spanish version of the variable dictionary used to generate the maps and other figures
    - ges_app_data.rdata: Final clean data used by the app (generated using ges_health_study_app/code/03_final_data_sets.R) and other
    ancillary data frames used by the app
    - ges_app_data.csv: Final clean data (population, environment, and health variables) used by the app in a CSV file
    - ges_app_data_dictionary.xlsx: Data dictionary describing the variables used in the app
    - helpers.R: Code for all of the packages, dictionaries, and plotting functions used by the app
    - text_dictionary.xlsx: A spreadsheet containing the English and Spanish versions of the text used in the app
- **/ges_health_study_app/code**: This folder includes the following scripts used to develop the app:
    - 01_format_shapefiles.R: cleaning and formatting shapefile data for the app
    - 02_acs_data.R: Using the tidycensus package to obtain and clean census variables
    - 03_final_data_sets.R: Code to format and save the final data sets for the app
- **rsconnect**: This folder contains files required for deployment on shinyapps.io (generated at deployment)
- **www**: This folder contains the .html and .png files for non-interactive maps and figures used in the app

### Data Sources
Population data were obtained from the 5-Year American Community Survey (2017-2021) 
from the [US Census Bureau](https://www.census.gov/programs-surveys/acs/news/data-releases.2021.html#list-tab-1133175109).

Health data were obtained from [CDC PLACES 2023 Release](https://data.cdc.gov/500-Cities-Places/PLACES-Local-Data-for-Better-Health-Place-Data-202/krqc-563j/about_data) and
[Colorado Enviroscreen 2.0](https://drive.google.com/file/d/12x7HmTJk6gqn7zidCwoSbXdpgAwpQTHb/view).

Life expectancy data were obtained from the [USALEEP Project](https://www.cdc.gov/nchs/nvss/usaleep/usaleep.html#data).

Environmental data were obtained from a number of sources:

- Criteria air pollutant (PM<sub>2.5</sub>, PM<sub>10</sub>, O<sub>3</sub>, NO<sub>2</sub>, SO<sub>2</sub>, and CO) data were 
obtained from [US EPA](https://aqs.epa.gov/aqsweb/documents/data_api.html)
- Alcohol retailer and cannabis cultivation license information were obtained from 
the [Colorado Department of Revenue](https://sbg.colorado.gov/) and food retail 
location data were obtained from [Denver Open Data](https://opendata-geospatialdenver.hub.arcgis.com/datasets/c65b86c4881044df859913bc0d9075a5_0/explore)
- Grocery store access data were obtained from [Denver Open Data](https://opendata-geospatialdenver.hub.arcgis.com/datasets/8721ca222d4348d3a6a0d74e7175309f_308/explore)
- Street temperature data were obtained from [Denver Green Continuum](https://www.denvergov.org/Government/Agencies-Departments-Offices/Agencies-Departments-Offices-Directory/Department-of-Transportation-and-Infrastructure/Programs-Services/Green-Infrastructure/Green-Continuum)
- Transportation noise data were obtained from [US Department of Transportation](https://www.bts.gov/geospatial/national-transportation-noise-map)
- Tree Equity Score data were obtained from [American Forests](https://www.americanforests.org/our-programs/tree-equity/)
- Per-1980 housing data obtained from the 5-Year American Community Survey (2017-2021) 
from the [US Census Bureau](https://www.census.gov/programs-surveys/acs/news/data-releases.2021.html#list-tab-1133175109)

Data used to develop the app are available [here--TO BE ADDED]().

### Overview of Population, Health, and Environmental Indicators
Our interactive mapping tool is an extension of our previous health impact assessment 
(HIA) conducted for the GES neighborhoods published in January 2024. This HIA is available
online in both [English](https://drive.google.com/file/d/1X5zeZe-dITYX2soG2wS891wp51Bb9oww/view?usp=drive_link) 
and [Spanish](https://drive.google.com/file/d/1fSWFgGYCd_53V4Po7EITAXYR04tNHefr/view?usp=drive_link). 
Due to the statutory limitations of our funding, we were not able to collect 
primary health or environmental data as part of the GES Health Study. Therefore, 
we included only data that were publicly available or had already been collected 
during previous research efforts. 

We further refined and updated (when needed) our HIA data for inclusion in the 
interactive mapping tool to ensure we were presenting data that were (A) sufficient 
to represent long-term trends at the neighborhood level and (B) of interest to 
the community. Our initial data selection focused on population characteristics 
and commonly used indicators of population health and air quality. Incorporating 
information we have received via conversations with our Community Council, we also 
aimed to include built environment data that reflected the lived experiences of 
neighborhood residents, e.g., access to grocery stores and cannabis cultivation 
sites. When feasible, we selected data that represented a longer period of time 
(i.e., five years) over data representing shorter durations and prioritized data 
that could be presented at the neighborhood level. Point-level data included in 
the HIA that could not be reasonable interpolated (e.g., water quality sampling 
locations along the South Platt River) were excluded. 

Our interactive mapping tool includes 10 indicators of population characteristics 
(race and ethnicity, educational attainment, age distributions, unemployment, 
insurance status, and nutritional assistance). These data were obtained at the 
census tract level from the 2017-2021 5-Year American Community Survey data 
release from the US Census Bureau1 using the tidycensus package in R.

We included 13 indicators of the physical, chemical, and built environments, 
including measures of food access, street temperature and noise, tree equity, 
and criteria air pollutants. Sources for these data include the City and County 
of Denver, the Colorado Department of Revenue,4 Denver Department of Transportation, 
the US Department of Transportation, American Forests, the US Census Bureau, and 
the US Environmental Protection Agency.

We included 25 indicators of health at the neighborhood level. These data were 
obtained primarily from the 2023 release of PLACES data from the Centers for 
Disease Control and Prevention. Life expectancy data were obtained from U.S. 
Small-Area Life Expectancy Estimates Project (USALEEP) and asthma 
hospitalization and low birth weight estimates were obtained from Colorado 
Enviroscreen 2.0.

### Overview of Data Formatting Methods
Data used in the GES Health Study Interactive Maps app were available at the point, 
30-m raster, or census tract level. Census tracts were assigned to a statistical 
neighborhood by spatially joined the census tracts with neighborhood boundaries 
made available from Denver Open Data. If a census tract overlapped with more 
than one neighborhood, it was assigned the neighborhood with the largest overlap.

To obtain neighborhood level estimates of population characteristics and health 
outcome indicators, we calculated the population-weighted mean of the census tract 
estimates. Population data were obtained from the US Census Bureau’s American 
Community Survey. For the population characteristics presented as percentages, 
we weighted the census tract estimates by the total population in the census tract. 
Health data were obtained primarily from the 2023 release of the PLACES data from 
the Centers for Disease Control and Prevention. Life expectancy data were obtained 
from U.S. Small-Area Life Expectancy Estimates Project (USALEEP)  and asthma 
hospitalization and low birth weight estimates were obtained from Colorado 
Enviroscreen 2.0. For the health indicators presented as rates, we weighted the 
census tract estimates using population estimates that corresponded with the age 
group represented by the indicator (e.g., for “Preventive Care among Older Men” 
we used the ACS estimates for the male populations ages 65 and older).

Estimates of the ratio of alcohol retailers and cannabis cultivation sites to grocery 
stores were based on licensing data from the Colorado Department of Revenue. 
Alcohol licenses were limited to off premises license types, including the following 
types: fermented malt beverage and wine off-premises, liquor licensed drug store, 
and retail liquor stores. To identify alcohol retailers, food markets and groceries, 
we downloaded data from 2021 for food market and groceries in Denver from the Denver 
Open Data Catalog. License locations were first geocoded, and then we used a 
gravity model approach to calculate spatial access following methods described by 
Trangenstein et al. The number of cannabis sites, alcohol retailers, and food 
locations were summarized for each statistical neighborhood in Denver, and the 
ratio of ratio of cannabis cultivation and alcohol retailers to food markets and 
groceries was calculated.

Data on surface temperatures averaged at the street segment level were made 
available to the ENVIRONS team by Denver Department of Transportation and 
Infrastructure (DDOTI) Division of Green Infrastructure. These data were originally 
published in their Denver Green Continuum Streets Guidelines in October, 2021. 
Specific details on how these data were generated are available in elsewhere. 
Because DDOTI originally summarized the land surface temperature data by street 
segment and then calculated a percentile rank, we opted to not calculate 
area-specific estimates of temperature. Instead, we calculated the average 
percentile rank of all streets that intersected with the neighborhood boundary.

Transportation noise was available from the US Department of Transportation via 
a 30-m raster. We used spatial averaging methods to estimate average exposures 
within the neighborhood. The raster was overlaid with a shapefile containing the 
neighborhood boundaries, and an average of all the values for pixels contained 
within the neighborhood boundaries was calculated.

Tree equity scores were available from American Forests at the census tract level 
and averaged to the neighborhood level for the interactive maps application. 

Criteria air pollutant data were available from the US Environmental Protection 
Agency (US EPA) at the level of the monitoring station. To estimate 
neighborhood-level exposures, we first averaged the daily measurements obtained 
from US EPA to calculate the long-term pollutant measurements at each location. 
Then, we used inverse-distance weighting to estimate long-term averages at the 
centroid of each neighborhood. Any monitoring data within 50 km of the census 
tract centroid were included. The number of locations for interpolation varied by 
pollutant. The most data were available for ozone (O3 ;11 monitoring locations) 
and particulate matter 2.5 (PM2.5 ;9 monitoring locations), followed by nitrogen 
dioxide (NO2 ;6 locations), particulate matter 10 (PM10 ;5 locations), carbon 
monoxide (CO ;4 locations), and sulfur dioxide (SO2 ;3 locations).
