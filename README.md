# GES Health Study Interactive Maps Application
The code in this repository was used to develop the GES Health Study Interactive 
Maps application. More information about the GES Health Study
is available on our [project website](http://geshealthstudy.org).

This project is a collaboration between the GES Health Study Community Council 
(Nancy Santos, Dolores Alfaro, Ana Varela, and Sandra Ruiz Parrilla) and the 
ENVIRONS team (Dr. Sheryl Magzamen, Dr. Sheena Martenies, Beth Lunsford, 
Nona Nyhart, Anna Kenyon, and Rocio Monroy-Tello). Former members of the GES 
Community Council who contributed to the interpretation of community data featured 
in this tool include Maria de Santiago, and Rebecca Trujillo. Marshall Thomas 
(former ENVIRONS team member) also contributed to the development of the data sets 
used in this tool.

The GES Health Study Interactive Map Application was developed using Shiny for R.

### File Inventory

This repository contained all of the code and data files needed to launch the 
GES Health Study Interactive Maps Application using Shiny for R.

- **app.R** contains the app's UI and server functions.
- **/ges_health_study_app**: This folder includes files required to run the application:
    - dictionary_english.xlsx: The English version of the data dictionary used to generate the maps and other figures
    - dictionary_spanish.xlsx: The Spanish version of the data dictionary used to generate the maps and other figures
    - ges_app_data.rdata: Final clean data used by the app (generated using ges_health_study_app/code/03_final_data_sets.R)
    - helpers.R: Code for all of the packages, dictionaries, and plotting functions used by the app
    - text_dictionary.xlsx: A spreadsheet containing the English and Spanish versions of the text used in the app
- **/ges_health_study_app/code**: This folder includes the following scripts used to develop the app:
    - 01_format_shapefiles.R: cleaning and formatting shapefile data for the app
    - 02_acs_data.R: Using the tidecensus package to obtain and clean census variables
    - 03_final_data_sets.R: Code to format and save the final data sets for the app
- **www**: This folder contains the .html and .png files used in the app

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

### Overview of Data Formatting Methods
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
census tract level from the 2017-2021 5-Year American Community Survey data release 
from the US Census Bureau. These data were obtained from the US Census Bureau API 
using the tidycensus package in R.

We included 13 indicators of the physical, chemical, and built environments, 
including measures of food access, heat and noise, tree equity, and criteria air 
pollutants. 

Most data were available at the point, 30-m raster, or census tract level. To 
obtain neighborhood level estimates of indicators available at the census tract level 
(i.e., population characteristics and health indicators), we spatially joined the 
census tracts with neighborhood boundaries made available from Denver Open Data 
and calculated the population-weighted mean of the census tract estimates. If a 
census tract overlapped with more than one neighborhood, it was assigned the 
neighborhood with the largest overlap. For the population characteristics, we 
weighted the census tract estimates by the total population in the census tract. 
For the health indicators, we weighted the census tract estimates using population 
estimates that corresponded with the age group represented by the indicator (e.g., 
for “Preventive Care among Older Men” we used the ACS estimates for the male 
populations ages 65 and older).

Criteria air pollutant data were available from the US Environmental Protection 
Agency (US EPA) at the level of the monitoring station. To estimate neighborhood-level
exposures, we first averaged the daily measurements obtained from US EPA to calculate 
the long-term pollutant measurements at each location. Then, we used inverse-distance 
weighting to estimate long-term averages at the centroid of each neighborhood. 
Any monitoring data within 50 km of the census tract centroid were included. The 
number of locations for interpolation varied by pollutant. The most data were 
available for O<sub>3</sub> (11 monitoring locations) and PM<sub>2.5</sub> (9 monitoring locations), 
followed by NO<sub>2</sub> (6 locations), PM<sub>10</sub> (5 locations), CO (4 locations), 
and SO<sub>2</sub> (3 locations). 

For indicators available at the raster level (i.e., transportation noise), we used 
spatial averaging methods to estimate exposures within the neighborhood. The rasters 
were overlaid with a shapefile containing the neighborhood boundaries, and an 
average of all the values for pixels contained within the neighborhood boundaries was 
calculated. 






