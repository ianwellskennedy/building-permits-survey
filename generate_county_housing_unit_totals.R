# Packages ---- 

# Set the package names to read in
packages <- c("tidyverse", "tidycensus", "openxlsx", "writexl", "ggmap", "arcgisbinding", "sf", "rmapshaper", "conflicted", "rvest", "stringr")

# Install packages that are not yet installed
installed_packages <- packages %in% rownames(installed.packages())

if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Load the packages
invisible(lapply(packages, library, character.only = TRUE))

# Remove unneeded variables
rm(packages, installed_packages)

# File paths ----

county_housing_stock_file_path <- "inputs/CO-EST2024-HU.xlsx"
county_building_permits_file_path <- "inputs/county_level_building_permits_2024_thru_2025Q2.xlsx"
county_shp_file_path <- "C:/Users/ianwe/Downloads/shapefiles/2023/Counties/cb_2023_us_county_500k.shp"

output_tabular_data_file_path <- "outputs/shapefiles/permit_share_of_housing_units_by_county.xlsx"
output_shp_file_path <- "outputs/shapefiles/permit_share_of_housing_units_by_county.shp"

# # Read in data ---- 
# 
# county_housing_stock <- read.xlsx(county_housing_stock_file_path, sheet = 'clean')
# county_shapefile <- st_read(county_shp_file_path)
# 
# county_building_permits_2024 <- read.xlsx(county_building_permits_file_path, sheet = '2024 Annual')
# county_building_permits_2024Q1 <- read.xlsx(county_building_permits_file_path, sheet = '2024 Q1')
# county_building_permits_2024Q2 <- read.xlsx(county_building_permits_file_path, sheet = '2024 Q2')
# county_building_permits_2025Q1 <- read.xlsx(county_building_permits_file_path, sheet = '2025 Q1')
# county_building_permits_2025Q2 <- read.xlsx(county_building_permits_file_path, sheet = '2025 Q2')

# Downloading permit files  ----

# URL of the directory
url <- "https://www2.census.gov/econ/bps/County/"

# Read the HTML of the directory
page <- read_html(url)

# Extract all links from the page
links <- page %>%
  html_nodes("a") %>%
  html_attr("href")

# Keep only links that contain a "c"
permit_files <- links %>%
  str_subset("c.txt")     # Filter for filenames containing "c.txt"

# Create full URLs
permit_file_urls <- paste0(url, permit_files)

# Create a download folder
download_dir <- "inputs/raw-bps-downloads"

# Download all matching files
for (i in seq_along(c_file_urls)) {
  
  dest_file <- file.path(download_dir, basename(c_file_urls[i]))
  message("Downloading: ", c_file_urls[i])
  
  tryCatch({
    download.file(c_file_urls[i], dest_file, mode = "wb")
  }, error = function(e) {
    message("Failed to download: ", c_file_urls[i])
  })
}

message("Download complete! Files are saved in: ", download_dir)

# Clean housing stock data and shape file ----

county_shapefile <- county_shapefile %>%
  select(GEOID, NAME, NAMELSAD, STATE_NAME) %>%
  rename(county_fips_code = GEOID, county_name = NAMELSAD, county_short_name = NAME, state = STATE_NAME) %>%
  filter(!state %in% c("American Samoa", "Commonwealth of the Northern Mariana Islands", "Puerto Rico", "United States Virgin Islands", "Guam"))

county_housing_stock <- county_housing_stock %>%
  mutate(county = str_remove(county, '^\\.')) %>%
  mutate(state = str_extract(county, ",\\s*(.*)") %>% str_remove("^,\\s*"),
         county = str_trim(str_remove(county, ",.*"))) %>%
  rename(county_name = county)

county_housing_stock <- county_housing_stock %>%
  left_join(county_shapefile, by = c('county_name', 'state'))

# Clean building permits data ----

clean_county_building_permits <- function(data){
  data <- data %>%
    mutate(county_fips_code = paste0(state_fips_code, county_fips_code),
           total_units = one_unit_units + two_unit_units + three_four_unit_units + five_plus_unit_units,
           county_name = str_trim(county_name)) %>%
    select(county_fips_code, total_units)
}

county_building_permits_2024 <- clean_county_building_permits(county_building_permits_2024) %>%
  rename(total_units_2024 = total_units)
county_building_permits_2024Q1 <- clean_county_building_permits(county_building_permits_2024Q1) %>%
  rename(total_units_2024Q1 = total_units)
county_building_permits_2024Q2 <- clean_county_building_permits(county_building_permits_2024Q2) %>%
  rename(total_units_2024Q2 = total_units)
county_building_permits_2025Q1 <- clean_county_building_permits(county_building_permits_2025Q1) %>%
  rename(total_units_2025Q1 = total_units)
county_building_permits_2025Q2 <- clean_county_building_permits(county_building_permits_2025Q2) %>%
  rename(total_units_2025Q2 = total_units)

county_building_permits <- county_building_permits_2024 %>%
  left_join(county_building_permits_2024Q1, by = c('county_fips_code')) %>%
  left_join(county_building_permits_2024Q2, by = c('county_fips_code')) %>%
  left_join(county_building_permits_2025Q1,  by = c('county_fips_code')) %>%
  left_join(county_building_permits_2025Q2, by = c('county_fips_code')) 

county_building_permits <- county_building_permits %>%
  mutate(total_units = total_units_2024 + total_units_2025Q2 - total_units_2024Q2) %>%
  mutate(total_units = case_when(
    is.na(total_units) ~ 0,
    total_units < 0 ~ 0,
    T ~ total_units
  )) %>%
  select(county_fips_code, total_units, total_units_2025Q2, total_units_2025Q1, total_units_2024Q2, total_units_2024)

rm(county_building_permits_2024, county_building_permits_2025Q1, county_building_permits_2025Q2, county_building_permits_2024Q1, county_building_permits_2024Q2)

county_data_joined <- county_housing_stock %>%
  left_join(county_building_permits, by = 'county_fips_code')

missing <- county_data_joined %>%
  filter(is.na(total_units))

county_data_joined <- county_data_joined %>%
  filter(!is.na(total_units)) %>%
  select(county_name, county_short_name, county_fips_code, state, housing_units_2024, starts_with('total_units'), geometry)

county_data_joined <- county_data_joined %>%
  mutate(sh_of_stock = total_units / housing_units_2024,
         sh_of_stock_Q2 = (total_units_2025Q2 - total_units_2025Q1) / housing_units_2024)

# Read in county shapefile ----

# Convert 'Counties' back to a spatial data frame
county_data_joined <- st_as_sf(county_data_joined)

# Check to make sure there is an Active ArcGIS Installation
arc.check_product()

# Output the ACS county data to the path specified
arc.write(path = output_file_path, data = county_data_joined, overwrite = T, validate = T)
