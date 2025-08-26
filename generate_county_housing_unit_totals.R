# Packages ---- 

# Set the package names to read in
packages <- c("tidyverse", "tidycensus", "openxlsx", "writexl", "ggmap", "arcgisbinding", "sf", "rmapshaper", "conflicted", "rvest", "stringr", "janitor")

# Install packages that are not yet installed
installed_packages <- packages %in% rownames(installed.packages())

if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Load the packages
invisible(lapply(packages, library, character.only = TRUE))

# Remove unneeded variables
rm(packages, installed_packages)

conflicts_prefer(dplyr::filter)

# File paths / environment variables ----

county_housing_stock_file_path <- "inputs/CO-EST2024-HU.xlsx"
county_shp_file_path <- "C:/Users/ianwe/Downloads/shapefiles/2023/Counties/cb_2023_us_county_500k.shp"

county_building_permits_folder <- "inputs/raw-bps-downloads/co"

output_tabular_data_file_path <- "outputs/permit_share_of_housing_units_by_county.xlsx"
output_trailing_twelve_months_shp_file_path <- "C:/Users/ianwe/Downloads/ArcGIS projects for github/building-permits-survey/shapefiles/permit_share_of_housing_units_by_county_trailing_twelve_months.shp"
output_trailing_three_months_shp_file_path <- "C:/Users/ianwe/Downloads/ArcGIS projects for github/building-permits-survey/shapefiles/permit_share_of_housing_units_by_county_trailing_three_months.shp"
output_current_month_shp_file_path <- "C:/Users/ianwe/Downloads/ArcGIS projects for github/building-permits-survey/shapefiles/permit_share_of_housing_units_by_county_current_month.shp"

# # Downloading permit files  ----
# 
# # URL of the directory
# url <- "https://www2.census.gov/econ/bps/County/"
# 
# # Read the HTML of the directory
# page <- read_html(url)
# 
# # Extract all links from the page
# links <- page %>%
#   html_nodes("a") %>%
#   html_attr("href")
# 
# # Keep only links that contain a "c"
# permit_files <- links %>%
#   str_subset("c.txt")     # Filter for filenames containing "c.txt"
# 
# # Create full URLs
# permit_file_urls <- paste0(url, permit_files)
# 
# # Create a download folder
# download_dir <- "inputs/raw-bps-downloads"
# 
# # Download all matching files
# for (i in seq_along(permit_file_urls)) {
#   
#   dest_file <- file.path(download_dir, basename(permit_file_urls[i]))
#   message("Downloading: ", permit_file_urls[i])
#   
#   tryCatch({
#     download.file(permit_file_urls[i], dest_file, mode = "wb")
#   }, error = function(e) {
#     message("Failed to download: ", permit_file_urls[i])
#   })
# }
# 
# message("Download complete! Files are saved in: ", download_dir)

# Read in county housing stock and shape file ---- 

county_housing_stock <- read.xlsx(county_housing_stock_file_path, sheet = 'clean')
county_shp <- st_read(county_shp_file_path)

# Clean county housing stock and shape file ----

county_shp <- county_shp %>%
  select(NAMELSAD, NAME, STATE_NAME, GEOID) %>%
  rename(county_fips_code = GEOID, county_name = NAMELSAD, county_short_name = NAME, state = STATE_NAME) %>%
  filter(!state %in% c("American Samoa", "Commonwealth of the Northern Mariana Islands", "Puerto Rico", "United States Virgin Islands", "Guam"))

county_shp_info <- county_shp %>%
  st_drop_geometry()

county_housing_stock <- county_housing_stock %>%
  mutate(county = str_remove(county, '^\\.')) %>%
  mutate(state = str_extract(county, ",\\s*(.*)") %>% str_remove("^,\\s*"),
         county = str_trim(str_remove(county, ",.*"))) %>%
  rename(county_name = county)

county_housing_stock <- county_housing_stock %>%
  left_join(county_shp, by = c('county_name', 'state'))

county_shp_geo <- county_shp %>%
  select(county_fips_code, geometry)

# Read in building permits data ----

download_dir <- "inputs/raw-bps-downloads"

# List all .txt files
permit_files <- list.files(download_dir, pattern = "c\\.txt$", full.names = TRUE)
permit_files <- permit_files[grepl("23|24|25", permit_files)]

# Read each file into a list of data frames
permit_data_list <- lapply(permit_files, function(file) {
  message("Reading: ", file)
  read_delim(file, 
             delim = ",",      # Adjust delimiter if needed
             col_names = TRUE, 
             show_col_types = FALSE)
})

# Combine all files into one big data frame
permit_data_all <- bind_rows(permit_data_list, .id = "source_file")

# Generate time-series of permit data ----

permit_data_cleaned <- permit_data_all %>%
  # Filter away unrecognized categories
  filter(FIPS...3 != "000") %>%
  rename(state_fips_code = FIPS...2, county_fips_code = FIPS...3, 
         one_unit_bldgs = ...7, one_unit_units = `1-unit`, one_unit_value =  ...9,
         two_unit_bldgs = ...10, two_unit_units = `2-units`, two_unit_value = ...12, 
         three_four_unit_bldgs = ...13, three_four_unit_units = `3-4 units`, three_four_unit_value = ...15, 
         five_plus_unit_bldgs = ...16, five_plus_unit_units = `5+ units`, five_plus_unit_value = ...18) %>%
  clean_names() %>%
  mutate(county_fips_code = paste0(state_fips_code, county_fips_code)) %>%
  select(survey, county_fips_code, starts_with('one_unit'), starts_with('two_unit'), starts_with('three_four_unit'), starts_with('five_plus_unit')) %>%
  filter(survey != 'Date')

permit_data_cleaned <- permit_data_cleaned %>%
  mutate(across(one_unit_bldgs:five_plus_unit_value, ~as.numeric(.)),
         month = as.Date(paste0(survey, "01"), format = "%Y%m%d"),
         year = year(month),
         quarter = paste0(year, " Q", quarter(month))) %>%
  select(month, quarter, year, everything()) %>%
  select(-survey)

permit_data_cleaned <- permit_data_cleaned %>%
  left_join(county_shp_info, by = 'county_fips_code')

permit_data_cleaned <- permit_data_cleaned %>%
  select(month:year, county_name, county_short_name, county_fips_code, state, month:year, everything()) %>%
  filter(year >= 2023)

# Output tabular data ----

write.xlsx(permit_data_cleaned, output_tabular_data_file_path)

# Output spatial data ----

permit_data_cleaned_spatial_historical <- permit_data_cleaned %>%
  left_join(county_shp_geo, by = 'county_fips_code') %>%
  mutate(month = as.character(month))

twelve_months_ago <- as.Date(max(permit_data_cleaned_spatial_historical$month)) %m-% months(12)
 
permit_data_cleaned_spatial_recent <- permit_data_cleaned %>%
  left_join(county_shp_geo, by = 'county_fips_code') %>%
  filter(month > twelve_months_ago) %>%
  select(-c(ends_with('_value'), ends_with('_bldgs')))

permit_data_cleaned_spatial_trailing_twelve_months <- permit_data_cleaned_spatial_recent %>%
  group_by(county_name, county_short_name, county_fips_code, state) %>%
  summarize(across(one_unit_units:five_plus_unit_units, ~sum(., na.rm = T))) %>%
  ungroup() %>%
  mutate(total_units = one_unit_units + two_unit_units + three_four_unit_units + five_plus_unit_units)

three_months_ago <- as.Date(max(permit_data_cleaned_spatial_historical$month)) %m-% months(3)

permit_data_cleaned_spatial_trailing_three_months <- permit_data_cleaned_spatial_recent %>%
  filter(month > three_months_ago) %>%
  group_by(county_name, county_short_name, county_fips_code, state) %>%
  summarize(across(one_unit_units:five_plus_unit_units, ~sum(., na.rm = T))) %>%
  ungroup() %>%
  mutate(total_units = one_unit_units + two_unit_units + three_four_unit_units + five_plus_unit_units) 

current_month <- as.Date(max(permit_data_cleaned_spatial_historical$month))

permit_data_cleaned_spatial_current_month <- permit_data_cleaned_spatial_recent %>%
  filter(month == current_month) %>%
  group_by(county_name, county_short_name, county_fips_code, state) %>%
  summarize(across(one_unit_units:five_plus_unit_units, ~sum(., na.rm = T))) %>%
  ungroup() %>%
  mutate(total_units = one_unit_units + two_unit_units + three_four_unit_units + five_plus_unit_units) 


permit_data_cleaned_spatial_trailing_twelve_months <- permit_data_cleaned_spatial_trailing_twelve_months %>%
  left_join(county_housing_stock, by = c('county_name', 'county_short_name', 'county_fips_code', 'state')) %>%
  mutate(tot_shr = (total_units / housing_units_2024)*100) %>%
  st_as_sf()

permit_data_cleaned_spatial_trailing_three_months <- permit_data_cleaned_spatial_trailing_three_months %>%
  left_join(county_housing_stock, by = c('county_name', 'county_short_name', 'county_fips_code', 'state')) %>%
  mutate(tot_shr = (total_units*4 / housing_units_2024)*100) %>%
  st_as_sf()

permit_data_cleaned_spatial_current_month <- permit_data_cleaned_spatial_current_month %>%
  left_join(county_housing_stock, by = c('county_name', 'county_short_name', 'county_fips_code', 'state')) %>%
  mutate(tot_shr = (total_units*12 / housing_units_2024)*100) %>%
  st_as_sf()


# Check to make sure there is an Active ArcGIS Installation
arc.check_product()

# Output the ACS county data to the path specified
arc.write(path = output_trailing_twelve_months_shp_file_path, data = permit_data_cleaned_spatial_trailing_twelve_months, overwrite = T, validate = T)
arc.write(path = output_trailing_three_months_shp_file_path, data = permit_data_cleaned_spatial_trailing_three_months, overwrite = T, validate = T)
arc.write(path = output_current_month_shp_file_path, data = permit_data_cleaned_spatial_current_month, overwrite = T, validate = T)
