# _targets.R

library(targets)
# see https://books.ropensci.org/targets/literate-programming.html
library(tarchetypes)

source("./scripts/my-setup.R")
source("./scripts/constants2.R")
source("./scripts/functions.R")
source("./scripts/functions2.R")

list(
  # data from IPUMS NHGIS
  tar_target(nc_county_boundaries, get_nc_county_boundaries(my_proj)),
  tar_target(nc_tract_boundaries2019, get_nc_tract_boundaries2019(my_proj)),
  tar_target(nc_tract_boundaries2020, get_nc_tract_boundaries2020(my_proj)),
  tar_target(nc_city_boundaries, get_nc_city_boundaries(my_proj)),
  
  # tar_target(pop_decennial_total, get_pop_decennial),
  tar_target(pop_decennial, get_pop_decennial(nc_county_region_mapping)),
  tar_target(pop_acs, get_acs(my_proj)),
  tar_target(pop_race, get_race(my_proj, cpi)),
  tar_target(pop_income, get_pop_income_tract(nc_tract_boundaries2019, cpi)),
  tar_target(pop_income_standardized2010, get_pop_income_tract_standardized2010(nc_tract_boundaries2019, cpi)),
  
  # from other sources
  tar_target(nc_county_region_mapping, make_nc_county_region_mapping(nc_county_boundaries)),
  tar_target(cpi, get_cpi()),
  tar_target(nc_mt, get_tallest_mt_nc(my_proj), format = "qs"),
  
  # the following use tidycensus and (under the covers) tigris staring with exploration.13
  tar_target(pop_state, get_pop_state(my_proj), format = "qs"),
  tar_target(pop_2000_tract, get_pop_2000_tract(my_proj), format = "qs"),
  tar_target(pop_2010_tract, get_pop_2010_tract(my_proj), format = "qs"),
  tar_target(pop_2020_tract, get_pop_2020_tract(my_proj), format = "qs"),
  tar_target(pop_hisp, get_pop_hisp(my_proj), format = "qs"),
  ###tar_target(pop_2000_block, get_pop_2000_block(my_proj), format = "qs"),
  tar_target(pop_2010_block, get_pop_2010_block(my_proj), format = "qs"),
  tar_target(pop_2020_block, get_pop_2020_block(my_proj), format = "qs"),
  # TODO: fix the interpolated functions
  # NOP function for now:
  tar_target(pop_2000_2020_interpolated_tract, get_pop_2000_2020_interpolated_tract()), 
  # tar_target(pop_2000_2020_interpolated_tract, get_pop_2000_2020_interpolated_tract(pop_2000_tract,
  #                                                                                   pop_2010_tract,
  #                                                                                   pop_2020_tract,
  #                                                                                   pop_2010_block,
  #                                                                                   pop_2020_block), 
  #           format = "qs"),
  NULL # makes it possible to put a comma at end of every line and comment or delete them at will
)