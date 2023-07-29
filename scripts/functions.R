# functions.R


######
even_or_odd <- function(x) {
  # even = 1; odd = -1
  if_else(x / 2 == floor(x / 2), 1, -1)
}

######
get_cpi <- function() {
  
  read_csv("./data/bls-cpi-all-urban-consumers-CUUR0000SA0.csv",
                     skip = 15) |>
    clean_names()
  
}


######
plot_rank_population <- function(dta = d_pop_growth_u18_long,
                                 my_var = "pop",
                                 my_title = "",
                                 fun_caption = my_caption
) {
  
  # test
  # dta = d_pop_growth_u18_long
  # my_var = "pop"
  # my_title = "Test with 'pop' variable"
  # fun_caption = my_caption
  
  xx <- dta |>
    filter(variable == {{ my_var }}) |>
    arrange(variable, county, year) |>
    group_by(year) |>
    mutate(rank_pop = 101 - rank(value, ties.method = "min")
           #rank_label = glue("{county} {sprintf(round(100 * pct_growth, digits = 1), fmt = '%.1f')}%")
    ) |>
    ungroup() |> 
    arrange(rank_pop) |>
    mutate(county = as_factor(county)) #sets factor levels to current order
  
  xx_path <- xx |>
    filter(year %in% c(1980, 1990, 2000, 2010, 2020)) |>
    select(year, county, value, rank_pop, positive_growth) |>
    arrange(county, year) |>
    mutate(year_to = lead(year),
           rank_pop_to = lead(rank_pop),
           value_to = lead(value),
           positive_growth_to = lead(positive_growth))
  
  xx_path_thick <- xx_path |>
    group_by(year) |>
    arrange(rank_pop) |>
    mutate(wide_line = (row_number() / 20 == floor(row_number() / 20))) |>
    ungroup() |>
    filter(wide_line,
           year == 2020) |>
    select(county)
  
  # with unit rank on y axis
  p1 <- xx |>
    filter(year %in% c(1980, 1990, 2000, 2010, 2020)) |>
    ggplot(aes(year, rank_pop)) +
    geom_segment(data = xx_path |> filter(!year_to == 1980),
                 aes(x = year, xend = year_to, y = rank_pop, yend = rank_pop_to, color = positive_growth_to), 
                 size = 0.25, alpha = 0.5) +
    geom_segment(data = xx_path |> 
                   filter(!year_to == 1980) |>
                   inner_join(xx_path_thick,
                              by = "county"),
                 aes(x = year, xend = year_to, y = rank_pop, yend = rank_pop_to, color = positive_growth_to), 
                 size = 1, alpha = 0.5) +
    geom_point(aes(size = value, color = positive_growth), alpha = 0.2) + #size = 3, 
    geom_text(data = xx |> filter(year == 1980),
              aes(year, rank_pop, label = county), 
              hjust = 1, nudge_x = -1,
              size = 2, alpha = 0.6, color = "black") +
    geom_text(data = xx |> filter(year == 2020),
              aes(year, rank_pop, label = county), 
              hjust = 0, nudge_x = 1,
              size = 2, alpha = 0.6, color = "black") +
    scale_x_continuous(breaks = c(1980, 1990, 2000, 2010, 2020),
                       position = "top") +
    scale_y_reverse(breaks = c(1, 25, 50, 75, 100)) +
    scale_size_continuous(range = c(0.5, 10), labels = label_number(scale_cut = cut_short_scale())) +
    scale_color_manual(values = my_manual_colors) +
    expand_limits(x = c(1970, 2035)) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "bottom") +
    guides(color =  "none") +
    labs(title = my_title,
         subtitle = "Fixed-space rank on y axis",
         x = NULL,
         y = NULL,
         size = "Population")
  
  # with pct_growth on y axis
  
  ylim_pop_max <- max(xx$value) - 0.02
  ylim_pop_min <- min(xx$value) + 0.02
  
  p2 <- xx |>
    filter(year %in% c(1980, 1990, 2000, 2010, 2020)) |>
    ggplot(aes(year, value)) +
    geom_segment(data = xx_path |> filter(!year_to == 1980),
                 aes(x = year, xend = year_to, y = value, yend = value_to, color = positive_growth_to),
                 size = 0.25, alpha = 0.5) +
    geom_segment(data = xx_path |>
                   filter(!year_to == 1980) |>
                   inner_join(xx_path_thick,
                              by = "county"),
                 aes(x = year, xend = year_to, y = value, yend = value_to, color = positive_growth_to),
                 size = 1, alpha = 0.5) +
    geom_point(aes(size = value, color = positive_growth),alpha = 0.2) +
    geom_text(data = xx |> filter(year == 1980),
              aes(year, value, label = county),
              hjust = 1, nudge_x = -1,
              size = 2, alpha = 0.6, color = "black",
              check_overlap = TRUE) +
    geom_text(data = xx |> filter(year == 2020),
              aes(year, value, label = county),
              hjust = 0, nudge_x = 1,
              size = 2, alpha = 0.6, color = "black",
              check_overlap = TRUE) +
    scale_x_continuous(breaks = c(1980, 1990, 2000, 2010, 2020),
                       position = "top") +
    scale_y_log10(labels = label_number(scale_cut = cut_short_scale())) + #breaks = -5:15 * 0.05, labels = percent_format()) +scale_size_continuous(range = c(0.5, 10)) +
    scale_color_manual(values = my_manual_colors) +
    expand_limits(x = c(1970, 2030)) +
    coord_cartesian(ylim = c(ylim_pop_min, ylim_pop_max)) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "none") +
    labs(title = " ",
         subtitle = "Same rank order with population on y axis",
         x = NULL,
         y = NULL,
         caption = paste0(fun_caption, 
                          "\nCounty at every 20th rank in 2020 has thicker line")
    )
  
  p1 + p2
}


######
plot_rank_growth <- function(dta = d_pop_growth_u18_long,
                      my_var = "pop",
                      my_title = "",
                      fun_caption = my_caption
                      ) {
  
  # test
  # dta = d_pop_growth_u18_long
  # my_var = "pop"
  # my_title = "Test with 'pop' variable"
  # fun_caption = my_caption
  
  xx <- dta |>
    filter(variable == {{ my_var }}) |>
    arrange(variable, county, year) |>
    group_by(year) |>
    mutate(rank_growth = 101 - rank(pct_growth, ties.method = "min"),
           rank_label = glue("{county} {sprintf(round(100 * pct_growth, digits = 1), fmt = '%.1f')}%")) |>
    ungroup()
  
  xx_path <- xx |>
    filter(year %in% c(1980, 1990, 2000, 2010, 2020)) |>
    select(year, county, pct_growth, rank_growth, rank_label, positive_growth) |>
    arrange(county, year) |>
    mutate(year_to = lead(year),
           rank_growth_to = lead(rank_growth),
           pct_growth_to = lead(pct_growth),
           positive_growth_to = lead(positive_growth))
  
  xx_path_thick <- xx_path |>
    group_by(year) |>
    arrange(rank_growth) |>
    mutate(wide_line = (row_number() / 20 == floor(row_number() / 20))) |>
    ungroup() |>
    filter(wide_line,
           year == 2020) |>
    select(county)
  
  # with unit rank on y axis
  p1 <- xx |>
    filter(year %in% c(1980, 1990, 2000, 2010, 2020)) |>
    ggplot(aes(year, rank_growth)) +
    geom_segment(data = xx_path |> filter(!year_to == 1980),
                 aes(x = year, xend = year_to, y = rank_growth, yend = rank_growth_to, color = positive_growth_to), 
                 size = 0.25, alpha = 0.5) +
    geom_segment(data = xx_path |> 
                   filter(!year_to == 1980) |>
                   inner_join(xx_path_thick,
                              by = "county"),
                 aes(x = year, xend = year_to, y = rank_growth, yend = rank_growth_to, color = positive_growth_to), 
                 size = 1, alpha = 0.5) +
    geom_point(aes(size = value, color = positive_growth), alpha = 0.2) + #size = 3, 
    geom_text(data = xx |> filter(year == 1980),
              aes(year, rank_growth, label = rank_label), 
              hjust = 1, nudge_x = -1,
              size = 2, alpha = 0.6, color = "black") +
    geom_text(data = xx |> filter(year == 2020),
              aes(year, rank_growth, label = rank_label), 
              hjust = 0, nudge_x = 1,
              size = 2, alpha = 0.6, color = "black") +
    scale_x_continuous(breaks = c(1980, 1990, 2000, 2010, 2020),
                       position = "top") +
    scale_y_reverse(breaks = c(1, 25, 50, 75, 100)) +
    scale_size_continuous(range = c(0.5, 10), labels = label_number(scale_cut = cut_short_scale())) +
    scale_color_manual(values = my_manual_colors) +
    expand_limits(x = c(1970, 2035)) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "bottom") +
    guides(color =  "none") +
    labs(title = my_title,
         subtitle = "Fixed-space rank on y axis",
         x = NULL,
         y = NULL,
         size = "Population")
  
  # with pct_growth on y axis
  
  ylim_growth_max <- max(xx$pct_growth) - 0.02
  ylim_growth_min <- min(xx$pct_growth) + 0.02
  
  p2 <- xx |>
    filter(year %in% c(1980, 1990, 2000, 2010, 2020)) |>
    ggplot(aes(year, pct_growth, )) +
    geom_segment(data = xx_path |> filter(!year_to == 1980),
                 aes(x = year, xend = year_to, y = pct_growth, yend = pct_growth_to, color = positive_growth_to),
                 size = 0.25, alpha = 0.5) +
    geom_segment(data = xx_path |>
                   filter(!year_to == 1980) |>
                   inner_join(xx_path_thick,
                              by = "county"),
                 aes(x = year, xend = year_to, y = pct_growth, yend = pct_growth_to, color = positive_growth_to),
                 size = 1, alpha = 0.5) +
    geom_point(aes(size = value, color = positive_growth),alpha = 0.2) + # size = 3, 
    geom_text(data = xx |> filter(year == 1980),
              aes(year, pct_growth, label = county), #rank_label),
              hjust = 1, nudge_x = -1,
              size = 2, alpha = 0.6, color = "black",
              check_overlap = TRUE) +
    geom_text(data = xx |> filter(year == 2020),
              aes(year, pct_growth, label = county), #rank_label),
              hjust = 0, nudge_x = 1,
              size = 2, alpha = 0.6, color = "black",
              check_overlap = TRUE) +
    scale_x_continuous(breaks = c(1980, 1990, 2000, 2010, 2020),
                       position = "top") +
    scale_y_continuous(breaks = seq(from = -1, to = 1, by = 0.05), labels = percent_format()) +
    scale_size_continuous(range = c(0.5, 10)) +
    scale_color_manual(values = my_manual_colors) +
    expand_limits(x = c(1970, 2030)) +
    coord_cartesian(ylim = c(ylim_growth_min, ylim_growth_max)) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "none") +
    labs(title = " ",
         subtitle = "Same rank order with growth rate on y axis",
         x = NULL,
         y = NULL,
         caption = paste0(fun_caption, 
                          "\nCounty at every 20th rank in 2020 has thicker line")
    )
  
  p1 + p2
}


######
get_nc_county_boundaries <- function(my_proj,
                                     my_geo_unit = "county") {
  
  # If using a different census year, need to check variable name and sumfile
  # Default ("county") borders haven't changed, so doesn't matter which census
  get_decennial(geography = my_geo_unit, 
                variables = c(pop = "P001001"),
                sumfile = "sf1",
                state = "NC",
                year = 2000,
                geometry = TRUE,
                output = "wide"
  ) |>
    clean_names() |>
    mutate(county = str_extract(name, "[A-z]+"),
           county = if_else(county == "New", "New Hanover", county)) |>
    select(-name, -pop) |>
    st_transform(crs = my_proj)
  
}

# test
# xtest <- get_nc_county_boundaries(my_proj)



######
get_nc_tract_boundaries2019 <- function(my_proj) {
  # ACS 2019 uses 2010 tracts
  
  sf::st_read(dsn = "./data/ipums/nhgis0015_shape/nhgis0015_shapefile_tl2019_us_tract_2019/") |>
  #sf::st_read(dsn = "./data/ipums/nhgis0016_shape/nhgis0016_shapefile_tl2020_us_tract_2020/") |>
    filter(STATEFP == "37") |> # NC only
    st_transform(crs = my_proj) |>
    st_simplify(dTolerance = 100, #100m
                preserveTopology = TRUE) |>
    clean_names() |>
    rename(tracta = tractce,
           name_tract = name,
           description_tract = namelsad) |>
    select(-c(mtfcc, funcstat, aland, awater, intptlat, intptlon, shape_leng)) |> #gisjoin, geoid, 
    st_transform(crs = my_proj) |>
    mutate(name_tract_core = str_extract(name_tract, "\\d+")) |>
    relocate(name_tract_core, .before = name_tract)
  
}



######
get_nc_tract_boundaries2020 <- function(my_proj) {
  
  #sf::st_read(dsn = "./data/ipums/nhgis0015_shape/nhgis0015_shapefile_tl2019_us_tract_2019/") |>
  sf::st_read(dsn = "./data/ipums/nhgis0016_shape/nhgis0016_shapefile_tl2020_us_tract_2020/") |>
    filter(STATEFP == "37") |> # NC only
    st_transform(crs = my_proj) |>
    st_simplify(dTolerance = 100, #100m
                preserveTopology = TRUE) |>
    clean_names() |>
    rename(tracta = tractce,
           name_tract = name,
           description_tract = namelsad) |>
    select(-c(mtfcc, funcstat, aland, awater, intptlat, intptlon, shape_leng)) |> #gisjoin, geoid, 
    st_transform(crs = my_proj) |>
    mutate(name_tract_core = str_extract(name_tract, "\\d+")) |>
    relocate(name_tract_core, .before = name_tract)
  
}


######
get_nc_city_boundaries <- function(my_proj) {
  
  st_read("./data/nc-onemap/NCDOT_City_Boundaries.geojson") |>
    st_transform(crs = my_proj) |>
    clean_names() |>
    mutate(across(starts_with("county"), str_to_title)) |>
    select(-c(ends_with(c("date", "upper_case", "participate")), intersect_feature_id, changed, change_year)) |>
    filter(population_estimate > 90000)
  
}

# test
# xtest <- get_nc_city_boundaries(my_proj)


######
make_nc_county_region_mapping <- function(nc_boundaries) {
  nc_boundaries |>
    st_drop_geometry() |> 
    distinct(county) |>
    mutate(region = case_when(
      county %in% urban_crescent  ~ "Urban crescent",
      county %in% coastal         ~ "Coastal",
      county %in% mountain        ~ "Mountains",
      county %in% manufacturing   ~ "Manufacturing",
      TRUE                        ~ "Agriculture"
    ),
    n_counties = case_when(
      county %in% urban_crescent  ~ length(urban_crescent),
      county %in% coastal         ~ length(coastal),
      county %in% mountain        ~ length(mountain),
      county %in% manufacturing   ~ length(manufacturing),
      TRUE                        ~ 100 - length(urban_crescent) - length(coastal) - length(mountain) - length(manufacturing)
    )
    )
}


######
get_pop_decennial <- function(county_region_mapping) {
  
  # Map the original variable names to new, consolidated names
  vmapping <- readxl::read_xlsx("./data/variable-mapping-ipums-11.xlsx",
                                skip = 14) |>
    select(-starts_with(".")) |>
    clean_names() |>
    mutate(new_var = str_to_lower(new_var),
           new_var = str_replace(new_var, "[-]", "_")) |>
    fill(new_var, .direction = "down") |>
    filter(!str_detect(old_var, "^#"),
           !is.na(old_var)
    ) |>
    separate(old_var, 
             into = c("variable", "variable_description"), 
             sep = ":      ",
             extra = "merge")
  
  vmap <- vmapping |>
    mutate(
      new_var = case_when(
        str_detect(new_var, "u5")        ~ paste0(new_var, "_preschool"),
        str_detect(new_var, "5_9")       ~ paste0(new_var, "_elementary school"),
        str_detect(new_var, "10_17")     ~ paste0(new_var, "_middle/high school"),
        str_detect(new_var, "18_21")     ~ paste0(new_var, "_college age"),
        str_detect(new_var, "22_34")     ~ paste0(new_var, "_young career"),
        str_detect(new_var, "35_64")     ~ paste0(new_var, "_main career"),
        str_detect(new_var, "65_74")     ~ paste0(new_var, "_early retirement"),
        str_detect(new_var, "75")        ~ paste0(new_var, "_late retirement"),
        TRUE                             ~ new_var
      ),
      var_id = case_when(
      str_detect(new_var, "u5")        ~ 1,
      str_detect(new_var, "5_9")       ~ 2,
      str_detect(new_var, "10_17")     ~ 3,
      str_detect(new_var, "18_21")     ~ 4,
      str_detect(new_var, "22_34")     ~ 5,
      str_detect(new_var, "35_64")     ~ 6,
      str_detect(new_var, "65_74")     ~ 7,
      str_detect(new_var, "75")        ~ 8,
      str_detect(new_var, "total")     ~ 10,
      str_detect(new_var, "t_")        ~ 9,
      TRUE                             ~ -999
    )) |>
    relocate(var_id, .before = "new_var") |>
    # relocate(year, .after = "new_var") |>
    arrange(var_id, variable)
  
  years_in_scope <- c("1970", "1980", "1990", "2000", "2010", "2020")
  
  dta_ts_tmp <- read_csv("./data/ipums/nhgis0011_csv/nhgis0011_ts_nominal_county.csv") |>
    filter(STATE == "North Carolina",
           YEAR %in% years_in_scope) |>
    select( -starts_with(c("GISJOIN", "SCSAA", "SMSAA", "REGIONA", "DIVISIONA", "COUNTYA", "AREANAME",
                           "CBSAA", "METDIVA", "CSAA", "MSA_CMSAA", "PMSAA", "ANPSADPI", "NECMAA", "NAME"))
    ) |>
    select( -c("STATE", "STATEFP", "STATENH", "COUNTYFP", "COUNTYNH")) |> 
    mutate(COUNTY = str_remove(COUNTY, " County"))
  
  normalize_varnames <- function(my_tbl, varname) {
    # assumes vmap exists in the parent env
    
    # test
    # my_tbl = dta_ts_tmp
    # varname = c("A3", "AR", "D0", "A0")
    
    my_tbl |>
      pivot_longer(cols = starts_with(varname), names_to = "variable", values_to = "value") |>
      left_join(vmap |> select(variable, var_id, new_var),
                by = "variable"
      ) |>
      select(year = YEAR, county = COUNTY, variable, value, var_id, new_var) |>
      arrange(county, year, var_id)
  }
  
  dta_ts <- normalize_varnames(dta_ts_tmp, c("A3", "AR", "D0", "A0")) |>
    filter(!is.na(new_var))
  
  new_var_levels = vmap |>
    distinct(var_id, new_var) |>
    arrange(var_id, new_var) |>
    #filter(!new_var %in% c("xtotal", "total")) |>
    pull(new_var)
  
  df_age <- dta_ts |>
    mutate(var_factor = factor(new_var, levels = new_var_levels)) |>
    left_join(county_region_mapping,
              by = "county")
}


######
get_acs <- function(my_region_mapping) {
  
  # test
  # my_region_mapping = tar_read(nc_county_region_mapping)
  
  # Map the original variable names to new, consolidated names
  vmapping <- readxl::read_xlsx("./data/variable-mapping-ipums-13.xlsx",
                                skip = 14) |>
    select(-starts_with(".")) |>
    clean_names() |>
    mutate(new_var = str_to_lower(new_var),
           new_var = str_replace(new_var, "[-]", "_")) |>
    fill(new_var, .direction = "down") |>
    filter(!str_detect(old_var_2010, "^#"),
           !is.na(old_var_2010)
    )
  
  vmap_2010 <- vmapping |>
    select(new_var, ends_with("_2010")) |>
    mutate(year = 2010) |>
    rename_with(~str_remove(., "_\\d{4}"), ends_with("_2010"))
  
  vmap_2020 <- vmapping |>
    select(new_var, ends_with("_2020")) |>
    mutate(year = 2020) |>
    rename_with(~str_remove(., "_\\d{4}"), ends_with("_2020"))
  
  vmap <- bind_rows(vmap_2010, 
                    vmap_2020
  ) |>
    filter(!str_detect(new_var, "^#"),
           !is.na(old_var)) |>
    separate(old_var, 
             into = c("variable", "variable_description"), 
             sep = ":[ ]+",
             extra = "merge") |>
    mutate(new_var = str_replace(new_var, "[+]", "p"),
           new_var = case_when(
             str_detect(new_var, "[mf]_u5")        ~ paste0(new_var, "_preschool"),
             str_detect(new_var, "[mf]_5_9")       ~ paste0(new_var, "_elementary school"),
             str_detect(new_var, "[mf]_10_17")     ~ paste0(new_var, "_middle/high school"),
             str_detect(new_var, "[mf]_18_21")     ~ paste0(new_var, "_college age"),
             str_detect(new_var, "[mf]_22_34")     ~ paste0(new_var, "_young career"),
             str_detect(new_var, "[mf]_35_64")     ~ paste0(new_var, "_main career"),
             str_detect(new_var, "[mf]_65_74")     ~ paste0(new_var, "_early retirement"),
             str_detect(new_var, "[mf]_75")        ~ paste0(new_var, "_late retirement"),
             TRUE                             ~ new_var
           )) |>
    # TODO figure out why this case_when()is creating this warning:
    # Warning: Expected 2 pieces. Additional pieces discarded in 184 rows [3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, ...].
    mutate(var_id = case_when(
      str_detect(new_var, "[mf]_u5")        ~ 1,
      str_detect(new_var, "[mf]_5_9")       ~ 2,
      str_detect(new_var, "[mf]_10_17")     ~ 3,
      str_detect(new_var, "[mf]_18_21")     ~ 4,
      str_detect(new_var, "[mf]_22_34")     ~ 5,
      str_detect(new_var, "[mf]_35_64")     ~ 6,
      str_detect(new_var, "[mf]_65_74")     ~ 7,
      str_detect(new_var, "[mf]_75")        ~ 8,
      str_detect(new_var, "[mf]_total")     ~ 9,
      str_detect(new_var, "t_total")        ~ 10,
      #str_detect(new_var, "^(i_)|(_i_)")  ~ 12, # income
      str_detect(new_var, "^[i_]|moe_i_")  ~ 11, # income
      str_detect(new_var, "hi_total")       ~ 20,
      str_detect(new_var, "hi_lt_10")       ~ 21,
      str_detect(new_var, "hi_10_15")       ~ 22,
      str_detect(new_var, "hi_15_20")       ~ 23,
      str_detect(new_var, "hi_20_25")       ~ 24,
      str_detect(new_var, "hi_25_30")       ~ 25,
      str_detect(new_var, "hi_30_35")       ~ 26,
      str_detect(new_var, "hi_35_40")       ~ 27,
      str_detect(new_var, "hi_40_45")       ~ 28,
      str_detect(new_var, "hi_45_50")       ~ 29,
      str_detect(new_var, "hi_50_60")       ~ 30,
      str_detect(new_var, "hi_60_75")       ~ 31,
      str_detect(new_var, "hi_75_100")      ~ 32,
      str_detect(new_var, "hi_100_125")     ~ 33,
      str_detect(new_var, "hi_125_150")     ~ 34,
      str_detect(new_var, "hi_150_200")     ~ 35,
      str_detect(new_var, "hi_200p")        ~ 36,
      TRUE                                  ~ -999
    )) |>
    relocate(var_id, .before = "new_var") |>
    relocate(year, .after = "new_var") |>
    mutate(sex = case_when(
      str_detect(new_var, "^(f_)|(_f_)")       ~ "female", 
      str_detect(new_var, "^(m_)|(_m_)")       ~ "male",
      #str_detect(new_var, "t_total")           ~ "both",
      TRUE                                     ~ "both"
    ),
    is_moe = str_detect(new_var, "^moe_"),
    new_var = str_replace(new_var, "[fm]_", "a")
    ) |>
    arrange(var_id, year, variable)
  
  # TODO: fix MOE where multiple variables are combined (once the data is transformed--not in the mapping df)
  #         pick max(moe) for each new_var. This isn't perfect, but good enough for my purposed
  
  
  years_in_scope <- c("2006-2010", "2016-2020")
  
  dta_acs2010_tmp <- read_csv("./data/ipums/nhgis0013_csv/nhgis0013_ds176_20105_county.csv") |>
    filter(STATE == "North Carolina",
           YEAR %in% years_in_scope) |>
    select(-c(COUNTYA:NAME_E)) |>
    select( -starts_with(c("GISJOIN", "SCSAA", "SMSAA", "REGIONA", "DIVISIONA", "COUNTYA", "AREANAME",
                           "CBSAA", "METDIVA", "CSAA", "MSA_CMSAA", "PMSAA", "ANPSADPI", "NECMAA", "NAME",
                           "STATE", "STATEFP", "STATENH", "COUNTYFP", "COUNTYNH"))
    ) |>
    mutate(COUNTY = str_remove(COUNTY, " County"))
  
  dta_acs2020_tmp <- read_csv("./data/ipums/nhgis0013_csv/nhgis0013_ds249_20205_county.csv") |>
    filter(STATE == "North Carolina",
           YEAR %in% years_in_scope) |>
    select(-c(COUNTYA:NAME_E)) |>
    select( -starts_with(c("GISJOIN", "SCSAA", "SMSAA", "REGIONA", "DIVISIONA", "COUNTYA", "AREANAME",
                           "CBSAA", "METDIVA", "CSAA", "MSA_CMSAA", "PMSAA", "ANPSADPI", "NECMAA", "NAME",
                           "STATE", "STATEFP", "STATENH", "COUNTYFP", "COUNTYNH", "STUSAB"))
    ) |>
    mutate(COUNTY = str_remove(COUNTY, " County"))
  
  ###
  normalize_varnames <- function(my_tbl, varname) {
    # assumes vmap exists in the parent env
    
    # test
    # my_tbl = dta_acs2020_tmp
    # varname = c("AMPK", "AMPV", "AMR7", "AMR8", "AMTC")
    
    my_tbl |>
      pivot_longer(cols = starts_with(varname), names_to = "variable", values_to = "value") |>
      left_join(vmap |> select(variable, var_id, new_var, sex, is_moe),
                by = "variable"
      ) |>
      select(year = YEAR, county = COUNTY, variable, value, var_id, new_var, sex, is_moe) |>
      arrange(county, year, var_id)
  }
  
  dta_acs2010 <- normalize_varnames(dta_acs2010_tmp, c("JLZ", "JOH", "JOI", "JQB", "JLZ")) |>
    filter(!is.na(new_var))
  
  dta_acs2020 <- normalize_varnames(dta_acs2020_tmp, c("AMPK", "AMPV", "AMR7", "AMR8", "AMTC")) |>
    filter(!is.na(new_var))
  
  new_var_levels = vmap |>
    distinct(var_id, new_var) |>
    arrange(var_id, new_var) |>
    #filter(!new_var %in% c("xtotal", "total")) |>
    pull(new_var)
  
  # debug:
  # getting error msg: "Last error: `x` and `y` must share the same src."
  #
  # this is a little better: error msg changed to "Last error: Join columns must be present in data."
  # my_region_mapping <- tibble(my_region_mapping) # a workaround for join below
  #
  # this works (but I don't want to be loading a tar object directly, since the dependency will be hidden):
  # my_region_mapping <- tar_read(nc_county_region_mapping) 
  
  df_acs <- bind_rows(dta_acs2010,
                      dta_acs2020) |>
    mutate(var_factor = factor(new_var, levels = new_var_levels),
           year = factor(year, levels = c("2006-2010", "2016-2020"))
    ) #|>
    # TODO: finish debugging this; for now, join region after loading tar object in main scripts
    # left_join(my_region_mapping,
    #           by = "county")
  
  df_acs
  
}

# test
# get_acs(tar_read(nc_county_region_mapping))


######
get_race <- function(my_region_mapping, cpi) {
  
  # test
  # my_region_mapping = tar_read(nc_county_region_mapping)
  # df_cpi = tar_read(cpi)
  
  df_cpi = cpi
  
  # Map the original variable names to new, consolidated names
  vmapping <- readxl::read_xlsx("./data/variable-mapping-ipums-14.xlsx",
                                skip = 14) |>
    select(-starts_with(".")) |>
    clean_names() |>
    mutate(new_var = str_to_lower(new_var),
           new_var = str_replace(new_var, "[-]", "_")) |>
    fill(new_var, .direction = "down") |>
    filter(!str_detect(new_var, "^#"),
           !is.na(old_var)
    )
  
  vmap <- vmapping |>
    separate(old_var, 
             into = c("variable", "variable_description"), 
             sep = ":[ ]+",
             extra = "merge") |>
    mutate(var_id = case_when(
      str_detect(new_var, "white")               ~ 1,
      str_detect(new_var, "black")               ~ 2,
      str_detect(new_var, "native")              ~ 3,
      str_detect(new_var, "asian")               ~ 4,
      str_detect(new_var, "two_or_more")         ~ 5,
      str_detect(new_var, "^i_med_fam_12mo")     ~ 6,
      str_detect(new_var, "^i_pci_12mo")         ~ 7,
      str_detect(new_var, "moe_i_med_fam_12mo")  ~ 6,
      str_detect(new_var, "moe_i_pci_12mo")      ~ 7,
      TRUE                                       ~ -999
    )) |>
    relocate(var_id, .before = "new_var") |>
    #relocate(year, .after = "new_var") |>
    arrange(var_id, variable)
  
  
  
  years_in_scope <- c("2006-2010", "2016-2020")
  
  dta_ts_tmp <- read_csv("./data/ipums/nhgis0014_csv/nhgis0014_ts_nominal_county.csv") |>
    filter(STATE == "North Carolina",
           #YEAR %in% years_in_scope
           ) |>
    #select(-c(COUNTYA:NAME_E)) |>
    select( -starts_with(c("GISJOIN", "SCSAA", "SMSAA", "REGIONA", "DIVISIONA", "COUNTYA", "AREANAME",
                           "CBSAA", "METDIVA", "CSAA", "MSA_CMSAA", "PMSAA", "ANPSADPI", "NECMAA", "NAME",
                           "STATE", "STATEFP", "STATENH", "COUNTYFP", "COUNTYNH"))
    ) |>
    mutate(COUNTY = str_remove(COUNTY, " County"))
  
  
  ###
  normalize_varnames <- function(my_tbl, varname) {
    # assumes vmap exists in the parent env
    
    # test
    # my_tbl = dta_acs2020_tmp
    # varname = c("AMPK", "AMPV", "AMR7", "AMR8", "AMTC")
    
    my_tbl |>
      pivot_longer(cols = starts_with(varname), names_to = "variable", values_to = "value") |>
      left_join(vmap |> select(variable, var_id, new_var),
                by = "variable"
      ) |>
      select(year = YEAR, county = COUNTY, variable, value, var_id, new_var) |>
      arrange(county, year, var_id)
  }
  
  dta_ts <- normalize_varnames(dta_ts_tmp, c("B1", "AB", "BD")) |> # skipping "BS" Household income since categories are too limited 
    mutate(year = case_when(
      year == "2008-2012"    ~ "2012",
      year == "2015-2019"    ~ "2019",
      TRUE                   ~ year
    ),
    year = as.integer(year))
  
  new_var_levels = vmap |>
    distinct(var_id, new_var) |>
    arrange(var_id, new_var) |>
    #filter(!new_var %in% c("xtotal", "total")) |>
    pull(new_var)
  
  # debug:
  # getting error msg: "Last error: `x` and `y` must share the same src."
  #
  # this is a little better: error msg changed to "Last error: Join columns must be present in data."
  # my_region_mapping <- tibble(my_region_mapping) # a workaround for join below
  #
  # this works (but I don't want to be loading a tar object directly, since the dependency will be hidden):
  # my_region_mapping <- tar_read(nc_county_region_mapping) 
  
  year_to = max(dta_ts$year)
  
  xx <- dta_ts |>
    mutate(var_factor = factor(new_var, levels = new_var_levels)) |>
    # Adjust for inflation -- but only for variables holding dollar values
    group_by(county, year) |>
    mutate(value2 = value * df_cpi[[which(df_cpi[, "year"] == year_to), "annual"]] /
                              df_cpi[[which(df_cpi[, "year"] == year), "annual"]]
    ) |>
    ungroup() |>
    mutate(value_nominal = value,
           value = if_else(str_detect(new_var, "^i_"), value2, value)
    ) |>
    select(-value2)
  
  # TODO: finish CPI adjustment above
  
}

# test
# get_race(tar_read(nc_county_region_mapping))

# test
# df_cpi <- tar_read(cpi)
# year_from <- 1970
# year_to <- 2019
# df_cpi[[which(df_cpi[, "year"] == year_from), "annual"]]
# df_cpi[[which(df_cpi[, "year"] == year_to), "annual"]]
# adj_value = value at year_from * (cpi factor at year X / cpi factor at year X)

######
get_pop_income_tract <- function(nc_tract_boundaries2019, cpi) {
  
  # test
  # nc_tract_boundaries2019 = tar_read(nc_tract_boundaries2019)
  # my_region_mapping = tar_read(nc_county_region_mapping)
  # df_cpi = tar_read(cpi)
  
  df_cpi = cpi
  
  # Map the original variable names to new, consolidated names
  vmapping <- readxl::read_xlsx("./data/variable-mapping-ipums-15.xlsx",
                                skip = 14) |>
    select(-starts_with(".")) |>
    clean_names() |>
    mutate(new_var = str_to_lower(new_var),
           new_var = str_replace(new_var, "[-]", "_")) |>
    fill(new_var, .direction = "down") |>
    filter(!str_detect(new_var, "^#"),
           !is.na(old_var)
    )
  
  vmap <- vmapping |>
    separate(old_var, 
             into = c("variable", "variable_description"), 
             sep = ":[ ]+",
             extra = "merge") |>
    mutate(var_id = case_when(
      # str_detect(new_var, "white")               ~ 1,
      # str_detect(new_var, "black")               ~ 2,
      # str_detect(new_var, "native")              ~ 3,
      # str_detect(new_var, "asian")               ~ 4,
      # str_detect(new_var, "two_or_more")         ~ 5,
      str_detect(new_var, "^t_total")               ~ 1,
      str_detect(new_var, "^i_med_house_12mo")      ~ 2,
      str_detect(new_var, "^i_med_fam_12mo")        ~ 3,
      str_detect(new_var, "^i_pci_12mo")            ~ 4,
      str_detect(new_var, "moe_t_total")            ~ 1,
      str_detect(new_var, "moe_i_med_house_12mo")   ~ 2,
      str_detect(new_var, "moe_i_med_fam_12mo")     ~ 3,
      str_detect(new_var, "moe_i_pci_12mo")         ~ 4,
      TRUE                                          ~ -999
    )) |>
    relocate(var_id, .before = "new_var") |>
    #relocate(year, .after = "new_var") |>
    arrange(var_id, variable)
  
  
  
 # years_in_scope <- c("2006-2010", "2016-2020")
  
  dta_ts_tmp <- read_csv("./data/ipums/nhgis0015_csv/nhgis0015_ts_nominal_tract.csv") |>
    filter(STATEFP == "37" # North Carolina
           #YEAR %in% years_in_scope
    ) |>
    #select(-c(COUNTYA:NAME_E)) |>
    select( -starts_with(c("SCSAA", "SMSAA", "REGIONA", "DIVISIONA", "COUNTYA", "AREANAME",
                           "CBSAA", "METDIVA", "CSAA", "MSA_CMSAA", "PMSAA", "ANPSADPI", "NECMAA", "NAME",
                           "STATENH", "COUNTYNH")) # keep "STATEFP", "COUNTYFP", "STATE", "GISJOIN", 
    ) |>
    select(-"STATE") |>
    mutate(COUNTY = str_remove(COUNTY, " County"),
           GISJOIN = str_pad(GISJOIN, width = 14, side = "right", pad = "0")
           )
  
  
  ###
  normalize_varnames <- function(my_tbl, varname) {
    # assumes vmap exists in the parent env
    
    # test
    # my_tbl = dta_ts_tmp
    # varname =  c("AV0", "B79", "AB2", "BD5")
    
    my_tbl |>
      pivot_longer(cols = starts_with(varname), names_to = "variable", values_to = "value") |>
      left_join(vmap |> select(variable, var_id, new_var),
                by = "variable"
      ) |>
      clean_names() |>
      select(year, county, statefp, countyfp, tracta, variable, value, var_id, new_var, gisjoin) |> #year = YEAR, county = COUNTY
      arrange(county, tracta, year, var_id)
  }
  
  # ???
    
  dta_ts <- normalize_varnames(dta_ts_tmp, c("AV0", "B79", "AB2", "BD5")) |> # skipping "BS" Household income since categories are too limited 
    mutate(year = case_when(
      year == "2008-2012"    ~ "2012",
      year == "2015-2019"    ~ "2019",
      TRUE                   ~ year
    ),
    year = as.integer(year)) |>
    right_join(nc_tract_boundaries2019 |>
                st_drop_geometry() |>
                select(gisjoin, statefp, countyfp, tracta, name_tract_core),
              by = c("statefp", "countyfp", "tracta", "gisjoin")
    ) #|>
    # mutate(name_tract_core = str_extract(name_tract, "\\d+")) |>
    # relocate(c(name_tract, name_tract_core), .after = tracta)
  
  new_var_levels = vmap |>
    distinct(var_id, new_var) |>
    arrange(var_id) |>
    #filter(!new_var %in% c("xtotal", "total")) |>
    pull(new_var)
  
  # debug:
  # getting error msg: "Last error: `x` and `y` must share the same src."
  #
  # this is a little better: error msg changed to "Last error: Join columns must be present in data."
  # my_region_mapping <- tibble(my_region_mapping) # a workaround for join below
  #
  # this works (but I don't want to be loading a tar object directly, since the dependency will be hidden):
  # my_region_mapping <- tar_read(nc_county_region_mapping) 
  
  year_to = max(dta_ts$year)
  
  dta_ts |>
    mutate(var_factor = factor(new_var, levels = new_var_levels)) |>
    # Adjust for inflation -- but only for variables holding dollar values
    group_by(county, year) |>
    mutate(value2 = value * df_cpi[[which(df_cpi[, "year"] == year_to), "annual"]] /
             df_cpi[[which(df_cpi[, "year"] == year), "annual"]]
    ) |>
    ungroup() |>
    mutate(value_nominal = value,
           value = if_else(str_detect(new_var, "^i_"), value2, value)
    ) |>
    select(-value2)
  
}



######
get_pop_income_tract_standardized2010 <- function(nc_tract_boundaries2019, cpi) {
  
  # NOTE: only population in tracts is standardized (not income data)
  # test
  # nc_tract_boundaries2019 = tar_read(nc_tract_boundaries2019)
  # my_region_mapping = tar_read(nc_county_region_mapping)
  # df_cpi = tar_read(cpi)
  
  df_cpi = cpi
  
  # Map the original variable names to new, consolidated names
  vmapping <- readxl::read_xlsx("./data/variable-mapping-ipums-16.xlsx",
                                skip = 14) |>
    #select(-starts_with(".")) |>
    clean_names() |>
    filter(!str_detect(new_var, "^#"),
           !is.na(old_var)) |>
    mutate(new_var = str_to_lower(new_var),
           new_var = str_replace(new_var, "[-]", "_")) |>
    fill(new_var, .direction = "down")
  
  vmap <- vmapping |>
    separate(old_var, 
             into = c("variable", "variable_description"), 
             sep = ":[ ]+",
             extra = "merge") |>
    mutate(var_id = case_when(
      str_detect(new_var, "^t_total")               ~ 1,
      str_detect(new_var, "^i_med_house_12mo")      ~ 2,
      str_detect(new_var, "^i_med_fam_12mo")        ~ 3,
      str_detect(new_var, "^i_pci_12mo")            ~ 4,
      str_detect(new_var, "moe_t_total")            ~ 1,
      str_detect(new_var, "moe_i_med_house_12mo")   ~ 2,
      str_detect(new_var, "moe_i_med_fam_12mo")     ~ 3,
      str_detect(new_var, "moe_i_pci_12mo")         ~ 4,
      TRUE                                          ~ -999
    )) |>
    relocate(var_id, .before = "new_var") |>
    arrange(var_id, variable)
  
  #years_in_scope <- c("2006-2010", "2016-2020")
  
  dta1_tmp <- read_csv("./data/ipums/nhgis0016_csv/nhgis0016_ts_geog2010_tract.csv") |>
    filter(STATEA == "37" # North Carolina
           #YEAR %in% years_in_scope
    ) |>
    rename(YEAR = DATAYEAR) |>
    #select(-c(COUNTYA:NAME_E)) |>
    select( -starts_with(c("GISJOIN", "GEOGYEAR", "SCSAA", "SMSAA", "REGIONA", "DIVISIONA", "COUNTYA", "AREANAME",
                           "CBSAA", "METDIVA", "CSAA", "MSA_CMSAA", "PMSAA", "ANPSADPI", "NECMAA", "NAME",
                           "STATENH", "COUNTYNH")) # keep "STATEFP", "COUNTYFP", "STATE", "NHGISCODE", 
    ) |>
    select(-"STATE") |>
    mutate(COUNTY = str_remove(COUNTY, " County"),
           #GISJOIN = str_pad(GISJOIN, width = 14, side = "right", pad = "0")
           )
  
  dta2_tmp <- read_csv("./data/ipums/nhgis0016_csv/nhgis0016_ts_nominal_tract.csv") |>
    filter(STATEFP == "37" # North Carolina
           #YEAR %in% years_in_scope
    ) |>
    #select(-c(COUNTYA:NAME_E)) |>
    select( -starts_with(c("GISJOIN", "SCSAA", "SMSAA", "REGIONA", "DIVISIONA", "COUNTYA", "AREANAME", #"NHGISCODE", 
                           "CBSAA", "METDIVA", "CSAA", "MSA_CMSAA", "PMSAA", "ANPSADPI", "NECMAA", "NAME",
                           "STATENH", "COUNTYNH")) # keep "STATEFP", "COUNTYFP", "STATE", 
    ) |>
    select(-"STATE") |>
    mutate(COUNTY = str_remove(COUNTY, " County"),
           #GISJOIN = str_pad(GISJOIN, width = 14, side = "right", pad = "0")
           ) |>
    mutate(YEAR = case_when(
      YEAR == "2008-2012"    ~ "2012",
      YEAR == "2015-2019"    ~ "2019",
      TRUE                   ~ YEAR
    ),
    YEAR = as.integer(YEAR))
  
  
  ###
  normalize_varnames <- function(my_tbl, varname) {
    # assumes vmap exists in the parent env
    
    # test
    # my_tbl = dta_ts_tmp
    # varname =  c("AV0", "B79", "AB2", "BD5")
    
    my_tbl |>
      pivot_longer(cols = starts_with(varname), names_to = "variable", values_to = "value") |>
      left_join(vmap |> select(variable, var_id, new_var),
                by = "variable"
      ) |>
      clean_names() |>
      select(year, county, statefp, countyfp, tracta, variable, value, var_id, new_var) |> #year = YEAR, county = COUNTY
      arrange(county, tracta, year, var_id)
  }
  
  # dta_ts_anti <- anti_join(dta1_tmp,
  #                          dta2_tmp,
  #                          by = c("GISJOIN", "COUNTY", "TRACTA", "YEAR"))
  
  dta_ts <- left_join(dta1_tmp,
                      dta2_tmp,
                      by = c("COUNTY", "TRACTA", "YEAR") #"GISJOIN", 
  ) |>
    normalize_varnames(c("CL8", "AV0", "B79", "AB2", "BD5")) |> # skipping "BS" Household income since categories are too limited 
    group_by(county) |>
    fill(c(statefp, countyfp), .direction = "downup") |>
    ungroup() |>
    # TODO: Decide whether to keep the following
    filter(str_detect(new_var, "t_total")) |>
    mutate(value = replace_na(value, 0)) |>
    right_join(nc_tract_boundaries2019 |>
                 st_drop_geometry() |>
                 select(statefp, countyfp, tracta, name_tract_core), #, gisjoin
               by = c("statefp", "countyfp", "tracta") #
    ) #|>
  # mutate(name_tract_core = str_extract(name_tract, "\\d+")) |>
  # relocate(c(name_tract, name_tract_core), .after = tracta)
    
  new_var_levels <- vmap |>
    distinct(var_id, new_var) |>
    arrange(var_id) |>
    #filter(!new_var %in% c("xtotal", "total")) |>
    pull(new_var)
  
  # debug:
  # getting error msg: "Last error: `x` and `y` must share the same src."
  #
  # this is a little better: error msg changed to "Last error: Join columns must be present in data."
  # my_region_mapping <- tibble(my_region_mapping) # a workaround for join below
  #
  # this works (but I don't want to be loading a tar object directly, since the dependency will be hidden):
  # my_region_mapping <- tar_read(nc_county_region_mapping) 
  
  # year_to = max(dta_ts$year)
  
  dta_ts |>
    mutate(var_factor = factor(new_var, levels = new_var_levels)) #|>
    # Adjust for inflation -- but only for variables holding dollar values
    # group_by(county, year) |>
    # mutate(value2 = value * df_cpi[[which(df_cpi[, "year"] == year_to), "annual"]] /
    #          df_cpi[[which(df_cpi[, "year"] == year), "annual"]]
    # ) |>
    # ungroup() |>
    # mutate(value_nominal = value,
    #        value = if_else(str_detect(new_var, "^i_"), value2, value)
    # ) |>
    # select(-value2)
  
}

