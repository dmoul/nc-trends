# functions2.R

###### State-level data

get_pop_state <- function(proj) {
  
  get_pop_state_2000 <-  function() {
    get_decennial(
      geography = c("state"),
      variables = c(pop = "P001001"),
      year = 2000,
      geometry = TRUE,
      output = "wide"
    ) |>
      clean_names() |>
      mutate(year = 2000) |>
      st_transform(crs = my_proj)
  }
  
  get_pop_state_2010 <-  function() {
    get_decennial(
      geography = c("state"),
      variables = c(pop = "P001001"),
      year = 2010,
      geometry = TRUE,
      output = "wide"
    ) |>
      clean_names() |>
      mutate(year = 2010) |>
      st_transform(crs = my_proj)
  }
  
  get_pop_state_2020 <-  function() {
    get_decennial(
      geography = c("state"),
      variables = c(pop = "P1_001N"), # only available from PL file as of Oct 2022
      year = 2020,
      geometry = TRUE,
      output = "wide"
    ) |>
      clean_names() |>
      mutate(year = 2020) |>
      st_transform(crs = my_proj)
  }
  
  bind_rows(
    get_pop_state_2000(),
    get_pop_state_2010(),
    get_pop_state_2020()
  ) |>
    rename(state = name)
  
}

# test
# xx <- get_pop_state(my_proj)


###### Tract-level data

get_pop_2000_tract <-  function(proj) {
  get_decennial(
    geography = c("tract"),
    variables = c(pop = "P001001"),
    state = "NC",
    year = 2000,
    geometry = TRUE,
    output = "wide"
  ) |>
    clean_names() |>
    separate(name, into = c("tract_name", "county", "state"), sep = ", ") |>
    mutate(tract_name = str_extract(tract_name, "\\d+(\\.\\d+)?"),
           county = str_extract(county, "^.+(?= County)")
    ) |>
    select(-c(state)) |>
    mutate(year = 2000) |>
    st_transform(crs = proj)
}

get_pop_2010_tract <-  function(proj) {
  get_decennial(
    geography = c("tract"),
    variables = c(pop = "P001001"),
    state = "NC",
    year = 2010,
    geometry = TRUE,
    output = "wide"
  ) |>
    clean_names() |>
    separate(name, into = c("tract_name", "county", "state"), sep = ", ") |>
    mutate(tract_name = str_extract(tract_name, "\\d+(\\.\\d+)?"),
           county = str_extract(county, "^.+(?= County)")
    ) |>
    select(-c(state)) |>
    mutate(year = 2010) |>
    st_transform(crs = proj)
}

get_pop_2020_tract <-  function(proj) {
  get_decennial(
    geography = c("tract"),
    variables = c(pop = "P1_001N"),
    #sumfile = "sf1",
    state = "NC",
    year = 2020,
    geometry = TRUE,
    output = "wide"
  ) |>
    clean_names() |>
    separate(name, into = c("tract_name", "county", "state"), sep = ", ") |>
    mutate(tract_name = str_extract(tract_name, "\\d+(\\.\\d+)?"),
           county = str_extract(county, "^.+(?= County)")
    ) |>
    select(-c(state)) |>
    mutate(year = 2020) |>
    st_transform(crs = proj)
}


###### Census block-level data

## 2000 ##

get_pop_2000_block <- function(proj) {

d_pop_2000_block_tmp <-  get_decennial(geography = c("block"),
                                       variables = c(pop = "P001001"),
                                       state = "NC",
                                       year = 2000,
                                       geometry = TRUE,
                                       output = "wide"
) |>
  clean_names() |>
  mutate(year = 2000) |>
  relocate(year, .after = name)

d_pop_2000_block_detail_tmp  <- d_pop_2000_block_tmp |>
  st_drop_geometry() |>
  separate(name, into = c("block_name", "block_group", "tract_name", "county", "state"), sep = ", ") |>
  mutate(block_name = str_extract(block_name, "\\d+"),
         block_group = str_extract(block_group, "\\d+"),
         tract_name = str_extract(tract_name, "\\d+(\\.\\d+)?"),
         county = str_extract(county, "^.+(?= County)")
  ) |>
  select(-c(state))

d_pop_2000_block <- inner_join(d_pop_2000_block_tmp |>
                                 select(-c(name)),
                               d_pop_2000_block_detail_tmp |>
                                 select(-c(year, pop)),
                               by = c("geoid")
) |>
  relocate(c(year, pop), .after = county) |>
  st_transform(crs = proj)

}


## 2010 ##

get_pop_2010_block <- function(proj) {
  

d_pop_2010_block_tmp <-  get_decennial(geography = c("block"),
                                       variables = c(pop = "P001001"),
                                       state = "NC",
                                       year = 2010,
                                       geometry = TRUE,
                                       output = "wide"
) |>
  clean_names() |>
  mutate(year = 2010) |>
  relocate(year, .after = name)

d_pop_2010_block_detail_tmp  <- d_pop_2010_block_tmp |>
  st_drop_geometry() |>
  separate(name, into = c("block_name", "block_group", "tract_name", "county", "state"), sep = ", ") |>
  mutate(block_name = str_extract(block_name, "\\d+"),
         block_group = str_extract(block_group, "\\d+"),
         tract_name = str_extract(tract_name, "\\d+(\\.\\d+)?"),
         county = str_extract(county, "^.+(?= County)")
  ) |>
  select(-c(state))

d_pop_2010_block <- inner_join(d_pop_2010_block_tmp |>
                                 select(-c(name)),
                               d_pop_2010_block_detail_tmp |>
                                 select(-c(year, pop)),
                               by = c("geoid")
) |>
  relocate(c(year, pop), .after = county) |>
  st_transform(crs = proj)

}

## 2020 ##

get_pop_2020_block <- function(proj) {

d_pop_2020_block_tmp <-  get_decennial(geography = c("block"),
                                       variables = c(pop = "P1_001N"),
                                       state = "NC",
                                       year = 2020,
                                       geometry = TRUE,
                                       output = "wide"
) |>
  clean_names() |>
  mutate(year = 2020) |>
  relocate(year, .after = name)

d_pop_2020_block_detail_tmp  <- d_pop_2020_block_tmp |>
  st_drop_geometry() |>
  separate(name, into = c("block_name", "block_group", "tract_name", "county", "state"), sep = ", ") |>
  mutate(block_name = str_extract(block_name, "\\d+"),
         block_group = str_extract(block_group, "\\d+"),
         tract_name = str_extract(tract_name, "\\d+(\\.\\d+)?"),
         county = str_extract(county, "^.+(?= County)")
  ) |>
  select(-c(state))

d_pop_2020_block <- inner_join(d_pop_2020_block_tmp |>
                                 select(-c(name)),
                               d_pop_2020_block_detail_tmp |>
                                 select(-c(year, pop)),
                               by = c("geoid")
) |>
  relocate(c(year, pop), .after = county) |>
  st_transform(crs = proj)

}

###### 

get_pop_2000_2020_interpolated_tract <- function(pop_2000_tract, pop_2010_tract, pop_2020_tract,
                                    pop_2010_block, pop_2020_block) {
  
  # create population-weighted interpolation that allows tract-level comparisons 2000-2010 and 2010-2020
  # OUTPUT: A dataset of class sf with the geometries and an ID column from `to` (the target shapes) 
  #           but with numeric attributes of `from` interpolated to those shapes.
  
  # test
  # pop_2000_tract <- tar_read(pop_2000_tract)
  # pop_2010_tract <- tar_read(pop_2010_tract)
  # pop_2020_tract <- tar_read(pop_2020_tract)
  # pop_2010_block <- tar_read(pop_2010_block)
  # pop_2020_block <- tar_read(pop_2020_block)

  
  ## 2000 - 2010
  
  pop_weights <- pop_2010_block
  
  pop_2000_using_2010_tract_boundaries <- interpolate_pw(
    from = pop_2000_tract |> 
      select(geoid, pop) |>
      filter(!st_is_empty(geometry)),
    to = pop_2010_tract |> 
      select(geoid, pop) |>
      filter(!st_is_empty(geometry)),
    to_id = "geoid",
    extensive = TRUE,
    weights = pop_weights,
    weight_column = "pop",
    crs = my_proj
  ) |>
    rename(value = pop) |>
    mutate(
      year = 2000,
      variable = "pop",
    ) 
  
  # test
  # plot(pop_2000_2010_interpolate_pw |> select(value))
  #
  # pop_2000_2010_interpolate_pw |>
  #   filter(value == 0) |>
  #   #filter(is.nan(value) | is.nan(value))
  #   select(value) |>
  #   plot()
    
  ## 2010-2020
  
  pop_weights <- pop_2020_block
  
  pop_2010_using_2020_tract_boundaries <- interpolate_pw(
    from = pop_2010_tract |> select(geoid, pop),
    to = pop_2020_tract |> select(geoid, pop),
    to_id = "geoid",
    extensive = TRUE,
    weights = pop_weights,
    weight_column = "pop",
    crs = my_proj
  ) |>
    rename(value = pop) |>
    mutate(
      year = 2010,
      variable = "pop",
    ) 
  
  # test
  # plot(pop_2010_2020_interpolate_pw)
  #
  # pop_2010_2020_interpolate_pw |>
  #   filter(value == 0) |>
  #   #filter(is.nan(value) | is.nan(value))
  #   select(value) |>
  #   plot()
  
  bind_rows(
    pop_2000_using_2010_tract_boundaries,
    pop_2010_using_2020_tract_boundaries
  )
  
}


######
plot_rank_population <- function(dta = d_pop_growth_u18_long,
                                 my_var = "pop",
                                 my_title = "",
                                 fun_caption = my_caption_nhgis
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
                 linewidth = 0.25, alpha = 0.5) +
    geom_segment(data = xx_path |> 
                   filter(!year_to == 1980) |>
                   inner_join(xx_path_thick,
                              by = "county"),
                 aes(x = year, xend = year_to, y = rank_pop, yend = rank_pop_to, color = positive_growth_to), 
                 linewidth = 1, alpha = 0.5) +
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
                 linewidth = 0.25, alpha = 0.5) +
    geom_segment(data = xx_path |>
                   filter(!year_to == 1980) |>
                   inner_join(xx_path_thick,
                              by = "county"),
                 aes(x = year, xend = year_to, y = value, yend = value_to, color = positive_growth_to),
                 linewidth = 1, alpha = 0.5) +
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
                 linewidth = 0.25, alpha = 0.5) +
    geom_segment(data = xx_path |> 
                   filter(!year_to == 1980) |>
                   inner_join(xx_path_thick,
                              by = "county"),
                 aes(x = year, xend = year_to, y = rank_growth, yend = rank_growth_to, color = positive_growth_to), 
                 linewidth = 1, alpha = 0.5) +
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
                 linewidth = 0.25, alpha = 0.5) +
    geom_segment(data = xx_path |>
                   filter(!year_to == 1980) |>
                   inner_join(xx_path_thick,
                              by = "county"),
                 aes(x = year, xend = year_to, y = pct_growth, yend = pct_growth_to, color = positive_growth_to),
                 linewidth = 1, alpha = 0.5) +
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
even_or_odd <- function(x) {
  # even = 1; odd = -1
  if_else(x / 2 == floor(x / 2), 1, -1)
}

######
get_tallest_mt_nc <- function(the_proj = NA) {
  # INPUT the CRS to use if not default of WGS84
  # OUTPUT: return sf object of top NC mountains in the projection the_proj
  #         with data from table on wiki page
  
  wiki_page <- "https://en.wikipedia.org/wiki/List_of_mountains_in_North_Carolina"
  
  nc_mt_wiki_page <- read_html(wiki_page)
  nc_mt_wiki_tbl_tmp <- html_elements(nc_mt_wiki_page, "table")[1] |>
    html_table()
  nc_mt_wiki_tbl <- nc_mt_wiki_tbl_tmp[[1]] |>
    clean_names() |>
    separate(location, into = c("dms", "decimal_ew", "decimal"), 
             extra = "merge", fill = "right",
             sep = " / ") |>
    separate(decimal, into = c("lat", "junk", "long", "name"),
             sep = "[^((\\w)?(\\d)?(\\.)?( )?)]+",
             convert = TRUE) |>
    mutate(name = str_extract(summit, "(\\w( )?)+"),
           elevation = str_replace_all(elevation, c("ft" = "ft ",
                                                    "," = "")),
           long = long * -1) |>
    select(rank, name, mountain_range, elevation, isolation, lat, long)
  
  nc_mt_tmp <- st_as_sf(nc_mt_wiki_tbl,
                       coords = c("long", "lat"),
                       crs = "WGS84")
  
  if(is.na(the_proj)) {
    nc_mt_tmp
    } else {
      st_transform(nc_mt_tmp,
                   crs = the_proj)
    }
  
}

######

get_pop_hisp <- function(the_proj) {
  
  d_hisp_2000_county <-  get_decennial(geography = c("county"), 
                                       variables = c(total = "P004001",
                                                     total_hisp = "P004002",
                                                     total_hisp_non = "P004003"),
                                       state = "NC",
                                       year = 2000,
                                       geometry = TRUE,
                                       output = "wide"
  ) |>
    clean_names() |>
    mutate(year = 2000) |>
    relocate(year, .after = name)
  
  d_hisp_2010_county <-  get_decennial(geography = c("county"), 
                                       variables = c(total = "P005001",
                                                     total_hisp_non = "P005002"),
                                       state = "NC",
                                       year = 2010,
                                       geometry = TRUE,
                                       output = "wide"
  ) |>
    clean_names() |>
    mutate(total_hisp = total - total_hisp_non,
           year = 2010) |>
    relocate(year, .after = name)
  
  d_hisp_2020_county <-  get_decennial(geography = c("county"), 
                                       variables = c(total = "P2_001N", 
                                                     total_hisp = "P2_002N",
                                                     total_hisp_non = "P2_003N"),
                                       state = "NC",
                                       year = 2020,
                                       geometry = TRUE,
                                       output = "wide"
  ) |>
    clean_names() |>
    mutate(year = 2020) |>
    relocate(year, .after = name)
  
  d_hisp_county <- bind_rows(
    d_hisp_2000_county,
    d_hisp_2010_county,
    d_hisp_2020_county
  ) |>
    mutate(county = str_extract(name, "^.+(?= County)"),
           pct_hisp = total_hisp / total,
           pct_hisp_non = total_hisp_non / total) |>
    arrange(geoid, year) |> # geoid is county
    group_by(geoid) |>
    mutate(n_change_total = total - lag(total),
           n_change_hisp = total_hisp - lag(total_hisp),
           n_change_hisp_non = total_hisp_non - lag(total_hisp_non),
           pct_change_total = total / lag(total) - 1,
           pct_change_hisp = total_hisp / lag(total_hisp) - 1,
           pct_change_hisp_non = total_hisp_non / lag(total_hisp_non) - 1,
           cagr_total = (total / lag(total))^(1 / (year - lag(year))) - 1,
           cagr_hisp = (total_hisp / lag(total_hisp))^(1 / (year - lag(year))) - 1,
           cagr_hisp_non = (total_hisp_non / lag(total_hisp_non))^(1 / (year - lag(year))) - 1
    ) |>
    ungroup() |>
    st_transform(crs = the_proj) 
  
}

######

get_pop_2000_2020_interpolated_tract <- function() {
  # no-op
  TRUE
}
