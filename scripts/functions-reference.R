# functions-reference.R

#' @title Load census data and geometry from tidycensus
#' @description return US population and household income by country for the specified year
#' @return df with census data and geometry
#' @param which_states character vector of two-letter state abbreviations
#' @param yr integer year for which you want census data
#' @examples
#' TBD
get_census_data <- function(which_states, yr = 2000, which_source = "census") {
  ## census and geometry data from tidycensus
  ## which_source: "census" or "acs"
  ## in case you want to see what other data variables exist:
  # v10 <- load_variables(2010, "acs5", cache = TRUE)
  
  # test
  # which_states = "NC"
  # yr = 2010
  # which_source = "census"
  
  get_pop_inc_counties_of_state <- function(state, yr) {
    # input: one state, one year
    # output: df sf with pop, medincome, area, and geometry
    
    # TODO: do better error checking: not all combinations of yr an which_source are valid
    
    # test
    # yr <- 2000
    # state <- "NC"
    
    # in sf3:
    #   P001001 Total TOTAL POPULATION [1]
    #   P053001 Median household income in 1999 MEDIAN HOUSEHOLD INCOME IN 1999 (DOLLARS) [1]
    
    if (which_source == "census") {
      if (yr <= 2000) {
        # use get_decennial with sf3
        d_df <- get_decennial(geography = c("county"), 
                              variables = c(pop = "P001001", medincome = "P053001"),
                              # following are your variables if using get_acs() instead of get_decennial()
                              # variables = c(pop = "B01003_001", 
                              #               medincome = "B19013_001"), 
                              sumfile = "sf3", # need sf3 for median income but sf3 wasn't published after 2000
                              state = state,
                              year = yr,
                              geometry = TRUE,
                              output = "wide"
        ) 
      } else {
        # use get_decennial with sf1 and no medincome variable
        # need sf3 for median income but sf3 wasn't published after 2000
        d_df <- get_decennial(geography = c("county"), 
                              variables = c(pop = "P001001"),
                              sumfile = "sf1",
                              state = state,
                              year = yr,
                              geometry = TRUE,
                              output = "wide"
        ) %>%
          mutate(medincome = NA_real_)
      }
    } else if (which_source == "acs") {
      # use get_acs
      d_df <- get_acs(geography = c("county"), 
                      variables = c(pop = "B01003_001",
                                    medincome = "B19013_001"),
                      state = state,
                      year = yr,
                      geometry = TRUE,
                      output = "wide"
                      
      ) %>%
        # ACS creates estimates. We'll treat them as point values as the decennial census does.
        rename(pop = popE,
               medincome = medincomeE) %>%
        dplyr::select(-popM, -medincomeM)
    } else {
      stop("which_source must be 'census' or 'acs'")
    }
    
    d_df %>%
      mutate(state = state) %>% # include the two-letter state abbreviation in the df
      dplyr::select(state, everything()) %>%
      clean_names() %>%
      left_join(.,
                county_area, # area columns in m^2
                by = "geoid")
    
  }
  
  # county data from tigris::counties()
  county_data <- read_rds("./data/modified/us-counties.rds")
  county_area <- county_data %>%
    dplyr::select(geoid, aland, awater, atotal)
  
  census_info <- map2_df(which_states, yr, get_pop_inc_counties_of_state)
  census_info <- bind_cols(census_info,
                           calc_area = st_area(census_info)) %>%
    rename(county = name)  %>% # a better name than 'name'
    st_transform(crs = "WGS84")
  
  # simplify the geometry (remove vertices to reduce size of data frame and perhaps draw map faster)
  # but potential downside when calculating average light levels per county
  # to simplify set tol = something other than 0
  tol = 0
  if (tol != 0) {
    # following https://stackoverflow.com/questions/60008135/st-simplify-dtolerence-with-decimal-degree
    # to avoid error: "st_simplify does not correctly simplify longitude/latitude data, dTolerance needs to be in decimal degrees"
    census_info <- st_transform(census_info, crs = "ESRI:54032") # azimuthal equidistant
    census_info <- st_simplify(census_info, dTolerance = tol, preserveTopology = TRUE)
    census_info <- st_transform(census_info, crs = "WGS84")
  }
  
  census_info
  
}


#' @title Census and geometry data for state(s) in our focus area
#' @description A layer for plotting that you can visually highlight
#' @return df with census data and geometry
#' @param census_info df with census data and geometry
#' @param which_states character vector of two-letter state abbreviations
#' @param my_bbox bounding box named vector
#' @examples
#' TBD
census_focus <- function(census_info, which_states, my_bbox) {
  nc_census_info <- filter(census_info, state %in% which_states) %>%
    st_crop(my_bbox)
}

### NOT USED
#' #' @title Census and geometry data for state(s) NOT in our focus area
#' #' @description A layer for plotting that you can visually deemphasize
#' #' @return df with census data and geometry
#' #' @param census_info df with census data and geometry
#' #' @param which_states character vector of two-letter state abbreviations
#' #' @param my_bbox bounding box named vector
#' #' @examples
#' #' TBD
#' census_not_focus <- function(census_info, which_states, my_bbox) {
#'   not_nc_census_info <- filter(census_info, !state %in% which_states) %>%
#'     st_crop(my_bbox)
#' }


#' @title Geometry data for state boundaries
#' @description A layer for plotting
#' @return sf with state boundaries
#' @param census_not_focus_info df with census data and geometry
#' @examples
#' TBD
state_boundaries <- function(census_focus_info) {
  #not_nc_state_boundaries <- 
  census_focus_info %>%
    group_by(state) %>%
    summarize(boundary = st_union(geometry)) %>%
    ungroup()
}

