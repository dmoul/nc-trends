# my-setup.R

knitr::opts_chunk$set(#echo = FALSE,
                      fig.retina = 3
                      #warning = TRUE,
                      #message = TRUE #,
                      #width = 81
                      )
options(scipen = 5)

library(targets)
library(janitor)
library(fs)
library(qs) # for storing tar_target() objects

library(tidyverse)
options(dplyr.summarise.inform = FALSE)
theme_set(theme_light())

library(geofacet)
library(ggridges)
library(ggrepel)
library(patchwork)
library(gt)

library(glue)
library(sf)
sf_use_s2(FALSE)

# library(raster)
# library(terra)
# library(ggthemes) # theme_map()
# library(ggridges)
# library(dagitty)
# library(ggdag)


library(cowplot) # for theme_map()

library(tidycensus) # also pulls in tigris
library(tigris)
options(tigris_use_cache = TRUE)

library(units)
library(scales)

library(rvest) # used in get_tallest_mt_nc() 

#set.seed(123) # uncomment when needed

# let's make some of the modeling stuff go faster 
# not bothering to load doParallel since we are making only this call
doParallel::registerDoParallel(cores = parallel::detectCores() - 1) 

# census_api_key("add your key here", install = TRUE)
