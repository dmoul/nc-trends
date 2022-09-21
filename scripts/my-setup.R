# my-setup.R

knitr::opts_chunk$set(echo = TRUE,
                      fig.retina = 3,
                      warning = TRUE,
                      message = TRUE,
                      width = 81)
options(tigris_use_cache = TRUE)

library(targets)

library(tidyverse)
library(glue)
library(sf)
# library(raster)
# library(terra)
# library(ggthemes) # theme_map()
# library(ggridges)
# library(dagitty)
# library(ggdag)
# library(patchwork)

library(tidycensus) # also pulls in tigris
library(units)
library(scales)

# library(rstanarm)
# library(bayesplot)
# library(latex2exp)

#library(hrbrthemes)
# remotes::install_github("hrbrmstr/hrbrthemes")
# run 2021-08-01 to address segfault errors like this:
# sh: line 1: 11020 Segmentation fault: 11  '/Library/Frameworks/R.framework/Versions/4.1/Resources/library/Rttf2pt1/exec//ttf2pt1' -a -GfAe '/Library/Frameworks/R.framework/Versions/4.1/Resources/library/hrbrthemes/fonts/roboto-condensed/RobotoCondensed-Bold.ttf' '/var/folders/0j/hjw8sflx19l6rw4s1l68qq640000gn/T//RtmpGN3PSJ/fonts/RobotoCondensed-Bold' 2>&1
# theme_set(theme_ipsum_ps() + 
#             theme(panel.grid.minor = element_blank()))
# import_plex_sans()

# library(tidymodels)
# # remotes::install_github("tidymodels/multilevelmod")
# # on 2021-07-16 also installed Rcpp    (1.0.6 -> 1.0.7       ) [CRAN]  
# # but not parsnip (0.1.6 -> f522f8f4d...) [GitHub]
# library("multilevelmod")
# library(splines)
# 
# library(gt)

theme_set(theme_light())

set.seed(123)

# let's make some of the modeling stuff go faster 
# not bothering to load doParallel since we are making only this call
doParallel::registerDoParallel(cores = parallel::detectCores() - 1) 

# # get locations of GeoTIFF files
# source("./R/source-data.R")

# # for vfold_cv()
# n_folds <- 10


# remainder is from https://github.com/hadley/mastering-shiny/blob/master/common.R

knitr::opts_chunk$set(width = 81)

options(width = 81,
        # Better rlang tracebacks
        rlang_trace_top_env = rlang::current_env())

# Errors ------------------------------------------------------------------

# Make error messages closer to base R
registerS3method("wrap", "error", envir = asNamespace("knitr"),
                 function(x, options) {
                   msg <- conditionMessage(x)
                   
                   call <- conditionCall(x)
                   if (is.null(call)) {
                     msg <- paste0("Error: ", msg)
                   } else {
                     msg <- paste0("Error in ", deparse(call)[[1]], ": ", msg)
                   }
                   
                   msg <- error_wrap(msg)
                   knitr:::msg_wrap(msg, "error", options)
                 }
)

error_wrap <- function(x, width = getOption("width")) {
  lines <- strsplit(x, "\n", fixed = TRUE)[[1]]
  paste(strwrap(lines, width = width), collapse = "\n")
}

