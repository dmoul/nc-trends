# constants2.R


my_caption <- "Source: US Census via tidycensus; plot: Daniel Moul"
my_caption_nhgis <- "Source: US Census via NHGIS/IPUMS; plot: Daniel Moul"

my_manual_colors <- c("firebrick", "dodgerblue", "darksalmon", "grey50")

my_proj <- "EPSG:6542" #6542 # NAD83(2011) / North Carolina            projected    6318 m
# the above determined after running suggest_crs(nc_county_boundaries)


###### define-regional-groupings
# approximated from "North Carolina beyond the connected age: the Tar Heel State in 2050" By Michael L. Walden

# urban_crescent <- c("Johnston", "Wake", "Durham", "Orange", "Alamance", "Chatham",
#                     "Guilford", "Randolf", "Forsyth", "Davidson", "Randolph", "Rowan", "Cabarrus", 
#                     "Mecklenburg", "Lincoln", "Gaston", "Union")

urban_crescent <- c("Johnston", "Wake", "Durham", "Orange", "Alamance", #"Chatham",
                    "Guilford", "Forsyth", "Davidson", "Rowan", "Cabarrus", #"Randolph", 
                    "Mecklenburg", "Gaston", "Union") #"Lincoln", 

# coastal <- c("Currituck", "Dare", "Hyde", "Beaufort", "Pamlico", "Craven", "Jones", "Carteret",
#              "Onslow", "Pender", "New Hanover", "Brunswick")

coastal <- c("Currituck", "Dare", "Carteret", #"Jones", "Pamlico", "Beaufort", "Hyde", "Craven", 
             "Onslow", "Pender", "New Hanover", "Brunswick")

mountain <- c("Ashe", "Alleghany", "Watauga", "Wilkes", "Avery", "Caldwell", "Alexander", 
              "Mitchell", "Yancey", "Madison", 
              "Haywood", "Buncombe", "McDowell", "Burke", 
              "Swain", "Graham", "Cherokee", "Clay", "Macon", "Jackson", "Transylvania", "Henderson"
)

manufacturing <- c("Polk", "Rutherford", "Cleveland", 
                   "Nash", "Edgecombe", "Martin", "Pitt", "Greene", "Wilson")


region_colors <- tribble(
  ~region,                              ~color, 
  "Agriculture",                        "mediumseagreen",
  "Coastal",                            "mediumblue",
  "Manufacturing",                      "firebrick",
  "Mountains",                          "peru",
  "Urban crescent",                     "lightskyblue",
  "NC",                                 "khaki"
)
