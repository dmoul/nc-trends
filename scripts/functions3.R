# function3.R

# import from IMPUMS NHGIS

fname <- "./data/ipums/nhgis0017_csv/nhgis0017_ts_geog2010_county_codebook.txt"

df_codebook <- tibble(
  V1 = read_lines(fname,
                  skip = 52)
) |>
  mutate(data_source = if_else(str_detect(V1, "^[A-Z]"), 
                               V1, 
                               NA_character_),
         V1 = if_else(!str_detect(V1, "^[A-Z]"), 
                      str_squish(V1), 
                      NA_character_)
  ) |>
  fill(data_source, .direction = "down") |>
  filter(str_detect(V1, "[A-Z]")) |>
  separate(V1, into = c("variable", "description"), 
           sep = ": ", 
           extra = "merge", 
           fill = "right"
  ) |>
  mutate(description = if_else(str_detect(description, "^Persons"),
                               paste0("estimate: ", description),
                               description
  )
  ) |>
  separate(description, into = c("type1", "type2", "description"), 
           sep = ":", 
           extra = "merge", 
           fill = "right"
  ) |>
  separate(description, into = c("race", "sex", "agegroup"), 
           sep = "~", 
           extra = "merge", 
           fill = "right"
  ) |>
  mutate(var_type = if_else(is.na(type2),
                            type1,
                            type2),
         estimate_type = case_when(
           str_detect(type1, "estimate")   ~ "estimate",
           str_detect(type1, "Lower")      ~ "lower bound",
           str_detect(type1, "Upper")      ~ "upper bound",
           TRUE                            ~ NA_character_
         )
  ) |>
  select(-c(type1, type2))



