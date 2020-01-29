library(tidyverse)
library(jsonlite)
library(geojsonio)
library(sf)
library(cwi)
library(rmapshaper)

acs_year <- 2018

col_n_distinct <- function(.data, col) {
  n <- .data %>%
    pull({{ col }}) %>%
    n_distinct(na.rm = TRUE)
  if (n == 1) {
    .data %>% select(-{{ col }})
  } else {
    .data
  }
}

prof_list <- readRDS(str_glue("output_data/all_nhood_{acs_year}_acs_health_comb.rds")) %>%
  map(mutate_at, vars(topic, level, indicator), as_factor)

lvls <- bind_rows(prof_list) %>%
  distinct(level) %>%
  mutate(l = paste(row_number(), level, sep = "_")) %>%
  select(l, level) %>%
  deframe() %>%
  as.list()

# nested json, all cities
prof_wide <- prof_list %>%
  map(select, -display, -city, -year) %>%
  map(col_n_distinct, town) %>%
  map(rename, location = name) %>%
  map(mutate, level = fct_recode(level, !!!lvls)) %>%
  map(~split(., .$topic)) %>%
  map_depth(2, pivot_wider, names_from = indicator) %>%
  map_depth(2, select, -topic) %>%
  .[sort(names(.))]


# meta
# topics:
#   income:
#     display: Income
#     indicators: [povertyXshare, low_incomeXshare]
# indicators:
#   povertyXcount:
#     display: Population in poverty
#     format: ','

meta <- bind_rows(prof_list) %>%
  mutate(type = str_extract(indicator, "^[a-z]+(?=X)")) %>%
  rename(location = name) %>%
  distinct(topic, indicator, type, display) %>%
  mutate(format = if_else(topic == "life_expectancy", ".1f", recode(type, "t" = ",", "m" = ".0%"))) %>%
  mutate(displayTopic = topic %>%
           fct_relabel(camiller::clean_titles) %>%
           fct_relabel(str_replace, "(?<=^Income) ", " by age: ") %>%
           fct_recode("Race and ethnicity" = "Race")) %>%
  split(.$topic) %>%
  map(function(tdf) {
    idf <- tdf %>%
      select(-topic, -displayTopic)
      
    list(
      display = as.character(unique(tdf$displayTopic)),
      indicators = idf
    )
  })


write_json(prof_wide, str_glue("to_viz/nhood_wide_{acs_year}.json"), auto_unbox = TRUE)
write_json(meta, str_glue("to_viz/nhood_meta_{acs_year}.json"), auto_unbox = TRUE)

shps <- list.files("input_data/shapes", full.names = TRUE) %>%
  set_names(str_extract, "\\w+(?=_topo)") %>%
  map(topojson_read) %>%
  # map(st_as_sf) %>%
  map(select, name, town, geometry) %>%
  map(st_set_crs, 4326) %>%
  map(st_cast, "MULTIPOLYGON")

shps %>%
  iwalk(~topojson_write(.x, object_name = "city", file = str_glue("to_viz/cities/{.y}_topo.json")))
# sucks, but for now I'm just importing all topo files & making one object


# topos <- list.files("to_viz/cities", full.names = TRUE) %>%
#   set_names(str_extract, "\\w+(?=_topo)") %>%
#   map(topojson_read) %>%
#   map(select, -id)


topos <- shps %>%
  map(topojson_list, object_name = "city")

write_json(topos, "to_viz/cities_topo_list.json", auto_unbox = TRUE)



# meta on sources, urls
sources <- read_delim("_utils/sources.txt", delim = ";")
dwurls <- read_csv("_utils/dataworld_urls.csv") %>%
  deframe() %>%
  as.list()

lst(sources, dwurls) %>%
  jsonlite::write_json("to_viz/sources_meta.json", auto_unbox = TRUE)
