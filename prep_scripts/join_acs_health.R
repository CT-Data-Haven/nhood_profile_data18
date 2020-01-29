library(tidyverse)
library(cwi)

cdc_meta <- read_csv("_utils/cdc_indicators.txt")

acs <- bind_rows(
  readRDS(str_glue("input_data/acs_to_prep_for_viz_2018.rds")),
  readRDS(str_glue("input_data/nhv_acs_to_prep_for_viz_2018.rds")) %>% map(mutate, town = NA)
) %>%
  distinct(level, topic, display, name, .keep_all = TRUE) %>%
  mutate(level = fct_relabel(level, ~str_match(., "([a-z]*?)s?$")[, 2]),
         year = "2018",
         city = if_else(name == "Connecticut", NA_character_, city))

health <- readRDS("output_data/nhood_health_indicators_2019.rds") %>%
  left_join(cdc_meta %>% select(indicator, question = display, display = new_display), by = "question") %>%
  select(-question) %>%
  mutate(indicator = paste("m", indicator, sep = "X"),
         town = if_else(level == "neighborhood", coalesce(town, city), town))

prof_df <- bind_rows(acs, health) %>%
  mutate(indicator = str_replace_all(indicator, c("^estimate" = "t", "^share" = "m")) %>%
           str_replace_all("\\s", "X"))

prof_list <- prof_df %>%
  split(.$city) %>%
  set_names(~str_replace_all(., " ", "_") %>% tolower()) %>%
  map(function(df) {
    bind_rows(prof_df %>% filter(name == "Connecticut"), df) %>%
      fill(city, .direction = "downup")
  })

iwalk(prof_list, function(df, city) {
  df %>%
    select(-indicator, -topic, -city) %>%
    distinct(name, display, year, .keep_all = TRUE) %>%
    pivot_wider(names_from = c(display, year)) %>%
    write_csv(str_glue("to_distro/{city}_nhood_2018_acs_health_comb.csv"))
})
  
saveRDS(prof_list, "output_data/all_nhood_2018_acs_health_comb.rds")