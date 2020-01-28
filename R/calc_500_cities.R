library(tidyverse)
library(cwi)
library(RSocrata)

meta <- read_csv("input_data/cdc_indicators.txt")



cdc_query <- list(
  stateabbr = "CT",
  data_value_type = "Crude prevalence",
  "$select" = "cityname,tractfips,year,category,short_question_text,data_value,geographiclevel"
) %>%
  imap(~paste(.y, .x, sep = "=")) %>%
  str_flatten(collapse = "&")
cdc_url <- "https://chronicdata.cdc.gov/resource/6vp6-wxuq.json"

cdc_df <- read.socrata(paste(cdc_url, cdc_query, sep = "?"), Sys.getenv("SOCRATA_KEY")) %>%
  as_tibble() %>%
  select(city = cityname, year, name = tractfips, level = geographiclevel, question = short_question_text, value = data_value, topic = category) %>%
  mutate_at(vars(year, value), as.numeric) %>%
  mutate_at(vars(topic, question), camiller::clean_titles) %>%
  mutate(value = value / 100,
         level = fct_recode(as_factor(level), "neighborhood" = "Census Tract", "town" = "City"),
         name = coalesce(name, city)) %>% 
  semi_join(meta, by = c("question" = "display"))

# get populations based on which years present in 500 cities
pops <- unique(cdc_df$year) %>%
  set_names() %>%
  map_dfr(~tidycensus::get_acs("tract", variables = "B01003_001", year = ., state = "09"), .id = "year") %>%
  select(year, name = GEOID, pop = estimate) %>%
  mutate(year = as.numeric(year))


weights <- list(
  "New Haven" = nhv_tracts,
  Hartford = hartford_tracts,
  Bridgeport = bridgeport_tracts,
  Stamford = stamford_tracts
) %>%
  bind_rows(.id = "city") %>%
  bind_rows(distinct(., city) %>% mutate(name = city, geoid = city, weight = 1)) %>%
  select(city, neighborhood = name, name = geoid, weight, town)

# pop doesn't matter for cities, so just fill in 1
cdc_avg <- cdc_df %>%
  left_join(weights, by = c("city", "name")) %>%
  left_join(pops, by = c("year", "name")) %>%
  filter(city %in% unique(weights$city)) %>% 
  replace_na(list(pop = 1)) %>%
  group_by(level, year, city, town, topic, question, neighborhood) %>%
  summarise(value = weighted.mean(value, w = weight * pop)) %>%
  ungroup() %>%
  arrange(topic, question, city, level) %>%
  filter(!is.na(value))

write_csv(cdc_avg, "output_data/500_cities_neighborhood_2019.csv")
