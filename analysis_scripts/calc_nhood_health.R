library(tidyverse)
library(cwi)
library(RSocrata)

meta <- read_csv("_utils/cdc_indicators.txt")


weights <- list(
  "New Haven" = nhv_tracts,
  Hartford = hartford_tracts,
  Bridgeport = bridgeport_tracts,
  Stamford = stamford_tracts
) %>%
  bind_rows(.id = "city") %>%
  bind_rows(distinct(., city) %>% mutate(name = city, geoid = city, weight = 1)) %>%
  select(city, neighborhood = name, name = geoid, weight, town)

cities <- weights %>%
  distinct(city, town) %>%
  mutate(town = coalesce(town, city))


# 500 CITIES DATA FROM SOCRATA
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
         name = coalesce(name, city),
         year = as.character(year)) %>% 
  semi_join(meta, by = c("question" = "display"))

# get populations based on which years present in 500 cities
pops <- c(set_names(unique(cdc_df$year)), "2010-2015" = 2015) %>%
  map_dfr(~tidycensus::get_acs("tract", variables = "B01003_001", year = as.numeric(.), state = "09"), .id = "year") %>%
  select(year, name = GEOID, pop = estimate)


# pop doesn't matter for cities, so just fill in 1
cdc_avg <- cdc_df %>%
  left_join(weights, by = c("city", "name")) %>%
  left_join(pops, by = c("year", "name")) %>%
  filter(city %in% unique(weights$city)) %>% 
  replace_na(list(pop = 1)) %>%
  group_by(level, year, city, town, topic, question, neighborhood) %>%
  summarise(value = weighted.mean(value, w = weight * pop)) %>%
  ungroup() %>%
  rename(name = neighborhood)


# LIFE EXPECTANCY
# not totally necessary since data hasn't changed since last time, but it's easy enough to recalculate and good to have all in one place
life_read <- read_csv("https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NVSS/USALEEP/CSV/CT_A.CSV") %>%
  select(tract = 1, value = 5) %>%
  mutate(year = "2010-2015") %>%
  left_join(pops, by = c("tract" = "name", "year"))

life_city <- life_read %>%
  left_join(tract2town, by = "tract") %>%
  inner_join(cities, by = "town") %>%
  mutate(level = "town") %>%
  rename(name = town)

life_ct <- life_read %>% mutate(level = "state", name = "Connecticut")

life_nhood <- life_read %>%
  inner_join(weights, by = c("tract" = "name")) %>%
  mutate(level = "neighborhood") %>%
  rename(name = neighborhood)


life_df <- bind_rows(life_ct, life_city, life_nhood) %>%
  replace_na(list(weight = 1)) %>%
  group_by(level, year, city, town, 
           topic = "Life expectancy", question = "Life expectancy",
           name) %>%
  summarise(value = weighted.mean(value, w = weight * pop))

rm(life_read, life_city, life_ct, life_nhood)


health_avg <- bind_rows(
  life_df %>% mutate(value = round(value, 1)), 
  cdc_avg %>% mutate(value = round(value, 2))
) %>%
  ungroup() %>%
  mutate(topic = as_factor(topic) %>% 
           fct_relevel("Life expectancy") %>%
           fct_relabel(str_replace_all, "\\s", "_") %>%
           fct_relabel(tolower)) %>%
  arrange(topic, question, city, level) %>%
  filter(!is.na(value))

saveRDS(health_avg, "output_data/nhood_health_indicators_2019.rds")

health_avg %>%
  select(-topic) %>%
  pivot_wider(names_from = c(question, year)) %>%
  write_csv("output_data/nhood_health_indicators_wide_2019.csv")
