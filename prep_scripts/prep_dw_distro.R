library(tidyverse)
library(data.world)

dataset <- "camille86/neighborhoods18"
hdrs <- read_csv("_utils/indicator_headings.txt") %>%
  distinct(indicator, display) %>%
  mutate(indicator = str_replace(indicator, "^(estimate|share)\\s(.+)$", "\\2_\\1")) %>%
  bind_rows(read_csv("_utils/headings_extra.txt"))


# neighborhood profiles, individual files (?)
nhood_req <- list.files("to_distro", pattern = "nhood_2018.+\\.csv$") %>%
  map(function(path) {
    city <- str_extract(path, "^\\w+(?=_nhood_2018)")
    desc <- paste("ACS basic indicators, CDC life expectancy estimates, 500 Cities Project averages,", 
                  camiller::clean_titles(city, cap_all = TRUE))
    url <- paste0("https://github.com/CT-Data-Haven/nhood_profile_data18/blob/master/to_distro/", path)
    file <- paste(city, "nhood_2018.csv", sep = "_")
    print(url)
    file_create_or_update_request(file_name = path, description = desc, labels = list("clean data", "sample"), url = url)
  })

walk(nhood_req, ~update_dataset(dataset, dataset_update_request(files = list(.))))

dwapi::update_dataset(dataset, dataset_update_request(license_string = "CC-BY-SA"))
dwapi::sync(dataset)