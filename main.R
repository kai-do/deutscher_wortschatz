library(rdnb)
library(tidyverse)

katze_buche <- dnb_search(title='katze', limit = 100, year = 2020)

de_en_dict <- read.delim('datasets/de-en.txt', na.strings = '') %>%
  mutate(englisches_wort = trimws(englisches_wort),
         genus = str_extract(deutsches_wort, "\\{.*\\}"),
         genus = case_when(genus == "{m}" ~ "maskulin",
                           genus == "{f}" ~ "feminin",
                           genus == "{n}" ~ "neutrum",
                           genus == "{pl}" ~ "plural"),
         prafix = str_extract(deutsches_wort, "\\((.*?)\\)"),
         kasus = str_extract(prafix, "\\[(.*?)\\]"),
         prafix = str_remove(prafix, "\\[(.*?)\\]"),
         deutsches_wort = trimws(str_remove(str_remove(deutsches_wort, "\\((.*?)\\)"), "\\{.*\\}"))) %>%
  select(prafix, deutsches_wort, englisches_wort, wortklasse, kasus, genus, subjekt)

de_en_dict_einfach <- de_en_dict %>%
  group_by(deutsches_wort) %>%
  mutate(definition = row_number()) %>%
  filter(definition == 1) %>%
  select(-definition)

