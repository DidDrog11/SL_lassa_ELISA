library(tidyverse)
library(readxl)
library(here)
library(sf)

sample_inventory <- read_xlsx(here("input", "sample_inventory.xlsx"))

ELISA_results_raw <- read_csv(here("input", "elisa_results_2022-06-23.csv")) %>%
  select(plate, rodent_uid, blood_sample_id, `450`, `630`)

ELISA_results_pre_processed <- ELISA_results_raw %>%
  left_join(sample_inventory %>%
              select(rodent_uid_s = rodent_uid, blood_id),
            by = c("blood_sample_id" = "blood_id")) %>%
  mutate(rodent_uid = coalesce(rodent_uid, rodent_uid_s),
         rodent_uid = case_when(str_detect(blood_sample_id, "neg|negative|NC|pos|positive|PC") ~ as.character(NA),
                                TRUE ~ rodent_uid)) %>%
  select(-rodent_uid_s) %>%
  mutate(`450-630` = `450` - `630`)

cut_offs <- ELISA_results_pre_processed %>%
  filter(str_detect(blood_sample_id, "neg|negative|NC")) %>%
  group_by(plate) %>%
  summarise(cut_off = mean(`450-630`, na.rm = TRUE) + 0.15)

ELISA_results_processed <- ELISA_results_pre_processed %>%
  left_join(cut_offs, by = c("plate")) %>%
  mutate(result = case_when(!str_detect(blood_sample_id, "neg|negative|NC") ~ `450-630`/cut_off))

positive_control_qc <- ELISA_results_processed %>%
  filter(str_detect(blood_sample_id, "pos|positive|PC")) %>%
  mutate(valid = if_else(`450-630` >= 0.6, TRUE, FALSE))

ELISA_final <- ELISA_results_processed %>%
  mutate(interpretation = factor(case_when(result >= 1.1 ~ "Positive",
                                    result < 1.1 & result > 0.9 ~ "Equivocal",
                                    result <= 0.9 ~ "Negative"), levels = c("Positive", "Negative", "Equivocal")))
  
ELISA_final %>%
  filter(!is.na(rodent_uid)) %>%
  group_by(rodent_uid) %>%
  arrange(interpretation) %>%
  slice(1) %>%
  janitor::tabyl(interpretation)
  
write_rds(ELISA_final, here("output", "ELISA_final.rds"))  
  
# Exploration

trap_data <- read_rds(here("input", "trap_spatial.rds")) %>%
  filter(!is.na(rodent_uid)) %>%
  select(date_set, village, visit, grid_number, trap_number, habitat_group, site_habitat, rodent_uid, geometry)

rodent_data <- read_csv(here("input", "rodent_data.csv")) %>%
  select(rodent_uid, initial_species_id, group, weight, head_body, tail, hind_foot, ear, length_skull, sex, age_group, trap_uid) %>%
  mutate(initial_species_id = case_when(str_detect(initial_species_id, "(?i)mastomys") ~ "mastomys_spp",
                                        str_detect(initial_species_id, "(?i)minutoides|mus_") ~ "mus_spp",
                                        str_detect(initial_species_id, "(?i)sikapusi|lophuromys") ~ "lophuromys_spp",
                                        str_detect(initial_species_id, "(?i)striatus") ~ "lemniscomys_spp",
                                        str_detect(initial_species_id, "(?i)malacomys") ~ "malacomys_spp",
                                        str_detect(initial_species_id, "(?i)praomys|proamys") ~ "praomys_spp",
                                        str_detect(initial_species_id, "(?i)rattus") ~ "rattus_spp",
                                        str_detect(initial_species_id, "(?i)shrew") ~ "crocidura_spp",
                                        str_detect(initial_species_id, "(?i)Tatera") ~ "gerbillinae_spp",
                                        TRUE ~ initial_species_id))

enriched_ELISA <- ELISA_final %>%
  filter(!str_detect(blood_sample_id, "neg|pos|NC|PC")) %>%
  left_join(trap_data, by = "rodent_uid") %>%
  left_join(rodent_data, by = "rodent_uid") %>%
  distinct()

negative_ELISA <- enriched_ELISA %>%
  filter(interpretation == "Negative") %>%
  distinct() %>%
  filter(!rodent_uid %in% enriched_ELISA$rodent_uid[enriched_ELISA$interpretation == "Positive"])

positive_ELISA <- enriched_ELISA %>%
  filter(interpretation == "Positive") %>%
  distinct() %>%
  filter(!rodent_uid %in% enriched_ELISA$rodent_uid[enriched_ELISA$interpretation == "Negative"])

mixed_ELISA <- enriched_ELISA %>%
  filter(!rodent_uid %in% c(positive_ELISA$rodent_uid, negative_ELISA$rodent_uid))

total_rodents <- rodent_data %>%
  group_by(initial_species_id) %>%
  summarise(total = n()) %>%
  left_join(positive_ELISA %>%
              group_by(initial_species_id) %>%
              summarise(n_positive = n())) %>%
  mutate(perc_pos = round(n_positive/total * 100, 1)) %>%
  left_join(negative_ELISA %>%
              group_by(initial_species_id) %>%
              summarise(n_negative = n())) %>%
  mutate(perc_neg = round(n_negative/total * 100, 1))

ggplot(positive_ELISA %>%
         mutate(village = str_to_sentence(village),
                initial_species_id = str_to_sentence(str_replace_all(initial_species_id, "_", " "))) %>%
         group_by(village, visit) %>%
         summarise(n_positive = n())) +
  geom_col(aes(x = visit, y = n_positive, fill = village), position = position_dodge()) +
  scale_y_continuous(limits = c(0, 7)) +
  labs(x = "Visit",
       y = "N positive samples",
       colour = "Village") +
  theme_bw()
