library(tidyverse)
library(readxl)
library(here)
library(sf)

sample_inventory <- read_xlsx(here("input", "sample_inventory_2023-05-15.xlsx"))

ELISA <- read_csv(here("input", "elisa_results_2022-09-07.csv")) %>%
  select(plate, rodent_uid, blood_sample_id, `450`, `630`)

ELISA_id <- ELISA %>%
  filter(!str_detect(blood_sample_id, "neg|pos|NC|PC")) %>%
  select(-rodent_uid) %>%
  left_join(sample_inventory %>%
              select(rodent_uid, blood_id), by = c("blood_sample_id" = "blood_id")) %>%
  select(rodent_uid, blood_sample_id)

ELISA_results_raw <- left_join(ELISA %>%
                                 select(-rodent_uid), ELISA_id, by = c("blood_sample_id")) %>%
  select(plate, rodent_uid, blood_sample_id, `450`, `630`)

ELISA_awaited <- sample_inventory %>%
  filter(!rodent_uid %in% ELISA_results_raw$rodent_uid)

ELISA_results_pre_processed <- ELISA_results_raw %>%
  left_join(sample_inventory %>%
              select(rodent_uid_s = rodent_uid, blood_id),
            by = c("blood_sample_id" = "blood_id")) %>%
  mutate(rodent_uid = coalesce(rodent_uid, rodent_uid_s),
         rodent_uid = case_when(str_detect(blood_sample_id, "neg|negative|NC|pos|positive|PC") ~ as.character(NA),
                                TRUE ~ rodent_uid)) %>%
  select(-rodent_uid_s) %>%
  mutate(`450-630` = `450` - `630`) %>%
  distinct()

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
  
# Exploration

combined_data <- readRDS(gzcon(url("https://github.com/DidDrog11/rodent_trapping/raw/main/data/data_for_export/combined_data.rds")))

trap_data <- tibble(combined_data$trap_data) %>%
  select(-geometry)

rodent_data <- combined_data$rodent_data %>%
  select(rodent_uid, species, clean_names, genus, age_group, weight, head_body, hind_foot, ear, length_skull, sex, village, visit, trap_uid)

enriched_ELISA <- ELISA_final %>%
  filter(!str_detect(blood_sample_id, "neg|pos|NC|PC")) %>%
  filter(!str_detect(rodent_uid, "BAM")) %>%
  group_by(rodent_uid) %>%
  arrange(interpretation) %>%
  slice(1) %>%
  ungroup() %>%
  full_join(rodent_data, by = "rodent_uid") %>%
  full_join(sample_inventory %>%
              select(visit, rodent_uid) %>%
              filter(!str_detect(rodent_uid, "BAM"))) %>%
  arrange(visit, rodent_uid)
  left_join(trap_data, by = c("rodent_uid", "trap_uid")) %>%
  distinct()

ELISA_data <- list(ELISA_results = ELISA_final,
                   ELISA_enriched = enriched_ELISA)

write_rds(ELISA_data, here("output", "ELISA_output.rds"))  

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
  group_by(clean_names) %>%
  summarise(total = n()) %>%
  left_join(positive_ELISA %>%
              distinct(rodent_uid, .keep_all = TRUE) %>%
              group_by(clean_names) %>%
              summarise(n_positive = n())) %>%
  mutate(perc_pos = round(n_positive/total * 100, 1)) %>%
  left_join(negative_ELISA %>%
              distinct(rodent_uid, .keep_all = TRUE) %>%
              group_by(clean_names) %>%
              summarise(n_negative = n())) %>%
  mutate(perc_neg = round(n_negative/total * 100, 1))

ggplot(positive_ELISA %>%
         mutate(village = str_to_sentence(village),
                clean_names = str_to_sentence(str_replace_all(clean_names, "_", " "))) %>%
         group_by(village, visit) %>%
         summarise(n_positive = n())) +
  geom_col(aes(x = visit, y = n_positive, fill = village), position = position_stack()) +
  labs(x = "Visit",
       y = "N positive samples",
       colour = "Village") +
  theme_bw()
