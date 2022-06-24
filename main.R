library(tidyverse)
library(readxl)
library(here)

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
  
  
  