
pacman::p_load(readxl, tidyverse)
plasma_npx <- read_csv("data/plasma_npx_level.csv", show_col_types = FALSE)
plasma_sample <- read_csv("data/plasma_sample_level.csv", show_col_types = FALSE)
serum_npx <- read_csv("data/serum_npx_level.csv", show_col_types = FALSE)
serum_sample <- read_csv("data/serum_sample_level.csv", show_col_types = FALSE)
PLASMA <- plasma_npx %>% left_join(plasma_sample, by = c("SampleID"   ,   "Individual_ID" )) %>% dplyr::arrange(Individual_ID, Assay)
SERUM <- serum_npx %>% left_join(serum_sample, by = c("SampleID"   ,   "Individual_ID" )) %>% dplyr::arrange(Assay, Individual_ID)
