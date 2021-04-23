# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
borovecki_data_clean_aug_all_genes <- read_tsv(file = "data/03_borovecki_data_clean_aug_all_genes.tsv.gz")


# Wrangle data ------------------------------------------------------------
#borovecki_data_clean_aug %>% ...


# Model data
#borovecki_data_clean_aug %>% ...


# Visualise data ----------------------------------------------------------
#borovecki_data_clean_aug %>% ...


# Write data --------------------------------------------------------------
#write_tsv(...)
#ggsave(...)