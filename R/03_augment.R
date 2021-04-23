# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
borovecki_data_clean <- read_tsv(file = "data/02_borovecki_data_clean.tsv")


# Wrangle data ------------------------------------------------------------
borovecki_data_clean_aug_all_genes <- borovecki_data_clean  %>%
  relocate(outcome)
  



# Write data --------------------------------------------------------------
write_tsv(x = borovecki_data_clean_aug_all_genes,
          path = "data/03_borovecki_data_clean_aug_all_genes.tsv.gz")
