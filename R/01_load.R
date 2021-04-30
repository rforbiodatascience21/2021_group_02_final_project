
# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load and wrangle data ---------------------------------------------------------------

# Make tibble where first column is the gene shared between the patients
borovecki_data <- dir(path = "data/raw", pattern = "*.tsv", full.names = T) %>%
  map(read_tsv) %>%
  reduce(inner_join, by = "ID_REF")


# Write data --------------------------------------------------------------
write_tsv(x = borovecki_data,
          file = "data/01_borovecki_data.tsv")

