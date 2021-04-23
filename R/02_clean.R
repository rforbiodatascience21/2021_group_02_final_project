# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
borovecki_data <- read_tsv(file = "data/01_borovecki_data.tsv")


# Wrangle data ------------------------------------------------------------
borovecki_data_clean <- borovecki_data # %>% ...


# Write data --------------------------------------------------------------
write_tsv(x = borovecki_data_clean,
          file = "data/02_borovecki_data_clean.tsv")