# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
borovecki_data <- read_tsv(file = "data/01_borovecki_data.tsv")



# Wrangle data ------------------------------------------------------------


# Transpose the tibble 
borovecki_data_clean <- borovecki_data %>%
  drop_na() %>%
  pivot_longer(cols = -ID_REF, 
               names_to = "Patient", 
               values_to = "Values") %>%
  pivot_wider(id_cols = Patient, 
              names_from = ID_REF, 
              values_from = Values)
  

# Write data --------------------------------------------------------------
write_tsv(x = borovecki_data_clean,
          file = "data/02_borovecki_data_clean.tsv")
