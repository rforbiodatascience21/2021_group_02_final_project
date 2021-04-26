# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
borovecki_data_clean <- read_tsv(file = "data/02_borovecki_data_clean.tsv")


# Wrangle data ------------------------------------------------------------

# Create an outcome column and move it to the front
borovecki_data_clean_aug_all_genes <- borovecki_data_clean %>%
  mutate(outcome = case_when(str_detect(ID_REF, "pre_symp") ~ "pre_symptomatic",
                                                                 str_detect(ID_REF, "control") ~ "control",
                                                                 str_detect(ID_REF, "symp") ~ "symptomatic")) %>%
  select(outcome, everything(), -"ID_REF")


# Subset the data, only include marker genes
borovecki_data_clean_aug_marker_genes <- borovecki_data_clean_aug_all_genes  %>%
  select("outcome", "201012_at", "202653_s_at", "208374_s_at", "200989_at", "212287_at", 
         "218589_at", "217816_s_at", "213044_at", 
         "201071_x_at", "213168_at", "201023_at", "217783_s_at")




# Write data --------------------------------------------------------------
write_tsv(x = borovecki_data_clean_aug_all_genes,
          file = "data/03_borovecki_data_clean_aug_all_genes.tsv")

write_tsv(x = borovecki_data_clean_aug_marker_genes,
          file = "data/03_borovecki_data_clean_aug_marker_genes.tsv")

  



