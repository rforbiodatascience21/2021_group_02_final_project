# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")
library("dplyr")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
borovecki_data <- read_tsv(file = "data/01_borovecki_data.tsv")


# Wrangle data ------------------------------------------------------------

# Transpose the tibble, use the first row as column name and create a outcome 
# column based on the ID_REF column
borovecki_data_clean <- as_tibble(cbind(nms = names(borovecki_data), t(borovecki_data))) %>%
  set_names(.[1, ]) %>%
  slice(-1) %>%
  mutate(outcome = case_when(str_detect(ID_REF, "pre_symp") ~ "pre_symptomatic",
                             str_detect(ID_REF, "control") ~ "control",
                             str_detect(ID_REF, "symp") ~ "symptomatic")) %>%
  select(outcome, everything(), -"ID_REF")

# Plot the outcome column
#outcome_stat <- borovecki_data_clean %>%
#  group_by(outcome) %>%
#  count()

#ggplot(data = outcome_stat, mapping = aes(x = outcome, weight = n)) + 
#  geom_histogram(stat="count")



# Write data --------------------------------------------------------------
write_tsv(x = borovecki_data_clean,
          file = "data/02_borovecki_data_clean.tsv")
