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

# Remove NA values...
borovecki_data_clean <- borovecki_data # %>%  Remove NA

# Move outcome column to the front - THIS IS DONE IN 3 AUGMENT :)
# borovecki_data_clean <- borovecki_data  # %>% relocate(outcome)



# Plot the outcome column
#outcome_stat <- borovecki_data_clean %>%
#  group_by(outcome) %>%
#  count()

#ggplot(data = outcome_stat, mapping = aes(x = outcome, weight = n)) + 
#  geom_histogram(stat="count")



# Write data --------------------------------------------------------------
write_tsv(x = borovecki_data_clean,
          file = "data/02_borovecki_data_clean.tsv")
