
# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
load("data/raw/borovecki.RData")
#We add the y matrix as a column in the big matrix (naming the column 'outcome')
borovecki_data <- mutate(as_tibble(pluck(borovecki,"x")),
                          outcome=pluck(borovecki,"y"))


# Wrangle data ------------------------------------------------------------
#my_data <- my_data_raw # %>% ...


# Write data --------------------------------------------------------------
#write_tsv(x = my_data,
#          file = "data/01_my_data.tsv")