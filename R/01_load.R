
# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
load("data/raw/borovecki.RData")



# Wrangle data ------------------------------------------------------------

#We create a tibble consisting of the x and add the the y matrix as variable "outcome" 

borovecki_data <- mutate(as_tibble(pluck(borovecki,"x")),
                         outcome=pluck(borovecki,"y")) 



# Write data --------------------------------------------------------------
write_tsv(x = borovecki_data,
          file = "data/01_borovecki_data.tsv")

