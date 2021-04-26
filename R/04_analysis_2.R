## PCA

# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")
library(broom)
library(cowplot)


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------

# Load marker gene dataset
borovecki_data_clean_aug_marker_genes <- read_tsv(file = "data/03_borovecki_data_clean_aug_marker_genes.tsv")

# Load all genes dataset
borovecki_data_clean_aug_all_genes <- read_tsv(file = "data/03_borovecki_data_clean_aug_all_genes.tsv")


# Wrangle data ------------------------------------------------------------

# Model data

pca_fit <- function(data){
  return(data %>% 
           select(where(is.numeric)) %>% # retain only numeric columns
           scale() %>% # scale data
           prcomp())
}

pca_fit_m <- pca_fit(borovecki_data_clean_aug_marker_genes) 

pca_fit_a <- pca_fit(borovecki_data_clean_aug_all_genes)

# Visualise data ----------------------------------------------------------

# Plot PCA

pca_fit_m %>% 
  augment(borovecki_data_clean_aug_marker_genes) %>% # add original dataset back in
  ggplot(aes(.fittedPC1, .fittedPC2, color = outcome)) + 
  geom_point(size = 1.5) +
  scale_color_manual(
    values = c(symptomatic = "#D55E00", pre_symptomatic = "#00FF00", control = "#0072B2")
  ) +
  theme_half_open(12) + background_grid()

pca_fit_a %>%
  augment(borovecki_data_clean_aug_all_genes) %>% # add original dataset back in
  ggplot(aes(.fittedPC1, .fittedPC2, color = outcome)) + 
  geom_point(size = 1.5) +
  scale_color_manual(
    values = c(symptomatic = "#D55E00", pre_symptomatic = "#00FF00", control = "#0072B2")
  ) +
  theme_half_open(12) + background_grid()

# Plot Variance explained by each principal component
  
pca_fit_m %>%
  tidy(matrix = "eigenvalues") %>%
  ggplot(aes(PC, percent)) +
  geom_col(fill = "#56B4E9", alpha = 0.8) +
  scale_x_continuous(breaks = 1:9) +
  scale_y_continuous(
    labels = scales::percent_format(),
    expand = expansion(mult = c(0, 0.01))
  ) +
  theme_minimal_hgrid(12)

pca_fit_a %>%
  tidy(matrix = "eigenvalues") %>%
  ggplot(aes(PC, percent)) +
  geom_col(fill = "#56B4E9", alpha = 0.8) +
  scale_x_continuous(breaks = 1:9) +
  scale_y_continuous(
    labels = scales::percent_format(),
    expand = expansion(mult = c(0, 0.01))
  ) +
  theme_minimal_hgrid(12)











