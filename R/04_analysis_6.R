# PCA ANALYSIS OF OUR OWN MARKER GENES

# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------

#Load data for all genes
borovecki_data_clean_aug_all_genes <- read_tsv(file = "data/03_borovecki_data_clean_aug_all_genes.tsv")

#Load names of our own genes as vector
own_marker_genes <- c(read_lines(file = "data/own_marker_genes.txt"))



# Wrangle data ------------------------------------------------------------
#Make dataset of our own marker genes
borovecki_data_clean_aug_own_marker_genes <- borovecki_data_clean_aug_all_genes %>%
  select(c(outcome), own_marker_genes)



# Visualise data ----------------------------------------------------------
own_marker_genes_pca_fit_plot <- pca_plot(borovecki_data_clean_aug_own_marker_genes) +
  ggtitle("PCA - Own Marker Genes")



# Write data --------------------------------------------------------------
ggsave(file = "Results/own_marker_genes_PCA.png", plot = own_marker_genes_pca_fit_plot)
