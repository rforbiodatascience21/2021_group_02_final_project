# KMEANS ANALYSIS

# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library(tidyverse)
library(broom)
library(cowplot)


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------

# Load marker genes dataset
borovecki_data_clean_aug_marker_genes <- read_tsv(file = "data/03_borovecki_data_clean_aug_marker_genes.tsv")



# Wrangle data ------------------------------------------------------------

#Do kmeans of marker genes
kmeans_marker_genes_data <- kmeans_func(borovecki_data_clean_aug_marker_genes)

#Do PCA fit of marker genes
pca_fit_marker_genes_data <- pca_fit(borovecki_data_clean_aug_marker_genes)



# Visualise data ----------------------------------------------------------

#Plot kmeans clustering for marker genes
kmeans_marker_genes_plot <- kmeans_plot(pca_fit_marker_genes_data, 
                                        kmeans_marker_genes_data) + 
  ggtitle("Marker genes")



# Write data --------------------------------------------------------------
ggsave(file = "Results/kmeans_plot.png", 
       plot = kmeans_marker_genes_plot,
       width = 6.77, 
       height = 2.83)
