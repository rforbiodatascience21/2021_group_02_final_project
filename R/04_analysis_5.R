# KMEANS ANALYSIS

# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------

# Load marker genes dataset
borovecki_data_clean_aug_marker_genes <- read_tsv(file = "data/03_borovecki_data_clean_aug_marker_genes.tsv")

# Load all genes dataset
borovecki_data_clean_aug_all_genes <- read_tsv(file = "data/03_borovecki_data_clean_aug_all_genes.tsv")



# Wrangle data ------------------------------------------------------------

#Do kmeans of both marker and all genes
kmeans_marker_genes_data <- kmeans_func(borovecki_data_clean_aug_marker_genes)
kmeans_all_genes_data <- kmeans_func(borovecki_data_clean_aug_all_genes)

#Do PCA fit of both marker and all genes
pca_fit_marker_genes_data <- pca_fit(borovecki_data_clean_aug_marker_genes)
pca_fit_all_genes_data <- pca_fit(borovecki_data_clean_aug_all_genes)


# Visualise data ----------------------------------------------------------

#Plot kmeans clustering for marker genes
kmeans_marker_genes_plot <- kmeans_plot(pca_fit_marker_genes_data, kmeans_marker_genes_data) + 
  ggtitle("Marker genes", 
          subtitle = "K-means clustering")

#Plot kmeans clustering for all genes
kmeans_all_genes_plot <- kmeans_plot(pca_fit_marker_genes_data, kmeans_marker_genes_data) + 
  ggtitle("All genes", 
          subtitle = "K-means clustering")

kmeans_plots <- kmeans_marker_genes_plot + kmeans_all_genes_plot +
  plot_layout(guides = "collect")


# Write data --------------------------------------------------------------
ggsave(file = "Results/kmeans_plots.png", 
       plot = kmeans_plots,
       width = 6.77, 
       height = 2.83)
