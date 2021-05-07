# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")
library(broom)  # devtools::install_github("tidymodels/broom")
library(cowplot)
library(patchwork)
library(viridis)

# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")




# Load data ---------------------------------------------------------------

# Load marker genes dataset
borovecki_data_clean_aug_marker_genes <- read_tsv(file = "data/03_borovecki_data_clean_aug_marker_genes.tsv")

# Load all genes dataset
borovecki_data_clean_aug_all_genes <- read_tsv(file = "data/03_borovecki_data_clean_aug_all_genes.tsv")


# Wrangle data ------------------------------------------------------------

borovecki_data_clean_aug_marker_genes_log2 <-  borovecki_data_clean_aug_marker_genes %>%
  mutate_if(is.numeric, log2)


borovecki_data_clean_aug_all_genes_log2 <-  borovecki_data_clean_aug_all_genes %>%
  mutate_if(is.numeric, log2)

# Model data


# Visualise data ----------------------------------------------------------

# Plot PCA






pm <- pca_plot(borovecki_data_clean_aug_marker_genes) + 
  ggtitle("PCA - Marker Genes") 

pa <- pca_plot(borovecki_data_clean_aug_all_genes) + 
  ggtitle("PCA - All Genes")


pm2 <- pca_plot(borovecki_data_clean_aug_marker_genes_log2) + 
  ggtitle("PCA - Marker Genes - log2") 

pa2 <- pca_plot(borovecki_data_clean_aug_all_genes_log2) + 
  ggtitle("PCA - All Genes - log2")

pca_plots <- (pm + pa) / (pm2 + pa2) +
  plot_layout(guides = "collect") #Common legend

# Plot Variance explained by each principal component




vm <- variance_plot(borovecki_data_clean_aug_marker_genes) + 
  ggtitle("Variance Explained - Marker Genes") + 
  theme(plot.title = element_text(size = 10, face = "bold"))

va <- variance_plot(borovecki_data_clean_aug_all_genes) + 
  ggtitle("Variance Explained - All Genes") + 
  theme(plot.title = element_text(size = 10, face = "bold"))


vm2 <- variance_plot(borovecki_data_clean_aug_marker_genes_log2) + 
  ggtitle("Variance Explained - Marker Genes - log2") + 
  theme(plot.title = element_text(size = 10, face = "bold"))

va2 <- variance_plot(borovecki_data_clean_aug_all_genes_log2) + 
  ggtitle("Variance Explained - All Genes - log2") + 
  theme(plot.title = element_text(size = 10, face = "bold"))

variance_explained_plots <- (vm + va) / (vm2 + va2) 
  

#KMEANS 
#Plot kmeans clustering for marker genes
kmeans_marker_genes_plot <- kmeans_plot(pca_fit(borovecki_data_clean_aug_marker_genes), kmeans_func(borovecki_data_clean_aug_marker_genes)) + 
  ggtitle("Marker genes", subtitle = "K-means clustering")

#Plot kmeans clustering for all genes
kmeans_all_genes_plot <- kmeans_plot(pca_fit(borovecki_data_clean_aug_all_genes), kmeans_func(borovecki_data_clean_aug_all_genes)) + 
  ggtitle("All genes", subtitle = "K-means clustering")

kmeans_plots <- kmeans_marker_genes_plot + 
                kmeans_all_genes_plot +
                plot_layout(guides = "collect") #Common legend


# Write data --------------------------------------------------------------
ggsave(file = "Results/kmeans_plots.png", plot = kmeans_plots)

ggsave(file = "Results/pca_plots.png", plot = pca_plots)

ggsave(file = "Results/variance_explained_plots.png", plot = variance_explained_plots)



