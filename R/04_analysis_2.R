# PCA PLOT FOR THE MARKER GENES FOUND IN THE PAPER

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

#Load names of our own genes as vector
own_marker_genes <- c(read_lines(file = "data/own_marker_genes.txt"))



# Wrangle data ------------------------------------------------------------


# Make dataset of our own marker genes
borovecki_data_clean_aug_own_marker_genes <- borovecki_data_clean_aug_all_genes %>%
  select(c(outcome), own_marker_genes)

# Log2 transform numeric data

borovecki_data_clean_aug_all_genes_log2 <-  borovecki_data_clean_aug_all_genes %>%
  mutate_if(is.numeric, log2)

borovecki_data_clean_aug_marker_genes_log2 <-  borovecki_data_clean_aug_marker_genes %>%
  mutate_if(is.numeric, log2)

borovecki_data_clean_aug_own_marker_genes_log2 <-  borovecki_data_clean_aug_own_marker_genes %>%
  mutate_if(is.numeric, log2)



# Visualise data ----------------------------------------------------------

# Plot PCA

pa <- pca_plot(borovecki_data_clean_aug_all_genes) + 
  ggtitle("PCA - All Genes") + 
  theme(plot.title = element_text(size = 10, face = "bold"))


pm <- pca_plot(borovecki_data_clean_aug_marker_genes) + 
  ggtitle("PCA - Marker Genes") + 
  theme(plot.title = element_text(size = 10, face = "bold"))

po <- pca_plot(borovecki_data_clean_aug_own_marker_genes) + 
  ggtitle("PCA - Own Marker Genes") + 
  theme(plot.title = element_text(size = 10, face = "bold"))


pa2 <- pca_plot(borovecki_data_clean_aug_all_genes_log2) + 
  ggtitle("PCA - All Genes - log2") + 
  theme(plot.title = element_text(size = 10, face = "bold"))

pm2 <- pca_plot(borovecki_data_clean_aug_marker_genes_log2) + 
  ggtitle("PCA - Marker Genes - log2") + 
  theme(plot.title = element_text(size = 10, face = "bold"))

po2 <- pca_plot(borovecki_data_clean_aug_own_marker_genes_log2) + 
  ggtitle("PCA - Own Marker Genes - log2") + 
  theme(plot.title = element_text(size = 10, face = "bold"))



pca_plots <- (pa + pa2) / (pm + pm2) / (po + po2) +
  plot_layout(guides = "collect") #Common legend

# Plot Variance explained by each principal component

va <- variance_plot(borovecki_data_clean_aug_all_genes) + 
  ggtitle("Variance Explained - All Genes") + 
  theme(plot.title = element_text(size = 8, face = "bold"))


vm <- variance_plot(borovecki_data_clean_aug_marker_genes) + 
  ggtitle("Variance Explained - Marker Genes") + 
  theme(plot.title = element_text(size = 8, face = "bold"))

vo <- variance_plot(borovecki_data_clean_aug_own_marker_genes) + 
  ggtitle("Variance Explained - Own Marker Genes") + 
  theme(plot.title = element_text(size = 8, face = "bold"))

va2 <- variance_plot(borovecki_data_clean_aug_all_genes_log2) +
  ggtitle("Variance Explained - All Genes - log2") +
  theme(plot.title = element_text(size = 8, face = "bold"))

vm2 <- variance_plot(borovecki_data_clean_aug_marker_genes_log2) + 
  ggtitle("Variance Explained - Marker Genes - log2") + 
  theme(plot.title = element_text(size = 8, face = "bold"))

vo2 <- variance_plot(borovecki_data_clean_aug_own_marker_genes_log2) + 
  ggtitle("Variance Explained - Own Marker Genes - log2") + 
  theme(plot.title = element_text(size = 8, face = "bold"))




variance_explained_plots <- (va + va2) / (vm + vm2) / (vo + vo2) 
  

# Write data --------------------------------------------------------------

ggsave(file = "Results/pca_plots.png", plot = pca_plots)

ggsave(file = "Results/variance_explained_plots.png", plot = variance_explained_plots)



