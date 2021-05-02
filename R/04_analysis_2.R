# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")
library(broom)  # devtools::install_github("tidymodels/broom")
library(cowplot)
library(patchwork)

# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")




# Load data ---------------------------------------------------------------

# Load marker genes dataset
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

#Kmeans clustering (Julies)
kmeans_marker_genes <- kmeans_func(borovecki_data_clean_aug_marker_genes)
kmeans_all_genes <- kmeans_func(borovecki_data_clean_aug_all_genes)

#PCA fit (Julies)
pca_fit_marker_genes <- pca_fit(borovecki_data_clean_aug_marker_genes)
pca_fit_all_genes <- pca_fit(borovecki_data_clean_aug_all_genes)



# Visualise data ----------------------------------------------------------

# Plot PCA

pca_plot <- function(data){
  return(pca_fit(data)  %>% 
           augment(data) %>% # add original dataset back in
           ggplot(aes(.fittedPC1, .fittedPC2, color = outcome)) + 
           geom_point(size = 1.5) +
           scale_color_manual(
             values = c(symptomatic = "#D55E00", pre_symptomatic = "#00FF00", control = "#0072B2")
           ) +
           theme_half_open(12) + background_grid()+
           xlab("PC1") + ylab("PC2"))
}




pm <- pca_plot(borovecki_data_clean_aug_marker_genes) + 
  ggtitle("PCA - Marker Genes") 

pa <- pca_plot(borovecki_data_clean_aug_all_genes) + 
  ggtitle("PCA - All Genes")

pm + pa

# Plot Variance explained by each principal component

variance_plot <- function(data){
  return(pca_fit(data) %>%
           tidy(matrix = "eigenvalues") %>%
           ggplot(aes(PC, percent)) +
           geom_col(fill = "#56B4E9", alpha = 0.8) +
           scale_x_continuous(breaks = 1:9) +
           scale_y_continuous(
             labels = scales::percent_format(),
             expand = expansion(mult = c(0, 0.01))
           ) +
           theme_minimal_hgrid(12))
}


vm <- variance_plot(borovecki_data_clean_aug_marker_genes) + 
  ggtitle("Variance Explained - Marker Genes") 

va <- variance_plot(borovecki_data_clean_aug_all_genes) + 
  ggtitle("Variance Explained - All Genes")

vm + va
  

#KMEANS (Julies)
#Plot kmeans clustering for marker genes
kmeans_marker_genes_plot <- kmeans_plot(pca_fit_marker_genes, kmeans_marker_genes) + 
  ggtitle("Marker genes", subtitle = "K-means clustering")

#Plot kmeans clustering for all genes
kmeans_all_genes_plot <- kmeans_plot(pca_fit_all_genes, kmeans_all_genes) + 
  ggtitle("All genes", subtitle = "K-means clustering")

kmeans_plots <- kmeans_marker_genes_plot + 
                kmeans_all_genes_plot +
                plot_layout(guides = "collect") #Common legend


# Write data --------------------------------------------------------------
ggsave(file = "Results/kmeans_plots.png", plot = kmeans_plots)
