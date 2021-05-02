# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library(tidyverse)
library(tidymodels)
library(broom)
library(cowplot)
library(viridis)
library(ggridges)
library(patchwork)


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")



# Load data ---------------------------------------------------------------

# Load marker genes dataset
borovecki_data_clean_aug_marker_genes <- read_tsv(file = "data/03_borovecki_data_clean_aug_marker_genes.tsv")

# Load all genes dataset
borovecki_data_clean_aug_all_genes <- read_tsv(file = "data/03_borovecki_data_clean_aug_all_genes.tsv")



# Wrangle data ------------------------------------------------------------

#KMEANS-------------------
#Do kmeans on marker genes
kmeans_marker_genes <- kmeans_func(borovecki_data_clean_aug_marker_genes)

#Do kmeans on all genes
kmeans_all_genes <- kmeans_func(borovecki_data_clean_aug_all_genes)

# PCA on marker genes
pca_fit_marker_genes <- pca_fit(borovecki_data_clean_aug_marker_genes)

#PCA on all genes
pca_fit_all_genes <- pca_fit(borovecki_data_clean_aug_all_genes)



#RANDOM DATA-----------------
#Extract 12 random genes 
set.seed(22)
random_genes <- borovecki_data_clean_aug_all_genes %>%
  select(-c(outcome, "201012_at", "202653_s_at", "208374_s_at", "200989_at",
            "212287_at", "218589_at", "217816_s_at", "213044_at",
            "201071_x_at", "213168_at", "201023_at", "217783_s_at")) %>% #Remove the 12 marker genes
  sample(size = 12)


#Append the outcome column, pivot and add type to random genes
random_genes_data <- borovecki_data_clean_aug_all_genes %>%
  select(outcome) %>%
  cbind(random_genes) %>% #Do this with join instead? 
  pivot_longer(cols = -outcome, #Pivot all columns except outcome
               names_to = "Gene",
               values_to = "Value") %>%
  add_column(Type = "Random genes", .before = "outcome") #Add a factor column and place it before the outcome column


#Do the same to the marker gene dataframe and join the two dataframes
marker_and_random_data <- borovecki_data_clean_aug_marker_genes %>%
  pivot_longer(cols = -outcome, #Pivot all columns except outcome
               names_to = "Gene",
               values_to = "Value") %>%
  add_column(Type = "Marker genes", .before = "outcome") %>% #Add a factor column and place it before the outcome column
  rbind(random_genes_data)



# Visualise data ----------------------------------------------------------


#KMEANS-------------------
#Plot kmeans clusters for marker genes
kmeans_marker_genes_plot <- kmeans_plot(pca_fit_marker_genes, kmeans_marker_genes) + 
  ggtitle("K-means clustering - Marker genes")


#Plot kmeans cluster for all genes
kmeans_marker_genes_plot <- kmeans_plot(pca_fit_all_genes, kmeans_all_genes) + 
  ggtitle("K-means clustering - All genes")


#BOXPLOT---------------------


#Boxplot log2 transformed - marker genes
log2_marker_genes_boxplot <- log2_boxplot(borovecki_data_clean_aug_marker_genes) +
  ggtitle("Distribution of marker genes expression")


#Boxplot log2 transformed - all genes
log2_all_genes_boxplot <- log2_boxplot(borovecki_data_clean_aug_all_genes) +
  ggtitle("Distribution of all genes expression")


log2_marker_genes_boxplot + log2_all_genes_boxplot


#RIDGELINE----------------
#Ridgplot divided into control and symptomatic
marker_and_random_data %>%
  ggplot(mapping = aes(x = Value, y = outcome, fill = stat(x))) +
  geom_density_ridges_gradient() +
  scale_fill_viridis() +
  theme_ridges() + 
  theme(legend.position = "none", axis.title.y = element_blank()) +
  scale_y_discrete(limit = c("symptomatic", "pre_symptomatic", "control")) +
  labs(x = "Expression (unit??)") +
  facet_wrap(~ Type)



# Write data --------------------------------------------------------------
#ggsave(file = "") #Do we need a results folder?