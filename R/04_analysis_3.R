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
kmeans_marker_genes <- borovecki_data_clean_aug_marker_genes %>%
  select(where(is.numeric)) %>% #Only use the numeric columns
  kmeans(centers = 3) %>% #Do kmeans with 3 centroids
  augment(borovecki_data_clean_aug_marker_genes) #Combine kmeans data with original data

#Do kmeans on all genes
kmeans_all_genes <- borovecki_data_clean_aug_all_genes %>%
  select(where(is.numeric)) %>% #Only use the numeric columns
  kmeans(centers = 3) %>% #Do kmeans with 3 centroids
  augment(borovecki_data_clean_aug_all_genes) #Combine kmeans data with original data


#Aleks' PCA function
pca_fit <- function(data){
  return(data %>% 
           select(where(is.numeric)) %>% # retain only numeric columns
           scale() %>% # scale data
           prcomp())
}

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


#Append the outcome column, pivot and add type
random_genes_data <- borovecki_data_clean_aug_all_genes %>%
  select(outcome) %>%
  cbind(random_genes) %>% #Do this with join instead? 
  pivot_longer(cols = -outcome, #Pivot all columns except outcome
               names_to = "Gene",
               values_to = "Value") %>%
  add_column(Type = "Random genes", .before = "outcome") #Add a factor column and place it befor the outcome column


#Do the same to the marker gene dataframe and join the two dataframes
marker_and_random_data <- borovecki_data_clean_aug_marker_genes %>%
  pivot_longer(cols = -outcome, #Pivot all columns except outcome
               names_to = "Gene",
               values_to = "Value") %>%
  add_column(Type = "Marker genes", .before = "outcome") %>% #Add a factor column and place it befor the outcome column
  rbind(random_genes_data)



# Visualise data ----------------------------------------------------------

#KMEANS-------------------
#Plot kmeans clusters for marker genes
pca_fit_marker_genes %>%
  augment(kmeans_marker_genes) %>% #Combine PC coordinates with original data
  ggplot(mapping = aes(x = .fittedPC1, y = .fittedPC2, colour = .cluster, shape = outcome)) +
  geom_point(size = 2) +
  scale_colour_viridis(discrete = TRUE) +
  theme_half_open(12) + 
  background_grid() +
  labs(x = "PC1", 
       y = "PC2", 
       title = "K-means clustering - Marker Genes", 
       colour = "Kmeans cluster",
       shape = "Actual cluster")


#Plot kmeans cluster for all genes
pca_fit_all_genes %>%
  augment(kmeans_all_genes) %>% #Combine PC coordinates with original data
  ggplot(mapping = aes(x = .fittedPC1, y = .fittedPC2, colour = .cluster, shape = outcome)) +
  geom_point(size = 2) +
  scale_color_viridis(discrete = TRUE) +
  theme_half_open(12) + 
  background_grid() +
  labs(x = "PC1", 
       y = "PC2", 
       title = "K-means clustering - All Genes", 
       colour = "Kmeans cluster",
       shape = "Actual cluster")


#BOXPLOT---------------------
#Boxplot log2 transformed - marker genes
borovecki_data_clean_aug_marker_genes %>%
  pivot_longer(cols = -outcome, #Pivot all columns except outcome
               names_to = "Gene",
               values_to = "Value") %>%
  mutate(Value = log2(Value)) %>% #Log transform expression values 
  ggplot(mapping = aes(x = outcome, 
                       y = Value, 
                       fill = outcome)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_viridis(discrete = TRUE) +
  theme(legend.position = "none") +
  labs(x = "Outcome", 
       y = "Log2 expression (Unit??)",
       title = "Distribution of marker genes expression")


#Boxplot log2 transformed - all genes
borovecki_data_clean_aug_all_genes %>%
  pivot_longer(cols = -outcome, #Pivot all columns except outcome
               names_to = "Gene",
               values_to = "Value") %>%
  mutate(Value = log2(Value)) %>% #Log transform expression values 
  ggplot(mapping = aes(x = outcome, 
                       y = Value, 
                       fill = outcome)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_viridis(discrete = TRUE) +
  theme(legend.position = "none") +
  labs(x = "Outcome", 
       y = "Log2 expression (Unit??)",
       title = "Distribution of all genes expresseion")


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