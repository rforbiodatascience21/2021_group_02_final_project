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
               names_to = "gene",
               values_to = "expression") %>%
  add_column(type = "Random genes", .before = "outcome") #Add a factor column and place it before the outcome column


#Do the same to the marker gene dataframe and join the two dataframes
marker_and_random_data_long <- borovecki_data_clean_aug_marker_genes %>%
  pivot_longer(cols = -outcome, #Pivot all columns except outcome
               names_to = "gene",
               values_to = "expression") %>%
  add_column(type = "Marker genes", .before = "outcome") %>% #Add a factor column and place it before the outcome column
  rbind(random_genes_data)



# Visualise data ----------------------------------------------------------

#BOXPLOT---------------------
#Boxplot log2 transformed - marker genes
log2_marker_genes_boxplot <- log2_boxplot(borovecki_data_clean_aug_marker_genes) +
  ggtitle("Distribution of marker genes expression")

#Boxplot log2 transformed - all genes
log2_all_genes_boxplot <- log2_boxplot(borovecki_data_clean_aug_all_genes) +
  ggtitle("Distribution of all genes expression")

log2_boxplots <- log2_marker_genes_boxplot + log2_all_genes_boxplot



#RIDGELINE----------------
#Ridgline for both marker genes and random genes
ridgeline_random_comparison_plot <- marker_and_random_data_long %>%
  mutate(expression = log2(expression)) %>% #Log2 transform expression values
  ggplot(mapping = aes(x = expression, y = outcome, fill = stat(x))) +
  geom_density_ridges_gradient() +
  scale_fill_viridis() +
  theme_ridges() + 
  theme(legend.position = "none", axis.title.y = element_blank()) +
  scale_y_discrete(limit = c("symptomatic", "pre_symptomatic", "control")) +
  labs(x = "Expression (unit??)") +
  facet_wrap(~ type)
print(ridgeline_random_comparison_plot)



# Write data --------------------------------------------------------------
ggsave(file = "Results/boxplots.png", plot = log2_boxplots)

ggsave(file = "Results/ridgeline.png", plot = ridgeline_random_comparison_plot) 
