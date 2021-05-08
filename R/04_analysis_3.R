# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library(tidyverse)
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

#Load random genes dataset 
borovecki_data_clean_aug_random_genes <- read_tsv(file = "data/03_borovecki_data_clean_aug_random_genes.tsv")



# Wrangle data ------------------------------------------------------------

#Pivot random gene dataframe and add type as factor column
random_genes_data_long <- borovecki_data_clean_aug_random_genes %>% 
  pivot_longer(cols = -outcome,
               names_to = "gene",
               values_to = "expression") %>%
  add_column(type = "Random genes", 
             .before = "outcome")


#Do the same to the marker gene dataframe and join with random gene dataframe
marker_and_random_data_long <- borovecki_data_clean_aug_marker_genes %>%
  pivot_longer(cols = -outcome,
               names_to = "gene",
               values_to = "expression") %>%
  add_column(type = "Marker genes", 
             .before = "outcome") %>%
  rbind(random_genes_data_long)



# Visualise data ----------------------------------------------------------

#BOXPLOT---------------------
#Boxplot log2 transformed - marker genes
log2_marker_genes_boxplot <- borovecki_data_clean_aug_marker_genes %>%
  long_log2() %>%
  boxplot_func() +
  ylim(-5, 17) +
  ggtitle("Marker genes", subtitle = "Distribution of mRNA expression") +
  ylab("log2(mRNA expression)")

#Boxplot log2 transformed - all genes
log2_all_genes_boxplot <- borovecki_data_clean_aug_all_genes %>%
  long_log2() %>%
  boxplot_func() +
  ylim(-5, 17) +
  ggtitle("All genes", subtitle = "Distribution of mRNA expression") +
  ylab("log2(mRNA expression)")

log2_boxplots <- log2_marker_genes_boxplot + log2_all_genes_boxplot

  

#RIDGELINE----------------
#Ridgline for both marker genes and random genes
ridgeline_random_comparison_plot <- marker_and_random_data_long %>%
  mutate(expression = log2(expression)) %>%
  ggplot(mapping = aes(x = expression, 
                       y = outcome, 
                       fill = stat(x))) +
  geom_density_ridges_gradient() +
  scale_fill_viridis() +
  theme_ridges() + 
  theme(legend.position = "none", 
        axis.title.y = element_blank()) +
  scale_y_discrete(limit = c("symptomatic", "pre_symptomatic", "control")) +
  labs(x = "log2(mRNA expression)") +
  facet_wrap(~ type)



# Write data --------------------------------------------------------------
ggsave(file = "Results/boxplots.png", 
       plot = log2_boxplots, 
       width = 6.77, 
       height = 2.83)

ggsave(file = "Results/ridgeline.png", 
       plot = ridgeline_random_comparison_plot, 
       width = 6.77, 
       height = 2.83) 

