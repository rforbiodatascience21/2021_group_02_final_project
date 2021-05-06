# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library(tidyverse)
library(patchwork)
library(ggplot2)


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

# Editing data to be more reader-friendly for plots and get non-duplicated rownames
marker_genes_wide <- borovecki_data_clean_aug_marker_genes %>%
  mutate(outcome = str_c(outcome," patient ",rownames(.))) %>% 
  mutate(outcome = str_replace_all(outcome, "_", "-"))

#Use the 12 random genes and rename them for plotting
random_genes_wide <- borovecki_data_clean_aug_random_genes %>% 
  rename_if(is.numeric, ~ paste("random:",.))

#Name marker genes as such and concatenate with marker genes
random_and_marker_genes_wide <- marker_genes_wide %>%
  rename_if(is.numeric, ~ paste("marker:",.)) %>% 
  cbind(random_genes_wide)
  
  
# Doing log2 transformation
marker_genes_long <- marker_genes_wide %>% 
  long_log2()

random_and_marker_genes_long <- random_and_marker_genes_wide %>% 
  long_log2()

#fold change giver vel ikke mening da vi ville have 3 elementer på 1 akse
#prøv
#burde jeg lave på Emilie's selected marker genes?
#we hope not

frst_foldchange_marker_genes <- marker_genes_wide %>%
  filter(
        str_detect(outcome, "control")
        ) %>% 
  select(where(is.numeric)) %>% 
  map_dbl(.,mean)

control_subset_marker_genes <- marker_genes_wide %>%
  filter(
    str_detect(outcome, "control")) %>% 
  pivot_longer(cols = -outcome,
               names_to = "gene",
               values_to = "expression")
  
foldchange_marker_genes <- marker_genes_wide %>%
  filter(
    str_detect(outcome, "control")) %>% 
  pivot_longer(cols = -outcome,
               names_to = "gene",
               values_to = "expression") %>% 
  group_by(gene) %>% 
  nest() %>% 
  ungroup() %>% 
  mutate(gene_mean = map(data, ~ mean(.$expression)))


foldchange_marker_genes <- marker_genes_wide  %>% 
  pivot_longer(cols = -outcome,
               names_to = "gene",
               values_to = "expression") %>% 
  group_by(gene) %>% 
  mutate(newdata = 
           expression/mean(
             control_subset_marker_genes %>% select(expression)
                          ))#expression needs to only be controls

# Visualise data ----------------------------------------------------------

ggplot(data = marker_genes_long, 
       mapping = aes(x = gene, 
                     y = fct_rev(outcome), 
                     fill = expression)) + 
  geom_tile() +
  theme(axis.text.x = element_text(angle = 50, hjust = 1)) + 
  scale_fill_gradient2(low = "red", high = "green", mid = "white", midpoint = 7)+
  ylab("Patients") +
  ggtitle("Heatmap over marker genes' expression level (Log2)")

ggplot(data = random_and_marker_genes_long, 
       mapping = aes(x = gene, 
                     y = fct_rev(outcome), 
                     fill = expression)) + 
  geom_tile() +
  theme(axis.text.x = element_text(angle = 50, hjust = 1)) + 
  scale_fill_gradient2(low = "red", high = "green", mid = "white", midpoint = 7)+
  ylab("Patients")+
  ggtitle("Heatmap over marker genes and 12 random genes' expression level (Log2)")



# Write data --------------------------------------------------------------
#ggsave(file = "") 