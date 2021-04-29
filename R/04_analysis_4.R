# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library(tidyverse)
library(patchwork)
library(ggplo)


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------

# Load marker genes dataset
borovecki_data_clean_aug_marker_genes <- read_tsv(file = "data/03_borovecki_data_clean_aug_marker_genes.tsv")

# Load all genes dataset
borovecki_data_clean_aug_all_genes <- read_tsv(file = "data/03_borovecki_data_clean_aug_all_genes.tsv")



# Wrangle data ------------------------------------------------------------

# Editing data to be more reader-friendly for plots and get non-duplicated rownames
marker_genes_wide <- borovecki_data_clean_aug_marker_genes %>% 
  mutate(Patient = paste("Patient",rownames(.))) %>% 
  mutate(Patient = paste(Patient,outcome)) %>% 
  mutate(Patient = str_replace_all(Patient, "_", "-")) %>% 
  select(-outcome)

# Changing to long format
marker_genes_long <- marker_genes_wide %>% 
  pivot_longer(cols = -Patient,
               names_to = "Genes",
               values_to = "Expression")


# Visualise data ----------------------------------------------------------

ggplot(data = marker_genes_long, 
       mapping = aes(x = Genes, y = Patient, fill = Expression)) + 
  geom_tile() +
  theme(axis.text.x = element_text(angle = 50, hjust = 1)) + 
  scale_fill_gradient2(low = "green", high = "red", mid = "white", midpoint = 600)

#skal måske køres hvor vi udvælger fra den store tabel? like tager nogle random gener ud? 
#mest/mindst expressed genes? eller er de bare marker genes?


# Write data --------------------------------------------------------------
#ggsave(file = "") 