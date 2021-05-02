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



# Wrangle data ------------------------------------------------------------

# Editing data to be more reader-friendly for plots and get non-duplicated rownames
marker_genes_wide <- borovecki_data_clean_aug_marker_genes %>% 
  mutate(Patient = paste("patient",rownames(.))) %>% 
  mutate(Patient = paste(outcome,Patient)) %>% 
  mutate(Patient = str_replace_all(Patient, "_", "-")) %>% 
  select(-outcome)

#Extract 12 random genes 
set.seed(22)
random_genes <- borovecki_data_clean_aug_all_genes %>%
  select(-c(outcome, "201012_at", "202653_s_at", "208374_s_at", "200989_at",
            "212287_at", "218589_at", "217816_s_at", "213044_at",
            "201071_x_at", "213168_at", "201023_at", "217783_s_at")) %>% #Remove the 12 marker genes
  sample(size = 12) 

names(random_genes) <- paste("random:",names(random_genes))

random_and_marker_genes_wide <- marker_genes_wide %>%
  rename_if(is.numeric, ~ paste("marker:",.)) %>% 
  cbind(random_genes)
  
  
  
# Changing to long format
marker_genes_long <- marker_genes_wide %>% 
  pivot_longer(cols = -Patient,
               names_to = "Genes",
               values_to = "Expression")

random_and_marker_genes_long <- random_and_marker_genes_wide %>% 
  pivot_longer(cols = -Patient,
               names_to = "Genes",
               values_to = "Expression")
# Doing log2 transformation
marker_genes_long <- marker_genes_long %>% 
  mutate(Log2 = log2(Expression))

random_and_marker_genes_long <- random_and_marker_genes_long %>% 
  mutate(Log2 = log2(Expression))

#fold change giver vel ikke mening da vi ville have 3 elementer på 1 akse
#burde jeg lave på Emilie's selected marker genes?
#skal nok have lavet en funktion til at lave ens tabel lang og log transformeret


# Visualise data ----------------------------------------------------------

ggplot(data = marker_genes_long, 
       mapping = aes(x = Genes, y = fct_rev(Patient), fill = Log2)) + 
  geom_tile() +
  theme(axis.text.x = element_text(angle = 50, hjust = 1)) + 
  scale_fill_gradient2(low = "red", high = "green", mid = "white", midpoint = 7)+
  ylab("Patients")+
  ggtitle("Heatmap over marker genes' expression level (Log2)")

ggplot(data = random_and_marker_genes_long, 
       mapping = aes(x = Genes, y = fct_rev(Patient), fill = Log2)) + 
  geom_tile() +
  theme(axis.text.x = element_text(angle = 50, hjust = 1)) + 
  scale_fill_gradient2(low = "red", high = "green", mid = "white", midpoint = 7)+
  ylab("Patients")+
  ggtitle("Heatmap over marker genes and 12 random genes' expression level (Log2)")



# Write data --------------------------------------------------------------
#ggsave(file = "") 