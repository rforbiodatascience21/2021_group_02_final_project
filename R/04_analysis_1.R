# LOG 2 FOLD CHANGE PLOT FOR SELECTING MARKER GENES

# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")
library("ggrepel")



# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
borovecki_data_clean_aug_all_genes <- read_tsv(file = "data/03_borovecki_data_clean_aug_all_genes.tsv")


# Wrangle data ------------------------------------------------------------

# Transform tibble so each row is one gene and each column is a patient. Compute
# The mean for each gene stratified on patient/control-status. Compute Log2 fold
# change and significant level. 
borovecki_data_per_gene <- borovecki_data_clean_aug_all_genes %>%
  mutate(Patient = str_c("Patient_", row_number(), "_", outcome)) %>%
  select(Patient, everything(), -outcome) %>%
  pivot_longer(cols = -Patient, 
               names_to = "Gene", 
               values_to = "Value") %>%
  pivot_wider(id_cols = "Gene", 
              names_from = "Patient", 
              values_from = "Value") %>%
  mutate(Control_mean = rowMeans(across(contains("control")))) %>%
  mutate(Patient_mean = rowMeans(across(contains("symp")))) %>%
  mutate(Log2_foldchange = log2(Patient_mean/Control_mean))  %>%
  mutate(Significant_level = case_when(Log2_foldchange >= 1.8 ~ "Significantly upregulated",
                                       Log2_foldchange <= -1 ~ "Significantly downregulated",
                                       TRUE ~ "Non-significant")) %>%
  select(Gene, Patient_mean, Control_mean, Log2_foldchange, Significant_level, everything())


# Find the marker genes, using significant level >2.4, write the result to file
own_marker_genes <- borovecki_data_per_gene %>%
  top_n(12, wt = Log2_foldchange) %>%
  pull(Gene)



# Define the marker genes used in the paper
paper_marker_genes <- c("201012_at", "202653_s_at", "208374_s_at", "200989_at", 
                        "212287_at", "218589_at", "217816_s_at", "213044_at", 
                        "201071_x_at", "213168_at", "201023_at", "217783_s_at")

# Annotate the marker genes in the large tibble
own_marker_genes_tibble <- add_marker_genes_to_tibble(
  borovecki_data_per_gene, own_marker_genes)
  
paper_marker_genes_tibble <- add_marker_genes_to_tibble(
  borovecki_data_per_gene, paper_marker_genes)


# Visualize data ----------------------------------------------------------

# Visualize using our own selected marker genes
own_log2_fold_change_plot <- log2_fold_change_plot(
          data = own_marker_genes_tibble, 
          plot_title = "Log2 fold change of expression data - own marker genes",
          pre_dif_y_nodge = 0.5)

# Visualize using the marker genes from the paper
paper_log2_fold_change_plot <- log2_fold_change_plot(
          data = paper_marker_genes_tibble, 
          plot_title = "Log2 fold change of expression data - paper marker genes",
          pre_dif_y_nodge = 0)


# Write data --------------------------------------------------------------

write_lines(own_marker_genes, file = "data/own_marker_genes.txt")

ggsave(file = "Results/own_log2_fold_change.png", 
       plot = own_log2_fold_change_plot, 
       width = 25, 
       height= 14, 
       unit = "cm")
ggsave(file = "Results/paper_log2_fold_change.png", 
       plot = paper_log2_fold_change_plot, 
       width = 22, 
       height= 14, 
       unit = "cm") 
