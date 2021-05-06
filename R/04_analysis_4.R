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
  rename_if(is.numeric, ~ paste("random:",.)) %>% 
  select(-outcome)

#Name marker genes as such and concatenate with marker genes
random_and_marker_genes_wide <- marker_genes_wide %>%
  rename_if(is.numeric, ~ paste("marker:",.)) %>% 
  cbind(random_genes_wide)


# Preparing log2(foldchange) ####

#Finding the means of the control group
control_means_marker_genes <- marker_genes_wide %>%
  find_control_means

#dividing the matrix with the control group's means for each gene
foldchange_marker_genes <- marker_genes_wide %>%
  select(where(is.numeric)) %>% 
  mapply('/', . , control_means_marker_genes) %>% 
  cbind( . , 
         marker_genes_wide %>% select(outcome)
  )

#Finding the means of the control group
control_means_random_and_marker_genes <- random_and_marker_genes_wide %>%
  find_control_means

#dividing the matrix with the control group's means for each gene
foldchange_random_and_marker_genes <- random_and_marker_genes_wide %>%
  select(where(is.numeric)) %>% 
  mapply('/', . , control_means_random_and_marker_genes) %>% 
  cbind( . , 
         random_and_marker_genes_wide %>% select(outcome)
  )
  
# Doing log2 transformation
marker_genes_long <- marker_genes_wide %>% 
  long_log2()

random_and_marker_genes_long <- random_and_marker_genes_wide %>% 
  long_log2()

foldchange_marker_genes_long <- foldchange_marker_genes %>% 
  long_log2()

foldchange_random_and_marker_genes_long <- foldchange_random_and_marker_genes %>% 
  long_log2()

#control_subset_marker_genes <- marker_genes_wide %>%
#  filter(
#    str_detect(outcome, "control")) %>% 
#  pivot_longer(cols = -outcome,
#               names_to = "gene",
#               values_to = "expression")
  
#foldchange_marker_genes <- marker_genes_wide %>%
#  filter(
#    str_detect(outcome, "control")) %>% 
#  pivot_longer(cols = -outcome,
#               names_to = "gene",
#               values_to = "expression") %>% 
#  group_by(gene) %>% 
#  nest() %>% 
#  ungroup() %>% 
#  mutate(gene_mean = map(data, ~ mean(.$expression)))


#foldchange_marker_genes <- marker_genes_wide  %>% 
#  pivot_longer(cols = -outcome,
#               names_to = "gene",
#               values_to = "expression") %>% 
#  group_by(gene) %>% 
#  mutate(newdata = 
#           expression/mean(
#             control_subset_marker_genes %>% select(expression)
#                          ))#expression needs to only be controls

# Doing this for the whole dataset ####
all_genes_wide <- borovecki_data_clean_aug_all_genes %>% 
  mutate(outcome = str_replace_all(outcome, "_", "-"))

#Finding the means of the control group
control_means_all_genes <- all_genes_wide %>%
  find_control_means

#dividing the matrix with the control group's means for each gene
foldchange_all_genes <- all_genes_wide %>%
  select(where(is.numeric)) %>% 
  mapply('/', . , control_means_all_genes) %>% 
  cbind( . , 
         all_genes_wide %>% select(outcome)
  )

# Doing log2 transformation
foldchange_all_genes_long <- foldchange_all_genes %>% 
  long_log2() 
  #apply(.,sort,decreasing=F)
  #filter(B > mean(B, na.rm = TRUE))
  #select(where(colMeans(.)>1))

# Visualise data ----------------------------------------------------------

ggplot(data = marker_genes_long, 
       mapping = aes(x = gene, 
                     y = fct_rev(outcome), 
                     fill = expression)) + 
  geom_tile() +
  theme(axis.text.x = element_text(angle = 50, hjust = 1)) + 
  scale_fill_gradient2(low = "red", high = "green", mid = "white", midpoint = 7)+
  ylab("Patients") +
  xlab("Genes") +
  ggtitle("Heatmap over marker genes' expression level (Log2)")

ggplot(data = random_and_marker_genes_long, 
       mapping = aes(x = gene, 
                     y = fct_rev(outcome), 
                     fill = expression)) + 
  geom_tile() +
  theme(axis.text.x = element_text(angle = 50, hjust = 1)) + 
  scale_fill_gradient2(low = "red", high = "green", mid = "white", midpoint = 7)+
  ylab("Patients")+
  xlab("Genes") +
  ggtitle("Heatmap over marker genes and 12 random genes' expression level (Log2)")

ggplot(data = foldchange_marker_genes_long, 
       mapping = aes(x = gene, 
                     y = fct_rev(outcome), 
                     fill = expression)) + 
  geom_tile() +
  theme(axis.text.x = element_text(angle = 50, hjust = 1)) + 
  scale_fill_gradient2(low = "red", high = "green", mid = "white", midpoint = 0)+
  ylab("Patients") +
  xlab("Genes") +
  ggtitle("Heatmap over marker genes' expression level (Log2(FC))")

ggplot(data = foldchange_random_and_marker_genes_long, 
       mapping = aes(x = gene, 
                     y = fct_rev(outcome), 
                     fill = expression)) + 
  geom_tile() +
  theme(axis.text.x = element_text(angle = 50, hjust = 1)) + 
  scale_fill_gradient2(low = "red", high = "#0dff00", mid = "white", midpoint = 0)+
  ylab("Patients") +
  xlab("Genes") +
  ggtitle("Heatmap over marker genes' and 12 random genes' expression level (Log2(FC))")


ggplot(data = foldchange_all_genes_long, 
       mapping = aes(x = gene, 
                     y = fct_rev(outcome), 
                     fill = expression)) + 
  geom_tile() +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) + 
  scale_fill_gradient2(low = "red", high = "#0dff00", mid = "black", midpoint = 0)+
  ylab("Patients") +
  xlab("Genes") +
  ggtitle("Heatmap over all genes' expression level (Log2(FC))")



# Write data --------------------------------------------------------------
#ggsave(file = "") 