# HEATMAP OF LOG 2 FOLD CHANGE FOR MARKER GENES AND RANDOM GENES

# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(patchwork)


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------

# Load marker genes dataset
borovecki_data_clean_aug_marker_genes <- read_tsv(file = "data/03_borovecki_data_clean_aug_marker_genes.tsv")

# Load all genes dataset
borovecki_data_clean_aug_all_genes <- read_tsv(file = "data/03_borovecki_data_clean_aug_all_genes.tsv")

# Load random genes dataset 
borovecki_data_clean_aug_random_genes <- read_tsv(file = "data/03_borovecki_data_clean_aug_random_genes.tsv")


# Wrangle data ------------------------------------------------------------

# Editing data to be more reader-friendly for plots and get non-duplicated rownames
marker_genes_wide <- borovecki_data_clean_aug_marker_genes %>%
  mutate(outcome = str_c(outcome," patient ",rownames(.))) %>% 
  mutate(outcome = str_replace_all(outcome, "_", "-"))

# Use the 12 random genes and rename them
random_genes_wide <- borovecki_data_clean_aug_random_genes %>% 
  mutate(outcome = str_c(outcome," patient ",rownames(.))) %>% 
  mutate(outcome = str_replace_all(outcome, "_", "-"))



# Preparing log2(foldchange) ####

# Finding the means of the control group
control_means_marker_genes <- marker_genes_wide %>%
  find_control_means(.)

# Dividing the matrix with the control group's means for each gene 
# and put outcome back
foldchange_marker_genes <- marker_genes_wide %>%
  select(where(is.numeric)) %>% 
  mapply('/', . , control_means_marker_genes) %>% 
  cbind( . , 
         marker_genes_wide %>% select(outcome)
        )


# Finding the means of the control group
control_means_random_genes <- random_genes_wide %>%
  find_control_means(.)

# Dividing the matrix with the control group's means for each gene 
# and putting outcome back
foldchange_random_genes <- random_genes_wide %>%
  select(where(is.numeric)) %>% 
  mapply('/', . , control_means_random_genes) %>% 
  cbind( . , 
         random_genes_wide %>% select(outcome)
       )
  

# Doing log2 transformation
foldchange_marker_genes_long <- foldchange_marker_genes %>% 
  long_log2(.)

foldchange_random_genes_long <- foldchange_random_genes %>%
  long_log2(.)




# Visualise data ----------------------------------------------------------


markerPlotOnly <- ggplot(data = foldchange_marker_genes_long, 
       mapping = aes(x = gene, 
                     y = fct_rev(outcome), 
                     fill = expression)) + 
  geom_tile() +
  theme(axis.text.x = element_text(angle = 50, hjust = 1)) + 
  scale_fill_gradient2(low = "red", high = "green", mid = "white", midpoint = 0)+
  ylab("Patients") +
  xlab("Genes") +
  labs(fill = "Log2(foldchange)")+
  ggtitle("Marker genes' expression level")


randomPlotOnly <- ggplot(data = foldchange_random_genes_long, 
                         mapping = aes(x = gene, 
                                       y = fct_rev(outcome), 
                                       fill = expression)) + 
  geom_tile() +
  theme(axis.text.x = element_text(angle = 50, hjust = 1)) + 
  scale_fill_gradient2(low = "red", high = "green", mid = "white", midpoint = 0)+
  ylab("Patients") +
  xlab("Genes") +
  labs(fill = "Log2(foldchange)")+
  ggtitle("Random genes' expression level")


gatheredPlot <- markerPlotOnly + 
  randomPlotOnly + theme(legend.position = "none",
                         axis.text.y = element_blank(), 
                        axis.ticks.y = element_blank(),
                        axis.title.y = element_blank())+
  plot_layout(guides = "collect")+
  plot_annotation(title = "Heatmap over marker genes' and 12 random genes' expression level (Log2(FC))")


markerPlot <- markerPlotOnly +
  ggtitle("Heatmap over marker genes' expression level (Log2(FC))")

# Write data --------------------------------------------------------------
ggsave(file = "Results/heatmap_log2_fold_change_marker_genes.png", 
       plot = markerPlot, 
       width = 26, height= 14, unit = "cm")

ggsave(file = "Results/heatmap_log2_fold_change_random_and_marker_genes.png", 
       plot = gatheredPlot, 
       width = 26, height= 14, unit = "cm")
