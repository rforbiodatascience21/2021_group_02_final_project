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

df <- borovecki_data_clean_aug_all_genes %>%
  mutate(Patient = paste("Patient", rownames(.))) %>% 
  mutate(Patient = paste(Patient, outcome)) %>% 
  select(Patient, everything(), -outcome) %>%
  pivot_longer(cols = -Patient, names_to = "Gene") %>%
  pivot_wider(names_from = Patient, values_from=value) %>%
  rowwise %>%
  mutate(Control_sum = sum(across(matches("control")))) %>%
  mutate(Patient_sum = sum(across(matches("symp")))) %>%
  mutate(Log2_foldchange = log2(Patient_sum/Control_sum))  %>%
  mutate(Significant_level = case_when(Log2_foldchange >= 1.8 ~ "Significantly up regulated",
                                       Log2_foldchange <= -1 ~ "Significantly down regulated",
                                       TRUE ~ "Non-significant")) 


# Find the marker genes 
marker_genes <- df %>%
    filter(Log2_foldchange > 2.8) %>%
    pull(Gene) 
marker_genes


# Annotate the marker genes in the large tibble
df <- df %>%
  mutate(Marker_gene = case_when(is.element(Gene, marker_genes) ~ "1",
                                 TRUE ~ "0")) %>%
  mutate(Marker_gene_name = case_when(is.element(Gene, marker_genes) ~ Gene,
                                      TRUE ~ "")) %>%
  select(Gene, Marker_gene, Marker_gene_name, Patient_sum, Control_sum, Log2_foldchange, Significant_level, everything())
  


their_marker_genes <- c("201012_at", "202653_s_at", "208374_s_at", "200989_at", 
                  "212287_at", "218589_at", "217816_s_at", "213044_at", 
                  "201071_x_at", "213168_at", "201023_at", "217783_s_at")

own_marker_genes <- c("221727_at", "221510_s_at", "219540_at", "219356_s_at",
                      "213941_x_at", "213701_at", "213111_at", "212286_at", 
                      "209649_at", "204286_s_at")




# Visualize data ----------------------------------------------------------
df %>%  
  ggplot(mapping = aes(x = Gene, 
                       y = Log2_foldchange,
                       color = Significant_level,
                       shape = Marker_gene)) +
  geom_point(size = 2) +
  geom_hline(yintercept = 1.8, color = "blue", size = 1) + 
  geom_hline(yintercept = -1, color = "blue", size = 1) + 
  labs(x = "Gene", 
       y = "Log2 fold change", 
       title = "Log2 fold change of expression data",
       shape = "Marker gene") +
  scale_colour_manual(name = "Significant level", values = c("grey54", 
                                                             "red", "green3")) + 
  theme(axis.text.x = element_blank(), axis.ticks = element_blank()) +
  geom_label_repel(aes(label = Marker_gene_name), nudge_y = -0.2, size = 2.6, 
                   color = "black", min.segment.length = unit(0, 'lines'))


# Write data --------------------------------------------------------------
#write_tsv(...)
#ggsave(...)
