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

# we should not repeat the summing of the column to the control and symp df, find a way to do it in one step
control_df <- borovecki_data_clean_aug_all_genes %>%
  filter(outcome == "control") %>%
  select(everything(), -outcome) %>%
  rownames_to_column() %>%
  pivot_longer(-rowname, names_to = "Gene") %>% 
  pivot_wider(names_prefix = "Control_", names_from=rowname, values_from=value) %>%
  rowwise %>% 
  mutate(Control_sum = sum(c_across(where(is.numeric)))) %>%
  select(Gene, Control_sum, everything())
control_df

symp_df <- borovecki_data_clean_aug_all_genes %>%
  filter(outcome == "symptomatic" | outcome == "pre_symptomatic") %>%
  select(everything(), -outcome) %>%
  rownames_to_column() %>%
  pivot_longer(-rowname, names_to = "Gene") %>% 
  pivot_wider(names_prefix = "Patient_", names_from=rowname, values_from=value) %>%
  rowwise %>% 
  mutate(Patient_sum = sum(c_across(where(is.numeric)))) %>%
  select(Gene, Patient_sum, everything())
symp_df

marker_genes <- c("201012_at", "202653_s_at", "208374_s_at", "200989_at", 
                  "212287_at", "218589_at", "217816_s_at", "213044_at", 
                  "201071_x_at", "213168_at", "201023_at", "217783_s_at")

# Merge the two dataframes and compute the log2 fold change 
full_df <- full_join(symp_df, control_df, by = "Gene") %>%
  mutate(Log2_foldchange = log2(Patient_sum/Control_sum))  %>%
  mutate(Significant_level =  case_when(Log2_foldchange >= 1.8 ~ "Significantly up regulated",
                                        Log2_foldchange <= -1 ~ "Significantly down regulated",
                                        TRUE ~ "Non-significant")) %>%
  mutate(Marker_gene_name = case_when(is.element(Gene, marker_genes) ~ Gene,
                                      TRUE ~ "")) %>%
  select(Gene, Marker_gene_name, Patient_sum, Control_sum, Log2_foldchange, Significant_level, everything())
full_df




# Visualize data ----------------------------------------------------------
full_df %>%  
  ggplot(mapping = aes(x = Gene, 
                       y = Log2_foldchange,
                       color = Significant_level,
                       label = Marker_gene_name)) +
  geom_point() +
  geom_hline(yintercept = 1.8, color = "blue", size = 1) + 
  geom_hline(yintercept = -1, color = "blue", size = 1) + 
  labs(x = "Gene", 
       y = "Log2 fold change", 
       title = "Log2 fold change of expression data") +
  scale_colour_manual(name = "Significant level", values = c("grey54", "red", "green")) + 
  theme(axis.text.x = element_blank()) +
  #geom_point(aes(colour = Marker_gene_name))
  #geom_text(aes(label = Marker_gene_name, color = "blue"), size=3.1) 
  geom_text_repel(size = 3.1,  max.overlaps = Inf, color = "black")


# Write data --------------------------------------------------------------
#write_tsv(...)
#ggsave(...)
