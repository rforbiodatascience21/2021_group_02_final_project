# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


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


# Merge the two dataframes and compute the log2 fold change 
full_df <- full_join(symp_df, control_df, by = "Gene") %>%
  mutate(Log2_foldchange = log2(Patient_sum/Control_sum))  %>%
  mutate(Significant_level =  case_when(Log2_foldchange >= 1.8 ~ "Significantly upregulated",
                                        Log2_foldchange <= -1 ~ "Significantly downregulated",
                                        TRUE ~ "Non-significant")) %>%
  select(Gene, Patient_sum, Control_sum, Log2_foldchange, Significant_level, everything())




# Visualise data ----------------------------------------------------------
full_df %>%  
  ggplot(mapping = aes(x = Gene, 
                       y = Log2_foldchange,
                       color = Significant_level)) +
  geom_point() +
  geom_hline(yintercept = 1.8, color = "blue", size = 1) + 
  geom_hline(yintercept = -1, color = "blue", size = 1) + 
  labs(x = "Gene", 
       y = "Log2 fold change", 
       title = "Log2-fold change of expression data") +
  scale_colour_manual(name = "Significant level", values = c("black", "red", "green")) + 
  theme(axis.text.x = element_blank())


# Write data --------------------------------------------------------------
#write_tsv(...)
#ggsave(...)