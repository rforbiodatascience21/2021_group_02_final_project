## PCA

# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")
library(broom)
library(cowplot)


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------

# Load marker gene dataset
borovecki_data_clean_aug_marker_genes <- read_tsv(file = "data/03_borovecki_data_clean_aug_marker_genes.tsv")

# Load all genes dataset
borovecki_data_clean_aug_all_genes <- read_tsv(file = "data/03_borovecki_data_clean_aug_all_genes.tsv")


# Wrangle data ------------------------------------------------------------

# Model data


pca_fit_m <- borovecki_data_clean_aug_marker_genes %>% 
  select(where(is.numeric)) %>% # retain only numeric columns
  scale() %>% # scale data
  prcomp() # do PCA


pca_fit_a <- borovecki_data_clean_aug_all_genes %>% 
  select(where(is.numeric)) %>% # retain only numeric columns
  scale() %>% # scale data
  prcomp() # do PCA

# Visualise data ----------------------------------------------------------

# Plot PCA

pca_fit_m %>%
  augment(borovecki_data_clean_aug_marker_genes) %>% # add original dataset back in
  ggplot(aes(.fittedPC1, .fittedPC2, color = outcome)) + 
  geom_point(size = 1.5) +
  scale_color_manual(
    values = c(symptomatic = "#D55E00", pre_symptomatic = "#00FF00", control = "#0072B2")
  ) +
  theme_half_open(12) + background_grid()

pca_fit_a %>%
  augment(borovecki_data_clean_aug_all_genes) %>% # add original dataset back in
  ggplot(aes(.fittedPC1, .fittedPC2, color = outcome)) + 
  geom_point(size = 1.5) +
  scale_color_manual(
    values = c(symptomatic = "#D55E00", pre_symptomatic = "#00FF00", control = "#0072B2")
  ) +
  theme_half_open(12) + background_grid()

---
  
  pca_fit_m %>%
  tidy(matrix = "eigenvalues") %>%
  ggplot(aes(PC, percent)) +
  geom_col(fill = "#56B4E9", alpha = 0.8) +
  scale_x_continuous(breaks = 1:9) +
  scale_y_continuous(
    labels = scales::percent_format(),
    expand = expansion(mult = c(0, 0.01))
  ) +
  theme_minimal_hgrid(12)

pca_fit_a %>%
  tidy(matrix = "eigenvalues") %>%
  ggplot(aes(PC, percent)) +
  geom_col(fill = "#56B4E9", alpha = 0.8) +
  scale_x_continuous(breaks = 1:9) +
  scale_y_continuous(
    labels = scales::percent_format(),
    expand = expansion(mult = c(0, 0.01))
  ) +
  theme_minimal_hgrid(12)

---
# plot rotation matrix for marker genes
  
arrow_style <- arrow(
  angle = 20, ends = "first", type = "closed", length = grid::unit(8, "pt")
)


pca_fit_m %>%
  tidy(matrix = "rotation") %>%
  pivot_wider(names_from = "PC", names_prefix = "PC", values_from = "value") %>%
  ggplot(aes(PC1, PC2)) +
  geom_segment(xend = 0, yend = 0, arrow = arrow_style) +
  geom_text(
    aes(label = column),
    hjust = 1, nudge_x = -0.02, 
    color = "#904C2F"
  ) +
  xlim(-1.25, .5) + ylim(-.5, 1) +
  coord_fixed() + # fix aspect ratio to 1:1
  theme_minimal_grid(12)



---









# Write data --------------------------------------------------------------
#write_tsv(...)
#ggsave(...)