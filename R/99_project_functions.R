#DATA WRANGLING FUNCTIONS-------------------------------------------------------

#Pivot longer and log transform values
long_log2 <- function(data){
  return(data %>%
           pivot_longer(cols = -outcome, 
                        names_to = "gene",
                        values_to = "expression") %>%
           mutate(expression = log2(expression))) 
}

# Find marker genes
find_marker_genes <- function(data, significant_level){
  return(data %>%
           filter(Log2_foldchange > significant_level) %>%
           pull(Gene))
}

find_control_means <- function(data){
  return(data %>%
           filter(str_detect(outcome, "control")) %>% 
           select(where(is.numeric)) %>% 
           map_dbl(mean))
}


# Add marker genes to tibble
add_marker_genes_to_tibble <- function(data, marker_genes){
  return(data %>%
           mutate(Marker_gene = case_when(is.element(Gene, marker_genes) ~ "1",
                                          TRUE ~ "0")) %>%
           mutate(Marker_gene_name = case_when(is.element(Gene, marker_genes) ~ Gene,
                                               TRUE ~ "")) %>%
           select(Gene, Marker_gene, Marker_gene_name, Patient_mean, Control_mean, 
                  Log2_foldchange, Significant_level, everything()))
} 



#MODELLING FUNCTIONS------------------------------------------------------------

#PCA analysis
pca_fit <- function(data){
  return(data %>% 
           select(where(is.numeric)) %>%
           scale() %>%
           prcomp())
}


#Kmeans clustering
kmeans_func <- function(data){
  return(data %>%
           select(where(is.numeric)) %>%
           kmeans(centers = 3) %>%
           augment(data))
}



#PLOTTING FUNCTIONS-------------------------------------------------------------

# Log2 fold change plot
log2_fold_change_plot <- function(data, plot_title, pre_dif_y_nodge){
  return(data %>%
           ggplot(mapping = aes(x = Gene, 
                                y = Log2_foldchange,
                                color = Marker_gene,
                                fill = Significant_level),show_guide=F) +
           geom_point(size = 2,
                      stroke = 1,
                      shape = 21) +
           geom_hline(yintercept = 1.8, 
                      color = "blue", 
                      size = 1) + 
           geom_hline(yintercept = -1, 
                      color = "blue", 
                      size = 1) +
           labs(x = "Gene", 
                y = "Log2 fold change", 
                title = plot_title,
                shape = "Marker gene") +
           theme(axis.text.x = element_blank(), 
                 axis.ticks = element_blank()) +
           geom_label_repel(aes(label = Marker_gene_name), 
                            nudge_y = pre_dif_y_nodge, 
                            size = 2.6, 
                            color = "black", 
                            min.segment.length = unit(0, 'lines'), 
                            max.overlaps = Inf) +
           guides(fill = guide_legend(override.aes = aes(label = ""))) +
           scale_fill_manual(name = "Significance level", 
                             values = c("Non-significant" = "grey54", 
                                        "Significantly downregulated" = "red", 
                                        "Significantly upregulated" = "green3")) +
           scale_color_manual(name = "Marker gene", 
                              values = c("0"="#00000000",
                                         "1" = "black")))
}


#Point plot of kmeans clusters with PC1 and PC2 on the axes
kmeans_plot <- function(pca_fit_data, kmeans_data){
  return(pca_fit_data %>%
           augment(kmeans_data) %>% #Combine PC coordinates with original data
           ggplot(mapping = aes(x = .fittedPC1, 
                                y = .fittedPC2, 
                                colour = .cluster, 
                                shape = outcome)) +
           geom_point(size = 2) +
           scale_colour_viridis(discrete = TRUE) +
           theme_half_open(12) + 
           background_grid() +
           labs(x = "PC1", 
                y = "PC2",
                colour = "Kmeans cluster",
                shape = "Actual cluster"))
}


#Boxplot of expression
boxplot_func <- function(long_data){
  return(long_data %>%
           ggplot(mapping = aes(x = outcome, 
                                y = expression, 
                                fill = outcome)) +
           geom_boxplot(alpha = 0.7) +
           scale_fill_viridis(discrete = TRUE) +
           theme(legend.position = "none") +
           xlab("Outcome"))
}

pca_plot <- function(data){
  return(pca_fit(data)  %>% 
           augment(data) %>% 
           ggplot(aes(x = .fittedPC1, y = .fittedPC2, color = outcome)) + 
           geom_point(size = 1.5) +
           scale_color_manual(
             values = c(symptomatic = "#D55E00", pre_symptomatic = "#00FF00", control = "#0072B2")
           ) +
           theme_half_open(font_size = 12) + 
           background_grid()+
           xlab("PC1") + ylab("PC2"))
}

variance_plot <- function(data){
  return(pca_fit(data) %>%
           tidy(matrix = "eigenvalues") %>%
           ggplot(aes(PC, percent)) +
           geom_col(fill = "#56B4E9", alpha = 0.8) +
           scale_x_continuous(breaks = 1:9) +
           scale_y_continuous(
             labels = scales::percent_format(),
             expand = expansion(mult = c(0, 0.01))
           ) +
           theme_minimal_hgrid(12))
}
