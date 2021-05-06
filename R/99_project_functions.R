#DATA STUFF FUNCTIONS-----------------------------------------------------------

#Pivot longer and log transform values
long_log2 <- function(data){
  return(data %>%
           pivot_longer(cols = -outcome, #Pivot all columns except outcome
                        names_to = "gene",
                        values_to = "expression") %>%
           mutate(expression = log2(expression))) #Log transform expression values
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
           select(where(is.numeric)) %>% # retain only numeric columns
           scale() %>% # scale data
           prcomp())
}


#Kmeans clustering
kmeans_func <- function(data){
  return(data %>%
           select(where(is.numeric)) %>% #Retain only numeric columns
           kmeans(centers = 3) %>% #Do kmeans with 3 centroids
           augment(data)) #Append cluster column to original data
}



#PLOTTING FUNCTIONS-------------------------------------------------------------

# Log2 fold change plot
log2_fold_change_plot <- function(data, plot_title){
  return(data %>%
           ggplot(mapping = aes(x = Gene, 
                              y = Log2_foldchange,
                              color = Significant_level,
                              shape = Marker_gene)) +
           geom_point(size = 2) +
           geom_hline(yintercept = 1.8, color = "blue", size = 1) + 
           geom_hline(yintercept = -1, color = "blue", size = 1) + 
           labs(x = "Gene", 
                y = "Log2 fold change", 
                title = plot_title,
                shape = "Marker gene") +
           scale_colour_manual(name = "Significant level", values = 
                                 c("grey54", "red", "green3")) + 
           theme(axis.text.x = element_blank(), axis.ticks = element_blank()) +
           geom_label_repel(aes(
                            label = Marker_gene_name), 
                            nudge_y = -0.2, 
                            size = 2.6, color = "black", 
                            min.segment.length = unit(0, 'lines'), 
                            max.overlaps = Inf))
  
}


#Point plot of kmeans clusters with PC1 and PC2 on the axes
kmeans_plot <- function(pca_fit_data, kmeans_data){
  return(pca_fit_data %>%
           augment(kmeans_data) %>% #Combine PC coordinates with original data
           ggplot(mapping = aes(x = .fittedPC1, y = .fittedPC2, colour = .cluster, shape = outcome)) +
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
