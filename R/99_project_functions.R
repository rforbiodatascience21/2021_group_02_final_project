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

#Point plot of kmeans clusters with PC1 and PC2 on the axes
kmeans_plot <- function(pca_fit_data, kmeans_data){
  return(pca_fit_data %>%
           augment(kmeans_data) %>% #Combine PC coordineates with original data
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


#Boxplot of log2 expression
log2_boxplot <- function(data){
  return(data %>%
           pivot_longer(cols = -outcome, #Pivot all columns except outcome
                        names_to = "gene",
                        values_to = "expression") %>%
           mutate(expression = log2(expression)) %>% #Log transform expression values 
           ggplot(mapping = aes(x = outcome, 
                                y = expression, 
                                fill = outcome)) +
           geom_boxplot(alpha = 0.7) +
           scale_fill_viridis(discrete = TRUE) +
           theme(legend.position = "none") +
           labs(x = "Outcome", 
                y = "Log2 expression (Unit??)"))
}