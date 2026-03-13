# I want to do hierarchical clustering on the users. I will proceed with agglomerative clustering first

#libraries:
library(tidyverse)
library(gridExtra)
library(grid)
library(randomForest)
library(caret)
library(rpart)
library(Polychrome)
library(pROC)
library(klaR)
library(FactoMineR)
library(factoextra)
library(Rtsne)
library(cluster)

# Data Prep
users <- read.csv('derived_data/user_cleaned.csv') %>% dplyr::select(-X)
users[] <- lapply(users, as.factor)
users2 <- users %>% dplyr::select(-user_id, -location, -user_age)

# Compute dissimilarity matrix
gower_dist <- daisy(users2, metric = "gower")

# Hierarchical clustering
hc <- hclust(gower_dist, method = "average")  

#use of silhouette statistic to get number of clusters
sil_width <- c()
for(k in 2:20){
  clusters <- cutree(hc, k)  # hierarchical on sample
  sil <- silhouette(clusters, gower_dist)
  sil_width[k] <- mean(sil[, 3])
}

df_sil <- data.frame(
  k = 2:20,
  sil = sil_width[2:20]
)

# UNC Carolina blue
unc_blue <- "#4B9CD3"

ggplot(df_sil, aes(x = k, y = sil)) +
  geom_line(color = "#4B9CD3", size = 1.3) +
  geom_point(size = 3, color = "#4B9CD3", fill = "#13294B", shape = 21) +
  theme_bw(base_size = 14) +
  labs(
    x = "Number of Clusters",
    y = "Average Silhouette Width",
    title = "Silhouette Width vs Number of Clusters"
  ) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(face = "bold"),
    panel.border = element_blank(),
    axis.line = element_line(color = "black")
  )
ggsave('figures/silhouette_width.png')

#13 clusters is best
clusters_13 <- cutree(hc, k = 13)

tsne_res <- Rtsne(
  as.matrix(gower_dist),
  is_distance = TRUE,
  perplexity = 30
)

tsne_df <- data.frame(
  Dim1 = tsne_res$Y[,1],
  Dim2 = tsne_res$Y[,2],
  cluster = factor(clusters_13)
)
# Base theme
base_theme <- theme_bw(base_size = 14) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(face = "bold"),
    panel.border = element_blank(),
    axis.line = element_line(color = "black"),
    legend.position = "right"
  )

# Extract colors without using scales library
# Cluster levels
cluster_levels <- sort(unique(tsne_df$cluster))
cluster_colors <- grDevices::hcl.colors(length(cluster_levels), palette = "Dark3")
names(cluster_colors) <- cluster_levels

# Generate small plots
cluster_plots <- lapply(cluster_levels, function(cl){
  ggplot(subset(tsne_df, cluster == cl),
         aes(x = Dim1, y = Dim2, color = cluster)) +
    geom_point(size = 1.2, alpha = 0.8) +
    scale_color_manual(values = cluster_colors) +
    labs(title = paste("Cluster", cl)) +
    base_theme +
    theme(legend.position = "none")
})


# Main t-SNE plot
p_main <- ggplot(tsne_df, aes(x = Dim1, y = Dim2, color = cluster)) +
  geom_point(size = 1.8, alpha = 0.8) +
  scale_color_manual(values = cluster_colors) +
  labs(
    title = "t-SNE Visualization of 13 Clusters",
    x = "t-SNE Dimension 1",
    y = "t-SNE Dimension 2",
    color = "Cluster"
  ) +
  base_theme


# 4×5 layout
layout_matrix <- rbind(
  c( 1,  2,  3,  4,  5),
  c( 6,  7, 14, 14,  8),
  c( 9, 10, 14, 14, 11),
  c(12, 13, NA, NA, NA)
)

# Combine grobs (1–13 = clusters; 14 = main plot)
grobs_list <- c(cluster_plots, list(p_main))

g <- arrangeGrob(
  grobs = grobs_list,
  layout_matrix = layout_matrix,
  widths  = c(1, 1, 1.3, 1.3, 1),
  heights = c(1, 1.2, 1.2, 1)
)

grid::grid.draw(g)

png("figures/tsne_aggclus.png", width = 3000, height = 2000, res = 200)
grid::grid.draw(g)
invisible(dev.off())
