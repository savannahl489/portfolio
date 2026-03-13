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

users <- read.csv('derived_data/user_cleaned.csv') %>% dplyr::select(-X)

users[] <- lapply(users, as.factor)
users2 <- users %>% dplyr::select(-user_id, -location, -user_age)

#using entropy to find ideal number of clusters:
entropy <- function(x) {
  p <- prop.table(table(x))
  -sum(p * log(p), na.rm = TRUE)
}

entropy_k <- numeric()

for (k in 2:15) {
  set.seed(123)
  km <- kmodes(users2, modes = k)
  clusters <- km$cluster
  
  # compute entropy OF EACH CLUSTER
  cluster_entropies <- sapply(1:k, function(c) {
    # average entropy across all variables for this cluster
    mean(sapply(users2, function(var) entropy(var[clusters == c])))
  })
  
  # weight by cluster size
  sizes <- table(clusters)
  weighted_entropy <- sum(cluster_entropies * (sizes / sum(sizes)))
  
  entropy_k[k] <- weighted_entropy
}


# Plot
df_plot <- data.frame(
  k = 2:15,
  entropy = entropy_k[2:15]
)

ggplot(df_plot, aes(x = k, y = entropy)) +
  geom_line(color = "#4B9CD3", size = 1.3) +
  geom_point(size = 3, color = "#4B9CD3", fill = "#13294B", shape = 21) +
  theme_bw(base_size = 14) +
  labs(
    x = "k (Number of Clusters)",
    y = "Weighted Average Entropy",
    title = "Entropy vs Number of Clusters"
  ) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(face = "bold"),
    panel.border = element_blank(),
    axis.line = element_line(color = "black")
  )
ggsave('figures/entropy_plot.png')

# looks like 8 clusters is ideal
result <- kmodes(users2, modes = 8)

#Based on these results, I would say that the modes of the 8 clusters are 
#better. Now I will use t-SNE and Gower distance to visualize the results.



gower_dist <- daisy(users2, metric = "gower")

set.seed(123)  # for reproducibility

tsne_res <- Rtsne(as.matrix(gower_dist), 
                  is_distance = TRUE, 
                  perplexity = 30,   # typical starting point
                  max_iter = 1000)

# Assuming kmodes clustering with k=8
cluster_labels <- result$cluster  # your cluster assignments

tsne_df <- data.frame(
  Dim1 = tsne_res$Y[,1],
  Dim2 = tsne_res$Y[,2],
  Cluster = factor(cluster_labels)
)

# Base styling for all plots
base_theme <- theme_bw(base_size = 14) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(face = "bold"),
    panel.border = element_blank(),
    axis.line = element_line(color = "black"),
    legend.position = "right"
  )

# Extract colors from main plot
cluster_levels <- sort(unique(tsne_df$Cluster))
cluster_colors <- scales::hue_pal()(length(cluster_levels)) # default ggplot2 hues
names(cluster_colors) <- cluster_levels

p_main <- ggplot(tsne_df, aes(x = Dim1, y = Dim2, color = Cluster)) +
  geom_point(size = 1.8, alpha = 0.8) +
  scale_color_manual(values = cluster_colors) +
  labs(
    title = "t-SNE Visualization of 8 Clusters",
    x = "t-SNE Dimension 1",
    y = "t-SNE Dimension 2",
    color = "Cluster"
  ) +
  base_theme
cluster_plots <- lapply(cluster_levels, function(cl){
  ggplot(subset(tsne_df, Cluster == cl),
         aes(x = Dim1, y = Dim2, color = Cluster)) +
    geom_point(size = 1.2, alpha = 0.8) +
    scale_color_manual(values = cluster_colors) +
    labs(title = paste("Cluster", cl)) +
    base_theme +
    theme(legend.position = "none")   # hide legend for small plots
})

# Number your plots
# We'll assign numbers 1–8 for the surrounding plots, 9 for the main plot

layout_matrix <- rbind(
  c(1, 2, 3, 4),
  c(5, 9, 9, 6),
  c(7, 9, 9, 8)   # optional row if needed; can fill with NA
)

grobs_list <- c(cluster_plots, list(p_main))

widths  <- c(1, 1, 1, 1)  # middle columns twice as wide
heights <- c(1, 1, 1)  # middle rows twice as tall

g <- arrangeGrob(
  grobs = grobs_list,
  layout_matrix = layout_matrix,
  widths = widths,
  heights = heights
)

grid.draw(g)


png("figures/tsne_plot.png", width = 3000, height = 2000, res = 200)
grid.draw(g)
invisible(dev.off())
