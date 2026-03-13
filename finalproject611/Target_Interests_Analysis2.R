#Target Interests: Using an Average of User Interests

#libraries:

library(tidyverse)
library(gridExtra)
library(grid)
library(randomForest)
library(caret)
library(rpart)
library(Polychrome)
library(pROC)

# dataloading:

ade <- read.csv('source_data/ad_events.csv')
ads <- read.csv('derived_data/ads_cleaned.csv') %>% select(-X)
camp <- read.csv('source_data/campaigns.csv')
users <- read.csv('derived_data/user_cleaned.csv') %>% select(-X)
comp1 <- read.csv('derived_data/composite_data1.csv') %>% select(-X)
comp2 <- read.csv('derived_data/composite_data2.csv') %>% select(-X)


# getting the data ready
user_interests <- c(
  "user_fitness", "user_health", "user_food", "user_lifestyle", "user_fashion",
  "user_news", "user_finance", "user_photography", "user_technology", 
  "user_travel", "user_gaming", "user_sports", "user_art"
)
target_interests <- gsub("user_", "target_", user_interests)

interests_data <- comp2 %>%
  mutate(across(all_of(target_interests), as.factor)) %>% 
  distinct(user_id, .keep_all = TRUE) %>% 
  group_by(ad_id, across(all_of(target_interests))) %>%
  summarize(across(all_of(user_interests), ~ mean(.x, na.rm = TRUE)))

interests_data

set.seed(123)

train_idx <- sample(seq_len(nrow(interests_data)), size = 0.8 * nrow(interests_data))

train_data <- interests_data[train_idx, ]
test_data  <- interests_data[-train_idx, ]

models <- list()

for (tcol in target_interests) {
  
  # Formula: target_x ~ user_interest1 + user_interest2 + ...
  f <- as.formula(
    paste(tcol, "~", paste(user_interests, collapse = " + "))
  )
  
  models[[tcol]] <- randomForest(
    formula = f,
    data = train_data,
    ntree = 100,
    mtry = 3,
    sampsize = c("0" = sum(train_data[[tcol]] == "1"), "1" = sum(train_data[[tcol]] == "1"))
  )
}

#predictions

preds <- lapply(target_interests, function(tcol) {
  predict(models[[tcol]], newdata = test_data)
})

preds <- as.data.frame(preds)
colnames(preds) <- target_interests

# evaluation:

truth <- test_data[target_interests]
conf_mats <- lapply(target_interests, function(tcol) {
  caret::confusionMatrix(
    data = factor(preds[[tcol]], levels=c(0,1)),
    reference = factor(truth[[tcol]], levels=c(0,1))
  )
})

names(conf_mats) <- target_interests

cm_to_df <- function(cm, label) {
  df <- as.data.frame(cm$table)   # Confusion matrix table
  colnames(df) <- c("Prediction", "Reference", "Freq")
  df$target <- label
  df
}

cm_dfs <- bind_rows(
  lapply(target_interests, function(tcol) {
    cm_to_df(conf_mats[[tcol]], tcol)
  })
)
cm_plots <- lapply(target_interests, function(tcol) {
  ggplot(filter(cm_dfs, target == tcol),
         aes(x = Reference, y = Prediction, fill = Freq)) +
    geom_tile(color = "white") +
    geom_text(aes(label = Freq), size = 6) +
    scale_fill_gradient(low = "#e6f0fa", high = "#4B97C9") +  # UNC light → dark blue
    labs(title = tcol, x = "Reference", y = "Prediction") +
    theme_bw(base_size = 11) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      axis.title = element_text(face = "bold"),
      panel.border = element_blank(),
      axis.line = element_line(color = "black"),
      legend.position = "none"
    )
})

do.call(grid.arrange, c(cm_plots, ncol = 4))

png("figures/average_interest_confusion_matrix_grid.png", width = 2000, height = 1500, res = 150)

# Plot the grid
do.call(grid.arrange, c(cm_plots, ncol = 4))

# Close the device
invisible(dev.off())

# to make the AUC plots

probs <- lapply(target_interests, function(tcol) {
  predict(models[[tcol]], newdata = test_data, type = "prob")[, "1"]
})
probs <- as.data.frame(probs)
colnames(probs) <- target_interests

# --- compute ROC objects
rocs <- lapply(target_interests, function(tcol) {
  roc(response = as.numeric(as.character(test_data[[tcol]])),
      predictor = probs[[tcol]],
      quiet = TRUE)
})
names(rocs) <- target_interests

# Convert ROCs to data frame for ggplot
roc_df <- bind_rows(
  lapply(target_interests, function(tcol) {
    r <- rocs[[tcol]]

    # Extract full ROC curve safely
    pts <- coords(r, x = "all", ret = c("specificity", "sensitivity"), transpose = FALSE)

    data.frame(
      tcol = tcol,
      fpr  = 1 - pts$specificity,
      tpr  = pts$sensitivity,
      auc  = as.numeric(auc(r))
    )
  })
)


my_colors <- palette36.colors(n = 13)
names(my_colors) <- target_interests

ggplot(roc_df, aes(x = fpr, y = tpr, color = tcol)) +
  geom_line(size = 1.3) +
  geom_abline(lty = 2, color = "gray50") +
  scale_color_manual(values = my_colors) +
  theme_bw(base_size = 14) +
  labs(
    title = "ROC Curves",
    x = "False Positive Rate",
    y = "True Positive Rate",
    color = "Model"
  ) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(face = "bold"),
    panel.border = element_blank(),
    axis.line = element_line(color = "black"),
    legend.position = "right"
  )


ggsave('figures/average_interest_ROC.png')

#So, it seems as though some of these models did do fairly well in terms of 
#using user interests to predict the target interests. Just out of curiosity, I 
#produced a variable importance plot for the target_travel model.

imp <- importance(models[['target_travel']])
imp_df <- data.frame(
  variable = rownames(imp),
  importance = imp[, "MeanDecreaseGini"]
)


ggplot(imp_df, aes(x = reorder(variable, importance), y = importance)) +
  geom_col(fill = "#4B9CD3") +  # Carolina Blue
  coord_flip() +
  theme_bw(base_size = 14) +
  labs(
    title = "Variable Importance: Target_Travel",
    x = "Variable",
    y = "Mean Decrease Gini"
  ) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(face = "bold"),
    panel.border = element_blank(),
    axis.line = element_line(color = "black")
  )


ggsave('figures/travel_varimpplot.png')

#This is a very interesting result. It seems as though, from the model, ads 
#targeting travel ended up attracting users with interest in fashion.