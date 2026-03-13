# EDA: General Distributions

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

interests <-c("user_fitness", "user_health", "user_food", "user_lifestyle", "user_fashion", 
              "user_news", "user_finance", "user_photography", "user_technology", "user_travel", 
              "user_gaming", "user_sports", "user_art")

interact_sum_interests <- data.frame(
  interests = names(colSums(comp2[, interests])),
  total_count = colSums(comp2[, interests])
)
interact_sum_interests

ggplot(interact_sum_interests, aes(x = interests, y = total_count)) +
  geom_col(fill = "#4B9CD3",    
           color = "white",  
           alpha = 0.9
  ) +
  labs(
    title = "Distribution of Interests Among All Users",
    x = "Interest",
    y = "Number of Users"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5, color = "#003366"),
    axis.title = element_text(face = "bold", color = "#003366"),
    axis.text = element_text(color = "#333333"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90"),
    plot.background = element_rect(fill = "white", color = NA),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave("figures/interact_interests_total.png")

## Age Distributions
user_age_group <- users %>% group_by(age_group) %>% summarize(count = n())

ggplot(user_age_group, aes(x = age_group, y = count)) +
  geom_col(fill = "#4B9CD3",    
           color = "white",  
           alpha = 0.9
  ) +
  labs(
    title = "Distribution of User Age Groups",
    x = "Age Group",
    y = "Number of Users"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5, color = "#003366"),
    axis.title = element_text(face = "bold", color = "#003366"),
    axis.text = element_text(color = "#333333"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90"),
    plot.background = element_rect(fill = "white", color = NA),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave("figures/user_age_group_distr.png")