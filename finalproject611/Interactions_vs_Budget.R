#Interactions vs Budget

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

#3) How does budget factor into the campaign? does higher budget = more people 
#of that interest interacting with ads? Computation of a ratio of budget per
#day???
  

ib_df <- comp2 %>% group_by(campaign_id, total_budget, duration_days) %>% 
  summarize(interactions = n()) %>% 
  mutate(interactions_per_day = interactions/duration_days,
         budget_per_day = total_budget/duration_days)
ib_df

unc_blue   <- "#4B9CD3"   # Carolina Blue
unc_navy   <- "#13294B"   # Navy
unc_gray   <- "#F2F2F2"   # Light gray background

theme_unc <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.background  = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = unc_gray, color = NA),
      panel.grid.major = element_line(color = "#D0D0D0"),
      panel.grid.minor = element_line(color = "#E5E5E5"),
      axis.title       = element_text(color = unc_navy, face = "bold"),
      axis.text        = element_text(color = unc_navy),
      plot.title       = element_text(color = unc_navy, face = "bold", hjust = 0.5),
      
      # legend styling
      legend.background = element_rect(fill = "white", color = NA),
      legend.key        = element_rect(fill = unc_gray, color = NA),
      legend.title      = element_text(color = unc_navy, face = "bold"),
      legend.text       = element_text(color = unc_navy)
    )
}

ggplot(ib_df, aes(x = budget_per_day, 
                  y = interactions_per_day, 
                  color = as.numeric(duration_days))) +
  geom_point(size = 3, alpha = 0.8) +
  scale_color_gradient(
    name = "Duration (days)",
    low = unc_blue,
    high = unc_navy
  ) +
  labs(
    title = "Scatterplot of Interactions Per Day vs Budget Per Day",
    x = "Budget per Day",
    y = "Interactions per Day"
  ) +
  theme_unc()

ggsave('figures/interactions_vs_budget.png')

theme_unc2 <- function() {
  theme_minimal(base_size = 14) +
    theme(
      axis.title       = element_text(color = unc_navy, face = "bold"),
      axis.text        = element_text(color = unc_navy),
      plot.title       = element_text(color = unc_navy, face = "bold", hjust = 0.5),                  # Dark navy
      panel.grid.major = element_line(color = "gray85"),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
}

camp_ad_count <- comp2 %>% distinct(campaign_id, ad_id) %>% 
  group_by(campaign_id) %>% summarize(ads = n())
ggplot(camp_ad_count, aes(x = ads)) +
  geom_histogram(binwidth = 2,
                 fill = "#4B9CD3",      # Carolina Blue
                 color = "#003366") +   # Navy outline
  labs(
    title = "Ad Count in Campaigns Distribution",
    x = "Ad Count",
    y = "Count"
  ) +
  theme_unc2()

ggsave('figures/campaign_ad_count.png')
