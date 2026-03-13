# EDA: Over/Under Interactions + Deepdive
#libraries:

library(tidyverse)
library(gridExtra)
library(grid)
library(randomForest)

# dataloading:

ade <- read.csv('source_data/ad_events.csv')
ads <- read.csv('derived_data/ads_cleaned.csv') %>% select(-X)
camp <- read.csv('source_data/campaigns.csv')
users <- read.csv('derived_data/user_cleaned.csv') %>% select(-X)
comp1 <- read.csv('derived_data/composite_data1.csv') %>% select(-X)
comp2 <- read.csv('derived_data/composite_data2.csv') %>% select(-X)



# No NA values:
which(is.na(comp2))



## 0.1
  
#To answer this question, I wanted to first visualize a distribution of the 
#number of interactions per user.

interact_count <- comp2 %>% group_by(user_id) %>% summarise(count = n()) %>%
  left_join(users, by = 'user_id')

ggplot(interact_count, aes(x = count)) +
  geom_histogram(
    bins = 30, 
    fill = "#4B9CD3",    
    color = "white",  
    alpha = 0.9
  ) +
  labs(
    title = "Distribution of User Interaction Counts",
    x = "Interaction Count",
    y = "Number of Users"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5, color = "#003366"),
    axis.title = element_text(face = "bold", color = "#003366"),
    axis.text = element_text(color = "#333333"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90"),
    plot.background = element_rect(fill = "white", color = NA)
  )
ggsave("figures/interaction_count_histogram.png", width = 7, height = 5, dpi = 300)



#The histogram shows a huge range for the x axis, which makes me question why 
#when all the observations seem grouped to one side. Thus, I am making a boxplot
#of this data to see what in the world is going on.


ggplot(interact_count, aes(y = count)) +
  geom_boxplot(
    fill = "#4B9CD3",    # Carolina blue fill
    color = "#003366",   # dark navy border
    outlier.color = "#003366",
    outlier.alpha = 0.7,
    width = 0.3
  ) +
  labs(
    title = "Distribution of User Interaction Counts",
    subtitle = "Boxplot showing spread and outliers of user interaction counts",
    y = "Interaction Count",
    x = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(
      face = "bold", size = 16, hjust = 0.5, color = "#003366"
    ),
    plot.subtitle = element_text(
      hjust = 0.5, color = "#4B9CD3", size = 12
    ),
    axis.title.y = element_text(face = "bold", color = "#003366"),
    axis.text = element_text(color = "#333333"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90"),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

ggsave("figures/interaction_count_boxplot.png", width = 7, height = 5, dpi = 300)

#It looks like there are a handful of users who are highly interactive with 
#these ads. It may be wise to filter into these users and look at what 
#differentiates these users. I will look at four primary variables of potential 
#interest: gender, age, country and interests.

#Firstly, I've isolated out the specific datapoints that are outliers in the 
#following code:

# Compute quartiles and IQR
Q1 <- quantile(interact_count$count, 0.25)
Q3 <- quantile(interact_count$count, 0.75)
IQR <- Q3 - Q1

# Define lower and upper bounds
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Create a label for each observation
interact_count_outliers <- interact_count %>%
  mutate(outlier_label = 
           ifelse(count < lower_bound | 
                    count > upper_bound, "Outlier", "Normal")) %>% 
  filter(outlier_label == "Outlier")


#Then, I will split these outliers into whether they have overinteracted or 
#underinteracted.


over_interact <- interact_count_outliers %>% filter(count > 36)
under_interact <- interact_count_outliers %>% filter(count < 36)


### Gender

#Looking at gender, I mainly want to see if there is a significant difference in
#terms of count between the genders. I decided to use tables to show this:


over_interact_gender <- over_interact %>% group_by(user_gender) %>% 
  summarize(count_gender = n())

under_interact_gender <- under_interact %>% group_by(user_gender) %>% 
  summarize(count_gender = n())


og <- tableGrob(over_interact_gender)
png("figures/overinteract_gender.png", width = 200, height = 100)
grid.draw(og)
invisible(dev.off())

ug <- tableGrob(under_interact_gender)
png("figures/underinteract_gender.png", width = 200, height = 100)
grid.draw(ug)
invisible(dev.off())

### Age


ggplot(over_interact, aes(x = user_age)) +
  geom_histogram(
    bins = 30, 
    fill = "#4B9CD3",    
    color = "white",  
    alpha = 0.9
  ) +
  labs(
    title = "Distribution in Age of Over-Active Users",
    x = "Age",
    y = "Number of Users"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5, color = "#003366"),
    axis.title = element_text(face = "bold", color = "#003366"),
    axis.text = element_text(color = "#333333"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90"),
    plot.background = element_rect(fill = "white", color = NA)
  )
ggsave('figures/overinteract_age.png', width = 7, height = 5, dpi = 300)

ggplot(under_interact, aes(x = user_age)) +
  geom_histogram(
    bins = 10, 
    fill = "#4B9CD3",    
    color = "white",  
    alpha = 0.9
  ) +
  labs(
    title = "Distribution in Age of Under-Active Users",
    x = "Age",
    y = "Number of Users"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5, color = "#003366"),
    axis.title = element_text(face = "bold", color = "#003366"),
    axis.text = element_text(color = "#333333"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90"),
    plot.background = element_rect(fill = "white", color = NA)
  )
ggsave('figures/underinteract_age.png', width = 7, height = 5, dpi = 300)

### Country:

over_interact_country <- over_interact %>% group_by(country) %>% 
  summarize(count_country = n())

under_interact_country <- under_interact %>% group_by(country) %>% 
  summarize(count_country = n())

# to get proportion of users within each country:

interact_count_2 <- comp2 %>% group_by(user_id) %>% summarise(count = n()) %>%
  left_join(users, by = 'user_id') %>% group_by(country) %>% 
  summarize(total_country = n())

ooc <- over_interact_country %>% inner_join(interact_count_2, by = "country") %>%
  mutate(proportion = count_country / total_country)
uuc <- under_interact_country %>% inner_join(interact_count_2, by = "country") %>%
  mutate(proportion = count_country / total_country)

oc <- tableGrob(ooc)
png("figures/overinteract_country.png")
grid.draw(oc)
invisible(dev.off())

uc <- tableGrob(uuc)
png("figures/underinteract_country.png")
grid.draw(uc)
invisible(dev.off())


### Interests

interests <-c("user_fitness", "user_health", "user_food", "user_lifestyle", "user_fashion", 
              "user_news", "user_finance", "user_photography", "user_technology", "user_travel", 
              "user_gaming", "user_sports", "user_art")
under_interests_df <- data.frame(
  interests = names(colSums(under_interact[, interests])),
  count = colSums(under_interact[, interests])
)
over_interests_df <- data.frame(
  interests = names(colSums(over_interact[, interests])),
  count = colSums(over_interact[, interests])
)

ggplot(over_interests_df, aes(x = interests, y = count)) +
  geom_col(fill = "#4B9CD3",    
           color = "white",  
           alpha = 0.9
  ) +
  labs(
    title = "Distribution of Interests Among Over-Interactive Users",
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

ggsave("figures/overinteract_interests.png")

ggplot(under_interests_df, aes(x = interests, y = count)) +
  geom_col(fill = "#4B9CD3",    
           color = "white",  
           alpha = 0.9
  ) +
  labs(
    title = "Distribution of Interests Amongst Under-Interactive Users",
    x = "Interests",
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

ggsave("figures/underinteract_interests.png")