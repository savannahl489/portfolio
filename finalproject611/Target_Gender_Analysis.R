# Do ads/ campaigns draw in the genders they want to draw in?

#libraries:

library(tidyverse)
library(gridExtra)
library(grid)

# dataloading:

ade <- read.csv('source_data/ad_events.csv')
ads <- read.csv('derived_data/ads_cleaned.csv') %>% select(-X)
camp <- read.csv('source_data/campaigns.csv')
users <- read.csv('derived_data/user_cleaned.csv') %>% select(-X)
comp1 <- read.csv('derived_data/composite_data1.csv') %>% select(-X)
comp2 <- read.csv('derived_data/composite_data2.csv') %>% select(-X)

# to get the gender:
target_gender_count <- tableGrob(head(comp2 %>% 
                                   group_by(ad_id, target_gender) %>% 
                                   summarize(
                                     count_female = sum(user_gender == "Female",
                                                        na.rm = TRUE),
                                     count_male   = sum(user_gender == "Male",  
                                                        na.rm = TRUE),
                                     count_other = sum(user_gender == "Other", 
                                                       na.rm = TRUE)
                                   )))
png("figures/targetvsuser_gender_table.png", width = 450, height = 200)
grid.draw(target_gender_count)
invisible(dev.off())


#It seems like consistently, whether or not the add targeted males or females, 
#males interacted a lot more in comparison to that of females. 
#This might be attributed to the distribution in gender amongst the observed 
#users-- males might just happen to be the majority within the dataset. We saw 
#this earlier in the outlier deepdive as a possible cause. Just to examine this 
#distribution:
  
gender_table <- tableGrob(users %>% group_by(user_gender) %>% tally())
gender_table
png("figures/gender_table.png", width = 200, height = 100)
grid.draw(gender_table)
invisible(dev.off())


#So, it does seem as though there is a pretty large difference in terms of 
#counts of users among the three categories of gender. Thus, it may make more 
#sense for us to look at the proportion of users interacting with these ads when
#looking at gender. Wait... I had almost forgotten, users can likely interact 
#with the same ad multiple times. It might be wise to look at both unique users 
#interacting with the add and total interactions regardless of whether users 
#interacted with an add more than once.

#Here is the plan: I will endeavor to make two plots: X axis as proportion of 
#males, Y axis as proportion of females, legend as the target gender For each 
#version of the above (unique users vs just proportion of interactions)


# unique user visualization:
# to get data ready:
user_gender_count <- users %>% group_by(user_gender) %>% tally() %>% pull(n)
tguu <- comp2 %>% 
  group_by(ad_id, target_gender, user_id, user_gender) %>% 
  group_by(ad_id, target_gender) %>% 
  summarize(
    prop_female = sum(user_gender == "Female", na.rm = TRUE)/user_gender_count[1],
    prop_male   = sum(user_gender == "Male",   na.rm = TRUE)/user_gender_count[2],
    prop_other = sum(user_gender == "Other", na.rm = TRUE)/user_gender_count[3]
  )

# looking at target interest categories:
unique(tguu$target_gender)

# Because of these categories, for now, I will eliminate the "Other" user gender
#category, and just focus on male, female in terms of proportions

ggplot(tguu, aes(
  x = prop_male,
  y = prop_female,
  color = target_gender
)) +
  geom_point(size = 3, alpha = 0.8) +
  scale_color_manual(
    values = c(
      "Male"   = "#13294B",  # UNC Navy
      "Female" = "#4B9CD3",  # Carolina Blue
      "All"    = "#999999"   # Gray
    )
  ) +
  theme_bw(base_size = 14) +
  labs(
    title = "Proportion Male vs Female",
    x = "Proportion Male",
    y = "Proportion Female",
    color = "Target Gender"
  ) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(face = "bold"),
    panel.border = element_blank(),
    axis.line = element_line(color = "black"),
    legend.position = "right"
  )

ggsave("figures/target_gender_vs_user_gender.png", width = 7, height = 5, dpi = 300)

#In general, I think based on individual users and their distribution in gender,
#no one gender is proportionally speaking overinteracting with any one add in 
#any overwhelming amount. Given this indication, I think it really isn't all 
#that worth it in terms of the potential for it to be interesting to look at the
#other figure-- where I am looking at the total sum of interactions rather than 
#just unique users.