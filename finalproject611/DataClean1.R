#libraries:

library(tidyverse)

# dataloading:

ade <- read.csv('source_data/ad_events.csv')
ads <- read.csv('source_data/ads.csv')
camp <- read.csv('source_data/campaigns.csv')
user <- read.csv('source_data/users.csv')

# cleaning up ads and users interests variable with onehot encoding

user2 <- user %>% 
  mutate(user_interests = strsplit(interests, ",\\s*")) %>%  # split by comma + optional space
  unnest(user_interests) %>%
  mutate(value = 1) %>%
  pivot_wider(
    names_from = user_interests,
    values_from = value,
    values_fill = 0,
    names_prefix = "user_"
  ) %>% select(-interests)

write.csv(user2, 'derived_data/user_cleaned.csv') # user_cleaned done

# Generating cleaned ads dataset:

ads2 <- ads %>%
  mutate(target_interests = strsplit(target_interests, ",\\s*")) %>%  # split by comma + optional space
  unnest(target_interests) %>%
  mutate(value = 1) %>%
  pivot_wider(
    names_from = target_interests,
    values_from = value,
    values_fill = 0,
    names_prefix = "target_"
  )
write.csv(ads2, 'derived_data/ads_cleaned.csv')

# Do target_interests align with the same as user interests?

setdiff(names(user2), names(ads2)) # Yes

# Composite 1 Data (without users) created

comp1 <- ade %>% left_join(ads2, by = 'ad_id' ) %>% left_join(camp, by = 'campaign_id')
write.csv(comp1, 'derived_data/composite_data1.csv')

# Full composite data created:

comp2 <- ade %>% left_join(ads2, by = 'ad_id') %>% 
  left_join(camp, by = 'campaign_id') %>% left_join(user2, by = 'user_id')
write.csv(comp2, 'derived_data/composite_data2.csv') 
