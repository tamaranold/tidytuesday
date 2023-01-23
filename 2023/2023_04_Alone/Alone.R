# Tidytuesday
# Alone

# load libraries
library(tidyverse)

# load data
survivalists <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-24/survivalists.csv')

# ideas ------------------------------------------------------------------------
# who is dropping out when 

# data wrangling
survivalists <- survivalists %>%
  mutate(id = row_number()) %>%
  group_by(days_lasted) %>%
  mutate(id_in_group = row_number(),
         point = ifelse(n() > 1,
                        1 -(id_in_group - 1) * 0.1,
                        6))  %>%
  ungroup() # !!!!

survivalists_long <- survivalists %>%
  select(days_lasted, reason_category) %>%
  mutate(id = row_number()) %>%
  group_by(id) %>%
  slice(rep(1, each = days_lasted)) %>%
  mutate(days_lasted = 1:n(),
         in_game = ifelse(!is.na(reason_category) & days_lasted == max(days_lasted),
                          0,
                          1),
         smooth = ifelse(is.na(reason_category), 0, days_lasted^8 / max(days_lasted^8)))


# plot -------------------------------------------------------------------------
ggplot() +
  geom_line(data = survivalists_long,
            aes(x = days_lasted,
                y = smooth, 
                group = id
            )) +
  geom_point(data = survivalists_long %>% group_by(id) %>% slice_tail(n = 1), 
             aes(x = days_lasted, 
                 y = point,
                 group = id, 
                 color = reason_category)) +
  scale_y_reverse() +
  scale_x_continuous(position = "top")
