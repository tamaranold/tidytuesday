# title: tidytuesday
# subtitle: brain injuries
# author: tamara nold
# date: 01-05-2020

# load packages 
library(tidyverse)
library(hrbrthemes)
library(ggforce)

# load data
tbi_age <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_age.csv')

# TBI by age
age <- tbi_age %>%
  filter(!age_group %in% c("0-17", "Total")) %>%
  mutate(age_group = fct_relevel(age_group, "5-14", after = 1)) %>%
  count(age_group, wt= number_est) 

## main causes of TBI by age 
tbi_age %>%
  filter(!age_group %in% c("0-17", "Total")) %>%
  count(age_group, injury_mechanism, wt = number_est) %>%
  ggplot() +
  geom_col(aes(x = injury_mechanism, y = n)) +
  facet_wrap(fct_relevel(age_group, "5-14", after = 1) ~ . , ncol = 4)

# -> 0-4 unintentional Falls
# -> 45 and older unintentional falls  - increases for each age group
# -> 15-34 most injuries becouse of motor vehicle crashes

## main plot
ggplot(age, aes(x = age_group, y = n)) +
  geom_col(fill = "darkslategray2", width = .7) +
  scale_y_continuous(labels = scales::comma) +
  geom_mark_hull(aes(filter = age_group == "15-24",
                     label = "15-24 Years",
    description = "TBI of young adults are mostly caused by motor vehicle crashes."),
                 fill = NA, color = NA, label.family = "Roboto Condensed",
    con.colour = "white", con.size = 1) +
  geom_mark_hull(aes(filter = age_group == "75+",
                     label = "75 Years and older",
                     description = "Unintentional falls are the main reason for TBI of elderly."),
                 fill = NA, color = NA, label.family = "Roboto Condensed",
                 con.colour = "white", con.size = 1) +
  geom_mark_hull(aes(filter = age_group == "0-4",
                     label = "4 Years and younger",
                     description = "Unintentional falls are the main reason for TBI of young children"),
                 fill = NA, color = NA, label.family = "Roboto Condensed",
                 con.colour = "white", con.size = 1, con.cap = -50) +
  geom_mark_hull(aes(filter = age_group == "45-54",
                     label = NULL,
                     description = "The frequency of TBI by unintentional falls increases at the Age of 45 for every age group."),
                 fill = NA, color = NA, label.family = "Roboto Condensed",
                 con.colour = "white", con.size = 1, con.cap = -50) +
  labs(title = "Traumatic Brain Injuries (TBI) in the US in 2014",
       subtitle = "One of every 60 people in the U.S. lives with a TBI related disability. Moderate and severe TBI can lead to a lifetime of physical, cognitive, emotional, and behavioral changes.",
       caption = "Data: TidyTuesday | Traumatic Brain Injury (TBI) | Graphic by: Tamara Nold",
       x = "Age Group in Years",
       y = "Number of TBI Cases") + 
  theme_ft_rc() +
  theme(plot.title.position = "plot")
  
