# title: Measels and Vaccinations
# author: Tamara
# date: 04-03-2020

# load packages
library(tidyverse)
library(tidytuesdayR)
library(summarytools)
library(naniar)
library(broom)
library(ggtext)
library(hrbrthemes)

# read data
tuesdata <- tidytuesdayR::tt_load('2020-02-25')
measles <- tuesdata$measles

# summary 
view(dfSummary(measles))

# tidy 
measles_tidy <- measles %>%
  select(-district) %>%
  filter(year != "null") %>%
  replace_with_na(list(mmr = -1, 
                       overall = -1))
         
# summary 
view(dfSummary(measles_tidy))


vac_type_year <- measles_tidy %>% 
  filter(type %in% c("Private", "Public")) %>%
  group_by(year, state, type) %>%
  summarise(mmr_mean = mean(mmr, na.rm = TRUE),
            overall_mean = mean(overall, na.rm = TRUE))
  
measle_type_year <- measles_tidy %>% 
  filter(type %in% c("Private", "Public")) %>%
  group_by(year, state, type) %>%
  summarise(list(tidy(t.test(mmr)))) %>%
  unnest() %>%
  filter(!state %in% c("Connecticut", "Colorado"))

measle_type_year %>%
  ggplot() +
  geom_errorbar(aes(ymin = conf.low,
                    ymax = conf.high,
                    x = fct_rev(state), 
                    color = type)) +
  geom_point(aes(x = fct_rev(state),
                 y = estimate,
                 color = type)) +
  labs(title = "Measles, Mumps, and Rubella
       vaccination rate in US-Schools",
       subtitle = "Compare <b style='color:#00bfc4'>Public</b> and <b style='color:#f8776d'>Private</b> Schools between 2017-2019",
       y = "Vaccination Rate",
       x = "States",
       caption = "Data: The Wallstreet Journal | Plot: Tamara | Project: Tidytuesday") +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     limits = c(85, 100)) +
  coord_flip() +
  theme_ipsum_rc() +
  theme(plot.subtitle = element_markdown(hjust = .9),
        plot.title = element_markdown(hjust = .9),
        legend.position = "none")

