# title: tidytuesday
# subtitle: the office
# author: tamara nold
# date: 30.04.2020

# load packages 
library(tidyverse)
library(ggrepel)
library(gghighlight)
library(hrbrthemes)
library(ggtext)

# load data
tuesdata <- tidytuesdayR::tt_load('2020-03-17')
office_ratings <- tuesdata$office_ratings

# Which Episodes of The Office are the best?
# Does good/bad episodes get more votes than normal raings?
p1 <- ggplot(office_ratings, aes(y = imdb_rating, x = total_votes)) +
  geom_point(color = "red", alpha = .5) +
  gghighlight(imdb_rating > 9.5) +
  geom_hline(yintercept = 9.5, color = "red", alpha = .3) +
  geom_text_repel(aes(label = paste(title, paste(season, "x", episode), sep = "\n")), 
                  data = office_ratings[office_ratings$imdb_rating > 9.5, ],
                  color = "red", 
                  family = "Roboto Condensed", 
                  size = 3) +
  labs(title = "Best Episodes of <I>The Office</I>",
       y = "Rating (0-10)",
       x = "Number of Votes",
       caption = "<b style='color:red'>Red points mark the three best ranked episodes with title, season and episode</b><br>
         Data: TidyTuesday | The Office | Graphic by: Tamara Nold") +
  theme_ft_rc() +
  theme(plot.title = element_markdown(),
        plot.caption = element_markdown())
 
# Which seasons are the best?
office_ratings %>%
  group_by(season) %>%
  summarise(mean = mean(imdb_rating),
            median = median(imdb_rating))

p2 <- ggplot(office_ratings, aes(x = factor(season), y = imdb_rating)) +
  geom_boxplot() +
  geom_boxplot(data = office_ratings[office_ratings$season == 4, ], 
             color = "red") +
  geom_point() +
  geom_point(data = office_ratings[office_ratings$imdb_rating > 9.5, ], 
             color = "red") +
  labs(title = "Best Season of <I>The Office</I>",
       y = "Rating (0-10)",
       x = "Season",
       caption = "<b style='color:red'>Red boxplot marks season 4 as best season (highest mean of episode ratings) <br>
Red points mark the three best ranked episodes</b><br>
       Data: TidyTuesday | The Office | Graphic by: Tamara Nold" ) +
  theme_ft_rc() +
  theme(plot.caption = element_markdown(),
        plot.title = element_markdown())


  

