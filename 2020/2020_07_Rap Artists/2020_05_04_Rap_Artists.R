# title: tiedytuesday
# subtitle: rap artist
# author: tamara nold
# date: 2020-05-04

# load package
library(tidyverse)
library(summarytools)
library(hrbrthemes)

# load data
tuesdata <- tidytuesdayR::tt_load('2020-04-14')
rank <- tuesdata$rankings

# glimspe data
view(dfSummary((rank)))

# period of best rap
ggplot(rank) +
  geom_point(aes(x = year, 
                 y = points,
                 color = gender))

# subset Top 5
rank5 <- rank %>% 
  top_n(5, points) %>%
  arrange(desc(points)) %>% 
  mutate(rank = ID) %>%
  pivot_longer(cols = c(points, n1, n2, n3, n4, n5),
               names_to = "voting", 
               values_to = "voting_result") %>%
  mutate(voting = fct_relevel(as.factor(voting),
                                "points"),
         voting_number = as.factor(as.numeric(voting)),
         rank = as.factor(rank),
         voting = fct_recode(as.factor(voting),
                             `Total points` = "points",
                             `Number of votes as #1` = "n1",
                             `Number of votes as #2` = "n2",
                             `Number of votes as #3` = "n3",
                             `Number of votes as #4` = "n4",
                             `Number of votes as #5` = "n5"),
         song = paste(artist, title, year, sep = "\n"))


ggplot() +
  geom_point(data = filter(rank5, voting == "Total points"),
    aes(x = voting_number, 
                 y = rev(rank),
                 size = 30)) +
  geom_point(data = filter(rank5, voting != "Total points"),
             aes(x = voting_number, 
                 y = rev(rank),
                 size = voting_result,
                 color = rev(rank))) +
  geom_text(data = rank5, 
            aes(x = voting_number,
                y = rev(rank),
                label = voting_result), 
            color = "white",
            family = "Roboto Condensed",
            fontface = "bold", 
            size = 4) +
  scale_size(range = c(15, 35)) +
  geom_vline(xintercept = 1.6) +
  scale_x_discrete(labels = levels(rank5$voting), 
                   position = "top") +
  scale_y_discrete(labels = rev(unique(rank5$song))) +
  labs(title = "Best hip-hop track ever",
       subtitle = "BBC Music asked more than 100 music industry folks for their five favorite hip-hop tracks \n10 points for first ranked track, eight points for second and so on down to two points for fifth place \nThe song with the most points won",
       x = "",
       y = "",
       caption = "Data: Tidytuesday | Rap Artists | Graphic: Tamara Nold") +
  theme_ipsum_rc() +
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        plot.title.position = "plot",
        plot.title = element_text(hjust = .5),
        plot.subtitle = element_text(size = 12, 
                                     hjust = .5))

