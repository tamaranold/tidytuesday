# title: tidytuesday
# subtitle: tour de france
# author: tamara nold
# date: 02-05-2020

# load packages
library(tidyverse)
library(lubridate)
library(tidygeocoder)
library(hrbrthemes)
library(patchwork)
library(ggtext)

# load data 
tuesdata <- tidytuesdayR::tt_load('2020-04-07')
tdf_stages <- tuesdata$tdf_stages %>%
  mutate(Stage = as.numeric(Stage))

tdf_winners <- tuesdata$tdf_winners %>% 
  filter(format(start_date, "%Y") %in% c(1903, 2017))

# compare stages 1903 with 2017
first <- tdf_stages %>%
  filter(format(Date, '%Y') == "1903") %>%
  geocode(Origin, method = "osm", lat = lat_origin, long = long_origin) %>%
  geocode(Destination, method = "osm", lat = lat_destination, long = long_destination) 

last <- tdf_stages %>%
  filter(format(Date, '%Y') == "2017") %>%
  geocode(Origin, method = "osm", lat = lat_origin, long = long_origin) %>%
  geocode(Destination, method = "osm", lat = lat_destination, long = long_destination) 

last %>% 
  count(Type)

plot_first <-ggplot(first) +
  borders("world", regions = c("France", "Germany", "Luxembourg", 
                               "Belgium", "Switzerland"),
          colour = "grey", fill = "whitesmoke") +
  geom_point(aes(y = lat_origin, x = long_origin, color = Stage), size = 4) +
  geom_point(aes(y = lat_destination, x = long_destination, color = Stage), size = 4) +
  geom_point(data = first[which(first$Stage == 1), ], 
             aes(y = lat_origin, x = long_origin), color = "red", size = 4) +
  geom_segment(aes(y = lat_origin, yend = lat_destination, 
                   x = long_origin, xend = long_destination), 
               size = 1, color = "darkblue",
               arrow = arrow(length = unit(0.05, "inches"), type = "closed")) +
  labs(title = "Tour 1903", 
       y = "",
       x = "",
       caption = "The first Tour startet on 01.07.1903 and finished on 18.07.1903.<br>It contained of <B>6 Stages, mainly plain stages</B>.<br
       >Just the second stage from Lyon to Marseille was a montain stage.<br>The french <B>Maurice Garin</B> was the first Tour de France winner and<br>finished the 2428 km distance in 95 hours.") +
  theme_ft_rc() +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "none",
        plot.caption = element_markdown(color = "lightblue",
                                        size = 11))
  
plot_last <- ggplot(last) +
  borders("world", regions = c("France", "Germany", "Luxembourg", 
                               "Belgium", "Switzerland"),
          colour = "grey", fill = "whitesmoke") +
  geom_point(aes(y = lat_origin, x = long_origin, color = Stage), size = 4) +
  geom_point(aes(y = lat_destination, x = long_destination, color = Stage), size = 4) +
  geom_point(data = last[which(last$Stage == 1), ], 
             aes(y = lat_origin, x = long_origin), color = "red", size = 4) +
  geom_segment(aes(y = lat_origin, yend = lat_destination, 
                   x = long_origin, xend = long_destination), 
               size = 1, color = "darkblue",
               arrow = arrow(length = unit(0.05, "inches"), type = "closed")) +
  labs(title = "Tour 2017", 
       y = "",
       x = "",
       caption = "In 2017 the Tour de France were hold from 01.07. until 23.07.<br>The <B>stages increased on 21</B> and included 8 flat, 6 medium and<br>5 high mountain stages as well as an individual time trial.<br><B>Chris Froome</B> was the 106th Tour winner.<br>He finished the 3540 km distance in 86 hours.") +
  theme_ft_rc() +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "none",
        plot.caption = element_markdown(color = "lightblue",
                                        size = 11))  


# patchwork
plot_first + plot_last +
  plot_annotation(
    title = 'Tour de France',
    subtitle = "The Tour de France is an annual men's multiple stage bicycle race primarily held in France, while also occasionally passing \nthrough nearby countries. The follwing plots compare the first tour from 1903 and the tour from 2017.",
    caption = "<b style='color:red'>Red</b> points represents start of the Tour. The <b style='color:cornflowerblue'>blue points</b> are the start and finish of stages. Lighter blue points represent later stages.<br>Arrows show the schematic route of each stage (it's not the actual route).<br>Data: TidyTuesday | Tour de France | Graphic by: Tamara Nold",
    theme = theme_ft_rc() + theme(plot.caption = element_markdown(color = "lightblue",
                                                                  size = 11),
                                  plot.subtitle = element_text(color = "lightblue"))
  ) 

  
