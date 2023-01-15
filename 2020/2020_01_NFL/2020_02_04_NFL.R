#---
# NFL
# Tamara Nold
# 04-02-2020
#---

#Load Packages
library(tidytuesdayR)
library(tidyverse)
library(stringr)
library(hrbrthemes)
library(ggtext)
library(grid)
library(gridExtra)

#Read Data
tuesdata <- tidytuesdayR::tt_load('2020-02-04')

standings <- tuesdata$standings
games <- tuesdata$games

#Set Theme
theme_set(theme_ft_rc())
update_geom_font_defaults(font_rc_light)

#Best Teams 
## Most Superbowl wins - MAP
wins <- standings %>% 
  select(team, team_name, sb_winner) %>%
  mutate(full_name = str_c(team, team_name, sep = " ")) %>%
  filter(sb_winner == "Won Superbowl") %>%
  add_count(full_name) %>%
  group_by(full_name) %>%
  slice(1) %>%
  arrange(desc(n)) %>%
  add_column(lat = c(46.537289, 39.290440, 40.713051,
                 40.442169, 39.742043, 44.513332,
                 39.791000, 39.099724, 29.951065,
                 39.95233, 47.608013, 27.964157), 
         long = c(-102.861549, -76.612328, -74.007233, 
                  -79.994957, -104.991531, -88.015831,
                  -86.148003, -94.578331, -90.071533,
                  -75.16379, -122.335167, -82.452606)) 
  

usa <- map_data("usa")

p.map <- ggplot() + 
  geom_polygon(data = usa, 
               aes(x = long, y = lat, group = group),
               color = "gray40", fill = "grey40") + 
  coord_fixed(1.3) +
  geom_point(data = wins, 
             aes(x = long, y = lat, size = n),
             color = "gold1") +
  labs(title = "Super Bowl Winners", 
       x = NULL, 
       y = NULL, 
       subtitle = "The New England Patriots have the most Super Bowl championship titles. They have won 6 times.",
       caption = "Point Size represents the # of Wins.") +
  geom_text(data = wins[1,], 
            aes(x = long , y = lat, label = full_name), 
            hjust = -0.1,
            vjust = 0.4,
            color = "gold",
            size = 4, 
            fontface = "bold") +
  geom_text(data = wins[2:nrow(wins),], 
            aes(x = long , y = lat, label = full_name), 
            hjust = -0.1,
            vjust = 0.4,
            color = "gold",
            size = 2.8) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "none",
        plot.title = element_text(hjust = .5),
        plot.subtitle = element_text(size = 10, hjust = .5))

p.map 

## Best Offense - PLOT DOT
p.offense <- standings %>% 
  mutate(full_name = str_c(team, team_name, sep = " ")) %>%
  group_by(full_name) %>%
  summarise(offensive_ranking = mean(offensive_ranking)) %>%
  arrange(desc(offensive_ranking)) %>%
  slice(1:7) %>%
  ggplot(aes(x = fct_reorder(full_name, offensive_ranking), y = offensive_ranking)) +
  geom_point(size = 7, color = "gold") +
  geom_segment(aes(xend = full_name, yend = 0), 
               size = 3, color = "gold") +
  geom_point(size = 3, color = "black") +
  geom_text(aes(label = round(offensive_ranking, 1)),
            vjust = 0.3, nudge_y = 0.9, size = 4, color = "gold") +
  coord_flip() +
  labs(y = "Offense Ranking as measured by SRS",
       x = "", 
       title = "Best Offense Teams") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none",
        plot.title = element_text(hjust = .5))


## Best Defense - PLOT DOT
p.defense <- standings %>% 
  mutate(full_name = str_c(team, team_name, sep = " ")) %>%
  group_by(full_name) %>%
  summarise(defensive_ranking = mean(defensive_ranking)) %>%
  arrange(desc(defensive_ranking)) %>%
  slice(1:7) %>%
  ggplot(aes(x = fct_reorder(full_name, defensive_ranking), 
             y = defensive_ranking)) +
  geom_point(size = 7, color = "gold") +
  geom_segment(aes(xend = full_name, yend = 0), 
               size = 3, color = "gold") +
  geom_point(size = 3, color = "black") +
  geom_text(aes(label = round(defensive_ranking, 1)),
            vjust = 0.3, nudge_y = 0.5, size = 4, color = "gold") +
  coord_flip() +
  labs(y = "Defense Ranking as measured by SRS",
       x = "", 
       title = "Best Defense Teams") +
   theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none",
        plot.title = element_text(hjust = .5))

#Matches - TEXT
## Max Points in Game 
gp <- games %>%
  select(year, date, home_team, away_team, winner, pts_win, pts_loss) %>%
  arrange(desc(pts_win)) %>%
  slice(1) 

p.max.point <- ggplot(gp) +
  geom_blank() +
  coord_fixed(ratio = 1/200) +
  labs(title = "Max Points",
       subtitle = paste("<br>
                        <b style='color:#ffc205;font-size:35px'>", gp$pts_win, "</b><br>
                        by", gp$winner, "<br>",
                        gp$home_team, "vs.", gp$away_team, "<br>",
                        gp$pts_win, ":", gp$pts_loss, "<br>",
                        gp$year, "", gp$date)) +
  theme(plot.subtitle = element_markdown(hjust = .5),
        plot.title = element_markdown(hjust = .5))

## Max Pointdifference in Game 
gp2 <- games %>%
  select(year, date, home_team, away_team, winner, pts_win, pts_loss) %>%
  mutate(pts_diff = pts_win - pts_loss) %>%
  arrange(desc(pts_diff)) %>%
  slice(1) 

p.max.diff <- ggplot(gp2) +
  geom_blank() +
  coord_fixed(ratio = 1/200) +
  labs(title = "Max Point Difference",
       subtitle = paste("<br>
                        <b style='color:#ffc205;font-size:35px'>", gp2$pts_diff, "</b><br>
                        by", gp2$winner, "<br>",
                        gp2$home_team, "vs.", gp2$away_team, "<br>",
                        gp2$pts_win, ":", gp2$pts_loss, "<br>",
                        gp2$year, "", gp2$date)) +
  theme(plot.subtitle = element_markdown(hjust = .5),
        plot.title = element_markdown(hjust = .5))


## Most Yards Win
gp3 <- games %>%
  select(year, date, home_team, away_team, winner, 
         yds_win) %>%
  arrange(desc(yds_win)) %>%
  slice(1)

p.max.yds <- ggplot(gp3) +
  geom_blank() +
  coord_fixed(ratio = 1/200) +
  labs(title = "Max Yards Won",
       subtitle = paste("<br>
       <b style='color:#ffc205;font-size:35px'>", gp3$yds_win, "</b><br>
       by", gp3$winner, "<br>",
       gp3$home_team, "vs.", gp3$away_team, "<br>",
       gp3$year, "", gp3$date, "<br>")) +
  theme(plot.subtitle = element_markdown(hjust = .5),
        plot.title = element_markdown(hjust = .5))

#Grid Plots 
grid.draw(grobTree(rectGrob(gp=gpar(fill="#252A33", lwd=0)), 
grid.arrange(
  grobs = list(p.map, p.offense, p.defense, p.max.point, p.max.diff, p.max.yds),
  widths = c(1, 1, 1, 1, 1, 1),
  heights = c(2, 1, 1),
  layout_matrix = rbind(c(1, 1, 1, 1, 1, 1),
                        c(2, 2, 2, 3, 3, 3),
                        c(4, 4, 5, 5, 6, 6)),
  top = textGrob(
    "SUPER BOWL STATS FROM SEASONS 2000-2019",
    gp = gpar(fontfamily = font_rc, fontsize = 35, fontface = 2,
              col = "gold"), hjust = .5),
  bottom = textGrob(
    "Data: Pro Football Reference | Project: TidyTuesday | Author: Tamara",
    gp = gpar(fontfamily = font_rc_light, fontsize = 9, col = "white"),
    hjust = .9,
    x = 1
  )
)))

# save 1000x1200
