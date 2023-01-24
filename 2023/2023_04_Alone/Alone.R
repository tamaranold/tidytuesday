# Tidytuesday
# Alone

# load libraries
library(tidyverse)
library(extrafont)

# load data
survivalists <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-24/survivalists.csv')

# ideas ------------------------------------------------------------------------
# who is dropping out when 

# data wrangling
survivalists <- survivalists %>%
  mutate(id = row_number()) %>%
  group_by(days_lasted, reason_category) %>%
  mutate(id_in_group = row_number())  %>%
  ungroup() %>%
  mutate(point = case_when(is.na(reason_category) ~ 0,
                           !is.na(reason_category) & id_in_group > 1 ~
                           1 -(id_in_group - 1) * 0.02,
                           TRUE ~ 1))

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

# theme ------------------------------------------------------------------------

pal <- c("#E76E4D", "#3D405B", "#2BAF70")
pal_na <- "#F1B554"
back_col <- "#FAF9F0"
high_col <- "#81B29A"

fnt <- "Caladea"

theme_set(theme_minimal(base_family = fnt))

theme_update(plot.background = element_rect(fill = back_col),
             axis.title.y = element_blank(),
             axis.text.y = element_blank(),
             panel.grid.major.y = element_blank(),
             panel.grid.minor.y = element_blank(),
             panel.grid.minor.x = element_blank(),
             legend.position = "bottom",
             plot.title.position = "plot",
             plot.caption.position = "plot")


# plot -------------------------------------------------------------------------
ggplot() +
  geom_line(data = survivalists_long,
            aes(x = days_lasted,
                y = smooth, 
                group = id
            ), 
            color = "#8A7656", 
            size = .5) +
  geom_point(data = survivalists, 
             aes(x = days_lasted, 
                 y = point,
                 group = id, 
                 color = reason_category),
             shape = 18,
             size = 2.5) +
  scale_y_reverse() +
  scale_x_continuous(position = "top",
                     n.breaks = 10) +
  scale_color_manual(values = pal,
                     na.value = pal_na)
  
