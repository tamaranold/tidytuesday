# Tudytuesday R 

# load packages
library(tidyverse)
library(tidytuesdayR)
library(hrbrthemes)
library(extrafont)

# load data
nyt_titles <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-10/nyt_titles.tsv')
nyt_full <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-10/nyt_full.tsv')

# cite
cite_data <- 'Kelly, Nicholas; White, Nicole, Glass, Loren, 03/01/2021, “The Program Era > Project,” \nDOI: https://doi.org/10.18737/CNJV1733p4520210415, Post45 Data > Collective, V1.'

# palette
art_palette <- c("#D81774", "#005282", "#F1CE32", "#892678",
                 "#FA9F1B", "#1FABAC", "#ED2730", "#BDE7F9",
                 "#23513D", "#590D0F", "#F9CEE1", "#14667E",
                 "#9ECDC5", "#7B4198", "#52514F", "#1B80B8")

# Top 15 Best-Selling Authors in weeks
top_15_authors <- nyt_full %>%
  select(author, week) %>%
  distinct() %>%
  count(author) %>%
  slice_max(n, n = 15) %>%
  pull(author)

# # of Best-Sellers and top 3 book by author
data_top <- nyt_full %>% 
  filter(author %in% top_15_authors) %>%
  group_by(author, title) %>%
  mutate(n_weeks = n()) %>%
  group_by(author) %>%
  mutate(n_books = length(unique(title))) %>%
  arrange(author, desc(n_weeks)) %>%
  mutate(booklist = paste(unique(str_to_sentence(title))[1:3],
                          collapse = ", "), 
         legend = paste(n_books, "Best Sellers:", booklist))
  
top_titles <- data_top %>%
  arrange(factor(author, levels = top_15_authors)) %>%
  group_by(author) %>%
  slice_head() %>%
  pull(legend)
  
# create plot
data_top %>%  
  ggplot() +
  geom_tile(aes(x = week,
                y = fct_relevel(author, top_15_authors),
                fill = fct_relevel(author, top_15_authors))) +
  labs(title = "NYT BEST-SELLING AUTHORS",
       subtitle = "BOOK RANKED TOP 5",
       x = "",
       y = "",
       caption = paste0("data: ", cite_data)) +
  guides(fill = guide_legend(title.position = "top",
                             ncol = 2)) +
  scale_y_discrete(position = "right",
                   limits = rev) +
  scale_fill_manual(values = art_palette,
                    labels = str_wrap(top_titles, 50),
                    name = "# of Best-Sellers & 3 longest sales streaks") +
  theme_ft_rc(base_family = "Source Sans Pro",
              caption_family = "Source Sans Pro",
              plot_title_family =  "Source Code Pro Black",
              subtitle_family = "Source Code Pro Black",
              base_size = 10) +
  theme(plot.title.position = "plot",
        plot.title = element_text(face = "bold",
                                  lineheight = 0,
                                  size = 27),
        plot.subtitle = element_text(face = "bold",
                                     lineheight = 0, 
                                     size = 18,
                                     color = "#c4c4c8",
                                     margin = margin(b = 25),
                                     vjust = 5.2),
        panel.grid.major.y = element_blank(),
        legend.position = "bottom",
        legend.box.spacing = unit(25, "pt"),
        legend.key.size = unit(4, "pt"),
        legend.justification = c("center", "bottom"),
        legend.text = element_text(margin = margin(t = 3,
                                                   b = 3)),
        plot.caption = element_text(size = 8,
                                    margin = margin(t = 15)),
        plot.caption.position = "plot")   