# Art History

# load packages 
library(tidyverse)
library(rvest)
library(ggpattern)
library(tidytext)

# load data
artists <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-17/artists.csv')

# ideas ------------------------------------------------------------------------
# mean space per artist (top/bottom or both)
# show size in treemap or barplot
# fill tiles/bars with painting from artist

# data wrangling ---------------------------------------------------------------
# mean space for each top artists
artists_mean_space <- artists %>% 
  group_by(book, artist_unique_id, artist_name) %>%
  summarise(mean_space = mean(space_ratio_per_page_total)) %>%
  arrange(book, desc(mean_space)) %>%
  group_by(book) %>%
  slice_head(n = 7)

# name of top artists 
imporatant_artists <- artists_mean_space %>%
  pull(artist_name) %>%
  unique 

# data frame for top artists
imporatant_artists_painting <- imporatant_artists %>%
  data.frame %>%
  rename("name" = 1)

# scrape paintings from artists ------------------------------------------------ 
for(i in imporatant_artists) {
  url <- paste0("https://www.google.de/search?q=",
                str_replace_all(i, " ", "+"),
                "+painting&tbm=isch&ved=2ahUKEwiqpKnriNL8AhUan_0HHQMtB2sQ2-cCegQIABAA&oq=mone+painting&gs_lcp=CgNpbWcQAzIHCAAQgAQQEzIICAAQBRAeEBM6BAgAEB46BggAEAgQHjoFCAAQgAQ6CAgAEIAEELEDOgQIABADOgsIABCABBCxAxCDAToGCAAQHhAKOgoIABAIEB4QChATOggIABAIEB4QE1DXBljdK2C1LmgEcAB4AIABRogBvQeSAQIxNpgBAKABAaoBC2d3cy13aXotaW1nwAEB&sclient=img&ei=aWXIY-rLE5q-9u8Pg9qc2AY")
  page <- read_html(url) 
  node <- html_nodes(page, xpath = '//img')
  img <- as.character(node[[2]])
  
  imporatant_artists_painting[which(imporatant_artists %in% i), "img"] <- img
  
}

# add url for paintings on artists_mean_space
plot_data <- artists_mean_space %>%
  left_join(imporatant_artists_painting %>% 
              mutate(img = str_extract(img, 'src=".*">'),
                     img = str_replace(img, 'src="', ""),
                     img = str_replace(img, '">', "")),  
            by = c("artist_name" = "name"))

# information about books ans spacing ------------------------------------------


# create plot ------------------------------------------------------------------

plot_data %>%
  mutate(artist_name = reorder_within(as.factor(artist_name), 
                        desc(mean_space), 
                        book)) %>%
ggplot(aes(x = artist_name,
       y = mean_space)) +
  geom_col_pattern(
    aes(pattern_filename = I(img)),
    pattern = "image",
    pattern_type = "squish") +
  geom_text(
    aes(label = round(mean_space, 1),
        vjust = - 0.4)
  ) +
  labs(title = "Most important artists off all time",
       subtitle = "Based on the number of pages in art history textbooks") +
  scale_x_reordered() +
  scale_y_continuous(limits = c(0, 3.5)) +
  facet_wrap(~ book,
             ncol = 1,
             scales = "free_x") 
 