# Art History

# load packages 
library(tidyverse)
library(rvest)

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
  slice_head(n = 10)

# name of top artists 
imporatant_artists <- artists_mean_space %>%
  pull(artist_name) %>%
  unique 

# data frame for names
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
  img <- as.character(node[[1]])
  
  imporatant_artists_painting[which(imporatant_artists %in% i), "img"] <- img
  
}


