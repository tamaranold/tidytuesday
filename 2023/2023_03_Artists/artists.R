# Tidy Tuesday
# Art History

# load packages
library(tidyverse)
library(rvest)
library(ggpattern)
library(tidytext)
library(hrbrthemes)
library(ggtext)

# load data
artists <-
  readr::read_csv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-17/artists.csv'
  )

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
for (i in imporatant_artists) {
  url <- paste0(
    "https://www.google.de/search?q=",
    str_replace_all(i, " ", "+"),
    "+painting&tbm=isch&ved=2ahUKEwiqpKnriNL8AhUan_0HHQMtB2sQ2-cCegQIABAA&oq=mone+painting&gs_lcp=CgNpbWcQAzIHCAAQgAQQEzIICAAQBRAeEBM6BAgAEB46BggAEAgQHjoFCAAQgAQ6CAgAEIAEELEDOgQIABADOgsIABCABBCxAxCDAToGCAAQHhAKOgoIABAIEB4QChATOggIABAIEB4QE1DXBljdK2C1LmgEcAB4AIABRogBvQeSAQIxNpgBAKABAaoBC2d3cy13aXotaW1nwAEB&sclient=img&ei=aWXIY-rLE5q-9u8Pg9qc2AY"
  )
  page <- read_html(url)
  node <- html_nodes(page, xpath = '//img')
  img <- as.character(node[[2]])
  
  imporatant_artists_painting[which(imporatant_artists %in% i), "img"] <-
    img
  
}

# add url for paintings on artists_mean_space
plot_data <- artists_mean_space %>%
  left_join(
    imporatant_artists_painting %>%
      mutate(
        img = str_extract(img, 'src=".*">'),
        img = str_replace(img, 'src="', ""),
        img = str_replace(img, '">', "")
      ),
    by = c("artist_name" = "name")
  )

# information about books ans spacing ------------------------------------------
# Gardner and Jansons
info_by_book <- artists %>%
  group_by(book) %>%
  summarise(
    edition_min = min(edition_number),
    edition_max = max(edition_number),
    edition_n = length(unique(edition_number)),
    year_min = min(year),
    year_max = max(year),
    artists_n = length(unique(artist_unique_id))
  ) %>%
  add_column(book_full_name = c("Gardner’s Art Through the Ages",
                                "Janson’s History of Art"))

artists %>%
  filter(book == "Janson") %>%
  summarise(unique(edition_number))


# Artists
info_artists <- artists %>%
  group_by(artist_unique_id) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  select(where(is.character)) %>%
  summarise(
    n = n(),
    n_nk = sum(str_detect(artist_name, "N/A")),
    female_percent = sum(artist_gender == "Female") / n(),
    male_percent = sum(artist_gender == "Male") / n(),
    gender_nk_percent = sum(artist_gender == "N/A") / n(),
    nationality_other_percent = sum(artist_nationality_other == "Other") /
      n(),
    race_nwi_percent = sum(artist_race_nwi == "Non-White") / n(),
    race_wi_percent = sum(artist_race_nwi == "White") / n()
  )

# Spacing
info_space <- artists %>%
  summarise(
    space_mean = mean(space_ratio_per_page_total),
    space_min = min(space_ratio_per_page_total),
    space_max = max(space_ratio_per_page_total)
  )

info_space_artists <- artists %>%
  filter(space_ratio_per_page_total %in% c(info_space$space_min,
                                           info_space$space_max))


# create plot ------------------------------------------------------------------
# facet labels
label_books <- paste0(
  info_by_book$book_full_name,
  ", including ",
  info_by_book$edition_max,
  " editions from ",
  info_by_book$year_min,
  " to ",
  info_by_book$year_max,
  ", in total the series features ",
  info_by_book$artists_n,
  " artists"
)

# caption
caption <-
  paste0(
    "In total Gardner's Art Through the Ages and Jansons's History of Art feature ",
    info_artists$n,
    " artists. ",
    info_artists$n_nk,
    " are not knwon by name.",
    " The majority of artists are males (",
    scales::percent(info_artists$male_percent),
    "),<br>just ",
    scales::percent(info_artists$female_percent),
    " are female and the gender of ",
    scales::percent(info_artists$gender_nk_percent),
    " is not known.",
    " Most common nationalities are French, Spanish, British, American and German, only ",
    scales::percent(info_artists$nationality_other_percent),
    " where born<br>in another country.And nearly ",
    scales::percent(info_artists$race_wi_percent),
    " are white.",
    "In the art history textbooks each artist where presented on",
    scales::percent(info_space$space_mean),
    " of a page. The most attention was given to ",
    info_space_artists$artist_name[1],
    "<br>in the ",
    info_space_artists$edition_number[1],
    "th edition of ",
    info_space_artists$book[1],
    " with ",
    round(info_space_artists$space_ratio_per_page_total[1], 1),
    " pages. Where as the shortest paragraph was written for ",
    info_space_artists$artist_name[2],
    " with ",
    scales::percent(info_space_artists$space_ratio_per_page_total[2], accuracy = 0.1),
    " of a page in the ",
    info_space_artists$edition_number[2],
    " edition of ",
    info_space_artists$book[2],
    ".<br><br>",
    "Data: Lemus S, Stam H (2022). arthistory: Art History Textbook Data"
  )

# plot
plot_data %>%
  mutate(artist_name = reorder_within(as.factor(artist_name),
                                      desc(mean_space),
                                      book)) %>%
  ggplot(aes(x = artist_name,
             y = mean_space)) +
  geom_col_pattern(aes(pattern_filename = I(img)),
                   pattern = "image",
                   pattern_type = "squish") +
  geom_text(aes(label = round(mean_space, 1),
                vjust = -0.4),
            family = "Source Sans Pro") +
  labs(
    title = "GOAT OF ART",
    subtitle = "Based on the average number of pages in art history textbooks",
    caption = caption,
    x = "",
    y = ""
  ) +
  scale_x_reordered() +
  scale_y_continuous(limits = c(0, 3.7)) +
  facet_wrap( ~ book,
              ncol = 1,
              scales = "free_x",
              labeller = labeller(book = c(Gardner = label_books[1],
                                           Janson = label_books[2]))) +
  theme_ft_rc(
    base_family = "Source Sans Pro",
    caption_family = "Source Sans Pro",
    plot_title_family =  "Source Code Pro Black",
    subtitle_family = "Source Code Pro Black",
    base_size = 10
  ) +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(
      face = "bold",
      lineheight = 0,
      size = 22,
      margin = margin(b = 0)
    ) ,
    plot.subtitle = element_markdown(
      face = "bold",
      lineheight = 0,
      size = 16,
      color = "#c4c4c8",
      margin = margin(b = 8)
    ),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_blank(),
    plot.caption = element_markdown()
  )
