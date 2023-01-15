# TidyTuesday

# load packages
library(tidyverse)
library(geojsonio)
library(RColorBrewer)
library(rgdal)
library(broom)
library(rgeos)
library(countrycode)
library(hrbrthemes)
library(readxl)
library(ggtext)

# load data --------------------------------------------------------------------
# data
feederwatch <-
  readr::read_csv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-10/PFW_2021_public.csv'
  )

# region names and abbreviation
url <-
  "https://raw.githubusercontent.com/vincentarelbundock/countrycode/master/data/custom_dictionaries/us_states.csv"
state_dict <- read.csv(url, stringsAsFactors = FALSE)[, -3] %>%
  add_row(state = "District of Columbia",
          abbreviation = "DC")

# hexbin data
spdf <-
  geojson_read(paste0(
    dirname(rstudioapi::getSourceEditorContext()$path),
    "/us_states_hexgrid.geojson"
  ),  what = "sp")

spdf@data = spdf@data %<>%
  mutate(google_name = gsub(" \\(United States\\)", "", google_name))

spdf_fortified <- tidy(spdf, region = "google_name")

# bird names
bird_names <-
  read_excel(paste0(
    dirname(rstudioapi::getSourceEditorContext()$path),
    "/FeederWatch_Data_Dictionary.xlsx"
  ),
  sheet = 2)
# theme ------------------------------------------------------------------------
# color scheme
art_palette <- c(
  "#D81774",
  "#005282",
  "#F1CE32",
  "#892678",
  "#FA9F1B",
  "#1FABAC",
  "#ED2730",
  "#BDE7F9",
  "#23513D",
  "#590D0F",
  "#F9CEE1",
  "#14667E",
  "#9ECDC5",
  "#7B4198",
  "#52514F",
  "#1B80B8"
)


# data wrangling ---------------------------------------------------------------
# birds in the us
feederwatch_us <- feederwatch %>%
  separate(subnational1_code,
           into = c("country", "region"),
           sep = "-") %>%
  filter(country %in% c("US"),
         valid == 1)

bird_in_region <- feederwatch_us %>%
  group_by(region, species_code) %>%
  summarise(n = sum(how_many)) %>%
  slice_head(n = 1) %>%
  ungroup () %>%
  left_join(state_dict,
            by = c("region" = "abbreviation")) %>%
  left_join(spdf_fortified,
            by = c("state" = "id")) %>%
  left_join(bird_names %>%
              select(1, 3),
            by = c("species_code" = "SPECIES_CODE"))


# plot -------------------------------------------------------------------------

# Calculate the centroid of each hexagon to add the label:
centers <-
  cbind.data.frame(data.frame(gCentroid(spdf, byid = TRUE),
                              id = spdf@data$iso3166_2))

# Caption
cap <- "FeederWatch is a November-April survey of birds that visit
    backyards, nature centers, community areas, and other locales in
    North America. Citizen scientists could watch birds in areas with plantings,
    habitat, water, or food that attracts birds.The schedule is completely
    flexible. People count birds as long as they like on days of their choosing,
    then enter their counts online. This allows anyone to track what is
    happening to birds around your home and to contribute to a continental
    data-set of bird distribution and abundance."

# Now I can plot this shape easily as described before:
ggplot() +
  geom_polygon(
    data = bird_in_region,
    aes(
      x = long,
      y = lat,
      group = group,
      fill = PRIMARY_COM_NAME
    ),
    color = "black"
  ) +
  geom_text(
    data = centers,
    aes(x = x,
        y = y,
        label = id),
    color = "black",
    family = "Source Sans Pro"
  ) +
  labs(
    x = "",
    y = "",
    fill = "",
    title = "MOST COMMON BIRDS IN THE US",
    subtitle = "bird sightings from 2020 to 2021",
    caption = paste(c(
      strwrap(cap, 120),
      "<br>Data: Project FeederWatch"
    ),
    collapse = "<br>")
  ) +
  scale_fill_manual(values = art_palette) +
  coord_map() +
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
      size = 26
    ),
    plot.subtitle = element_text(
      face = "bold",
      lineheight = 0,
      size = 18,
      color = "#c4c4c8",
      margin = margin(b = 25),
      vjust = 5.2
    ),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "bottom",
    legend.key.size = unit(8, "pt"),
    legend.justification = c("center", "bottom"),
    plot.caption = element_markdown()
  )
