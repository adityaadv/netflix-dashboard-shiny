library(plotly)
library(maps)
library(tidyverse)
library(scales)
library(lubridate)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(igraph)
library(networkD3)

# for replacing NAs
mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# for creating plotly animation frames
accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)],], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}

# read data
df_netflix <- read_csv("data_input/netflix_titles.csv")

#cleaning data
df_netflix <- df_netflix %>%
  replace_na(
    list(
      country = mode(df_netflix$country),
      rating = mode(df_netflix$rating),
      director = "Unknown",
      cast = "Unknown",
      date_added = mode(df_netflix$date_added)
    )
  ) %>%
  mutate(
    main_country = map(str_split(country, ", "), 1),
    main_cast = map(str_split(cast, ", "), 1),
    genre = map(str_split(listed_in, ", "), 1)
  ) %>%
  unnest() %>%
  mutate(
    type = as.factor(type),
    date_added = mdy(date_added),
    year_added = year(date_added),
    main_country = str_remove(main_country, ","),
    target_age = factor(
      sapply(
        rating,
        switch,
        'TV-PG' = 'Older Kids',
        'TV-MA' = 'Adults',
        'TV-Y7-FV' = 'Older Kids',
        'TV-Y7' = 'Older Kids',
        'TV-14' = 'Teens',
        'R' = 'Adults',
        'TV-Y' = 'Kids',
        'NR' = 'Adults',
        'PG-13' = 'Teens',
        'TV-G' = 'Kids',
        'PG' = 'Older Kids',
        'G' = 'Kids',
        'UR' = 'Adults',
        'NC-17' = 'Adults'
      ),
      level = c("Kids",
                "Older Kids",
                "Teens",
                "Adults")
    )
  )

#Preparing Chroloplette Map Data

mapdata <- map_data("world")

netflix_for_map <- df_netflix %>%
  mutate(main_country = str_replace_all(
    main_country,
    c(
      "United States" = "USA",
      "United Kingdom" = "UK",
      "Hong Kong" = "China",
      "Soviet Union" = "Russia",
      "West Germany" = "Germany"
    )
  ))

movie_df <- df_netflix %>%
  filter(type == "Movie")

tv_df <- df_netflix %>%
  filter(type == "TV Show")

for_network <- df_netflix %>% 
  select(director, cast) %>% 
  mutate(director = str_split(director, ", ")) %>% 
  unnest(cols = c(director)) %>% 
  mutate(cast = str_split(cast, ", ")) %>% 
  unnest(c(cast))
