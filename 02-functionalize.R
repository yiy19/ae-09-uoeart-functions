# load packages ----------------------------------------------------------------

library(tidyverse)
library(rvest)

# function: scrape_art_info() --------------------------------------------------
# check how many inputs there are
scrape_art_info <- function(x){

  # read page at url ----
  page <- read_html(x)
  
  # scrape headers ----
  headers <- page %>%
    html_nodes("th") %>%
    html_text()
  
  # scrape values ----
  values <- page %>%
    html_nodes("td") %>%
    html_text() %>%
    str_squish()
  
  # put together in a tibble and add link to help keep track ----
  tibble(headers, values) %>%
    pivot_wider(names_from = headers, values_from = values) %>%
    add_column(link = x)
  
}

# load data to get links -------------------------------------------------------

uoe_art <- read_csv("data/uoe-art.csv")

# apply function ---------------------------------------------------------------

scrape_art_info(uoe_art$link[1]) %>% 
  glimpse()
scrape_art_info(uoe_art$link[2]) %>% 
  glimpse()
scrape_art_info(uoe_art$link[3]) %>% 
  glimpse()
