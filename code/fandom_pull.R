# Pull all rows for a specific fandom tag from works, convert to long data frame


library(tidyverse)
library(ggplot2)


# Import data
(load("data/works.Rda"))
(load("data/tags.Rda"))

tdf <- tags
wdf <- works

# Add column for work id
wdf <- wdf %>% 
  arrange(desc(row_number())) %>%   # invert rows first to count up
  mutate(wid = row_number()) %>% 
  select(creat_date, wid, everything())


## Helper functions first: Need to look up a tag id and find it in a string

# Regex to match: Start of line OR non-digit, tag, non-digit OR end of line
makePat <- function(x) {
  pat <- paste0("(^|[^[:digit:]])", x, "([^[:digit:]]|$)") 
}

# Find tag number for a given name
getTagID <- function(x, df = tdf) {
  df %>% filter(name == x) %>% pull(var = id)
}


## Pick a fandom and filter works for those entries

# 1. Pull row(s) for a given tag and convert tag column into long data frame

# Filter to works rows of interest
tagpat <- makePat(getTagID("RWBY"))

wred <- wdf %>% 
  filter(grepl(pattern = tagpat, x = tags))


# Convert tags values to list-column, then unnest into long data frame
wlist <- wred %>% 
  mutate(tag_list = stringr::str_split(.data$tags, stringr::fixed("+")))

wlong <- wlist %>% 
  unnest(tag_list) %>% 
  mutate(tag_list = as.integer(tag_list)) %>% 
  select(-tags)

# Add tag names to long data frame
wtagged <- wlong %>% 
  left_join(tdf %>% select(id, type, name), 
            by = c("tag_list" = "id"))

save(wred, wtagged, file = "data/works_RWBY.Rda")
