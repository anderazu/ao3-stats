# Pull all rows for a specific fandom tag from works, convert to long data frame


library(tidyverse)
library(ggplot2)


# Import data
(load("data/works.Rda"))
(load("data/tags.Rda"))

tdf <- tags
wdf <- works


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

# 2. Add tag names to long data frame
wtagged <- wlong %>% 
  left_join(tdf %>% select(id, type, name), 
            by = c("tag_list" = "id"))

# Check: Any tags I couldn't match?
wtagged %>% 
  mutate(mismatch = is.na(name)) %>% 
  group_by(mismatch) %>% 
  count()

# Inspect any rows without a tag match
wtagged %>% 
  mutate(mismatch = is.na(name)) %>% 
  filter(mismatch) 

# Example
tags %>% filter(id > 54605320)


## Filter tags to in-use list

# Pull in-use tags from filtered works frame
tags_used <- wlong %>% 
  select(tag_list) %>% 
  distinct() %>% 
  arrange(tag_list)

# Match tags frame info to each in-use tag
tred <- tags_used %>% 
  left_join(tags, by = c("tag_list" = "id")) %>% 
  rename(id = tag_list)


# Save filtered data frames
save(tred, file = "data/tags_RWBY.Rda")
save(wred, wtagged, file = "data/works_RWBY.Rda")
