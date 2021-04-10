# Pull all rows for a specific fandom tag from the works frame


library(tidyverse)
library(ggplot2)


# Import data
(load("data/work_samp.Rda"))
(load("data/tag_samp.Rda"))

tdf <- tag_samp
wdf <- work_samp

# See what fandom(s) are available to try
fandoms <- tdf %>% filter(type == "Fandom") 
ftags <- fandoms %>% select(id, name, merger_id)


## Pick one (hello, Supernatural) and filter works for those entries

# What needs to match: 
# 1. Start of line, code, then +
# 2. +, code, +
# 3. + (or not!), code, end of line

# Phrased differently: Start of line OR non-digit, tag, non-digit OR end of line
pat <- paste0("(^|[^[:digit:]])", ftags$id[1], "([^[:digit:]]|$)")

wdf %>% 
  filter(grepl(pattern = pat, x = tags)) 

wdf %>% 
  filter(grepl(pattern = pat, x = tags)) %>% 
  tally()


# Move from hard-coded value to function

makePat <- function(x) {
  pat <- paste0("(^|[^[:digit:]])", x, "([^[:digit:]]|$)") 
}

# Test on a sequence of IDs
tagpats <- map_chr(tdf$id, makePat)


# Function to find tag number for a given name
getTagID <- function(df, x) {
  df %>% filter(name == x) %>% pull(var = id)
}


## Pull row(s) for a given tag and convert tag column into long data frame

# Filter to works rows of interest
tagpat <- makePat(getTagID(tdf, "Supernatural"))
wred <- wdf %>% 
  filter(grepl(pattern = tagpat, x = tags))

# Convert tags values to list-column
wred <- wred %>% 
  mutate(tag_list = stringr::str_split(.data$tags, stringr::fixed("+")))
wred$tag_list

# Unnest to make long data frame
wred %>% unnest(tag_list)

# Before saving this, add a column for work number
wred2 <- wred %>% 
  mutate(wid = row_number()) %>% 
  select(creat_date, wid, everything())

wlong <- wred2 %>% 
  unnest(tag_list) %>% 
  mutate(tag_list = as.integer(tag_list))


## Add tag names to long data frame

# Function to find tag name for a given id
getTagName <- function(df, x) {
  df %>% filter(id == x) %>% pull(var = name)
}

# Wait, I'm being silly, this is what join is for
wtagged <- wlong %>% 
  left_join(tdf %>% select(id, type, name), 
            by = c("tag_list" = "id")) %>% 
  select(-tags)
