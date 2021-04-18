# Pull all rows for a specific fandom tag from works, convert to long data frame

# 18 Apr 2021: There's a single RWBY-used tag that still has a merger id. Why?


library(tidyverse)


# Import data
(load("data/works_merged.Rda"))
(load("data/tags.Rda"))
#(load("data/tags_merged.Rda"))

works <- works_merged
rm(works_merged)

#tags <- tags_merged
#rm(tags_merged)


## Helper functions: Need to look up a tag id and find it in a string

# Regex to match: Start of line OR non-digit, tag, non-digit OR end of line
makePat <- function(x) {
  pat <- paste0("(^|[^[:digit:]])", x, "([^[:digit:]]|$)") 
}

# Find tag number for a given name
getTagID <- function(x, df = tags) {
  df %>% filter(name == x) %>% pull(var = id)
}


## Pick a fandom and filter works for those entries

# 1. Pull row(s) for a given tag and convert tag column into long data frame

# Filter to works rows of interest
tagpat <- makePat(getTagID("RWBY"))

wred <- works %>% 
  filter(grepl(pattern = tagpat, x = tags))


# Convert tags values to list-column, then unnest into long data frame
wlong_fandom <- wred %>% 
  mutate(tag_list = stringr::str_split(.data$tags, 
                                       stringr::fixed("+"))) %>% 
  unnest(tag_list) %>% 
  mutate(tag_list = as.integer(tag_list)) %>% 
  select(-tags)

# 2. Add tag names to long data frame
wtagged_fandom <- wlong_fandom %>% 
  left_join(tags %>% select(id, type, name), 
            by = c("tag_list" = "id"))

# Check: Any tags I couldn't match?
wtagged_fandom %>% 
  mutate(mismatch = is.na(name)) %>% 
  group_by(mismatch) %>% 
  count()  # 9 with NA for a name

# Inspect any rows without a tag match
wtagged_fandom %>% 
  mutate(mismatch = is.na(name)) %>% 
  filter(mismatch) 

# Example
tags %>% filter(id > 54605320)


## Filter tags to in-use list

# Pull in-use tags from filtered works frame
tags_used <- wlong_fandom %>% 
  ungroup() %>% 
  select(tag_list) %>% 
  distinct() %>% 
  arrange(tag_list)

# Match tags frame info to each in-use tag
tred <- tags_used %>% 
  left_join(tags, by = c("tag_list" = "id")) %>% 
  rename(id = tag_list)

tred %>% filter(!is.na(merger_id))  # Why is there still a non-merged tag here?


# Save filtered data frames
save(tred, file = "data/tags_RWBY.Rda")
save(wred, wtagged_fandom, file = "data/works_RWBY.Rda")
