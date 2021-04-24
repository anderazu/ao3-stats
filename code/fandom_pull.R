# Pull all rows for a specific fandom tag from works, convert to long data frame

# 18 Apr 2021: There's a single RWBY-used tag that still has a merger id. Why?
# 24 Apr 2021: Fixed, see merged_works_debug.R or merge_tags.Rmd for details.


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
  pat <- paste0("(^|\\+)", x, "(\\+|$)") 
}

# Find tag number for a given name
# Need to update, only works for exact matches
getTagID <- function(x, df = tags) {
  df %>% filter(name == x) %>% pull(var = id)
}

# If you don't know the exact name, will need to look for it
tags %>%
  filter(str_detect(name, pattern = "Portia"))


## Pick a fandom and filter works for those entries

# 1. Pull row(s) for a given tag and convert tag column into long data frame

# Filter to works rows of interest
tagpat <- makePat(getTagID("RWBY"))

#tags %>% 
#  filter(type == "Fandom") %>% 
#  filter(str_detect(name, pattern = "Stardew"))

#tagpat <- makePat(21232893)  # My Time at Portia
#tagpat <- makePat(8713639)  # Stardew Valley

ptm <- proc.time()
wred <- works %>% 
  filter(grepl(pattern = tagpat, x = tags))
(proc.time() - ptm)   


# Convert tags values to list-column, then unnest into long data frame
ptm <- proc.time()
wlong_fandom <- wred %>% 
  mutate(tag_list = stringr::str_split(.data$tags, 
                                       stringr::fixed("+"))) %>% 
  unnest(tag_list) %>% 
  mutate(tag_list = as.integer(tag_list)) %>% 
  select(-tags)
(proc.time() - ptm)   

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
#tags %>% filter(id > 43613120)


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

tred %>% filter(!is.na(merger_id))  # Should be empty if tags merged correctly


# Save filtered data frames
save(tred, file = "data/tags_RWBY.Rda")
save(wred, wtagged_fandom, file = "data/works_RWBY.Rda")
