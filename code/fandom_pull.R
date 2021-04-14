# Pull all rows for a specific fandom tag from works, convert to long data frame


library(tidyverse)


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


## Merge duplicate tags in works list

# 1. Subset edge data frame by whether rows need merging
temp1 <- wtagged %>% 
  left_join(tred %>% select(id, merger_id), 
            by = c("tag_list" = "id")) %>% 
  filter(is.na(merger_id)) %>%   # if merger_id = NA, no merge needed
  select(-merger_id)

temp2 <- wtagged %>% 
  left_join(tred %>% select(id, merger_id), 
            by = c("tag_list" = "id")) %>% 
  filter(!is.na(merger_id))      # non-NA merger_id, need to merge

# 2. For frame of duplicated tags, replace with merged ID and name
temp2 <- temp2 %>% 
  select(-tag_list) %>%   
  left_join(tred %>% select(id, name), 
            by = c("merger_id" = "id")) %>%  # pull in merged names and tags
  rename(tag_list = merger_id, 
         name = name.y) %>%   # align names with df_rel
  select(-name.x) %>%    # remove old name and tag ID
  relocate(tag_list, .before = type)  # align column order with wtagged

# 3. Recombine frames into new, merged works list
wmerged <- bind_rows(temp1, temp2) %>% 
  arrange(wid)

rm(temp1, temp2) # clean up temp objects


## Merge duplicate tags in tags list

# Check that tags to be merged have identical types
tred %>% 
  left_join(tred %>% select(-merger_id), 
            by = c("merger_id" = "id")) %>% 
  filter(!is.na(merger_id)) %>% 
  mutate(type_match = type.x == type.y) %>% 
  group_by(type_match) %>% 
  count() %>% 
  mutate(frac = n / nrow(tred))
# Verdict: Usually, but not always! There are NAs, check those out.
tred %>% 
  left_join(tred %>% select(-merger_id), 
            by = c("merger_id" = "id")) %>% 
  filter(!is.na(merger_id)) %>% 
  mutate(type_match = type.x == type.y) %>% 
  filter(is.na(type_match)) 

tags %>% filter(id == "26465")

# ...Ah, shitsticks, this happens because about 5% of the tags point to IDs that 
#  aren't in the fandom-specific tag frame. I need to do the merger on the 
#  original huge frame for this to work.

# Save filtered data frames
save(tred, file = "data/tags_RWBY.Rda")
save(wred, wtagged, file = "data/works_RWBY.Rda")
