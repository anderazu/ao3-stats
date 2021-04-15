# Merge duplicate tags in big data frame


library(tidyverse)


# Import data
(load("data/tags.Rda"))


## Big task: Merge duplicate tags 

# Can't wait to do this until it's fandom-specific, or needed tags are missing.

# Check that tags to be merged have identical types
ptm <- proc.time()
tags %>% 
  left_join(tags %>% select(-merger_id), 
            by = c("merger_id" = "id")) %>% 
  filter(!is.na(merger_id)) %>% 
  mutate(type_match = type.x == type.y) %>% 
  group_by(type_match) %>% 
  count() %>% 
  mutate(frac = n / nrow(tags))
(proc.time() - ptm)   # about 6 seconds

# There are a few NAs, check those out.
tagNAs <- tags %>% 
  left_join(tags %>% select(-merger_id), 
            by = c("merger_id" = "id")) %>% 
  filter(!is.na(merger_id)) %>% 
  mutate(type_match = type.x == type.y) %>% 
  filter(is.na(type_match)) %>% 
  select(id:merger_id)

tags %>% filter(id %in% tagNAs$merger_id)

# I think these are genuinely missing tags? And it's 20 out of all 14.5 million,
#  so I'll move on for now. 


## Proceed with merging tag frame!

# 1. Subset tag data frame by whether rows need merging
temp1 <- tags %>% 
  filter(is.na(merger_id))    # if merger_id = NA, no merge needed
  
temp2 <- tags %>% 
  filter(!is.na(merger_id))      # non-NA merger_id, need to merge

# 2. For frame of duplicated tags, swap in "canonical" id and name
temp2 <- temp2 %>% 
  left_join(tags %>% select(id, name), 
            by = c("merger_id" = "id")) %>%  # pull in merged names and tags
  select(-name.x) %>%    # remove old name
  mutate(id = merger_id) %>% # id to keep (save the old one)
  rename(name = name.y) %>%   # align names with original tags frame
  relocate(name, .before = canonical)  # restore column order

# 3. Recombine frames into new, merged tags list
# Accumulate cached_count values from tags that got merged
merger_counts <- temp2 %>% 
  group_by(id) %>% 
  summarize(cached_ct = sum(cached_count))

ptm <- proc.time()
tags_merged <- bind_rows(temp1, temp2) %>%
  left_join(merger_counts, by = "id") %>% 
  mutate(cached_ct = replace_na(cached_ct, 0), 
         cached_count = cached_count + cached_ct) %>% 
  filter(is.na(merger_id)) %>% 
  select(-cached_ct, -merger_id) %>%  # no longer needed
  arrange(id)
(proc.time() - ptm)   # about 4 seconds

rm(temp1, temp2)  # cleanup

# How much memory does this save?
object.size(tags)
object.size(tags_merged)


# Save merged tag list
save(tags_merged, file = "data/tags_merged.Rda")
