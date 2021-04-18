# Merge duplicate tags in big data frame--testing and timing two methods.
# Turns out, the more elegant (fewer temp variables) method is sliiightly 
#  slower. Keep my original version.


library(tidyverse)


# Import data
(load("data/tags.Rda"))


## Preliminary checks

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

## Method 1: Split into two data frames, merge the relevant one, recombine

ptm <- proc.time()
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

#ptm <- proc.time()
tags_merged <- bind_rows(temp1, temp2) %>%
  left_join(merger_counts, by = "id") %>% 
  mutate(cached_ct = replace_na(cached_ct, 0), 
         cached_count = cached_count + cached_ct) %>% 
  filter(is.na(merger_id)) %>% 
  select(-cached_ct, -merger_id) %>%  # no longer needed
  arrange(id)
#(proc.time() - ptm)   # about 4 seconds

rm(temp1, temp2)  # cleanup
(proc.time() - ptm)   # about 15 seconds for the whole thing


## Method 2: Merge in to existing frame

ptm <- proc.time()
# Use coalesce to save either merger_id (if not NA) or original id
tags_merged2 <- tags %>% 
  left_join(tags %>% select(id, name), 
            by = c("merger_id" = "id")) %>% 
  mutate(id_final = coalesce(merger_id, id),
         name_final = coalesce(name.y, name.x)) %>% 
  select(-id, -name.x, -name.y) %>% 
  relocate(id = id_final, .before = "type") %>% 
  relocate(name = name_final, .after = "type")

# Still need a helper frame to accumulate cached_count values
merger_counts2 <- tags_merged2 %>% 
  filter(!is.na(merger_id)) %>% 
  group_by(id) %>% 
  summarize(cached_ct = sum(cached_count))

tags_merged2 <- tags_merged2 %>% 
  left_join(merger_counts2, by = "id") %>% 
  mutate(cached_ct = replace_na(cached_ct, 0), 
         cached_count = cached_count + cached_ct) %>% 
  filter(is.na(merger_id)) %>% 
  select(-cached_ct, -merger_id) %>%  # no longer needed
  arrange(id)
(proc.time() - ptm)   # about 15 seconds for the whole thing (same!)


# Do they match?
identical(tags_merged, tags_merged2)

# How much memory does this save?
object.size(tags)
object.size(tags_merged)
object.size(tags_merged2)


# Save merged tag list
save(tags_merged, file = "data/tags_merged.Rda")
