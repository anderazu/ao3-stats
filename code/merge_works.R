# Unfold works into long data frame and do tag merger

# Saves the following new files:
# 1. works_long.Rda - One row per tag on a work, duplicate tags not merged
# 2. works_tagged.Rda - One row per tag on a work, duplicate tags merged, column 
#    for tag name
# 3. works_merged.Rda - One row per work, tags in +-separated character string

# 24 Apr 2021: Some tags have two layers of merger IDs, so run a second merge.


library(tidyverse)


# Import data
(load("data/works.Rda"))


## Unfold works to long data frame

# Get memory errors if we run this straight. Solution courtesy of: 
#  https://www.researchgate.net/post/How_to_solve_Error_cannot_allocate_vector_of_size_12_Gb_in_R
if(.Platform$OS.type == "windows") withAutoprint({
  memory.size()
  memory.size(TRUE)
  memory.limit()
})
memory.limit(size=56000)

# 1. Make long data frame of works

# Convert tags values to list-column, then unnest into long data frame
ptm <- proc.time()
wlong <- works %>% 
  mutate(tag_list = stringr::str_split(.data$tags, 
                                       stringr::fixed("+"))) %>% 
  unnest(tag_list) %>% 
  select(-tags) %>% 
  mutate(tag_list = as.integer(tag_list))
(proc.time() - ptm)   # about 5-7 minutes

object.size(works)
object.size(wlong)

# Fewer lines of code: separate_rows version
#ptm <- proc.time()
#wlong2 <- works %>% 
#  separate_rows(tags, convert = TRUE) %>% 
#  rename(tag_list = tags)
#(proc.time() - ptm)   # about 8 minutes, seems to be slower

#size1 <- object.size(wlong2)
#rm(wlong2)


# Save if changed (skip if not, this takes a minute)
#save(wlong, file = "data/works_long.Rda")


# 2. Merge duplicate tags in long data frame

# Clean up if needed, because this is riding my memory pretty hard
rm(works)

# NOW bring in tags
(load("data/tags.Rda"))

# Steps: 
# 1. Grab id and merger_id column from tags frame
# 2. Make new tag column equal to merger_id (if present) or id (if no merger_id)
# 3. Then pull in the name of the root (merged) tag with another left_join
ptm <- proc.time()
wtagged <- wlong %>% 
  left_join(tags %>% select(id, merger_id),         # step 1
            by = c("tag_list" = "id")) %>% 
  mutate(tag = coalesce(merger_id, tag_list)) %>%   # step 2
  select(-tag_list, -merger_id) #%>% 
  #left_join(tags %>% select(id, name),   # do this after second merge
  #          by = c("tag" = "id")) %>% 
  #rename(tag_name = name)
(proc.time() - ptm)   # about 3 minutes


# Biggest issue: Some of the tags left in wtagged still have merger_id values
wtagged %>% 
  select(tag) %>% 
  distinct() %>% 
  left_join(tags, by = c("tag" = "id")) %>% 
  filter(!is.na(merger_id))


# Clean up, then do a second merge
rm(wlong)
#rm(tags)

ptm <- proc.time()
wtagged <- wtagged %>%   # now, input is existing wtagged frame
  left_join(tags %>% select(id, merger_id), 
            by = c("tag" = "id")) %>%   # same join except tag_list -> tag now
  mutate(tag2 = coalesce(merger_id, tag)) %>% 
  select(-tag, -merger_id) %>% 
  left_join(tags %>% select(id, name), 
            by = c("tag2" = "id")) %>% 
  rename(tag_name = name, tag = tag2)
(proc.time() - ptm)   # about 3 minutes

# And test again: See if any tags in wtagged have merger_id values
wtagged %>% 
  select(tag) %>% 
  distinct() %>% 
  left_join(tags, by = c("tag" = "id")) %>% 
  filter(!is.na(merger_id))


# Smaller issue: Any tag names missing?
wtagged %>% 
  mutate(mismatch = is.na(tag_name)) %>% 
  group_by(mismatch) %>% 
  count() %>% 
  mutate(frac = n / nrow(wtagged))
# Yep, about 8200 out of 120 million rows, or 0.007%

# Inspect rows without a tag name 
wtagged %>% 
  mutate(mismatch = is.na(tag_name)) %>% 
  filter(mismatch) 


# Save long tagged works frame
save(wtagged, file = "data/works_tagged.Rda")  


## Remake a shorter works frame with tags as a single character column

ptm <- proc.time()
works_merged <- wtagged %>% 
  group_by(wid) %>% 
  select(-tag_name) %>% 
  nest(tag_list = tag) %>% 
  mutate(tags = map_chr(tag_list, 
                        ~ paste0(pull(.x), collapse = "+"))) %>% 
  select(-tag_list) %>% 
  ungroup()
(proc.time() - ptm)  # this takes a while (didn't time yet, at least 30 min?)


# Save tag-merged works frame
save(works_merged, file = "data/works_merged.Rda")  
