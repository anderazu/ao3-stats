# Unfold works into long data frame and do tag merger


library(tidyverse)


# Import data
(load("data/works.Rda"))


## Unfold works to long data frame

# Convert tags values to list-column, then unnest into long data frame
ptm <- proc.time()
wlist <- works %>% 
  mutate(tag_list = stringr::str_split(.data$tags, stringr::fixed("+")))
(proc.time() - ptm)   # about 55 seconds

# Run out of memory unless you're careful!
rm(works)
gc()

if(.Platform$OS.type == "windows") withAutoprint({
  memory.size()
  memory.size(TRUE)
  memory.limit()
})
memory.limit(size=56000)

ptm <- proc.time()
wlong <- wlist %>% 
  unnest(tag_list) %>% 
  select(-tags) %>% 
  mutate(tag_list = as.integer(tag_list))
(proc.time() - ptm)   # about 4 minutes

object.size(wlist)
object.size(wlong)

# Clean up, because this is riding my memory pretty hard
rm(wlist)

#save(wlong, file = "data/works_long.Rda")


# 2. Merge tags in long data frame

# NOW bring in tags
(load("data/tags.Rda"))

# Steps: 
# 1. Grab id and merger_id column from tags frame
# 2. Make new tag column equal to merger_id (if present) or id (if no merger_id)
# 3. Then pull in the name of the root (merged) tag with another left_join
wtagged <- wlong %>% 
  left_join(tags %>% select(id, merger_id), 
            by = c("tag_list" = "id")) %>% 
  mutate(tag = coalesce(merger_id, tag_list)) %>% 
  select(-tag_list, -merger_id) %>% 
  left_join(tags %>% select(id, name), 
            by = c("tag" = "id")) %>% 
  rename(tag_name = name)


# Check: Any tags I couldn't match?
wtagged %>% 
  mutate(mismatch = is.na(tag_name)) %>% 
  group_by(mismatch) %>% 
  count() %>% 
  mutate(frac = n / nrow(wtagged))
# Yep, about 8200 out of 120 million rows, or 0.007%

# Inspect any rows without a tag match
wtagged %>% 
  mutate(mismatch = is.na(tag_name)) %>% 
  filter(mismatch) 


# Need that memory
rm(wlong)

# Save long tagged works frame
save(wtagged, file = "data/works_tagged.Rda")  


## Remake a shorter works frame with tags as a single character column?