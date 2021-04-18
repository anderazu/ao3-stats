# Unfold works into long data frame and do tag merger


library(tidyverse)


# Import data
(load("data/works.Rda"))


## Unfold works to long data frame

# Solution courtesy of: 
#  https://www.researchgate.net/post/How_to_solve_Error_cannot_allocate_vector_of_size_12_Gb_in_R
if(.Platform$OS.type == "windows") withAutoprint({
  memory.size()
  memory.size(TRUE)
  memory.limit()
})
memory.limit(size=56000)

# Fewer lines of code: separate_rows version
#ptm <- proc.time()
#wlong2 <- works %>% 
#  separate_rows(tags, convert = TRUE) %>% 
#  rename(tag_list = tags)
#(proc.time() - ptm)   # about 8 minutes

#size1 <- object.size(wlong2)
#rm(wlong2)

# Convert tags values to list-column, then unnest into long data frame
ptm <- proc.time()
wlong <- works %>% 
  mutate(tag_list = stringr::str_split(.data$tags, 
                                       stringr::fixed("+"))) %>% 
  unnest(tag_list) %>% 
  select(-tags) %>% 
  mutate(tag_list = as.integer(tag_list))
(proc.time() - ptm)   # about 7 minutes

object.size(works)
object.size(wlong)

# Save if changed (skip if not, this takes a minute)
#save(wlong, file = "data/works_long.Rda")


# 2. Merge tags in long data frame

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
  left_join(tags %>% select(id, merger_id), 
            by = c("tag_list" = "id")) %>% 
  mutate(tag = coalesce(merger_id, tag_list)) %>% 
  select(-tag_list, -merger_id) %>% 
  left_join(tags %>% select(id, name), 
            by = c("tag" = "id")) %>% 
  rename(tag_name = name)
(proc.time() - ptm)   # about 


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