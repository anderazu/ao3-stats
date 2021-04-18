# Unfold works into long data frame and do tag merger

# Test on smaller data set where I can check the output more carefully.


library(tidyverse)


# Import data
(load("data/work_samp.Rda"))

works <- work_samp#[1:20, ]


## Unfold works to long data frame

# Method 1: Convert tags values to list-column, then unnest into long data frame
ptm <- proc.time()
wlist <- works %>% 
  mutate(tag_list = stringr::str_split(.data$tags, stringr::fixed("+")))

wlong <- wlist %>% 
  unnest(tag_list) %>% 
  select(-tags) %>% 
  mutate(tag_list = as.integer(tag_list))
proc.time() - ptm


# Method 2: Go straight to long data frame via separate_rows
ptm <- proc.time()
wlong2 <- works %>% 
  separate_rows(tags, convert = TRUE) %>% 
  rename(tag_list = tags)
proc.time() - ptm

# Compare sizes
object.size(works)
object.size(wlist)
object.size(wlong)
object.size(wlong2)

# Check: Not actually equal! (Fixed now.)
identical(wlong, wlong2)
all.equal(wlong, wlong2)
# This happened because wlong had a problems attribute that was saved in the
#  separate_rows method but dropped in the other method. The problems attribute
#  is just parse warnings from the original read_csv, so I should remove it.


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


## Check for errors

# Check: Any tags I couldn't match? (Not in this toy data set)
wtagged %>% 
  mutate(mismatch = is.na(tag_name)) %>% 
  group_by(mismatch) %>% 
  count() %>% 
  mutate(frac = n / nrow(wtagged))

# Inspect any rows without a tag match
wtagged %>% 
  mutate(mismatch = is.na(tag_name)) %>% 
  filter(mismatch) 

# Check: Does my original number of tags match my current number of rows?
wlist %>% 
  mutate(ntags = map_int(tag_list, length)) 
sapply(wlist$tag_list, length)
# Yep


## Refold wtagged into having tags in a single character string

# Nest tags back into list column?
wlong_temp <- wlong %>% 
  group_by(wid) %>% 
  nest(tag_list = tag_list) %>% 
  mutate(tags = map_chr(tag_list, 
                        ~ paste0(pull(.x), collapse = "+")))

# Compare original and re-folded tags vector
identical(wlong_temp$tags, works$tags)

# Now do it with the merged tags list
wtemp <- wtagged %>% 
  group_by(wid) %>% 
  select(-tag_name) %>% 
  nest(tag_list = tag) %>% 
  mutate(tags = map_chr(tag_list, 
                        ~ paste0(pull(.x), collapse = "+"))) %>% 
  select(-tag_list)

# Spot-check swaps
works
wtemp
tags %>% filter(id == "125")
tags %>% filter(id == "89")
tags %>% filter(id == "714295")
