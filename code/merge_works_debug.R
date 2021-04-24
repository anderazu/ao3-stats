# Unfold works into long data frame and do tag merger

# Checking because in looking at RWBY works, I found a tag (266145) that still 
#  appears in unmerged form.

library(tidyverse)


# Import data
(load("data/works.Rda"))


## Helper function(s)

# Regex to match: Start of line OR non-digit, tag, non-digit OR end of line
makePat <- function(x) {
  pat <- paste0("(^|\\+)", x, "(\\+|$)") 
}

tagpat <- makePat(266145)

# Check on number of rows with tag that doesn't merge
works %>% filter(grepl(pattern = tagpat, x = tags))

# This is fewer and different works than when I run the same code on the 
#  works_merged frame in the fandom_pull.R file. Now I'm DEFINITELY confused. Am
#  I somehow reverse-merging that tag?


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
  left_join(tags %>% select(id, merger_id),        # step 1
            by = c("tag_list" = "id")) %>%  
  mutate(tag = coalesce(merger_id, tag_list)) %>%  # step 2
  select(-tag_list, -merger_id) #%>% 
  #left_join(tags %>% select(id, name),   # hold off until after second merge
  #          by = c("tag" = "id")) %>% 
  #rename(tag_name = name)
(proc.time() - ptm)   # about 3 minutes


# Check: Any tag names I couldn't match?
wtagged %>% 
  mutate(mismatch = is.na(tag_name)) %>% 
  group_by(mismatch) %>% 
  count() %>% 
  mutate(frac = n / nrow(wtagged))
# Yep, about 8200 out of 120 million rows, or 0.007%

# Inspect any rows without a tag name 
wtagged %>% 
  mutate(mismatch = is.na(tag_name)) %>% 
  filter(mismatch) 

# Clean up if needed
#rm(wlong)
#rm(tags)


## Check on problem tag

wlong %>% filter(tag_list == 266145)
# # A tibble: 3 x 7
# creat_date     wid language restricted complete word_count tag_list
# <date>       <int> <chr>    <lgl>      <lgl>         <dbl>    <int>
# 1 2011-07-22  189414 en       FALSE      TRUE           7277   266145
# 2 2017-08-11 3012368 en       FALSE      TRUE           4087   266145
# 3 2019-09-21 4997381 en       FALSE      TRUE            998   266145

wtagged %>% filter(tag == 266145)
# # A tibble: 102 x 8
# creat_date     wid language restricted complete word_count    tag tag_name            
# <date>       <int> <chr>    <lgl>      <lgl>         <dbl>  <dbl> <chr>               
# 1 2012-12-20  480187 en       FALSE      TRUE           1078 266145 Abuse of Shakespeare
# 2 2014-02-15  905378 en       FALSE      TRUE           2432 266145 Abuse of Shakespeare
# 3 2014-04-27  985073 en       FALSE      TRUE           1001 266145 Abuse of Shakespeare
# 4 2014-07-08 1077651 en       TRUE       TRUE            711 266145 Abuse of Shakespeare
# 5 2014-07-15 1087377 en       FALSE      TRUE           6440 266145 Abuse of Shakespeare
# 6 2014-08-04 1112814 en       FALSE      TRUE           3734 266145 Abuse of Shakespeare
# 7 2014-12-08 1261397 en       FALSE      TRUE          16011 266145 Abuse of Shakespeare
# 8 2015-03-19 1408832 en       FALSE      TRUE           5664 266145 Abuse of Shakespeare
# 9 2015-06-02 1520039 en       FALSE      TRUE          10500 266145 Abuse of Shakespeare
# 10 2015-07-18 1588513 en       FALSE      TRUE            166 266145 Abuse of Shakespeare
# # ... with 92 more rows

# Tags that are merged to but also have merger ids?
dup_canonical <- tags %>% 
  filter(canonical == TRUE, !is.na(merger_id))

# Is there a third layer of merge needed (whew, looks like no)
tags %>% 
  filter(id %in% dup_canonical$merger_id) %>% 
  View()

# The test: See if any of the tags now in wtagged have merger_id values
wtagged %>% 
  select(tag) %>% 
  distinct() %>% 
  left_join(tags, by = c("tag" = "id")) %>% 
  filter(!is.na(merger_id))
# # A tibble: 32 x 6
# tag type         name                        canonical cached_count merger_id
# <dbl> <chr>        <chr>                       <lgl>            <dbl>     <dbl>
# 1   720031 Relationship Redacted                    FALSE                3    114341
# 2   266145 Freeform     Abuse of Shakespeare        TRUE                 3    514794
# 3 35301761 Freeform     Inspired by a Bastille Song TRUE                 2  31327834
# 4 31706686 Freeform     Princess Park Jimin (BTS)   TRUE                 0  28360970
# 5  2141243 Freeform     Cat Jensen                  FALSE               14  27745492
# 6 12781465 Relationship J'onn J'onzz & Cisco Ramon  TRUE                 1  21949908
# 7 48216622 Character    Malice Sanders              TRUE                 1  28529279
# 8 21794688 Freeform     Yoonseok Week 2018          TRUE                 7  21793836
# 9 39558514 Character    Jealousy Sanders            TRUE                 4  28529279
# 10 28219598 Character    Hatred Sanders              TRUE                 2  28529279
# # ... with 22 more rows

# Do a second merge
ptm <- proc.time()
wtagged <- wtagged %>%   # input is existing wtagged frame
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
# A tibble: 0 x 6
# ... with 6 variables: tag <dbl>, type <chr>, name <chr>, canonical <lgl>, cached_count <dbl>, merger_id <dbl>

