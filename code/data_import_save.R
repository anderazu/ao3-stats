# Import the works and tags .csv files, do a bit of preliminary cleanup, and 
# save as .Rda files for further processing.

# 10 Apr 2021: Reorder works oldest -> newest and add id column. I was doing
#   this everywhere else anyway, or confusing myself because I thought I had.
# 18 Apr 2021: Remove problems attribute from works frame (just held parse 
#   warnings, not needed once I've cleaned up the X7 column).


library(tidyverse)

# Import data
tags <- read_csv("data/tags-20210226.csv")
works <- read_csv("data/works-20210226.csv")


## Check for import problems

problems(tags)
problems(works)  # parse warnings from X7 column, clean up below


## Cleanup of data columns

# 1. Spaces in column names are annoying, ditch that
names(works)[1] <- "creat_date"

# 2. Remove column of NAs (made by extra comma at end of header row)
works <- works %>% select(-X7)

# That caused the problems attribute, so clean that up now
object.size(works)
attr(works, "problems") <- NULL
object.size(works)

# 3. Reorder from oldest -> newest and add column for work id
works <- works %>% 
  arrange(desc(row_number())) %>%   # invert rows first to count up
  mutate(wid = row_number()) %>% 
  select(creat_date, wid, everything())


## Save files for later use

# Save them to .Rda files (individually because huge)
save(tags, file = "data/tags.Rda")
save(works, file = "data/works.Rda")


# Save first 100 lines of each to CSV and RDA (for test-driving code)
work_samp <- works %>% slice_head(n = 100)
tag_samp <- tags %>% slice_head(n = 100)

write_csv(work_samp, file = "data/work_samp.csv")
write_csv(tag_samp, file = "data/tag_samp.csv")

save(work_samp, file = "data/work_samp.Rda")
save(tag_samp, file = "data/tag_samp.Rda")