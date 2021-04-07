# Import the works and tags .csv files, do a bit of preliminary cleanup, and 
# save as .Rda files for further processing.

library(tidyverse)

# Import data
tags <- read_csv("data/tags-20210226.csv")
works <- read_csv("data/works-20210226.csv")


# Spaces in column names are annoying, ditch that
names(works)[1] <- "creat_date"

# Remove column of NAs (made by extra comma at end of header row)
works <- works %>% select(-X7)


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